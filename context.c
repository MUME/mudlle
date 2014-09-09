/*
 * Copyright (c) 1993-2012 David Gay and Gustav Hållberg
 * All rights reserved.
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose, without fee, and without written agreement is hereby granted,
 * provided that the above copyright notice and the following two paragraphs
 * appear in all copies of this software.
 *
 * IN NO EVENT SHALL DAVID GAY OR GUSTAV HALLBERG BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF DAVID GAY OR
 * GUSTAV HALLBERG HAVE BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * DAVID GAY AND GUSTAV HALLBERG SPECIFICALLY DISCLAIM ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
 * FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS ON AN
 * "AS IS" BASIS, AND DAVID GAY AND GUSTAV HALLBERG HAVE NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "alloc.h"
#include "builtins.h"
#include "code.h"
#include "context.h"
#include "error.h"
#include "global.h"
#include "stack.h"

#include "runtime/basic.h"
#include "runtime/runtime.h"
#include "runtime/stringops.h"

/* Function contexts */
/* ----------------- */

/* Mudlle call contexts, stacked */
struct call_stack *call_stack;

/* Catch contexts */
/* -------------- */

struct catch_context *catch_context;

struct ccontext ccontext;

uword internal_seclevel;
bool seclevel_valid;
value maxseclevel = makeint(MAX_SECLEVEL);

long exception_signal;
value exception_value;
struct catch_context *exception_context;

const struct session_info cold_session = {
  .minlevel    = 0,
  .maxseclevel = MAX_SECLEVEL
};

value seclevel_to_maxseclevel(int seclev)
{
  if (seclev >= LEGACY_SECLEVEL)
    return makeint(MAX_SECLEVEL);
  return makeint(seclev);
}

uword get_effective_seclevel(void)
{
  uword seclev = get_seclevel();
  long maxlev = intval(maxseclevel);
  return seclev > maxlev ? maxlev : seclev;
}

int mcatch(void (*fn)(void *x), void *x, enum call_trace_mode call_trace_mode)
{
  struct catch_context context;
  bool ok;

#ifdef AMIGA
  context.old_activation_stack = activation_stack;
  context.old_registers_valid = registers_valid;
  GCPRO1(context.old_activation_stack);
#endif

  context.call_trace_mode = call_trace_mode;

  context.occontext = ccontext;
  context.old_gcpro = gcpro;
  context.old_stack_depth = stack_depth();
  context.old_call_stack = call_stack;
  context.old_seclevel = internal_seclevel;
  context.old_maxseclevel = maxseclevel;

  context._mjmpbuf = NULL;

  context.parent = catch_context;
  catch_context = &context;

  check_allow_mudlle_call();

  if (nosigsetjmp(context.exception))
    {
#ifdef AMIGA
      registers_valid = context.old_registers_valid;
      activation_stack = context.old_activation_stack;
#endif
      ccontext = context.occontext;
#if defined(i386) && defined(USE_CCONTEXT)
      GCCHECK(ccontext.callee[0]);
      GCCHECK(ccontext.callee[1]);
      GCCHECK(ccontext.caller[0]);
      GCCHECK(ccontext.caller[1]);
#endif
      gcpro = context.old_gcpro;
      call_stack = context.old_call_stack;

      /* Pop any extra stuff from stack */
      int extra_depth = stack_depth() - context.old_stack_depth;
      assert(extra_depth >= 0);
      while (extra_depth--) stack_pop();

      forbid_mudlle_calls = NULL;

      ok = false;
    }
  else
    {
      fn(x);
      exception_signal = 0;
      ok = true;
      assert(call_stack == context.old_call_stack);
    }

  assert(forbid_mudlle_calls == NULL);

  set_seclevel(context.old_seclevel);
  maxseclevel = context.old_maxseclevel;

  /* handle setjmp context */
  if (context._mjmpbuf)
    {
      context._mjmpbuf->context = NULL;
      context._mjmpbuf = NULL;

      catch_context = context.parent;

      switch (exception_signal) {
      case SIGNAL_ERROR:
        mthrow(exception_signal, exception_value);
      case SIGNAL_LONGJMP:
        if (exception_context != &context)
          mthrow(exception_signal, exception_value);
        ok = true;
        break;
      case 0:
        break;
      }
    }
  else
    catch_context = context.parent;

#ifdef AMIGA
  UNGCPRO();
#endif

  return ok;
}

value mjmpbuf(void)
{
  if (catch_context->_mjmpbuf == NULL)
    {
      catch_context->_mjmpbuf =
        (struct mjmpbuf *)allocate_string(type_private,
                                          sizeof *catch_context->_mjmpbuf
                                          - sizeof (struct obj));
      catch_context->_mjmpbuf->ptype = makeint(PRIVATE_MJMPBUF);
      catch_context->_mjmpbuf->context = catch_context;
    }
  return catch_context->_mjmpbuf;
}

bool is_mjmpbuf(value buf)
{
  struct mjmpbuf *mbuf = buf;
  return (TYPE(mbuf, type_private)
          && mbuf->ptype == makeint(PRIVATE_MJMPBUF)
          && mbuf->context != NULL);
}

/* Session context */
/* --------------- */

struct session_context *session_context;

ulong xcount;			/* Loop detection */
uword minlevel;			/* Minimum security level */

#ifdef i386
ulong mudlle_stack_limit, hard_mudlle_stack_limit;
#endif

void session_start(struct session_context *context,
                   const struct session_info *info)
{
  context->parent = session_context;
  session_context = context;

  context->_muduser = info->muser;
  context->_mudout = info->mout;
  context->_muderr = info->merr;

  context->call_count = MAX_CALLS;
  context->recursion_count = MAX_RECURSION;

#ifdef i386
  context->old_stack_limit = mudlle_stack_limit;
  asm("movl %%esp,%0" : "=rm" (mudlle_stack_limit));
  mudlle_stack_limit -= MAX_RECURSION * 16;
  if (mudlle_stack_limit < hard_mudlle_stack_limit)
    mudlle_stack_limit = hard_mudlle_stack_limit;
#endif

  context->old_minlevel = minlevel;
  minlevel = info->minlevel;

  context->old_maxseclevel = intval(maxseclevel);
  maxseclevel = makeint(info->maxseclevel);

  context->old_xcount = xcount;
  xcount = MAX_FAST_CALLS;

  assert(info->maxseclevel >= MIN_SECLEVEL
         && info->maxseclevel <= MAX_SECLEVEL);

  context->old_gcpro = gcpro;
}

void cold_session_start(struct session_context *context,
                        uword maxseclev)
{
  struct session_info info = cold_session;
  info.maxseclevel = maxseclev;
  session_start(context, &info);
}

void session_end(void)
{
  assert(gcpro == session_context->old_gcpro);

  minlevel = session_context->old_minlevel;
  maxseclevel = makeint(session_context->old_maxseclevel);
#ifdef i386
  mudlle_stack_limit = session_context->old_stack_limit;
#endif
  xcount = session_context->old_xcount;
  session_context = session_context->parent;
}

void unlimited_execution(void)
{
  /* Effectively remove execution limits for current session */
  session_context->recursion_count = session_context->call_count = 0;
  xcount = 0;
#ifdef i386
  mudlle_stack_limit = hard_mudlle_stack_limit;
#endif
}


/* Global context */
/* -------------- */

void reset_context(void)
{
  stack_clear();
  call_stack = NULL;
  catch_context = NULL;
  session_context = NULL;
  memset(&ccontext, 0, sizeof ccontext);
}

struct list *mudcalltrace;	  /* list(cons(recipient, unhandled_only?)) */

void add_call_trace(value v, bool unhandled_only)
{
  assert(TYPE(v, type_outputport) || TYPE(v, type_character));

  mudcalltrace = alloc_list(alloc_list(v, makebool(unhandled_only)),
			    mudcalltrace);
}

void remove_call_trace(value v)
{
  for (struct list **this = &mudcalltrace;
       *this;
       this = (struct list **)&(*this)->cdr)
    {
      struct list *elem = (*this)->car;
      assert(TYPE(elem, type_pair));
      if (elem->car == v)
        {
          *this = (*this)->cdr;
          break;
        }
    }
}

void context_init(void)
{
  staticpro(&exception_value);
  staticpro(&mudcalltrace);
  reset_context();
}
