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
#include "utils.h"

#include "runtime/basic.h"
#include "runtime/mudlle-string.h"
#include "runtime/runtime.h"

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

struct mexception mexception;

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

bool mcatch(void (*fn)(void *x), void *x, enum call_trace_mode call_trace_mode)
{
  struct catch_context context = {
    .call_trace_mode = call_trace_mode,
    .parent          = catch_context,
    .old_ccontext    = ccontext,
    .old_gcpro       = gcpro,
    .old_stack_depth = stack_depth(),
    .old_call_stack  = call_stack
  };
  catch_context = &context;

  uword old_seclevel = internal_seclevel;
  value old_maxseclevel = maxseclevel;

  check_allow_mudlle_call();

  bool ok;
  if (nosigsetjmp(context.exception))
    {
      ccontext = context.old_ccontext;
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
      mexception.sig = SIGNAL_NONE;
      ok = true;
      assert(call_stack == context.old_call_stack);
    }

  assert(forbid_mudlle_calls == NULL);

  set_seclevel(old_seclevel);
  maxseclevel = old_maxseclevel;

  catch_context = context.parent;

  /* handle setjmp context */
  if (context.mjmpbuf)
    {
      bool was_used = context.mjmpbuf->result == NULL;
      context.mjmpbuf->context = NULL;
      context.mjmpbuf->result = NULL;
      context.mjmpbuf = NULL;

      switch (mexception.sig)
        {
        case SIGNAL_NONE:
          break;
        case SIGNAL_LONGJMP:
          if (was_used)
            {
              ok = true;
              break;
            }
          /* fallthrough */
        case SIGNAL_ERROR:
          mrethrow();
        }
    }

  return ok;
}

value mjmpbuf(value *result)
{
  assert(catch_context->mjmpbuf == NULL);
  struct mjmpbuf *jb = (struct mjmpbuf *)allocate_string(
    type_private, sizeof *jb - sizeof (struct obj));
  jb->p.ptype = makeint(PRIVATE_MJMPBUF);
  jb->context = catch_context;
  jb->result  = result;
  catch_context->mjmpbuf = jb;
  return jb;
}

bool is_mjmpbuf(value buf)
{
  struct mjmpbuf *mbuf = buf;
  return (TYPE(mbuf, type_private)
          && mbuf->p.ptype == makeint(PRIVATE_MJMPBUF)
          && mbuf->context != NULL);
}

/* Session context */
/* --------------- */

struct session_context *session_context;

ulong xcount;			/* Loop detection */
uword minlevel;			/* Minimum security level */

#if defined i386 || defined __x86_64__
ulong mudlle_stack_limit, hard_mudlle_stack_limit;
#endif

void session_start(struct session_context *context,
                   const struct session_info *info)
{
  context->s = (struct call_stack){
    .next = call_stack,
    .type = call_session
  };
  call_stack = &context->s;
  context->parent = session_context;
  session_context = context;

  context->_muduser = info->muser;
  context->_mudout = info->mout;
  context->_muderr = info->merr;

  context->recursion_count = MAX_RECURSION;

#if defined i386 || defined __x86_64__
  context->old_stack_limit = mudlle_stack_limit;
  mudlle_stack_limit = get_stack_pointer() - MAX_RECURSION * 16;
  if (mudlle_stack_limit < hard_mudlle_stack_limit)
    mudlle_stack_limit = hard_mudlle_stack_limit;
#endif

  context->old_minlevel = minlevel;
  minlevel = info->minlevel;

  context->old_maxseclevel = intval(maxseclevel);
  maxseclevel = makeint(info->maxseclevel);

  context->old_xcount = xcount;
  xcount = MAX_LOOP_COUNT;

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
  assert(call_stack == &session_context->s);
  call_stack = session_context->s.next;
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
  session_context->recursion_count = 0;
  xcount = MAX_TAGGED_INT;
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
  ccontext = (struct ccontext){ 0 };
}

struct list *mudcalltrace;	  /* list(cons(recipient, unhandled_only?)) */

void add_call_trace(value v, bool unhandled_only)
{
  assert(TYPE(v, type_oport) || TYPE(v, type_character));

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
  staticpro(&mudcalltrace);
  reset_context();
}
