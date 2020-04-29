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

#include "mudlle-config.h"

#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sys/resource.h>
#include <sys/time.h>

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

seclev_t internal_seclevel;
bool seclevel_valid;
value maxseclevel = makeint(MAX_SECLEVEL);

struct mexception mexception;

const struct session_info cold_session = {
  .minlevel    = 0,
  .maxseclevel = MAX_SECLEVEL
};

value seclevel_to_maxseclevel(seclev_t seclev)
{
  if (seclev >= LEGACY_SECLEVEL)
    return makeint(MAX_SECLEVEL);
  return makeint(seclev);
}

seclev_t get_effective_seclevel(void)
{
  seclev_t seclev = get_seclevel();
  seclev_t maxlev = intval(maxseclevel);
  return seclev > maxlev ? maxlev : seclev;
}

/* use volatile automatic-storage for setjmp()/longjmp() */
static bool safe_mcatch(void (*fn)(void *x), void *x,
                        struct catch_context *volatile context)
{
  /* hard stack limit can temporarily be changed by check_segv() to handle
     exceptions using the alternate signal stack */
  volatile ulong old_hard_mudlle_stack_limit = hard_mudlle_stack_limit;
  if (sigsetjmp(context->exception, 0))
    {
      hard_mudlle_stack_limit = old_hard_mudlle_stack_limit;

      ccontext = context->old_ccontext;
#ifdef USE_CCONTEXT
#define __CALLER_GCCHECK(n, reg) GCCHECK(ccontext.caller.reg)
#define __CALLEE_GCCHECK(n, reg) GCCHECK(ccontext.callee.reg)
      FOR_CALLER_SAVE(__CALLER_GCCHECK, SEP_SEMI);
      FOR_CALLEE_SAVE(__CALLEE_GCCHECK, SEP_SEMI);
#undef __CALLEE_GCCHECK
#undef __CALLER_GCCHECK
#endif
      gcpro = context->old_gcpro;
      call_stack = context->old_call_stack;

      /* pop any extra stuff from stack */
      int extra_depth = stack_depth() - context->old_stack_depth;
      assert(extra_depth >= 0);
      while (extra_depth--) stack_pop();

      forbid_mudlle_calls = NULL;

      return false;
    }
  else
    {
      fn(x);
      mexception.sig = SIGNAL_NONE;
      assert(call_stack == context->old_call_stack);
      return true;
    }
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

  seclev_t old_seclevel = internal_seclevel;
  value old_maxseclevel = maxseclevel;

  check_allow_mudlle_call();

  bool ok = safe_mcatch(fn, x, &context);

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
  return (TYPE(mbuf, private)
          && mbuf->p.ptype == makeint(PRIVATE_MJMPBUF)
          && mbuf->context != NULL);
}

/* Session context */
/* --------------- */

struct session_context *session_context;

ulong xcount;			/* Loop detection */
seclev_t minlevel;		/* Minimum security level */

ulong mudlle_stack_limit, hard_mudlle_stack_limit;

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


  context->old_stack_limit = mudlle_stack_limit;
  mudlle_stack_limit = get_stack_pointer() - MAX_STACK_DEPTH;
  if (mudlle_stack_limit < hard_mudlle_stack_limit)
    mudlle_stack_limit = hard_mudlle_stack_limit;

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
                        seclev_t maxseclev)
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
  mudlle_stack_limit = session_context->old_stack_limit;
  xcount = session_context->old_xcount;
  session_context = session_context->parent;
}

void unlimited_execution(void)
{
  /* Effectively remove execution limits for current session */
  xcount = MAX_TAGGED_INT;
  mudlle_stack_limit = hard_mudlle_stack_limit;
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
  assert(TYPE(v, oport) || TYPE(v, character));

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
      assert(TYPE(elem, pair));
      if (elem->car == v)
        {
          *this = (*this)->cdr;
          break;
        }
    }
}

/* if RLIMIT_STACK is not infinite, set the hard mudlle stack limit,
   reserving some space */
void set_mudlle_stack_limit(unsigned long reserved)
{
  struct rlimit lim;
  if (getrlimit(RLIMIT_STACK, &lim) < 0)
    {

      perror("getrlimit(RLIMIT_STACK, ...)");
      exit(EXIT_FAILURE);
    }

  rlim_t size = lim.rlim_cur;
  if (size == RLIM_INFINITY || size <= reserved)
    return;

  size -= reserved;
  unsigned long sp = get_stack_pointer();
  if (sp <= size)
    return;

  hard_mudlle_stack_limit = sp - size;
}

void context_init(void)
{
  staticpro(&mudcalltrace);
  reset_context();

  if (hard_mudlle_stack_limit == 0)
    set_mudlle_stack_limit(4096);
}
