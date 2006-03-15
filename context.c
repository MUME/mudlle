/*
 * Copyright (c) 1993-2006 David Gay and Gustav Hållberg
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
#include "mudlle.h"
#include "alloc.h"
#include "types.h"
#include "code.h"
#include "stack.h"
#include "global.h"
#include "print.h"
#include "error.h"
#include "runtime/runtime.h"
#include "runtime/stringops.h"
#include "runtime/basic.h"
#include "builtins.h"

/* Function contexts */
/* ----------------- */

/* Mudlle call contexts, stacked */
struct call_stack *call_stack;

/* Catch contexts */
/* -------------- */

struct catch_context *catch_context;

struct ccontext ccontext;

uword seclevel;

long exception_signal;
value exception_value;
struct catch_context *exception_context;

int mcatch(void (*fn)(void *x), void *x, int display_error)
{
  struct catch_context context;
  int ok;

#ifdef AMIGA
  struct gcpro gcpro1;

  context.old_activation_stack = activation_stack;
  context.old_registers_valid = registers_valid;
  GCPRO1(context.old_activation_stack);
#endif

  context.display_error = display_error;

  context.occontext = ccontext;
  context.old_gcpro = gcpro;
  context.old_stack_depth = stack_depth();
  context.old_call_stack = call_stack;
  context.old_seclevel = seclevel;

  context._mjmpbuf = NULL;

  context.parent = catch_context;
  catch_context = &context;

  if (nosigsetjmp(context.exception))
    { 
      int extra_depth;

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
      extra_depth = stack_depth() - context.old_stack_depth;
      assert(extra_depth >= 0);
      while (extra_depth--) stack_pop();

      ok = FALSE;
    }
  else 
    {
      fn(x);
      exception_signal = 0;
      ok = TRUE;
      assert(call_stack == context.old_call_stack);
    }

  seclevel = context.old_seclevel;

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
        ok = TRUE;
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

int is_mjmpbuf(value buf)
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
		   uword new_minlevel,
		   Muser new_muduser, Mio new_mudout, Mio new_muderr)
{
  context->parent = session_context;
  session_context = context;

  context->_muduser = new_muduser;
  context->_mudout = new_mudout;
  context->_muderr = new_muderr;
  context->data = NULL;
  context->call_count = MAX_CALLS;
  context->recursion_count = MAX_RECURSION;

  context->old_minlevel = minlevel;
  minlevel = new_minlevel;
  context->old_xcount = xcount;
  xcount = 0;			/* High limit with standalone version */

#ifdef i386
  context->old_stack_limit = mudlle_stack_limit;
  asm("movl %%esp,%0" : "=rm" (mudlle_stack_limit));
  mudlle_stack_limit -= MAX_RECURSION * 16;
  if (mudlle_stack_limit < hard_mudlle_stack_limit)
    mudlle_stack_limit = hard_mudlle_stack_limit;
#endif
}

void session_end(void)
{
  xcount = session_context->old_xcount;
  minlevel = session_context->old_minlevel;
#ifdef i386
  mudlle_stack_limit = session_context->old_stack_limit;
#endif
  session_context = session_context->parent;
}

void unlimited_execution(void)
{
  /* Effectively remove execution limits for current session */
  session_context->recursion_count = session_context->call_count = 0;
  xcount = 0;
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

void add_call_trace(value v, int unhandled_only)
{
  assert(TYPE(v, type_outputport) || TYPE(v, type_character));

  mudcalltrace = alloc_list(alloc_list(v, makebool(unhandled_only)),
			    mudcalltrace);
}

void remove_call_trace(value v)
{
  struct list *prev, *this;
  struct gcpro gcpro1, gcpro2, gcpro3;

  prev = NULL;
  this = mudcalltrace;

  GCPRO2(prev, this);
  GCPRO(gcpro3, v);

  while (this)
    {
      struct list *elem = this->car;
      assert(TYPE(elem, type_pair));

      if (elem->car == v)
	{
	  if (prev == NULL)
	    mudcalltrace = this->cdr;
	  else
	    prev->cdr = this->cdr;
	  break;
	}

      prev = this;
      this = this->cdr;
    }

  UNGCPRO();
}

void context_init(void)
{
#if 0
  FILE *ctfile = fopen("mudlle-calltraces.txt", "w");
  mudcalltrace = alloc_list(alloc_list(make_file_outputport(ctfile), 0), NULL);
#else
  mudcalltrace = NULL;
#endif

  staticpro(&exception_value);
  staticpro((value *)&mudcalltrace);
  reset_context();
}
