/*
 * Copyright (c) 1993-1999 David Gay and Gustav Hållberg
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
#ifdef i386
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

  catch_context = context.parent;

#ifdef AMIGA
  UNGCPRO();
#endif

  return ok;
}

void mthrow(long signal, value val)
{
  exception_signal = signal;
  exception_value = val;
  nosiglongjmp(catch_context->exception, signal);
}

/* Session context */
/* --------------- */

struct session_context *session_context;

ulong xcount;			/* Loop detection */
uword minlevel;			/* Minimum security level */

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

  context->old_minlevel = minlevel;
  minlevel = new_minlevel;
  context->old_xcount = xcount;
  xcount = 0;			/* High limit with standalone version */
}

void session_end(void)
{
  xcount = session_context->old_xcount;
  minlevel = session_context->old_minlevel;
  session_context = session_context->parent;
}

void unlimited_execution(void)
{
  /* Effectively remove execution limits for current session */
  session_context->call_count = 0;
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


void context_init(void)
{
  staticpro(&exception_value);
  reset_context();
}
