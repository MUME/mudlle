/* $Log: context.c,v $
 * Revision 1.1  1995/07/15  15:49:27  arda
 * New files, missing from previous commit.
 *
 *
 * Purpose: an attempt at providing a centralised view of the mudlle context,
 *   and the operations to control it.
 */

static char rcsid[] = "$Id: context.c,v 1.1 1995/07/15 15:49:27 arda Exp $";

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

/* Current catch context, with extra private information */
struct private_catch_context
{
  struct private_catch_context *parent;
  struct catch_context public;	/* Public information */
  jmp_buf exception;		/* The return point */

  /* Save values of constant/local/preserved M variables, to be able to restore
     them after a throw(). In an ideal world, everything would be in the
     call_stack ... */
  struct call_stack *old_call_stack;
  int old_stack_depth;
  struct gcpro *old_gcpro;
  uword old_seclevel;
#ifdef AMIGA
  struct vector *old_activation_stack;
  int old_registers_valid;
#endif
};

static struct private_catch_context *private_catch_context;
struct catch_context *catch_context;

uword seclevel;

long exception_signal;
value exception_value;

int catch(void (*fn)(void *x), void *x, int display_error)
{
  volatile struct private_catch_context context;
  int ok;

#ifdef AMIGA
  struct gcpro gcpro1;

  context.old_activation_stack = activation_stack;
  context.old_registers_valid = registers_valid;
  GCPRO1(context.old_activation_stack);
#endif

#ifdef sparc
  context.public.old_frame_start = frame_start;
  context.public.old_frame_end = frame_end;
#endif

  context.public.display_error = display_error;

  context.old_gcpro = gcpro;
  context.old_stack_depth = stack_depth();
  context.old_call_stack = call_stack;
  context.old_seclevel = seclevel;

  context.parent = private_catch_context;
  catch_context = &context.public;
  private_catch_context = &context;


  if (_setjmp(context.exception)) 
    { 
      int extra_depth;

#ifdef sparc
      frame_start = context.public.old_frame_start;
      frame_end = context.public.old_frame_end;
#endif
#ifdef AMIGA
      registers_valid = context.old_registers_valid;
      activation_stack = context.old_activation_stack;
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

  if (private_catch_context = context.parent)
    catch_context = &private_catch_context->public;
  else
    catch_context = NULL;

#ifdef AMIGA
      UNGCPRO();
#endif

  return ok;
}

NORETURN void throw(long signal, value val)
{
  exception_signal = signal;
  exception_value = val;
  _longjmp(private_catch_context->exception, signal);
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
  context->call_count = MAX_CALLS;

  context->old_minlevel = minlevel;
  minlevel = new_minlevel;
  context->old_xcount = xcount;
#ifdef MUME
  xcount = MAX_FAST_CALLS;	/* Different counter for machine code */
#else
  xcount = 0;			/* High limit with standalone version */
#endif
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
  private_catch_context = NULL;
  session_context = NULL;
}


void context_init(void)
{
  staticpro(&exception_value);
  reset_context();
}
