/* $Log: error.c,v $
 * Revision 1.7  1995/07/30  14:23:29  arda
 * Undocumented changes, as usual
 *
 * Revision 1.6  1995/07/15  15:24:17  arda
 * Context cleanup.
 * Remove GCDEBUG.
 *
 * Revision 1.5  1994/09/15  19:46:37  arda
 * Performance improvements:
 *   setjmp -> _setjmp (setjmp is horrendously slow)
 *   cold_protect
 * reset_limits split from reset_interpreter
 * fix division of negative numbers
 * Add ?\{n,r,t}
 * gc_size returns "mutable" size
 *
 * Revision 1.4  1994/08/22  11:18:25  arda
 * Moved code allocation to ins.c
 * Changes for mudlle compiler in MUME.
 *
 * Revision 1.3  1994/08/16  19:15:50  arda
 * Mudlle compiler for sparc now fully functional (68k compiler now needs
 * updating for primitives).
 * Changes to allow Sparc trap's for runtime errors.
 * Also added flags to primitives for better calling sequences.
 *
 * Revision 1.13  1994/04/12  20:12:05  arda
 * (MD) Alignments and fixes + unknown from others...
 *
 * Revision 1.12  1994/03/08  01:50:51  arda
 * (MD) New Istari.
 *
 * Revision 1.11  1994/02/24  08:33:37  arda
 * Owl: New error messages.
 *
 * Revision 1.10  1994/02/12  17:25:41  arda
 * Owl: MUME IV (special procedures eliminated).
 *
 * Revision 1.9  1993/10/03  14:07:24  dgay
 * Bumper disun8 update.
 *
 * Revision 1.8  1993/05/02  13:03:04  un_mec
 * Owl: ARGH! Bugs.
 *
 * Revision 1.7  1993/03/29  09:25:37  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.4  1993/03/17  12:50:58  dgay
 * Added security features.
 *
 * Revision 1.3  1993/03/14  16:16:37  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.5  1993/01/30  12:14:05  un_mec
 * Owl: Mudlle reactions installed, with loading and editing commands.
 * Also new: room commands, actions (only tell for now).
 *
 * Revision 1.4  1993/01/26  09:49:13  un_mec
 * Owl:
 * - Limit mudlle execution time (prevent infinite loops).
 * - Add mudlle reaction procedures.
 *
 * Revision 1.3  1993/01/11  16:15:39  un_mec
 * Run emacs with security installed. Users may only edit in
 * /home/mud/mume/lib/mudlle/<their name>/.
 * Arata and higher can edit any user's directory.
 * /mudlle can now be opened to all gods (on disun8 initially).
 *
 * Add read-only variables error message.
 *
 * Add some object ops.
 *
 * Revision 1.2  1993/01/08  23:57:44  un_mec
 * Owl: Add character and object types.
 *
 * Revision 1.1  1992/12/27  21:42:16  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

static char rcsid[] = "$Id: error.c,v 1.7 1995/07/30 14:23:29 arda Exp $";

#include <stdlib.h>
#include <string.h>

#include "runtime/runtime.h"
#include "mudio.h"
#include "print.h"
#include "alloc.h"

static char *errors[] = {
  "bad function",
  "stack underflow",
  "bad type",
  "divide by zero",
  "bad index",
  "bad value",
  "variable is read-only",
  "function probably has an infinite loop",
  "function probably has an infinite recursion",
  "wrong number of parameters",
  "insufficient privilege",
  "value is read only",
  "user interrupt"
};

static void print_error(int error)
{
  if (error < last_runtime_error && error >= 0)
    mprintf(muderr, "%s" EOL, errors[error]);
  else
    mprintf(muderr, "error %d" EOL, error);
  mprintf(muderr, "Call trace is:" EOL);
}

static void print_bytecode_frame(struct call_stack *frame, int onstack)
{
  int i;
  struct code *fcode = frame->u.mudlle.fn->code;

  if (fcode->filename->str[0])
    {
      if (fcode->varname)
	mprint(muderr, prt_display, fcode->varname);
      else mputs("<fn>", muderr);

      mputs("[", muderr);
      mprint(muderr, prt_display, fcode->filename);
      mprintf(muderr, ":%d](", fcode->lineno);

      /* Warning: This is somewhat intimate with the
	 implementation of the compiler */
      for (i = 0; i < frame->u.mudlle.nargs; i++)
	{
	  value v;

	  if (onstack)
	    v = stack_get(frame->u.mudlle.nargs - i - 1);
	  else
	    {
	      struct variable *argi =
		frame->u.mudlle.locals->data[frame->u.mudlle.nargs - i - 1];

	      v = argi->value;
	    }

	  if (i > 0) mputs(", ", muderr);
	  mprint(muderr, prt_print, v);
	}

      mputs(")" EOL, muderr);
    }
}

static void print_c_frame(struct call_stack *frame)
{
  mprintf(muderr, "%s(", frame->u.c.op->name);

#define C_ARG(n) frame->u.c.arg ## n

  if (frame->u.c.nargs >= 1)
    mprint(muderr, prt_print, C_ARG(1));
  if (frame->u.c.nargs >= 2)
    {
      mputs(", ", muderr);
      mprint(muderr, prt_print, C_ARG(2));
    }
  if (frame->u.c.nargs >= 3)
    {
      mputs(", ", muderr);
      mprint(muderr, prt_print, C_ARG(3));
    }
  if (frame->u.c.nargs >= 4)
    {
      mputs(", ", muderr);
      mprint(muderr, prt_print, C_ARG(4));
    }
  if (frame->u.c.nargs >= 5)
    {
      mputs(", ", muderr);
      mprint(muderr, prt_print, C_ARG(5));
    }

  mputs(")" EOL, muderr);
}

static void print_c_frame_stack(struct call_stack *frame)
{
  int i;

  mprintf(muderr, "%s(", frame->u.c.op->name);

  for (i = 0; i < frame->u.c.nargs; i++)
    {
      if (i > 0) mputs(", ", muderr);
      mprint(muderr, prt_print, stack_get(frame->u.c.nargs - i - 1));
    }
  mputs(")" EOL, muderr);
}

static NORETURN void basic_error(runtime_errors error, int onstack)
{
#ifdef sparc
  /* Make sure that we have valid GC info */
  frame_start = catch_context->old_frame_start;
  frame_end = catch_context->old_frame_end;
#endif

  if (catch_context->display_error && muderr)
    {
      struct call_stack *scan;

      if (mudout) mflush(mudout);
      print_error(error);

      for (scan = call_stack; scan; scan = scan->next)
	{
	  switch (scan->type)
	    {
	    case call_c:
	      if (onstack) print_c_frame_stack(scan);
	      else print_c_frame(scan);
	      break;
	    case call_bytecode:
	      print_bytecode_frame(scan, onstack);
	      break;
	    case call_compiled:
	      mputs("<compiled>" EOL, muderr);
	      break;
	    }
	  /* Only the first frame can be on the stack */
	  onstack = FALSE;
	}
	
    }
  throw(SIGNAL_ERROR, makeint(error));
}

NORETURN void runtime_error(runtime_errors error)
/* Effects: Runtime error 'error' has occured. Dump the call_stack to
     mudout & throw back to the exception handler with SIGNAL_ERROR
     and the error code in exception_value.
   Note: Never returns
*/
{
  basic_error(error, FALSE);
}

NORETURN void early_runtime_error(runtime_errors error)
/* Effects: Runtime error 'error' has occured in a primitive operation. 
     Dump the call_stack (plus the primitive operation call) to
     mudout & throw back to the exception handler with SIGNAL_ERROR
     and the error code in exception_value.
     Call this function instead of runtime_error if the arguments of the
     function at the top of call_stack are still on the stack.
   Note: Never returns
*/
{
  basic_error(error, TRUE);
}

void error_init(void)
{
}
