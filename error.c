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

#include <stdlib.h>
#include <string.h>
#include <stddef.h>

#include "runtime/runtime.h"
#include "mudio.h"
#include "print.h"
#include "alloc.h"
#include "error.h"

const char *mudlle_errors[last_runtime_error] = {
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
  "security violation",
  "value is read only",
  "user interrupt",
  "pattern not matched"
};

static void print_error(int error)
{
  if (error < last_runtime_error && error >= 0)
    mprintf(muderr, "%s" EOL, mudlle_errors[error]);
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

	      v = argi->vvalue;
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

static void print_mcode(struct mcode *base)
{
  struct gcpro gcpro1;

  GCPRO1(base);
  if (base->varname)
    mprint(muderr, prt_display, base->varname);
  else mputs("<fn>", muderr);
  mputs("[", muderr);
  mprint(muderr, prt_display, base->filename);
  mprintf(muderr, ":%d](<compiled>)" EOL, base->lineno);
  UNGCPRO();
}

#ifdef sparc
static void print_pc(ulong _pc)
{
  ulong *pc = (ulong *)_pc;

  /* Check for moving code */
  if ((ubyte *)pc >= gcblock && (ubyte *)pc < gcblock + gcblocksize)
    {
      /* We have found a code object.
	 First find where it begins by locating its
	 magic sequence */
      while (pc[0] != 0xffffffff ||
	     pc[-1] != 0xffffffff) pc--;

      /* Print it */
      print_mcode((struct mcode *)((char *)pc - 4 -
				   offsetof(struct mcode, magic)));

    }
}

static struct ccontext *print_cc_frame(struct ccontext *cc)
{
  ulong *frame = cc->frame_end, *start = cc->frame_start;
  static struct ccontext next;

  print_pc(frame[1]); /* first frame pc lives in l1... */

  while (frame != start)
    {
      /*assert(frame < start);*/
      /* Grr. Why doesn't assert() make a bloody core file ? */
      if (frame >= start) abort();
      print_pc(frame[15]); /* ret adr, i.e. caller's frame pc is in i7 */
      frame = (ulong *)frame[14]; /* next frame in i6 */
    }
  
  
  /* Get link to next set of frames */
  /* This will have been left in l3/l2 by mc_invoke, so: */
  next.frame_start = (ulong *)frame[3];
  next.frame_end = (ulong *)frame[2];
  return &next;
}
#endif

#ifdef i386
static int print_pc(ulong _pc)
{
  ulong *pc = (ulong *)ALIGN(_pc, 4);

  /* Check for moving code */
  if (!((ubyte *)pc >= gcblock && (ubyte *)pc < gcblock + gcblocksize))
    return FALSE;

  /* We have found a code object.
     First find where it begins by locating its
     magic sequence */
  while (pc[0] != 0xffffffff ||
	 pc[-1] != 0xffffffff) pc--;

  /* Print it */
  print_mcode((struct mcode *)((char *)pc - 4 -
			       offsetof(struct mcode, magic)));

  return TRUE;
}

static struct ccontext *print_cc_frame(struct ccontext *cc)
{
  ulong *sp, *bp;

  assert(cc->frame_start);

  sp = cc->frame_end_sp;
  bp = cc->frame_end_bp;
  assert(bp >= sp && cc->frame_start > sp && bp != cc->frame_start);

  /* The return address is sometimes in retadr, sometimes at sp[-1] */
  if (!print_pc(sp[-1]))
    print_pc(cc->retadr);

  while (bp < cc->frame_start)
    {
      /* bp[-1] is caller's closure
       * bp[0] is previous bp
       * bp[1] is return address
       * sp[0] -> bp[-2] is mudlle values
       */
      /* Not using closure because plan on removing it in some cases */
      print_pc(bp[1]);
      assert(bp[0] > (ulong)bp);
      bp = (ulong *)bp[0];
    }

  assert(bp == cc->frame_start);

  return (struct ccontext *)((char *)bp - (12 + sizeof *cc + 8));
}
#endif

#ifndef USE_CCONTEXT
static struct ccontext *print_cc_frame(struct ccontext *cc)
{
  mputs("<compiled>" EOL, muderr);
  return cc;
}
#endif

static void basic_error(runtime_errors error, int onstack) NORETURN;

static void basic_error(runtime_errors error, int onstack)
{
  if (catch_context->display_error && muderr)
    {
      struct call_stack *scan;
      struct ccontext *cc = &ccontext;

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
	      cc = print_cc_frame(cc);
	      break;
	    }
	  /* Only the first frame can be on the stack */
	  onstack = FALSE;
	}
	
    }
  mthrow(SIGNAL_ERROR, makeint(error));
}

void runtime_error(runtime_errors error)
/* Effects: Runtime error 'error' has occured. Dump the call_stack to
     mudout & throw back to the exception handler with SIGNAL_ERROR
     and the error code in exception_value.
   Note: Never returns
*/
{
  basic_error(error, FALSE);
}

void early_runtime_error(runtime_errors error)
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
