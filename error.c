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

#include <stdlib.h>
#include <string.h>
#include <stddef.h>

#define AFTER_SKIP_FRAMES 16
#define BEFORE_SKIP_FRAMES 32

#include "runtime/runtime.h"

#include "context.h"
#include "alloc.h"
#include "error.h"
#include "ins.h"


const char *const mudlle_errors[] = {
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
  "pattern not matched",
  "compilation error"
};
CASSERT_VLEN(mudlle_errors, last_runtime_error);

int suppress_extra_calltrace;

static void print_bytecode_frame(struct call_stack *frame, bool onstack)
{
  struct code *fcode = frame->u.mudlle.fn->code;
  struct gcpro gcpro1;

  GCPRO1(fcode);

  if (fcode->filename->str[0])
    {
      if (fcode->varname)
	mprint(muderr, prt_display, fcode->varname);
      else
	mputs("<fn>", muderr);

      mputs("(", muderr);

      /* Warning: This is somewhat intimate with the
	 implementation of the compiler */
      for (int i = 0; i < frame->u.mudlle.nargs; i++)
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

      mputs(") at ", muderr);
      mprint(muderr, prt_display, fcode->filename);
      
      struct code *code = frame->u.mudlle.code;
      int offset = (frame->u.mudlle.offset - 1 -
                    ((instruction *)(&code->constants[code->nb_constants]) -
                     (instruction *)code));
      mprintf(muderr, ":%d" EOL, get_code_line_number(fcode, offset));
    }

  UNGCPRO();
}

static void print_c_frame(struct call_stack *frame)
{
  mprintf(muderr, "%s(", frame->u.c.prim->op->name);

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
  mprintf(muderr, "%s(", frame->u.c.prim->op->name);

  for (int i = 0; i < frame->u.c.nargs; i++)
    {
      if (i > 0) mputs(", ", muderr);
      mprint(muderr, prt_print, stack_get(frame->u.c.nargs - i - 1));
    }
  mputs(")" EOL, muderr);
}

#ifndef NOCOMPILER
/*
 * Looks up (return) address ofs in mcode and returns which line
 * number it comes from.
 *
 * The line number data is stored in offset/linenumber pairs, where
 * by default, only a delta is stored, one byte for each value:
 *
 *   <delta offset> <delta line>
 *
 * Which means that <delta offset> bytes of code exist on the current
 * source line, and that the next line has line number <current line>
 * + <delta line>.
 *
 * If <delta offset> doesn't fit in an unsigned byte (except 255), or
 * if <delta line> doesn't fit in a signed byte, a byte of 255 is
 * stored, followed by two 32-bit ints for <new offset> and <new line>.
 *
 * See ins_lineno() in ax86.mud for the generation of this data.
 */
static long find_line(struct mcode *mcode, ulong ofs)
{
  char *data = mcode->linenos->str;
  char *enddata = data + string_len(mcode->linenos);
  long offset = 0;
  long line = 0;

  for (;;)
    {
      long prevline = line;
      char b = *data++;

      if ((unsigned char)b == 255)
        {
          offset = *(unsigned int *)data;
          data += 4;
          line = *(unsigned int *)data;
          data += 4;
        }
      else
        {
          offset += (unsigned char)b;
          line += (signed char)*data++;
        }
      if (offset >= ofs)
        return prevline;
      if (data == enddata)
        return line;
    }
}

static void print_mcode(struct mcode *base, ulong ofs)
{
  struct gcpro gcpro1;
  long line = find_line(base, ofs);

  GCPRO1(base);
  if (base->varname)
    mprint(muderr, prt_display, base->varname);
  else
    mputs("<fn>", muderr);

  mputs("(<compiled>) at ", muderr);
  mprint(muderr, prt_display, base->filename);
  mprintf(muderr, ":%ld" EOL, line);
  UNGCPRO();
}
#endif /* !NOCOMPILER */

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

#ifndef NOCOMPILER
static struct ccontext *print_cc_frame(struct ccontext *cc)
{
  iterate_cc_frame(cc, print_mcode_frame);

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
#endif /* !NOCOMPILER */
#endif /* sparc */

#if defined(i386) && !defined(NOCOMPILER)
static int find_mcode(ulong _pc, void (*func)(struct mcode *, ulong ofs))
{
  ulong *pc = (ulong *)ALIGN(_pc, 4);

  /* Check for moving code */
  if (!((ubyte *)pc >= gcblock && (ubyte *)pc < gcblock + gcblocksize))
    return false;

  /* We have found a code object.
     First find where it begins by locating its
     magic sequence */
  while (pc[0] != 0xffffffff ||
	 pc[-1] != 0xffffffff) pc--;

  ++pc;
  /* Print it */
  func((struct mcode *)((char *)pc - offsetof(struct mcode, mcode)),
       _pc - (ulong)pc);

  return true;
}

static struct ccontext *iterate_cc_frame(struct ccontext *cc,
					 void (*func)(struct mcode *,
                                                      ulong))
{
  ulong *sp, *bp;
  int count = 0;

  assert(cc->frame_start);

  sp = cc->frame_end_sp;
  bp = cc->frame_end_bp;
  assert(bp >= sp && cc->frame_start > sp && bp != cc->frame_start);

  /* The return address is sometimes in retadr, sometimes at sp[-1] */
  if (!find_mcode(sp[-1], func))
    find_mcode(cc->retadr, func);

  while (bp < cc->frame_start)
    {
      if (count++ == BEFORE_SKIP_FRAMES)
	{
	  ulong *frames[AFTER_SKIP_FRAMES];
	  int i = 0;

	  while (bp < cc->frame_start && i < AFTER_SKIP_FRAMES)
	    {
	      frames[i++] = bp;
	      bp = (ulong *)bp[0];
	    }
	  if (bp >= cc->frame_start)
	    bp = frames[0];
	  else
	    {
	      i = 0;
	      while (bp < cc->frame_start)
		{
		  frames[i++ % AFTER_SKIP_FRAMES] = bp;
		  bp = (ulong *)bp[0];
		}
	      mprintf(muderr, "   *** %d frame%s skipped ***" EOL, i,
		      i == 1 ? "" : "s");
	      bp = frames[i % AFTER_SKIP_FRAMES];
	    }
	}

      /* bp[-1] is caller's closure
       * bp[0] is previous bp
       * bp[1] is return address
       * sp[0] -> bp[-2] is mudlle values
       */
      /* Not using closure because plan on removing it in some cases */
      find_mcode(bp[1], func);
      assert(bp[0] > (ulong)bp);
      bp = (ulong *)bp[0];
    }

  assert(bp == cc->frame_start);

  return (struct ccontext *)((char *)bp - (12 + sizeof *cc + 8));
}

static struct ccontext *print_cc_frame(struct ccontext *cc)
{
  return iterate_cc_frame(cc, print_mcode);
}
#endif /* i386 && !NOCOMPILER */

#if !defined(USE_CCONTEXT) && !defined(NOCOMPILER)
static struct ccontext *print_cc_frame(struct ccontext *cc)
{
  mputs("<compiled>" EOL, muderr);
  return cc;
}
#endif

static void print_call_trace(runtime_errors error, bool onstack)
{
  struct catch_context *catch_ctxt = catch_context;
  int count = 0;
#ifndef NOCOMPILER
  struct ccontext *cc = &ccontext;
#endif

  if (error == error_none)
    ;
  else if (error < last_runtime_error && error >= 0)
    mprintf(muderr, "%s" EOL, mudlle_errors[error]);
  else
    mprintf(muderr, "error %d" EOL, error);
  mprintf(muderr, "Call trace is:" EOL);

  for (struct call_stack *scan = call_stack; scan; scan = scan->next)
    {
      /* not sure if 'while' is necessary here... */
      while (catch_ctxt && scan == catch_ctxt->old_call_stack)
        {
          if (catch_ctxt->call_trace_mode == call_trace_barrier)
            return;
          catch_ctxt = catch_ctxt->parent;
        }

      if (count++ == BEFORE_SKIP_FRAMES)
	{
	  struct call_stack *scans[AFTER_SKIP_FRAMES];

	  for (int i = 0;
               i < AFTER_SKIP_FRAMES && scan;
               ++i, scan = scan->next)
	    scans[i] = scan;
	  if (scan == NULL)
	    scan = scans[0];
	  else
	    {
              int i;
	      for (i = 0; scan; scan = scan->next)
		scans[i++ % AFTER_SKIP_FRAMES] = scan;
	      mprintf(muderr, "   *** %d frame%s skipped ***" EOL, i,
		      i == 1 ? "" : "s");
	      scan = scans[i % AFTER_SKIP_FRAMES];
	    }	  
	}

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
#ifdef NOCOMPILER
	  abort();
#else
	  cc = print_cc_frame(cc);
	  break;
#endif
	}
      /* Only the first frame can be on the stack */
      onstack = false;
    }
}	

static long stack_depth_count;
static struct vector *stack_trace_res;

#ifndef NOCOMPILER
static void count_stack_depth(struct mcode *mcode, ulong ofs)
{
  ++stack_depth_count;
}

static void get_cc_stack_trace(struct mcode *mcode, ulong ofs)
{
  stack_trace_res->data[stack_depth_count++] = mcode;
}
#endif

struct vector *get_mudlle_call_trace(void)
{
#ifndef NOCOMPILER
  struct ccontext *cc = &ccontext;
#endif

  stack_depth_count = 0;

  for (struct call_stack *scan = call_stack; scan; scan = scan->next)
#ifndef NOCOMPILER
    if (scan->type == call_compiled)
      cc = iterate_cc_frame(cc, count_stack_depth);
    else
#endif
      ++stack_depth_count;

  stack_trace_res = alloc_vector(stack_depth_count);

  struct gcpro gcpro1;
  GCPRO1(stack_trace_res);

#ifndef NOCOMPILER
  cc = &ccontext;
#endif

  stack_depth_count = 0;
  for (struct call_stack *scan = call_stack; scan; scan = scan->next)
    {
      switch (scan->type)
	{
	case call_c:
	  stack_trace_res->data[stack_depth_count++] = scan->u.c.prim;
	  break;
	case call_bytecode:
	  stack_trace_res->data[stack_depth_count++] = scan->u.mudlle.fn;
	  break;
	case call_compiled:
#ifdef NOCOMPILER
	  abort();
#else
	  cc = iterate_cc_frame(cc, get_cc_stack_trace);
	  break;
#endif
	}
    }
  UNGCPRO();

  return stack_trace_res;
}

/* call f(e, data) for all error observers e (port or character), with
   muderr set to the appropriate value */
static void for_all_muderr(void (*f)(value e, void *data), void *data)
{
  bool seen = false;
  if (catch_context->call_trace_mode != call_trace_off && muderr)
    {
      if (mudout) mflush(mudout);
      f(muderr, data);
      seen = true;
    }

  if (mudcalltrace && (!seen || !suppress_extra_calltrace))
    {
      struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
      struct list *l = mudcalltrace;
      struct list *elem = NULL, *prev = NULL;
      Mio omuderr;

      omuderr = muderr;
      GCPRO2(omuderr, l);
      GCPRO(gcpro3, elem);
      GCPRO(gcpro4, prev);

      while (l)
	{
	  elem = l->car;
	  assert(TYPE(elem, type_pair));

	  if (TYPE(elem->car, type_outputport))
	    muderr = elem->car;
	  else
	    {
	      if (prev == NULL)
		mudcalltrace = l->cdr;
	      else
		prev->cdr = l->cdr;
	      goto nevermind;
	    }

          /* ignore elem if elem has already seen the call trace */
	  if (seen && muderr == omuderr)
	    goto nevermind;

          /* if cdr(elem), ignore call traces that are handled otherwise */
	  if (istrue(elem->cdr) && (seen || omuderr))
	    goto nevermind;

          f(elem->car, data);
	  pflush(muderr);

	  prev = l;

	nevermind:
	  l = l->cdr;
	}

      muderr = omuderr;
      UNGCPRO();
    }
}

struct basic_error_info {
  runtime_errors error;
  bool onstack;
  const char *message;
};

static void basic_error_print(value e, void *data)
{
  struct basic_error_info *info = data;

  if (info->message)
    mprintf(muderr, "%s" EOL, info->message);
  print_call_trace(info->error, info->onstack);
  if (TYPE(e, type_character))
    mputs(EOL, muderr);
}

static void basic_error(runtime_errors error, bool onstack, const char *msg)
{
  struct basic_error_info info = {
    .error   = error,
    .onstack = onstack,
    .message = msg
  };
  for_all_muderr(basic_error_print, &info);
}

void runtime_warning(const char *msg)
{
#ifdef MUDLLE_INTERRUPT
  check_interrupt();
#endif
  basic_error(error_none, false, msg);
}

void runtime_error(runtime_errors error)
/* Effects: Runtime error 'error' has occured. Dump the call_stack to
     mudout & throw back to the exception handler with SIGNAL_ERROR
     and the error code in exception_value.
   Note: Never returns
*/
{
#ifdef MUDLLE_INTERRUPT
  check_interrupt();
#endif
  basic_error(error, false, NULL);
  mthrow(SIGNAL_ERROR, makeint(error));
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
  basic_error(error, true, NULL);
  mthrow(SIGNAL_ERROR, makeint(error));
}

void error_init(void)
{
}
