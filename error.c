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

#include <stdlib.h>
#include <string.h>
#include <stddef.h>

#define AFTER_SKIP_FRAMES 16
#define BEFORE_SKIP_FRAMES 32

#include "runtime/runtime.h"
#include "runtime/stringops.h"

#include "builtins.h"
#include "context.h"
#include "alloc.h"
#include "error.h"
#include "ins.h"
#include "print.h"
#include "utils.h"


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
  "compilation error",
  "abort"
};
CASSERT_VLEN(mudlle_errors, last_runtime_error);

int suppress_extra_calltrace;

static void display_code_location(struct code *code, int lineno)
{
  GCPRO1(code);

  bool use_fname = !use_nicename || !TYPE(code->nicename, type_string);

  if (use_fname && TYPE(code->nicename, type_string)
      && !string_equalp(code->nicename, code->filename))
    {
      pputs(" [", muderr);
      output_value(muderr, prt_display, false, code->nicename);
      pputc(']', muderr);
    }

  pputs(" at ", muderr);
  output_value(muderr, prt_display, false,
               use_fname ? code->filename : code->nicename);
  pprintf(muderr, ":%d", lineno);

  UNGCPRO();
}

static void output_arg(value arg)
{
  output_value(muderr, prt_write, false, arg);
}

static int get_icode_line(struct call_stack *frame)
{
  struct icode *code = frame->u.mudlle.code;
  int offset = (frame->u.mudlle.offset - 1 -
                ((instruction *)(&code->constants[code->nb_constants]) -
                 (instruction *)code));
  return get_code_line_number(code, offset);
}

static void print_bytecode_frame(struct call_stack *frame, bool onstack)
{
  struct icode *fcode = frame->u.mudlle.code;
  GCPRO1(fcode);

  if (fcode->code.nicename->str[0])
    {
      if (fcode->code.varname)
	output_value(muderr, prt_display, false, fcode->code.varname);
      else
	pputs("<fn>", muderr);

      pputc('(', muderr);

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

	  if (i > 0) pputs(", ", muderr);
	  output_arg(v);
	}

      pputc(')', muderr);

      int lineno = get_icode_line(frame);

      display_code_location(&fcode->code, lineno);
      pputc('\n', muderr);
    }

  UNGCPRO();
}

struct c_info {
  const char *name;
  int formal_args, actual_args;
  bool is_vararg, is_operator;
};

static struct c_info c_frame_info(const struct call_stack *frame)
{
  const struct primitive_ext *op;
  switch (frame->type)
    {
    case call_string:
      return (struct c_info){ .name = frame->u.c.u.name };
    case call_c:
      op = frame->u.c.u.prim->op;
      break;
    case call_primop:
      op = frame->u.c.u.op;
      break;
    default:
      abort();
    }
  return (struct c_info){
    .name = op->name,
    .is_vararg = op->nargs == NVARARGS,
    .is_operator = op->flags & OP_OPERATOR,
    .formal_args = op->nargs
  };
}

static value get_c_stack_arg(const struct call_stack *frame,
                             const struct c_info *info, int arg)
{
  return stack_get(info->actual_args - arg - 1);
}

static value get_c_vararg_arg(const struct call_stack *frame,
                              const struct c_info *info, int arg)
{
  return ((struct vector *)frame->u.c.args[0])->data[arg];
}

static value get_c_arg(const struct call_stack *frame,
                       const struct c_info *info, int arg)
{
  return frame->u.c.args[arg];
}

static void print_c_frame(const struct call_stack *frame, bool onstack)
{
  struct c_info info = c_frame_info(frame);
  value (*getarg)(const struct call_stack *, const struct c_info *info, int);

  info.actual_args = frame->u.c.nargs;
  if (info.is_vararg)
    {
      assert(!onstack);
      assert(info.actual_args == 1 && TYPE(frame->u.c.args[0], type_vector));
      info.actual_args = vector_len((struct vector *)frame->u.c.args[0]);
      getarg = get_c_vararg_arg;
    }
  else
    getarg = onstack ? get_c_stack_arg : get_c_arg;

  if (info.actual_args == info.formal_args && info.is_operator)
    {
      bool is_set = false, is_ref = false;
      switch (info.actual_args)
        {
        case 1:
          pputs(strcmp("negate", info.name) == 0 ? "-" : info.name, muderr);
          output_arg(getarg(frame, &info, 0));
          goto done;
        case 3:
          is_set = strcmp(info.name, "set!") == 0;
          assert(is_set);
        case 2:
          is_ref = strcmp(info.name, "ref") == 0;
          output_arg(getarg(frame, &info, 0));
          if (is_ref || is_set)
            pputc('[', muderr);
          else
            pprintf(muderr, " %s ", info.name);
          output_arg(getarg(frame, &info, 1));
          if (is_ref || is_set)
            pputc(']', muderr);
          if (is_set)
            {
              pputs(" = ", muderr);
              output_arg(getarg(frame, &info, 2));
            }
          goto done;
        default:
          abort();
        }
    }

  pprintf(muderr, "%s(", info.name);
  const char *prefix = "";
  for (int i = 0; i < info.actual_args; ++i)
    {
      pputs(prefix, muderr);
      output_arg(getarg(frame, &info, i));
      prefix = ", ";
    }
  pputc(')', muderr);
 done:
  pputc('\n', muderr);
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

/* nargs < 0 means *args points to an argument vector */
static void print_mcode(struct mcode *base, ulong ofs, value *args, int nargs,
                        void *data)
{
  long line = find_line(base, ofs);

  GCPRO1(base);
  if (base->code.varname)
    output_value(muderr, prt_display, false, base->code.varname);
  else
    pputs("<fn>", muderr);

  bool vararg = nargs < 0;
  if (vararg)
    nargs = vector_len((struct vector *)*args);

  pputc('(', muderr);
  const char *prefix = "";
  for (int n = 0; n < nargs; ++n)
    {
      pputs(prefix, muderr);
      value arg = vararg ? ((struct vector *)*args)->data[n] : args[n];
      output_arg(arg);
      prefix = ", ";
    }

  pputc(')', muderr);
  display_code_location(&base->code, line);
  pputs(" [c]\n", muderr);
  UNGCPRO();
}

static void print_prim(const struct primitive_ext *op)
{
  pprintf(muderr, "%s(<compiled>)\n", op->name);
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
static int find_mcode(ulong pcadr,
                      void (*func)(struct mcode *, ulong ofs, value *args,
                                   int nargs, void *data),
                      const struct primitive_ext *last_primop,
                      void (*primfunc)(const struct primitive_ext *),
                      ulong *bp,
                      int nargs,
                      void *data)
{
  ulong *pc = (ulong *)(pcadr & ~(CODE_ALIGNMENT - 1));

  /* Check for moving code */
  if (!((ubyte *)pc >= gcblock && (ubyte *)pc < gcblock + gcblocksize))
    return false;

  /* We have found a code object.
     First find where it begins by locating its
     magic sequence */
  while (pc[-1] != 0xffffffff || pc[-2] != 0xffffffff)
    pc = (ulong *)((ulong)pc - CODE_ALIGNMENT);

  ulong mcode_addr = (ulong)pc - offsetof(struct mcode, mcode);

  struct mcode *mcode = (struct mcode *)mcode_addr;
  /* n.b., return address may be to after the last instruction as it
     could be a call to berror_xxx */
  assert(pcadr >= (ulong)pc && pcadr <= (ulong)pc + mcode->code_length);

  /* Print it */
  if (primfunc)
    {
      /* check for relative call to primitive */
      const ubyte *op = (const ubyte *)(pcadr - 5);
      if (op[0] == 0xe8)
        {
          ulong primadr = pcadr + *(long *)(op + 1);
          const struct primitive_ext *prim;
          if (primadr == (ulong)bcall_secure
              /* mov $imm,%edx */
              && op[-10] == (0xb8 | 2))
            {
              struct primitive *p = *(struct primitive **)(op - 9);
              prim = p->op;
            }
          else
            {
              if (primadr == (ulong)bapply_varargs
                  /* mov $imm,%ecx */
                  && op[-5] == (0xb8 | 1))
                primadr = *(ulong *)(op - 4);
              prim = lookup_primitive(primadr);
            }
          if (prim && prim != last_primop)
            primfunc(prim);
        }
    }

  value *args = (value *)bp + 2;
  if (nargs >= 0)
    ;
  else if (mcode->code.arg_types == NULL)
    {
      /* vararg argument vector is the first spilled local */
      args = (value *)bp - 1;
      assert(TYPE((struct vector *)*args, type_vector));
    }
  else
    nargs = vector_len(mcode->code.arg_types);

  func(mcode, pcadr - (ulong)pc, args, nargs, data);

  return true;
}

static struct ccontext *iterate_cc_frame(
  struct ccontext *cc,
  void (*func)(struct mcode *mcode, ulong ofs, value *args, int nargs,
               void *data),
  const struct primitive_ext *last_primop,
  void (*primfunc)(const struct primitive_ext *),
  int nargs,
  void *data)
{
  assert(cc->frame_start);

  ulong *sp, *bp;
  ccontext_frame(cc, &bp, &sp);

  /* The return address is at sp[-1] */
  find_mcode(sp[-1], func, last_primop, primfunc, bp, nargs, data);

  int count = 0;
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
	      pprintf(muderr, "   *** %d frame%s skipped ***\n", i,
		      i == 1 ? "" : "s");
	      bp = frames[i % AFTER_SKIP_FRAMES];
	    }
	}

      /* bp[<=-1] are locals
       * bp[-1] is the vararg vector (if applicable)
       * bp[0] is previous bp
       * bp[1] is return address
       * bp[>=2] are arguments
       * sp[0] -> bp[-2] is mudlle values
       */
      /* Not using closure because plan on removing it in some cases */
      find_mcode(bp[1], func, NULL, NULL, (ulong *)(bp[0]), -1, data);
      assert(bp[0] > (ulong)bp);
      bp = (ulong *)bp[0];
    }

  assert(bp == cc->frame_start);

  /* see START_INVOKE: ebx, esi, edi, and the non-union field of
     struct call_stack are pushed above this frame's ccontext */
  return (struct ccontext *)((char *)bp - (12 + sizeof *cc + 8));
}

static struct ccontext *print_cc_frame(
  struct ccontext *cc,
  const struct primitive_ext *last_primop,
  int nargs)
{
  return iterate_cc_frame(cc, print_mcode, last_primop, print_prim, nargs,
                          NULL);
}
#endif /* i386 && !NOCOMPILER */

#if !defined(USE_CCONTEXT) && !defined(NOCOMPILER)
static struct ccontext *print_cc_frame(struct ccontext *cc)
{
  pputs("<compiled>\n", muderr);
  return cc;
}
#endif

static void print_call_trace(runtime_errors error, bool onstack,
                             int c_onstack_nargs)
{
  struct catch_context *catch_ctxt = catch_context;
  int count = 0;
#ifndef NOCOMPILER
  struct ccontext *cc = &ccontext;
#endif

  if (error == error_none)
    ;
  else if (error < last_runtime_error && error >= 0)
    pprintf(muderr, "%s\n", mudlle_errors[error]);
  else
    pprintf(muderr, "error %d\n", error);
  pputs("Call trace is:\n", muderr);

#ifndef NOCOMPILER
  const struct primitive_ext *last_primop = NULL;
#endif
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
	      pprintf(muderr, "   *** %d frame%s skipped ***\n", i,
		      i == 1 ? "" : "s");
	      scan = scans[i % AFTER_SKIP_FRAMES];
	    }
	}

#ifndef NOCOMPILER
      const struct primitive_ext *this_primop = NULL;
#endif
      switch (scan->type)
	{
        case call_primop:
#ifndef NOCOMPILER
          this_primop = scan->u.c.u.op;
#endif
          /* fallthrough */
        case call_string:
	case call_c:
	  print_c_frame(scan, onstack);
	  break;
	case call_bytecode:
	  print_bytecode_frame(scan, onstack);
	  break;
	case call_compiled:
#ifdef NOCOMPILER
	  abort();
#else
	  cc = print_cc_frame(cc, last_primop, c_onstack_nargs);
	  break;
#endif
	}
#ifndef NOCOMPILER
      last_primop = this_primop;
#endif
      /* Only the first frame can be on the stack */
      onstack = false;
      c_onstack_nargs = -1;
    }
}

#ifndef NOCOMPILER
static void count_stack_depth(struct mcode *mcode, ulong ofs, value *args,
                              int nargs, void *data)
{
  ++*(int *)data;
}

struct get_cc_stack_trace_data {
  struct vector *vec;
  int idx;
  bool lines;
};

static void get_cc_stack_trace(struct mcode *mcode, ulong ofs, value *args,
                               int nargs, void *data)
{
  struct get_cc_stack_trace_data *sdata = data;
  value v = mcode;
  if (sdata->lines)
    v = alloc_list(v, makeint(find_line(mcode, ofs)));
  sdata->vec->data[sdata->idx++] = v;
}
#endif

struct vector *get_mudlle_call_trace(bool lines)
{
#ifndef NOCOMPILER
  struct ccontext *cc = &ccontext;
#endif

  int depth = 0;

  for (struct call_stack *scan = call_stack; scan; scan = scan->next)
#ifndef NOCOMPILER
    if (scan->type == call_compiled)
      cc = iterate_cc_frame(cc, count_stack_depth, NULL, NULL, -1, &depth);
    else
#endif
      ++depth;

  struct get_cc_stack_trace_data sdata = {
    .vec   = alloc_vector(depth),
    .idx   = 0,
    .lines = lines
  };

  GCPRO1(sdata.vec);

#ifndef NOCOMPILER
  cc = &ccontext;
#endif

  for (struct call_stack *scan = call_stack; scan; scan = scan->next)
    {
      value v;
      switch (scan->type)
	{
        case call_string:
          v = alloc_string(scan->u.c.u.name);
	  goto ok;
        case call_primop:
          v = alloc_string(scan->u.c.u.op->name);
	  goto ok;
	case call_c:
	  v = scan->u.c.u.prim;
	  goto ok;
	case call_bytecode:
          {
            v = scan->u.mudlle.fn;
            if (lines)
              v = alloc_list(v, makeint(get_icode_line(scan)));
            sdata.vec->data[sdata.idx++] = v;
            continue;
          }
	case call_compiled:
#ifdef NOCOMPILER
	  abort();
#else
	  cc = iterate_cc_frame(cc, get_cc_stack_trace, NULL, NULL, -1,
                                &sdata);
          continue;
#endif
	}
      abort();
    ok:
      if (lines)
        v = alloc_list(v, NULL);
      sdata.vec->data[sdata.idx++] = v;
    }
  UNGCPRO();

  assert(sdata.idx == depth);

  return sdata.vec;
}

/* call f(e, data) for all error observers e (port or character), with
   muderr set to the appropriate value */
static void for_all_muderr(void (*f)(value e, void *data), void *data)
{
  bool send_to_muderr = (catch_context->call_trace_mode != call_trace_off
                         && muderr);
  if (send_to_muderr)
    {
      if (mudout) pflush(mudout);
      f(muderr, data);
      if (suppress_extra_calltrace)
        return;
    }

  struct list *l = mudcalltrace;
  if (l == NULL)
    return;

  struct list *elem = NULL, *prev = NULL;
  struct oport *omuderr = muderr;

  GCPRO4(omuderr, l, elem, prev);

  for (; l != NULL; l = l->cdr)
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
          continue;
        }

      /* ignore elem if elem has already seen the call trace */
      if (send_to_muderr && muderr == omuderr)
        continue;

      /* if cdr(elem), ignore call traces that are handled otherwise */
      if (istrue(elem->cdr) && omuderr)
        continue;

      f(elem->car, data);
      pflush(muderr);

      prev = l;
    }

  muderr = omuderr;
  UNGCPRO();
}

struct basic_error_info {
  runtime_errors error;
  bool onstack;
  int c_onstack_nargs;
  const char *warning;
};

static void basic_error_print(value e, void *data)
{
  const struct basic_error_info *info = data;

  if (info->warning)
    pprintf(muderr, "warning: %s\n", info->warning);
  print_call_trace(info->error, info->onstack, info->c_onstack_nargs);
  if (TYPE(e, type_character))
    pputc('\n', muderr);
}

static void basic_error(runtime_errors error, bool onstack,
                        int c_onstack_nargs, const char *msg)
{
  struct basic_error_info info = {
    .error           = error,
    .onstack         = onstack,
    .c_onstack_nargs = c_onstack_nargs,
    .warning         = msg
  };
  for_all_muderr(basic_error_print, &info);
}

void runtime_warning(const char *msg)
{
#ifdef MUDLLE_INTERRUPT
  check_interrupt();
#endif
  basic_error(error_none, false, -1, msg);
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
  basic_error(error, false, -1, NULL);
  mthrow(SIGNAL_ERROR, makeint(error));
}

void compiled_early_runtime_error(runtime_errors error, int nargs)
{
  basic_error(error, true, nargs, NULL);
  mthrow(SIGNAL_ERROR, makeint(error));
}

void interpreted_early_runtime_error(runtime_errors error)
/* Effects: Runtime error 'error' has occured in a primitive operation.
     Dump the call_stack (plus the primitive operation call) to
     mudout & throw back to the exception handler with SIGNAL_ERROR
     and the error code in exception_value.
     Call this function instead of runtime_error if the arguments of the
     function at the top of call_stack are still on the stack.
   Note: Never returns
*/
{
  basic_error(error, true, -1, NULL);
  mthrow(SIGNAL_ERROR, makeint(error));
}

void primitive_runtime_error(runtime_errors error,
                             const struct primitive_ext *op,
                             int nargs, ...)
{
  /* prevent duplicate entries */
  if (call_stack && call_stack->type == call_c
      && call_stack->u.c.u.prim->op == op)
    runtime_error(error);

  struct call_stack me = {
    .next = call_stack,
    .type = call_primop,
    .u.c = {
      .u.op = op,
      .nargs = nargs
    }
  };
  va_list va;
  va_start(va, nargs);
  for (int i = 0; i < nargs; ++i)
    me.u.c.args[i] = va_arg(va, value);
  va_end(va);

  call_stack = &me;
  runtime_error(error);
}

void error_init(void)
{
}
