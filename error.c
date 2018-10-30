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

#include <stdlib.h>
#include <string.h>
#include <stddef.h>

#define AFTER_SKIP_FRAMES 16
#define BEFORE_SKIP_FRAMES 32

#include "builtins.h"
#include "context.h"
#include "alloc.h"
#include "dwarf.h"
#include "error.h"
#include "global.h"
#include "ins.h"
#include "print.h"
#include "strbuf.h"
#include "stack.h"
#include "utils.h"

#include "runtime/arith.h"
#include "runtime/basic.h"
#include "runtime/mudlle-string.h"
#include "runtime/runtime.h"
#include "runtime/support.h"


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

static void display_code_location(struct code *code, uint32_t lineno)
{
  GCPRO(code);

  bool use_fname = !use_nicename || !TYPE(code->nicename, string);

  if (use_fname && TYPE(code->nicename, string)
      && !string_equalp(code->nicename, code->filename))
    {
      pputs(" [", muderr);
      output_value(muderr, prt_display, code->nicename);
      pputc(']', muderr);
    }

  pputs(" at ", muderr);
  output_value(muderr, prt_display,
               use_fname ? code->filename : code->nicename);
  pprintf(muderr, ":%lu", (ulong)lineno);

  UNGCPRO();
}

static void output_arg(value arg)
{
  output_value(muderr, prt_write, arg);
}

static uint32_t get_icode_line(struct call_stack_mudlle *frame)
{
  struct icode *code = frame->code;
  uint32_t offset = (frame->offset - 1
                     - ((uint8_t *)(&code->constants[code->nb_constants])
                        - (uint8_t *)code));
  return dwarf_lookup_line_number(&code->code, offset);
}

static void print_arg_name(struct vector *argv, int i)
{
  if (argv == NULL)
    return;

  struct list *e = argv->data[i];
  assert(TYPE(e, pair));
  struct string *name = e->car;
  if (TYPE(name, string))
    {
      pswrite(muderr, name);
      pputc('=', muderr);
    }
}

static struct vector *maybe_get_arguments(struct code *code, int nargs)
{
  if (code_is_vararg(code))
    return NULL;
  struct vector *v = code->arguments.argv;
  return vector_len(v) == nargs ? v : NULL;
}

static void print_bytecode_frame(struct call_stack *frame, bool onstack)
{
  struct call_stack_mudlle *mframe = (struct call_stack_mudlle *)frame;
  struct icode *fcode = mframe->code;
  GCPRO(fcode);

  if (fcode->code.varname)
    output_value(muderr, prt_display, fcode->code.varname);
  else
    pputs("<fn>", muderr);

  pputc('(', muderr);

  struct vector *arguments = maybe_get_arguments(&fcode->code, mframe->nargs);

  for (int i = 0; i < mframe->nargs; i++)
    {
      value v;

      /* Warning: This is somewhat intimate with the implementation of
         the compiler */
      if (onstack)
        v = stack_get(mframe->nargs - i - 1);
      else
        {
          struct variable *argi
            = mframe->locals->data[mframe->nargs - i - 1];
          v = argi->vvalue;
        }

      if (i > 0)
        pputs(", ", muderr);
      print_arg_name(arguments, i);
      output_arg(v);
    }

  pputc(')', muderr);

  uint32_t lineno = get_icode_line((struct call_stack_mudlle *)frame);
  display_code_location(&fcode->code, lineno);
  pputc('\n', muderr);

  UNGCPRO();
}

static void print_called_value(value called)
{
  if (is_any_primitive(called))
    pputs(((struct primitive *)called)->op->name, muderr);
  else if (TYPE(called, closure))
    {
      struct code *code = ((struct closure *)called)->code;
      if (code->varname)
        output_value(muderr, prt_display, code->varname);
      else
        pputs("<fn>", muderr);
    }
  else
    output_arg(called);
}

struct c_info {
  const struct prim_op *op;
  const char *name;
  int formal_args, actual_args;
  bool is_vararg, is_operator;
};

static void c_frame_info(struct c_info *dst, const struct call_stack *frame)
{
  struct call_stack_c_header *cframe = (struct call_stack_c_header *)frame;
  const struct prim_op *op;
  switch (cframe->s.type)
    {
    case call_string:
      *dst = (struct c_info){ .name = cframe->u.name };
      return;
    case call_c:
      op = cframe->u.prim->op;
      break;
    case call_primop:
      op = cframe->u.op;
      break;
    case call_invalid:
    case call_invalid_argp:
      /* special handling in print_c_frame() */
      *dst = (struct c_info){ .name = NULL };
      return;
    default:
      abort();
    }
  *dst = (struct c_info){
    .op          = op,
    .name        = op->name,
    .is_vararg   = op->nargs == NVARARGS,
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
  struct call_stack_c *cframe = (struct call_stack_c *)frame;
  return ((struct vector *)cframe->args[0])->data[arg];
}

static value get_c_argp(const struct call_stack *frame,
                        const struct c_info *info, int arg)
{
  struct call_stack_c_argp *vframe = (struct call_stack_c_argp *)frame;
  return vframe->argp[arg];
}

static value get_c_arg(const struct call_stack *frame,
                       const struct c_info *info, int arg)
{
  struct call_stack_c *cframe = (struct call_stack_c *)frame;
  return cframe->args[arg];
}

static void print_prim_suffix(const struct prim_op *op,
                              const struct session_context *next_session)
{
}

static bool print_global_name(value midx)
{
  if (!integerp(midx))
    return false;
  long gidx = intval(midx);
  if (gidx <= 0 || gidx >= intval(environment->used))
    return false;
  pswrite(muderr, GNAME(gidx));
  return true;
}

static void print_c_frame(const struct call_stack *frame, bool onstack,
                          const struct prim_op *last_primop,
                          const struct session_context *next_session)
{
  struct call_stack_c *cframe = (struct call_stack_c *)frame;
  struct c_info info;
  c_frame_info(&info, frame);
  if (last_primop != NULL && info.op == last_primop)
    return;
  value (*getarg)(const struct call_stack *, const struct c_info *info, int);

  info.actual_args = cframe->c.nargs;
  if (cframe->c.s.type == call_invalid_argp)
    getarg = get_c_argp;
  else if (info.is_vararg)
    {
      assert(!onstack);
      assert(info.actual_args == 1 && TYPE(cframe->args[0], vector));
      info.actual_args = vector_len((struct vector *)cframe->args[0]);
      getarg = get_c_vararg_arg;
    }
  else
    getarg = onstack ? get_c_stack_arg : get_c_arg;

  if (info.actual_args == info.formal_args && info.is_operator)
    {
      bool is_set = false;
      switch (info.actual_args)
        {
        case 1:
          if (info.op == global_read_ext)
            {
              if (!print_global_name(getarg(frame, &info, 0)))
                goto not_op;
              goto done;
            }
          pputs(info.op == negate_prim_ext
                ? "-"
                : (info.op == dereference_prim_ext
                   ? "*"
                   : info.name),
                muderr);
          output_arg(getarg(frame, &info, 0));
          goto done;
        case 3:
          is_set = info.op == setb_prim_ext;
          assert(is_set);
          /* fallthrough */
        case 2: ;
          bool is_gset = info.op == global_write_ext;
          bool is_ref = info.op == ref_prim_ext;
          bool is_setref = info.op == set_refb_prim_ext;
          if (!is_gset)
            {
              if (is_setref)
                pputc('*', muderr);
              output_arg(getarg(frame, &info, 0));
            }
          else if (!print_global_name(getarg(frame, &info, 0)))
            goto not_op;
          if (!is_setref && !is_gset)
            {
              if (is_ref || is_set)
                pputc('[', muderr);
              else
                pprintf(muderr, " %s ", info.name);
              output_arg(getarg(frame, &info, 1));
              if (is_ref || is_set)
                pputc(']', muderr);
            }
          if (is_set || is_setref || is_gset)
            {
              pputs(" = ", muderr);
              output_arg(getarg(frame, &info, is_set ? 2 : 1));
            }
          goto done;
        default:
          abort();
        }
    }

 not_op:
  if (frame->type == call_invalid || frame->type == call_invalid_argp)
    print_called_value(cframe->c.u.value);
  else
    {
      assert(info.name != NULL);
      pputs(info.name, muderr);
    }
  pputc('(', muderr);
  const char *prefix = "";
  for (int i = 0; i < info.actual_args; ++i)
    {
      pputs(prefix, muderr);
      value arg = getarg(frame, &info, i);
      if (i == 0 && info.op != NULL && (info.op->flags & OP_NUL_STR)
          && TYPE(arg, string))
        write_nul_string(muderr, arg);
      else
        output_arg(arg);
      prefix = ", ";
    }
  pputc(')', muderr);
  if (info.op != NULL)
    print_prim_suffix(info.op, next_session);
 done:
  pputc('\n', muderr);
}

#ifndef NOCOMPILER
static void print_args(value *args, int nargs, bool nul_str,
                       struct vector *arguments)
{
  bool vararg = nargs < 0;
  if (vararg)
    nargs = vector_len((struct vector *)*args);

  GCPRO(arguments);
  pputc('(', muderr);
  const char *prefix = "";
  for (int n = 0; n < nargs; ++n)
    {
      pputs(prefix, muderr);
      print_arg_name(arguments, n);
      value arg = vararg ? ((struct vector *)*args)->data[n] : args[n];
      if (n == 0 && nul_str && TYPE(arg, string))
        write_nul_string(muderr, arg);
      else
        output_arg(arg);
      prefix = ", ";
    }
  UNGCPRO();

  pputc(')', muderr);
}

/* nargs < 0 means *args points to a gc-protected argument vector */
static void print_mcode(struct mcode *base, ulong ofs, value *args, int nargs,
                        void *data)
{
  /* address is the return address (or pc + 1 for segv) */
  if (ofs > 0)
    --ofs;
  uint32_t line = dwarf_lookup_line_number(&base->code, ofs);

  GCPRO(base);
  if (base->code.varname)
    output_value(muderr, prt_display, base->code.varname);
  else
    pputs("<fn>", muderr);

  struct vector *arguments = maybe_get_arguments(&base->code, nargs);
  print_args(args, nargs, false, arguments);

  pputs(" [c]", muderr);
  display_code_location(&base->code, line);
  pputc('\n', muderr);
  UNGCPRO();
}

static void print_prim(const struct prim_op *op,
                       const struct session_context *next_session,
                       value *args, int nargs)
{
  pputs(op->name, muderr);
  if (args == NULL)
    pputs("(<compiled>)", muderr);
  else
    print_args(args, nargs, op->flags & OP_NUL_STR, NULL);
  print_prim_suffix(op, next_session);
  pputc('\n', muderr);
}

static void print_any(value called, value *args, int nargs)
{
  print_called_value(called);
  print_args(args, nargs, false, NULL);
  pputc('\n', muderr);
}

#endif /* !NOCOMPILER */

#if (defined __i386__ || defined __x86_64__) && !defined(NOCOMPILER)

#ifdef __i386__
enum cpu_reg {
  x86_reg_eax = 0,
  x86_reg_ebx = 3,
  x86_reg_ecx = 1,
  x86_reg_edx = 2,
  x86_reg_esp = 4,
  x86_reg_ebp = 5,
  x86_reg_esi = 6,
  x86_reg_edi = 7,
  reg_argcount = x86_reg_eax,
  reg_closure_in = x86_reg_edx
};
#elif defined __x86_64__
enum cpu_reg {
  x64_reg_rax = 0,
  x64_reg_rbx = 3,
  x64_reg_rcx = 1,
  x64_reg_rdx = 2,
  x64_reg_rsp = 4,
  x64_reg_rbp = 5,
  x64_reg_rsi = 6,
  x64_reg_rdi = 7,
  x64_reg_r8  = 8,
  x64_reg_r9  = 9,
  x64_reg_r10 = 10,
  x64_reg_r11 = 11,
  x64_reg_r12 = 12,
  x64_reg_r13 = 13,
  x64_reg_r14 = 14,
  x64_reg_r15 = 15,
  reg_argcount = x64_reg_rax,
  reg_arg0 = x64_reg_rdi,
  reg_arg1 = x64_reg_rsi,
  reg_arg2 = x64_reg_rdx,
  reg_closure_in = x64_reg_r10
};

static bool get_imm64_mov(const uint8_t *op, enum cpu_reg reg, int64_t *v)
{
  uint8_t rex = 0x48 | ((reg >> 3) & 1);

  if (op[0] != rex
      || op[1] != (0xb8 | (reg & 7)))
    {
      abort();
    }
  *v = *(int64_t *)(op + 2);
  return true;
}
#endif  /* __x86_64__ */

static bool get_imm32_mov(const uint8_t *op, enum cpu_reg reg, int32_t *v)
{
  assert(reg <= 7); /* not supported (yet) */

  if (op[0] != (0xb8 | reg))
    {
      abort();
    }
  *v = *(int32_t *)(op + 1);
  return true;
}

static void handle_primitive_frame(
  ulong pcadr,
  const struct prim_op *last_primop,
  const struct session_context *next_session,
  void (*primfunc)(
    const struct prim_op *op,
    const struct session_context *next_session,
    value *args, int nargs),
  void (*anyfunc)(value called, value *args, int nargs),
  ulong *last_sp)
{
  value *cargs = NULL;
  int cnargs = -1;
  const uint8_t *op;
  ulong primadr;

#ifdef __i386__
  /* check for relative call to primitive */
  op = (const uint8_t *)(pcadr - 5);
  if (op[0] != 0xe8)
    return;
  primadr = pcadr + *(long *)(op + 1);
#elif defined __x86_64__
  /* check for %rip-relative call to primitive */
  op = (const uint8_t *)(pcadr - 6);
  if (op[0] != 0xff || op[1] != 0x15)
    return;
  int32_t ofs = *(int32_t *)(op + 2);
  primadr = *(ulong *)(pcadr + ofs);
#else
  #error Unsupported architecture
#endif

  const struct prim_op *prim = NULL;
  if (primadr == (ulong)bcall_secure)
    {
#ifdef __i386__
      const int seclev_bytes = 4; /* op16 mov imm16 */
#elif defined __x86_64__
      const int seclev_bytes = 5; /* op16 rex mov imm16 */
#else
  #error Unsupported architecture
#endif

      /* (x86 only) push args...
       * mov $prim,closure_in
       * mov $nargs,argcount  or  xor argcount,argcount
       * mov $seclev,%cx (x86) / %r11w (x86-64)
       * call bcall_secure */

      bool is_xor = ((const uint16_t *)(op - seclev_bytes))[-1] == 0xc031;
      int argc_bytes = is_xor ? 2 : 5;

#ifdef __i386__
      int32_t i;
      if (is_xor)
        i = 0;
      else if (!get_imm32_mov(op - seclev_bytes - 5, reg_argcount, &i))
        return;
      cnargs = i;
      cargs = (value *)last_sp;

      if (!get_imm32_mov(op - seclev_bytes - argc_bytes - 5,
                         reg_closure_in, &i))
        return;
#elif defined __x86_64__
      int64_t i;
      if (!get_imm64_mov(op - seclev_bytes - argc_bytes - 10,
                         reg_closure_in, &i))
        return;
#else
  #error Unsupported architecture
#endif
      prim = ((struct primitive *)i)->op;
    }
#ifdef __x86_64__
  else if (primadr == (ulong)bcall_prim
           || primadr == (ulong)bcall_prim_noalloc)
    {
      /* mov callee,closure_in
       * call bcall_prim{,_noalloc} */
      int64_t i;
      if (!get_imm64_mov(op - 10, reg_closure_in, &i))
        return;
      primadr = i;
    }
#endif  /* __x86_64__ */
  else if (primadr == (ulong)bcall)
    {
      /* push args...
       * mov callee,closure_in
       * mov $nargs,argcount   or   xor argcount,argcount
       * call bcall */

      if (last_sp[-3] == (ulong)bcall_primitive_tail)
        {
          /* last_sp: [..0] mudlle args, [-1] mudlle pc, [-2] bp,
             [-3] bcall_primitive_tail, [-4] callee,
             [-5] argcount */
          primadr = last_sp[-4];
          cnargs = last_sp[-5];
          cargs = (value *)last_sp;
        }
    }
  else if (primadr == (ulong)bapply_varargs)
    {
#ifdef __i386__
      /* push argvector
       * mov callee,%edx
       * mov seclev,%cx
       * call bapply_varargs */

      /* last_sp: [-1] mudlle pc, [-2] bp, [-3] argvector,
         [-4] callee */
      primadr = last_sp[-4];
#elif defined __x86_64__
      /* push argvector
         mov callee,arg0
         mov seclev,arg2
         call bapply_varargs */
      int64_t i64;
      if (!get_imm64_mov(op - 5 - 10, reg_arg0, &i64))
        return;
      primadr = i64;
#else
  #error Unsupported architecture
#endif
      cnargs = -1;
      cargs = (value *)last_sp;
    }
  else if (primadr == (ulong)bcall_varargs)
    {
#ifdef __i386__
      /* push args
       * mov callee,%edx
       * mov $argcount,%eax
       * mov $seclev,%cx
       * call bapply_varargs */
      const enum cpu_reg argreg = x86_reg_eax;
      const enum cpu_reg calleereg = x86_reg_edx;
      const int seclev_bytes = 4; /* op16 mov imm16 */
#elif defined __x86_64__
      /* push args
       * mov callee,arg0
       * mov $argcount,arg1
       * mov $seclev,arg2w
       * call bapply_varargs */
      const enum cpu_reg argreg = reg_arg1;
      const enum cpu_reg calleereg = reg_arg0;
      const int seclev_bytes = 4; /* op16 mov imm16 */
#else
  #error Unsupported architecture
#endif

      /* note that argcount is > 1; argcount = 0 ends up as
         bapply_varargs */

      /* last_sp: [..0] args, [-1] mudlle pc, [-2] bp */

      cargs = (value *)last_sp;

      int32_t i;
      if (!get_imm32_mov(op - seclev_bytes - 5, argreg, &i))
        return;
      cnargs = i;

#ifdef __i386__
      if (!get_imm32_mov(op - seclev_bytes - 2 * 5, calleereg, &i))
        return;
      primadr = i;
#elif defined __x86_64__
      int64_t i64;
      if (!get_imm64_mov(op - seclev_bytes - 5 - 10, calleereg, &i64))
        return;
      primadr = i64;
#else
  #error Unsupported architecture
#endif
    }

  if (prim == NULL)
    prim = lookup_primitive(primadr);

  if (prim && prim != last_primop)
    primfunc(prim, next_session, cargs, cnargs);
}

static void handle_mcode_frame(
  ulong pcadr,
  void (*func)(struct mcode *, ulong ofs, value *args,
               int nargs, void *data),
  const struct prim_op *last_primop,
  const struct session_context *next_session,
  void (*primfunc)(
    const struct prim_op *op,
    const struct session_context *next_session,
    value *args, int nargs),
  void (*anyfunc)(value called, value *args, int nargs),
  ulong *last_sp, ulong *bp,
  int nargs,
  void *data)
{
  struct mcode *mcode = find_pc_mcode(pcadr, (ulong)gcblock,
                                      (ulong)gcblock + gcblocksize);
  if (mcode == NULL)
    return;

  if (primfunc)
    handle_primitive_frame(pcadr, last_primop, next_session, primfunc,
                           anyfunc, last_sp);

  /* args..., pc, *bp  */
  value *args = (value *)bp + 2;
  if (nargs >= 0)
    ;
  else if (code_is_vararg(&mcode->code))
    {
      /* vararg argument vector is the first spilled local */
      args = (value *)bp - 1;
      assert(TYPE((struct vector *)*args, vector));
    }
  else
    nargs = vector_len(mcode->code.arguments.argv);

  func(mcode, pcadr - (ulong)&mcode->mcode[0], args, nargs, data);
}

static struct ccontext *iterate_cc_frame(
  struct ccontext *cc,
  void (*func)(struct mcode *mcode, ulong ofs, value *args, int nargs,
               void *data),
  const struct prim_op *last_primop,
  const struct session_context *next_session,
  void (*primfunc)(const struct prim_op *op,
                   const struct session_context *next_session,
                   value *args, int nargs),
  void (*anyfunc)(value called, value *args, int nargs),
  int nargs,
  void *data)
{
  assert(cc->frame_start);

  ulong *sp, *bp;
  ccontext_frame(cc, &bp, &sp);

  /* The return address is at sp[-1] */
  handle_mcode_frame(sp[-1], func, last_primop, next_session, primfunc,
                     anyfunc, sp, bp, nargs, data);

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
      handle_mcode_frame(bp[1], func, NULL, NULL, NULL, NULL, NULL,
                         (ulong *)(bp[0]), -1, data);
      assert(bp[0] > (ulong)bp);
      bp = (ulong *)bp[0];
    }

  assert(bp == cc->frame_start);

  return next_ccontext(cc);
}

static struct ccontext *print_cc_frame(
  struct ccontext *cc,
  const struct prim_op *last_primop,
  const struct session_context *next_session,
  int nargs)
{
  return iterate_cc_frame(cc, print_mcode, last_primop, next_session,
                          print_prim, print_any, nargs, NULL);
}
#endif /* (__i386__ || __x86_64__) && !NOCOMPILER */

#if !defined(USE_CCONTEXT) && !defined(NOCOMPILER)
static struct ccontext *print_cc_frame(struct ccontext *cc)
{
  pputs("<compiled>\n", muderr);
  return cc;
}
#endif

static void print_call_trace(enum runtime_error error, bool onstack,
                             int c_onstack_nargs)
{
  struct session_context *session = session_context, *next_session = NULL;
  struct catch_context *catch_ctxt = catch_context;
  int count = 0;
#ifndef NOCOMPILER
  struct ccontext *cc = &ccontext;
#endif

  const struct prim_op *last_primop = NULL;
  if (error == error_none)
    last_primop = warning_prim_ext;
  pputs("Call trace is:\n", muderr);

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

      const struct prim_op *this_primop = NULL;
      switch (scan->type)
	{
        case call_primop:
          this_primop = ((struct call_stack_c_header *)scan)->u.op;
          /* fallthrough */
        case call_string:
	case call_c:
        case call_invalid:
        case call_invalid_argp:
	  print_c_frame(scan, onstack, last_primop, next_session);
	  break;
	case call_bytecode:
	  print_bytecode_frame(scan, onstack);
	  break;
	case call_compiled:
#ifdef NOCOMPILER
	  abort();
#else
	  cc = print_cc_frame(cc, last_primop, next_session, c_onstack_nargs);
	  break;
#endif
        case call_session:
          next_session = session;
          session = ((struct session_context *)scan)->parent;
          break;
	}
      last_primop = this_primop;
      /* Only the first frame can be on the stack */
      onstack = false;
      c_onstack_nargs = -1;
    }
}

struct get_cc_stack_trace_data {
  struct vector *vec;
  int idx;
  bool lines;
};

#ifndef NOCOMPILER
static void count_stack_depth(struct mcode *mcode, ulong ofs, value *args,
                              int nargs, void *data)
{
  ++*(int *)data;
}

static void get_cc_stack_trace(struct mcode *mcode, ulong ofs, value *args,
                               int nargs, void *data)
{
  struct get_cc_stack_trace_data *sdata = data;
  value v = mcode;
  if (sdata->lines)
    {
      /* address is the return address (or pc + 1 for segv) */
      if (ofs > 0)
        --ofs;
      v = alloc_list(v, makeint(dwarf_lookup_line_number(&mcode->code, ofs)));
    }
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
    {
#ifndef NOCOMPILER
      if (scan->type == call_compiled)
        {
          cc = iterate_cc_frame(cc, count_stack_depth, NULL, NULL, NULL, NULL,
                                -1, &depth);
          continue;
        }
#endif
      if (scan->type == call_session)
        continue;
      ++depth;
    }

  struct get_cc_stack_trace_data sdata = {
    .vec   = alloc_vector(depth),
    .idx   = 0,
    .lines = lines
  };
  GCPRO(sdata.vec);

#ifndef NOCOMPILER
  cc = &ccontext;
#endif

  for (struct call_stack *scan = call_stack; scan; scan = scan->next)
    {
      value v;
      switch (scan->type)
	{
        case call_string:
          v = alloc_string(((struct call_stack_c_header *)scan)->u.name);
	  goto ok;
        case call_primop:
          v = alloc_string(((struct call_stack_c_header *)scan)->u.op->name);
	  goto ok;
	case call_c:
	  v = ((struct call_stack_c_header *)scan)->u.prim;
	  goto ok;
	case call_bytecode:
          {
            struct call_stack_mudlle *mscan = (struct call_stack_mudlle *)scan;
            v = mscan->fn;
            if (lines)
              v = alloc_list(v, makeint(get_icode_line(mscan)));
            sdata.vec->data[sdata.idx++] = v;
            continue;
          }
	case call_compiled:
#ifdef NOCOMPILER
	  abort();
#else
	  cc = iterate_cc_frame(cc, get_cc_stack_trace, NULL, NULL, NULL, NULL,
                                -1, &sdata);
          continue;
#endif
        case call_session:
          continue;
        case call_invalid:
        case call_invalid_argp:
          /* can only happen during error handling */
          abort();
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
  if (catch_context->call_trace_mode == call_trace_off)
    return;

  bool send_to_muderr = (catch_context->call_trace_mode != call_trace_no_err
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

  GCPRO(omuderr, l, elem, prev);

  for (; l != NULL; l = l->cdr)
    {
      assert(TYPE(l, pair));
      elem = l->car;
      assert(TYPE(elem, pair));

      if (TYPE(elem->car, oport))
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
  enum runtime_error error;
  bool onstack;
  int c_onstack_nargs;
  const char *warning;
};


static void basic_error_print(value e, void *data)
{
  const struct basic_error_info *info = data;


  if (info->warning)
    pprintf(muderr, "%s: %s\n",
            (info->error == error_none
             ? "warning"
             : mudlle_errors[info->error]),
            info->warning);
  else
    pprintf(muderr, "%s\n", mudlle_errors[info->error]);

  print_call_trace(info->error, info->onstack, info->c_onstack_nargs);
}

static void basic_error(enum runtime_error error, bool onstack,
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

void runtime_error(enum runtime_error error)
{
  runtime_error_message(error, NULL);
}

void runtime_error_message(enum runtime_error error, const char *msg)
/* Effects: Runtime error 'error' has occured. Dump the call_stack to
     mudout & throw back to the exception handler with SIGNAL_ERROR
     and the error code in exception_error.
   Note: Never returns
*/
{
#ifdef MUDLLE_INTERRUPT
  check_interrupt();
#endif
  basic_error(error, false, -1, msg);
  mthrow(SIGNAL_ERROR, error);
}

void compiled_early_runtime_error(enum runtime_error error, int nargs)
{
  basic_error(error, true, nargs, NULL);
  mthrow(SIGNAL_ERROR, error);
}

void interpreted_early_runtime_error(enum runtime_error error)
/* Effects: Runtime error 'error' has occured in a primitive operation.
     Dump the call_stack (plus the primitive operation call) to
     mudout & throw back to the exception handler with SIGNAL_ERROR
     and the error code in exception_error.
     Call this function instead of runtime_error if the arguments of the
     function at the top of call_stack are still on the stack.
   Note: Never returns
*/
{
  basic_error(error, true, -1, NULL);
  mthrow(SIGNAL_ERROR, error);
}

static void internal_primitive_runtime_error(enum runtime_error error,
                                             const char *msg,
                                             const struct prim_op *op,
                                             int nargs, va_list va)
{
  bool is_vararg = op->nargs == NVARARGS || nargs > MAX_PRIMITIVE_ARGS;

  struct call_stack *ostack = call_stack;

  /* prevent duplicate entries */
  if (call_stack && call_stack->type == call_c
      && ((struct call_stack_c_header *)call_stack)->u.prim->op == op)
    goto done;

  struct {
    struct call_stack_c_header c;
    value args[MAX_PRIMITIVE_ARGS];
  } me;
  me.c = (struct call_stack_c_header){
    .s = {
      .next = call_stack,
      .type = call_primop,
    },
    .u.op  = op,
    .nargs = is_vararg ? 1 : nargs
  };

  if (is_vararg)
    me.args[0] = make_vector(nargs, va);
  else
    for (int i = 0; i < nargs; ++i)
      me.args[i] = va_arg(va, value);

  call_stack = &me.c.s;

 done:
  if (error != error_none)
    runtime_error_message(error, msg);

  runtime_warning(msg);
  call_stack = ostack;
}

void primitive_runtime_error(enum runtime_error error,
                             const struct prim_op *op,
                             int nargs, ...)
{
  assert(error != error_none);
  va_list va;
  va_start(va, nargs);
  internal_primitive_runtime_error(error, NULL, op, nargs, va);
  /* not reached */
  abort();
}

void primitive_runtime_error_msg(enum runtime_error error,
                                 const char *msg,
                                 const struct prim_op *op,
                                 int nargs, ...)
{
  assert(error != error_none);
  va_list va;
  va_start(va, nargs);
  internal_primitive_runtime_error(error, msg, op, nargs, va);
  /* not reached */
  abort();
}

void primitive_runtime_warning(const char *msg,
                               const struct prim_op *op,
                               int nargs, ...)
{
  va_list va;
  va_start(va, nargs);
  internal_primitive_runtime_error(error_none, msg, op, nargs, va);
  va_end(va);
}

static struct strbuf sb_error_message = SBNULL;

static const char *bad_type_message(value v, enum mudlle_type expected)
{
  sb_empty(&sb_error_message);
  sb_printf(&sb_error_message, "expected %s; got %s",
            mudlle_type_names[expected],
            mudlle_type_names[TYPEOF(v)]);
  return sb_str(&sb_error_message);
}

void bad_call_error(enum runtime_error error, value callee,
                    int nargs, value *argp)
{
  struct call_stack_c_argp cs = {
    .c = {
      .s = {
        .next = call_stack,
        .type = call_invalid_argp
      },
      .u.value = callee,
      .nargs   = nargs
    },
    .argp = argp
  };
  call_stack = &cs.c.s;
  runtime_error(error);
}

void bad_type_error(value v, enum mudlle_type expected)
{
  runtime_error_message(error_bad_type, bad_type_message(v, expected));
}

static void sb_add_one_type(struct strbuf *sb, enum mudlle_type t,
                            bool *first, unsigned left)
{
  if (*first)
    *first = false;
  else
    sb_addstr(sb, left == 0 ? " or " : ", ");
  sb_addstr(sb, mudlle_type_names[t]);
}

static void sb_add_typeset(struct strbuf *sb, unsigned typeset)
{
  bool first = true;
  /* first loop through synthetic types; then through normal types */
  for (enum mudlle_type t = stype_none; ;)
    {
      unsigned tset = type_typeset(t);
      if (tset == 0 ? typeset == 0 : ((typeset & tset) == tset))
        {
          typeset &= ~tset;
          sb_add_one_type(sb, t, &first, typeset);
        }
      if (typeset == 0)
        return;
      ++t;
      if (t == last_type)
        return;
      if (t == last_synthetic_type)
        t = 0;
    }
}

static const char *bad_typeset_message(value v, unsigned expected)
{
  sb_empty(&sb_error_message);
  sb_addstr(&sb_error_message, "expected ");
  sb_add_typeset(&sb_error_message, expected);
  sb_printf(&sb_error_message, "; got %s", mudlle_type_names[TYPEOF(v)]);
  return sb_str(&sb_error_message);
}

const char *out_of_range_message(long v, long minval, long maxval)
{
  sb_empty(&sb_error_message);
  sb_printf(&sb_error_message, "%ld ", v);
  if (minval <= MIN_TAGGED_INT)
    sb_printf(&sb_error_message, "> %ld", maxval);
  else if (maxval >= MAX_TAGGED_INT)
    sb_printf(&sb_error_message, "< %ld", minval);
  else
    sb_printf(&sb_error_message, "not in [%ld..%ld]", minval, maxval);
  return sb_str(&sb_error_message);
}

const char *not_callable_message(int nargs)
{
  sb_empty(&sb_error_message);
  sb_printf(&sb_error_message, "not callable with %d argument%s",
            nargs, nargs == 1 ? "" : "s");
  return sb_str(&sb_error_message);
}

void primitive_bad_type_error(value v, enum mudlle_type expected,
                              const struct prim_op *op,
                              int nargs, ...)
{
  va_list va;
  va_start(va, nargs);
  internal_primitive_runtime_error(error_bad_type,
                                   bad_type_message(v, expected),
                                   op, nargs, va);
  /* not reached */
  abort();
}

void primitive_bad_typeset_error(value v, unsigned expected,
                                 const struct prim_op *op,
                                 int nargs, ...)
{
  va_list va;
  va_start(va, nargs);
  internal_primitive_runtime_error(error_bad_type,
                                   bad_typeset_message(v, expected),
                                   op, nargs, va);
  /* not reached */
  abort();
}

void bad_typeset_error(value v, unsigned expected)
{
  runtime_error_message(error_bad_type, bad_typeset_message(v, expected));
}

void out_of_range_error(long v, long minval, long maxval)
{
  runtime_error_message(error_bad_value,
                        out_of_range_message(v, minval, maxval));
}
