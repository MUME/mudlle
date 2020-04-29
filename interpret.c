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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "alloc.h"
#include "call.h"
#include "code.h"
#include "context.h"
#include "error.h"
#include "global.h"
#include "ins.h"
#include "interpret.h"
#include "stack.h"
#include "utils.h"

#include "runtime/arith.h"
#include "runtime/basic.h"
#include "runtime/mudlle-string.h"
#include "runtime/runtime.h"

/* As good a place as any other */
extern const char COPYRIGHT[];
const char COPYRIGHT[] = "\
Copyright (c) 1993-2012 David Gay and Gustav H\xe5llberg\n\
All rights reserved.\n\
\n\
Permission to use, copy, modify, and distribute this software for any\n\
purpose, without fee, and without written agreement is hereby granted,\n\
provided that the above copyright notice and the following two paragraphs\n\
appear in all copies of this software.\n\
\n\
IN NO EVENT SHALL DAVID GAY OR GUSTAV HALLBERG BE LIABLE TO ANY PARTY FOR\n\
DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT\n\
OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF DAVID GAY OR\n\
GUSTAV HALLBERG HAVE BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.\n\
\n\
DAVID GAY AND GUSTAV HALLBERG SPECIFICALLY DISCLAIM ANY WARRANTIES,\n\
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND\n\
FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS ON AN\n\
\"AS IS\" BASIS, AND DAVID GAY AND GUSTAV HALLBERG HAVE NO OBLIGATION TO\n\
PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.\n\
";

static inline value invoke_stack(struct closure *c, int nargs);

/* Macros for fast access to the GC'ed stack & code structures.
   RESTORE_INS & RESTORE_STACK must be called after anything that may
   have caused a GC
*/

struct stack_cache {
  value *used;                  /* cached address of stack->used */
  value *pos;                   /* next stack slot to be pushed to */
};

#define RESTORE_STACK() do {                                            \
  stack_cache.used = &stack->used;                                      \
  stack_cache.pos = stack->values->data + intval(*stack_cache.used);    \
} while (0)

#define _FAST_POPN(n)                                           \
  (*stack_cache.used = mudlle_iadd(*stack_cache.used, -(n)),    \
   *(stack_cache.pos -= (n)))
#define FAST_POP() (_FAST_POPN(1))
#define FAST_POPN(n) do {                                       \
  ulong __n = (n);                                              \
  (void)_FAST_POPN(__n);                                        \
} while (0)
#define FAST_PUSH(v) do {                                       \
  *stack_cache.used = mudlle_iadd(*stack_cache.used, 1);        \
  *stack_cache.pos++ = (v);                                     \
} while (0)
#define FAST_GET(n)    (stack_cache.pos[-((n) + 1)])
#define FAST_SET(n, v) (stack_cache.pos[-((n) + 1)] = (v))

#define RESTORE_INS() ((void)(ins = (uint8_t *)me.code + ins_index))
#define INSUINT8()  (ins_index++, *ins++)
#define INSINT8()   ((int8_t)INSUINT8())
#define INSOPER()   ((enum operator)INSUINT8())
#define INSUINT16() (ins_index += 2, ins += 2, (ins[-2] << 8) | ins[-1])
#define INSINT16()  ((int16_t)INSUINT16())

#define SAVE_OFFSET() ((void)(me.offset = ins_index))

#define CONST(n) (me.code->constants[n])

#define IERROR(n) do {				\
  SAVE_OFFSET();				\
  runtime_error(n);				\
} while (0)

#define IERROR_TYPE(arg, type) do {		\
  SAVE_OFFSET();				\
  bad_type_error(arg, type);			\
} while (0)

#define IEARLY_ERROR(n) do {			\
  SAVE_OFFSET();				\
  interpreted_early_runtime_error(n);		\
} while (0)


void do_interpret(struct closure *fn, int nargs)
{
  uint8_t *ins;

  struct stack_cache stack_cache;


  if (get_stack_pointer() < mudlle_stack_limit)
    runtime_error(error_recurse);

  if (nargs > MAX_FUNCTION_ARGS)
    runtime_error(error_wrong_parameters);

#ifdef MUDLLE_INTERRUPT
  check_interrupt();
#endif

  assert(TYPE(fn->code, code));

  struct call_stack_mudlle me = {
    .s = {
      .next = call_stack,
      .type = call_bytecode
    },
    .fn     = fn,
    .code   = (struct icode *)fn->code,
    .locals = NULL,
    .nargs  = nargs,
    .offset = -1,
  };
  call_stack = &me.s;

#ifdef PROFILE_CALL_COUNT
  ++me.code->code.call_count;
#endif

  /* make local variables */
  me.locals = allocate_locals(me.code->nb_locals);

#define LOCAL   me.locals->data[INSUINT8()]
#define CLOSURE me.fn->variables[INSUINT8()]

  /* Pre-initialise call stack entry for calls to C primitives */
  struct {
    struct call_stack_c_header c;
    value args[MAX_PRIMITIVE_ARGS];
  } primop;
  primop.c.s = (struct call_stack){
    .next = &me.s,
    .type = call_c
  };

  static ulong instruction_number;
  ulong start_ins = instruction_number;

  seclev_t seclev = me.code->code.seclevel;
  if (seclev < minlevel)
    interpreted_early_runtime_error(error_security_violation);

  value mseclev = seclevel_to_maxseclevel(seclev);
  value old_maxseclevel = maxseclevel;
  if ((long)mseclev < (long)old_maxseclevel)
    maxseclevel = mseclev;

  stack_reserve(me.code->stkdepth); /* Ensure enough space on stack */
  RESTORE_STACK();

  /* Loop over instructions, executing each one */
  /* As code may move with gc, we can't have a pointer into it.
     So we base ourselves on code (which is protected) and pretend
     that is an array of instructions. */
  ulong ins_index = ((uint8_t *)(&me.code->constants[me.code->nb_constants])
                     - (uint8_t *)me.code);
  RESTORE_INS();

  for (;;) {
    struct obj *called;
    const struct prim_op *op;

    instruction_number++;
    enum operator byteop = INSOPER();
    switch (byteop)
      {
      case op_return: goto done;

      case op_constant1:
	FAST_PUSH(CONST(INSUINT8()));
	GCCHECK(FAST_GET(0));
	break;
      case op_constant2:
        FAST_PUSH(CONST(INSUINT16()));
        GCCHECK(FAST_GET(0));
        break;
      case op_integer1:
	FAST_PUSH(makeint(INSINT8()));
	break;
      case op_integer2:
	FAST_PUSH(makeint(INSINT16()));
	break;

	/* Note: Semantics of closure, closure_code & vclass_closure could be
	   a bit different (simpler):
	     - closure creates an empty closure and pushes it on the stack
	     - vclass_closure & closure_code modify the closure at the top of
	       the stack.
	   This removes restrictions on the use of these instructions, but
	   makes the code a bit more complicated (and slower).
	   New restriction: No instructions but op_closure_var may appear
	   between op_closure and op_closure_code. The closure is in an
	   unsafe state for GC.
	   As the restrictions are not a problem for compile.c, the simpler
	   implementation is chosen. */
      case op_closure:
        {
	  struct closure *new_closure = unsafe_alloc_closure(INSUINT8());
          /* No GC allowed after this point till op_closure_code is executed */
          RESTORE_STACK();
          RESTORE_INS();
          FAST_PUSH(new_closure);

          for (struct variable **next_var = new_closure->variables;;)
            {
              enum operator cop = INSOPER();
              if (cop == op_closure_var_local)
                {
                  *next_var++ = LOCAL;
                  continue;
                }
              if (cop == op_closure_var_closure)
                {
                  *next_var++ = CLOSURE;
                  continue;
                }

              if (cop == op_closure_code1)
                new_closure->code = CONST(INSUINT8());
              else if (cop == op_closure_code2)
                new_closure->code = CONST(INSUINT16());
              else
                abort();
              GCCHECK(new_closure->code);
              break;
            }
          break;
        }

#define C_ARG(n) primop.args[n]
#define C_SETARG(n, arg) C_ARG(n) = arg; GCCHECK(C_ARG(n))

#define C_START_CALL(n, pop) do {               \
          SAVE_OFFSET();                        \
          struct primitive *__pop = (pop);      \
          primop.c.u.prim = __pop;              \
          op = __pop->op;                       \
          primop.c.nargs = n;			\
          call_stack = &primop.c.s;             \
        } while (0)

#define C_END_CALL(result) do {                 \
          value __result = (result);            \
          call_stack = &me.s;			\
          GCCHECK(__result);			\
          RESTORE_STACK();			\
          RESTORE_INS();                        \
          FAST_PUSH(__result);                  \
        } while (0)

      case op_execute_primitive_1arg:
	C_START_CALL(1, GVAR(INSUINT16()));
	C_SETARG(0, FAST_POP());
	set_seclevel(seclev);
	C_END_CALL(op->op(C_ARG(0)));
	break;
      case op_execute_primitive_2arg:
	C_START_CALL(2, GVAR(INSUINT16()));
	C_SETARG(1, FAST_POP());
	C_SETARG(0, FAST_POP());
	set_seclevel(seclev);
	C_END_CALL(op->op(C_ARG(0), C_ARG(1)));
	break;

      case op_execute_global_1arg:
	called = GVAR(INSUINT16());
	nargs = 1;
	goto execute_fn;
      case op_execute_global_2arg:
	called = GVAR(INSUINT16());
	nargs = 2;
	goto execute_fn;

      case op_execute2:
        nargs = INSUINT16();
        goto do_op_execute;
      case op_execute:
	nargs = INSUINT8();
      do_op_execute:
	called = FAST_POP();

      execute_fn:
	set_seclevel(DEFAULT_SECLEVEL);
	switch (TYPEOF(called))
	  {
	  case type_varargs:
	    {
	      struct primitive *pop = (struct primitive *)called;

              /* set zero args in case there is a GC */
              C_START_CALL(0, pop);
	      struct vector *args = UNSAFE_ALLOCATE_RECORD(vector, nargs);
	      RESTORE_STACK();
	      for (ulong i = nargs; i > 0;)
                args->data[--i] = FAST_POP();

              primop.c.nargs = 1;
	      C_SETARG(0, args);
              vararg_op_fn vop = op->op;
	      C_END_CALL(vop(args, nargs));
	      break;
	    }

	  case type_secure:
	    C_START_CALL(nargs, (struct primitive *)called);
	    if (DEFAULT_SECLEVEL < op->seclevel
                || intval(maxseclevel) < op->seclevel)
	      IEARLY_ERROR(error_security_violation);
	    goto execute_primitive;

	  case type_primitive:
	    C_START_CALL(nargs, (struct primitive *)called);

	  execute_primitive:
	    if (nargs != op->nargs)
	      IEARLY_ERROR(error_wrong_parameters);

            for (int arg = nargs; arg > 0; )
              {
                --arg;
                C_SETARG(arg, FAST_POP());
              }

            {
              value result;
              switch (nargs)
                {
                case 0:
                  result = op->op();
                  break;

#define __CPRIMARG(N) C_ARG(N - 1)
#define __CALL_PRIM(N)                                                  \
                  case N:                                               \
                    result = op->op(CONCATCOMMA(N, __CPRIMARG));        \
                    break
                  DOPRIMARGS(__CALL_PRIM, SEP_SEMI);
#undef __CALL_PRIM
#undef __CPRIMARG
                default:
                  abort();
                }
              C_END_CALL(result);
            }
	    break;

	  case type_closure:
	    {
	      struct closure *c = (struct closure *)called;

	      SAVE_OFFSET();
	      if (c->code->o.type == type_mcode)
		{
		  value result = invoke_stack(c, nargs);
		  RESTORE_STACK();
		  FAST_PUSH(result);
		}
	      else
		{
		  do_interpret(c, nargs);
		  RESTORE_STACK();
		}
	      RESTORE_INS();
	      break;
	    }
	  default:
            {
              SAVE_OFFSET();
              primop.c.s.type = call_invalid;
              primop.c.u.value = called;
              primop.c.nargs = nargs;
              call_stack = &primop.c.s;
              IEARLY_ERROR(error_bad_function);
            }
	  }
	break;

      case op_execute_secure2:
        nargs = INSUINT16();
        goto do_op_execute_secure;
      case op_execute_secure:
	nargs = INSUINT8();
      do_op_execute_secure:
	called = FAST_POP();

	/* Compiler only generates this for secure primitives in
	   protected modules (normally system) */
	assert(pointerp(called) && called->type == type_secure);

	C_START_CALL(nargs, (struct primitive *)called);
	set_seclevel(seclev);
	if (seclev < op->seclevel || intval(maxseclevel) < op->seclevel)
	  IEARLY_ERROR(error_security_violation);
	goto execute_primitive;

      case op_execute_primitive2:
        nargs = INSUINT16();
        goto do_op_execute_primitive;
      case op_execute_primitive:
	nargs = INSUINT8();
      do_op_execute_primitive:
	called = FAST_POP();

	/* Compiler only generates this for primitives in
	   protected modules (normally system) */
	assert(pointerp(called) && called->type == type_primitive);

	C_START_CALL(nargs, (struct primitive *)called);
	goto execute_primitive;

      case op_execute_varargs2:
        nargs = INSUINT16();
        goto do_op_execute_varargs;
      case op_execute_varargs:
	{
          nargs = INSUINT8();
        do_op_execute_varargs:
	  called = FAST_POP();

	  /* Compiler only generates this for varargs primitives in
	     protected modules (normally system) */
	  assert(pointerp(called) && called->type == type_varargs);

	  set_seclevel(seclev);

	  struct primitive *pop = (struct primitive *)called;

          /* set zero args in case there is a GC */
          C_START_CALL(0, pop);
	  struct vector *args = UNSAFE_ALLOCATE_RECORD(vector, nargs);
	  RESTORE_STACK();
	  for (ulong i = nargs; i > 0; )
            args->data[--i] = FAST_POP();

          primop.c.nargs = 1;
	  SAVE_OFFSET();
	  C_SETARG(0, args);
          vararg_op_fn vop = op->op;
	  C_END_CALL(vop(args, nargs));
	  break;
	}

      case op_argcheck:		/* A CISCy instruction :-) */
	if (nargs != INSUINT8()) IEARLY_ERROR(error_wrong_parameters);
	for (ulong i = 0; i < nargs; i++)
	  ((struct variable *)me.locals->data[i])->vvalue
            = FAST_GET(i);
	break;
      case op_varargs:		/* Another CISCy instruction... */
	{
	  struct vector *args = UNSAFE_ALLOCATE_RECORD(vector, nargs);

	  RESTORE_STACK();
	  RESTORE_INS();
	  for (int j = 0; j < nargs; j++)
	    args->data[nargs - j - 1] = FAST_GET(j);

	  ((struct variable *)me.locals->data[0])->vvalue = args;
	  me.nargs = 1;
	  FAST_POPN(nargs);
	  break;
	}
      case op_discard:
	FAST_POPN(1);
	break;
      case op_exit_n:
        {
          value result = FAST_POP();
          FAST_POPN(INSUINT8());
          FAST_PUSH(result);
          break;
        }
      case op_pop_n:
	FAST_POPN(INSUINT8());
	break;
      case op_branch_z1:
	if (!istrue(FAST_POP())) goto branch1;
	(void)INSINT8();
	break;
      case op_branch_z2:
	if (!istrue(FAST_POP())) goto branch2;
	(void)INSINT16();
	break;
      case op_branch_nz1:
	if (istrue(FAST_POP())) goto branch1;
	(void)INSINT8();
	break;
      case op_branch_nz2:
	if (istrue(FAST_POP())) goto branch2;
	(void)INSINT16();
	break;
      case op_loop1:
	FAST_POPN(1);
#ifdef MUDLLE_INTERRUPT
	check_interrupt();
#endif
	/* FALL THROUGH */
      case op_branch1:
      branch1:
	{
	  int8_t offset = INSINT8();

	  ins_index += offset;
	  ins += offset;
	  break;
	}

      case op_loop2:
	FAST_POPN(1);
#ifdef MUDLLE_INTERRUPT
	check_interrupt();
#endif
	/* FALL THROUGH */
      case op_branch2:
      branch2:
	{
	  int16_t offset = INSINT16();

	  ins_index += offset;
	  ins += offset;
	  break;
	}

#define RECALL(access) FAST_PUSH(((struct variable *)access)->vvalue)
#define VREF(access)   FAST_PUSH(access)
#define ASSIGN(access) do {                                     \
          struct variable *_var = (struct variable *)(access);  \
          _var->vvalue = FAST_GET(0);                           \
        } while (0)

      case op_clear_local: ((struct variable *)LOCAL)->vvalue = NULL; break;

      case op_recall_local:   RECALL(LOCAL);   break;
      case op_recall_closure: RECALL(CLOSURE); break;
      case op_recall_global:
        {
          ulong goffset = INSUINT16();

          SAVE_OFFSET();
          check_global_read(goffset);

          FAST_PUSH(GVAR(goffset));
          break;
        }
      case op_vref_local:     VREF(LOCAL);     break;
      case op_vref_closure:   VREF(CLOSURE);   break;
        /* op_vref_global is invalid */
      case op_assign_local:   ASSIGN(LOCAL);   break;
      case op_assign_closure: ASSIGN(CLOSURE); break;
      case op_assign_global:
	{
	  ulong goffset = INSUINT16();

	  SAVE_OFFSET();
          value val = FAST_GET(0);
	  check_global_write(val, goffset);
	  GVAR(goffset) = val;
	  break;
	}
      case op_define:
        /* like op_assign global, but no error checking */
	GVAR(INSUINT16()) = FAST_GET(0);
	break;

	/* The builtin operations */
      case op_builtin_eq:
        {
          value arg1 = FAST_POP();
          FAST_SET(0, makebool(FAST_GET(0) == arg1));
          break;
        }
      case op_builtin_neq:
        {
          value arg1 = FAST_POP();
          FAST_SET(0, makebool(FAST_GET(0) != arg1));
          break;
        }

#define INTEGER_OP(op, opname) do {		\
	  value arg2 = FAST_POP();              \
	  value arg1 = FAST_GET(0);             \
	  if ((long)arg1 & (long)arg2 & 1)	\
	    FAST_SET(0, op);			\
	  else					\
            {                                   \
              code_ ## opname(arg1, arg2);      \
              abort();                          \
            }                                   \
	} while (0)

      case op_builtin_lt:
	INTEGER_OP(makebool((long)arg1 < (long)arg2), smaller);
	break;
      case op_builtin_le:
	INTEGER_OP(makebool((long)arg1 <= (long)arg2), smaller_equal);
	break;
      case op_builtin_gt:
	INTEGER_OP(makebool((long)arg1 > (long)arg2), greater);
	break;
      case op_builtin_ge:
	INTEGER_OP(makebool((long)arg1 >= (long)arg2), greater_equal);
	break;

      case op_builtin_addint:
        INTEGER_OP((value)((long)arg1 + (long)arg2 - 1), iadd);
        break;

      case op_builtin_add:
        {
          value arg2 = FAST_POP();
          value arg1 = FAST_GET(0);
          if ((long)arg1 & (long)arg2 & 1)
            FAST_SET(0, (value)((long)arg1 + (long)arg2 - 1));
          else if (TYPE(arg1, string) && TYPE(arg2, string))
            {
              arg1 = string_plus(arg1, arg2);
              RESTORE_INS();
              RESTORE_STACK();
              FAST_SET(0, arg1);
            }
          else
            {
              /* call for nice call trace */
              code_plus(arg1, arg2);
              abort();
            }
          break;
        }

      case op_builtin_sub:
	INTEGER_OP((value)((long)arg1 - (long)arg2 + 1), subtract);
	break;

      case op_builtin_bitand:
	INTEGER_OP((value)((long)arg1 & (long)arg2), bitand);
	break;
      case op_builtin_bitor:
	INTEGER_OP((value)((long)arg1 | (long)arg2), bitor);
	break;

      case op_builtin_not:
	FAST_SET(0, makebool(!istrue(FAST_GET(0))));
	break;

	/* These could be optimised */
      case op_builtin_ref:
        {
          SAVE_OFFSET();
          value arg2 = FAST_POP();
          value arg1 = code_ref(FAST_GET(0), arg2);
          GCCHECK(arg1);
          RESTORE_STACK();
          RESTORE_INS();
          FAST_SET(0, arg1);
          break;
        }

      case op_builtin_set:
        {
          SAVE_OFFSET();
          value arg2 = FAST_POP();
          value arg1 = FAST_POP();
          arg1 = code_setb(FAST_GET(0), arg1, arg2);
          GCCHECK(arg1);
          RESTORE_STACK();
          RESTORE_INS();
          FAST_SET(0, arg1);
          break;
        }

      case op_typeset_check:
        {
          value arg2 = FAST_POP();
          assert(integerp(arg2));
          long typeset = intval(arg2);
          value arg1 = FAST_GET(INSUINT8());
          if (!is_typeset(arg1, typeset))
            {
              SAVE_OFFSET();
              bad_typeset_error(arg1, typeset);
            }
          break;
        }

        /* type checks follow */
#define _SIMPLE_TYPECHECK(arg, type) {                  \
          value arg1 = FAST_GET(INSUINT8());            \
          if (!TYPE(arg1, type))                        \
            IERROR_TYPE(arg1, type_ ## type);           \
          break;                                        \
        }
#define _OP_TYPECHECK(simple, arg, type) IF(simple)(    \
          _SIMPLE_TYPECHECK(arg, type),                 \
          goto pointer_typecheck;)
#define OP_TYPECHECK(type, arg)                         \
        case op_typecheck_ ## type:                     \
          _OP_TYPECHECK(                                \
            IF(IS_MARK(_TYPE_IS_NULL_ ## type))(        \
              1,                                        \
              IF(IS_MARK(_TYPE_IS_INT_ ## type))(       \
                1, 0)),                                 \
            arg, type)

      FOR_PLAIN_TYPES(OP_TYPECHECK,)

        {
        pointer_typecheck: ;
          struct obj *arg1 = FAST_GET(INSUINT8());
          enum mudlle_type type = byteop - op_typecheck;
          if (!pointerp(arg1) || arg1->type != type)
            IERROR_TYPE(arg1, type);
          break;
        }

      case op_typecheck_none:
	IERROR(error_bad_type);

      case op_typecheck_any:
        break;

      case op_typecheck_function:
	{
	  value arg1 = FAST_GET(INSUINT8());

	  if (!is_function(arg1))
            IERROR_TYPE(arg1, stype_function);
	  break;
	}

      case op_typecheck_list:
        CASSERT_EXPR(last_synthetic_type == 27);
        {
          value arg1 = FAST_GET(INSUINT8());
          if (arg1 && !TYPE(arg1, pair))
            IERROR_TYPE(arg1, stype_list);
          break;
        }

      default: abort();
      }
  }
 done:
  maxseclevel = old_maxseclevel;

  call_stack = me.s.next;

  me.code->instruction_count += instruction_number - start_ins;
}


/* Interface to machine code. */

static inline value invoke_stack(struct closure *c, int nargs)
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, mcode);
     The stack must contain at least nargs entries.
   Effects: Executes c(nargs arguments taken from the stack)
   Returns: c's result
*/
{
  if (nargs == 0)
    return invoke0(c);

  if (nargs > MAX_PRIMITIVE_ARGS)
    {
      GCPRO(c);
      struct vector *extra = UNSAFE_ALLOCATE_RECORD(vector, nargs);
      UNGCPRO();
      struct stack_cache stack_cache;
      RESTORE_STACK();
      for (int i = nargs; i > 0; )
        extra->data[--i] = FAST_POP();
      return invokev(c, extra);
    }

  struct stack_cache stack_cache;
  RESTORE_STACK();

#define __POPARG(N) __PRIMARG(N) = FAST_GET(nargs - N)
#define __INVOKE(N)                             \
  case N:                                       \
    {                                           \
      CONCATSEMI(N, __POPARG);                  \
      FAST_POPN(N);                             \
      return invoke ## N(c, PRIMARGNAMES ## N); \
    }

  switch (nargs)
    {
      DOPRIMARGS(__INVOKE, SEP_EMPTY)
    default: abort();
    }

#undef __POPARG
#undef __INVOKE
}

/* Interface from machine code - backend specific */

#if (defined __i386__ || defined __x86_64__) && !defined NOCOMPILER
/* called from x{64,86}builtins.S; args[nargs] must be GC-protected */
value builtin_call_interpreter(struct closure *closure, int nargs,
                               const value *args);
value builtin_call_interpreter(struct closure *closure, int nargs,
                               const value *args)
{
  if (nargs > 0)
    {
      /* Reserve stack space */
      GCPRO(closure);
      stack_reserve(nargs);
      UNGCPRO();
      struct stack_cache stack_cache;
      RESTORE_STACK();

      for (int i = 0; i < nargs; i++)
        {
          value v = *args++;
          FAST_PUSH(v);
        }
    }

  do_interpret(closure, nargs);

  return stack_pop();
}
#endif
