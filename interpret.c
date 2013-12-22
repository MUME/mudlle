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

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "alloc.h"
#include "call.h"
#include "code.h"
#include "context.h"
#include "error.h"
#include "global.h"
#include "interpret.h"
#include "stack.h"

#include "runtime/arith.h"
#include "runtime/runtime.h"
#include "runtime/stringops.h"
#include "runtime/basic.h"

/* As good a place as any other */
const char COPYRIGHT[] = "\
Copyright (c) 1993-2012 David Gay and Gustav Hållberg\n\
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

#define RESTORE_STACK() (stkpos = stack->values->data + intval(stack->used))
#define _FAST_POPN(n) (stack->used = (value)((long)stack->used - ((n) + (n))), *(stkpos -= (n)))
#define FAST_POP() (_FAST_POPN(1))
#define FAST_POPN(n) do {                       \
    ulong __n = (n);                            \
    (void)_FAST_POPN(__n);                      \
  } while (0)
#define FAST_PUSH(v) do {                               \
    stack->used = (value)((long)stack->used + 2);       \
    *stkpos++ = (v);                                    \
  } while(0)
#define FAST_GET(n) (stkpos[-((n) + 1)])
#define FAST_SET(n, v) (stkpos[-((n) + 1)]= (v))

#define RESTORE_INS() (ins = ((instruction *)me.u.mudlle.code) + ins_index)
#define INSUBYTE() (ins_index++, *ins++)
#define INSBYTE() ((sbyte)INSUBYTE())
#define INSUWORD() (ins_index += 2, ins += 2, (ins[-2] << 8) | (ins[-1]))
#define INSWORD() ((word)INSUWORD())

#define SAVE_OFFSET() do { me.u.mudlle.offset = ins_index; } while (0)

#define CONST(n) (me.u.mudlle.code->constants[n])

#define IERROR(n) do {				\
  SAVE_OFFSET();				\
  runtime_error(n);				\
} while (0)

#define IEARLY_ERROR(n) do {			\
  SAVE_OFFSET();				\
  early_runtime_error(n);			\
} while (0)

void do_interpret(struct closure *fn, int nargs)
{
  ulong new_var = 0;
  struct closure *new_closure = NULL;
  instruction *ins;
  value *stkpos;		/* Pointer into stack */


#ifdef i386
  {
    ulong sp;
    asm("movl %%esp,%0" : "=rm" (sp));
    if (sp < mudlle_stack_limit) runtime_error(error_recurse);
  }
#endif

#ifdef MUDLLE_INTERRUPT
  check_interrupt();
#endif

  assert(TYPE(fn->code, type_code));

  struct call_stack me;
  /* Initialise call stack entry */
  me.type = call_bytecode;
  me.u.mudlle.fn = fn;
  me.u.mudlle.code = (struct icode *)fn->code;
  me.u.mudlle.locals = NULL;
  me.u.mudlle.nargs = nargs;
  me.u.mudlle.offset = -1;
  me.next = call_stack;
  call_stack = &me;

  /* Pre-initialise call stack entry for calls to C primitives */
  struct call_stack primop;
  primop.next = &me;
  primop.type = call_c;
  memset(primop.u.c.args, 0, sizeof primop.u.c.args);

  /* Make local variables */
  me.u.mudlle.locals = allocate_locals(me.u.mudlle.code->nb_locals);

  /* Profiling */
  me.u.mudlle.code->call_count++;

  static ulong instruction_number;
  ulong start_ins = instruction_number;

  if (me.u.mudlle.code->code.seclevel < minlevel)
    early_runtime_error(error_security_violation);

  stack_reserve(me.u.mudlle.code->stkdepth); /* Ensure enough space on stack */
  RESTORE_STACK();

  /* Loop over instructions, executing each one */
  /* As code may move with gc, we can't have a pointer into it.
     So we base ourselves on code (which is protected) and pretend
     that is an array of instructions. */
  ulong ins_index = ((instruction *)(&me.u.mudlle.code->constants[
                                       me.u.mudlle.code->nb_constants])
                     - (instruction *)me.u.mudlle.code);
  RESTORE_INS();

  for (;;) {
    struct obj *called;
    const struct primitive_ext *op;

    instruction_number++;
    instruction byteop = INSUBYTE();
    switch (byteop)
      {
      case op_return: goto done;

      case op_constant1:
	FAST_PUSH(CONST(INSUBYTE()));
	GCCHECK(FAST_GET(0));
	break;
      case op_constant2:
        FAST_PUSH(CONST(INSUWORD()));
        GCCHECK(FAST_GET(0));
        break;
      case op_integer1:
	FAST_PUSH(makeint(INSBYTE()));
	break;
      case op_integer2:
	FAST_PUSH(makeint(INSWORD()));
	break;

	/* Note: Semantics of closure, closure_code & closure_var could be
	   a bit different (simpler):
	     - closure creates an empty closure and pushes it on the stack
	     - closure_var & closure_code modify the closure at the top of
	       the stack.
	   This removes restrictions on the use of these instructions, but
	   makes the code a bit more complicated (and slower).
	   New restriction: No instructions but op_closure_var may appear
	   between op_closure and op_closure_code. The closure is in an
	   unsafe state for GC.
	   As the restrictions are not a problem for compile.c, the simpler
	   implementation is chosen. */
      case op_closure:
	new_closure = unsafe_alloc_closure(INSUBYTE());
	/* No GC allowed after this point till op_closure_code is executed */
	RESTORE_STACK();
	RESTORE_INS();
	FAST_PUSH(new_closure);
	new_var = 0;
	break;
      case op_closure_code1:
	new_closure->code = (struct code *)CONST(INSUBYTE());
	GCCHECK(new_closure->code);
	/*TYPEIS(new_closure->code, type_code);*/
	/* new_closure is now safe ... */
	break;
      case op_closure_code2:
	new_closure->code = (struct code *)CONST(INSUWORD());
	GCCHECK(new_closure->code);
	/*TYPEIS(new_closure->code, type_code);*/
	/* new_closure is now safe ... */
	break;

#define C_SETARG(n, arg) C_ARG(n) = arg; GCCHECK(C_ARG(n))
#define C_ARG(n) primop.u.c.args[n]

#define C_START_CALL(n, pop) do {               \
          SAVE_OFFSET();                        \
          struct primitive *__pop = (pop);      \
          __pop->call_count++;			\
          primop.u.c.u.prim = __pop;            \
          op = __pop->op;                       \
          primop.u.c.nargs = n;			\
          call_stack = &primop;                 \
        } while (0)

#define C_END_CALL(result) do {                 \
          value __result = (result);            \
          call_stack = &me;			\
          GCCHECK(__result);			\
          RESTORE_STACK();			\
          RESTORE_INS();                        \
          FAST_PUSH(__result);                  \
        } while (0)

      case op_execute_primitive1:
	C_START_CALL(1, GVAR(INSUWORD()));
	C_SETARG(0, FAST_POP());
	set_seclevel(me.u.mudlle.code->code.seclevel);
	C_END_CALL(op->op(C_ARG(0)));
	break;
      case op_execute_primitive2:
	C_START_CALL(2, GVAR(INSUWORD()));
	C_SETARG(1, FAST_POP());
	C_SETARG(0, FAST_POP());
	set_seclevel(me.u.mudlle.code->code.seclevel);
	C_END_CALL(op->op(C_ARG(0), C_ARG(1)));
	break;

      case op_execute_global1:
	called = GVAR(INSUWORD());
	nargs = 1;
	goto execute_fn;
      case op_execute_global2:
	called = GVAR(INSUWORD());
	nargs = 2;
	goto execute_fn;
      case op_execute:
	called = FAST_POP();
	nargs = INSUBYTE();

      execute_fn:
	if (!pointerp(called)) IERROR(error_bad_function);
	set_seclevel(DEFAULT_SECLEVEL);
	switch (called->type)
	  {
	  case type_varargs:
	    {
	      struct vector *args;

	      struct primitive *pop = (struct primitive *)called;

	      /* call_stack must be set after allocate_record, so can't use
		 C_START_CALL macro */
	      pop->call_count++;
	      primop.u.c.u.prim = pop;
	      op = pop->op;
	      primop.u.c.nargs = 1;

	      args = (struct vector *)unsafe_allocate_record(type_vector,
                                                             nargs);
	      RESTORE_STACK();
	      for (ulong i = nargs; i > 0;)
                args->data[--i] = FAST_POP();

	      call_stack = &primop;
	      C_SETARG(0, args);
	      C_END_CALL(op->op(args, nargs));
	      break;
	    }

	  case type_secure:
	    C_START_CALL(nargs, (struct primitive *)called);
	    if (DEFAULT_SECLEVEL < op->seclevel)
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
#define __CALL_PRIM(N)                                                  \
                  case N:                                               \
                    result = call_primop ## N(op->op, &C_ARG(0));       \
                    break;
                  DOPRIMARGS(__CALL_PRIM)
#undef __CALL_PRIM
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
	  default: IERROR(error_bad_function);
	  }
	break;

      case op_execute_secure:
	called = FAST_POP();
	nargs = INSUBYTE();

	/* Compiler only generates this for secure primitives in
	   protected modules (normally system) */
	assert(pointerp(called) && called->type == type_secure);

	C_START_CALL(nargs, (struct primitive *)called);
	set_seclevel(me.u.mudlle.code->code.seclevel);
	if (me.u.mudlle.code->code.seclevel < op->seclevel)
	  IEARLY_ERROR(error_security_violation);
	goto execute_primitive;

      case op_execute_primitive:
	called = FAST_POP();
	nargs = INSUBYTE();

	/* Compiler only generates this for primitives in
	   protected modules (normally system) */
	assert(pointerp(called) && called->type == type_primitive);

	C_START_CALL(nargs, (struct primitive *)called);
	goto execute_primitive;

      case op_execute_varargs:
	{
	  struct vector *args;

	  called = FAST_POP();
	  nargs = INSUBYTE();

	  /* Compiler only generates this for varargs primitives in
	     protected modules (normally system) */
	  assert(pointerp(called) && called->type == type_varargs);

	  set_seclevel(me.u.mudlle.code->code.seclevel);

	  struct primitive *pop = (struct primitive *)called;

	  /* call_stack must be set after allocate_record, so can't use
	     C_START_CALL macro */
	  pop->call_count++;
	  primop.u.c.u.prim = pop;
	  op = pop->op;
	  primop.u.c.nargs = 1;

	  args = (struct vector *)unsafe_allocate_record(type_vector, nargs);
	  RESTORE_STACK();
	  for (ulong i = nargs; i > 0; )
            args->data[--i] = FAST_POP();

	  call_stack = &primop;
	  SAVE_OFFSET();
	  C_SETARG(0, args);
	  C_END_CALL(op->op(args, nargs));
	  break;
	}

      case op_argcheck:		/* A CISCy instruction :-) */
	if (nargs != INSUBYTE()) IEARLY_ERROR(error_wrong_parameters);
	for (ulong i = 0; i < nargs; i++)
	  ((struct variable *)me.u.mudlle.locals->data[i])->vvalue
            = FAST_GET(i);
	break;
      case op_varargs:		/* Another CISCy instruction... */
	{
	  struct vector *args = (struct vector *)unsafe_allocate_record(
            type_vector, nargs);

	  RESTORE_STACK();
	  RESTORE_INS();
	  for (int j = 0; j < nargs; j++)
	    args->data[nargs - j - 1] = FAST_GET(j);

	  ((struct variable *)me.u.mudlle.locals->data[0])->vvalue = args;
	  me.u.mudlle.nargs = 1;
	  FAST_POPN(nargs);
	  break;
	}
      case op_discard:
	FAST_POPN(1);
	break;
      case op_exit_n:
        {
          value result = FAST_POP();
          FAST_POPN(INSUBYTE());
          FAST_PUSH(result);
          break;
        }
      case op_pop_n:
	FAST_POPN(INSUBYTE());
	break;
      case op_dup: {
	value v = FAST_GET(0);
	FAST_PUSH(v);
	break;
      }
      case op_branch_z1:
	if (!istrue(FAST_POP())) goto branch1;
	(void)INSBYTE();
	break;
      case op_branch_z2:
	if (!istrue(FAST_POP())) goto branch2;
	(void)INSWORD();
	break;
      case op_branch_nz1:
	if (istrue(FAST_POP())) goto branch1;
	(void)INSBYTE();
	break;
      case op_branch_nz2:
	if (istrue(FAST_POP())) goto branch2;
	(void)INSWORD();
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
	  sbyte offset = INSBYTE();

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
	  word offset = INSWORD();

	  ins_index += offset;
	  ins += offset;
	  break;
	}

#define LOCAL me.u.mudlle.locals->data[INSUBYTE()]
#define CLOSURE me.u.mudlle.fn->variables[INSUBYTE()]

#define RECALL(access) FAST_PUSH(((struct variable *)access)->vvalue)
#define VREF(access)   FAST_PUSH(access)
#define ASSIGN(access) do {                                     \
          struct variable *_var = (struct variable *)(access);  \
          _var->vvalue = FAST_GET(0);                           \
        } while (0)
#define ADDCLOSURE(access) do {                                 \
          new_closure->variables[new_var++] = (access);         \
        } while (0)

      case op_clear_local: ((struct variable *)LOCAL)->vvalue = NULL; break;
      case op_recall + local_var:   RECALL(LOCAL);               break;
      case op_recall + closure_var: RECALL(CLOSURE);             break;
      case op_recall + global_var:  FAST_PUSH(GVAR(INSUWORD())); break;
      case op_vref   + local_var:   VREF(LOCAL);                 break;
      case op_vref   + closure_var: VREF(CLOSURE);               break;
      case op_vref   + global_var:  abort();                     break;
      case op_assign + local_var:   ASSIGN(LOCAL);               break;
      case op_assign + closure_var: ASSIGN(CLOSURE);             break;
      case op_assign + global_var:
	{
	  ulong goffset = INSUWORD();

	  if (GCONSTANT(goffset)) IERROR(error_variable_read_only);
	  GVAR(goffset) = FAST_GET(0);
	  break;
	}
      case op_define:
        /* like op_assign global, but no error checking */
	GVAR(INSUWORD()) = FAST_GET(0);
	break;
      case op_closure_var + local_var: ADDCLOSURE(LOCAL); break;
      case op_closure_var + closure_var: ADDCLOSURE(CLOSURE); break;

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
	  if (integerp(arg1) && integerp(arg2))	\
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

      case op_builtin_add:
        {
          value arg2 = FAST_POP();
          value arg1 = FAST_GET(0);
          if (integerp(arg1) && integerp(arg2))
            FAST_SET(0, (value)((long)arg1 + (long)arg2 - 1));
          else if (TYPE(arg1, type_string) && TYPE(arg2, type_string))
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
	INTEGER_OP((value)((long)arg1 - (long)arg2 + 1), minus);
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
          arg1 = code_set(FAST_GET(0), arg1, arg2);
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
          value arg1 = FAST_GET(INSUBYTE());
          if (~intval(arg2) & (1U << TYPEOF(arg1))) IERROR(error_bad_type);
          break;
        }

      /* The type checks */
      case op_typecheck + type_integer:
        {
          value arg1 = FAST_GET(INSUBYTE());
          if (!integerp(arg1)) IERROR(error_bad_type);
          break;
        }

      case op_typecheck + type_code:
      case op_typecheck + type_closure:
      case op_typecheck + type_variable:
      case op_typecheck + type_internal:
      case op_typecheck + type_primitive:
      case op_typecheck + type_varargs:
      case op_typecheck + type_secure:
      case op_typecheck + type_string:
      case op_typecheck + type_vector:
      case op_typecheck + type_pair:
      case op_typecheck + type_symbol:
      case op_typecheck + type_table:
      case op_typecheck + type_private:
      case op_typecheck + type_object:
      case op_typecheck + type_character:
      case op_typecheck + type_gone:
      case op_typecheck + type_outputport:
      case op_typecheck + type_mcode:
      case op_typecheck + type_float:
      case op_typecheck + type_bigint:
      case op_typecheck + type_reference:
        CASSERT_EXPR(last_type == 23);
        {
          value arg1 = FAST_GET(INSUBYTE());
          if (!TYPE(arg1, byteop - op_typecheck))
            IERROR(error_bad_type);
          break;
        }

      case op_typecheck + type_null:
        {
          value arg1 = FAST_GET(INSUBYTE());
          if (arg1) IERROR(error_bad_type);
          break;
        }

      case op_typecheck + stype_none:
	IERROR(error_bad_type);

      case op_typecheck + stype_any:
        break;

      case op_typecheck + stype_function:
	{
	  value arg1 = FAST_GET(INSUBYTE());
	  if (!pointerp(arg1)) IERROR(error_bad_type);

	  mtype type = ((struct obj *)arg1)->type;
	  if (!(type == type_closure || type == type_primitive ||
		type == type_varargs || type == type_secure))
	    IERROR(error_bad_type);
	  break;
	}

      case op_typecheck + stype_list:
        CASSERT_EXPR(last_synthetic_type == 27);
        {
          value arg1 = FAST_GET(INSUBYTE());
          if (arg1 && !TYPE(arg1, type_pair))
            IERROR(error_bad_type);
          break;
        }

      default: abort();
      }
  }
 done:
  call_stack = me.next;

  me.u.mudlle.code->instruction_count += instruction_number - start_ins;

  if (session_context->recursion_count)
    ++session_context->recursion_count;
}


/* Interface to machine code. */

#define __POPARG(N) __PRIMARG(N) = FAST_GET(nargs - N)
#define __INVOKE(N)                             \
  case N:                                       \
    {                                           \
      RESTORE_STACK();                          \
      CONCATSEMI(N, __POPARG);                  \
      FAST_POPN(N);                             \
      return invoke ## N(c, PRIMARGNAMES ## N); \
    }
static inline value invoke_stack(struct closure *c, int nargs)
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
     The stack must contain at least nargs entries.
   Effects: Executes c(nargs arguments taken from the stack)
   Returns: c's result
*/
{
  value *stkpos;		/* Pointer into stack */

  switch (nargs)
    {
    case 0:
      return invoke0(c);
    DOPRIMARGS(__INVOKE)
    default:
      {
        GCPRO1(c);
        struct vector *extra
          = (struct vector *)unsafe_allocate_record(type_internal, nargs);
        UNGCPRO();
        RESTORE_STACK();
        for (int i = nargs; i > 0; )
          extra->data[--i] = FAST_POP();
        return invoke(c, extra);
      }
    }
}

#undef __POPARG
#undef __INVOKE

/* Interface from machine code - backend specific */

#ifdef AMIGA

value __asm interpreter_start(register __d1 struct closure *_c,
			      register __d0 uword argcount,
			      register __d2 value arg1,
			      register __d3 value _arg2,
			      register __d4 value _arg3,
			      register __d5 value _extra)
{
  value arg2 = _arg2, arg3 = _arg3, extra = _extra;
  struct closure *c = _c;

  if (argcount > 0)
    {
      GCPRO4(arg2, arg3, extra, c);
      stack_push(arg1);
      if (argcount > 1)
	{
	  stack_push(arg2);
	  if (argcount > 2)
	    {
	      stack_push(arg3);
	      if (argcount > 3)
		if (argcount == 4) stack_push(extra);
		else
		  {
		    int i, l = argcount - 3;

		    for (i = 0; i < l; i++)
		      stack_push(((struct vector *)extra)->data[i]);
		  }
	    }
	}
      UNGCPRO();
    }

  do_interpret(c, argcount);

  return stack_pop();
}

#endif

#ifdef sparc

value interpret_closure;
int interpret_nargs;

value interpreter_start(value arg1, value arg2, value arg3, value arg4,
                        value arg5, value arg6, value first_extra, ...)
{
  if (interpret_nargs > 0)
    {
      value *stkpos;
      struct dynpro *protection = NULL;
      int nextra = interpret_nargs - 6, i;
      value *extra = NULL;

      if (nextra > 0) /* Fetch args from stack, but can't allocate as they
			 might be destroyed. */
	{
	  /* Protect the values */
	  protection = alloca(nextra * sizeof(struct dynpro));
	  memset((char *)protection, 0, nextra * sizeof(struct dynpro));
	  extra = &first_extra;
	  for (i = 0; i < nextra; i++) dynpro(protection + i, extra[i]);
	}

      /* Reserve stack space */
      GCPRO6(arg1, arg2, arg3, arg4, arg5, arg6);
      stack_reserve(interpret_nargs);
      UNGCPRO();

      RESTORE_STACK();

      FAST_PUSH(arg1);
      if (interpret_nargs > 1)
	{
	  FAST_PUSH(arg2);
	  if (interpret_nargs > 2)
	    {
	      FAST_PUSH(arg3);
	      if (interpret_nargs > 3)
		{
		  FAST_PUSH(arg4);
		  if (interpret_nargs > 4)
		    {
		      FAST_PUSH(arg5);
		      if (interpret_nargs > 5)
			{
			  FAST_PUSH(arg6);
			  if (interpret_nargs > 6)
			    for (i = 0; i < nextra; i++)
			      {
				FAST_PUSH(protection[i].obj);
				undynpro(protection + i);
			      }
			}
		    }
		}
	    }
	}
    }

  do_interpret(interpret_closure, interpret_nargs);

  return stack_pop();
}

#endif

#ifdef i386

value interpreter_start(struct closure *closure, int nargs, const value *args)
{
  if (nargs > 0)
    {
      value *stkpos;
      /* Reserve stack space */
      GCPRO1(closure);
      stack_reserve(nargs);
      UNGCPRO();
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

void interpret_init(void)
{
#ifdef sparc
  staticpro(&interpret_closure);
#endif
}
