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

#include "alloc.h"
#include "call.h"
#include "code.h"
#include "context.h"
#include "error.h"
#include "global.h"
#include "interpret.h"
#include "stack.h"

#include "runtime/runtime.h"
#include "runtime/stringops.h"
#include "runtime/basic.h"

/* As good a place as any other */
const char COPYRIGHT[] = "\
Copyright (c) 1993-2006 David Gay and Gustav Hållberg\n\
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

static INLINE value invoke_stack(struct closure *c, int nargs);

/* Macros for fast access to the GC'ed stack & code structures.
   RESTORE_INS & RESTORE_STACK must be called after anything that may
   have caused a GC
*/

#define RESTORE_STACK() (stkpos = stack->values->data + intval(stack->used))
#define _FAST_POPN(n) (stack->used = (value)((long)stack->used - ((n) + (n))), *(stkpos -= (n)))
#define FAST_POP() (_FAST_POPN(1))
#define FAST_POPN(n) ((void)_FAST_POPN(n))
#define FAST_PUSH(v) do { stack->used = (value)((long)stack->used + 2); *stkpos++ = (v); } while(0)
#define FAST_GET(n) (stkpos[-((n) + 1)])
#define FAST_SET(n, v) (stkpos[-((n) + 1)]= (v))

#define RESTORE_INS() (ins = ((instruction *)me.u.mudlle.code) + ins_index)
#define INSUBYTE() (ins_index++, *ins++)
#define INSBYTE() ((sbyte)INSUBYTE())
#define INSUWORD() (byte1 = *ins++, ins_index += 2, (byte1 << 8) + *ins++)
#define INSWORD() ((word)INSUWORD())

#define SAVE_OFFSET() do { me.u.mudlle.offset = ins_index; } while (0)

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
  struct call_stack me, primop;
  ulong ins_index, new_var = 0;
  struct closure *new_closure = NULL;
  instruction *ins, byteop;
  ubyte byte1;
  value *stkpos;		/* Pointer into stack */
  ulong i;
  static ulong instruction_number;
  ulong start_ins;
  value arg1, arg2, result;
  struct obj *called;
  struct primitive *pop;
  const struct primitive_ext *op;
  

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

  /* Initialise call stack entry */
  me.type = call_bytecode;
  me.u.mudlle.fn = fn;
  me.u.mudlle.code = fn->code;
  me.u.mudlle.locals = NULL;
  me.u.mudlle.nargs = nargs;
  me.u.mudlle.offset = -1;
  me.next = call_stack;
  call_stack = &me;

  /* Pre-initialise call stack entry for calls to C primitives */
  primop.next = &me;
  primop.type = call_c;
  primop.u.c.arg1 = primop.u.c.arg2 = primop.u.c.arg3 = primop.u.c.arg4 = primop.u.c.arg5 = NULL;

  /* Make local variables */
  me.u.mudlle.locals = allocate_locals(me.u.mudlle.code->nb_locals);

  /* Profiling */
  me.u.mudlle.code->call_count++;
  start_ins = instruction_number;

  if (me.u.mudlle.code->seclevel < minlevel)
    early_runtime_error(error_security_violation);

  stack_reserve(me.u.mudlle.code->stkdepth); /* Ensure enough space on stack */
  RESTORE_STACK();

  /* Loop over instructions, executing each one */
  /* As code may move with gc, we can't have a pointer into it.
     So we base ourselves on code (which is protected) and pretend
     that is an array of instructions. */
  ins_index = (instruction *)(&me.u.mudlle.code->constants[me.u.mudlle.code->nb_constants]) -
    (instruction *)me.u.mudlle.code;
  RESTORE_INS();

  for (;;) {
    instruction_number++;
    switch (byteop = INSUBYTE())
      {
      case op_return: goto done;

      case op_constant1:
	FAST_PUSH(me.u.mudlle.code->constants[INSUBYTE()]);
	GCCHECK(FAST_GET(0));
	break;
      case op_constant2:
	FAST_PUSH(me.u.mudlle.code->constants[INSUWORD()]);
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
	new_closure->code = (struct code *)me.u.mudlle.code->constants[INSUBYTE()];
	GCCHECK(new_closure->code);
	/*TYPEIS(new_closure->code, type_code);*/
	/* new_closure is now safe ... */
	break;
      case op_closure_code2:
	new_closure->code = (struct code *)me.u.mudlle.code->constants[INSUWORD()];
	GCCHECK(new_closure->code);
	/*TYPEIS(new_closure->code, type_code);*/
	/* new_closure is now safe ... */
	break;

#define C_SETARG(n, arg) C_ARG(n) = arg; GCCHECK(C_ARG(n))
#define C_ARG(n) primop.u.c.arg ## n

#define C_START_CALL(n)				\
        SAVE_OFFSET();				\
	pop->call_count++;			\
        primop.u.c.prim = pop;			\
        op = pop->op;				\
	primop.u.c.nargs = n;			\
	call_stack = &primop

#define C_END_CALL()				\
	call_stack = &me;			\
	GCCHECK(result);			\
	RESTORE_STACK();			\
	RESTORE_INS();				\
	FAST_PUSH(result);

      case op_execute_primitive1:
	pop = GVAR(INSUWORD());
	C_START_CALL(1);
	C_SETARG(1, FAST_POP());
	seclevel = me.u.mudlle.code->seclevel;
	result = op->op(C_ARG(1));
	C_END_CALL();
	break;
      case op_execute_primitive2:
	pop = GVAR(INSUWORD());
	C_START_CALL(2);
	C_SETARG(2, FAST_POP());
	C_SETARG(1, FAST_POP());
	seclevel = me.u.mudlle.code->seclevel;
	result = op->op(C_ARG(1), C_ARG(2));
	C_END_CALL();
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
	seclevel = DEFAULT_SECLEVEL;
	switch (called->type)
	  {
	  case type_varargs:
	    {
	      struct vector *args;

	      pop = (struct primitive *)called;

	      /* call_stack must be set after allocate_record, so can't use
		 C_START_CALL macro */
	      pop->call_count++;
	      primop.u.c.prim = pop;
	      op = pop->op;
	      primop.u.c.nargs = 1;

	      args = (struct vector *)unsafe_allocate_record(type_vector, nargs);
	      RESTORE_STACK();
	      for (i = nargs; i > 0;) args->data[--i] = FAST_POP();

	      call_stack = &primop;
	      C_SETARG(1, args);
	      result = op->op(args, nargs);
	      C_END_CALL();
	      break;
	    }

	  case type_secure:
	    pop = (struct primitive *)called;
	    C_START_CALL(nargs);
	    if (seclevel < op->seclevel)
	      IEARLY_ERROR(error_security_violation);
	    goto execute_primitive;

	  case type_primitive:
	    pop = (struct primitive *)called;
	    C_START_CALL(nargs);

	  execute_primitive:
	    if (nargs != op->nargs)
	      IEARLY_ERROR(error_wrong_parameters);

	    switch (nargs)
	      {
	      case 0:
		result = op->op();
		break;
	      case 1:
		C_SETARG(1, FAST_POP());
		result = op->op(C_ARG(1));
		break;
	      case 2:
		C_SETARG(2, FAST_POP());
		C_SETARG(1, FAST_POP());
		result = op->op(C_ARG(1), C_ARG(2));
		break;
	      case 3:
		C_SETARG(3, FAST_POP());
		C_SETARG(2, FAST_POP());
		C_SETARG(1, FAST_POP());
		result = op->op(C_ARG(1), C_ARG(2), C_ARG(3));
		break;
	      case 4:
		C_SETARG(4, FAST_POP());
		C_SETARG(3, FAST_POP());
		C_SETARG(2, FAST_POP());
		C_SETARG(1, FAST_POP());
		result = op->op(C_ARG(1), C_ARG(2), C_ARG(3), C_ARG(4));
		break;
	      case 5:
		C_SETARG(5, FAST_POP());
		C_SETARG(4, FAST_POP());
		C_SETARG(3, FAST_POP());
		C_SETARG(2, FAST_POP());
		C_SETARG(1, FAST_POP());
		result = op->op(C_ARG(1), C_ARG(2), C_ARG(3), C_ARG(4),
				C_ARG(5));
		break;
	      default:
		/* A primitive can have a maximum of 5 arguments.
		 * If you need more, use VAROP() and check the argument count
		 * in the primitive */
		CASSERT_STMT(MAX_PRIMITIVE_ARGS == 5);
		assert(0);
	      }
	    C_END_CALL();
	    break;

	  case type_closure:
	    {
	      struct closure *c = (struct closure *)called;

	      SAVE_OFFSET();
	      if (c->code->o.type == type_mcode)
		{
		  result = invoke_stack(c, nargs);
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

	pop = (struct primitive *)called;
	C_START_CALL(nargs);
	seclevel = me.u.mudlle.code->seclevel;
	if (seclevel < op->seclevel)
	  IEARLY_ERROR(error_security_violation);
	goto execute_primitive;

      case op_execute_primitive:
	called = FAST_POP();
	nargs = INSUBYTE();

	/* Compiler only generates this for primitives in
	   protected modules (normally system) */
	assert(pointerp(called) && called->type == type_primitive);

	pop = (struct primitive *)called;
	C_START_CALL(nargs);
	seclevel = me.u.mudlle.code->seclevel;
	goto execute_primitive;

      case op_execute_varargs:
	{
	  struct vector *args;

	  called = FAST_POP();
	  nargs = INSUBYTE();

	  /* Compiler only generates this for varargs primitives in
	     protected modules (normally system) */
	  assert(pointerp(called) && called->type == type_varargs);

	  seclevel = me.u.mudlle.code->seclevel;

	  pop = (struct primitive *)called;

	  /* call_stack must be set after allocate_record, so can't use
	     C_START_CALL macro */
	  pop->call_count++;
	  primop.u.c.prim = pop;
	  op = pop->op;
	  primop.u.c.nargs = 1;

	  args = (struct vector *)unsafe_allocate_record(type_vector, nargs);
	  RESTORE_STACK();
	  for (i = nargs; i > 0;) args->data[--i] = FAST_POP();

	  call_stack = &primop;
	  SAVE_OFFSET();
	  C_SETARG(1, args);
	  result = op->op(args, nargs);
	  C_END_CALL();
	  break;
	}

      case op_argcheck:		/* A CISCy instruction :-) */
	if (nargs != INSUBYTE()) IEARLY_ERROR(error_wrong_parameters);
	for (i = 0; i < nargs; i++)
	  ((struct variable *)me.u.mudlle.locals->data[i])->vvalue = FAST_GET(i);
	break;
      case op_varargs:		/* Another CISCy instruction... */
	{
	  struct vector *args = (struct vector *)unsafe_allocate_record(type_vector, nargs);
	  int j;

	  RESTORE_STACK();
	  RESTORE_INS();
	  for (j = 0; j < nargs; j++)
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
	result = FAST_POP();
	i = INSUBYTE();
	FAST_POPN(i);
	FAST_PUSH(result);
	break;
      case op_pop_n:
	i = INSUBYTE();
	FAST_POPN(i);
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
#define ASSIGN(access) { \
  struct variable *var = (struct variable *)(access); \
  var->vvalue = FAST_GET(0); }
#define ADDCLOSURE(access) new_closure->variables[new_var++] = (access)

      case op_clear_local: ((struct variable *)LOCAL)->vvalue = NULL; break;
      case op_recall + local_var: RECALL(LOCAL); break;
      case op_recall + closure_var: RECALL(CLOSURE); break;
      case op_recall + global_var: FAST_PUSH(GVAR(INSUWORD())); break;
      case op_assign + local_var: ASSIGN(LOCAL); break;
      case op_assign + closure_var: ASSIGN(CLOSURE); break;
      case op_assign + global_var: 
	{
	  ulong goffset = INSUWORD();

	  if (GCONSTANT(goffset)) IERROR(error_variable_read_only);
	  GVAR(goffset) = FAST_GET(0);
	  break;
	}
      case op_define:		/* Like op_assign global, but no error checking */
	GVAR(INSUWORD()) = FAST_GET(0);
	break;
      case op_closure_var + local_var: ADDCLOSURE(LOCAL); break;
      case op_closure_var + closure_var: ADDCLOSURE(CLOSURE); break;

	/* The builtin operations */
      case op_builtin_eq:
	arg1 = FAST_POP();
	FAST_SET(0, makebool(FAST_GET(0) == arg1));
	break;
      case op_builtin_neq:
	arg1 = FAST_POP();
	FAST_SET(0, makebool(FAST_GET(0) != arg1));
	break;

#define INTEGER_OP(op) do {			\
	  arg2 = FAST_POP();			\
	  arg1 = FAST_GET(0);			\
	  if (integerp(arg1) && integerp(arg2))	\
	    FAST_SET(0, op);			\
	  else					\
	    IERROR(error_bad_type);		\
	} while (0)
	
      case op_builtin_lt:
	INTEGER_OP(makebool((long)arg1 < (long)arg2));
	break;
      case op_builtin_le:
	INTEGER_OP(makebool((long)arg1 <= (long)arg2));
	break;
      case op_builtin_gt:
	INTEGER_OP(makebool((long)arg1 > (long)arg2));
	break;
      case op_builtin_ge:
	INTEGER_OP(makebool((long)arg1 >= (long)arg2));
	break;

      case op_builtin_add:
	arg2 = FAST_POP();
	arg1 = FAST_GET(0);
	if (integerp(arg1) && integerp(arg2))
	  FAST_SET(0, (value)((long)arg1 + (long)arg2 - 1));
	else if (TYPE(arg1, type_string) && TYPE(arg2, type_string))
	  {
	    arg1 = string_append(arg1, arg2);
	    RESTORE_INS();
	    RESTORE_STACK();
	    FAST_SET(0, arg1);
	  }
	else
	  IERROR(error_bad_type);
	break;

      case op_builtin_sub:
	INTEGER_OP((value)((long)arg1 - (long)arg2 + 1));
	break;

      case op_builtin_bitand:
	INTEGER_OP((value)((long)arg1 & (long)arg2));
	break;
      case op_builtin_bitor:
	INTEGER_OP((value)((long)arg1 | (long)arg2));
	break;

      case op_builtin_not:
	FAST_SET(0, makebool(!istrue(FAST_GET(0))));
	break;

	/* These could be optimised */
      case op_builtin_ref:
	SAVE_OFFSET();
	arg2 = FAST_POP();
	arg1 = code_ref(FAST_GET(0), arg2);
	GCCHECK(arg1);
	RESTORE_STACK();
	RESTORE_INS();
	FAST_SET(0, arg1);
	break;

      case op_builtin_set:
	SAVE_OFFSET();
	arg2 = FAST_POP();
	arg1 = FAST_POP();
	arg1 = code_set(FAST_GET(0), arg1, arg2);
	GCCHECK(arg1);
	RESTORE_STACK();
	RESTORE_INS();
	FAST_SET(0, arg1);
	break;

      /* The type checks */
      case op_typecheck + type_integer:
	arg1 = FAST_GET(INSUBYTE());
	if (!integerp(arg1)) IERROR(error_bad_type);
	break;

      case op_typecheck + type_string:
      case op_typecheck + type_vector:
      case op_typecheck + type_pair:
      case op_typecheck + type_symbol:
      case op_typecheck + type_table:
      case op_typecheck + type_object:
      case op_typecheck + type_character:
      case op_typecheck + type_float:
      case op_typecheck + type_bigint:
      case op_typecheck + type_gone:
	arg1 = FAST_GET(INSUBYTE());
	if (!TYPE(arg1, byteop - op_typecheck)) IERROR(error_bad_type);
	break;

      case op_typecheck + type_null:
	arg1 = FAST_GET(INSUBYTE());
	if (arg1) IERROR(error_bad_type);
	break;

      case op_typecheck + stype_none:
	IERROR(error_bad_type);

      case op_typecheck + stype_function:
	{
	  int type;

	  arg1 = FAST_GET(INSUBYTE());
	  if (!pointerp(arg1)) IERROR(error_bad_type);

	  type = ((struct obj *)arg1)->type;
	  if (!(type == type_closure || type == type_primitive ||
		type == type_varargs || type == type_secure))
	    IERROR(error_bad_type);
	  break;
	}	

      case op_typecheck + stype_list:
	arg1 = FAST_GET(INSUBYTE());
	if (arg1 && !TYPE(arg1, type_pair)) IERROR(error_bad_type);
	break;

      default: assert(0);
      }
  }
 done:
  call_stack = me.next;

  me.u.mudlle.code->instruction_count += instruction_number - start_ins;

  if (session_context->recursion_count)
    ++session_context->recursion_count;
}


/* Interface to machine code. */

static INLINE value invoke_stack(struct closure *c, int nargs)
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
     The stack must contain at least nargs entries.
   Effects: Executes c(nargs arguments taken from the stack)
   Returns: c's result
*/
{
  struct gcpro gcpro1;
  struct vector *extra;
  value *stkpos;		/* Pointer into stack */
  int i;
  value arg1, arg2, arg3;

  switch (nargs)
    {
    case 0:
      return invoke0(c);
    case 1:
      RESTORE_STACK();
      return invoke1(c, FAST_POP());
    case 2:
      RESTORE_STACK();
      arg2 = FAST_POP(); arg1 = FAST_POP();
      return invoke2(c, arg1, arg2);
    case 3:
      RESTORE_STACK();
      arg3 = FAST_POP(); arg2 = FAST_POP(); arg1 = FAST_POP();
      return invoke3(c, arg1, arg2, arg3);
    default:
      GCPRO1(c);
      extra = (struct vector *)unsafe_allocate_record(type_internal, nargs);
      UNGCPRO();
      RESTORE_STACK();
      for (i = nargs; i > 0; ) extra->data[--i] = FAST_POP();
      return invoke(c, extra);
    }
}

/* Interface from machine code - backend specific */

#ifdef AMIGA

value __asm interpreter_start(register __d1 struct closure *_c,
			      register __d0 uword argcount,
			      register __d2 value arg1,
			      register __d3 value _arg2,
			      register __d4 value _arg3,
			      register __d5 value _extra)
{
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
  value arg2 = _arg2, arg3 = _arg3, extra = _extra;
  struct closure *c = _c;

  if (argcount > 0)
    {
      GCPRO2(arg2, arg3); GCPRO(gcpro3, extra); GCPRO(gcpro4, c);
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

value interpreter_start(value arg1, value arg2, value arg3, value arg4, value arg5,
			value arg6, value first_extra, ...)
{
  if (interpret_nargs > 0)
    {
      struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5, gcpro6;
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
      GCPRO2(arg1, arg2); GCPRO(gcpro3, arg3); GCPRO(gcpro4, arg4);
      GCPRO(gcpro5, arg5); GCPRO(gcpro6, arg6);
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

int interpret_nargs;

value interpreter_start(value closure, ...)
{
  if (interpret_nargs > 0)
    {
      value *stkpos;
      value *extra = NULL;
      struct gcpro gcpro1;
      int i;

      /* Reserve stack space */
      GCPRO1(closure);
      stack_reserve(interpret_nargs);
      UNGCPRO();
      RESTORE_STACK();

      extra = &closure + 1;
      for (i = 0; i < interpret_nargs; i++) FAST_PUSH(extra[i]);
    }

  do_interpret(closure, interpret_nargs);

  return stack_pop();
}

#endif

void interpret_init(void)
{
#ifdef sparc
  staticpro(&interpret_closure);
#endif
}
