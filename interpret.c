/* $Log: interpret.c,v $
 * Revision 1.46  1995/07/15  15:24:26  arda
 * Context cleanup.
 * Remove GCDEBUG.
 *
 * Revision 1.45  1995/06/28  20:55:08  arda
 * Bugs
 *
 * Revision 1.44  1995/04/29  20:05:20  arda
 * fix
 *
 * Revision 1.43  1995/01/22  15:11:42  arda
 * Linux patches.
 *
 * Revision 1.42  1994/10/09  15:14:27  arda
 * mudlle libraries.
 *
 * Revision 1.41  1994/10/09  06:42:19  arda
 * Libraries
 * Type inference
 * Many minor improvements
 *
 * Revision 1.40  1994/09/16  13:07:12  arda
 * Rename protect to catch.
 * New protect/unprotect functions (like dynpro/undynpro).
 *
 * Revision 1.39  1994/09/16  09:12:02  arda
 * Optimise main call path.
 *
 * Revision 1.38  1994/09/15  19:46:39  arda
 * Performance improvements:
 *   setjmp -> _setjmp (setjmp is horrendously slow)
 *   cold_protect
 * reset_limits split from reset_interpreter
 * fix division of negative numbers
 * Add ?\{n,r,t}
 * gc_size returns "mutable" size
 *
 * Revision 1.37  1994/08/29  13:17:21  arda
 * Contagious immutability.
 * Global array of values instead of variables.
 * Direct recursion.
 *
 * Revision 1.36  1994/08/26  04:35:37  arda
 * Fix bug in builtins.
 * Check gen0 size by type.
 *
 * Revision 1.35  1994/08/23  09:29:51  arda
 * Final(?) changes for mudlle compiler.
 *
 * Revision 1.34  1994/08/22  18:02:57  arda
 * Minor fixes.
 *
 * Revision 1.33  1994/08/22  11:18:33  arda
 * Moved code allocation to ins.c
 * Changes for mudlle compiler in MUME.
 *
 * Revision 1.32  1994/08/16  19:16:01  arda
 * Mudlle compiler for sparc now fully functional (68k compiler now needs
 * updating for primitives).
 * Changes to allow Sparc trap's for runtime errors.
 * Also added flags to primitives for better calling sequences.
 *
 * Revision 1.29  1994/04/12  20:11:49  arda
 * (MD) Alignments and fixes + unknown from others...
 *
 * Revision 1.28  1994/03/23  14:31:24  arda
 * *** empty log message ***
 *
 * Revision 1.27  1994/03/08  01:50:37  arda
 * (MD) New Istari.
 *
 * Revision 1.26  1994/02/24  08:32:57  arda
 * Owl: New error messages.
 *
 * Revision 1.25  1994/02/12  17:24:53  arda
 * Owl: Better code generated.
 *
 * Revision 1.24  1994/02/11  09:58:55  dgay
 * Owl: -Wall
 *      new shared string handling
 *      configuration file
 *
 * Revision 1.23  1994/02/03  22:50:00  dgay
 * Owl: C closures.
 *
 * Revision 1.22  1994/02/03  19:21:33  arda
 * nothing special(2)
 *
 * Revision 1.21  1994/01/08  12:49:48  dgay
 * Owl: Improved code generation for blocks (they are not implemented
 * as 0 argument functions anymore, they are folded into the current
 * function instead).
 *
 * Revision 1.20  1993/10/03  14:07:14  dgay
 * Bumper disun8 update.
 *
 * Revision 1.19  1993/08/06  07:05:19  un_autre
 * fix
 *
 * Revision 1.18  1993/07/21  20:36:45  un_mec
 * Owl: Added &&, ||, optimised if.
 *      Added branches to the intermediate language.
 *      Separated destiniation language generation into ins module
 *      (with some peephole optimisation)
 *      Standalone version of mudlle (mkf, runtime/mkf, mudlle.c) added to CVS
 *
 * Revision 1.17  1993/05/02  13:02:38  un_mec
 * Owl: ARGH! Bugs.
 *
 * Revision 1.16  1993/05/02  07:37:41  un_mec
 * Owl: New output (mudlle ports).
 *
 * Revision 1.15  1993/04/24  16:49:54  un_autre
 * Owl's
 *
 * Revision 1.13  1993/04/22  18:58:47  un_autre
 * (MD) & Owl. Bug fixes. /player fixes. EVER_WHINER flag. saving_spells adjusted.
 *
 * Revision 1.12  1993/04/17  10:01:18  un_autre
 * Various
 *
 * Revision 1.11  1993/04/11  09:20:27  un_autre
 * Owl: Bug fixes
 *
 * Revision 1.10  1993/04/10  09:17:00  un_mec
 * Owl: Debug mudlle.
 *
 * Revision 1.9  1993/03/29  09:24:02  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.5  1993/03/17  12:49:40  dgay
 * Fixed GC of help strings in code blocks.
 * Added security features.
 *
 * Revision 1.4  1993/03/16  10:44:18  dgay
 * Optimisations continue.
 *
 * Revision 1.3  1993/03/14  16:14:19  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.7  1993/02/14  21:09:05  un_mec
 * Owl: Fix code limits.
 *
 * Revision 1.6  1993/01/30  12:13:36  un_mec
 * Owl: Mudlle reactions installed, with loading and editing commands.
 *
 * Revision 1.5  1993/01/26  09:48:52  un_mec
 * Owl:
 * - Limit mudlle execution time (prevent infinite loops).
 * - Add mudlle reaction procedures.
 *
 * Revision 1.4  1993/01/11  16:15:10  un_mec
 * Added read-only variables.
 *
 * Revision 1.3  1993/01/08  23:57:07  un_mec
 * Owl: Allow characters and objects to appear in mudlle.
 *
 * Revision 1.2  1992/12/30  14:10:45  un_mec
 * Owl:
 * Several changes:
 * - Variables don't have separate value & function cells, instead their are
 *   now 2 types: type_function & type_variable.
 * - print_value: New types (list, vector), printing rationalised.
 * - New type: list (Lisp style pair)
 * - lexer.l: Debug read_from_string
 * - debug_level & DEBUG macro provided to help debugging.
 *
 * Revision 1.1  1992/12/27  21:41:12  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

static char rcsid[] = "$Id: interpret.c,v 1.46 1995/07/15 15:24:26 arda Exp $";

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "mudlle.h"
#include "interpret.h"
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

static INLINE value invoke_stack(struct closure *c, int nargs);

/* Macros for fast access to the GC'ed stack & code structures.
   RESTORE_INS & RESTORE_STACK must be called after anything that may
   have caused a GC
*/

#define RESTORE_STACK() (stkpos = stack->values->data + intval(stack->used))
#define FAST_POP() (FAST_POPN(1))
#define FAST_POPN(n) (stack->used = (value)((long)stack->used - ((n) + (n))), *(stkpos -= (n)))
#define FAST_PUSH(v) do { stack->used = (value)((long)stack->used + 2); *stkpos++ = (v); } while(0)
#define FAST_GET(n) (stkpos[-((n) + 1)])
#define FAST_SET(n, v) (stkpos[-((n) + 1)]= (v))

#define RESTORE_INS() (ins = ((instruction *)me.u.mudlle.code) + ins_index)
#define INSUBYTE() (ins_index++, *ins++)
#define INSBYTE() ((byte)INSUBYTE())
#define INSUWORD() (byte1 = *ins++, ins_index += 2, (byte1 << 8) + *ins++)
#define INSWORD() ((word)INSUWORD())

void do_interpret(struct closure *fn, int nargs)
{
  struct call_stack me, primop;
  ulong ins_index, new_var;
  struct closure *new_closure;
  instruction *ins, byteop;
  ubyte byte1;
  value *stkpos;		/* Pointer into stack */
  ulong i;
  static ulong instruction_number;
  ulong start_ins;
  value arg1, arg2, result;
  struct obj *called;
  struct primitive *pop;
  struct primitive_ext *op;
  
#ifdef MUME
  if (!--session_context->call_count) runtime_error(error_loop);
#endif

#ifdef INTERRUPT
  check_interrupt();
#endif

  /* Initialise call stack entry */
  me.type = call_bytecode;
  me.u.mudlle.fn = fn;
  me.u.mudlle.code = fn->code;
  me.u.mudlle.locals = NULL;
  me.u.mudlle.nargs = nargs;
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

  seclevel = me.u.mudlle.code->seclevel;	/* Execute at right sec. level */
  if (seclevel < minlevel) early_runtime_error(error_security_violation);

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

#define C_START_CALL(n) \
	pop->call_count++; \
        primop.u.c.op = op = pop->op; \
	primop.u.c.nargs = n; \
	call_stack = &primop

#define C_END_CALL() \
	call_stack = &me; \
	GCCHECK(result); \
	RESTORE_STACK(); \
	RESTORE_INS(); \
	FAST_PUSH(result); \
	seclevel = me.u.mudlle.code->seclevel

      case op_execute_primitive1:
	pop = GVAR(INSUWORD());
	C_START_CALL(1);
	C_SETARG(1, FAST_POP());
	result = op->op(C_ARG(1));
	C_END_CALL();
	break;
      case op_execute_primitive2:
	pop = GVAR(INSUWORD());
	C_START_CALL(2);
	C_SETARG(2, FAST_POP());
	C_SETARG(1, FAST_POP());
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
	if (!pointerp(called)) runtime_error(error_bad_function);
	switch (called->type)
	  {
	  case type_varargs:
	    {
	      struct vector *args;

	      pop = (struct primitive *)called;

	      /* call_stack must be set after allocate_record, so can't use
		 C_START_CALL macro */
	      pop->call_count++;
	      primop.u.c.op = op = pop->op;
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
	      early_runtime_error(error_security_violation);
	    goto execute_primitive;

	  case type_primitive:
	    pop = (struct primitive *)called;
	    C_START_CALL(nargs);

	  execute_primitive:
	    if (nargs != op->nargs)
	      early_runtime_error(error_wrong_parameters);

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
		result = op->op(C_ARG(1), C_ARG(2), C_ARG(3), C_ARG(4), C_ARG(5));
		break;
	      default:
		assert(0);	/* A primitive can have a maximum of 5 arguments.
				   If you need more, use VAROP() and check the
				   argument count in the primitive */
	      }
	    C_END_CALL();
	    break;

	  case type_closure:
	    {
	      struct closure *c = (struct closure *)called;

	      if (c->code->o.type == type_mcode)
		{
		  struct call_stack mcode; /* Mark machine code entry */

		  mcode.next = &me;
		  mcode.type = call_compiled;
		  call_stack = &mcode;
		  result = invoke_stack(c, nargs);
		  call_stack = &me;
		  RESTORE_STACK();
		  FAST_PUSH(result);
		}
	      else
		{
		  do_interpret(c, nargs);
		  RESTORE_STACK();
		}
	      RESTORE_INS();
	      seclevel = me.u.mudlle.code->seclevel;
	      break;
	    }
	  default: runtime_error(error_bad_function);
	  }
	break;

      case op_argcheck:		/* A CISCy instruction :-) */
	if (nargs != INSUBYTE()) early_runtime_error(error_wrong_parameters);
	for (i = 0; i < nargs; i++)
	  ((struct variable *)me.u.mudlle.locals->data[i])->value = FAST_GET(i);
	break;
      case op_varargs:		/* Another CISCy instruction... */
	{
	  struct vector *args = (struct vector *)unsafe_allocate_record(type_vector, nargs);
	  int i;

	  RESTORE_STACK();
	  RESTORE_INS();
	  for (i = 0; i < nargs; i++)
	    args->data[nargs - i - 1] = FAST_GET(i);

	  ((struct variable *)me.u.mudlle.locals->data[0])->value = args;
	  me.u.mudlle.nargs = 1;
	  FAST_POPN(nargs);
	  break;
	}
      case op_discard:
	FAST_POP();
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
      case op_dup:
	FAST_PUSH(FAST_GET(0));
	break;
      case op_branch_z1:
	if (!istrue(FAST_POP())) goto branch1;
	INSBYTE();
	break;
      case op_branch_z2:
	if (!istrue(FAST_POP())) goto branch2;
	INSWORD();
	break;
      case op_branch_nz1:
	if (istrue(FAST_POP())) goto branch1;
	INSBYTE();
	break;
      case op_branch_nz2:
	if (istrue(FAST_POP())) goto branch2;
	INSWORD();
	break;
      case op_loop1:
	FAST_POP();
#ifdef MUME
	if (!--session_context->call_count) runtime_error(error_loop);
#endif
#ifdef INTERRUPT
	check_interrupt();
#endif
	/* FALL THROUGH */
      case op_branch1:
      branch1:
	{
	  byte offset = INSBYTE();

	  ins_index += offset;
	  ins += offset;
	  break;
	}

      case op_loop2:
	FAST_POP();
#ifdef MUME
	if (!--session_context->call_count) runtime_error(error_loop);
#endif
#ifdef INTERRUPT
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

#define RECALL(access) FAST_PUSH(((struct variable *)access)->value)
#define ASSIGN(access) { \
  struct variable *var = (struct variable *)(access); \
  var->value = FAST_GET(0); }
#define ADDCLOSURE(access) new_closure->variables[new_var++] = (access)

      case op_clear_local: ((struct variable *)LOCAL)->value = NULL; break;
      case op_recall + local_var: RECALL(LOCAL); break;
      case op_recall + closure_var: RECALL(CLOSURE); break;
      case op_recall + global_var: FAST_PUSH(GVAR(INSUWORD())); break;
      case op_assign + local_var: ASSIGN(LOCAL); break;
      case op_assign + closure_var: ASSIGN(CLOSURE); break;
      case op_assign + global_var: 
	{
	  ulong goffset = INSUWORD();

	  if (GCONSTANT(goffset)) runtime_error(error_variable_read_only);
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

#define INTEGER_OP(op) \
	arg2 = FAST_POP(); \
	arg1 = FAST_GET(0); \
	if (integerp(arg1) && integerp(arg2)) \
	  FAST_SET(0, op); \
	else \
	  runtime_error(error_bad_type);
	
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
	else runtime_error(error_bad_type);
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
	arg2 = FAST_POP();
	arg1 = code_ref(FAST_GET(0), arg2);
	GCCHECK(arg1);
	RESTORE_STACK();
	RESTORE_INS();
	FAST_SET(0, arg1);
	break;

      case op_builtin_set:
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
	if (!integerp(arg1)) runtime_error(error_bad_type);
	break;

      case op_typecheck + type_string:
      case op_typecheck + type_vector:
      case op_typecheck + type_pair:
      case op_typecheck + type_symbol:
      case op_typecheck + type_table:
      case op_typecheck + type_object:
      case op_typecheck + type_character:
      case op_typecheck + type_gone:
	arg1 = FAST_GET(INSUBYTE());
	if (!TYPE(arg1, byteop - op_typecheck)) runtime_error(error_bad_type);
	break;

      case op_typecheck + type_null:
	arg1 = FAST_GET(INSUBYTE());
	if (arg1) runtime_error(error_bad_type);
	break;

      case op_typecheck + stype_none:
	runtime_error(error_bad_type);

      case op_typecheck + stype_function:
	{
	  int type;

	  arg1 = FAST_GET(INSUBYTE());
	  if (!pointerp(arg1)) runtime_error(error_bad_type);

	  type = ((struct obj *)arg1)->type;
	  if (!(type == type_closure || type == type_primitive ||
		type == type_varargs || type == type_secure))
	    runtime_error(error_bad_type);
	  break;
	}	

      case op_typecheck + stype_list:
	arg1 = FAST_GET(INSUBYTE());
	if (arg1 && !TYPE(arg1, type_pair)) runtime_error(error_bad_type);
	break;

      default: assert(0);
      }
  }
 done:
  call_stack = me.next;

  me.u.mudlle.code->instruction_count += instruction_number - start_ins;
}


/* Interface to machine code. */

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
  value result, arg1, arg2, arg3, arg4;
  value *stkpos;		/* Pointer into stack */
  int i;

  GCPRO1(c);
  push_registers();
  UNGCPRO();

  RESTORE_STACK();
  switch (nargs)
    {
    case 0:
      result = mc_invoke(c, 0, NULL, NULL, NULL, NULL);
      break;
    case 1:
      result = mc_invoke(c, 1, FAST_POP(), NULL, NULL, NULL);
      break;
    case 2:
      arg2 = FAST_POP(); arg1 = FAST_POP();
      result = mc_invoke(c, 2, arg1, arg2, NULL, NULL);
      break;
    case 3:
      arg3 = FAST_POP(); arg2 = FAST_POP(); arg1 = FAST_POP();
      result = mc_invoke(c, 3, arg1, arg2, arg3, NULL);
      break;
    case 4:
      arg4 = FAST_POP(); arg3 = FAST_POP(); arg2 = FAST_POP(); arg1 = FAST_POP();
      result = mc_invoke(c, 4, arg1, arg2, arg3, arg4);
      break;
    default:
      GCPRO1(c);
      extra = (struct vector *)unsafe_allocate_record(type_internal, nargs - 3);
      UNGCPRO();
      RESTORE_STACK();
      for (i = nargs - 3; i > 0; ) extra->data[--i] = FAST_POP();
      arg3 = FAST_POP(); arg2 = FAST_POP(); arg1 = FAST_POP();
      result = mc_invoke(c, nargs, arg1, arg2, arg3, extra);
      break;
    }
  pop_registers();
  return result;
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
	  for (i = 0; i < nextra; i++) dynpro(protection + i, extra + i);
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
				FAST_PUSH(extra[i]);
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
  value arg1, arg2, arg3, arg4, arg5;
  value *stkpos;		/* Pointer into stack */
  int i;

  RESTORE_STACK();
  switch (nargs)
    {
    case 0:
      return mc_invoke(NULL, NULL, NULL, NULL, NULL, c, 0);
    case 1:
      return mc_invoke(FAST_POP(), NULL, NULL, NULL, NULL, c, 1);
    case 2:
      arg2 = FAST_POP(); arg1 = FAST_POP();
      return mc_invoke(arg1, arg2, NULL, NULL, NULL, c, 2);
    case 3:
      arg3 = FAST_POP(); arg2 = FAST_POP(); arg1 = FAST_POP();
      return mc_invoke(arg1, arg2, arg3, NULL, NULL, c, 3);
    case 4:
      arg4 = FAST_POP(); arg3 = FAST_POP(); arg2 = FAST_POP(); arg1 = FAST_POP();
      return mc_invoke(arg1, arg2, arg3, arg4, NULL, c, 4);
    case 5:
      arg5 = FAST_POP();
      arg4 = FAST_POP(); arg3 = FAST_POP(); arg2 = FAST_POP(); arg1 = FAST_POP();
      return mc_invoke(arg1, arg2, arg3, arg4, arg5, c, 5);

    default:
      GCPRO1(c);
      extra = (struct vector *)unsafe_allocate_record(type_internal, nargs - 5);
      UNGCPRO();
      RESTORE_STACK();
      for (i = nargs - 5; i > 0; ) extra->data[--i] = FAST_POP();
      arg5 = FAST_POP();
      arg4 = FAST_POP(); arg3 = FAST_POP(); arg2 = FAST_POP(); arg1 = FAST_POP();
      return mc_invoke_vector(arg1, arg2, arg3, arg4, arg5, c, nargs, extra, 0);
    }
}

#endif

#ifdef linux

value interpreter_start(value arg1, value arg2, value arg3, value arg4, value arg5,
			value arg6, value first_extra, ...)
{
}

static INLINE value invoke_stack(struct closure *c)
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
     The stack must contain at least nargs entries.
   Effects: Executes c(nargs arguments taken from the stack)
   Returns: c's result
*/
{
}

#endif

void interpret_init(void)
{
#ifdef sparc
  staticpro(&interpret_closure);
#endif
}
