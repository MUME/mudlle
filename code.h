/* $Log: code.h,v $
 * Revision 1.15  1995/07/15  15:24:15  arda
 * Context cleanup.
 * Remove GCDEBUG.
 *
 * Revision 1.14  1995/04/29  20:05:18  arda
 * fix
 *
 * Revision 1.13  1994/10/09  06:41:50  arda
 * Libraries
 * Type inference
 * Many minor improvements
 *
 * Revision 1.12  1994/08/16  19:15:45  arda
 * Mudlle compiler for sparc now fully functional (68k compiler now needs
 * updating for primitives).
 * Changes to allow Sparc trap's for runtime errors.
 * Also added flags to primitives for better calling sequences.
 *
 * Revision 1.9  1994/03/23  14:31:16  arda
 * *** empty log message ***
 *
 * Revision 1.8  1994/02/12  17:24:40  arda
 * Owl: Better code generated.
 *
 * Revision 1.7  1994/01/08  12:49:37  dgay
 * Owl: Improved code generation for blocks (they are not implemented
 * as 0 argument functions anymore, they are folded into the current
 * function instead).
 *
 * Revision 1.6  1993/10/03  14:07:12  dgay
 * Bumper disun8 update.
 *
 * Revision 1.5  1993/07/21  20:36:34  un_mec
 * Owl: Added &&, ||, optimised if.
 *      Added branches to the intermediate language.
 *      Separated destiniation language generation into ins module
 *      (with some peephole optimisation)
 *      Standalone version of mudlle (mkf, runtime/mkf, mudlle.c) added to CVS
 *
 * Revision 1.4  1993/03/29  09:23:41  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.3  1993/03/14  16:13:59  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.2  1993/01/26  09:48:42  un_mec
 * Owl:
 * - Limit mudlle execution time (prevent infinite loops).
 * - Add mudlle reaction procedures.
 *
 * Revision 1.1  1992/12/27  21:40:58  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

#ifndef CODE_H
#define CODE_H

#include <limits.h>

/* Definition of generated code */

/* A bytecode style representation is used:
   Code to be interpreted is an array of bytes.
   Each instruction is stored in 1 byte, followed by an optional 1 or 2 byte
   argument.

   arg0 means no argument.
   arg1 means a byte argument.
   arg2 means a 2 byte argument (big-endian format). */

typedef ubyte instruction;

typedef enum { local_var, closure_var, global_var } variable_class;

enum {
  /* Simple operations */
  op_return,			/* arg0 */
  op_constant1,			/* arg1 is offset in code's constants */
  op_constant2,			/* arg2 is offset in code's constants */
  op_integer1,			/* arg1 is signed integer */
  op_integer2,			/* arg2 is signed integer */
  op_closure,			/* arg1 is # of closure variables */
  op_closure_code1,		/* arg1 is code's offset in constants */
  op_closure_code2,		/* arg2 is code's offset in constants */
  op_execute,			/* arg1 is # of parameters passed */
  op_execute_primitive1,	/* arg2 is global offset */
  op_execute_primitive2,	/* arg2 is global offset */
  op_execute_global1,		/* arg2 is global offset */
  op_execute_global2,		/* arg2 is global offset */
  op_argcheck,			/* arg1 is # of parameters expected */
  op_varargs,			/* arg0. Makes vector of arguments -> local var 0 */
  op_discard,			/* arg0. Pop top of stack (discard result) */
  op_pop_n,			/* arg1. Pop n stack entries */
  op_exit_n,			/* arg1. Pop top of stack, pop n entries, push old top of stack */
  op_dup,			/* arg0. Duplicate top of stack */
  /* All branch instructions must be consecutive, with the 1 byte version
     immediately preceding the 2 byte one.
     op_branch1 must be the first branch */
  op_branch1,			/* arg1 is signed offset from next instruction */
  op_branch2,			/* arg2 is signed offset from next instruction */
  op_loop1,			/* arg1 is signed offset from next instruction */
  op_loop2,			/* arg2 is signed offset from next instruction */
  op_branch_nz1,		/* arg1 is signed offset from next instruction */
  op_branch_nz2,		/* arg2 is signed offset from next instruction */
  op_branch_z1,			/* arg1 is signed offset from next instruction */
  op_branch_z2,			/* arg2 is signed offset from next instruction */

  op_clear_local,		/* arg1 is # of local variable to set to null */
  /* variable operations, which come in local_var, closure_var, global_var
     flavours, and take an arg1 (local, closure) or arg2 (global) indicating the
     offset in the corresponding variable list */
  op_recall,
  op_define = op_recall + global_var + 1,
				/* arg2 is # of global variable */
  op_assign,
  op_closure_var = op_assign + global_var + 1,
  /* Note: No global vars in closures ... */

  /* Builtin operations (very common) */
  op_builtin_eq = op_closure_var + closure_var + 1,
  op_builtin_neq,
  op_builtin_gt,
  op_builtin_lt,
  op_builtin_le,
  op_builtin_ge,
  op_builtin_ref,
  op_builtin_set,
  op_builtin_add,
  op_builtin_sub,
  op_builtin_bitand,
  op_builtin_bitor,
  op_builtin_not,

  op_typecheck			/* typecheck i: op_typecheck + i
				   arg1 is stack offset */
};

/* Max size of unsigned arg1 */
#define ARG1_MAX ((1 << CHAR_BIT) - 1)
/* Maximum for inline constants only, others can be larger */
#define INTEGER1_MAX ((1 << (CHAR_BIT - 1)) - 1)
#define INTEGER1_MIN (-(1 << (CHAR_BIT - 1)))
#define INTEGER2_MAX ((1 << (2 * CHAR_BIT - 1)) - 1)
#define INTEGER2_MIN (-(1 << (2 * CHAR_BIT - 1)))

#endif
