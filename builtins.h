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

#ifndef BUILTINS_H
#define BUILTINS_H

/* machine code primitives */

void interpreter_invoke(void);
/* Enter the interpreter from a machine code function */

/* Compiler support builtins (machine specific) */
#ifdef i386

void badd(), bdivide(), bmultiply(), bremainder(), bshift_left(),
  bshift_right(), btypeof(),

  balloc_closure(), balloc_cons(), balloc_variable(), balloc_vector(),
  bconcat(),

  bapply_varargs(), bcall(), bcall_error(), bcall_primitive_tail(),
  bcall_secure(), bcall_varargs(), bsave_caller(), bsave_caller_noalloc(),
  brestore_caller(),

  bvarargs(), bcleargc(), bcleargc0(), bcleargc1(), bcleargc2(), bcleargc3(),
  bcleargc4(),

  bearly_error_wrong_parameters(), berror_abort(), berror_bad_function(),
  berror_bad_index(), berror_bad_type(), berror_bad_value(), berror_compile(),
  berror_divide_by_zero(), berror_loop(), berror_no_match(), berror_recurse(),
  berror_security_violation(), berror_stack_underflow(),
  berror_user_interrupt(), berror_value_read_only(),
  berror_variable_read_only(), berror_wrong_parameters(),

  bref(), bset(),

  brglobal(), bwglobal();

#endif  /* i386 */

#endif  /* BUILTINS_H */
