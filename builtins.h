/*
 * Copyright (c) 1993-2004 David Gay and Gustav Hållberg
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

/* The machine code primitives */

#ifdef __cplusplus
extern "C" {
#endif
void interpreter_invoke(void);
/* Enter the interpreter from a machine code function */

/* Builtins for operations */
void bsubtract(void), bor(void), band(void), bleq(void), 
  blne(void), bllt(void), blle(void), blgt(void), blge(void), bbitor(void), 
  bbitxor(void), bbitand(void), bshift_left(void), bshift_right(void), 
  badd(void), bmultiply(void), bdivide(void), bremainder(void), bnegate(void), 
  bnot(void), bbitnot(void), bref(void),
  bcar(void), bcdr(void), bwglobal(void);

/* Compiler support builtins (machine specific) */
#ifdef AMIGA
value __asm mc_invoke(register __d1 struct closure *c,
		      register __d0 uword argcount,
		      register __d2 value arg1,
		      register __d3 value arg2,
		      register __d4 value arg3,
		      register __d5 value extra);

void bbadargs(void), bcall(void), balloc(void), bcompare(void), balloc_readonly(void),
  bcons(void), berror(void), bvarargs(void);
#endif

#ifdef sparc
void bcleargc(void), bcall(void), balloc_variable(void), bcompare(void),
  bcall_primitive(void), bcall_primitive_leaf(void), balloc_cons(void),
  bcall_primitive_leaf_noalloc(void), balloc_closure(void), bvarargs(void),
  balloc_cons_l0(void), bcall_secure(void), bcall_varargs(void);

value mc_invoke(value arg1, value arg2, value arg3, value arg4,
		value arg5, struct closure *c, int nargs);

value mc_invoke_vector(value arg1, value arg2, value arg3, value arg4,
		       value arg5, struct closure *c, int nargs,
		       struct vector *extra, int from);

void flush_windows(void);
void flush_icache(ubyte *from, ubyte *to);

#endif

#ifdef i386
void bshift_left(), bshift_right(), badd(), bmultiply(), bdivide(), 
  bremainder(), bref(), bset(), bwglobal(), bcleargc(), bvarargs(), bcall(), 
  balloc_variable(), balloc_cons(), balloc_closure(), bcall_primitive(), 
  bcall_primitive_leaf(), bcall_primitive_leaf_noalloc(), bcall_secure(), 
  bcall_varargs(), berror_bad_function(), berror_stack_underflow(), 
  berror_bad_type(), berror_divide_by_zero(), berror_bad_index(), 
  berror_bad_value(), berror_variable_read_only(), berror_loop(), 
  berror_recurse(), berror_wrong_parameters(), berror_security_violation(), 
  berror_value_read_only(), berror_user_interrupt(), 
  bcleargc0(void), bcleargc1(void), bcleargc2(void);

#endif
#ifdef __cplusplus
}
#endif
#endif
