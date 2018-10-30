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

#include "mudlle-macro.h"

/* machine code primitives */

void interpreter_invoke(void);
/* Enter the interpreter from a machine code function */

/* Compiler support builtins (machine specific) */
#if defined __i386__ || defined __x86_64__

#ifdef __x86_64__
 #define __IS_X86_64 1
#else
 #define __IS_X86_64 0
#endif

#define X86_BUILTINS_FOREACH(v)                 \
  v(badd)                                       \
  v(balloc_closure)                             \
  v(balloc_variable)                            \
  v(balloc_vector)                              \
  v(bapply_varargs)                             \
  v(bbitref)                                    \
  v(bcall)                                      \
  IF(__IS_X86_64)(v(bcall_prim),)               \
  IF(__IS_X86_64)(v(bcall_prim_noalloc),)       \
  v(bcall_secure)                               \
  v(bcall_varargs)                              \
  v(bcleargc)                                   \
  v(bcleargc0)                                  \
  v(bcleargc1)                                  \
  v(bcleargc2)                                  \
  v(bcleargc3)                                  \
  v(bcleargc4)                                  \
  v(bconcat)                                    \
  v(bcons)                                      \
  v(bdivide)                                    \
  v(bearly_error_wrong_parameters)              \
  v(berror_abort)                               \
  v(berror_bad_function)                        \
  v(berror_bad_index)                           \
  v(berror_bad_type)                            \
  v(berror_bad_value)                           \
  v(berror_compile)                             \
  v(berror_divide_by_zero)                      \
  v(berror_loop)                                \
  v(berror_no_match)                            \
  v(berror_recurse)                             \
  v(berror_security_violation)                  \
  v(berror_stack_underflow)                     \
  v(berror_user_interrupt)                      \
  v(berror_value_read_only)                     \
  v(berror_variable_read_only)                  \
  v(berror_wrong_parameters)                    \
  v(bmultiply)                                  \
  v(bpcons)                                     \
  v(bref)                                       \
  v(bremainder)                                 \
  IF(__IS_X86_64)(,v(brestore_caller))          \
  v(brglobal)                                   \
  IF(__IS_X86_64)(,v(bsave_caller))             \
  IF(__IS_X86_64)(,v(bsave_caller_noalloc))     \
  v(bset)                                       \
  v(bshift_left)                                \
  v(bshift_right)                               \
  v(bsymbol_ref)                                \
  v(btype_error)                                \
  v(btypeof)                                    \
  v(bvarargs)                                   \
  v(bwglobal)

#define DECL_PROTO(name) extern void name();
X86_BUILTINS_FOREACH(DECL_PROTO)
#undef DECL_PROTO

/* these aren't accessible via builtin_find() */
extern void bcall_primitive_tail();
extern void builtin_start(), builtin_end();

#endif  /* __i386__ || __x86_64__ */

#endif  /* BUILTINS_H */
