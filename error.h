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

#ifndef ERROR_H
#define ERROR_H

#include "types.h"

enum runtime_error {
/* end mudlle const */
  error_none = -1,
/* start mudlle const */
  error_bad_function,
  error_stack_underflow,
  error_bad_type,
  error_divide_by_zero,
  error_bad_index,
  error_bad_value,
  error_variable_read_only,
  error_loop,
  error_recurse,
  error_wrong_parameters,
  error_security_violation,
  error_value_read_only,
  error_user_interrupt,
  error_no_match,
  error_compile,
  error_abort,
  last_runtime_error
};

/* end mudlle const */
enum mudlle_signal {
  SIGNAL_NONE,
  SIGNAL_ERROR,
  SIGNAL_LONGJMP
};

extern const char *const mudlle_errors[];
extern int suppress_extra_calltrace;

noreturn void interpreted_early_runtime_error(enum runtime_error error);
/* Effects: Runtime error 'error' has occured. Dump the call_stack to
     mudout & throw back to the exception handler with SIGNAL_ERROR
     and the error code in exception_error.
     Call this function instead of runtime_error if the arguments of the
     function at the top of call_stack are still on the stack.
   Note: Never returns
*/
noreturn void compiled_early_runtime_error(enum runtime_error error,
                                           int nargs);
/* As above, but there are nargs arguments on the native stack. */

noreturn void runtime_error_message(enum runtime_error error, const char *msg);
noreturn void runtime_error(enum runtime_error error);
/* Effects: Runtime error 'error' has occured in a primitive operation.
     Dump the call_stack (plus the primitive operation call) to
     mudout & throw back to the exception handler with SIGNAL_ERROR
     and the error code in exception_error.
*/

void runtime_warning(const char *msg);

const char *out_of_range_message(long v, long minval, long maxval);
const char *not_callable_message(int nargs);

struct prim_op;
noreturn void primitive_runtime_error(enum runtime_error error,
                                      const struct prim_op *op,
                                      int nargs, ...);
noreturn void primitive_runtime_error_msg(enum runtime_error error,
                                          const char *msg,
                                          const struct prim_op *op,
                                          int nargs, ...);
noreturn void primitive_bad_typeset_error(value v, unsigned expected,
                                          const struct prim_op *op,
                                          int nargs, ...);
noreturn void primitive_bad_type_error(value v, enum mudlle_type expected,
                                       const struct prim_op *op,
                                       int nargs, ...);

void primitive_runtime_warning(const char *msg,
                               const struct prim_op *op,
                               int nargs, ...);

noreturn void bad_call_error(enum runtime_error error, value callee,
                             int nargs, value *argp);
noreturn void bad_type_error(value v, enum mudlle_type expected);
noreturn void bad_typeset_error(value v, unsigned expected);
noreturn void out_of_range_error(long v, long minval, long maxval);

struct vector *get_mudlle_call_trace(bool lines);

#endif /* ERROR_H */
