#ifndef RUNTIME_ERROR_H
#define RUNTIME_ERROR_H

#include <setjmp.h>

#define SIGNAL_ERROR 1

typedef enum { 
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
  last_runtime_error
} runtime_errors;

void error_init(void);

NORETURN void early_runtime_error(runtime_errors error);
/* Effects: Runtime error 'error' has occured. Dump the call_stack to
     mudout & throw back to the exception handler with SIGNAL_ERROR
     and the error code in exception_value.
     Call this function instead of runtime_error if the arguments of the
     function at the top of call_stack are still on the stack.
   Note: Never returns
*/

NORETURN void runtime_error(runtime_errors error);
/* Effects: Runtime error 'error' has occured in a primitive operation. 
     Dump the call_stack (plus the primitive operation call) to
     mudout & throw back to the exception handler with SIGNAL_ERROR
     and the error code in exception_value.
   Note: Never returns
*/

#endif

