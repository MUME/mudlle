/* $Log: call.h,v $
 * Revision 1.2  1995/07/16  09:16:49  arda
 * Add GCSTATS option.
 * Misc bug fixes.
 *
 * Revision 1.1  1995/07/15  15:49:26  arda
 * New files, missing from previous commit.
 *
 *
 * Purpose: call mudlle code from C
 */

#ifndef CALL_H
#define CALL_H

value call0(value c);
/* Effects: Calls c with no arguments
   Returns: c's result
   Requires: callable(c, 0) does not fail.
*/

value call1(value c, value arg);
/* Effects: Calls c with argument arg
   Returns: c's result
   Requires: callable(c, 1) does not fail.
*/

value call1plus(value c, value arg, struct vector *args);
/* Effects: Calls c with argument arg
   Returns: c's result
   Requires: callable(c, 1 + vector_len(args)) does not fail.
   Cheat: If c is a closure, it will do the argument count check, so
     the requirement is waved (otherwise cause_event/react_event
     become painful).
*/

value call(value c, struct vector *args);
/* Effects: Calls c with arguments args
   Returns: c's result
   Requires: callable(c, vector_len(args)) does not fail.
*/

void callable(value c, int nargs);
/* Effects: Causes an error of c is not something that can be called with
     nargs arguments.
*/

int callablep(value c, int nargs);
/* Returns: FALSE if c is not something that can be called with
     nargs arguments.
*/

/* as above, but trap errors */
/* Errors can be detected by checking exception_signal, it will be 0
   if all went well.
   Otherwise exception_signal and exception_value are set, and NULL is
   returned.
*/
value catch_call0(value c);
value catch_call1(value c, value arg);
value catch_call1plus(value c, value arg, struct vector *args);
value catch_call(value c, struct vector *args);

#endif
