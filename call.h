/*
 * Copyright (c) 1993-1999 David Gay and Gustav Hållberg
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

value call2(value c, value arg1, value arg2);
/* Effects: Calls c with arguments arg1, arg2
   Returns: c's result
   Requires: callable(c, 2) does not fail.
*/

value call3(value c, value arg1, value arg2, value arg3);
/* Effects: Calls c with arguments arg1, arg2, arg3
   Returns: c's result
   Requires: callable(c, 3) does not fail.
*/

value call4(value c, value arg1, value arg2, value arg3, value arg4);
/* Effects: Calls c with arguments arg1, arg2, arg3, arg4
   Returns: c's result
   Requires: callable(c, 4) does not fail.
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
value mcatch_call0(value c);
value mcatch_call1(value c, value arg);
value mcatch_call2(value c, value arg1, value arg2);
value mcatch_call3(value c, value arg1, value arg2, value arg3);
value mcatch_call4(value c, value arg1, value arg2, value arg3, value arg4);
value mcatch_call1plus(value c, value arg, struct vector *args);
value mcatch_call(value c, struct vector *args);

/* Machine language interface */

value invoke0(struct closure *c);
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c()
   Returns: c()'s result
*/

value invoke1(struct closure *c, value arg);
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c(arg)
   Returns: c(arg)'s result
*/

value invoke2(struct closure *c, value arg1, value arg2);
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c(arg1, arg2)
   Returns: c()'s result
*/

value invoke3(struct closure *c, value arg1, value arg2, value arg3);
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c(arg1, arg2, arg3)
   Returns: c()'s result
*/

value invoke4(struct closure *c, value arg1, value arg2, value arg3, value arg4);
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c(arg1, arg2, arg3, arg4)
   Returns: c()'s result
*/

value invoke1plus(struct closure *c, value arg, struct vector *args);
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c(args)
   Returns: c(args)'s result
*/

value invoke(struct closure *c, struct vector *args);
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c(args)
   Returns: c(args)'s result
*/

#endif
