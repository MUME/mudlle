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

#ifndef RUNTIME_H
#define RUNTIME_H

#include "mudlle.h"
#include "types.h"
#include "alloc.h"
#include "stack.h"
#include "global.h"
#include "error.h"
#include "utils.charset.h"

void runtime_init(void);

#define FULLOP(x, helpmsg, nargs, args, seclevel, flags, type) \
value code_ ## x args; \
static struct primitive_ext op_ ## x = { NULL, helpmsg, code_ ## x, nargs, flags, type, seclevel }; \
\
value code_ ## x args

#define TYPEDOP(x, helpmsg, nargs, args, flags, type) \
  MTYPE(type_ ## x, type); \
  FULLOP(x, helpmsg, nargs, args, 0, flags, type_ ## x)

#define OPERATION(x, helpmsg, nargs, args, flags) \
  FULLOP(x, helpmsg, nargs, args, 0, flags, NULL)

#define VAROP(x, helpmsg, flags) \
  FULLOP(x, helpmsg, -1, (struct vector *args, ulong nargs), 0, flags, NULL)

#define SECOP(x, helpmsg, nargs, args, seclevel, flags) \
  FULLOP(x, helpmsg, nargs, args, seclevel, flags, NULL)

#define LVL_IMPLEMENTOR 1

#define UNSAFEOP(x, helpmsg, nargs, args, flags) \
  SECOP(x, "UNSAFE:" helpmsg, nargs, args, LVL_IMPLEMENTOR, flags)

#define UNIMPLEMENTED(x, helpmsg, nargs, args, flags) \
  FULLOP(x, "UNIMPLEMENTED: " helpmsg, nargs, args, 0, flags, NULL) \
{ \
  runtime_error(error_bad_function); \
  undefined(); \
}

#define DEFINE(name, x) runtime_define(name, &op_ ## x)

void system_define(const char *name, value val);
/* Modifies: environment
   Requires: name not already exist in environment.
   Effects: Adds name to environment, with value val for the variable,
     as a 'define' of the system module.
*/

void define_string_vector(const char *name, const char **vec, int count);
void define_int_vector(const char *name, const int *vec, int count);

void runtime_define(const char *name, struct primitive_ext *op);

#define TYPEIS(v, want_type) \
  if (!TYPE((v), (want_type))) runtime_error(error_bad_type)

#define ISINT(v) if (!integerp((v))) runtime_error(error_bad_type)

#define CHECK_FAST_LOOP() \
  if (!--xcount) runtime_error(error_loop);

void check_interrupt(void);
/* Effects: Causes a user_interrupt runtime error if user caused SIGINT or SIGQUIT
*/

/* Return the undefined result */
#define undefined()  return undefined_value
/* Return a value to shut compiler up */
#define NOTREACHED return 0

extern value undefined_value;

/* Typing information for primitives */
/* A type signature is a string xxx.y, where the
   x's stand for the type of arguments, y for the type of the result.
   y can be ommitted for functions with undefined results.
   The following characters are used:

   f: function
   n: integer
   s: string
   v: vector
   l: list (pair or null)
   k: pair
   t: table
   y: symbol
   x: any
   o: other
   1-9: same type as corresponding argument (must be a previous arg)
   A-Z: special typesets, as follows:
    S: string or integer

  A typing is just an array of strings (terminated by NULL).
  Rep chosen for ease of type specification
*/

#define MTYPE(name, sig) static typing name = { sig, NULL }

#endif
