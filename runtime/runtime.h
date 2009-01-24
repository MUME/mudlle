/*
 * Copyright (c) 1993-2006 David Gay and Gustav Hållberg
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

#include "../mudlle.h"
#include "../types.h"
#include "../alloc.h"
#include "../stack.h"
#include "../global.h"
#include "../error.h"
#include "../charset.h"

void runtime_init(void);

#define FULLOP(x, name, helpmsg, nargs, args, seclevel,                 \
	       flags, type, storage_class)                              \
storage_class value code_ ## x args;                                    \
static const struct primitive_ext op_ ## x = {                          \
  (name) ? (name) : #x, helpmsg, code_ ## x, nargs, flags,              \
  type, seclevel, __FILE__, __LINE__                                    \
};                                                                      \
                                                                        \
storage_class value code_ ## x args

#define TYPEDOP(x, name, helpmsg, nargs, args, flags, type)             \
  MTYPE(type_ ## x, type);                                              \
  FULLOP(x, name, helpmsg, nargs, args, 0, flags,                       \
	 type_ ## x, static)

#define EXT_TYPEDOP(x, name, helpmsg, nargs, args, flags, type)         \
  MTYPE(type_ ## x, type);                                              \
  FULLOP(x, name, helpmsg, nargs, args, 0, flags,                       \
	 type_ ## x, /* extern */)

#define OPERATION(x, name, helpmsg, nargs, args, flags)                 \
  FULLOP(x, name, helpmsg, nargs, args, 0, flags, NULL, static)         \

#define EXT_OPERATION(x, name, helpmsg, nargs, args, flags)             \
  FULLOP(x, name, helpmsg, nargs, args, 0, flags, NULL, /* extern */)

#define VAROP(x, name, helpmsg, flags)                                  \
  FULLOP(x, name, helpmsg, -1, (struct vector *args, ulong nargs),      \
	 0, flags, NULL, static)

#define VARTOP(x, name, helpmsg, flags, type)                           \
  MTYPE(type_ ## x, type);                                              \
  FULLOP(x, name, helpmsg, -1, (struct vector *args, ulong nargs),      \
	 0, flags, type_ ## x, static)

#define SECOP(x, name, helpmsg, nargs, args, seclevel, flags)           \
  FULLOP(x, name, helpmsg, nargs, args, seclevel, flags,                \
	 NULL, static)

#define SECTOP(x, name, helpmsg, nargs, args, seclevel, flags, type)    \
  MTYPE(type_ ## x, type);                                              \
  FULLOP(x, name, helpmsg, nargs, args, seclevel, flags,                \
	 type_ ## x, static)

#  define LVL_IMPLEMENTOR 1

#define UNSAFEOP(x, name, helpmsg, nargs, args, flags)                  \
  SECOP(x, name, "UNSAFE:" helpmsg, nargs, args,                        \
	LVL_IMPLEMENTOR, flags)

#define UNSAFETOP(x, name, helpmsg, nargs, args, flags, type)           \
  SECTOP(x, name, "UNSAFE:" helpmsg, nargs, args,                       \
 	 LVL_IMPLEMENTOR, flags, type)

#define UNIMPLEMENTED(x, name, helpmsg, nargs, args, flags)             \
  FULLOP(x, name, "UNIMPLEMENTED: " helpmsg, nargs, args, 0, flags,     \
	 NULL, static)                                                  \
{                                                                       \
  runtime_error(error_bad_function);                                    \
  undefined();                                                          \
}

#define DEFINE(x) runtime_define(&op_ ## x)

void system_define(const char *name, value val);
/* Modifies: environment
   Requires: name not already exist in environment.
   Effects: Adds name to environment, with value val for the variable,
     as a 'define' of the system module.
*/

void define_string_vector(const char *name, const char *const *vec, int count);
void define_int_vector(const char *name, const int *vec, int count);

void runtime_define(const struct primitive_ext *op);

#define TYPEIS(v, want_type) do {				\
  if (!TYPE((v), (want_type))) runtime_error(error_bad_type);	\
} while (0)

#define ISINT(v) do {						\
  if (!integerp((v))) runtime_error(error_bad_type);		\
} while (0)

#define GETINT(v) (integerp(v)					\
		   ? intval(v)					\
		   : (runtime_error(error_bad_type), 0L))

#define CHECK_FAST_LOOP() \
  if (!--xcount) runtime_error(error_loop);

#ifdef MUDLLE_INTERRUPT
void check_interrupt(void);
#endif
/* Effects: Causes a user_interrupt runtime error if user caused
   SIGINT or SIGQUIT
*/

/* Return the undefined result */
#define undefined()  return makeint(42)
/* Return a value to shut compiler up */
#define NOTREACHED return 0

/* Typing information for primitives */
/* A type signature is a string xxx.y, where the
   x's stand for the type of arguments, y for the type of the result.
   y can be ommitted for functions with undefined results.
   The following characters are used:

   f: function (closure, primitive, vararg, secure)
   n: integer
   s: string
   v: vector
   l: list (pair or null)
   u: null
   k: pair
   t: table
   y: symbol
   x: any
   o: other
   1-9: same type as corresponding argument (must be a previous arg)
   A-Z: special typesets, as follows:
    S: string or integer
   *: Kleene closure of the previous type; must be followed by "."
   [...]: one of the enclosed characters

  A typing is just an array of strings (terminated by NULL).
  Rep chosen for ease of type specification

  Cf. the "typesets" variable in inference.mud
*/

#define MTYPE(name, sig) static const typing name = { sig, NULL }

void mudlle_consts_init(void);


#endif

