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

#ifndef RUNTIME_RUNTIME_H
#define RUNTIME_RUNTIME_H

#include "../alloc.h"
#include "../call.h"
#include "../charset.h"
#include "../context.h"
#include "../error.h"
#include "../global.h"
#include "../mudlle.h"
#include "../stack.h"
#include "../types.h"

void runtime_init(void);

void flag_check_failed(const struct primitive_ext *op, const char *flag);

#define CHECK_PRIM_FLAGS 1

#  define LVL_IMPLEMENTOR 1
#  define CASSERT_SECLEVEL(seclevel)            \
  CASSERT_EXPR(seclevel == 0 || seclevel == 1)

#define FULLOP(x, pname, helpmsg, nargs, args,                          \
               seclevel, flags, type, storage_class)                    \
static value flag_check_ ## x(PRIMARGS ## nargs);                       \
storage_class value code_ ## x args;                                    \
storage_class const struct primitive_ext op_ ## x = {                   \
  (pname) ? (pname) : #x, helpmsg,                                      \
  (CHECK_PRIM_FLAGS                                                     \
   ? (value (*)())flag_check_ ## x                                      \
   : (value (*)())code_ ## x),                                          \
  nargs, flags, type, seclevel, __FILE__, __LINE__                      \
};                                                                      \
                                                                        \
static value flag_check_ ## x(PRIMARGS ## nargs)                        \
{                                                                       \
  CASSERT_EXPR(!((flags) & ~ALL_OP_FLAGS));                             \
  CASSERT_EXPR((nargs) < 0 ? (seclevel) == 0 : true);                   \
  CASSERT_SECLEVEL(seclevel);                                           \
  /* optimization: allow faster (type_primitive vs. type_secure) OPs */ \
  /* for M-secure OPs that don't need a valid seclevel (ie. don't    */ \
  /* need to check against >= LEGACY_SECLEVEL). */                      \
  CASSERT_EXPR(!((flags) & OP_FASTSEC)                                  \
               || ((seclevel) > 0 && (seclevel) < LEGACY_SECLEVEL));    \
  if ((seclevel) > 0 && (flags) & OP_FASTSEC                            \
      && intval(maxseclevel) < (seclevel))                              \
    runtime_error(error_security_violation);                            \
  /* the call site (do_interpret/bcall/bcall_secure) should have */     \
  /* thrown error_security_violation already. */                        \
  if ((seclevel) > 0)                                                   \
    assert((seclevel) <= intval(maxseclevel));                          \
  ubyte *oldposgen0 = posgen0;                                          \
  const char *old_forbid_mudlle_calls = forbid_mudlle_calls;            \
  if ((flags) & (OP_LEAF | OP_NOESCAPE))                                \
    forbid_mudlle_calls = op_ ## x.name;                                \
  bool old_seclevel_valid = seclevel_valid;                             \
  seclevel_valid = ((seclevel) > 0 && !((flags) & OP_FASTSEC))          \
                   || nargs == NVARARGS;                                \
  value r = code_ ## x(PRIMARGNAMES ## nargs);                          \
  seclevel_valid = old_seclevel_valid;                                  \
  if (((flags) & OP_NOALLOC) && oldposgen0 != posgen0)                  \
    flag_check_failed(&op_ ## x, "noalloc");                            \
  if ((flags) & (OP_LEAF | OP_NOESCAPE))                                \
    forbid_mudlle_calls = old_forbid_mudlle_calls;                      \
  return r;                                                             \
}                                                                       \
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
  FULLOP(x, name, helpmsg, NVARARGS,                                    \
         (struct vector *args, ulong nargs),                            \
	 0, flags, NULL, static)

#define VARTOP(x, name, helpmsg, flags, type)                           \
  MTYPE(type_ ## x, type);                                              \
  FULLOP(x, name, helpmsg, NVARARGS,                                    \
         (struct vector *args, ulong nargs),                            \
	 0, flags, type_ ## x, static)

#define SECOP(x, name, helpmsg, nargs, args, seclevel, flags)           \
  FULLOP(x, name, helpmsg, nargs, args, seclevel, flags,                \
	 NULL, static)

#define SECTOP(x, name, helpmsg, nargs, args, seclevel, flags, type)    \
  MTYPE(type_ ## x, type);                                              \
  FULLOP(x, name, helpmsg, nargs, args, seclevel, flags,                \
	 type_ ## x, static)

#define UNSAFEOP(x, name, helpmsg, nargs, args, flags)                  \
  SECOP(x, name, "UNSAFE:" helpmsg, nargs, args,                        \
	LVL_IMPLEMENTOR, flags)

#define UNSAFETOP(x, name, helpmsg, nargs, args, flags, type)           \
  SECTOP(x, name, "UNSAFE:" helpmsg, nargs, args,                       \
 	 LVL_IMPLEMENTOR, flags, type)

#define UNIMPLEMENTED(x, name, helpmsg, nargs, args, flags, type)       \
TYPEDOP(x, name, "UNIMPLEMENTED: " helpmsg, nargs, args, flags, type)   \
{                                                                       \
  runtime_error(error_bad_function);                                    \
  undefined();                                                          \
}

#define DEFINE(x) runtime_define(&op_ ## x)

void system_define(const char *name, value val);
void system_write(const char *name, value val);
/* Modifies: environment
   Requires: name not already exist in environment.
   Effects: Adds name to environment, with value val for the variable,
     as a 'define' of the system module or as a system-write variable.
*/

struct vector *define_string_vector(const char *name, const char *const *vec,
                                    int count);
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

#define GETUINT(v) (integerp(v)					\
		    ? uintval(v)				\
		    : (runtime_error(error_bad_type), 0UL))

#ifdef MUDLLE_INTERRUPT
void check_interrupt(void);
#endif
/* Effects: Causes a user_interrupt runtime error if user caused
   SIGINT or SIGQUIT
*/

#define UNDEFINED_VALUE (makeint(42))
/* Return the undefined result */
#define undefined()  return UNDEFINED_VALUE

/* Typing information for primitives */
/* A type signature is a string xxx.y, where the
   x's stand for the type of arguments, y for the type of the result.
   y can be ommitted for functions with undefined results.
   The following characters are used:

   f: function (closure, primitive, vararg, secure)
   n: integer
   z: zero
   Z: non-zero
   s: string
   v: vector
   l: list (pair or null)
   u: null
   k: pair (mnemonic: kons)
   t: table
   y: symbol
   d: float (mnemonic: double)
   b: bigint
   r: reference
   x: any
   o: other
   1-9: same type as corresponding argument (must be a previous arg)
   A-Z: special typesets, as follows:
    B: integer or bigint (auto-converts to bigint)
    D: integer, bigint, or float (auto-converts to float)
    S: string or integer
   *: Kleene closure of the previous type; must be followed by "."
   [...]: one of the enclosed characters

  A typing is just an array of strings (terminated by NULL).
  Rep chosen for ease of type specification

  Cf. the "typesets" variable in inference.mud
*/

#define MTYPE(name, sig) static const typing name = { sig, NULL }

void mudlle_consts_init(void);

struct primitive_ext;
const struct primitive_ext *lookup_primitive(ulong adr);

#define STATIC_STRING(name, value)                      \
static ulong static_data_ ## name;                      \
static const STATIC_STRING_T(sizeof value) name = {     \
  .static_data = &static_data_ ## name,                 \
  .mobj = {                                             \
    .size = sizeof (struct string) + sizeof value,      \
    .garbage_type = garbage_static_string,              \
    .type = type_string,                                \
    .flags = OBJ_IMMUTABLE | OBJ_READONLY               \
  },                                                    \
  .str = value                                          \
}

#define GET_STATIC_STRING(name) ((struct string *)&(name).mobj)

extern struct string *const static_empty_string;

#endif /* RUNTIME_RUNTIME_H */
