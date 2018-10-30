/* utilities needed to write primitives */

#ifndef RUNTIME_PRIMS_H
#define RUNTIME_PRIMS_H

#include "runtime.h"

#include "../alloc.h"
#include "../call.h"
#include "../context.h"

#define PRIMARGVALSNVARARGS .args = { v0 }
#define PRIMARGVALS0
#define PRIMARGVALS1        .args = { PRIMARGNAMES1 }
#define PRIMARGVALS2        .args = { PRIMARGNAMES2 }
#define PRIMARGVALS3        .args = { PRIMARGNAMES3 }
#define PRIMARGVALS4        .args = { PRIMARGNAMES4 }
#define PRIMARGVALS5        .args = { PRIMARGNAMES5 }

#define PRIMARGDEFSNVARARGS value args[1];
#define PRIMARGDEFS0
#define PRIMARGDEFS1        value args[1];
#define PRIMARGDEFS2        value args[2];
#define PRIMARGDEFS3        value args[3];
#define PRIMARGDEFS4        value args[4];
#define PRIMARGDEFS5        value args[5];

#define _IS_VOID_void MARK
#define __IS_VOID(arg) IS_MARK(_IS_VOID_ ## arg)
#define _IS_VOID(arg) __IS_VOID(arg)
#define IF_VOID(...) IF(_IS_VOID(ARGN1(__VA_ARGS__)))
/* IF_VOID(arg)(TRUE, FALSE...) -> TRUE iff arg is (void) */

#define __THIS_OP_DECL const struct prim_op *const THIS_OP
#define __CODE_DECL(...)                        \
  (IF_VOID(__VA_ARGS__)                         \
   (__THIS_OP_DECL,                             \
    __VA_ARGS__, __THIS_OP_DECL))

#define __EXPAND(...) __VA_ARGS__
#define ___CODE_ARGS(name, args, ...)                   \
  IF_VOID args(&op_ ## name, __VA_ARGS__, &op_ ## name)
#define __CODE_ARGS(name, args, callargs)               \
  ___CODE_ARGS(name, args, __EXPAND callargs)

/*
 * Arguments for FULLOP():
 *
 *  x          identifier for the primitive; op_ ## x is its prim_op
 *  pname      string mudlle name of primitive; 0 for #x
 *  helpmsg    string help text
 *  nargs      number of arguments [0..MAX_PRIMITIVE_ARGS] or NVARARGS
 *  opargs     argument signature of op_code_ ## x function; if 'callargs'
 *             is an argument list, also used for code_ ## x function
 *  callargs   argument names from 'opargs'; can be zero if
 *             'storage_class' is static
 *  seclevel   security level; cf. CASSERT_SECLEVEL()
 *  flags      bitwise-or of OP_xxx flags
 *  type       type signature, of type 'const typing'
 *  storage_class    static or empty
 */
#define FULLOP(x, pname, helpmsg, nargs, opargs, callargs,              \
               seclevel, flags, type, storage_class)                    \
static int defined_ ## x;  /* used to warn if not DEFINE'd */           \
CASSERT(nargs >= NVARARGS && nargs <= MAX_PRIMITIVE_ARGS);              \
CASSERT(VLENGTH(type) > 0);                                             \
static value prim_ ## x(PRIMARGS ## nargs);                             \
static const struct prim_op op_ ## x = {                                \
  (pname) ? (pname) : #x, helpmsg,                                      \
  (value (*)())prim_ ## x,                                              \
  nargs, flags, type, seclevel, __FILE__, __LINE__                      \
};                                                                      \
                                                                        \
static value op_code_ ## x __CODE_DECL opargs;                          \
storage_class value code_ ## x                                          \
  IF_PAREN(callargs)(opargs,(PRIMARGS ## nargs))                        \
{                                                                       \
  return op_code_ ## x(                                                 \
    __CODE_ARGS(                                                        \
      x, opargs,                                                        \
      IF_PAREN(callargs)(callargs,(PRIMARGNAMES ## nargs))));           \
}                                                                       \
                                                                        \
static value prim_ ## x(PRIMARGS ## nargs)                              \
{                                                                       \
  CASSERT_EXPR(!((flags) & ~ALL_OP_FLAGS));                             \
  CASSERT_EXPR(((nargs) < 0 || ((flags) & OP_TRIVIAL)                   \
                ? (seclevel) == 0                                       \
                : true));                                               \
  CASSERT_SECLEVEL(seclevel);                                           \
  CASSERT_EXPR((~(flags) & OP_TRACE)                                    \
               || ((nargs) >= 0                                         \
                   && ((seclevel) == 0                                  \
                       || ((flags) & OP_FASTSEC))));                    \
  /* optimization: allow faster (type_primitive vs. type_secure) OPs */ \
  /* for M-secure OPs that don't need a valid seclevel (ie. don't    */ \
  /* need to check against >= LEGACY_SECLEVEL). */                      \
  CASSERT_EXPR(!((flags) & OP_FASTSEC)                                  \
               || ((seclevel) > 0 && (seclevel) < LEGACY_SECLEVEL));    \
  struct cstack {                                                       \
    struct call_stack_c_header c;                                       \
    PRIMARGDEFS ## nargs                                                \
  } cstack;                                                             \
                                                                        \
  bool secerr = ((seclevel) > 0 && (flags) & OP_FASTSEC                 \
                 && intval(maxseclevel) < (seclevel));                  \
  if (secerr || ((flags) & OP_TRACE))                                   \
    {                                                                   \
      cstack = (struct cstack){                                         \
        .c = {                                                          \
          .s = {                                                        \
            .next = call_stack,                                         \
            .typ ## e = call_primop,                                    \
          },                                                            \
          .u.op = &op_ ## x,                                            \
          .narg ## s = nargs == NVARARGS ? 1 : nargs                    \
        },                                                              \
        PRIMARGVALS ## nargs                                            \
      };                                                                \
      call_stack = &cstack.c.s;                                         \
      if (secerr)                                                       \
        runtime_error(error_security_violation);                        \
    }                                                                   \
  /* the call site (do_interpret/bcall/bcall_secure) should have */     \
  /* thrown error_security_violation already. */                        \
  if ((seclevel) > 0)                                                   \
    assert((seclevel) <= intval(maxseclevel));                          \
  uint8_t *oldposgen0 = posgen0;                                        \
  const char *old_forbid_mudlle_calls = forbid_mudlle_calls;            \
  bool old_seclevel_valid = seclevel_valid;                             \
  if (!((flags) & OP_TRIVIAL))                                          \
    {                                                                   \
      if ((flags) & (OP_LEAF | OP_NOESCAPE))                            \
        forbid_mudlle_calls = op_ ## x.name;                            \
      seclevel_valid = (((seclevel) > 0 && !((flags) & OP_FASTSEC))     \
                        || nargs == NVARARGS);                          \
    }                                                                   \
  value r = code_ ## x(PRIMARGNAMES ## nargs);                          \
  if (!((flags) & OP_TRIVIAL))                                          \
    {                                                                   \
      seclevel_valid = old_seclevel_valid;                              \
      if (((flags) & OP_NOALLOC) && oldposgen0 != posgen0)              \
        flag_check_failed(&op_ ## x, "noalloc");                        \
      if ((flags) & (OP_LEAF | OP_NOESCAPE))                            \
        forbid_mudlle_calls = old_forbid_mudlle_calls;                  \
    }                                                                   \
  if ((flags) & OP_TRACE)                                               \
    call_stack = call_stack->next;                                      \
  return r;                                                             \
}                                                                       \
static value op_code_ ## x __CODE_DECL opargs

#define TYPEDOP(x, name, helpmsg, nargs, args, flags, type)             \
  MTYPE(type_ ## x, type);                                              \
  FULLOP(x, name, helpmsg, nargs, args, 0,                              \
         0, flags, type_ ## x, static)

#define EXT_TYPEDOP(x, name, helpmsg, nargs, args,                      \
                    callargs, flags, type)                              \
  MTYPE(type_ ## x, type);                                              \
  FULLOP(x, name, helpmsg, nargs, args, callargs, 0,                    \
         flags, type_ ## x, /* extern */)

#define VARTOP(x, name, helpmsg, flags, type)                           \
  MTYPE(type_ ## x, type);                                              \
  FULLOP(x, name, helpmsg, NVARARGS,                                    \
         (struct vector *args, ulong nargs), 0,                         \
         0, flags, type_ ## x, static)

#define SECTOP(x, name, helpmsg, nargs, args, seclevel, flags, type)    \
  MTYPE(type_ ## x, type);                                              \
  FULLOP(x, name, helpmsg, nargs, args, 0,                              \
         seclevel, flags, type_ ## x, static)

#define UNSAFETOP(x, name, helpmsg, nargs, args, flags, type)           \
  SECTOP(x, name, "UNSAFE:" helpmsg, nargs, args,                       \
         LVL_IMPLEMENTOR, flags, type)

#define UNIMPLEMENTED(x, name, helpmsg, nargs, args, flags, type)       \
TYPEDOP(x, name, "UNIMPLEMENTED: " helpmsg, nargs, args, flags, type)   \
{                                                                       \
  runtime_error(error_bad_function);                                    \
  undefined();                                                          \
}

#define DEFINE(x) do {                          \
    (void)defined_ ## x;                        \
    runtime_define(&op_ ## x);                  \
} while (0)

/* use this to silence warning if a primitive isn't DEFINED'd */
#define NOT_DEFINED(x) static int UNUSED defined_ ## x

#define system_define(name, val) do {                           \
  STATIC_STRING(define_name, name);                             \
  system_string_define(GET_STATIC_STRING(define_name), (val));  \
} while (0)

/* Set 'dst' to an alloca'ed copy of 'src', or NULL if 'src' is longer
   than 'maxlen', or contains NUL characters. */
#define ALLOCA_STRING(dst, src, maxlen) do {    \
  struct string *__src = (src);                 \
  ulong __l = string_len(__src);                \
  (dst) = NULL;                                 \
  if (__l < (maxlen))                           \
    {                                           \
      char *__dst = alloca(__l + 1);            \
      memcpy(__dst, __src->str, __l + 1);       \
      if (strlen(__dst) == __l)                 \
        (dst) = __dst;                          \
    }                                           \
} while (0)

/* stop at first NUL */
#define ALLOCA_PATH(dst, src) do {              \
  struct string *__src = (src);                 \
  TYPEIS(__src, string);                        \
  size_t __l = strlen(__src->str);              \
  if (__l > PATH_MAX)                           \
    runtime_error(error_bad_value);             \
  char *__dst = alloca(__l + 1);                \
  memcpy(__dst, __src->str, __l + 1);           \
  (dst) = __dst;                                \
} while (0)

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
   *: Kleene closure of the previous type; must be followed by "."
   [...]: one of the enclosed characters

  A typing is just an array of strings (terminated by NULL).
  Rep chosen for ease of type specification

  Cf. the "typesets" variable in inference.mud
*/

#define MTYPE(name, sig) static const typing name = { sig, NULL }

#endif  /* RUNTIME_PRIMS_H */
