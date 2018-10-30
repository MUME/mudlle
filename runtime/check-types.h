#ifndef RUNTIME_CHECK_TYPES_H
#define RUNTIME_CHECK_TYPES_H

#include "../call.h"
#include "../mudlle-macro.h"

/*
  CHECK_TYPES(var0, type0, var1, type1, ...)

  Validates each var<N> against its type<N>. A type is one of:
    <type name>          Check that var is of that type.
                         See "add new <type name> ..." below if your type
                         doesn't work.
    any                  No checks performed.
    CT_FALSE             Check that var is an integer; check that it is zero.
    CT_FUNCTION          Check that is_function(var) is true.
    CT_CALLABLE(n)       Check that is_function(var) is true; check that it
                         can be called with 'n' arguments.
    CT_TYPES(<type name>...)
                         Check that var is one of the listed types.
    CT_TYPESET(typeset)  Check that var is in 'typeset' (an unsigned int)
    CT_INT(dst)          Check that var is an integer; store the result
                         in 'dst' (a long).
    CT_INT_P(arg, pred)  Check that var is an integer; check that
                         pred(intval(var), errmsg, arg) returns error_none.
    CT_RANGE(dst, min, max)
                         Check that var is an integer; check that
                         min <= intval(var) <= max; store the result in
                         'dist' (no type check).
    CT_TYPED_RANGE(type, dst, min, max)
                         Check that var is an integer; check that
                         min <= intval(var) <= max; store the result in 'dst';
                         assert that 'dst' is compatible with 'type'.
    OR(type0, type1)     Checks that var is of either type0 or type1.
    F(typeset, pred, arg)
                         Checks that 'var' is a type in 'typeset'; checks that
                         pred(var, msg, arg) returns error_none; optionally set
                         *msg to an error message (can be NULL).

  The initial type(set) check is performed first for all vars. After they all
  pass, any additional tests are performed (similar to pred() for F).

  In vararg functions, you need to manually test the number of arguments first.

  RUNTIME_ERROR(error, msg) triggers 'error' with message 'msg' (can be NULL).
  All vars must still be valid.
 */


#define __NOTHING(...)
#define __INCLUDE(...) __VA_ARGS__

#define __JOIN_ARGS(...) ( __VA_ARGS__, __INCLUDE_PAREN
#define __INCLUDE_PAREN(...) __VA_ARGS__ )

/* add new <type name> in the following lists */
#define _IS_MTYPE_character MARK
#define _IS_MTYPE_closure   MARK
#define _IS_MTYPE_integer   MARK
#define _IS_MTYPE_mcode     MARK
#define _IS_MTYPE_null      MARK
#define _IS_MTYPE_object    MARK
#define _IS_MTYPE_oport     MARK
#define _IS_MTYPE_pair      MARK
#define _IS_MTYPE_reference MARK
#define _IS_MTYPE_string    MARK
#define _IS_MTYPE_symbol    MARK
#define _IS_MTYPE_table     MARK
#define _IS_MTYPE_variable  MARK
#define _IS_MTYPE_vector    MARK

#define _IS_MTYPE(t) IS_MARK(_IS_MTYPE_ ## t)
#define IS_MTYPE(t) _IS_MTYPE(t)
#define IF_MTYPE(t) IF(IS_MTYPE(t))

#define __CT_E_any(v)
#define __CT_S_any         TYPESET_ANY
#define __CT_EXPAND_any(...)      __VA_ARGS__

#define ___CT_E_F(tsig, pred, cbdata, arg) || (         \
    __ct_error = pred(arg, &__ct_errmsg, cbdata),       \
    __ct_error != error_none)
#define __CT_E_F ___CT_E_F __JOIN_ARGS
#define __CT_S_F(tsig, pred, cbdata) tsig
#define __CT_EXPAND_F(...)        __INCLUDE

#define __CT_E_ORT(t, v)                                        \
  (false IF_MTYPE(t)(, __CT_EXPAND_ ## t(__CT_E_ ## t(v))))
#define ___CT_E_OR(t0, t1, v)                                   \
  || (is_typeset(v, __CT_S_ORT(t0))                             \
      ? __CT_E_ORT(t0, v)                                       \
      : __CT_E_ORT(t1, v))
#define __CT_E_OR ___CT_E_OR __JOIN_ARGS
#define __CT_S_ORT(t) IF_MTYPE(t)(                              \
    TSET(t),                                                    \
    __CT_EXPAND_ ## t(__CT_S_ ## t))
#define __CT_S_OR(t0, t1) (__CT_S_ORT(t0) | __CT_S_ORT(t1))
#define ___CT_EXPAND_OR(...) __VA_ARGS__
#define __CT_EXPAND_OR(...)  ___CT_EXPAND_OR

#define __IS_ANY_any MARK
#define ___CT(v, t, p) (__check = (v), __type = (t), !(p))
#define __CT(v, t)                                              \
  IF(IS_MARK(__IS_ANY_ ## t))(                                  \
    false,                                                      \
    IF_MTYPE(t)(                                                \
      /* regular type */                                        \
      ___CT(v, TSET(t), TYPE(v, t)),                            \
      /* special function */                                    \
      ___CT(v, __CT_S_ ## t, is_typeset((v), __CT_S_ ## t))))

#define ___CE(...) __VA_ARGS__  /* force evaluate macros */
#define __CE(v, t) IF_MTYPE(t)(, ___CE(__CT_E_ ## t(v)))
#define __CARGS(...) FOR_PAIRS(ARGN1, SEP_COMMA, __VA_ARGS__)

void check_types_wrong_nargs(void); /* used to signal an error below */

#define _CT_FAIL(s) _Static_assert(0, s)

/* verify that CHECK_TYPE(v, t) is supported */
#define __CT_VALID(v, t)                                        \
  IF_MTYPE(t)(                                                  \
    , IF(IS_MARK(__CT_EXPAND_ ## t(MARK)))(                     \
      , _CT_FAIL("Unsupported CHECK_TYPES() type " #t);))

#define __IS_X_X MARK
#define __IS_X(count) __IS_X_ ## count

#define __CHECK_NARGS(op, count) do {                   \
    if ((op)->nargs == NVARARGS)                        \
      ;                                                 \
    else if (!__builtin_constant_p((op)->nargs))        \
      assert((op)->nargs == count);                     \
    else if ((op)->nargs != count)                      \
      check_types_wrong_nargs();                        \
  } while (0)

#define __SEP_OR() ||
#define __CHECK_TYPES(op, count, ...) IF(               \
  IS_MARK(__IS_X(count)))(                              \
    _CT_FAIL("invalid number of arguments"),            \
  FOR_PAIRS(__CT_VALID, __NOTHING, __VA_ARGS__)         \
  __CHECK_NARGS(op, count);                             \
                                                        \
  enum runtime_error __ct_error;                        \
  const char *__ct_errmsg = NULL;                       \
  do {                                                  \
    value __check;                                      \
    unsigned __type;                                    \
    if (FOR_PAIRS(__CT, __SEP_OR, __VA_ARGS__))         \
      primitive_bad_typeset_error(                      \
        __check, __type, op, count,                     \
        __CARGS(__VA_ARGS__));                          \
    if (false FOR_PAIRS(__CE, __NOTHING, __VA_ARGS__))  \
      {                                                 \
        goto __ct_error_label;                          \
      __ct_error_label:                                 \
        primitive_runtime_error_msg(                    \
          __ct_error, __ct_errmsg, op, count,           \
          __CARGS(__VA_ARGS__));                        \
      }                                                 \
  } while (0))

#define __CHECK_TYPES0(op)                      \
  enum runtime_error __ct_error;                \
  const char *__ct_errmsg = NULL;               \
  __CHECK_NARGS(op, 0);                         \
  if (false)                                    \
    {                                           \
      goto __ct_error_label;                    \
    __ct_error_label:                           \
      primitive_runtime_error_msg(              \
        __ct_error, __ct_errmsg, op, 0);        \
    }

#define RUNTIME_ERROR(errno, msg) do {          \
    __ct_error = (errno);                       \
    __ct_errmsg = (msg);                        \
    goto __ct_error_label;                      \
  } while (0)

#define CHECK_TYPES(...)                                                \
  IF_NO_ARGS(__VA_ARGS__)(                                              \
    __CHECK_TYPES0(THIS_OP),                                            \
    __CHECK_TYPES(THIS_OP, VA_NPAIRS(__VA_ARGS__), __VA_ARGS__))
#define CHECK_TYPES_OP(op, ...)                                         \
  __CHECK_TYPES(op, VA_NPAIRS(__VA_ARGS__), __VA_ARGS__)

/* use "default" for type to allow any type */
#define __ASSERT_TYPE(x, t) _Generic((x), t: (void)0)

#define __CT_INT_P_E(var, msg, arg_pred)                \
  (ARGN2 arg_pred(intval(var), msg, ARGN1 arg_pred))
#define CT_INT_P(arg, pred)                             \
  F(TSET(integer), __CT_INT_P_E, (arg, pred))

#define __CT_INT_E(v, msg, dst) \
  (__ASSERT_TYPE(dst, long), dst = v, error_none)
#define CT_INT(dst) CT_INT_P(dst, __CT_INT_E)

#define __CT_TYPESET_E(v, msg, arg) error_none
#define CT_TYPESET(typeset) F(typeset, __CT_TYPESET_E, )

#define CT_CALLABLE(nargs) F(TYPESET_FUNCTION, function_callable, nargs)

#define CT_FUNCTION CT_TYPESET(TYPESET_FUNCTION)

static inline enum runtime_error ct_range_e(long v, long min, long max,
                                            const char **errmsg)
{
  if (min <= v && v <= max)
    return error_none;
  *errmsg = out_of_range_message(v, min, max);
  return error_bad_value;
}

#define ___CT_RANGE_E(v, msg, dst, type, min, max)                      \
  (__ASSERT_TYPE(dst, type),                                            \
   (__ct_error = ct_range_e(                                            \
     v,                                                                 \
     (min) < LONG_MIN ? LONG_MIN : (min),                               \
     (max) > LONG_MAX ? LONG_MAX : (max),                               \
     msg)) == error_none && (dst = v),                                  \
   __ct_error)

#define __CT_RANGE_E(v, msg, dst_type_min_max)                          \
  ___CT_RANGE_E(v, msg, ARGN1 dst_type_min_max, ARGN2 dst_type_min_max, \
                ARGN3 dst_type_min_max, ARGN4 dst_type_min_max)

#define CT_TYPED_RANGE(type, dst, min, max)             \
  CT_INT_P((dst, type, min, max), __CT_RANGE_E)
#define CT_RANGE(dst, min, max) CT_TYPED_RANGE(default, dst, min, max)

#define __CT_FALSE_E(var, msg, arg)                             \
  (isfalse(var)                                                 \
   ? error_none                                                 \
   : (*msg = "only false (0) allowed", error_bad_value))
#define CT_FALSE F(TSET(integer), __CT_FALSE_E, )

#define __SEP_BITOR() |
#define CT_TYPES(...) CT_TYPESET(FOR_ARGS(TSET, __SEP_BITOR, __VA_ARGS__))

#endif  /* RUNTIME_CHECK_TYPES_H */
