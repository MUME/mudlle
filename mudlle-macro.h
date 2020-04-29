#ifndef MUDLLE_MACRO_H
#define MUDLLE_MACRO_H

#include "mudlle-config.h"

#include <limits.h>

/* The following include defines the following macros:

   VA_NARGS(...) returns the number of arguments (1 or more)

   VA_NPAIRS(...) returns half the number of arguments, or
      X for odd number of arguments

   DEC(n) returns the integer n - 1

   INC(n) returns the integer n + 1

   FOR_ARGS(op, sep, a1, a2, ...) returns
      op(a1) sep() op(a2) sep() ...

   FOR_NARGS(op, cbarg, sep, a1, a2, ...) returns
      op(1, cbarg, a1) sep() op(2, cbarg, a2) sep() ...

   FOR_PAIRS(op, sep, a1, a2, a3, a4, ...) returns
      op(a1, a2) sep() op(a2, a3) sep() ...

   ARGN<N>(...) returns argument number <N> */

#include "mudlle-macro-n.h"

/* the second division protects against passing in some types of
   pointers rather than arrays */
#define VLENGTH(name) (sizeof (name) / sizeof (name)[0]         \
                       / !(sizeof (name) % sizeof (name)[0]))

/* align 'x' to a multiple of 'n' (a power of two)*/
#define MUDLLE_ALIGN(x, n) (((x) + (n) - 1) & ~((n) - 1))
/* padding needed to align 'x' to a multiple of 'n' (a power of two) */
#define PADDING(x, n)   (-(x) & ((n) - 1))

#define CASSERT(what) _Static_assert((what), "CASSERT(" #what ")")

#define CASSERT_EXPR(what)                                      \
  ((void)sizeof (struct {                                       \
      _Static_assert((what), "CASSERT_EXPR(" #what ")");        \
  }))
#define CASSERT_VLEN(name, len)                                 \
  _Static_assert(VLENGTH(name) == (len),                        \
                 "CASSERT_VLEN(" #name ", " #len ")")
#define CASSERT_SIZEOF(what, size)                              \
  _Static_assert(sizeof (what) == (size),			\
		 "CASSERT_SIZEOF(" #what ", " #size)

#define __STRINGIFY(x) #x
#define STRINGIFY(x) __STRINGIFY(x)

/* these macros check that the arguments are integers / pointers;
   they convert between pointers and integers, automatically promoting
   them to long */
#define int_ptr(l) ((void *)_Generic((l) | 0L,                          \
                                     long: (l) | 0L,                    \
                                     unsigned long: (l) | 0L))
#define ptr_int(p) _Generic((p), void *: (long)(p))

#define EXPAND_ARGS(...) __VA_ARGS__

#define _FORN(n, cbarg, arg) ARGN1 cbarg(n, ARGN2 cbarg)
/* FORN(n, op, sep, arg) returns op(1, arg) sep() op(2, arg) sep() ... */
#define FORN(n, op, sep, arg) _FOR_NA ## n(1, _FORN, (op, arg), sep,)

#define _FOR_ARGS(n, op, arg) op(arg)
/* FOR_ARGS(op, sep, arg1, arg2, ...) returns op(a1) sep() op(a2) ... */
#define FOR_ARGS(op, sep, ...) FOR_NARGS(_FOR_ARGS, op, sep, __VA_ARGS__)

#define __IF0(a, ...) __VA_ARGS__
#define __IF1(a, ...) a

#define _IF(cond) __IF ## cond
/* IF(0)(x, ...) -> ...
   IF(1)(x, ...) -> x */
#define IF(cond) _IF(cond)

#define MARK _, 1,
#define IS_MARK(...) ARGN2(__VA_ARGS__, 0)
/* IS_MARK(MARK ...) -> 1
   IS_MARK(x) -> 0 */

#define __IS_PAREN(...) MARK
#define _IS_PAREN(...) IS_MARK(__VA_ARGS__)
#define IF_PAREN(arg) IF(_IS_PAREN(__IS_PAREN arg))
/* IF_PAREN(arg)(TRUE, FALSE...) -> TRUE iff arg is (...) */

#define _IS_EMPTY() MARK
/* IF_EMPTY(arg)(TRUE, FALSE...) -> TRUE iff arg is the empty token;
   does not handle parentheses nor commas */
#define IF_EMPTY(arg) IF(IS_MARK(_IS_EMPTY arg ()))

#define _IS_ONE1 MARK
/* IF_ONE(arg)(TRUE, FALSE...) -> TRUE iff arg is 1 */
#define IF_ONE(arg) IF(IS_MARK(_IS_ONE ## arg))
#define _IS_TWO2 MARK
/* IF_TWO(arg)(TRUE, FALSE...) -> TRUE iff arg is 2 */
#define IF_TWO(arg) IF(IS_MARK(_IS_TWO ## arg))

#define _IF_NO_COMMA(n) IF_ONE(n)
/* IF_NO_COMMA(args...)(TRUE, FALSE...) -> TRUE iff args contains no comma */
#define IF_NO_COMMA(...) _IF_NO_COMMA(VA_NARGS(__VA_ARGS__))

/* IF_NO_ARGS(args...)(TRUE, FALSE...) -> TRUE iff args is the empty token */
#define IF_NO_ARGS(...)                         \
  IF(IF_NO_COMMA(__VA_ARGS__)(                  \
       IF_EMPTY(__VA_ARGS__)(1, 0),             \
       0))

#define SEP_EMPTY()
#define SEP_COMMA() ,
#define SEP_PLUS() +
#define SEP_SEMI() ;
#define SEP_SEMI_STR() ";"

#define _CONCAT_OP(n, op) op(n)
#define CONCATSEMI(N, op)  FORN(N, _CONCAT_OP, SEP_SEMI,  op)
#define CONCATCOMMA(N, op) FORN(N, _CONCAT_OP, SEP_COMMA, op)

#define P(n) (1U << (n))

#define NBSP "\240"
#define C_NBSP '\240'

#define MIN_VALUE(v)                            \
  _Generic(v,                                   \
           signed char:        SCHAR_MIN,       \
           unsigned char:      0,               \
           signed short:       SHRT_MIN,        \
           unsigned short:     0,               \
           int:                INT_MIN,         \
           unsigned:           0,               \
           long:               LONG_MIN,        \
           unsigned long:      0,               \
           long long:          LLONG_MIN,       \
           unsigned long long: 0)

#define MAX_VALUE(v)                            \
  _Generic(v,                                   \
           signed char:        SCHAR_MAX,       \
           unsigned char:      UCHAR_MAX,       \
           signed short:       SHRT_MAX,        \
           unsigned short:     USHRT_MAX,       \
           int:                INT_MAX,         \
           unsigned:           UINT_MAX,        \
           long:               LONG_MAX,        \
           unsigned long:      ULONG_MAX,       \
           long long:          LLONG_MAX,       \
           unsigned long long: ULLONG_MAX)

#define MIN_FIELD_VAL(t, f) MIN_VALUE(((t *)0)->f)
#define MAX_FIELD_VAL(t, f) MAX_VALUE(((t *)0)->f)

#endif	/* MUDLLE_MACRO_H */
