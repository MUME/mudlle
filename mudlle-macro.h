#ifndef MUDLLE_MACRO_H
#define MUDLLE_MACRO_H

#include "mudlle-config.h"

/* the second division protects against passing in some types of
   pointers rather than arrays */
#define VLENGTH(name) (sizeof (name) / sizeof (name)[0]         \
                       / !(sizeof (name) % sizeof (name)[0]))

#define CASSERT(what) _Static_assert((what), "CASSERT(" #what ")")

#define CASSERT_EXPR(what)                                      \
  ((void)sizeof (struct {                                       \
      _Static_assert((what), "CASSERT_EXPR(" #what ")");        \
      int i;                                                    \
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

#define __VA_NARGS(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, \
                   _13, _14, _15, _16, _17, _18, _19, _20, N, ...) N

/* VA_NARGS(...) returns the number of arguments in [1..20] */
#define VA_NARGS(...) __VA_NARGS(__VA_ARGS__, 20, 19, 18, 17, 16, 15, 14, \
                                 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)

/* VA_NPAIRS(...) returns half the number of arguments in [1..10], or X
   for odd number of arguments */
#define VA_NPAIRS(...) __VA_NARGS(__VA_ARGS__, 10, X, 9, X, 8, X, 7, X, 6, \
                                  X, 5, X, 4, X, 3, X, 2, X, 1, X)

#define __FOR1(op, sep, arg) op(1, arg)
#define __FOR2(op, sep, arg) __FOR1(op, sep, arg) sep() op(2, arg)
#define __FOR3(op, sep, arg) __FOR2(op, sep, arg) sep() op(3, arg)
#define __FOR4(op, sep, arg) __FOR3(op, sep, arg) sep() op(4, arg)
#define __FOR5(op, sep, arg) __FOR4(op, sep, arg) sep() op(5, arg)
#define __FOR6(op, sep, arg) __FOR5(op, sep, arg) sep() op(6, arg)
#define __FOR7(op, sep, arg) __FOR6(op, sep, arg) sep() op(7, arg)
#define __FOR8(op, sep, arg) __FOR7(op, sep, arg) sep() op(8, arg)
#define __FOR9(op, sep, arg) __FOR8(op, sep, arg) sep() op(9, arg)
#define FORN(n, op, sep, arg) __FOR ## n(op, sep, arg)

#define ___FORARGS_OP(op, arg) op(arg)
#define __FORARGS_OP(op, arg) ___FORARGS_OP(op, arg)
#define _FORARGS_OP(n, op_args)                                 \
  __FORARGS_OP(ARGN1 op_args, ARGN ## n ARGN2 op_args)
#define _FOR_ARGS(n, op, sep, args)                             \
  FORN(n, _FORARGS_OP, sep, (op, args))

#define FOR_ARGS(op, sep, ...)                                  \
  _FOR_ARGS(VA_NARGS(__VA_ARGS__), op, sep, (__VA_ARGS__))
/* FOR_ARGS(op, sep, a1, a2, ...) returns
   op(a1) sep() op(a2) sep() ... */

#define __FORP1(op, sep, a, b) op(a, b)
#define __FORP2(op, sep, a, b, ...) \
  op(a, b) sep() __FORP1(op, sep, __VA_ARGS__)
#define __FORP3(op, sep, a, b, ...) \
  op(a, b) sep() __FORP2(op, sep, __VA_ARGS__)
#define __FORP4(op, sep, a, b, ...) \
  op(a, b) sep() __FORP3(op, sep, __VA_ARGS__)
#define __FORP5(op, sep, a, b, ...) \
  op(a, b) sep() __FORP4(op, sep, __VA_ARGS__)
#define __FORP6(op, sep, a, b, ...) \
  op(a, b) sep() __FORP5(op, sep, __VA_ARGS__)
#define __FORP7(op, sep, a, b, ...) \
  op(a, b) sep() __FORP6(op, sep, __VA_ARGS__)
#define __FORP(n, op, sep, ...) __FORP ## n(op, sep, __VA_ARGS__)

#define _FOR_PAIRS(count, op, sep, ...) __FORP(count, op, sep, __VA_ARGS__)
/* FOR_PAIRS(op, sep, a1, a2, a3, a4, ...) returns
   op(a1, a2) sep() op(a2, a3) sep() ... */
#define FOR_PAIRS(op, sep, ...)                                 \
  _FOR_PAIRS(VA_NPAIRS(__VA_ARGS__), op, sep, __VA_ARGS__)

/* ARGN<N>: return argument number <N> */
#define ARGN1(_1, ...) _1
#define ARGN2(_1, _2, ...) _2
#define ARGN3(_1, _2, _3, ...) _3
#define ARGN4(_1, _2, _3, _4, ...) _4
#define ARGN5(_1, _2, _3, _4, _5, ...) _5
#define ARGN6(_1, _2, _3, _4, _5, _6, ...) _6
#define ARGN7(_1, _2, _3, _4, _5, _6, _7, ...) _7
#define ARGN8(_1, _2, _3, _4, _5, _6, _7, _8, ...) _8
#define ARGN9(_1, _2, _3, _4, _5, _6, _7, _8, _9, ...) _9

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
#define IS_PAREN(arg) _IS_PAREN(__IS_PAREN arg)
#define IF_PAREN(arg) IF(IS_PAREN(arg))
/* IF_PAREN(arg)(TRUE, FALSE...) -> TRUE iff arg is (...) */

#define _IS_EMPTY() MARK
#define IS_EMPTY(arg) IS_MARK(_IS_EMPTY arg ())
/* IF_EMPTY(arg)(TRUE, FALSE...) -> TRUE iff arg is the empty token;
   does not handle parentheses nor commas */
#define IF_EMPTY(arg) IF(IS_EMPTY(arg))

#define __IS_NO_COMMA_1 MARK
#define __IS_NO_COMMA(n) __IS_NO_COMMA_ ## n
#define _IS_NO_COMMA(n) IS_MARK(__IS_NO_COMMA(n))
/* IF_NO_COMMA(args...)(TRUE, FALSE...) -> TRUE iff args contains no comma */
#define IF_NO_COMMA(...) IF(_IS_NO_COMMA(VA_NARGS(__VA_ARGS__)))

/* IF_NO_ARGS(args...)(TRUE, FALSE...) -> TRUE iff args is the empty token */
#define IF_NO_ARGS(...)                         \
  IF(IF_NO_COMMA(__VA_ARGS__)(                  \
       IF_EMPTY(__VA_ARGS__)(1, 0),             \
       0))

#define SEP_COMMA() ,
#define SEP_SEMI() ;

#define _CONCAT_OP(n, op) op(n)
#define CONCATSEMI(N, op)  FORN(N, _CONCAT_OP, SEP_SEMI,  op)
#define CONCATCOMMA(N, op) FORN(N, _CONCAT_OP, SEP_COMMA, op)

#define P(n) (1U << (n))

#endif	/* MUDLLE_MACRO_H */
