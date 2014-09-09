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

#include <math.h>
#include <stdlib.h>

#include "arith.h"
#include "runtime.h"
#include "stringops.h"


static inline void check_integers(value a, value b,
                                  const struct primitive_ext *op)
{
  if ((long)a & (long)b & 1)
    return;
  primitive_runtime_error(error_bad_type, op, 2, a, b);
}

static inline void check_division(value a, value b,
                                  const struct primitive_ext *op)
{
  check_integers(a, b, op);
  if (b == makeint(0))
    primitive_runtime_error(error_divide_by_zero, op, 2, a, b);
}

TYPEDOP(sqrt, 0, "`n1 -> `n2. Returns the truncated square root of the"
        " non-negative integer `n1.", 1, (value n),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST, "n.n")
{
  long x = GETINT(n);
  if (x < 0) runtime_error(error_bad_value);

  return makeint((long)sqrt((double)x));
}

TYPEDOP(isinteger, "integer?", "`x -> `b. TRUE if `x is an integer",
        1, (value x),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST, "x.n")
{
  return makebool(integerp(x));
}

static const typing plus_tset = { "nn.n", "ss.s", NULL };
FULLOP(plus, "+", "`x1 `x2 -> `x. `x = `x1 + `x2 (for integers or strings).",
       2, (value v1, value v2),
       0, OP_LEAF | OP_NOESCAPE | OP_OPERATOR, plus_tset, /*extern*/)
{
  /* test for strings first as compiled code does integer addition and calls
     this primitive only to add strings */
  if (TYPE(v1, type_string) && TYPE(v2, type_string))
    return string_plus(v1, v2);
  check_integers(v1, v2, &op_plus);
  return (value)((long)v1 + (long)v2 - 1);
}

value string_plus(struct string *s1, struct string *s2)
{
  return string_append(s1, s2, &op_plus);
}

EXT_TYPEDOP(minus, "-", "`n1 `n2 -> `n. `n = `n1 - `n2", 2,
            (value v1, value v2),
            OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST | OP_OPERATOR,
            "nn.n")
{
  check_integers(v1, v2, &op_minus);
  return (value)(((long)v1 - (long)v2) | 1);
}

TYPEDOP(negate, 0, "`n1 -> `n2. `n2 = -`n1", 1, (value v),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST | OP_OPERATOR, "n.n")
{
  if (!integerp(v))
    runtime_error(error_bad_type);
  return (value)(2 - (long)v);
}

EXT_TYPEDOP(multiply, "*", "`n1 `n2 -> `n. `n = `n1 * `n2",
            2, (value v1, value v2),
            OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST | OP_OPERATOR,
            "nn.n")
{
  check_integers(v1, v2, &op_multiply);
  return makeint(intval(v1) * intval(v2));
}

EXT_TYPEDOP(divide, "/", "`n1 `n2 -> `n. `n = `n1 / `n2, truncating towards"
            " zero.",
            2, (value v1, value v2),
            OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST | OP_OPERATOR,
            "nn.n")
{
  check_division(v1, v2, &op_divide);
  return makeint(intval(v1) / intval(v2));
}

EXT_TYPEDOP(remainder, "%",
            "`n1 `n2 -> `n3. Returns the remainder of division of"
            " `n1 by `n2.\n"
            "(`n1 / `n2) * `n2 + `n3 = `n1. Cf. `modulo().",
            2, (value v1, value v2),
            OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST | OP_OPERATOR,
            "nn.n")
{
  check_division(v1, v2, &op_remainder);
  return makeint(intval(v1) % intval(v2));
}

TYPEDOP(modulo, 0, "`n1 `n2 -> `n3. `n3 = `n1 mod `n2.\n"
        "floor(`n1 / `n2) * `n2 + `n3 = `n1. Cf. `%.",
        2, (value v1, value v2),
        OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST, "nn.n")
{
  check_division(v1, v2, &op_modulo);
  long p1 = intval(v1), p2 = intval(v2);
  long result = p1 % p2;
  if (result && (p1 ^ p2) < 0)
    result += p2;
  return makeint(result);
}

#define CMPOP(name, op)                                                 \
EXT_TYPEDOP(name, #op, "`n1 `n2 -> `b. TRUE if `n1 " #op " `n2",        \
            2, (value v1, value v2),                                    \
            OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST | OP_OPERATOR,   \
            "nn.n")                                                     \
{                                                                       \
  check_integers(v1, v2, &op_ ## name);                                 \
  return makebool((long)v1 op (long)v2);                                \
}

CMPOP(smaller, <)
CMPOP(smaller_equal, <=)
CMPOP(greater, >)
CMPOP(greater_equal, >=)

TYPEDOP(max, 0, "`n1 `n2 -> `n. `n = max(`n1, `n2)", 2, (value v1, value v2),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST, "nn.n")
{
  check_integers(v1, v2, &op_max);
  return (long)v1 < (long)v2 ? v2 : v1;
}

TYPEDOP(min, 0, "`n1 `n2 -> `n. `n = min(`n1, `n2)", 2, (value v1, value v2),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST, "nn.n")
{
  check_integers(v1, v2, &op_min);
  return (long)v1 < (long)v2 ? v1 : v2;
}

TYPEDOP(abs, 0, "`n1 -> `n2. `n2 = |`n1|", 1, (value v),
        OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST, "n.n")
{
  ISINT(v);
  if ((long)v < 0) v = makeint(-intval(v));
  return v;
}

EXT_TYPEDOP(bitor, "|", "`n1 `n2 -> `n. `n = `n1 | `n2", 2,
            (value v1, value v2),
            OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST | OP_OPERATOR,
            "nn.n")
{
  check_integers(v1, v2, &op_bitor);
  return (value)((long)v1 | (long)v2);
}

TYPEDOP(bitxor, "^", "`n1 `n2 -> `n. `n = `n1 ^ `n2", 2, (value v1, value v2),
        OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST | OP_OPERATOR, "nn.n")
{
  check_integers(v1, v2, &op_bitxor);
  return (value)(((long)v1 ^ (long)v2) | 1);
}

EXT_TYPEDOP(bitand, "&", "`n1 `n2 -> `n. `n = `n1 & `n2", 2,
            (value v1, value v2),
            OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST | OP_OPERATOR,
            "nn.n")
{
  value result = (value)((long)v1 & (long)v2);
  if (!integerp(result))
    primitive_runtime_error(error_bad_type, &op_bitand, 2, v1, v2);
  return result;
}

EXT_TYPEDOP(shift_left, "<<", "`n1 `n2 -> `n. `n = `n1 << `n2",
            2, (value v1, value v2),
            OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST | OP_OPERATOR,
            "nn.n")
{
  check_integers(v1, v2, &op_shift_left);
  return makeint(intval(v1) << intval(v2));
}

EXT_TYPEDOP(shift_right, ">>", "`n1 `n2 -> `n. `n = `n1 >> `n2",
            2, (value v1, value v2),
            OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST | OP_OPERATOR,
            "nn.n")
{
  check_integers(v1, v2, &op_shift_right);
  return makeint(intval(v1) >> intval(v2));
}

TYPEDOP(rotate_left, "rol", "`n1 `n2 -> `n. `n = `n1 rotate left `n2", 2,
        (value v1, value v2),
        OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST, "nn.n")
{
  unsigned long l1 = GETUINT(v1);
  long l2 = GETINT(v2);
  l2 %= TAGGED_INT_BITS;

  return makeint((l1 << l2) | (l1 >> (TAGGED_INT_BITS - l2)));
}

TYPEDOP(rotate_right, "ror", "`n1 `n2 -> `n. `n = `n1 rotate right `n2", 2,
        (value v1, value v2),
        OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST, "nn.n")
{
  unsigned long l1 = GETUINT(v1);
  long l2 = GETINT(v2);
  l2 %= TAGGED_INT_BITS;

  return makeint((l1 >> l2) | (l1 << (TAGGED_INT_BITS - l2)));
}

TYPEDOP(bitnot, "~", "`n1 -> `n2. `n2 = ~`n1", 1, (value v),
        OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST | OP_OPERATOR, "n.n")
{
  if (!integerp(v))
    runtime_error(error_bad_type);
  return (value)((long)v ^ -2);
}

TYPEDOP(random, 0,
	"`n1 `n2 -> `n. Returns a uniform random number between `n1 and `n2"
        " (inclusive). Cf. `frandom(). `n1 must not be larger than `n2.",
	2, (value n1, value n2),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "nn.n")
{
  check_integers(n1, n2, &op_random);
  long min = intval(n1);
  long max = intval(n2);
  if (min > max) runtime_error(error_bad_value);

  double d = random() / (RAND_MAX + 1.0);
  return makeint(min + (int)(((double)max - min + 1) * d));
}

void arith_init(void)
{
  DEFINE(isinteger);
  DEFINE(plus);
  DEFINE(minus);
  DEFINE(negate);
  DEFINE(multiply);
  DEFINE(divide);
  DEFINE(remainder);
  DEFINE(modulo);
  DEFINE(smaller);
  DEFINE(smaller_equal);
  DEFINE(greater);
  DEFINE(greater_equal);
  DEFINE(min);
  DEFINE(max);
  DEFINE(abs);
  DEFINE(bitor);
  DEFINE(bitxor);
  DEFINE(bitand);
  DEFINE(shift_left);
  DEFINE(shift_right);
  DEFINE(bitnot);

  DEFINE(rotate_left);
  DEFINE(rotate_right);

  DEFINE(sqrt);
  DEFINE(random);

  system_define("MAXINT", makeint(MAX_TAGGED_INT));
  system_define("MININT", makeint(MIN_TAGGED_INT));
  system_define("INTBITS", makeint(TAGGED_INT_BITS));
}
