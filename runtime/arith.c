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

#include <math.h>
#include "runtime/runtime.h"
#include "stringops.h"

TYPEDOP(sqrt, "n1 -> n2. Returns square root of n1", 1, (value n),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "n.n")
{
  long x;

  ISINT(n);
  x = intval(n);
  if (x < 0) runtime_error(error_bad_value);

  return makeint((long)sqrt((double)x));
}

TYPEDOP(isinteger, "x -> b. TRUE if x is an integer", 1, (value x),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(integerp(x));
}

OPERATION(plus, "n1 n2 -> n. n = n1 + n2", 2, (value v1, value v2),
	  OP_LEAF | OP_NOESCAPE)
{
  if (integerp(v1) && integerp(v2)) return ((value)((long)v1 + (long)v2 - 1));
  else if (TYPE(v1, type_string) && TYPE(v2, type_string))
    return string_append(v1, v2);
  else runtime_error(error_bad_type);
  NOTREACHED;
}

OPERATION(minus, "n1 n2 -> n. n = n1 - n2", 2, (value v1, value v2),
	  OP_LEAF | OP_NOALLOC | OP_NOESCAPE)
{
  if (integerp(v1) && integerp(v2)) return ((value)((long)v1 - (long)v2 + 1));
  else runtime_error(error_bad_type);
  NOTREACHED;
}

TYPEDOP(negate, "n1 -> n2. n2 = -n1", 1, (value v),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "n.n")
{
  if (integerp(v)) return ((value)(2 -(long)v));
  else runtime_error(error_bad_type);
  NOTREACHED;
}

OPERATION(times, "n1 n2 -> n. n = n1 * n2", 2, (value v1, value v2),
	  OP_LEAF | OP_NOALLOC | OP_NOESCAPE)
{
  if (integerp(v1) && integerp(v2)) return (makeint(intval(v1) * intval(v2)));
  else runtime_error(error_bad_type);
  NOTREACHED;
}

OPERATION(divide, "n1 n2 -> n. n = n1 / n2", 2, (value v1, value v2),
	  OP_LEAF | OP_NOALLOC | OP_NOESCAPE)
{
  if (integerp(v1) && integerp(v2))
    {
      if (v2 == makeint(0)) runtime_error(error_divide_by_zero);
      return (makeint(intval(v1) / intval(v2)));
    }
  else runtime_error(error_bad_type);
  NOTREACHED;
}

OPERATION(remainder, "n1 n2 -> n. n = remainder of division of n1 by n2",
	  2, (value v1, value v2),
	  OP_LEAF | OP_NOALLOC | OP_NOESCAPE)
{
  if (integerp(v1) && integerp(v2))
    {
      if (v2 == makeint(0)) runtime_error(error_divide_by_zero);
      return (makeint(intval(v1) % intval(v2)));
    }
  else runtime_error(error_bad_type);
  NOTREACHED;
}

TYPEDOP(modulo, "n1 n2 -> n. n = n1 mod n2", 2, (value v1, value v2),
	  OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "nn.n")
{
  if (integerp(v1) && integerp(v2))
    {
      long result, p1 = intval(v1), p2 = intval(v2);
    
      if (p2 == 0) runtime_error(error_divide_by_zero);
    
      result = p1 % p2;
      if (((p1 < 0 && p2 > 0) || (p1 > 0 && p2 < 0)) && result != 0) result += p2;
      return (makeint(result));
    }
  else runtime_error(error_bad_type);
  NOTREACHED;
}

OPERATION(smaller, "n1 n2 -> b. TRUE if n1 < n2", 2, (value v1, value v2),
	  OP_LEAF | OP_NOALLOC | OP_NOESCAPE)
{
  if (integerp(v1) && integerp(v2)) return (makebool((long)v1 < (long)v2));
  else runtime_error(error_bad_type);
  NOTREACHED;
}

OPERATION(smaller_equal, "n1 n2 -> b. TRUE if n1 <= n2", 2, (value v1, value v2),
	  OP_LEAF | OP_NOALLOC | OP_NOESCAPE)
{
  if (integerp(v1) && integerp(v2)) return (makebool((long)v1 <= (long)v2));
  else runtime_error(error_bad_type);
  NOTREACHED;
}

OPERATION(greater, "n1 n2 -> b. TRUE if n1 > n2", 2, (value v1, value v2),
	  OP_LEAF | OP_NOALLOC | OP_NOESCAPE)
{
  if (integerp(v1) && integerp(v2)) return (makebool((long)v1 > (long)v2));
  else runtime_error(error_bad_type);
  NOTREACHED;
}

OPERATION(greater_equal, "n1 n2 -> b. TRUE if n1 >= n2", 2, (value v1, value v2),
	  OP_LEAF | OP_NOALLOC | OP_NOESCAPE)
{
  if (integerp(v1) && integerp(v2)) return (makebool((long)v1 >= (long)v2));
  else runtime_error(error_bad_type);
  NOTREACHED;
}

TYPEDOP(max, "n1 n2 -> n. n = max(n1, n2)", 2, (value v1, value v2),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "nn.n")
{
  value max;

  if (integerp(v1) && integerp(v2)) 
    {
      if ((long)v1 < (long)v2) max = v2;
      else max = v1;
      return (max);
    }
  else runtime_error(error_bad_type);
  NOTREACHED;
}

TYPEDOP(min, "n1 n2 -> n. n = min(n1, n2)", 2, (value v1, value v2),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "nn.n")
{
  value min;

  if (integerp(v1) && integerp(v2)) 
    {
      if ((long)v1 > (long)v2) min = v2;
      else min = v1;
      return (min);
    }
  else runtime_error(error_bad_type);
  NOTREACHED;
}

TYPEDOP(abs, "n1 -> n2. n2 = |n1|", 1, (value v),
	  OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "n.n")
{
  ISINT(v);
  if ((long)v < 0) v = makeint(-intval(v));
  return (v);
}

OPERATION(bitor, "n1 n2 -> n. n = n1 | n2", 2, (value v1, value v2),
	  OP_LEAF | OP_NOALLOC | OP_NOESCAPE)
{
  if (integerp(v1) && integerp(v2)) return ((value)((long)v1 | (long)v2));
  else runtime_error(error_bad_type);
  NOTREACHED;
}

OPERATION(bitxor, "n1 n2 -> n. n = n1 ^ n2", 2, (value v1, value v2),
	  OP_LEAF | OP_NOALLOC | OP_NOESCAPE)
{
  if (integerp(v1) && integerp(v2)) return ((value)(((long)v1 ^ (long) v2) | 1));
  else runtime_error(error_bad_type);
  NOTREACHED;
}

OPERATION(bitand, "n1 n2 -> n. n = n1 & n2", 2, (value v1, value v2),
	  OP_LEAF | OP_NOALLOC | OP_NOESCAPE)
{
  if (integerp(v1) && integerp(v2)) return ((value)((long)v1 & (long)v2));
  else runtime_error(error_bad_type);
  NOTREACHED;
}

OPERATION(shift_left, "n1 n2 -> n. n = n1 << n2", 2, (value v1, value v2),
	  OP_LEAF | OP_NOALLOC | OP_NOESCAPE)
{
  if (integerp(v1) && integerp(v2)) return (makeint(intval(v1) << intval(v2)));
  else runtime_error(error_bad_type);
  NOTREACHED;
}

OPERATION(shift_right, "n1 n2 -> n. n = n1 >> n2", 2, (value v1, value v2),
	  OP_LEAF | OP_NOALLOC | OP_NOESCAPE)
{
  if (integerp(v1) && integerp(v2)) return (makeint(intval(v1) >> intval(v2)));
  else runtime_error(error_bad_type);
  NOTREACHED;
}

OPERATION(bitnot, "n1 -> n2. n2 = ~n1", 1, (value v),
	  OP_LEAF | OP_NOALLOC | OP_NOESCAPE)
{
  if (integerp(v)) return ((value)(~(long)v | 1));
  else runtime_error(error_bad_type);
  NOTREACHED;
}

void arith_init(void)
{
  DEFINE("integer?", isinteger);
  DEFINE("+", plus);
  DEFINE("-", minus);
  DEFINE("negate", negate);
  DEFINE("*", times);
  DEFINE("/", divide);
  DEFINE("%", remainder);
  DEFINE("modulo", modulo);
  DEFINE("<", smaller);
  DEFINE("<=", smaller_equal);
  DEFINE(">", greater);
  DEFINE(">=", greater_equal);
  DEFINE("min", min);
  DEFINE("max", max);
  DEFINE("abs", abs);
  DEFINE("|", bitor);
  DEFINE("^", bitxor);
  DEFINE("&", bitand);
  DEFINE("<<", shift_left);
  DEFINE(">>", shift_right);
  DEFINE("~", bitnot);

  DEFINE("sqrt", sqrt);

  system_define("MAXINT", makeint(MAX_TAGGED_INT));
  system_define("MININT", makeint(MIN_TAGGED_INT));
}
