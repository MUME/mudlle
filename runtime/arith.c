/* $Log: arith.c,v $
 * Revision 1.12  1995/07/15  15:24:51  arda
 * Context cleanup.
 * Remove GCDEBUG.
 *
 * Revision 1.11  1994/10/09  06:44:01  arda
 * Libraries
 * Type inference
 * Many minor improvements
 *
 * Revision 1.10  1994/08/16  19:16:53  arda
 * Added flags to primitives for better calling sequences.
 *
 * Revision 1.9  1993/07/21  20:37:55  un_mec
 * Owl: Standalone version of mudlle (mkf, runtime/mkf, mudlle.c) added to CVS
 *      New builtin functions, new abbreviations (. = cons, ! = not).
 *
 * Revision 1.8  1993/06/25  15:38:05  un_autre
 * *** empty log message ***
 *
 * Revision 1.7  1993/05/27  00:12:36  un_autre
 * Owl Bug fixes
 *
 * Revision 1.6  1993/04/25  19:50:31  un_mec
 * Owl: Miscellaneous changes.
 *      I HATE fixing bugs twice.
 *
 * Revision 1.5  1993/04/22  18:59:10  un_autre
 * (MD) & Owl. Bug fixes. /player fixes. EVER_WHINER flag. saving_spells adjusted.
 *
 * Revision 1.4  1993/04/17  11:12:19  un_mec
 * Owl: A few new functions.
 *
 * Revision 1.3  1993/03/29  09:25:17  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.3  1993/03/14  16:16:28  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.1  1992/12/27  21:42:10  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

static char rcsid[] = "$Id: arith.c,v 1.12 1995/07/15 15:24:51 arda Exp $";

#include "runtime/runtime.h"
#include "stringops.h"

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
}

OPERATION(minus, "n1 n2 -> n. n = n1 - n2", 2, (value v1, value v2),
	  OP_LEAF | OP_NOALLOC | OP_NOESCAPE)
{
  if (integerp(v1) && integerp(v2)) return ((value)((long)v1 - (long)v2 + 1));
  else runtime_error(error_bad_type);
}

TYPEDOP(negate, "n1 -> n2. n2 = -n1", 1, (value v),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "n.n")
{
  if (integerp(v)) return ((value)(2 -(long)v));
  else runtime_error(error_bad_type);
}

OPERATION(times, "n1 n2 -> n. n = n1 * n2", 2, (value v1, value v2),
	  OP_LEAF | OP_NOALLOC | OP_NOESCAPE)
{
  if (integerp(v1) && integerp(v2)) return (makeint(intval(v1) * intval(v2)));
  else runtime_error(error_bad_type);
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
}

TYPEDOP(modulo, "n1 n2 -> n. n = n1 mod n2", 2, (value v1, value v2),
	  OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "nn.n")
{
  if (integerp(v1) && integerp(v2))
    {
      long result, p1 = intval(v1), p2 = intval(v2);
    
      if (p2 == 0) runtime_error(error_divide_by_zero);
    
      result = p1 % p2;
      if ((p1 < 0 && p2 > 0 || p1 > 0 && p2 < 0) && result != 0) result += p2;
      return (makeint(result));
    }
  else runtime_error(error_bad_type);
}

OPERATION(smaller, "n1 n2 -> b. TRUE if n1 < n2", 2, (value v1, value v2),
	  OP_LEAF | OP_NOALLOC | OP_NOESCAPE)
{
  if (integerp(v1) && integerp(v2)) return (makebool((long)v1 < (long)v2));
  else runtime_error(error_bad_type);
}

OPERATION(smaller_equal, "n1 n2 -> b. TRUE if n1 <= n2", 2, (value v1, value v2),
	  OP_LEAF | OP_NOALLOC | OP_NOESCAPE)
{
  if (integerp(v1) && integerp(v2)) return (makebool((long)v1 <= (long)v2));
  else runtime_error(error_bad_type);
}

OPERATION(greater, "n1 n2 -> b. TRUE if n1 > n2", 2, (value v1, value v2),
	  OP_LEAF | OP_NOALLOC | OP_NOESCAPE)
{
  if (integerp(v1) && integerp(v2)) return (makebool((long)v1 > (long)v2));
  else runtime_error(error_bad_type);
}

OPERATION(greater_equal, "n1 n2 -> b. TRUE if n1 >= n2", 2, (value v1, value v2),
	  OP_LEAF | OP_NOALLOC | OP_NOESCAPE)
{
  if (integerp(v1) && integerp(v2)) return (makebool((long)v1 >= (long)v2));
  else runtime_error(error_bad_type);
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
}

OPERATION(bitxor, "n1 n2 -> n. n = n1 ^ n2", 2, (value v1, value v2),
	  OP_LEAF | OP_NOALLOC | OP_NOESCAPE)
{
  if (integerp(v1) && integerp(v2)) return ((value)(((long)v1 ^ (long) v2) | 1));
  else runtime_error(error_bad_type);
}

OPERATION(bitand, "n1 n2 -> n. n = n1 & n2", 2, (value v1, value v2),
	  OP_LEAF | OP_NOALLOC | OP_NOESCAPE)
{
  if (integerp(v1) && integerp(v2)) return ((value)((long)v1 & (long)v2));
  else runtime_error(error_bad_type);
}

OPERATION(shift_left, "n1 n2 -> n. n = n1 << n2", 2, (value v1, value v2),
	  OP_LEAF | OP_NOALLOC | OP_NOESCAPE)
{
  if (integerp(v1) && integerp(v2)) return (makeint(intval(v1) << intval(v2)));
  else runtime_error(error_bad_type);
}

OPERATION(shift_right, "n1 n2 -> n. n = n1 >> n2", 2, (value v1, value v2),
	  OP_LEAF | OP_NOALLOC | OP_NOESCAPE)
{
  if (integerp(v1) && integerp(v2)) return (makeint(intval(v1) >> intval(v2)));
  else runtime_error(error_bad_type);
}

OPERATION(bitnot, "n1 -> n2. n2 = ~n1", 1, (value v),
	  OP_LEAF | OP_NOALLOC | OP_NOESCAPE)
{
  if (integerp(v)) return ((value)(~(long)v | 1));
  else runtime_error(error_bad_type);
}

#ifdef MUME
TYPEDOP(random,
	"n1 n2 -> n. Returns a (uniform) random number between n1 and n2 inclusive",
	2, (value n1, value n2),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "nn.n")
{
  ISINT(n1);
  ISINT(n2);
  if (intval(n1) > intval(n2)) runtime_error(error_bad_value);
  return makeint(number(intval(n1), intval(n2)));
}
#endif

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
#ifdef MUME
  DEFINE("random", random);
#endif
}
