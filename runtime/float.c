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

#include <errno.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#include "mudlle.h"
#include "mvalues.h"
#include "runtime.h"
#include "mudlle-float.h"

#if defined(sparc) && !defined(linux)
#include <ieeefp.h>

static int isinf(double x)
{
  int class = fpclass(x);

  return class == FP_NINF ? -1 : class == FP_PINF ? 1 : 0;
}
#endif

#define FUNFUNC(name)                                           \
  OPERATION(f ## name, 0,                                       \
            "`f1 -> `f2. Returns " #name "(`f1)", 1,            \
	    (value f), OP_LEAF | OP_NOESCAPE)                   \
{                                                               \
  return makefloat(name(floatval(f)));                          \
}

#define FBINFUNC(name, fname)                                   \
  OPERATION(f ## name, 0,                                       \
            "`f1 `f2 -> `f3. Returns " #name "(`f1, `f2)", 2,   \
	    (value f1, value f2), OP_LEAF | OP_NOESCAPE)        \
{                                                               \
  return makefloat(fname(floatval(f1), floatval(f2)));          \
}

#define FBINOP(name, op)                                        \
  OPERATION(f ## name, 0,                                       \
            "`f1 `f2 -> `f3. Returns `f1 " #op " `f2", 2,       \
	    (value f1, value f2), OP_LEAF | OP_NOESCAPE)        \
{                                                               \
  return makefloat(floatval(f1) op floatval(f2));               \
}

OPERATION(isfloatp, "float?", "`x -> `b. Returns TRUE if `x is a float",
          1, (value x),
	  OP_LEAF | OP_NOESCAPE | OP_NOALLOC)
{
  return makebool(TYPE(x, type_float));
}

OPERATION(isffinitep, "ffinite?",
          "`f -> `b. Returns TRUE if `f is neither infinite or not-a-number",
	  1, (value x), OP_LEAF | OP_NOESCAPE | OP_NOALLOC)
{
  return makebool(finite(floatval(x)));
}

OPERATION(isfnanp, "fnan?",
          "`f -> `b. Returns TRUE if `f is not-a-number (NaN)", 1, (value x),
	  OP_LEAF | OP_NOESCAPE | OP_NOALLOC)
{
  return makebool(isnan(floatval(x)));
}

OPERATION(isfinfp, "finf?", "`f -> `n. Returns -1 if `f is negative infinity"
          " (-Inf) or 1 for positive infinity (Inf), or 0 otherwise",
          1, (value x),
	  OP_LEAF | OP_NOESCAPE | OP_NOALLOC)
{
  return makeint(isinf(floatval(x)));
}

OPERATION(fabs, 0, "`f1 -> `f2. Returns | `f1 |", 1,
	  (value f), OP_LEAF | OP_NOESCAPE)
{
  return makefloat(fabs(floatval(f)));
}

OPERATION(fneg, 0, "`f1 -> `f2. Returns -`f1", 1,
	  (value f), OP_LEAF | OP_NOESCAPE)
{
  return makefloat(-floatval(f));
}

OPERATION(frandom, 0, " -> `f. Returns a random value in [0, 1)", 0,
	  (), OP_LEAF | OP_NOESCAPE)
{
#ifdef WIN32
  return makefloat(rand() / (RAND_MAX + 1.0));
#else
  return makefloat(random() / (RAND_MAX + 1.0));
#endif
}

OPERATION(fsign, 0, "`f -> `n. Returns -1 for negative `f, 1 for positive,"
          " 0 for `f == 0. Error for not-a-number (NaN)", 1,
	  (value f), OP_LEAF | OP_NOESCAPE)
{
  double d = floatval(f);
  
  if (isnan(d))
    runtime_error(error_bad_value);

  return makeint(d < 0 ? -1 : d > 0 ? 1 : 0);
}

OPERATION(ftoi, 0, "`f -> `n. Returns int(`f). Error if out of range",
          1, (value f), 
	  OP_LEAF | OP_NOESCAPE | OP_NOALLOC)
{
  double d = floatval(f);

  if (d >= MAX_TAGGED_INT + 1 || d <= MIN_TAGGED_INT - 1)
    runtime_error(error_bad_value);

  return makeint((long)d);
}

OPERATION(itof, 0, "`n -> `f. Returns the integer `n as a float", 1, (value n),
	  OP_LEAF | OP_NOESCAPE)
{
  ISINT(n);
  return makefloat((double)intval(n));
}

OPERATION(atof, 0, "`s -> `f. Converts string to float."
          " Returns `s if conversion failed",
	  1, (struct string *s),
	  OP_LEAF | OP_NOESCAPE)
{
  double d;
  
  TYPEIS(s, type_string);
  if (!mudlle_strtofloat(s->str, &d))
    return s;
  else
    return makefloat(d);
}

OPERATION(ftoa, 0, "`f -> `s. Converts float to a 0f... string."
          " See `format_float", 1, (value f),
	  OP_LEAF | OP_NOESCAPE)
{
  char buf[20];
  union {
    double d;
    long   l[2];
  } u;
  
  u.d = floatval(f);

  snprintf(buf, sizeof buf, "0f%08lx%08lx", u.l[1], u.l[0]);
  return alloc_string(buf);
}

TYPEDOP(format_float, 0,
        "`f `s0 -> `s1. Format float `f using string `s0 (printf format"
        " specifier eEgGf) into `s1",
        2, (value f, struct string *s), OP_LEAF | OP_NOESCAPE,
        "os.s")
{
  double d = floatval(f);
  char *sp, buf[128];
  static const char flags[] = "#0- +";
  static const char formats[] = "eEfgG";

  TYPEIS(s, type_string);

  sp = s->str;

  if (*sp++ != '%') 
    runtime_error(error_bad_value);

  while(strchr(flags, *sp))
    ++sp;

  while(*sp >= '0' && *sp <= '9')
    ++sp;
  if (*sp == '.')
    {
      ++sp;
      while(*sp >= '0' && *sp <= '9') ++sp;
    }

  if (!strchr(formats, *sp) || *(sp + 1)) 
    runtime_error(error_bad_value);

  snprintf(buf, sizeof buf, s->str, d);
  return alloc_string(buf);
}

OPERATION(fpow, 0, "`f1 `f2 -> `f3. Returns `f1 raised to the power of `f2",
	  2, (value f1, value f2), OP_LEAF | OP_NOESCAPE)
{
  double d1 = floatval(f1), d2 = floatval(f2);
  return makefloat(pow(d1, d2));
}

OPERATION(fcmp, 0, "`f1 `f2 -> `n. Returns -1 if `f1 < `f2, 0 if `f1 = `f2,"
          " 1 if `f1 > `f2",
	  2, (value f1, value f2), OP_LEAF | OP_NOESCAPE | OP_NOALLOC)
{
  double d1 = floatval(f1), d2 = floatval(f2);

  if (d1 < d2)
    return makeint(-1);
  if (d1 == d2)
    return makeint(0);
  if (d1 > d2)
    return makeint(1);

  runtime_error(error_bad_value);

  NOTREACHED;
}

FUNFUNC(sqrt)
FUNFUNC(exp)
FUNFUNC(log)
FUNFUNC(sin)
FUNFUNC(cos)
FUNFUNC(tan)
FUNFUNC(atan)
FUNFUNC(asin)
FUNFUNC(acos)

FUNFUNC(ceil)
FUNFUNC(floor)

FBINFUNC(atan2, atan2)
FBINFUNC(hypot, hypot)
FBINFUNC(mod, fmod)

FBINOP(add, +)
FBINOP(sub, -)
FBINOP(mul, *)
FBINOP(div, /)

#define DEFCONST(name) system_define(#name, alloc_mudlle_float(name))

void float_init(void)
{
  DEFINE(fsqrt);
  DEFINE(fexp);
  DEFINE(flog);
  DEFINE(fsin);
  DEFINE(fcos);
  DEFINE(ftan);
  DEFINE(fatan);
  DEFINE(fasin);
  DEFINE(facos);
  DEFINE(fmod);

  DEFINE(fceil);
  DEFINE(ffloor);

  DEFINE(fatan2);
  DEFINE(fhypot);
  DEFINE(fpow);

  DEFINE(fadd);
  DEFINE(fsub);
  DEFINE(fmul);
  DEFINE(fdiv);

  DEFINE(fneg);
  DEFINE(fabs);
  DEFINE(fsign);

  DEFINE(ftoa);
  DEFINE(ftoi);
  DEFINE(itof);
  DEFINE(fcmp);
  DEFINE(atof);
  DEFINE(format_float);

  DEFINE(frandom);

  DEFINE(isfloatp);
  DEFINE(isfnanp);
  DEFINE(isfinfp);
  DEFINE(isffinitep);

  DEFCONST(M_PI);
  DEFCONST(M_E);
  DEFCONST(M_LN2);
  DEFCONST(M_LN10);
  DEFCONST(M_1_PI);
  DEFCONST(M_SQRT2);
  DEFCONST(M_SQRT1_2);
  DEFCONST(M_LOG2E);
  DEFCONST(M_LOG10E);
  DEFCONST(HUGE_VAL);
}
