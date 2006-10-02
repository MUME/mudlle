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

#include "mvalues.h"
#include "runtime.h"
#include "mudlle-float.h"

#if defined(HAVE_LPKIT_LPKIT_H)
#  include <lpkit/lpkit.h>
#elif defined(HAVE_LP_SOLVE_LPKIT_H)
#  include <lp_solve/lpkit.h>
#  undef FAILURE
#else
#  warning lp-solve library (lpk) missing
#endif

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

TYPEDOP(isfloatp, "float?", "`x -> `b. Returns TRUE if `x is a float",
        1, (value x),
        OP_LEAF | OP_NOESCAPE | OP_NOALLOC, "x.n")
{
  return makebool(TYPE(x, type_float));
}

TYPEDOP(isffinitep, "ffinite?",
        "`f -> `b. Returns TRUE if `f is neither infinite or not-a-number",
        1, (value x), OP_LEAF | OP_NOESCAPE | OP_NOALLOC, "x.n")
{
  return makebool(finite(floatval(x)));
}

TYPEDOP(isfnanp, "fnan?",
        "`f -> `b. Returns TRUE if `f is not-a-number (NaN)", 1, (value x),
        OP_LEAF | OP_NOESCAPE | OP_NOALLOC, "x.n")
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

TYPEDOP(frandom, 0, " -> `f. Returns a random value in [0, 1)", 0,
        (void), OP_LEAF | OP_NOESCAPE, ".o")
{
#ifdef WIN32
  return makefloat(rand() / (RAND_MAX + 1.0));
#else
  return makefloat(random() / (RAND_MAX + 1.0));
#endif
}

TYPEDOP(fsign, 0, "`f -> `n. Returns -1 for negative `f, 1 for positive,"
        " 0 for `f == 0. Error for not-a-number (NaN)", 1,
        (value f), OP_LEAF | OP_NOESCAPE, "x.n")
{
  double d = floatval(f);
  
  if (isnan(d))
    runtime_error(error_bad_value);

  return makeint(d < 0 ? -1 : d > 0 ? 1 : 0);
}

TYPEDOP(ftoi, 0, "`f -> `n. Returns int(`f). Error if out of range",
        1, (value f), 
        OP_LEAF | OP_NOESCAPE | OP_NOALLOC, "x.n")
{
  double d = floatval(f);

  if (d >= MAX_TAGGED_INT + 1 || d <= MIN_TAGGED_INT - 1)
    runtime_error(error_bad_value);

  return makeint((long)d);
}

TYPEDOP(itof, 0, "`n -> `f. Returns the integer `n as a float", 1, (value n),
        OP_LEAF | OP_NOESCAPE, "n.o")
{
  ISINT(n);
  return makefloat((double)intval(n));
}

TYPEDOP(atof, 0, "`s -> `f. Converts string to float."
        " Returns `s if conversion failed",
        1, (struct string *s),
        OP_LEAF | OP_NOESCAPE, "s.x")
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
        "xs.s")
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

TYPEDOP(fcmp, 0, "`f1 `f2 -> `n. Returns -1 if `f1 < `f2, 0 if `f1 = `f2,"
        " 1 if `f1 > `f2",
        2, (value f1, value f2), OP_LEAF | OP_NOESCAPE | OP_NOALLOC,
        "xx.n")
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

VAROP(lp_solve, 0,
      "`v1[f]:objective `v2[v[f]]:constraints [`n1:minmax `v3[f]:lower_bound"
      " `v4[f]:upper_bound `v5[b]:integer?] -> `v[f]|`n. Maximize objective"
      " function `v1, given constraints `v2 and optional lower-upper bounds"
      " `v3 and `v4 (default between 0 and +INF). `v2 is a vector of"
      " constraints, each constraint is expressed as [`weights `sign `rhs]"
      " where sign <, =, > 0 if constraint must be <, =, > `rhs."
      " Nonzero elements of `v5 correspond to integer variables."
      " If `n1 <= 0, minimize objective function instead of maximizing."
      " The function returns the optimal vector or a numerical error code"
      " (see LP_* constants).",
      OP_LEAF)
{
  int variables, constraints, i, j;
  struct vector *objective, *constr_matrix;
  struct vector *lower_bound, *upper_bound, *intvar;
  struct vector *c;
  int min_max = 1;

  const int MAX_VARIABLES = 100;
  const int MAX_CONSTRAINTS = 100;

#ifdef HAVE_LIB_LPK
  lprec *lp;
  float f;
  int rc;
  struct gcpro gcpro1;
  struct vector *r;
#endif

  if (nargs < 2)
    runtime_error(error_wrong_parameters);

  objective = args->data[0];
  constr_matrix = args->data[1];
  lower_bound = upper_bound = intvar = NULL;

  TYPEIS(objective, type_vector);
  TYPEIS(constr_matrix, type_vector);

  variables = vector_len(objective);
  constraints = vector_len(constr_matrix);

  if (variables > MAX_VARIABLES || constraints > MAX_CONSTRAINTS)
    runtime_error(error_bad_index);

  if (nargs >= 3)
    {
      ISINT(args->data[2]);
      min_max = intval(args->data[2]);
    }

  if (nargs >= 4)
    {
      lower_bound = args->data[3];
      TYPEIS(lower_bound, type_vector);
      if (vector_len(lower_bound) != variables)
	runtime_error(error_bad_value);
    }

  if (nargs >= 5)
    {
      upper_bound = args->data[4];
      TYPEIS(upper_bound, type_vector);
      if (vector_len(upper_bound) != variables)
	runtime_error(error_bad_value);
    }

  if (nargs >= 6)
    {
      intvar = args->data[5];
      TYPEIS(intvar, type_vector);
      if (vector_len(intvar) != variables)
	runtime_error(error_bad_value);
    }

  if (nargs > 6)
    runtime_error(error_wrong_parameters);

  /* Type checking */
  for (j = 0; j < variables; ++j)
    floatval(objective->data[j]);

  if (lower_bound)
    for (j = 0; j < variables; ++j)
      floatval(lower_bound->data[j]);

  if (upper_bound)
    for (j = 0; j < variables; ++j)
      floatval(upper_bound->data[j]);

  if (intvar)
    for (j = 0; j < variables; ++j)
      floatval(intvar->data[j]);

  for (i = 0; i < constraints; ++i)
    {
      TYPEIS(c = constr_matrix->data[i], type_vector);
      if (vector_len(c) != variables + 2)
	runtime_error(error_bad_value);
      for (j = 0; j < variables + 2; ++j)
	floatval(c->data[j]);
    }

#ifdef HAVE_LIB_LPK
  /* Set up linear programming problem */
  lp = make_lp(constraints, variables);
  
  /* Choose maximization or minimization */
  if (min_max > 0)
    set_maxim(lp);
  else
    set_minim(lp);

  /* Set up objective function */
  for (j = 0; j < variables; ++j)
    set_mat(lp, 0, j + 1, floatval(objective->data[j]));

  /* Set up bounds */
  if (lower_bound)
    for (j = 0; j < variables; ++j)
      set_lowbo(lp, j + 1, floatval(lower_bound->data[j]));

  if (upper_bound)
    for (j = 0; j < variables; ++j)
      set_upbo(lp, j + 1, floatval(upper_bound->data[j]));

  /* Set up constraints */
  for (i = 0; i < constraints; ++i)
    {
      c = constr_matrix->data[i];
      for(j = 0; j < variables; ++j)
	set_mat(lp, i + 1, j + 1, floatval(c->data[j]));

      f = floatval(c->data[variables]);
      if (f < 0)
	set_constr_type(lp, i + 1, LE);
      else if (f > 0)
	set_constr_type(lp, i + 1, GE);
      else
	set_constr_type(lp, i + 1, EQ);

      set_rh(lp, i + 1, floatval(c->data[variables + 1]));
    }

  /* Set up integer variables */
  if (intvar)
    for (j = 0; j < variables; ++j)
      set_int(lp, j + 1, floatval(intvar->data[j]) != 0);

  /* Solve */
  rc = solve(lp);

  /* Return results */
  if (rc != 0)
    {
      delete_lp(lp);
      return makeint(rc);
    }
  else
    {
      r = alloc_vector(variables);
      GCPRO1(r);
      for (j = 0; j < variables; ++j)
	r->data[j] = makefloat((double)lp->best_solution[lp->rows + j + 1]);
      delete_lp(lp);
      UNGCPRO();
      return r;
    }
#else
  return makeint(0);
#endif /* HAVE_LIB_LPK */
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

  DEFINE(lp_solve);

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

  /* lp_solve return codes. 
   * Not all of these are currently in use, but let's reserve the names... */

#if defined(HAVE_LPKIT_LPKIT_H) || defined(HAVE_LP_SOLVE_LPKIT_H)
  system_define("LP_OPTIMAL",       makeint(OPTIMAL));
  system_define("LP_MILP_FAIL",     makeint(MILP_FAIL));
  system_define("LP_INFEASIBLE",    makeint(INFEASIBLE));
  system_define("LP_UNBOUNDED",     makeint(UNBOUNDED));
  system_define("LP_FAILURE",       makeint(4));       /* #undefined above */
  system_define("LP_RUNNING",       makeint(RUNNING));
  system_define("LP_FEAS_FOUND",    makeint(FEAS_FOUND));
  system_define("LP_NO_FEAS_FOUND", makeint(NO_FEAS_FOUND));
  system_define("LP_BREAK_BB",      makeint(BREAK_BB));
#endif
}
