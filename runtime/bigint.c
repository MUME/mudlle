/*
 * Copyright (c) 1993-2004 David Gay and Gustav Hållberg
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

#include <stdlib.h>
#include <limits.h>
#include <math.h>
#include <string.h>

#include "runtime.h"
#include "mudlle-float.h"

OPERATION(isbigint, "x -> b. True if x is a bigint", 1, (value x),
	  OP_LEAF | OP_NOESCAPE | OP_NOALLOC)
{
  return makebool(TYPE(x, type_bigint));
}

#ifdef USE_GMP
static struct alloc_list {
  struct alloc_list *next;
  void *data;
} *alloc_root = 0;

static void *mpz_alloc_fn(size_t size)
{
  struct alloc_list *m = (struct alloc_list *)malloc(sizeof *m);

  m->next = alloc_root;
  m->data = malloc(size);
  alloc_root = m;
  
  return m->data;
}

static void *mpz_realloc_fn(void *odata, size_t osize, size_t nsize)
{
  struct alloc_list *m = (struct alloc_list *)malloc(sizeof *m);

  m->next = alloc_root;
  m->data = malloc(nsize);
  memcpy(m->data, odata, osize);
  alloc_root = m;
  
  return m->data;
}

static void mpz_free_fn(void *data, size_t size)
{
}

void free_mpz_temps(void)
{
  struct alloc_list *m = alloc_root;

  while (m) 
    {
      struct alloc_list *next = m->next;

      free(m->data);
      free(m);

      m = next;
    }

  alloc_root = 0;
}

static struct bigint *get_bigint(value v)
{
  if (integerp(v))
    {
      mpz_t mpz;

      mpz_init_set_si(mpz, intval(v));
      v = (value)alloc_bigint(mpz);
      
      free_mpz_temps();
    }
  else
    TYPEIS(v, type_bigint);
  check_bigint(v);
  return v;
}

OPERATION(itobi, "n -> bi. Return n as a bigint", 1, (value n),
	  OP_LEAF | OP_NOESCAPE)
{
  mpz_t r;
  struct bigint *v;

  ISINT(n);
  mpz_init_set_si(r, intval(n));
  v = alloc_bigint(r);
  free_mpz_temps();

  return v;
}

OPERATION(ftobi, "f -> bi. Truncates f into a bigint", 
	  1, (struct mudlle_float *f), OP_LEAF | OP_NOESCAPE)
{
  mpz_t r;
  double d;
  struct bigint *v;

  d = floatval(f);

  if (!finite(d))
    runtime_error(error_bad_value);

  mpz_init_set_d(r, d);
  v = alloc_bigint(r);
  free_mpz_temps();

  return v;
}

OPERATION(bitoa, "bi -> s. Return a string representation for bi", 
	  1, (struct bigint *m), OP_LEAF | OP_NOESCAPE)
{
  char *buf;

  m = get_bigint(m);
  buf = alloca(mpz_sizeinbase(m->mpz, 10) + 2);
  mpz_get_str(buf, 10, m->mpz);

  return alloc_string(buf);
}

OPERATION(bitoa_base, "bi n -> s. Return a string representation for bi, "
	  "base n (2 - 32)",
	  2, (struct bigint *m, value v), OP_LEAF | OP_NOESCAPE)
{
  char *buf;
  long n;

  ISINT(v);
  m = get_bigint(m);
  if ((n = intval(v)) <= 1 || n > 32)
    runtime_error(error_bad_value);

  buf = alloca(mpz_sizeinbase(m->mpz, n) + 2);
  mpz_get_str(buf, n, m->mpz);

  return alloc_string(buf);
}

OPERATION(atobi, "s -> bi. Return the number in s as a bigint", 
	  1, (struct string *s), OP_LEAF | OP_NOESCAPE)
{
  mpz_t mpz;
  struct bigint *bi;
  struct gcpro gcpro1;

  TYPEIS(s, type_string);
  GCPRO1(s);

  if (mpz_init_set_str(mpz, s->str, 0))
    runtime_error(error_bad_value);

  bi = alloc_bigint(mpz);

  UNGCPRO();
  free_mpz_temps();

  return bi;
}

OPERATION(bitoi, "bi -> i. Return bi as an integer (error if overflow)", 
	  1, (struct bigint *m), OP_LEAF | OP_NOESCAPE)
{
  m = get_bigint(m);
  
  if (mpz_cmp_si(m->mpz, MAX_TAGGED_INT) > 0 ||
      mpz_cmp_si(m->mpz, MIN_TAGGED_INT) < 0)
    runtime_error(error_bad_value);

  return makeint(mpz_get_si(m->mpz));
}

OPERATION(bisgn, "bi -> n. Return -1 if bi < 0, 0 if bi == 0, or 1 if bi > 0",
	  1, (struct bigint *bi), OP_LEAF | OP_NOESCAPE)
{
  bi = get_bigint(bi);
  return makeint(mpz_sgn(bi->mpz));
}

OPERATION(bitof, "bi -> f. Return bi as a float", 
	  1, (struct bigint *m), OP_LEAF | OP_NOESCAPE)
{
  double d = mpz_get_d(get_bigint(m)->mpz);
  return makefloat(d);
}

OPERATION(bicmp, "bi1 bi2 -> n. Returns < 0 if bi1 < bi2, 0 if bi1 == bi2, "
	  "and > 0 if bi1 > bi2", 2, (struct bigint *m1, struct bigint *m2),
	  OP_LEAF | OP_NOESCAPE)
{
  struct gcpro gcpro1, gcpro2;

  GCPRO2(m1, m2);
  m1 = get_bigint(m1); 
  m2 = get_bigint(m2);
  UNGCPRO();

  return makeint(mpz_cmp(m1->mpz, m2->mpz));
}

OPERATION(bishl, "bi1 n -> bi2. Returns bi1 << n",
	  2, (struct bigint *bi, value v), OP_LEAF | OP_NOESCAPE)
{
  struct bigint *rm;
  mpz_t m;
  long n;

  ISINT(v);
  bi = get_bigint(bi);

  n = intval(v);
  if (n < 0)
    runtime_error(error_bad_value);

  mpz_init(m);
  mpz_mul_2exp(m, bi->mpz, n);
  rm = alloc_bigint(m);
  free_mpz_temps();

  return rm;
}

OPERATION(bipow, "bi1 n -> bi2. Returns bi1 raised to the power n",
	  2, (struct bigint *bi, value v), OP_LEAF | OP_NOESCAPE)
{
  mpz_t m;
  struct bigint *rm;
  long n;

  ISINT(v);
  bi = get_bigint(bi);

  n = intval(v);
  if (n < 0)
    runtime_error(error_bad_value);

  mpz_init(m);
  mpz_pow_ui(m, bi->mpz, n);
  rm = alloc_bigint(m);
  free_mpz_temps();

  return rm;
}

OPERATION(bisqrt, "bi1 -> bi2. Returns the integer part of sqrt(bi1)",
	  1, (struct bigint *bi), OP_LEAF | OP_NOESCAPE)
{
  mpz_t m;

  bi = get_bigint(bi);
  if (mpz_sgn(bi->mpz) < 0)
    runtime_error(error_bad_value);

  mpz_init(m);
  mpz_sqrt(m, bi->mpz);
  bi = alloc_bigint(m);
  free_mpz_temps();

  return bi;
}

OPERATION(bifac, "n -> bi1. Returns n!",
	  1, (value v), OP_LEAF | OP_NOESCAPE)
{
  mpz_t m;
  struct bigint *rm;
  long n;

  ISINT(v);

  n = intval(v);
  if (n < 0)
    runtime_error(error_bad_value);
  mpz_init(m);
  mpz_fac_ui(m, intval(v));
  rm = alloc_bigint(m);
  free_mpz_temps();

  return rm;
}

#define BIUNOP(name, desc)					\
OPERATION(bi ## name, "bi1 -> bi2. Returns " desc,		\
	  1, (struct bigint *bi), OP_LEAF | OP_NOESCAPE)	\
{								\
  mpz_t m;							\
  struct bigint *rm;						\
								\
  bi = get_bigint(bi);						\
  mpz_init(m);							\
  mpz_ ## name(m, bi->mpz);					\
  rm = alloc_bigint(m);						\
  free_mpz_temps();						\
								\
  return rm;							\
}

#define BIBINOP(name, sym, isdiv)					\
OPERATION(bi ## name, "bi1 bi2 -> bi3. Returns bi1 " #sym " bi2",	\
	  2, (struct bigint *bi1, struct bigint *bi2),			\
	  OP_LEAF | OP_NOESCAPE)					\
{									\
  struct gcpro gcpro1, gcpro2;						\
  mpz_t m;								\
  struct bigint *rm;							\
									\
  GCPRO2(bi1, bi2);							\
  bi1 = get_bigint(bi1);						\
  bi2 = get_bigint(bi2);						\
  UNGCPRO();								\
									\
  if (isdiv && !mpz_cmp_si(bi2->mpz, 0))				\
    runtime_error(error_divide_by_zero);				\
									\
  mpz_init(m);								\
  mpz_ ## name(m, bi1->mpz, bi2->mpz);					\
  rm = alloc_bigint(m);							\
  free_mpz_temps();							\
									\
  return rm;								\
}

#else  /* ! USE_GMP */

void free_mpz_temps(void)
{
}

UNIMPLEMENTED(itobi, "n -> bi. Return n as a bigint", 1, (value n),
	      OP_LEAF | OP_NOESCAPE)

UNIMPLEMENTED(ftobi, "f -> bi. Truncates f into a bigint", 
	      1, (struct mudlle_float *f), OP_LEAF | OP_NOESCAPE)

UNIMPLEMENTED(bitoa, "bi -> s. Return a string representation for bi", 
	      1, (struct bigint *m), OP_LEAF | OP_NOESCAPE)

UNIMPLEMENTED(bitoa_base, "bi n -> s. Return a string representation for bi, "
	      "base n (2 - 32)",
	      2, (struct bigint *m, value v), OP_LEAF | OP_NOESCAPE)

UNIMPLEMENTED(atobi, "s -> bi. Return the number in s as a bigint", 
	      1, (struct string *s), OP_LEAF | OP_NOESCAPE)

UNIMPLEMENTED(bitoi, "bi -> i. Return bi as an integer (error if overflow)", 
	      1, (struct bigint *m), OP_LEAF | OP_NOESCAPE)

UNIMPLEMENTED(bisgn, "bi -> n. Return -1 if bi < 0, 0 if bi == 0, or 1 if bi > 0",
	      1, (struct bigint *bi), OP_LEAF | OP_NOESCAPE)

UNIMPLEMENTED(bitof, "bi -> f. Return bi as a float", 
	      1, (struct bigint *m), OP_LEAF | OP_NOESCAPE)

UNIMPLEMENTED(bicmp, "bi1 bi2 -> n. Returns < 0 if bi1 < bi2, 0 if bi1 == bi2, "
	      "and > 0 if bi1 > bi2", 2, (struct bigint *m1, struct bigint *m2),
	      OP_LEAF | OP_NOESCAPE)

UNIMPLEMENTED(bishl, "bi1 n -> bi2. Returns bi1 << n",
	      2, (struct bigint *bi, value v), OP_LEAF | OP_NOESCAPE)

UNIMPLEMENTED(bipow, "bi1 n -> bi2. Returns bi1 raised to the power n",
	      2, (struct bigint *bi, value v), OP_LEAF | OP_NOESCAPE)

UNIMPLEMENTED(bisqrt, "bi1 -> bi2. Returns the integer part of sqrt(bi1)",
	      1, (struct bigint *bi), OP_LEAF | OP_NOESCAPE)

UNIMPLEMENTED(bifac, "n -> bi1. Returns n!",
	      1, (value v), OP_LEAF | OP_NOESCAPE)

#define BIUNOP(name, desc)					\
UNIMPLEMENTED(bi ## name, "bi1 -> bi2. Returns " desc,		\
	      1, (struct bigint *bi), OP_LEAF | OP_NOESCAPE)

#define BIBINOP(name, sym, isdiv)					\
UNIMPLEMENTED(bi ## name, "bi1 bi2 -> bi3. Returns bi1 " #sym " bi2",	\
	      2, (struct bigint *bi1, struct bigint *bi2),		\
	      OP_LEAF | OP_NOESCAPE)

#endif /* ! USE_GMP */

BIUNOP(com, "~bi")
BIUNOP(neg, "-bi")
BIUNOP(abs, "|bi|")

BIBINOP(add, +, 0)
BIBINOP(sub, -, 0)
BIBINOP(mul, *, 0)
BIBINOP(tdiv_q, /, 1)
BIBINOP(tdiv_r, %, 1)
BIBINOP(and, &, 0)
BIBINOP(ior, |, 0)

void bigint_init(void)
{
#ifdef USE_GMP
  mp_set_memory_functions(&mpz_alloc_fn, &mpz_realloc_fn, &mpz_free_fn);
#endif

  DEFINE("bigint?", isbigint);
  DEFINE("bicmp", bicmp);
  DEFINE("bisgn", bisgn);


  DEFINE("bitoi", bitoi);
  DEFINE("itobi", itobi);
  DEFINE("bitoa", bitoa);
  DEFINE("atobi", atobi);
  DEFINE("bitof", bitof);
  DEFINE("ftobi", ftobi);
  DEFINE("bitoa_base", bitoa_base);
  DEFINE("bineg", bineg);
  DEFINE("binot", bicom);
  DEFINE("biabs", biabs);

  DEFINE("bishl", bishl);
  DEFINE("bipow", bipow);
  DEFINE("bifac", bifac);
  DEFINE("bisqrt", bisqrt);

  DEFINE("biadd", biadd);
  DEFINE("bisub", bisub);
  DEFINE("bimul", bimul);
  DEFINE("bidiv", bitdiv_q);
  DEFINE("bimod", bitdiv_r);
  DEFINE("biand", biand);
  DEFINE("bior", biior);
}
