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

#include "runtime.h"
#include "vector.h"

struct vector *empty_vector;

TYPEDOP(vectorp, "vector?", "`x -> `b. TRUE if `x is a vector", 1, (value v),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(TYPE(v, type_vector));
}

TYPEDOP(make_vector, 0, "`n -> `v. Create an all-null vector of length `n,"
        " where 0 <= `n <= `MAX_VECTOR_SIZE.", 1, (value msize),
	OP_LEAF | OP_NOESCAPE, "n.v")
{
  long size = GETRANGE(msize, 0, MAX_VECTOR_SIZE);
  return alloc_vector(size);
}

TYPEDOP(vector_length, 0, "`v -> `n. Return length of vector",
        1, (struct vector *vec),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "v.n")
{
  TYPEIS(vec, type_vector);
  return makeint(vector_len(vec));
}

TYPEDOP(vector_fill, "vector_fill!",
        "`v `x -> `v. Set all elements of `v to `x",
	2, (struct vector *vec, value x),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "vx.1")
{
  TYPEIS(vec, type_vector);

  ulong len = vector_len(vec);
  /* allow readonly for empty vector */
  if (len == 0)
    return vec;

  if (obj_readonlyp(&vec->o))
    runtime_error(error_value_read_only);

  while (len-- > 0)
    vec->data[len] = x;

  return vec;
}

EXT_TYPEDOP(vector_ref, 0, "`v `n -> `x. Return the `n'th element of `v.\n"
            "Negative `n are counted from the end of `v.",
	    2, (struct vector *vec, value c),
	    OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "vn.x")
{
  TYPEIS(vec, type_vector);
  if (!integerp(c))
    primitive_runtime_error(error_bad_type, &op_vector_ref, 2, vec, c);

  long idx = intval(c);
  if (idx < 0)
    idx += vector_len(vec);
  if (idx < 0 || idx >= vector_len(vec))
    primitive_runtime_error(error_bad_index, &op_vector_ref, 2, vec, c);
  return vec->data[idx];
}

EXT_TYPEDOP(vector_set, "vector_set!",
            "`v `n `x -> `x. Set the `n'th element of `v to `x.\n"
            "Negative `n are counted from the end of `v.\n",
	    3, (struct vector *vec, value i, value c),
	    OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "vnx.3")
{
  TYPEIS(vec, type_vector);
  if (obj_readonlyp(&vec->o))
    primitive_runtime_error(error_value_read_only, &op_vector_set, 3,
                            vec, i, c);
  if (!integerp(i))
    primitive_runtime_error(error_bad_type, &op_vector_set, 3,
                            vec, i, c);

  long idx = intval(i);
  if (idx < 0)
    idx += vector_len(vec);
  if (idx < 0 || idx >= vector_len(vec))
    primitive_runtime_error(error_bad_index, &op_vector_set, 3,
                            vec, i, c);
  vec->data[idx] = c;

  return c;
}

TYPEDOP(vswap, "vswap!",
        "`v `n0 `n1 -> `v. Swaps elements `n0 and `n1 in the vector `v.",
        3, (struct vector *v, value n0, value n1),
        OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "vnn.v")
{
  TYPEIS(v, type_vector);
  long a = GETINT(n0), b = GETINT(n1);
  if (obj_readonlyp(&v->o))
    runtime_error(error_value_read_only);

  long vlen = vector_len(v);
  if (a < 0)
    a += vlen;
  if (b < 0)
    b += vlen;
  if (a < 0 || a >= vlen || b < 0 || b >= vlen)
    runtime_error(error_bad_index);
  value e = v->data[a];
  v->data[a] = v->data[b];
  v->data[b] = e;
  return v;
}

static const typing vector_tset = { "x*.v", NULL };

FULLOP(vector, 0, "`x0 `x1 ... -> `v. Returns a vector of the arguments",
       NVARARGS, (struct vector *args, ulong nargs), 0, OP_LEAF | OP_NOESCAPE,
       vector_tset, static)
{
  return args;
}

FULLOP(sequence, 0,
       "`x0 `x1 ... -> `v. Returns a read-only vector of the arguments",
       NVARARGS, (struct vector *args, ulong nargs), 0,
       OP_LEAF | OP_NOESCAPE | OP_CONST,
       vector_tset, static)
{
  // we could create immutable vectors here by scanning all args...
  return make_readonly(args);
}

void vector_init(void)
{
  empty_vector = alloc_vector(0);
  staticpro(&empty_vector);

  DEFINE(vectorp);
  DEFINE(make_vector);
  DEFINE(vector_length);
  DEFINE(vector_fill);
  DEFINE(vector_ref);
  DEFINE(vector_set);
  DEFINE(vector);
  DEFINE(sequence);
  DEFINE(vswap);

  system_define("MAX_VECTOR_SIZE", makeint(MAX_VECTOR_SIZE));
}
