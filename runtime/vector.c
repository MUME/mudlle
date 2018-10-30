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

#include "check-types.h"
#include "prims.h"
#include "vector.h"

struct vector *empty_vector;

static enum runtime_error ct_vector_index(long idx, const char **errmsg,
                                          struct vector *vec,
                                          long *dst)
{
  if (idx < 0)
    idx += vector_len(vec);
  if (idx < 0 || idx >= vector_len(vec))
    {
      *errmsg = "vector index out of range";
      return error_bad_index;
    }
  *dst = idx;
  return error_none;
}

#define __CT_VEC_IDX_E(v, msg, dst_vec)                                 \
  ct_vector_index(v, msg, ARGN2 dst_vec, &(ARGN1 dst_vec))
#define CT_VEC_IDX(dst, vec) CT_INT_P((dst, vec), __CT_VEC_IDX_E)

TYPEDOP(vectorp, "vector?", "`x -> `b. TRUE if `x is a vector", 1, (value v),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(TYPE(v, vector));
}

TYPEDOP(make_vector, 0, "`n -> `v. Create an all-null vector of length `n,"
        " where 0 <= `n <= `MAX_VECTOR_SIZE.", 1, (value msize),
	OP_LEAF | OP_NOESCAPE, "n.v")
{
  long size;
  CHECK_TYPES(msize, CT_RANGE(size, 0, MAX_VECTOR_SIZE));
  return alloc_vector(size);
}

TYPEDOP(vector_length, 0, "`v -> `n. Return length of vector",
        1, (struct vector *vec),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "v.n")
{
  CHECK_TYPES(vec, vector);
  return makeint(vector_len(vec));
}

TYPEDOP(vector_fill, "vector_fill!",
        "`v `x -> `v. Set all elements of `v to `x",
	2, (struct vector *vec, value x),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "vx.1")
{
  CHECK_TYPES(vec, vector,
              x,   any);
  ulong len = vector_len(vec);
  /* allow readonly for empty vector */
  if (len == 0)
    return vec;

  if (obj_readonlyp(&vec->o))
    RUNTIME_ERROR(error_value_read_only, NULL);

  while (len-- > 0)
    vec->data[len] = x;

  return vec;
}

EXT_TYPEDOP(vector_ref, 0, "`v `n -> `x. Return the `n'th element of `v.\n"
            "Negative `n are counted from the end of `v.",
	    2, (struct vector *vec, value c), (vec, c),
	    OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "vn.x")
{
  long idx;
  CHECK_TYPES(vec, vector,
              c,   CT_VEC_IDX(idx, vec));
  return vec->data[idx];
}

EXT_TYPEDOP(vector_set, "vector_set!",
            "`v `n `x -> `x. Set the `n'th element of `v to `x.\n"
            "Negative `n are counted from the end of `v.\n",
	    3, (struct vector *vec, value i, value c), (vec, i, c),
	    OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "vnx.3")
{
  long idx;
  CHECK_TYPES(vec, vector,
              i,   CT_VEC_IDX(idx, vec),
              c,   any);
  if (obj_readonlyp(&vec->o))
    RUNTIME_ERROR(error_value_read_only, NULL);
  vec->data[idx] = c;
  return c;
}

TYPEDOP(vswap, "vswap!",
        "`v `n0 `n1 -> `v. Swaps elements `n0 and `n1 in the vector `v.",
        3, (struct vector *v, value n0, value n1),
        OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "vnn.v")
{
  long a, b;
  CHECK_TYPES(v, vector,
              n0, CT_VEC_IDX(a, v),
              n1, CT_VEC_IDX(b, v));
  if (obj_readonlyp(&v->o))
    RUNTIME_ERROR(error_value_read_only, NULL);
  value e = v->data[a];
  v->data[a] = v->data[b];
  v->data[b] = e;
  return v;
}

static const typing vector_tset = { "x*.v", NULL };

FULLOP(vector, 0, "`x0 `x1 ... -> `v. Returns a vector of the arguments",
       NVARARGS, (struct vector *args, ulong nargs), 0, 0,
       OP_LEAF | OP_NOESCAPE, vector_tset, static)
{
  return args;
}

FULLOP(sequence, 0,
       "`x0 `x1 ... -> `v. Returns a read-only vector of the arguments",
       NVARARGS, (struct vector *args, ulong nargs), 0, 0,
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
