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

#include "runtime/runtime.h"
#include "vector.h"

TYPEDOP(vectorp, "x -> b. TRUE if x is a vector", 1, (value v),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(TYPE(v, type_vector));
}

TYPEDOP(make_vector, "n -> v. Create an empty vector of length n",
	1, (value size),
	OP_LEAF | OP_NOESCAPE, "n.v")
{
  struct vector *newp;

  ISINT(size);
  if(intval(size) < 0)
    runtime_error(error_bad_value);
  newp = alloc_vector(intval(size));
  return (newp);
}

TYPEDOP(vector_length, "v -> n. Return length of vector", 1, (struct vector *vec),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "v.n")
{
  TYPEIS(vec, type_vector);
  return (makeint(vector_len(vec)));
}

TYPEDOP(sequence_copy, "v -> v. Returns a readonly copy of vector v",
        1, (struct vector *vec),
	OP_LEAF | OP_NOESCAPE, "v.v")
{
  struct vector *result;
  struct gcpro gcpro1;
  long len;

  TYPEIS(vec, type_vector);
  len = vector_len(vec);

  GCPRO1(vec);
  result = alloc_vector(len);
  UNGCPRO();

  memcpy(result->data, vec->data, len * sizeof *vec->data);
  result->o.flags |= OBJ_READONLY;

  return result;
}

TYPEDOP(vector_fill, "v x -> v. Set all elements of v to x",
	2, (struct vector *vec, value x),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "vx.1")
{
  value *fill;
  ulong len;

  TYPEIS(vec, type_vector);

  if (vec->o.flags & OBJ_READONLY)
    runtime_error(error_value_read_only);

  len = vector_len(vec);
  for (fill = vec->data; len; fill++, len--) *fill = x;

  return vec;
}

TYPEDOP(vector_shift, "v n0 n1 n2 -> v. Moves n1 elements starting at"
	" index n0 n2 slots",
	4, (struct vector *vec, value mstart, value msize, value mdist),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "vnnn.1")
{
  long start = GETINT(mstart), size = GETINT(msize), dist = GETINT(mdist);
  long len;
  int i;

  TYPEIS(vec, type_vector);

  len = vector_len(vec);
  if (start < 0)
    start += len;

  if (start < 0 || start >= len)
    runtime_error(error_bad_value);
  if (size < 0 || start + size > len)
    runtime_error(error_bad_value);

  if (vec->o.flags & OBJ_READONLY)
    runtime_error(error_value_read_only);

  if (dist)
    for (i = 0; i < size; ++i)
      {
	int idx = dist < 0 ? start + i : start + size - 1 - i;
	int tidx = idx + dist;
	if (tidx >= 0 && tidx < len)
	  vec->data[tidx] = vec->data[idx];
	vec->data[idx] = NULL;
      }

  return vec;
}

EXT_TYPEDOP(vector_ref, "v n -> x. Return the n'th element of v",
	    2, (struct vector *vec, value c),
	    OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "vn.x")
{
  long idx;

  TYPEIS(vec, type_vector);
  ISINT(c);

  idx = intval(c);
  if (idx < 0)
    idx += vector_len(vec);
  if (idx < 0 || idx >= vector_len(vec)) runtime_error(error_bad_index);
  return vec->data[idx];
}

EXT_TYPEDOP(vector_set, "v n x -> x. Set the n'th element of v to x",
	    3, (struct vector *vec, value i, value c),
	    OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "vnx.3")
{
  long idx;

  TYPEIS(vec, type_vector);
  if (vec->o.flags & OBJ_READONLY) runtime_error(error_value_read_only);

  idx = GETINT(i);
  if (idx < 0)
    idx += vector_len(vec);
  if (idx < 0 || idx >= vector_len(vec)) runtime_error(error_bad_index);
  vec->data[idx] = c;

  return c;
}

static const typing vector_tset = { "x*.v", NULL };

FULLOP(vector, "x1 ... -> v. Returns a vector of the arguments",
       -1, (struct vector *args, ulong nargs), 0, OP_LEAF, 
       vector_tset, /* extern */)
{
  return args;
}

FULLOP(sequence, "x1 ... -> v. Returns a read-only vector of the arguments",
       -1, (struct vector *args, ulong nargs), 0, OP_LEAF, 
       vector_tset, /* extern */)
{
  // we could create immutable vectors here by scanning all args...
  args->o.flags |= OBJ_READONLY;

  return args;
}

void vector_init(void)
{
  DEFINE("vector?", vectorp);
  DEFINE("make_vector", make_vector);
  DEFINE("vector_length", vector_length);
  DEFINE("vector_fill!", vector_fill);
  DEFINE("vector_ref", vector_ref);
  DEFINE("vector_set!", vector_set);
  DEFINE("vector", vector);
  DEFINE("sequence", sequence);
  DEFINE("sequence_copy", sequence_copy);
  DEFINE("vector_shift!", vector_shift);
}
