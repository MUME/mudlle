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

TYPEDOP(vector_fill, "v x -> . Set all elements of v to x",
	2, (struct vector *vec, value x),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "vx.")
{
  value *fill;
  ulong len;

  TYPEIS(vec, type_vector);

  len = vector_len(vec);
  for (fill = vec->data; len; fill++, len--) *fill = x;
  undefined();
}

TYPEDOP(vector_ref, "v n -> x. Return the n'th element of v",
	2, (struct vector *vec, value c),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "vn.x")
{
  long idx;

  TYPEIS(vec, type_vector);
  ISINT(c);

  idx = intval(c);
  if (idx < 0 || idx >= vector_len(vec)) runtime_error(error_bad_index);
  return (vec->data[idx]);
}

TYPEDOP(vector_set, "v n x -> x. Set the n'th element of v to x",
	3, (struct vector *vec, value i, value c),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "vnx.")
{
  long idx;

  TYPEIS(vec, type_vector);
  if (vec->o.flags & OBJ_READONLY) runtime_error(error_value_read_only);
  ISINT(i);

  idx = intval(i);
  if (idx < 0 || idx >= vector_len(vec)) runtime_error(error_bad_index);
  vec->data[idx] = c;

  return c;
}

VAROP(vector, "x1 ... -> v. Returns a vector of the arguments",
      OP_LEAF)
{
  return args;
}

VAROP(sequence, "x1 ... -> v. Returns a sequence (readonly vector) of the arguments",
      OP_LEAF)
{
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
}
