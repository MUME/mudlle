/* $Log: vector.c,v $
 * Revision 1.9  1994/10/09  06:44:24  arda
 * Libraries
 * Type inference
 * Many minor improvements
 *
 * Revision 1.8  1994/08/29  15:44:55  arda
 * Mudlle stuff
 *
 * Revision 1.7  1994/08/22  11:19:08  arda
 * Changes for mudlle compiler in MUME.
 *
 * Revision 1.6  1994/08/16  19:17:22  arda
 * Added flags to primitives for better calling sequences.
 *
 * Revision 1.5  1993/10/03  14:07:28  dgay
 * Bumper disun8 update.
 *
 * Revision 1.4  1993/08/15  21:02:14  un_mec
 * Owl: Several extras functions.
 *      rent.
 *
 * Revision 1.3  1993/03/29  09:25:58  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.3  1993/03/14  16:16:57  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.1  1992/12/30  14:12:04  un_mec
 * Owl:
 * Several changes:
 * - Variables don't have separate value & function cells, instead their are
 *   now 2 types: type_function & type_variable.
 * 	-> new functions store, recall. Removed store-xx, recall-xx.
 * - New types: list (Lisp style pair), vector (array)
 *
 */

static char rcsid[] = "$Id: vector.c,v 1.9 1994/10/09 06:44:24 arda Exp $";

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
  struct vector *new;

  ISINT(size);
  new = alloc_vector(intval(size));
  return (new);
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
  long index;

  TYPEIS(vec, type_vector);
  ISINT(c);

  index = intval(c);
  if (index < 0 || index >= vector_len(vec)) runtime_error(error_bad_index);
  return (vec->data[index]);
}

TYPEDOP(vector_set, "v n x -> x. Set the n'th element of v to x",
	3, (struct vector *vec, value i, value c),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "vnx.")
{
  long index;

  TYPEIS(vec, type_vector);
  if (vec->o.flags & OBJ_READONLY) runtime_error(error_value_read_only);
  ISINT(i);

  index = intval(i);
  if (index < 0 || index >= vector_len(vec)) runtime_error(error_bad_index);
  vec->data[index] = c;

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
