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
#include "interpret.h"

TYPEDOP(cons, 0, "`x1 `x2 -> `l. Make a new pair from elements `x1 and `x2",
	2, (value car, value cdr),
	OP_LEAF | OP_NOESCAPE, "xx.k")
{
  return (alloc_list(car, cdr));
}

TYPEDOP(car, 0, "`l -> `x. Returns first element of pair `l",
        1, (struct list *l),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "k.x")
{
  TYPEIS(l, type_pair);
  return l->car;
}

TYPEDOP(cdr, 0, "`l -> `x. Returns 2nd element of pair `l", 1, (struct list *l),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "k.x")
{
  TYPEIS(l, type_pair);
  return l->cdr;
}

TYPEDOP(pairp, "pair?", "`x -> `b. Returns TRUE if `x is a pair", 1, (value v),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(TYPE(v, type_pair));
}

TYPEDOP(listp, "list?", "`x -> `b. Returns TRUE if `x is a pair or null",
        1, (value v),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(!v || TYPE(v, type_pair));
}

TYPEDOP(nullp, "null?", "`x -> `b. Returns TRUE if `x is the null object",
        1, (value v),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(v == NULL);
}

TYPEDOP(setcar, "set_car!", "`l `x ->. Sets the first element of pair `l to `x",
	2, (struct list *l, value x),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "kx.")
{
  TYPEIS(l, type_pair);
  if (l->o.flags & OBJ_READONLY) runtime_error(error_value_read_only);
  l->car = x;
  undefined();
}

TYPEDOP(setcdr, "set_cdr!", "`l `x ->. Sets the 2nd element of pair `l to `x",
	2, (struct list *l, value x),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "kx.")
{
  TYPEIS(l, type_pair);
  if (l->o.flags & OBJ_READONLY) runtime_error(error_value_read_only);
  l->cdr = x;
  undefined();
}

static const typing list_tset = { ".u", "xx*.k", NULL };

FULLOP(list, 0, "`x1 ... -> `l. Returns a list of the arguments",
       -1, (struct vector *args, ulong nargs), 0, OP_LEAF, 
       list_tset, static)
{
  struct list *l;
  struct gcpro gcpro1, gcpro2;

  l = NULL;
  GCPRO2(l, args);

  while (nargs > 0)
    l = alloc_list(args->data[--nargs], l);

  UNGCPRO();
  return l;
}

void list_init(void)
{
  DEFINE(listp);
  DEFINE(pairp);
  DEFINE(nullp);
  DEFINE(cons);
  DEFINE(car);
  DEFINE(cdr);
  DEFINE(setcar);
  DEFINE(setcdr);
  system_define("null", NULL);
  DEFINE(list);
}
