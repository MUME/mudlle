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
#include "list.h"
#include "prims.h"

#include "../interpret.h"

TYPEDOP(cons, 0, "`x1 `x2 -> `l. Make a new pair from elements `x1 and `x2."
        " See also `pcons().",
	2, (value car, value cdr),
	OP_LEAF | OP_NOESCAPE, "xx.k")
{
  return alloc_list(car, cdr);
}

TYPEDOP(pcons, 0, "`x1 `x2 -> `l. Make a new read-only, possibly immutable,"
        " pair from elements `x1 and `x2.",
	2, (value car, value cdr),
	OP_LEAF | OP_NOESCAPE | OP_CONST, "xx.k")
{
  unsigned imm = immutablep(car) && immutablep(cdr) ? OBJ_IMMUTABLE : 0;
  struct list *p = alloc_list(car, cdr);
  p->o.flags |= OBJ_READONLY | imm;
  return p;
}

TYPEDOP(car, 0, "`l -> `x. Returns the first element of pair `l.",
        1, (struct list *l),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "k.x")
{
  CHECK_TYPES(l, pair);
  return l->car;
}

TYPEDOP(cdr, 0, "`l -> `x. Returns second element of pair `l.",
        1, (struct list *l),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "k.x")
{
  CHECK_TYPES(l, pair);
  return l->cdr;
}

TYPEDOP(pairp, "pair?", "`x -> `b. Returns TRUE if `x is a pair", 1, (value v),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(TYPE(v, pair));
}

TYPEDOP(listp, "list?", "`x -> `b. Returns TRUE if `x is a pair or null",
        1, (value v),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(!v || TYPE(v, pair));
}

TYPEDOP(nullp, "null?", "`x -> `b. Returns TRUE if `x is the null object",
        1, (value v),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(v == NULL);
}

EXT_TYPEDOP(set_carb, "set_car!", "`l `x -> `x. Sets the first element of"
            " pair `l to `x",
            2, (struct list *l, value x), (l, x),
            OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "kx.1")
{
  CHECK_TYPES(l, pair,
              x, any);
  if (obj_readonlyp(&l->o))
    RUNTIME_ERROR(error_value_read_only, NULL);
  return l->car = x;
}

EXT_TYPEDOP(set_cdrb, "set_cdr!", "`l `x -> `x. Sets the second element of"
            " pair `l to `x",
            2, (struct list *l, value x), (l, x),
            OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "kx.1")
{
  CHECK_TYPES(l, pair,
              x, any);
  if (obj_readonlyp(&l->o))
    RUNTIME_ERROR(error_value_read_only, NULL);
  return l->cdr = x;
}

static const typing list_tset = { ".u", "xx*.k", NULL };

FULLOP(list, 0, "`x1 ... -> `l. Returns a list of the arguments",
       NVARARGS, (struct vector *args, ulong nargs), (args, nargs),
       0, OP_LEAF | OP_NOESCAPE, list_tset, static)
{
  struct list *l = NULL;
  GCPRO(l, args);

  while (nargs > 0)
    l = alloc_list(args->data[--nargs], l);

  UNGCPRO();
  return l;
}

FULLOP(plist, 0, "`x1 ... -> `l. Returns a read-only and (if all elements"
       " are immutable) immutable list of the arguments.",
       NVARARGS, (struct vector *args, ulong nargs), (args, nargs), 0,
       OP_LEAF | OP_NOESCAPE | OP_CONST,
       list_tset, static)
{
  struct list *l = NULL;
  GCPRO(l, args);

  bool immutable = true;
  while (nargs > 0)
    {
      value v = args->data[--nargs];
      immutable = immutable && immutablep(v);
      l = alloc_list(v, l);
      l->o.flags |= OBJ_READONLY | (immutable ? OBJ_IMMUTABLE : 0);
    }

  UNGCPRO();
  return l;
}

void list_init(void)
{
  DEFINE(listp);
  DEFINE(pairp);
  DEFINE(nullp);
  DEFINE(cons);
  DEFINE(pcons);
  DEFINE(car);
  DEFINE(cdr);
  DEFINE(set_carb);
  DEFINE(set_cdrb);
  system_define("null", NULL);
  DEFINE(list);
  DEFINE(plist);
}
