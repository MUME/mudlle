#include "runtime/runtime.h"
#include "interpret.h"

TYPEDOP(cons, "x1 x2 -> l. Make a new pair from elements x1 & x2",
	2, (value car, value cdr),
	OP_LEAF | OP_NOESCAPE, "xx.k")
{
  return (alloc_list(car, cdr));
}

TYPEDOP(car, "l -> x. Returns first element of pair l", 1, (struct list *l),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "k.x")
{
  TYPEIS(l, type_pair);
  return l->car;
}

TYPEDOP(cdr, "l -> x. Returns 2nd element of pair l", 1, (struct list *l),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "k.x")
{
  TYPEIS(l, type_pair);
  return l->cdr;
}

TYPEDOP(pairp, "x -> b. Returns TRUE if x is a pair", 1, (value v),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(TYPE(v, type_pair));
}

TYPEDOP(listp, "x -> b. Returns TRUE if x is a pair or null", 1, (value v),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(!v || TYPE(v, type_pair));
}

TYPEDOP(nullp, "x -> b. Returns TRUE if x is the null object", 1, (value v),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(v == NULL);
}

TYPEDOP(setcar, "l x ->. Sets the first element of pair l to x",
	2, (struct list *l, value x),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "kx.")
{
  TYPEIS(l, type_pair);
  if (l->o.flags & OBJ_READONLY) runtime_error(error_value_read_only);
  l->car = x;
  undefined();
}

TYPEDOP(setcdr, "l x ->. Sets the 2nd element of pair l to x",
	2, (struct list *l, value x),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "kx.")
{
  TYPEIS(l, type_pair);
  if (l->o.flags & OBJ_READONLY) runtime_error(error_value_read_only);
  l->cdr = x;
  undefined();
}

VAROP(list, "x1 ... -> l. Returns a list of the arguments",
      OP_LEAF)
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
  DEFINE("list?", listp);
  DEFINE("pair?", pairp);
  DEFINE("null?", nullp);
  DEFINE("cons", cons);
  DEFINE("car", car);
  DEFINE("cdr", cdr);
  DEFINE("set_car!", setcar);
  DEFINE("set_cdr!", setcdr);
  system_define("null", NULL);
  DEFINE("list", list);
}
