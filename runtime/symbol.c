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

#include <string.h>

#include "runtime/runtime.h"
#include "table.h"
#include "call.h"


TYPEDOP(symbolp, "symbol?", "`x -> `b. TRUE if `x is a symbol", 1, (value v),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(TYPE(v, type_symbol));
}

TYPEDOP(symbol_name, 0, "`sym -> `s. Returns the name of a symbol",
	1, (struct symbol *v),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "y.s")
{
  TYPEIS(v, type_symbol);
  return (v->name);
}

TYPEDOP(symbol_get, 0, "`sym -> `x. Returns the value of a symbol",
	1, (struct symbol *v),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "y.x")
{
  TYPEIS(v, type_symbol);
  return (v->data);
}

TYPEDOP(symbol_set, "symbol_set!",
        "`sym `x -> . Sets the value of symbol `sym to `x",
	2, (struct symbol *s, value val),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "yx.")
{
  TYPEIS(s, type_symbol);

  if (s->o.flags & OBJ_READONLY) runtime_error(error_value_read_only);
  s->data = val;
  undefined();
}

TYPEDOP(tablep, "table?", "`x -> `b. TRUE if `x is a symbol table", 1, (value v),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(TYPE(v, type_table));
}

TYPEDOP(make_table, 0, "-> `table. Create a new (empty) symbol table",
        0, (void), OP_LEAF | OP_NOESCAPE, ".t")
{
  return (alloc_table(DEF_TABLE_SIZE));
}

TYPEDOP(table_list, 0,
	"`table -> `l. Returns list of symbols in `table whose value non-null",
	1, (struct table *table),
	OP_LEAF | OP_NOESCAPE, "t.l")
{
  TYPEIS(table, type_table);

  return table_list(table);
}

static struct {
  struct vector *buckets;
  int used;
} copied_table;

static void copy_table_entry(struct symbol *sym)
{
  if (sym->data)
    copied_table.buckets->data[copied_table.used++] = sym;
}

static struct vector *make_table_copy(struct table *table, int *used)
{
  struct gcpro gcpro1;

  TYPEIS(table, type_table);

  GCPRO1(table);
  copied_table.buckets = alloc_vector(table_entries(table));
  UNGCPRO();

  copied_table.used = 0;
  
  table_foreach(table, copy_table_entry);

  *used = copied_table.used;
  return copied_table.buckets;
}

TYPEDOP(table_foreach, 0,
        "`c `table -> . Runs `c(`x) for each element `x in `table. It is safe"
        " to modify `table from `c()",
	2, (value f, struct table *table),
	0, "ft.")
{
  struct gcpro gcpro1, gcpro2;
  struct vector *buckets = NULL;
  int i, used;

  callable(f, 1);
  GCPRO2(f, buckets);

  buckets = make_table_copy(table, &used);

  for (i = 0; i < used; ++i)
    if (buckets->data[i])
      call1(f, buckets->data[i]);

  UNGCPRO();

  undefined();
}

TYPEDOP(table_reduce, 0, "`f `x0 `t -> `x. Reduces table `t with function"
        " `x = `f(`s, `x) for each symbol `s and initial value `x0",
	3, (value f, value x, struct table *table),
	0, "fxt.x")
{
  struct gcpro gcpro1, gcpro2, gcpro3;
  struct vector *buckets = NULL;
  int used;

  GCPRO3(f, x, buckets);
  callable(f, 2);

  buckets = make_table_copy(table, &used);

  for (int i = 0; i < used; ++i)
    if (buckets->data[i])
      x = call2(f, buckets->data[i], x);

  UNGCPRO();

  return x;
}

TYPEDOP(table_existsp, "table_exists?",
        "`c `table -> `x. Returns the first symbol `s in `table"
	" for which `c(`s) is true, or false",
	2, (value f, struct table *table),
	0, "ft.x")
{
  struct gcpro gcpro1, gcpro2;
  struct vector *buckets = NULL;
  value res = makebool(0);
  int i, used;

  callable(f, 1);
  GCPRO2(f, buckets);

  buckets = make_table_copy(table, &used);

  for (i = 0; i < used; ++i)
    if (buckets->data[i]
	&& istrue(call1(f, buckets->data[i])))
      {
	res = buckets->data[i];
	break;
      }

  UNGCPRO();

  return res;
}

TYPEDOP(table_vector, 0,
        "`table -> `v. Returns a vector of the entries in table",
	1, (struct table *table),
	OP_LEAF | OP_NOESCAPE, "t.v")
{
  struct gcpro gcpro1;
  struct vector *res, *buckets = NULL;
  int used;

  GCPRO1(buckets);

  buckets = make_table_copy(table, &used);

  if (used == vector_len(buckets))
    res = buckets;
  else
    {
      res = alloc_vector(used);
      memcpy(res->data,
	     buckets->data,
	     sizeof res->data[0] * used);
    }

  UNGCPRO();

  return res;
}

TYPEDOP(table_prefix, 0, "`table `s -> `l. Returns list of symbols in `table"
        " whose value is non-null, and whose name starts with `s",
        2, (struct table *table, struct string *name),
        OP_LEAF | OP_NOESCAPE, "ts.l")
{
  TYPEIS(table, type_table);
  TYPEIS(name, type_string);

  return table_prefix(table, name);
}

EXT_TYPEDOP(table_ref, 0,
            "`table `s -> `x. Returns the value of `s in `table, or null",
	    2, (struct table *table, struct string *s),
	    OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "ts.x")
{
  struct symbol *sym;

  TYPEIS(table, type_table);
  TYPEIS(s, type_string);

  if (!table_lookup_len(table, s->str, string_len(s), &sym)) return NULL;
  return sym->data;
}

TYPEDOP(table_lookup, 0, "`table `s -> `x. Returns the symbol for `s in"
        " `table, or false if none",
        2, (struct table *table, struct string *s),
        OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "ts.x")
{
  struct symbol *sym;

  TYPEIS(table, type_table);
  TYPEIS(s, type_string);

  if (!table_lookup_len(table, s->str, string_len(s), &sym)
      || !sym->data)
    return makebool(FALSE);
  return sym;
}

static value table_mset(struct table *table, struct string *s, value x)
{
  struct symbol *sym;

  TYPEIS(s, type_string);

  if (((struct obj *)table)->flags & OBJ_READONLY) 
    runtime_error(error_value_read_only);

  if (table_lookup_len(table, s->str, string_len(s), &sym)) 
    {
      if (sym->o.flags & OBJ_READONLY) runtime_error(error_value_read_only);
      sym->data = x;
    }
  else if (x)
    {
      struct gcpro gcpro1, gcpro2;

      GCPRO2(table, x);
      if (!(s->o.flags & OBJ_READONLY))
	{
          char *scopy;

          LOCALSTR(scopy, s);

	  /* make a copy of index string (otherwise it may get modified...) */
	  s = alloc_string_length(scopy, string_len(s));
	  s->o.flags |= OBJ_READONLY;
	}
      table_add_fast(table, s, x);
      UNGCPRO();
    }
  return x;
}

EXT_TYPEDOP(table_set, "table_set!",
            "`table `s `x -> `x. Sets the value of entry `s in"
	    " `table to `x",
	    3, (struct table *table, struct string *s, value x),
	    OP_LEAF | OP_NOESCAPE, "tsx.3")
{
  TYPEIS(table, type_table);
  return table_mset(table, s, x);
}

TYPEDOP(table_remove, "table_remove!",
        "`table `s -> `b. Removes the entry for `s in `table `x. "
	"Returns true if such an entry was found.",
	2, (struct table *table, struct string *s),
	OP_LEAF | OP_NOESCAPE, "ts.n")
{
  struct gcpro gcpro1;
  int r;

  TYPEIS(table, type_table);
  TYPEIS(s, type_string);
  if (((struct obj *)table)->flags & OBJ_READONLY)
    runtime_error(error_value_read_only);
  GCPRO1(s);
  r = table_remove_len(table, s->str, string_len(s));
  UNGCPRO();
  return makebool(r);
}


void symbol_init(void)
{
  DEFINE(tablep);
  DEFINE(make_table);
  DEFINE(table_ref);
  DEFINE(table_lookup);
  DEFINE(table_set);
  DEFINE(table_remove);

  DEFINE(table_list);
  DEFINE(table_vector);
  DEFINE(table_prefix);
  DEFINE(table_foreach);
  DEFINE(table_reduce);
  DEFINE(table_existsp);

  DEFINE(symbolp);
  DEFINE(symbol_name);
  DEFINE(symbol_get);
  DEFINE(symbol_set);
}
