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

#include <string.h>

#include "runtime.h"
#include "../table.h"
#include "../call.h"


TYPEDOP(symbolp, "symbol?", "`x -> `b. TRUE if `x is a symbol", 1, (value v),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_STR_READONLY, "x.n")
{
  return makebool(TYPE(v, type_symbol));
}

TYPEDOP(make_symbol, 0, "`s `x -> `sym. Creates a symbol with name `s and value `x. If `s is not read-only, a read-only copy will be used",
        2, (struct string *s, value v), OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "sx.y")
{
  TYPEIS(s, type_string);
  if (~s->o.flags & OBJ_READONLY)
    {
      /* technically, symbol names do not have to be readonly, but as
         they always are in tables, we make them so anyway */
      char *scopy;
      LOCALSTR(scopy, s);
      GCPRO1(v);
      s = make_readonly(alloc_string_length(scopy, string_len(s)));
      UNGCPRO();
    }
  return alloc_symbol(s, v);
}

TYPEDOP(symbol_name, 0, "`sym -> `s. Returns the name of a symbol",
	1, (struct symbol *v),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "y.s")
{
  TYPEIS(v, type_symbol);
  return v->name;
}

TYPEDOP(symbol_get, 0, "`sym -> `x. Returns the value of a symbol",
	1, (struct symbol *v),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "y.x")
{
  TYPEIS(v, type_symbol);
  return v->data;
}

EXT_TYPEDOP(symbol_set, "symbol_set!",
            "`sym `x -> `x. Sets the value of symbol `sym to `x",
            2, (struct symbol *s, value val),
            OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "yx.1")
{
  TYPEIS(s, type_symbol);

  if (s->o.flags & OBJ_READONLY) runtime_error(error_value_read_only);
  s->data = val;
  return val;
}

TYPEDOP(tablep, "table?", "`x -> `b. TRUE if `x is a symbol table",
        1, (value v),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(TYPE(v, type_table));
}

TYPEDOP(make_table, 0, "-> `table. Create a new (empty) symbol table",
        0, (void), OP_LEAF | OP_NOESCAPE, ".t")
{
  return alloc_table(DEF_TABLE_SIZE);
}

TYPEDOP(table_list, 0,
	"`table -> `l. Returns list of symbols in `table whose value non-null",
	1, (struct table *table),
	OP_LEAF | OP_NOESCAPE, "t.l")
{
  TYPEIS(table, type_table);

  return table_list(table);
}

struct copy_table_data {
  struct vector *buckets;
  int used;
};

static void copy_table_entry(struct symbol *sym, void *_data)
{
  if (sym->data == NULL)
    return;
  struct copy_table_data *data = _data;
  data->buckets->data[data->used++] = sym;
}

static struct vector *make_table_copy(struct table *table, int *used)
{
  TYPEIS(table, type_table);

  struct copy_table_data data;
  {
    GCPRO1(table);
    data = (struct copy_table_data){
      .buckets = alloc_vector(table_entries(table)),
      .used    = 0
    };
    UNGCPRO();
  }

  {
    GCPRO1(data.buckets);
    table_foreach(table, &data, copy_table_entry);
    UNGCPRO();
  }

  *used = data.used;
  return data.buckets;
}

TYPEDOP(table_foreach, 0,
        "`c `table -> . Runs `c(`sym) for each symbol `sym in `table."
        " It is safe to modify `table from `c().",
	2, (value f, struct table *table),
	0, "ft.")
{
  struct vector *buckets = NULL;
  int used;

  callable(f, 1);
  GCPRO2(f, buckets);

  buckets = make_table_copy(table, &used);

  for (int i = 0; i < used; ++i)
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
	0, "ft.[yn]")
{
  struct vector *buckets = NULL;
  value res = makebool(0);
  int used;

  callable(f, 1);
  GCPRO2(f, buckets);

  buckets = make_table_copy(table, &used);

  for (int i = 0; i < used; ++i)
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
        OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "ts.l")
{
  TYPEIS(table, type_table);
  TYPEIS(name, type_string);

  return table_prefix(table, name);
}

EXT_TYPEDOP(table_ref, 0,
            "`table `s -> `x. Returns the value of `s in `table, or null",
	    2, (struct table *table, struct string *s),
	    OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_STR_READONLY, "ts.x")
{
  if (!TYPE(table, type_table)
      || !TYPE(s, type_string))
    primitive_runtime_error(error_bad_type, &op_table_ref, 2, table, s);

  struct symbol *sym;
  if (!table_lookup_len(table, s->str, string_len(s), &sym)) return NULL;
  return sym->data;
}

TYPEDOP(table_lookup, 0, "`table `s -> `x. Returns the symbol for `s in"
        " `table, or false if none",
        2, (struct table *table, struct string *s),
        OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_STR_READONLY, "ts.[yn]")
{
  struct symbol *sym;

  TYPEIS(table, type_table);
  TYPEIS(s, type_string);

  if (!table_lookup_len(table, s->str, string_len(s), &sym)
      || !sym->data)
    return makebool(false);
  return sym;
}

runtime_errors safe_table_mset(struct table *table, struct string *s, value *x)
{
  if (!TYPE(s, type_string))
    return error_bad_type;

  if (((struct obj *)table)->flags & OBJ_READONLY)
    return error_value_read_only;

  struct symbol *sym;
  if (table_lookup_len(table, s->str, string_len(s), &sym))
    {
      if (sym->o.flags & OBJ_READONLY)
        return error_value_read_only;
      sym->data = *x;
    }
  else if (*x)
    {
      value x2 = *x;
      GCPRO2(table, x2);
      if (!(s->o.flags & OBJ_READONLY))
	{
	  /* make a copy of index string (otherwise it may get modified...) */
	  s = make_readonly(mudlle_string_copy(s));
	}
      table_add_fast(table, s, x2);
      UNGCPRO();
      *x = x2;
    }
  return error_none;
}

static value table_mset(value container, struct table *table,
                        struct string *s, value x,
                        const struct primitive_ext *op)
{
  GCPRO1(container);
  runtime_errors error = safe_table_mset(table, s, &x);
  UNGCPRO();
  if (error != error_none)
    primitive_runtime_error(error, op, 3, container, s, x);
  return x;
}

EXT_TYPEDOP(table_set, "table_set!",
            "`table `s `x -> `x. Sets the value of entry `s in"
	    " `table to `x",
	    3, (struct table *table, struct string *s, value x),
	    OP_LEAF | OP_NOESCAPE, "tsx.3")
{
  TYPEIS(table, type_table);
  return table_mset(table, table, s, x, &op_table_set);
}

TYPEDOP(table_remove, "table_remove!",
        "`table `s -> `b. Removes the entry for `s in `table `x. "
	"Returns true if such an entry was found.",
	2, (struct table *table, struct string *s),
	OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "ts.n")
{
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

  DEFINE(make_symbol);
  DEFINE(symbolp);
  DEFINE(symbol_name);
  DEFINE(symbol_get);
  DEFINE(symbol_set);
}
