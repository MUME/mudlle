/*
 * Copyright (c) 1993-2012 David Gay and Gustav H�llberg
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
#include "symbol.h"
#include "../call.h"
#include "../table.h"


TYPEDOP(symbolp, "symbol?", "`x -> `b. True if `x is a symbol.", 1, (value v),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_STR_READONLY, "x.n")
{
  return makebool(TYPE(v, type_symbol));
}

/* not OP_STR_READONLY as it would only apply to the name */
TYPEDOP(make_symbol, 0, "`s `x -> `sym. Creates a symbol with name `s and"
        " value `x.\nIf `s is not read-only, a read-only copy will be used.",
        2, (struct string *s, value v),
        OP_LEAF | OP_NOESCAPE, "sx.y")
{
  TYPEIS(s, type_string);
  if (!obj_readonlyp(&s->o))
    {
      /* technically, symbol names do not have to be readonly, but as
         they always are in tables, we make them so anyway */
      GCPRO1(v);
      s = make_readonly(mudlle_string_copy(s));
      UNGCPRO();
    }
  return alloc_symbol(s, v);
}

/* not OP_STR_READONLY as it would only apply to the name */
TYPEDOP(make_psymbol, 0, "`s `x -> `sym. Creates a read-only symbol with"
        " name `s and value `x.\n"
        "If `s is not read-only, a read-only copy will be used.",
        2, (struct string *s, value v),
        OP_LEAF | OP_NOESCAPE | OP_CONST, "sx.y")
{
  bool imm = immutablep(v);
  struct symbol *sym = code_make_symbol(s, v);
  sym->o.flags |= OBJ_READONLY | (imm ? OBJ_IMMUTABLE : 0);
  return sym;
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

  if (obj_readonlyp(&s->o))
    runtime_error(error_value_read_only);
  s->data = val;
  return val;
}

TYPEDOP(tablep, "table?", "`x -> `b. True if `x is a symbol table, including"
        " case- and accent-sensitive tables. Cf. `ctable?().",
        1, (value v),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(TYPE(v, type_table));
}

TYPEDOP(ctablep, "ctable?", "`x -> `b. True if `x is a case- and accent-"
        " sensitive table. Cf. `table?().",
        1, (struct table *t),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(TYPE(t, type_table) && is_ctable(t));
}

TYPEDOP(make_table, 0, "-> `table. Create a new (empty) case- and accent-"
        "insensitive symbol table. Cf. `make_ctable().",
        0, (void), OP_LEAF | OP_NOESCAPE, ".t")
{
  return alloc_table(DEF_TABLE_SIZE);
}

TYPEDOP(make_ctable, 0, "-> `table. Create a new (empty) case- and accent-"
        "sensitive symbol table. Cf. `make_table().",
        0, (void), OP_LEAF | OP_NOESCAPE, ".t")
{
  return alloc_ctable(DEF_TABLE_SIZE);
}

static struct table *vector_to_table(struct vector *v, bool readonly,
                                     bool ctable)
{
  TYPEIS(v, type_vector);
  long vlen = vector_len(v);
  int immutable = OBJ_IMMUTABLE;
  for (long i = 0; i < vlen; ++i)
    {
      struct symbol *sym = v->data[i];
      TYPEIS(sym, type_symbol);
      immutable &= sym->o.flags;
    }

  ulong tsize = table_good_size(vlen);
  if (tsize > MAX_VECTOR_SIZE)
    runtime_error(error_bad_value);

  GCPRO1(v);
  struct table *table = (ctable ? alloc_ctable : alloc_table)(tsize);
  for (long i = 0; i < vlen; ++i)
    {
      struct symbol *sym = v->data[i];
      if (table_mlookup(table, sym->name) != NULL)
        runtime_error(error_bad_value);
      table_add_sym_fast(table, sym);
    }
  UNGCPRO();

  if (readonly)
    (immutable ? immutable_table : protect_table)(table);

  return table;
}

TYPEDOP(vector_to_table, 0, "`v -> `table. Create a new table from the symbols"
        " in `v. There will be a runtime error if any symbols collide.",
        1, (struct vector *v), OP_LEAF | OP_NOESCAPE, "v.t")
{
  return vector_to_table(v, false, false);
}

TYPEDOP(vector_to_ptable, 0, "`v -> `table. Create a new read-only table from"
        " the symbols in `v.\n"
        "If all symbols are immutable, the table will also be immutable.\n"
        "There will be a runtime error if any symbols collide.",
        1, (struct vector *v), OP_LEAF | OP_NOESCAPE | OP_CONST, "v.t")
{
  return vector_to_table(v, true, false);
}

TYPEDOP(vector_to_ctable, 0, "`v -> `table. Create a new case- and"
        " accentuation-sensitive table from the symbols in `v.\n"
        "There will be a runtime error if any symbols collide.",
        1, (struct vector *v), OP_LEAF | OP_NOESCAPE, "v.t")
{
  return vector_to_table(v, false, true);
}

TYPEDOP(vector_to_pctable, 0, "`v -> `table. Create a new read-only case- and"
        " accentuation-sensitive table from the symbols in `v.\n"
        "If all symbols are immutable, the table will also be immutable.\n"
        "There will be a runtime error if any symbols collide.",
        1, (struct vector *v), OP_LEAF | OP_NOESCAPE | OP_CONST, "v.t")
{
  return vector_to_table(v, true, true);
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

  GCPRO1(table);
  struct copy_table_data data = {
    .buckets = alloc_vector(table_entries(table)),
    .used    = 0
  };
  UNGCPRO();
  table_foreach(table, &data, copy_table_entry);

  *used = data.used;
  return data.buckets;
}

TYPEDOP(table_foreach, 0,
        "`c `table -> . Runs `c(`sym) for each non-null symbol `sym in `table."
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
        " `x = `f(`s, `x) for each non-null symbol `s and initial value `x0.",
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
        "`c `table -> `x. Returns the first non-null symbol `s in `table"
	" for which `c(`s) is true, or false",
	2, (value f, struct table *table),
	0, "ft.[yz]")
{
  struct vector *buckets = NULL;
  value res = makebool(false);
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
        "`table -> `v. Returns a vector of the non-null entries in `table.",
	1, (struct table *table),
	OP_LEAF | OP_NOESCAPE, "t.v")
{
  int used;
  struct vector *buckets = make_table_copy(table, &used);
  if (used == vector_len(buckets))
    return buckets;

  GCPRO1(buckets);
  struct vector *res = alloc_vector(used);
  UNGCPRO();
  memcpy(res->data,
         buckets->data,
         sizeof res->data[0] * used);
  return res;
}

TYPEDOP(table_prefix, 0, "`table `s -> `l. Returns list of all symbols in"
        " `table whose value is non-null and whose name starts with `s.\n"
        "For non-ctables, case and accentuation are ignored.",
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

  struct symbol *sym = table_mlookup(table, s);
  return sym == NULL ? NULL : sym->data;
}

TYPEDOP(table_lookup, 0, "`table `s -> `x. Returns the symbol for `s in"
        " `table, or false if none. Cf. `table_symbol_ref().",
        2, (struct table *table, struct string *s),
        OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_STR_READONLY, "ts.[yz]")
{
  TYPEIS(table, type_table);
  TYPEIS(s, type_string);

  struct symbol *sym = table_mlookup(table, s);
  return sym ? sym : makebool(false);
}

TYPEDOP(table_symbol_ref, 0, "`table `s `x -> `sym. Returns the symbol for `s"
        " in `table, creating it with value `x if necessary."
        " Cf. `table_lookup().",
        3, (struct table *table, struct string *s, value x),
        OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "tsx.y")
{
  TYPEIS(table, type_table);
  TYPEIS(s, type_string);

  struct symbol *sym = table_mlookup(table, s);
  if (sym == NULL)
    {
      if (obj_readonlyp(&table->o))
        runtime_error(error_value_read_only);
      GCPRO2(table, x);
      if (!obj_readonlyp(&s->o))
        s = make_readonly(mudlle_string_copy(s));
      sym = alloc_symbol(s, x);
      UNGCPRO();
      table_add_sym_fast(table, sym);
    }
  return sym;
}

static value mudlle_table_set(value container, struct table *table,
                              struct string *s, value x,
                              const struct primitive_ext *op)
{
  enum runtime_error error;
  if (!TYPE(s, type_string))
    {
      error = error_bad_type;
      goto got_error;
    }
  error = safe_table_mset(table, s, &x);
  if (error == error_none)
    return x;

 got_error:
  primitive_runtime_error(error, op, 3, container, s, x);
}

EXT_TYPEDOP(table_set, "table_set!",
            "`table `s `x -> `x. Sets the value of entry `s in"
	    " `table to `x",
	    3, (struct table *table, struct string *s, value x),
	    OP_LEAF | OP_NOESCAPE, "tsx.3")
{
  TYPEIS(table, type_table);
  return mudlle_table_set(table, table, s, x, &op_table_set);
}

TYPEDOP(table_remove, "table_remove!",
        "`table `s -> `b. Removes the entry for `s in `table.\n"
        "This is a costly operation as it may need to partially rehash"
        " the table.\n"
	"Returns true if such an entry was found, even if it had value null.",
	2, (struct table *table, struct string *s),
	OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "ts.n")
{
  TYPEIS(table, type_table);
  TYPEIS(s, type_string);
  if (obj_readonlyp(&table->o))
    runtime_error(error_value_read_only);
  return makebool(table_remove_len(table, s->str, string_len(s)));
}

TYPEDOP(table_copy, 0,
        "`t0 -> `t1. Make a copy of table `t0, leaving out any null symbols.",
        1, (struct table *table),
        OP_LEAF | OP_NOESCAPE, "t.t")
{
  TYPEIS(table, type_table);
  return table_copy(table);
}

TYPEDOP(table_shallow_copy, 0,
        "`t0 -> `t1. Make a copy of table `t0, reusing its symbols.",
        1, (struct table *table),
        OP_LEAF | OP_NOESCAPE, "t.t")
{
  TYPEIS(table, type_table);
  return table_shallow_copy(table);
}


void symbol_init(void)
{
  DEFINE(tablep);
  DEFINE(ctablep);
  DEFINE(make_table);
  DEFINE(make_ctable);
  DEFINE(table_ref);
  DEFINE(table_lookup);
  DEFINE(table_symbol_ref);
  DEFINE(table_set);
  DEFINE(table_remove);
  DEFINE(table_copy);
  DEFINE(table_shallow_copy);

  DEFINE(table_list);
  DEFINE(table_vector);
  DEFINE(table_prefix);
  DEFINE(table_foreach);
  DEFINE(table_reduce);
  DEFINE(table_existsp);

  DEFINE(vector_to_table);
  DEFINE(vector_to_ptable);
  DEFINE(vector_to_ctable);
  DEFINE(vector_to_pctable);

  DEFINE(make_symbol);
  DEFINE(make_psymbol);
  DEFINE(symbolp);
  DEFINE(symbol_name);
  DEFINE(symbol_get);
  DEFINE(symbol_set);


  system_define("MAX_TABLE_ENTRIES", makeint(MAX_TABLE_ENTRIES));
}
