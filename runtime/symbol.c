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

#include <string.h>
#include "runtime/runtime.h"
#include "table.h"

TYPEDOP(symbolp, "x -> b. TRUE if x is a symbol", 1, (value v),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(TYPE(v, type_symbol));
}

TYPEDOP(symbol_name, "sym -> s. Returns the name of a symbol",
	1, (struct symbol *v),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "y.s")
{
  TYPEIS(v, type_symbol);
  return (v->name);
}

TYPEDOP(symbol_get, "sym -> x. Returns the value of a symbol",
	1, (struct symbol *v),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "y.x")
{
  TYPEIS(v, type_symbol);
  return (v->data);
}

TYPEDOP(symbol_set, "sym x -> . Sets the value of symbol sym to x",
	2, (struct symbol *s, value val),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "yx.")
{
  TYPEIS(s, type_symbol);

  if (s->o.flags & OBJ_READONLY) runtime_error(error_value_read_only);
  s->data = val;
  undefined();
}

TYPEDOP(tablep, "x -> b. TRUE if x is a symbol table", 1, (value v),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(TYPE(v, type_table));
}

TYPEDOP(make_table, "-> table. Create a new (empty) symbol table", 0, (void),
	OP_LEAF | OP_NOESCAPE, "n.t")
{
  return (alloc_table(DEF_TABLE_SIZE));
}

TYPEDOP(table_list,
	"table -> l. Returns list of symbols in table whose value isn't null",
	1, (struct table *table),
	OP_LEAF | OP_NOESCAPE, "n.l")
{
  TYPEIS(table, type_table);

  return table_list(table);
}

TYPEDOP(table_prefix, "table s -> l. Returns list of symbols in table whose value isn't null, and whose name starts with s",
	  2, (struct table *table, struct string *name),
	  OP_LEAF | OP_NOESCAPE, "ts.l")
{
  TYPEIS(table, type_table);
  TYPEIS(name, type_string);

  return table_prefix(table, name);
}

TYPEDOP(table_ref, "table s -> x. Returns the value of s in symbol table",
	  2, (struct table *table, struct string *s),
	  OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "ts.x")
{
  struct symbol *sym;

  TYPEIS(table, type_table);
  TYPEIS(s, type_string);

  if (!table_lookup(table, s->str, &sym)) return NULL;
  return sym->data;
}

TYPEDOP(table_lookup, "table s -> x. Returns the symbol for s in symbol table, or false if none",
	  2, (struct table *table, struct string *s),
	  OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "ts.x")
{
  struct symbol *sym;

  TYPEIS(table, type_table);
  TYPEIS(s, type_string);

  if (!table_lookup(table, s->str, &sym) || !sym->data) return makebool(FALSE);
  return sym;
}

static value table_mset(struct table *table, struct string *s, value x)
{
  struct symbol *sym;

  TYPEIS(s, type_string);

  if (((struct obj *)table)->flags & OBJ_READONLY) 
    runtime_error(error_value_read_only);

  if (table_lookup(table, s->str, &sym)) 
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
	  struct string *news;
	  struct gcpro gcpro3;

	  /* Make a copy of index string (otherwise it may get modified...) */
	  GCPRO(gcpro3, s);
	  news = (struct string *)allocate_string(type_string, string_len(s) + 1);
	  strcpy(news->str, s->str);
	  UNGCPRO1(gcpro3);
	  
	  s = news;
	  s->o.flags |= OBJ_READONLY;
	}
      table_add_fast(table, s, x);
      UNGCPRO();
    }
  return x;
}

TYPEDOP(table_set, "table s x -> x. Sets the value of entry s in symbol table to x",
	3, (struct table *table, struct string *s, value x),
	OP_LEAF | OP_NOESCAPE, "tsx.")
{
  TYPEIS(table, type_table);
  return table_mset(table, s, x);
}

TYPEDOP(table_remove, "table s -> b. Removes the entry for s in the table x. "
	"Returns true if such an entry was found.",
	2, (struct table *table, struct string *s),
	OP_LEAF | OP_NOESCAPE, "tsx.")
{
  struct gcpro gcpro1;
  int r;

  TYPEIS(table, type_table);
  TYPEIS(s, type_string);
  if (((struct obj *)table)->flags & OBJ_READONLY)
    runtime_error(error_value_read_only);
  GCPRO1(s);
  r = table_remove(table, s->str);
  UNGCPRO();
  return makebool(r);
}

void symbol_init(void)
{
  DEFINE("table?", tablep);
  DEFINE("make_table", make_table);
  DEFINE("table_ref", table_ref);
  DEFINE("table_lookup", table_lookup);
  DEFINE("table_set!", table_set);
  DEFINE("table_remove!", table_remove);

  DEFINE("table_list", table_list);
  DEFINE("table_prefix", table_prefix);
  DEFINE("symbol?", symbolp);
  DEFINE("symbol_name", symbol_name);
  DEFINE("symbol_get", symbol_get);
  DEFINE("symbol_set!", symbol_set);
}

