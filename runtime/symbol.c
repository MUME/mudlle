/* $Log: symbol.c,v $
 * Revision 1.13  1994/10/09  06:44:23  arda
 * Libraries
 * Type inference
 * Many minor improvements
 *
 * Revision 1.12  1994/09/15  19:48:10  arda
 * Performance improvements:
 *   improve reaction list rep
 *   make timed action lists C arrays
 * Check for readonly symbols
 *
 * Revision 1.11  1994/09/09  19:36:37  arda
 * Table prefixes.
 *
 * Revision 1.10  1994/09/06  13:54:03  arda
 * *** empty log message ***
 *
 * Revision 1.9  1994/08/16  19:17:21  arda
 * Added flags to primitives for better calling sequences.
 *
 * Revision 1.7  1993/11/27  11:29:25  arda
 * Owl: Major changes to affect.
 *      Save mudlle data with players & objects.
 *      Change skill format on disk.
 *      Other minor changes.
 *      Still needs full debugging.
 *
 * Revision 1.6  1993/10/03  14:07:28  dgay
 * Bumper disun8 update.
 *
 * Revision 1.5  1993/08/15  21:02:13  un_mec
 * Owl: Several extras functions.
 *      rent.
 *
 * Revision 1.4  1993/04/22  18:59:30  un_autre
 * (MD) & Owl. Bug fixes. /player fixes. EVER_WHINER flag. saving_spells adjusted.
 *
 * Revision 1.3  1993/03/29  09:25:56  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.3  1993/03/14  16:16:54  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.1  1992/12/27  21:42:25  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

static char rcsid[] = "$Id: symbol.c,v 1.13 1994/10/09 06:44:23 arda Exp $";

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

static value table_mset(struct table *table, struct string *s, value x)
{
  struct symbol *sym;

  TYPEIS(s, type_string);

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
	  struct gcpro gcpro1;

	  /* Make a copy of index string (otherwise it may get modified...) */
	  GCPRO1(s);
	  news = (struct string *)allocate_string(type_string, strlen(s->str) + 1);
	  strcpy(news->str, s->str);
	  UNGCPRO();
	  
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

#ifdef MUME
/* Support for mume instance symbol tables */

struct table *mume_table(struct dynpro *table)
{
  if (!table->obj) 
    {
      dynpro(table, alloc_table(DEF_TABLE_SIZE));
    }
  else if (!TYPE(table->obj, type_table)) /* Somehow, some tables got corrupted */
    {
      mlog("BUG: Symbol table corrupted.");
      table->obj = alloc_table(DEF_TABLE_SIZE);
    }
  return table->obj;
}

value mume_ref(struct dynpro *tab, struct string *s)
{
  struct gcpro gcpro1;
  struct table *table;
  struct symbol *sym;

  TYPEIS(s, type_string);
  if (!tab->obj) return NULL;
  GCPRO1(s);
  table = mume_table(tab);
  UNGCPRO();
  if (!table_lookup(table, s->str, &sym)) return NULL;
  return sym->data;
}

value mume_set(struct dynpro *tab, struct string *s, value x)
{
  struct gcpro gcpro1, gcpro2;
  struct table *table;

  GCPRO2(s, x);
  table = mume_table(tab);
  UNGCPRO();
  return table_mset(table, s, x);
}
#endif

void symbol_init(void)
{
  DEFINE("table?", tablep);
  DEFINE("make_table", make_table);
  DEFINE("table_ref", table_ref);
  DEFINE("table_set!", table_set);

  DEFINE("table_list", table_list);
  DEFINE("table_prefix", table_prefix);
  DEFINE("symbol?", symbolp);
  DEFINE("symbol_name", symbol_name);
  DEFINE("symbol_get", symbol_get);
  DEFINE("symbol_set!", symbol_set);
}

