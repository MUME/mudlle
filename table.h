/* $Log: table.h,v $
 * Revision 1.6  1994/10/09  06:43:03  arda
 * Libraries
 * Type inference
 * Many minor improvements
 *
 * Revision 1.5  1994/09/09  19:36:18  arda
 * TAble prefixes.
 *
 * Revision 1.4  1993/08/15  21:00:29  un_mec
 * Owl: Overload [].
 *      Added xcalloc, xrealloc.
 *
 * Revision 1.3  1993/04/22  18:58:55  un_autre
 * (MD) & Owl. Bug fixes. /player fixes. EVER_WHINER flag. saving_spells adjusted.
 *
 * Revision 1.2  1993/03/29  09:24:31  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.3  1993/03/14  16:15:01  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.1  1992/12/27  21:41:38  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

#ifndef TABLE_H
#define TABLE_H

#include "types.h"

struct table *alloc_table(ulong size);
/* Returns: A new symbol table, initially of size size.
   Requires: size be a power of 2, smaller or equal than 2^30.
*/

int table_lookup(struct table *table, const char *name, struct symbol **sym);
/* Effects: Looks for name in the symbol table table (case insensitive).
   Returns: TRUE if name is found. *pos is set to name's data.
     Otherwise, returns FALSE. table_add_fast can be called immediately
     if you wish to add an entry to name to the symbol table (but no intervening
     call to the module should be made).
*/

int table_set(struct table *table, const char *name, value data);
/* Effects: Sets table[name] to data, adds it if not already present
   Modifies: table
   Returns: FALSE if entry name was readonly
*/

struct symbol *table_add(struct table *table, struct string *name, value data);
/* Effects: Adds <name,data> to the symbol table.
   Returns: The symbol if it could be added, NULL if it was already in the
     symbol table.
   Modifies: table
*/

struct symbol *table_add_fast(struct table *table, struct string *name, value data);
/* Requires: table_lookup(table, name->str, ...) to have just failed.
   Effects: Adds <name,data> to the symbol table.
   Modifies: table
   Returns: The new symbol
*/

struct list *table_list(struct table *table);
/* Returns: A list which contains the symbols in symbol table table
     (elements whose value is null are omitted).
     The order is arbitrary.
*/

struct list *table_prefix(struct table *table, struct string *prefix);
/* Returns: A list of all the symbols in table whose name starts with
     prefix (case insensitive, like all table ops)
*/

#define DEF_TABLE_SIZE 32	/* Convenient initial size */

#endif
