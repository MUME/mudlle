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

#ifndef TABLE_H
#define TABLE_H

#include <string.h>

#include "mvalues.h"

struct table *alloc_table(ulong size);
/* Returns: A new symbol table, initially of size size.
   Requires: size be a power of 2, smaller or equal than 2^30.
*/

struct table *alloc_ctable(ulong size);
/* Returns: A new case/accent-sensitive symbol table, initially of size size.
   Requires: size be a power of 2, smaller or equal than 2^30.
*/

static inline bool is_ctable(struct table *t)
{
  return (long)t->used < 0;
}

struct symbol *table_lookup_len(struct table *table, const char *name,
                                size_t len);
struct symbol *table_mlookup(struct table *table, struct string *name);
static inline struct symbol *table_lookup(struct table *table,
                                          const char *name)
{
  return table_lookup_len(table, name, strlen(name));
}

/* Effects: Looks for name in the symbol table table (case insensitive).
   Returns: true if name is found. *pos is set to name's data.
     Otherwise, returns false. table_add_fast can be called immediately if you
     wish to add an entry to name to the symbol table (but no intervening call
     to the module should be made). */

struct symbol *table_remove(struct table *table, const char *name);
struct symbol *table_remove_len(struct table *table, const char *name,
				size_t nlength);
/* Effects: Removes table[name] from data. Rehashes nescessary values.
   Modifies: table
   Returns: The removed symbol, or NULL if not found.
*/

enum runtime_error safe_table_mset(struct table *table, struct string *s,
                                   value *x);
void table_mset(struct table *table, struct string *name, value data);
bool table_set(struct table *table, const char *name, value data);
bool table_set_len(struct table *table, const char *name, size_t nlength,
                   value data);
/* Effects: Sets table[name] to data, adds it if not already present
   Modifies: table
   Returns: false if entry name was readonly
*/

struct symbol *table_add(struct table *table, struct string *name, value data);
/* Effects: Adds <name,data> to the symbol table.
   Returns: The symbol if it could be added, NULL if it was already in the
     symbol table.
   Modifies: table
*/

struct symbol *table_add_sym_fast(struct table *table, struct symbol *sym);
struct symbol *table_add_fast(struct table *table, struct string *name,
                              value data);
/* Requires: table_lookup(table, (sym->)name->str, ...) to have just failed.
   Effects: Adds sym/<name,data> to the symbol table.
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

void protect_table(struct table *table);
void immutable_table(struct table *table);

static inline ulong table_entries(struct table *table)
{
  long n = intval(table->used);
  return n < 0 ? ~n : n;
}

struct symbol *table_exists(struct table *table,
                            bool (*check)(struct symbol *sym, void *data),
                            void *data);
void table_foreach(struct table *table, void *data,
                   void (*action)(struct symbol *, void *));

struct table *table_copy(struct table *table);
struct table *table_shallow_copy(struct table *table);

bool table_is_empty(struct table *table);
struct table *table_resize(struct table *table);

void rehash_table(struct table *table);

ulong symbol_hash_len(const char *name, size_t len, ulong size);
ulong case_symbol_hash_len(const char *name, size_t len, ulong size);

#define DEF_TABLE_SIZE 8	/* Convenient initial size */

static inline int table_good_size(int entries)
{
  entries = (entries * 4 + 2) / 3;
  /* find next higher power of two */
  entries |= entries >> 1;
  entries |= entries >> 2;
  entries |= entries >> 4;
  entries |= entries >> 8;
  entries |= entries >> 16;
  return ++entries;
}

#endif /* TABLE_H */
