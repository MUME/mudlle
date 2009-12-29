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

#ifndef TABLE_H
#define TABLE_H

#include "types.h"

struct table *alloc_table(ulong size);
/* Returns: A new symbol table, initially of size size.
   Requires: size be a power of 2, smaller or equal than 2^30.
*/

int table_lookup(struct table *table, const char *name, struct symbol **sym);
int table_lookup_len(struct table *table, const char *name, size_t nlength,
                     struct symbol **sym);
/* Effects: Looks for name in the symbol table table (case insensitive).
   Returns: true if name is found. *pos is set to name's data.
     Otherwise, returns false. table_add_fast can be called immediately
     if you wish to add an entry to name to the symbol table (but no intervening
     call to the module should be made).
*/

int table_remove(struct table *table, const char *name);
int table_remove_len(struct table *table, const char *name, size_t nlength);
/* Effects: Removes table[name] from data. Rehashes nescessary values.
   Modifies: table
   Returns: false if the entry wasn't found
*/

int table_set(struct table *table, const char *name, value data);
int table_set_len(struct table *table, const char *name, size_t nlength,
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

void protect_table(struct table *table);
void immutable_table(struct table *table);

struct symbol *table_exists(struct table *table, int (*check)(struct symbol *));
void table_foreach(struct table *table, void (*action)(struct symbol *));
int table_entries(struct table *table);

#define DEF_TABLE_SIZE 8	/* Convenient initial size */

#endif /* TABLE_H */
