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
#include <ctype.h>
#include "table.h"
#include "alloc.h"
#include "charset.h"

/* The hash table size must be a power of 2 */

struct table			/* Is a record */
{
  struct obj o;
  value size;			/* Of hash table */
  value used;
  struct vector *buckets;
};

static ulong hash_len(const char *_name, size_t len)
/* Randomly chosen hash function. Probably not very good. */
{
  unsigned const char *name = (unsigned const char *)_name;
  ulong code = 0;

  while (len--)
    {
      code = ((code << 1) + TO_7LOWER(*name)) ^ 0x57954317;
      name++;
    }

  return code;
}

struct table *alloc_table(ulong size)
/* Returns: A new symbol table, initially of size size.
   Requires: size be a power of 2, smaller or equal than 2^30.
*/
{
  struct table *newp;
  value vec;
  struct gcpro gcpro1;
  value isize = makeint(size);

  newp = (struct table *)allocate_record(type_table, 3);
  GCPRO1(newp);
  newp->size = isize;
  newp->used = makeint(0);
  vec = alloc_vector(size);
  newp->buckets = vec;
  UNGCPRO();

  return newp;
}

static ulong add_position;

int table_lookup_len(struct table *table, const char *name, size_t nlength,
                     struct symbol **sym)
/* Effects: Looks for name in the symbol table table.
   Returns: TRUE if name is found. *pos is set to name's data.
     Otherwise, returns FALSE. table_add_fast can be called immediately
     if you wish to add an entry to name to the symbol table (but no intervening
     call to the module should be made).
*/
{
  ulong size = intval(table->size);
  ulong hashcode = hash_len(name, nlength) & (size - 1), scan;
  struct symbol **bucket;

  /* Search for name in table */
  scan = hashcode;
  bucket = (struct symbol **)&table->buckets->data[scan];
  do {
    if (!*bucket) 
      {
	add_position = scan;
	return FALSE;
      }
    if (string_len((*bucket)->name) == nlength
        && mem8icmp(name, (*bucket)->name->str, nlength) == 0)
      {
	*sym = *bucket;
	return TRUE;
      }
    scan++;
    bucket++;
    if (scan == size)
      {
	scan = 0;
	bucket = (struct symbol **)&table->buckets->data[scan];
      }
    assert(scan != hashcode);	/* The table is never allowed to be full */
  } while (1);
}

int table_lookup(struct table *table, const char *name, struct symbol **sym)
/* Effects: Looks for name in the symbol table table.
   Returns: TRUE if name is found. *pos is set to name's data.
     Otherwise, returns FALSE. table_add_fast can be called immediately
     if you wish to add an entry to name to the symbol table (but no intervening
     call to the module should be made).
*/
{
  return table_lookup_len(table, name, strlen(name), sym);
}

int table_remove(struct table *table, const char *name)
{
  return table_remove_len(table, name, strlen(name));
}

int table_remove_len(struct table *table, const char *name, size_t nlength)
/* Effects: Removes table[name] from data. Rehashes nescessary values.
   Modifies: table
   Returns: FALSE if the entry wasn't found
*/
{
  struct symbol **bucket;
  ulong size = intval(table->size), scan;

  scan = hash_len(name, nlength) & (size - 1);
  bucket = (struct symbol **)&table->buckets->data[scan];

  do {
    if (!*bucket) 
      return FALSE;
    if (string_len((*bucket)->name) == nlength
        && mem8icmp(name, (*bucket)->name->str, nlength) == 0)
      {
	*bucket = 0;
	++bucket;
	++scan;
	if (scan == size)
	  {
	    scan = 0;
	    bucket = (struct symbol **)&table->buckets->data[scan];
	  }
	while (*bucket)
	  {
	    struct symbol *sym = *bucket, **newbuck;
	    ulong newpos = (hash_len(sym->name->str, string_len(sym->name)) 
                            & (size - 1));
	    
	    *bucket = 0;
	    newbuck = (struct symbol **)&table->buckets->data[newpos];
	    while (*newbuck) 
	      {
		newbuck++;
		newpos++;
		if (newpos == size) 
		  {
		    newpos = 0;
		    newbuck = (struct symbol **)&table->buckets->data[newpos];
		  }
	      } 
	    *newbuck = sym;
	    bucket++;
	    scan++;
	    if (scan == size)
	      {
		scan = 0;
		bucket = (struct symbol **)&table->buckets->data[scan];
	      }
	  }
	table->used = (value)((long)table->used - 2);
	return TRUE;
      }
    scan++;
    bucket++;
    if (scan == size)
      {
	scan = 0;
	bucket = (struct symbol **)&table->buckets->data[scan];
      }
  } while (1);
}

int table_set(struct table *table, const char *name, value data)
{
  return table_set_len(table, name, strlen(name), data);
}

int table_set_len(struct table *table, const char *name, size_t nlength,
                  value data)
/* Effects: Sets table[name] to data, adds it if not already present
   Modifies: table
   Returns: FALSE if entry name was readonly
*/
{
  struct symbol *sym;

  if (table_lookup_len(table, name, nlength, &sym)) 
    {
      if (sym->o.flags & OBJ_READONLY) return FALSE;
      sym->data = data;
    }
  else if (data)
    {
      struct gcpro gcpro1, gcpro2;
      struct string *s;

      GCPRO2(table, data);
      s = alloc_string_length(name, nlength);
      s->o.flags |= OBJ_READONLY;
      UNGCPRO();
      table_add_fast(table, s, data);
    }
  return TRUE;
}

struct symbol *table_add(struct table *table, struct string *name, value data)
/* Effects: Adds <name,data> to the symbol table.
   Returns: The symbol if it could be added, NULL if it was already in the
     symbol table.
   Modifies: table
*/
{
  struct symbol *dummy;

  if (table_lookup(table, name->str, &dummy)) return NULL;
  return table_add_fast(table, name, data);
}

struct symbol *table_add_fast(struct table *table, struct string *name,
                              value data)
/* Requires: table_lookup(table, name->str, ...) to have just failed.
   Effects: Adds <name,data> to the symbol table.
   Modifies: table
   Returns: The new symbol
*/
{
  ulong size = intval(table->size), newsize, i, max;
  struct vector *newp, *old;
  struct symbol **oldbucket;
  struct gcpro gcpro1, gcpro2;
  struct symbol *sym;

  assert(~table->o.flags & OBJ_READONLY);
  assert(~table->buckets->o.flags & OBJ_READONLY);

  GCCHECK(name); GCCHECK(data);
  assert(add_position < intval(table->size)
         && !table->buckets->data[add_position]);
  GCPRO1(table);
  sym = alloc_symbol(name, data);
  table->buckets->data[add_position] = sym;
  table->used = (value)((long)table->used + 2);

  /* If table is 3/4 full, increase its size */
  max = size / 2 + size / 4;
  if (intval(table->used) < max)
    {
      UNGCPRO();
      return sym;
    }

  /* Double table size */
  newsize = 2 * size;
  table->size = makeint(newsize);

  GCPRO(gcpro2, sym);
  newp = alloc_vector(newsize);
  old = table->buckets;
  table->buckets = newp;
  UNGCPRO();

  /* Copy data from old buckets into new ones */
  for (oldbucket = (struct symbol **)old->data, i = 0; i < size; oldbucket++, i++)
    if (*oldbucket)
      {
	ulong hashcode = (hash_len((*oldbucket)->name->str,
                                   string_len((*oldbucket)->name)) 
                          & (newsize - 1));
        ulong scan;
	value *bucket;

	scan = hashcode;
	bucket = &newp->data[scan];
	do {
	  if (!*bucket) 
	    {
	      *bucket = *oldbucket;
	      break;
	    }
	  scan++;
	  bucket++;
	  if (scan == newsize)
	    {
	      scan = 0;
	      bucket = &newp->data[scan];
	    }
	  assert(scan != hashcode); /* The table is never allowed to be full */
	} while (1);
      }
  return sym;
}

struct list *table_list(struct table *table)
/* Returns: A list which contains the symbols in symbol table table
     (elements whose value is null are omitted).
     The order is arbitrary.
*/
{
  struct gcpro gcpro1, gcpro2;
  struct list *l = NULL;
  struct symbol *sym;
  ulong size = intval(table->size);

  GCPRO2(l, table);
  while (size > 0)
    {
      size--;
      sym = table->buckets->data[size];
      if (sym && sym->data) l = alloc_list(sym, l);
    }
  UNGCPRO();

  return l;
}

static int prefixp(struct string *s1, struct string *s2)
/* Returns: TRUE if s1 is a prefix of s2
*/
{
  ulong l1 = string_len(s1), l2 = string_len(s2);
  const char *t1 = s1->str, *t2 = s2->str;

  if (l1 > l2) return FALSE;
  
  return mem8icmp(t1, t2, l1) == 0;
}

struct list *table_prefix(struct table *table, struct string *prefix)
/* Returns: A list of all the symbols in table whose name starts with
     prefix (case insensitive, like all table ops)
*/
{
  struct gcpro gcpro1, gcpro2, gcpro3;
  struct list *l = NULL;
  struct symbol *sym;
  ulong size = intval(table->size);

  GCPRO2(l, table); GCPRO(gcpro3, prefix);
  while (size > 0)
    {
      size--;
      sym = table->buckets->data[size];
      if (sym && sym->data && prefixp(prefix, sym->name)) l = alloc_list(sym, l);
    }
  UNGCPRO();

  return l;
}

struct symbol *table_exists(struct table *table, int (*check)(struct symbol *))
{
  struct gcpro gcpro1;
  struct symbol **bucket;
  int i, size;

  GCPRO1(table);
  size = intval(table->size);
  bucket = (struct symbol **)&table->buckets->data[0];
  for (i = 0; i < size; ++i, ++bucket)
    if (*bucket && check(*bucket))
      {
	UNGCPRO();
	return *bucket;
      }
  UNGCPRO();

  return 0;
}

void table_foreach(struct table *table, void (*action)(struct symbol *))
{
  struct gcpro gcpro1;
  int i, size;

  GCPRO1(table);
  size = intval(table->size);
  for (i = 0; i < size; ++i)
    {
      struct symbol *bucket = (struct symbol *)table->buckets->data[i];
      if (bucket)
	action(bucket);
    }
  UNGCPRO();
}

int table_entries(struct table *table)
{
  return intval(table->used);
}

void protect_table(struct table *table)
{
  table->o.flags |= OBJ_READONLY;
  table->buckets->o.flags |= OBJ_READONLY;
}

void immutable_table(struct table *table)
{
  table->o.flags |= OBJ_READONLY | OBJ_IMMUTABLE;
  table->buckets->o.flags |= OBJ_READONLY | OBJ_IMMUTABLE;
}
