/* $Log: table.c,v $
 * Revision 1.10  1994/10/09  06:43:02  arda
 * Libraries
 * Type inference
 * Many minor improvements
 *
 * Revision 1.9  1994/09/09  19:36:15  arda
 * TAble prefixes.
 *
 * Revision 1.8  1994/08/16  19:16:22  arda
 * Mudlle compiler for sparc now fully functional (68k compiler now needs
 * updating for primitives).
 * Changes to allow Sparc trap's for runtime errors.
 * Also added flags to primitives for better calling sequences.
 *
 * Revision 1.6  1993/12/26  12:12:20  arda
 * Fixed 1 year old bug.
 *
 * Revision 1.5  1993/05/20  16:24:42  un_mec
 * divers
 *
 *
 * nouvelle version avec 107 niveaux
 *
 * Revision 1.4  1993/04/22  18:58:54  un_autre
 * (MD) & Owl. Bug fixes. /player fixes. EVER_WHINER flag. saving_spells adjusted.
 *
 * Revision 1.3  1993/03/29  09:24:29  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.3  1993/03/14  16:14:58  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.2  1993/01/18  22:22:56  un_mec
 * Owl: Yucky GC bugs. Why didn't they happen earlier ??
 *
 * Revision 1.1  1992/12/27  21:41:36  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

static char rcsid[] = "$Id: table.c,v 1.10 1994/10/09 06:43:02 arda Exp $";

#include <string.h>
#include <ctype.h>
#include "mudlle.h"
#include "types.h"
#include "table.h"
#include "alloc.h"

/* The hash table size must be a power of 2 */

struct table			/* Is a record */
{
  struct obj o;
  value size;			/* Of hash table */
  value used;
  struct vector *buckets;
};

ulong hash(const char *_name)
/* Randomly chosen hash function. Probably not very good. */
{
  unsigned const char *name = (unsigned const char *)_name;
  ulong code = 0;

  while (*name)
    {
      code = ((code << 1) + tolower(*name)) ^ 0x57954317;
      name++;
    }

  return code;
}

struct table *alloc_table(ulong size)
/* Returns: A new symbol table, initially of size size.
   Requires: size be a power of 2, smaller or equal than 2^30.
*/
{
  struct table *new;
  value vec;
  struct gcpro gcpro1;
  value isize = makeint(size);

  new = (struct table *)allocate_record(type_table, 3);
  GCPRO1(new);
  new->size = isize;
  new->used = makeint(0);
  vec = alloc_vector(size);
  new->buckets = vec;
  UNGCPRO();

  return new;
}

static ulong add_position;

int table_lookup(struct table *table, const char *name, struct symbol **sym)
/* Effects: Looks for name in the symbol table table.
   Returns: TRUE if name is found. *pos is set to name's data.
     Otherwise, returns FALSE. table_add_fast can be called immediately
     if you wish to add an entry to name to the symbol table (but no intervening
     call to the module should be made).
*/
{
  ulong size = intval(table->size);
  ulong hashcode = hash(name) & (size - 1), scan;
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
    if (stricmp(name, (*bucket)->name->str) == 0)
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

int table_set(struct table *table, const char *name, value data)
/* Effects: Sets table[name] to data, adds it if not already present
   Modifies: table
   Returns: FALSE if entry name was readonly
*/
{
  struct symbol *sym;

  if (table_lookup(table, name, &sym)) 
    {
      if (sym->o.flags & OBJ_READONLY) return FALSE;
      sym->data = data;
    }
  else if (data)
    {
      struct gcpro gcpro1, gcpro2;
      struct string *s;

      GCPRO2(table, data);
      s = alloc_string(name);
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

struct symbol *table_add_fast(struct table *table, struct string *name, value data)
/* Requires: table_lookup(table, name->str, ...) to have just failed.
   Effects: Adds <name,data> to the symbol table.
   Modifies: table
   Returns: The new symbol
*/
{
  ulong size = intval(table->size), newsize, i, max;
  struct vector *new, *old;
  struct symbol **oldbucket;
  struct gcpro gcpro1, gcpro2;
  struct symbol *sym;

  GCCHECK(name); GCCHECK(data);
  assert(add_position < intval(table->size) && !table->buckets->data[add_position]);
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
  new = alloc_vector(newsize);
  old = table->buckets;
  table->buckets = new;
  UNGCPRO();

  /* Copy data from old buckets into new ones */
  for (oldbucket = (struct symbol **)old->data, i = 0; i < size; oldbucket++, i++)
    if (*oldbucket)
      {
	ulong hashcode = hash((*oldbucket)->name->str) & (newsize - 1), scan;
	value *bucket;

	scan = hashcode;
	bucket = &new->data[scan];
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
	      bucket = &new->data[scan];
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
  while (l1-- != 0)
    {
      if (tolower(*t1) != tolower(*t2)) return FALSE;
      t1++; t2++;
    }

  return TRUE;
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
