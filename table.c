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

#include <ctype.h>
#include <string.h>
#include <strings.h>

#include "alloc.h"
#include "charset.h"
#include "error.h"
#include "mvalues.h"
#include "table.h"

struct table_methods {
  struct table *(*alloc)(ulong size);
  ulong (*hash)(const char *name, size_t len, ulong size);
  ulong (*hash_str)(const char *name, size_t len);
  int (*compare)(const void *a, const void *b, size_t n);
  value (*make_used)(long n);
  int used_delta;
};

static const struct table_methods table_methods, case_table_methods;

static value table_makeint(long n)
{
  return makeint(n);
}

static value case_table_makeint(long n)
{
  return makeint(~n);
}

#define FNV_PRIME (sizeof (long) == 4           \
                   ? 0x01000193L                \
                   : 0x00000100000001b3L)
#define FNV_OFFSET (sizeof (long) == 4          \
                    ? 0x811c9dc5L               \
                    : 0xcbf29ce484222325L)

CASSERT(sizeof (long) == 4 || sizeof (long) == 8);

static ulong table_hash_str(const char *name, size_t len)
{
  ulong code = FNV_OFFSET;
  while (len--)
    {
      unsigned char c = *name++;
      code ^= TO_7LOWER(c);
      code *= FNV_PRIME;
    }
  return code;
}

static ulong case_table_hash_str(const char *name, size_t len)
{
  ulong code = FNV_OFFSET;
  while (len--)
    {
      unsigned char c = *name++;
      code ^= c;
      code *= FNV_PRIME;
    }
  return code;
}

/* FNV-1a hash from
   http://tools.ietf.org/html/draft-eastlake-fnv-03 */
static ulong internal_hash(const char *name, size_t len, ulong size,
                           ulong (*hash_str)(const char *, size_t len))
{
  ulong code = hash_str(name, len);
  assert(size && (size & (size - 1)) == 0);
  ulong mask = size - 1;
  int bits = 0;
  while (size > UINT_MAX)
    {
      const size_t uintbits = CHAR_BIT * sizeof (unsigned int);
      size >>= uintbits - 1; /* two shifts to avoid too-long-shift warning */
      size >>= 1;
      bits += uintbits;
    }
  bits += ffs(size) - 1;
  code ^= code >> bits;
  code &= mask;
  return code;
}

/* case- and accentuation-insensitive */
ulong symbol_hash_len(const char *name, size_t len, ulong size)
{
  return internal_hash(name, len, size, table_methods.hash_str);
}

/* case- and accentuation-sensitive */
ulong case_symbol_hash_len(const char *name, size_t len, ulong size)
{
  return internal_hash(name, len, size, case_table_methods.hash_str);
}

static const struct table_methods table_methods = {
  .alloc      = alloc_table,
  .hash       = symbol_hash_len,
  .hash_str   = table_hash_str,
  .compare    = mem8icmp,
  .make_used  = table_makeint,
  .used_delta = 1
};

static const struct table_methods case_table_methods = {
  .alloc      = alloc_ctable,
  .hash       = case_symbol_hash_len,
  .hash_str   = case_table_hash_str,
  .compare    = memcmp,
  .make_used  = case_table_makeint,
  .used_delta = -1
};

struct table *alloc_table(ulong size)
/* Returns: A new symbol table, initially of size size.
   Requires: size be a power of 2 <= MAX_VECTOR_SIZE
*/
{
  struct table *newp = (struct table *)allocate_record(
    type_table, grecord_fields(*newp));
  GCPRO(newp);
  newp->used = table_methods.make_used(0);
  value vec = alloc_vector(size);
  newp->buckets = vec;
  UNGCPRO();

  return newp;
}

struct table *alloc_ctable(ulong size)
/* Returns: A new symbol table, initially of size size.
   Requires: size be a power of 2 <= MAX_VECTOR_SIZE
*/
{
  struct table *t = alloc_table(size);
  t->used = case_table_methods.make_used(0);
  return t;
}

static const struct table_methods *get_methods(struct table *t)
{
  return is_ctable(t) ? &case_table_methods : &table_methods;
}

static ulong add_position;

/* return position of the symbol, or -1 if not found; updates add_position */
static long table_find(struct table *table, const char *name, size_t nlength)
{
  const struct table_methods *methods = get_methods(table);

  ulong size = vector_len(table->buckets);
  ulong hashcode = methods->hash(name, nlength, size);
  long scan = hashcode;
  struct symbol **bucket = (struct symbol **)&table->buckets->data[scan];
  for (;;)
    {
      if (*bucket == NULL)
        {
          add_position = scan;
          return -1;
        }
      if (string_len((*bucket)->name) == nlength
          && methods->compare(name, (*bucket)->name->str, nlength) == 0)
        return scan;
      ++scan;
      ++bucket;
      if (scan == size)
        {
          scan = 0;
          bucket = (struct symbol **)&table->buckets->data[scan];
        }
      assert(scan != hashcode);	/* The table is never allowed to be full */
    }
}

/* Returns symbol for name in table, or NULL. table_add_fast() can be called
   immediately if you wish to add an entry to name to the symbol table (but no
   intervening call to the module should be made). */
struct symbol *table_lookup_len(struct table *table, const char *name,
                                size_t len)
{
  long pos = table_find(table, name, len);
  if (pos < 0)
    return NULL;
  return table->buckets->data[pos];
}

struct symbol *table_mlookup(struct table *table, struct string *name)
{
  return table_lookup_len(table, name->str, string_len(name));
}

struct symbol *table_remove(struct table *table, const char *name)
{
  return table_remove_len(table, name, strlen(name));
}

struct symbol *table_remove_len(struct table *table, const char *name,
				size_t nlength)
/* Effects: Removes table[name] from data. Rehashes nescessary values.
   Modifies: table
   Returns: false if the entry wasn't found
*/
{
  long scan = table_find(table, name, nlength);
  if (scan < 0)
    return NULL;

  struct symbol *result = table->buckets->data[scan];
  struct symbol **bucket = (struct symbol **)&table->buckets->data[scan];
  long size = vector_len(table->buckets);
  *bucket = NULL;

  const struct table_methods *methods = get_methods(table);

  for (;;)
    {
      ++bucket;
      ++scan;
      if (scan == size)
        {
          scan = 0;
          bucket = (struct symbol **)&table->buckets->data[scan];
        }
      struct symbol *sym = *bucket;
      if (sym == NULL)
        break;
      ulong newpos = methods->hash(sym->name->str, string_len(sym->name),
                                   size);

      *bucket = NULL;
      struct symbol **newbuck
        = (struct symbol **)&table->buckets->data[newpos];
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
    }
  table->used = mudlle_iadd(table->used, -methods->used_delta);
  return result;
}

static bool table_set_len(struct table *table, const char *name,
                          size_t nlength, value data)
/* Effects: Sets table[name] to data, adds it if not already present
   Modifies: table
   Returns: false if entry name was readonly
*/
{
  struct symbol *sym = table_lookup_len(table, name, nlength);
  if (sym)
    {
      if (obj_readonlyp(&sym->o)) return false;
      sym->data = data;
    }
  else if (data)
    {
      GCPRO(table, data);
      struct string *s = make_readonly(alloc_string_length(name, nlength));
      UNGCPRO();
      table_add_fast(table, s, data);
    }
  return true;
}

bool table_set(struct table *table, const char *name, value data)
{
  return table_set_len(table, name, strlen(name), data);
}

/* *x is GC-protected */
enum runtime_error safe_table_mset(struct table *table, struct string *s,
                                   value *x)
{
  assert(TYPE(s, string));

  if (obj_readonlyp(&table->o))
    return error_value_read_only;

  struct symbol *sym = table_mlookup(table, s);
  if (sym)
    {
      if (obj_readonlyp(&sym->o))
        return error_value_read_only;
      sym->data = *x;
    }
  else if (*x)
    {
      if (table_entries(table) >= MAX_TABLE_ENTRIES)
        return error_bad_value; /* table is full */
      value x2 = *x;
      GCPRO(table, x2);
      if (!obj_readonlyp(&s->o))
	{
	  /* make a copy of index string or it may get modified */
	  s = make_readonly(mudlle_string_copy(s));
	}
      table_add_fast(table, s, x2);
      UNGCPRO();
      *x = x2;
    }
  return error_none;
}

void table_mset(struct table *table, struct string *name, value data)
{
  if (safe_table_mset(table, name, &data) != error_none)
    abort();
}

struct symbol *table_add(struct table *table, struct string *name, value data)
/* Effects: Adds <name,data> to the symbol table.
   Returns: The symbol if it could be added, NULL if it was already in the
     symbol table.
   Modifies: table
*/
{
  if (table_mlookup(table, name))
    return NULL;
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
  GCPRO(table);
  struct symbol *sym = alloc_symbol(name, data);
  UNGCPRO();
  return table_add_sym_fast(table, sym);
}

struct symbol *table_add_sym_fast(struct table *table, struct symbol *sym)
/* Requires: table_lookup(table, name->str, ...) to have just failed.
   Effects: Adds <name,data> to the symbol table.
   Modifies: table
   Returns: The symbol
*/
{
  ulong size = vector_len(table->buckets);

  assert(!obj_readonlyp(&table->o));
  assert(!obj_readonlyp(&table->buckets->o));
  assert(obj_readonlyp(&sym->name->o));

  assert(add_position < size
         && !table->buckets->data[add_position]);

  const struct table_methods *methods = get_methods(table);

  table->buckets->data[add_position] = sym;
  table->used = mudlle_iadd(table->used, methods->used_delta);

  /* If table is 3/4 full, increase its size */
  ulong max = size / 2 + size / 4;
  if (table_entries(table) < max)
    return sym;

  /* Double table size */
  GCPRO(table, sym);
  struct vector *newp = alloc_vector(2 * size);
  UNGCPRO();
  struct vector *old = table->buckets;
  table->buckets = newp;
  table->used = methods->make_used(0);

  for (long i = 0; i < size; ++i)
    {
      struct symbol *osym = old->data[i];
      if (osym == NULL)
        continue;
      if (table_find(table, osym->name->str, string_len(osym->name)) >= 0)
        abort();
      table_add_sym_fast(table, osym);
    }
  return sym;
}

struct list *table_list(struct table *table)
/* Returns: A list which contains the symbols in symbol table table
     (elements whose value is null are omitted).
     The order is arbitrary.
*/
{
  struct list *l = NULL;
  struct symbol *sym;
  ulong size = vector_len(table->buckets);

  GCPRO(l, table);
  while (size > 0)
    {
      size--;
      sym = table->buckets->data[size];
      if (sym && sym->data) l = alloc_list(sym, l);
    }
  UNGCPRO();

  return l;
}

struct list *table_prefix(struct table *table, struct string *prefix)
/* Returns: A list of all the symbols in table whose name starts with
     prefix (case insensitive, like all table ops)
*/
{
  ulong prelen = string_len(prefix);
  struct vector *buckets = table->buckets;
  ulong size = vector_len(table->buckets);
  struct list *l = NULL;

  const struct table_methods *methods = get_methods(table);

  GCPRO(l, buckets, prefix);
  while (size-- > 0)
    {
      struct symbol *sym = buckets->data[size];
      if (sym == NULL || sym->data == NULL)
        continue;
      ulong symlen = string_len(sym->name);
      if (prelen > symlen)
        continue;
      if (methods->compare(sym->name->str, prefix->str, prelen) == 0)
        l = alloc_list(sym, l);
    }
  UNGCPRO();

  return l;
}

/* 'check' must not cause GC */
struct symbol *table_exists(struct table *table,
                            bool (*check)(struct symbol *sym, void *data),
                            void *data)
{
  long size = vector_len(table->buckets);
  struct symbol **bucket = (struct symbol **)&table->buckets->data[0];
  for (long i = 0; i < size; ++i, ++bucket)
    if (*bucket && check(*bucket, data))
      return *bucket;

  return NULL;
}

/* 'action' must not modify the table, but may cause GC */
void table_foreach(struct table *table, void *data,
                   void (*action)(struct symbol *, void *))
{
  GCPRO(table);
  long size = vector_len(table->buckets);
  for (long i = 0; i < size; ++i)
    {
      struct symbol *bucket = (struct symbol *)table->buckets->data[i];
      if (bucket)
	action(bucket, data);
    }
  UNGCPRO();
}

/* copies table, reusing symbols */
struct table *table_shallow_copy(struct table *table)
{
  assert(TYPE(table, table));
  const struct table_methods *m = get_methods(table);
  size_t nbuckets = vector_len(table->buckets);
  GCPRO(table);
  struct table *new = m->alloc(nbuckets);
  UNGCPRO();
  assert(nbuckets == vector_len(new->buckets));
  memcpy(new->buckets->data, table->buckets->data,
         nbuckets * sizeof new->buckets->data[0]);
  new->used = table->used;
  return new;
}

/* makes a copy of the non-null elements of table */
struct table *table_copy(struct table *table)
{
  struct vector *buckets = table->buckets;
  ulong blen = vector_len(buckets);
  ulong count = 0;
  for (ulong n = 0; n < blen; ++n)
    {
      struct symbol *sym = buckets->data[n];
      if (sym && sym->data)
        ++count;
    }

  ulong nsize = table_good_size(count);
  struct table *ntable = NULL;

  const struct table_methods *methods = get_methods(table);

  GCPRO(ntable, buckets);
  ntable = methods->alloc(nsize);
  for (ulong n = 0; n < blen; ++n)
    {
      struct symbol *sym = buckets->data[n];
      if (sym && sym->data)
        table_mset(ntable, sym->name, sym->data);
    }
  UNGCPRO();
  return ntable;
}

void rehash_table(struct table *table)
{
  struct vector *buckets = table->buckets;
  ulong blen = vector_len(buckets);

  size_t bsize = blen * sizeof buckets->data[0];
  value *old = malloc(bsize);
  memcpy(old, buckets->data, bsize);
  memset(buckets->data, 0, bsize);

  /* pretend table is read/write; this is safe as we are not adding
     more table entries */
  ulong tflags = table->o.flags;
  table->o.flags = 0;
  ulong bflags = buckets->o.flags;
  buckets->o.flags = 0;

  const struct table_methods *methods = get_methods(table);
  value oused = table->used;
  table->used = methods->make_used(0);

  for (long i = 0; i < blen; ++i)
    {
      struct symbol *sym = old[i];
      if (sym == NULL)
        continue;

      struct string *name = sym->name;
      assert(obj_readonlyp(&name->o));
      if (table_find(table, name->str, string_len(name)) >= 0)
        abort();
      table_add_sym_fast(table, sym);
      assert(table->buckets == buckets);
    }

  assert(table->used == oused);

  table->o.flags = tflags;
  buckets->o.flags = bflags;

  free(old);
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
