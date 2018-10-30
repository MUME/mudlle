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

#include "../mudlle-config.h"

#include <stdlib.h>
#include <string.h>

#include "pattern.h"
#include "prims.h"

#include "../charset.h"
#include "../error.h"
#include "../table.h"

struct seen_values {
  struct {
    /* 'lhs->size' stores the index into the 'seen' array; 'lhs_size' keeps the
       original size */
    struct obj *lhs, *rhs;
    long lhs_size;
  } *seen;
  size_t size, used;
};

static bool recurse(value pat, value val, struct seen_values *seen);

struct table_data {
  struct seen_values *seen;
  struct table *table;
  size_t nentries;
};

/* return true unless sym can be found in data->table */
static bool recurse_symbol(struct symbol *sym, void *_data)
{
  struct table_data *data = _data;

  if (sym->data != NULL)
    ++data->nentries;

  value val = NULL;
  struct symbol *symval = table_mlookup(data->table, sym->name);
  if (symval)
    val = symval->data;
  return !recurse(sym->data, val, data->seen);
}

/* return true if we found too many entries */
static bool count_non_null_syms(struct symbol *sym, void *_data)
{
  struct table_data *data = _data;
  if (sym->data != NULL
      && data->nentries-- == 0)
    return true;
  return false;
}

/* return the vector_len(v), but handle that the size may be an index into seen
   values */
static long safe_vlen(struct vector *v, struct seen_values *seen)
{
  if (v->o.flags & OBJ_FLAG_0)
    {
      long idx = v->o.size;
      return (seen->seen[idx].lhs_size - sizeof v->o) / sizeof v->data[0];
    }
  return vector_len(v);
}

static bool recurse(value lhs, value rhs, struct seen_values *seen)
{
  if (!pointerp(lhs))
    return lhs == rhs;

  if (!pointerp(rhs))
    return false;

  struct obj *lhsobj = lhs, *rhsobj = rhs;

  enum mudlle_type lhstype = lhsobj->type;
  if (lhstype != rhsobj->type)
    return false;

  bool has_seen_lhs = lhsobj->flags & OBJ_FLAG_0;
  bool has_seen_rhs = rhsobj->flags & OBJ_FLAG_1;
  if (has_seen_lhs != has_seen_rhs)
    return false;
  if (has_seen_lhs)
    {
      size_t idx = lhsobj->size;
      return seen->seen[idx].rhs == rhs;
    }

  if (lhs == rhs)
    return true;

  /* update simple_equal?() in optimise.mud as well if more types are
     supported here */
  switch (lhstype)
    {
    case type_symbol:
    case type_vector:
    case type_pair:
    case type_table:
      break;
    case type_string:
      {
        struct string *lhsstr = lhs, *rhsstr = rhs;
        size_t len = string_len(lhsstr);
        return (len == string_len(rhsstr)
                && memcmp(lhsstr->str, rhsstr->str, len) == 0);
      }
    case type_float:
      return (((struct mudlle_float *)lhs)->d
              == ((struct mudlle_float *)rhs)->d);
    case type_bigint:
      check_bigint((struct bigint *)lhs);
      check_bigint((struct bigint *)rhs);
      return mpz_cmp(((struct bigint *)lhs)->mpz,
                     ((struct bigint *)rhs)->mpz) == 0;
    default:
      return false;
    }

  assert(lhsobj->garbage_type != garbage_static_string
         && rhsobj->garbage_type != garbage_static_string);

  if (seen->used == seen->size)
    {
      seen->size = seen->size ? seen->size * 2 : 16;
      seen->seen = realloc(seen->seen, sizeof *seen->seen * seen->size);
    }
  seen->seen[seen->used].lhs = lhs;
  seen->seen[seen->used].rhs = rhs;
  seen->seen[seen->used].lhs_size = lhsobj->size;
  lhsobj->size = seen->used++;
  lhsobj->flags |= OBJ_FLAG_0;
  rhsobj->flags |= OBJ_FLAG_1;

  switch (lhstype)
    {
    case type_symbol:
      {
        struct symbol *sympat = lhs, *symval = rhs;
        size_t symlen = string_len(sympat->name);
        return (symlen == string_len(symval->name)
                && mem8icmp(sympat->name->str, symval->name->str, symlen) == 0
                && recurse(sympat->data, symval->data, seen));
      }
    case type_vector:
      {
        long vl = safe_vlen(lhs, seen);
        if (vl != safe_vlen(rhs, seen))
          return false;
        for (long i = 0; i < vl; ++i)
          if (!recurse(((struct vector *)lhs)->data[i],
                       ((struct vector *)rhs)->data[i],
                       seen))
            return false;
        return true;
      }
    case type_pair:
      return (recurse(((struct list *)lhs)->car,
                      ((struct list *)rhs)->car,
                      seen)
              && recurse(((struct list *)lhs)->cdr,
                         ((struct list *)rhs)->cdr,
                         seen));
    case type_table:
      {
        if (is_ctable(lhs) != is_ctable(rhs))
          return false;
        struct table_data tdata = { .seen = seen, .table = rhs };
        if (table_exists((struct table *)lhs, recurse_symbol, &tdata)
            || table_exists((struct table *)rhs, count_non_null_syms, &tdata))
          return false;
        return tdata.nentries == 0;
      }
    default:
      abort();
    }
}

TYPEDOP(equalp, "equal?",
        "`x0 `x1 -> `b. Return true if `x0 is equal to `x1.\n"
        "Pairs, vectors, symbols, and tables are compared"
        " recursively.\n"
        "All objects are considered equal to themselves.\n"
        "No objects of different types are considered equal. E.g., a float"
        " is never considered equal to a bigint or an integer."
        " Also, a ctable is never considered equal to a table.\n"
        "Strings, bigints and floats are considered equal if `string_cmp,"
        " `bicmp, and `fcmp return zero, respectively.\n"
        "Symbol name comparisons are case- and accent-insensitive, except"
        " inside ctables.\n"
        "Table entries that are `null are considered equivalent to"
        " absent ones.\n"
        "The graph of the container objects (pairs, vectors, symbols, and"
        " tables) in `x0 and `x1 must be homomorphic. This matters if the"
        " same container is referenced more than once.",
        2, (value lhs, value rhs),
        OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_STR_READONLY | OP_CONST,
        "xx.n")
{
  struct seen_values seen = { .size = 0 };
  bool result = recurse(lhs, rhs, &seen);

  /* restore modified data */
  for (int i = 0; i < seen.used; ++i)
    {
      seen.seen[i].lhs->size = seen.seen[i].lhs_size;
      seen.seen[i].lhs->flags &= ~OBJ_FLAG_0;
      seen.seen[i].rhs->flags &= ~OBJ_FLAG_1;
    }
  free(seen.seen);

  return makebool(result);
}

void pattern_init(void)
{
  DEFINE(equalp);
}
