/*
 * Copyright (c) 1993-2004 David Gay and Gustav Hållberg
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
#include "mudlle.h"
#include "error.h"
#include "table.h"

static int recurse(value pat, value val);

static struct table *recurse_table_val;

static int check_table(struct symbol *sym)
{
  struct symbol *symval;

  return !(table_lookup(recurse_table_val, sym->name->str, &symval) &&
	   recurse(sym->data, symval->data));
}

static int recurse(value pat, value val)
{
  ubyte pattype;

  if (!pat)
    return !val;

  if (integerp(pat))
    return pat == val;

  if (val == pat)
    return 1;

  pattype = TYPEOF(pat);

  if (pattype != TYPEOF(val))
    return 0;

  switch (pattype) {
  case type_symbol:
    return (recurse(((struct symbol *)pat)->name,
		    ((struct symbol *)val)->name) &&
	    recurse(((struct symbol *)pat)->data,
		    ((struct symbol *)val)->data));
  case type_vector:
    {
      long i, vl = vector_len((struct vector *)pat);
      if (vl != vector_len((struct vector *)val))
	return 0;
      for (i = 0; i < vl; ++i)
	if (!recurse(((struct vector *)pat)->data[i],
		     ((struct vector *)val)->data[i]))
	  return 0;
      return 1;
    }
  case type_pair:
    return (recurse(((struct list *)pat)->car,
		    ((struct list *)val)->car) &&
	    recurse(((struct list *)pat)->cdr,
		    ((struct list *)val)->cdr));
  case type_string:
    return strcmp(((struct string *)pat)->str,
		  ((struct string *)val)->str) == 0;
  case type_float:
    return (((struct mudlle_float *)pat)->d ==
	    ((struct mudlle_float *)val)->d);
  case type_bigint:
    check_bigint((struct bigint *)pat);
    check_bigint((struct bigint *)val);
    return mpz_cmp(((struct bigint *)pat)->mpz,
		   ((struct bigint *)val)->mpz) == 0;
  case type_table:
    recurse_table_val = (struct table *)val;
    return !table_exists((struct table *)pat, check_table);
  default:
    return pat == val;
  }

  NOTREACHED;
}

OPERATION(pattern_match, "x y -> b. Returns TRUE if x is equal to y as the "
	  "pattern matcher sees it", 2, (value pat, value val),
	  OP_LEAF | OP_NOESCAPE)
{
  return makebool(recurse(pat, val));
}

void pattern_init(void)
{
  DEFINE("=>", pattern_match);
}
