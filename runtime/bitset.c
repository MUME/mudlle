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

#include "runtime/runtime.h"
#include "interpret.h"
#include <string.h>

TYPEDOP(new_bitset, 0,
        "`n -> `bitset. Returns a bitset usable for storing at least `n bits",
	1, (value n),
	OP_LEAF | OP_NOESCAPE, "n.s")
{
  long size;
  struct string *newp;

  if ((size = GETINT(n)) < 0)
    runtime_error(error_bad_value);

  size = (size + 7) >> 3;
  newp = (struct string *)allocate_string(type_string, size + 1);
  newp->str[size] = '\0';
  
  return newp;
}

TYPEDOP(bcopy, 0, "`bitset1 -> `bitset2. Makes a copy of `bitset1",
	1, (struct string *b),
	OP_LEAF | OP_NOESCAPE, "s.s")
{
  struct string *newp;
  struct gcpro gcpro1;
  long l;
  
  TYPEIS(b, type_string);
  
  GCPRO1(b);
  l = string_len(b) + 1;
  newp = (struct string *)allocate_string(type_string, l);
  memcpy(newp->str, b->str, l);
  UNGCPRO();
  
  return newp;
}

TYPEDOP(bclear, 0,
        "`bitset -> `bitset. Clears all bits of `bitset and returns it",
	1, (struct string *b),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "s.s")
{
  TYPEIS(b, type_string);
  memset(b->str, 0, string_len(b));
  return b;
}

TYPEDOP(set_bitb, "set_bit!", "`bitset `n -> . Sets bit `n in `bitset",
	2, (struct string *b, value _n),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "sn.")
{
  long n, i;
  
  TYPEIS(b, type_string);
  n = GETINT(_n);
  
  i = n >> 3;
  if (i < 0 || i >= string_len(b)) runtime_error(error_bad_index);
  b->str[i] |= 1 << (n & 7);
  
  undefined();
}

TYPEDOP(clear_bitb, "clear_bit!",
        "`bitset `n -> . Clears bit `n in `bitset",
	2, (struct string *b, value _n),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "sn.")
{
  long n, i;
  
  TYPEIS(b, type_string);
  n = GETINT(_n);
  
  i = n >> 3;
  if (i < 0 || i >= string_len(b)) runtime_error(error_bad_index);
  b->str[i] &= ~(1 << (n & 7));
  
  undefined();
}

TYPEDOP(bit_setp, "bit_set?", "`bitset `n -> `b. True if bit `n is set",
	2, (struct string *b, value _n),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "sn.n")
{
  long n, i;
  
  TYPEIS(b, type_string);
  n = GETINT(_n);
  i = n >> 3;
  if (i < 0 || i >= string_len(b)) runtime_error(error_bad_index);
  
  return makeint(b->str[i] & 1 << (n & 7));
}

TYPEDOP(bit_clearp, "bit_clear?",
        "`bitset `n -> `b. True if bit `n is not set",
	2, (struct string *b, value _n),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "sn.n")
{
  long n, i;
  
  TYPEIS(b, type_string);
  n = GETINT(_n);
  i = n >> 3;
  if (i < 0 || i >= string_len(b)) runtime_error(error_bad_index);
  
  return makebool(!(b->str[i] & 1 << (n & 7)));
}

/* All binary ops expect same-sized bitsets */

TYPEDOP(bunion, 0,
        "`bitset1 `bitset2 -> `bitset3. `bitset3 = `bitset1 U `bitset2",
	2, (struct string *b1, struct string *b2),
	OP_LEAF | OP_NOESCAPE, "ss.s")
{
  struct string *b3;
  char *sb1, *sb2, *sb3;
  long l;
  struct gcpro gcpro1, gcpro2;
  
  TYPEIS(b1, type_string);
  TYPEIS(b2, type_string);
  l = string_len(b1);
  if (l != string_len(b2)) runtime_error(error_bad_value);
  l = l + 1;
  
  GCPRO2(b1, b2);
  b3 = (struct string *)allocate_string(type_string, l);
  UNGCPRO();
  
  sb1 = b1->str; sb2 = b2->str; sb3 = b3->str;
  while (--l >= 0) *sb3++ = *sb1++ | *sb2++;
  
  return b3;
}

TYPEDOP(bintersection, 0,
        "`bitset1 `bitset2 -> `bitset3. `bitset3 = `bitset1 /\\ `bitset2",
	2, (struct string *b1, struct string *b2),
	OP_LEAF | OP_NOESCAPE, "ss.s")
{
  struct string *b3;
  char *sb1, *sb2, *sb3;
  long l;
  struct gcpro gcpro1, gcpro2;
  
  TYPEIS(b1, type_string);
  TYPEIS(b2, type_string);
  l = string_len(b1);
  if (l != string_len(b2)) runtime_error(error_bad_value);
  l = l + 1;
  
  GCPRO2(b1, b2);
  b3 = (struct string *)allocate_string(type_string, l);
  UNGCPRO();
  
  sb1 = b1->str; sb2 = b2->str; sb3 = b3->str;
  while (--l >= 0) *sb3++ = *sb1++ & *sb2++;
  
  return b3;
}

TYPEDOP(bdifference, 0,
        "`bitset1 `bitset2 -> `bitset3. `bitset3 = `bitset1 - `bitset2",
	2, (struct string *b1, struct string *b2),
	OP_LEAF | OP_NOESCAPE, "ss.s")
{
  struct string *b3;
  char *sb1, *sb2, *sb3;
  long l;
  struct gcpro gcpro1, gcpro2;
  
  TYPEIS(b1, type_string);
  TYPEIS(b2, type_string);
  l = string_len(b1);
  if (l != string_len(b2)) runtime_error(error_bad_value);
  l = l + 1;
  
  GCPRO2(b1, b2);
  b3 = (struct string *)allocate_string(type_string, l);
  UNGCPRO();
  
  sb1 = b1->str; sb2 = b2->str; sb3 = b3->str;
  while (--l >= 0) *sb3++ = *sb1++ & ~*sb2++;
  
  return b3;
}

TYPEDOP(bunionb, "bunion!",
        "`bitset1 `bitset2 -> `bitset1. `bitset1 = `bitset1 U `bitset2",
	2, (struct string *b1, struct string *b2),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "ss.s")
{
  char *sb1, *sb2;
  long l;
  
  TYPEIS(b1, type_string);
  TYPEIS(b2, type_string);
  l = string_len(b1);
  if (l != string_len(b2)) runtime_error(error_bad_value);
  
  sb1 = b1->str; sb2 = b2->str;
  while (l-- > 0) *sb1++ |= *sb2++;
  
  return b1;
}

TYPEDOP(bintersectionb, "bintersection!",
        "`bitset1 `bitset2 -> `bitset1. `bitset1 = `bitset1 /\\ `bitset2",
	2, (struct string *b1, struct string *b2),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "ss.s")
{
  char *sb1, *sb2;
  long l;
  
  TYPEIS(b1, type_string);
  TYPEIS(b2, type_string);
  l = string_len(b1);
  if (l != string_len(b2)) runtime_error(error_bad_value);
  
  sb1 = b1->str; sb2 = b2->str;
  while (l-- > 0) *sb1++ &= *sb2++;
  
  return b1;
}

TYPEDOP(bdifferenceb, "bdifference!",
        "`bitset1 `bitset2 -> `bitset1. `bitset1 = `bitset1 - `bitset2",
	2, (struct string *b1, struct string *b2),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "ss.s")
{
  char *sb1, *sb2;
  long l;
  
  TYPEIS(b1, type_string);
  TYPEIS(b2, type_string);
  l = string_len(b1);
  if (l != string_len(b2)) runtime_error(error_bad_value);
  
  sb1 = b1->str; sb2 = b2->str;
  while (l-- > 0) *sb1++ &= ~*sb2++;
  
  return b1;
}

TYPEDOP(bassignb, "bassign!",
        "`bitset1 `bitset2 -> `bitset1. `bitset1 = `bitset2",
	2, (struct string *b1, struct string *b2),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "ss.s")
{
  long l;
  
  TYPEIS(b1, type_string);
  TYPEIS(b2, type_string);
  l = string_len(b1);
  if (l != string_len(b2)) runtime_error(error_bad_value);
  
  memcpy(b1->str, b2->str, l);
  
  return b1;
}

TYPEDOP(bitset_inp, "bitset_in?",
        "`bitset1 `bitset2 -> `b. True if `bitset1 is a subset of `bitset2",
	2, (struct string *b1, struct string *b2),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "ss.n")
{
  char *sb1, *sb2;
  long l;
  
  TYPEIS(b1, type_string);
  TYPEIS(b2, type_string);
  l = string_len(b1);
  if (l != string_len(b2)) runtime_error(error_bad_value);
  
  sb1 = b1->str; sb2 = b2->str;
  while (l-- >= 0) if (*sb1++ & ~*sb2++) return makebool(FALSE);
  
  return makebool(TRUE);
}

TYPEDOP(bitset_eqp, "bitset_eq?",
        "`bitset1 `bitset2 -> `b. True if `bitset1 == `bitset2",
	2, (struct string *b1, struct string *b2),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "ss.n")
{
  long l;
  
  TYPEIS(b1, type_string);
  TYPEIS(b2, type_string);
  l = string_len(b1);
  if (l != string_len(b2)) runtime_error(error_bad_value);
  
  return makebool(memcmp(b1->str, b2->str, l) == 0);
}

TYPEDOP(bemptyp, "bempty?",
        "`bitset -> `b. True if `bitset has all bits clear",
	1, (struct string *b),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "s.n")
{
  long l;
  char *sb;
  
  TYPEIS(b, type_string);
  
  l = string_len(b);
  sb = b->str;
  while (l-- > 0)
    if (*sb++) return makebool(FALSE);
  
  return makebool(TRUE);
}

TYPEDOP(bcount, 0, "`bitset -> `n. Returns the number of bits set in `bitset",
	1, (struct string *b),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "s.n")
{
  long l, n;
  char bi, *sb;
  static const char count[16] = {
    0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4
 };
  
  TYPEIS(b, type_string);
  l = string_len(b);
  sb = b->str;
  n = 0;
  while (l-- > 0)
    {
      bi = *sb++;
      n = n + count[bi & 15] + count[(bi >> 4) & 15];
    }
  return makeint(n);
}

void bitset_init(void)
{
  DEFINE(new_bitset);
  DEFINE(bcopy);
  DEFINE(bclear);
  DEFINE(set_bitb);
  DEFINE(clear_bitb);
  DEFINE(bit_setp);
  DEFINE(bit_clearp);
  DEFINE(bunion);
  DEFINE(bintersection);
  DEFINE(bdifference);
  DEFINE(bunionb);
  DEFINE(bintersectionb);
  DEFINE(bdifferenceb);
  DEFINE(bassignb);
  DEFINE(bitset_inp);
  DEFINE(bitset_eqp);
  DEFINE(bemptyp);
  DEFINE(bcount);
}
