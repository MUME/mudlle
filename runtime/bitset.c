/* $Log: bitset.c,v $
 * Revision 1.3  1994/10/09  06:44:03  arda
 * Libraries
 * Type inference
 * Many minor improvements
 * */

static char rcsid[] = "$Id: bitset.c,v 1.3 1994/10/09 06:44:03 arda Exp $";

#include "runtime/runtime.h"
#include "interpret.h"
#include <string.h>

TYPEDOP(new_bitset, "n -> bitset. Returns a bitset usable for storing n bits",
	1, (value n),
	OP_LEAF | OP_NOESCAPE, ".s")
{
  long size;
  struct string *new;
  
  ISINT(n);
  size = (intval(n) + 7) >> 3;
  new = (struct string *)allocate_string(type_string, size + 1);
  new->str[size] = '\0';
  
  return new;
}

TYPEDOP(bcopy, "bitset1 -> bitset2. Makes a copy of bitset1",
	1, (struct string *b),
	OP_LEAF | OP_NOESCAPE, "s.s")
{
  struct string *new;
  struct gcpro gcpro1;
  long l;
  
  TYPEIS(b, type_string);
  
  GCPRO1(b);
  l = string_len(b) + 1;
  new = (struct string *)allocate_string(type_string, l);
  memcpy(new->str, b->str, l);
  UNGCPRO();
  
  return new;
}

TYPEDOP(bclear, "bitset -> bitset. Clears all bits of bitset and returns it",
	1, (struct string *b),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "s.s")
{
  TYPEIS(b, type_string);
  memset(b->str, 0, string_len(b));
  return b;
}

TYPEDOP(set_bitb, "bitset n -> . Sets bit n of specified bitset",
	2, (struct string *b, value _n),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "sn.")
{
  long n, i;
  
  TYPEIS(b, type_string);
  ISINT(_n); n = intval(_n);
  
  i = n >> 3;
  if (i < 0 || i >= string_len(b)) runtime_error(error_bad_index);
  b->str[i] |= 1 << (n & 7);
  
  undefined();
}

TYPEDOP(clear_bitb, "bitset n -> . Clears bit n of specified bitset",
	2, (struct string *b, value _n),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "sn.")
{
  long n, i;
  
  TYPEIS(b, type_string);
  ISINT(_n); n = intval(_n);
  
  i = n >> 3;
  if (i < 0 || i >= string_len(b)) runtime_error(error_bad_index);
  b->str[i] &= ~(1 << (n & 7));
  
  undefined();
}

TYPEDOP(bit_setp, "bitset n -> b. True if bit n is set",
	2, (struct string *b, value _n),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "sn.n")
{
  long n, i;
  
  TYPEIS(b, type_string);
  ISINT(_n); n = intval(_n);
  i = n >> 3;
  if (i < 0 || i >= string_len(b)) runtime_error(error_bad_index);
  
  return makeint(b->str[i] & 1 << (n & 7));
}

TYPEDOP(bit_clearp, "bitset n -> b. True if bit n is set",
	2, (struct string *b, value _n),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "sn.n")
{
  long n, i;
  
  TYPEIS(b, type_string);
  ISINT(_n); n = intval(_n);
  i = n >> 3;
  if (i < 0 || i >= string_len(b)) runtime_error(error_bad_index);
  
  return makebool(!(b->str[i] & 1 << (n & 7)));
}

/* All binary ops expect same-sized bitsets */

TYPEDOP(bunion, "bitset1 bitset2 -> bitset3. bitset3 = bitset1 U bitset2",
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

TYPEDOP(bintersection, "bitset1 bitset2 -> bitset3. bitset3 = bitset1 /\\ bitset2",
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

TYPEDOP(bdifference, "bitset1 bitset2 -> bitset3. bitset3 = bitset1 - bitset2",
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

TYPEDOP(bunionb, "bitset1 bitset2 -> bitset1. bitset1 = bitset1 U bitset2",
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

TYPEDOP(bintersectionb, "bitset1 bitset2 -> bitset1. bitset1 = bitset1 /\\ bitset2",
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

TYPEDOP(bdifferenceb, "bitset1 bitset2 -> bitset1. bitset1 = bitset1 - bitset2",
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

TYPEDOP(bassignb, "bitset1 bitset2 -> bitset1. bitset1 = bitset2",
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

TYPEDOP(bitset_inp, "bitset1 bitset2 -> b. True if bitset1 is a subset of bitset2",
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

TYPEDOP(bitset_eqp, "bitset1 bitset2 -> b. True if bitset1 == bitset2",
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

TYPEDOP(bemptyp, "bitset -> b. True if bitset has all bits clear",
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

TYPEDOP(bcount, "bitset -> n. Returns the number of bits set in bitset",
	1, (struct string *b),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "s.n")
{
  long l, n;
  char bi, *sb;
  static char count[16] = { 0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4 };
  
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
  DEFINE("new_bitset", new_bitset);
  DEFINE("bcopy", bcopy);
  DEFINE("bclear", bclear);
  DEFINE("set_bit!", set_bitb);
  DEFINE("clear_bit!", clear_bitb);
  DEFINE("bit_set?", bit_setp);
  DEFINE("bit_clear?", bit_clearp);
  DEFINE("bunion", bunion);
  DEFINE("bintersection", bintersection);
  DEFINE("bdifference", bdifference);
  DEFINE("bunion!", bunionb);
  DEFINE("bintersection!", bintersectionb);
  DEFINE("bdifference!", bdifferenceb);
  DEFINE("bassign!", bassignb);
  DEFINE("bitset_in?", bitset_inp);
  DEFINE("bitset_eq?", bitset_eqp);
  DEFINE("bempty?", bemptyp);
  DEFINE("bcount", bcount);
}
