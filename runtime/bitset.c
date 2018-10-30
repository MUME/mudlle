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

#include <string.h>

#include "bitset.h"
#include "check-types.h"
#include "prims.h"

#include "../interpret.h"
#include "../utils.h"

TYPEDOP(new_bitset, 0,
        "`n -> `bitset. Returns an empty bitset usable for storing at"
        " least `n bits.",
	1, (value n),
	OP_TRIVIAL | OP_LEAF | OP_NOESCAPE, "n.s")
{
  long size;
  CHECK_TYPES(n, CT_RANGE(size, 0, MAX_STRING_SIZE * CHAR_BIT));
  size = (size + CHAR_BIT - 1) / CHAR_BIT;
  struct string *newp = alloc_empty_string(size);
  memset(newp->str, 0, size);
  return newp;
}

TYPEDOP(bclear, 0,
        "`bitset -> `bitset. Clears all bits of `bitset and returns it",
	1, (struct string *b),
	OP_TRIVIAL | OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "s.s")
{
  CHECK_TYPES(b, string);
  memset(b->str, 0, string_len(b));
  return b;
}

static inline enum runtime_error ct_bit_e(long n, const char **errmsg,
                                          ulong *dstchr,
                                          unsigned char *dstmask,
                                          struct string *bitset)
{
  ulong maxn = string_len(bitset);
  if (maxn == 0)
    {
      *errmsg = "empty bitset";
      return error_bad_value;
    }
  maxn = maxn * CHAR_BIT - 1;
  if (n < 0 || n > maxn)
    {
      *errmsg = out_of_range_message(n, 0, maxn);
      return error_bad_index;
    }
  ulong u = n;
  *dstchr = u / CHAR_BIT;
  *dstmask = P(u % CHAR_BIT);
  return error_none;
}

#define __CT_BIT_E(var, msg, dstchr_dstmask_bitset)     \
  ct_bit_e(var, msg, &(ARGN1 dstchr_dstmask_bitset),    \
           &(ARGN2 dstchr_dstmask_bitset),              \
           ARGN3 dstchr_dstmask_bitset)
#define CT_BIT(dstchr, dstmask, bitset)                 \
  CT_INT_P((dstchr, dstmask, bitset), __CT_BIT_E)

TYPEDOP(set_bitb, "set_bit!", "`bitset `n -> . Sets bit `n in `bitset",
	2, (struct string *b, value mnum),
	OP_TRIVIAL | OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "sn.")
{
  ulong i;
  unsigned char mask;
  CHECK_TYPES(b, string,
              mnum, CT_BIT(i, mask, b));
  b->str[i] |= mask;
  undefined();
}

TYPEDOP(clear_bitb, "clear_bit!",
        "`bitset `n -> . Clears bit `n in `bitset",
	2, (struct string *b, value mnum),
	OP_TRIVIAL | OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "sn.")
{
  ulong i;
  unsigned char mask;
  CHECK_TYPES(b,    string,
              mnum, CT_BIT(i, mask, b));
  b->str[i] &= ~mask;
  undefined();
}

static bool bit_is_set(const struct prim_op *op,
                       struct string *b, value mnum)
{
  ulong i;
  unsigned char mask;
  CHECK_TYPES_OP(op,
                 b,    string,
                 mnum, CT_BIT(i, mask, b));
  return b->str[i] & mask;
}

EXT_TYPEDOP(bit_setp, "bit_set?", "`bitset `n -> `b. True if bit `n is set",
            2, (struct string *b, value n), (b, n),
            OP_TRIVIAL | OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST, "sn.n")
{
  return makebool(bit_is_set(THIS_OP, b, n));
}

TYPEDOP(bit_clearp, "bit_clear?",
        "`bitset `n -> `b. True if bit `n is not set",
	2, (struct string *b, value n),
	OP_TRIVIAL | OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST, "sn.n")
{
  return makebool(!bit_is_set(THIS_OP, b, n));
}

/* All binary ops expect same-sized bitsets */
static struct string *bitset_binop(const struct prim_op *pop,
                                   struct string *b1, struct string *b2,
                                   bool alloc_new,
                                   void (*op)(const char *s1, const char *s2,
                                              char *to, size_t length))
{
  CHECK_TYPES_OP(pop, b1, string, b2, string);
  long l = string_len(b1);
  if (l != string_len(b2))
    RUNTIME_ERROR(error_bad_value, "arguments of different length");

  struct string *result;
  if (alloc_new)
    {
      GCPRO(b1, b2);
      result = alloc_empty_string(l);
      UNGCPRO();
    }
  else
    result = b1;

  op(b1->str, b2->str, result->str, l);
  return result;
}

static void bunion_op(const char *s1, const char *s2, char *to, size_t length)
{
  while (length--)
    *to++ = *s1++ | *s2++;
}

TYPEDOP(bunion, 0,
        "`bitset1 `bitset2 -> `bitset3. `bitset3 = `bitset1 U `bitset2",
	2, (struct string *b1, struct string *b2),
	OP_TRIVIAL | OP_LEAF | OP_NOESCAPE, "ss.s")
{
  return bitset_binop(THIS_OP, b1, b2, true, bunion_op);
}

static void bintersection_op(const char *s1, const char *s2, char *to,
                             size_t length)
{
  while (length--)
    *to++ = *s1++ & *s2++;
}

TYPEDOP(bintersection, 0,
        "`bitset1 `bitset2 -> `bitset3. `bitset3 = `bitset1 /\\ `bitset2",
	2, (struct string *b1, struct string *b2),
	OP_TRIVIAL | OP_LEAF | OP_NOESCAPE, "ss.s")
{
  return bitset_binop(THIS_OP, b1, b2, true, bintersection_op);
}

static void bdifference_op(const char *s1, const char *s2, char *to,
                           size_t length)
{
  while (length--)
    *to++ = *s1++ & ~*s2++;
}

TYPEDOP(bdifference, 0,
        "`bitset1 `bitset2 -> `bitset3. `bitset3 = `bitset1 - `bitset2",
	2, (struct string *b1, struct string *b2),
	OP_TRIVIAL | OP_LEAF | OP_NOESCAPE, "ss.s")
{
  return bitset_binop(THIS_OP, b1, b2, true, bdifference_op);
}

TYPEDOP(bunionb, "bunion!",
        "`bitset1 `bitset2 -> `bitset1. `bitset1 = `bitset1 U `bitset2",
	2, (struct string *b1, struct string *b2),
	OP_TRIVIAL | OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "ss.s")
{
  return bitset_binop(THIS_OP, b1, b2, false, bunion_op);
}

TYPEDOP(bintersectionb, "bintersection!",
        "`bitset1 `bitset2 -> `bitset1. `bitset1 = `bitset1 /\\ `bitset2",
	2, (struct string *b1, struct string *b2),
	OP_TRIVIAL | OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "ss.s")
{
  return bitset_binop(THIS_OP, b1, b2, false, bintersection_op);
}

TYPEDOP(bdifferenceb, "bdifference!",
        "`bitset1 `bitset2 -> `bitset1. `bitset1 = `bitset1 - `bitset2",
	2, (struct string *b1, struct string *b2),
	OP_TRIVIAL | OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "ss.s")
{
  return bitset_binop(THIS_OP, b1, b2, false, bdifference_op);
}

static void bassign_op(const char *s1, const char *s2, char *to,
                       size_t length)
{
  memcpy(to, s2, length);
}

TYPEDOP(bassignb, "bassign!",
        "`bitset1 `bitset2 -> `bitset1. `bitset1 = `bitset2",
	2, (struct string *b1, struct string *b2),
	OP_TRIVIAL | OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "ss.s")
{
  return bitset_binop(THIS_OP, b1, b2, false, bassign_op);
}

TYPEDOP(bitset_inp, "bitset_in?",
        "`bitset1 `bitset2 -> `b. True if `bitset1 is a subset of `bitset2",
	2, (struct string *b1, struct string *b2),
	OP_TRIVIAL | OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST, "ss.n")
{
  CHECK_TYPES(b1, string, b2, string);
  long l = string_len(b1);
  if (l != string_len(b2))
    RUNTIME_ERROR(error_bad_value, "arguments of different length");

  const char *sb1 = b1->str, *sb2 = b2->str;
  while (l-- >= 0)
    if (*sb1++ & ~*sb2++)
      return makebool(false);

  return makebool(true);
}

TYPEDOP(bitset_eqp, "bitset_eq?",
        "`bitset1 `bitset2 -> `b. True if `bitset1 == `bitset2",
	2, (struct string *b1, struct string *b2),
	OP_TRIVIAL | OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST, "ss.n")
{
  CHECK_TYPES(b1, string, b2, string);
  long l = string_len(b1);
  if (l != string_len(b2))
    RUNTIME_ERROR(error_bad_value, "arguments of different length");
  return makebool(memcmp(b1->str, b2->str, l) == 0);
}

TYPEDOP(bemptyp, "bempty?",
        "`bitset -> `b. True if `bitset has all bits clear",
	1, (struct string *b),
	OP_TRIVIAL | OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST, "s.n")
{
  CHECK_TYPES(b, string);
  long l = string_len(b);
  const char *sb = b->str;
  while (l-- > 0)
    if (*sb++) return makebool(false);

  return makebool(true);
}

TYPEDOP(bcount, 0, "`bitset -> `n. Returns the number of bits set in `bitset",
	1, (struct string *b),
	OP_TRIVIAL | OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST, "s.n")
{
  CHECK_TYPES(b, string);

  long l = string_len(b);
  const unsigned char *sb = (const unsigned char *)b->str;
  int n = 0;
  while (l >= sizeof (unsigned long))
    {
      n += popcountl(*(unsigned long *)sb);
      l -= sizeof (unsigned long);
      sb += sizeof (unsigned long);
    }
  if (l > 0)
    {
      unsigned long u = 0;
      do
        u = (u << CHAR_BIT) | *sb++;
      while (--l > 0);
      n += popcountl(u);
    }
  return makeint(n);
}

void bitset_init(void)
{
  DEFINE(new_bitset);
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
