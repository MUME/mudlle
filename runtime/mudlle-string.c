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

#include <ctype.h>
#include <errno.h>
#include <fnmatch.h>
#include <iconv.h>
#include <locale.h>
#include <stdlib.h>
#include <string.h>

#include "../call.h"
#include "../charset.h"
#include "../print.h"

#include "check-types.h"
#include "prims.h"
#include "mudlle-string.h"


#ifdef USE_PCRE
#  ifdef HAVE_PCRE_PCRE_H
#    include <pcre/pcre.h>
#  elif HAVE_PCRE_H
#    include <pcre.h>
#  else
#    error "Do not know where to find pcre.h"
#  endif
#endif

#ifdef HAVE_CRYPT_H
#  include <crypt.h>
#elif defined HAVE_CRYPT
#  include <unistd.h>
#endif

#ifdef USE_PCRE
/* these flags unfortunately overlap with some of PCRE's normal flags */
#define PCRE_7BIT    ((MAX_TAGGED_INT >> 1) + 1) /* highest bit */
#define PCRE_INDICES (PCRE_7BIT >> 1)            /* second highest bit */
#define PCRE_BOOLEAN (PCRE_7BIT >> 2)            /* third highest bit */
#endif

struct mregexp {
  struct mprivate p;
  struct string *re;
};

static bool is_regexp(value _re)
{
  struct mregexp *re = _re;
  return (TYPE(re, private)
          && re->p.ptype == makeint(PRIVATE_REGEXP));
}

enum runtime_error ct_string_index(long idx, const char **errmsg,
                                   struct string *str, bool beyond,
                                   long *dst)
{
  if (idx < 0)
    idx += string_len(str);
  if (idx < 0 || idx >= string_len(str) + beyond)
    {
      *errmsg = "string index out of range";
      return error_bad_index;
    }
  *dst = idx;
  return error_none;
}

TYPEDOP(stringp, "string?", "`x -> `b. TRUE if `x is a string", 1, (value v),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_STR_READONLY, "x.n")
{
  return makebool(TYPE(v, string));
}

TYPEDOP(make_string, 0, "`n -> `s. Create an all-zero string of length `n,"
        " where 0 <= `n <= `MAX_STRING_SIZE.",
        1, (value msize), OP_LEAF | OP_NOESCAPE, "n.s")
{
  int size = GETRANGE(msize, 0, MAX_STRING_SIZE);
  struct string *newp = alloc_empty_string(size);
  /* alloc_empty_string() doesn't zero its data,
   * and we don't want to pass sensitive info to
   * someone on accident, so let's zero away... */
  memset(newp->str, 0, size);
  return newp;
}

TYPEDOP(string_length, 0, "`s -> `n. Return length of string",
        1, (struct string *str),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_STR_READONLY, "s.n")
{
  CHECK_TYPES(str, string);
  return makeint(string_len(str));
}

#define DEF_MEM_TRANS(name, op)                         \
static void name(char *dst, const char *src, size_t l)  \
{                                                       \
  for (; l; --l, ++src, ++dst)                          \
    *dst = op(*src);                                    \
}                                                       \
static void name(char *dst, const char *src, size_t l)

DEF_MEM_TRANS(mem8lwr, TO_8LOWER);
DEF_MEM_TRANS(mem8upr, TO_8UPPER);
DEF_MEM_TRANS(mem7prt, TO_7PRINT);

static value string_translate(struct string *src,
                              void (*f)(char *, const char *, size_t),
                              const struct prim_op *op)
{
  CHECK_TYPES_OP(op, src, string);
  long l = string_len(src);

  GCPRO(src);
  struct string *dst = alloc_empty_string(l);
  UNGCPRO();

  f(dst->str, src->str, l);
  return dst;
}

TYPEDOP(string_downcase, 0, "`s0 -> `s1. Returns a copy of `s0 with all"
        " characters lower case",
	1, (struct string *s),
	OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "s.s")
{
  return string_translate(s, mem8lwr, THIS_OP);
}

TYPEDOP(string_upcase, 0, "`s0 -> `s1. Returns a copy of `s0 with all"
        " characters upper case",
	1, (struct string *s),
	OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "s.s")
{
  return string_translate(s, mem8upr, THIS_OP);
}

TYPEDOP(string_7bit, 0, "`s0 -> `s1. Returns a copy of `s0 with all characters"
        " converted to printable 7 bit form",
	1, (struct string *s),
	OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "s.s")
{
  return string_translate(s, mem7prt, THIS_OP);
}


static value string_span(const char *start, const char *end,
                         const char *accept, long alen)
{
  const char *str = start;
  if (alen == 1)
    {
      char c = *accept;
      for (; str < end; ++str)
        if (*str != c)
          break;
    }
  else
    for (; str < end; ++str)
      if (memchr(accept, *str, alen) == NULL)
        break;
  return makeint(str - start);
}

static value string_cspan(const char *start, const char *end,
                          const char *reject, long rlen)
{
  const char *str = start;
  if (rlen == 0)
    ;
  else if (rlen == 1)
    {
      char c = *reject;
      for (; str < end; ++str)
        if (*str == c)
          break;
    }
  else
    for (; str < end; ++str)
      if (memchr(reject, *str, rlen) != NULL)
        break;
  return makeint(str - start);
}

static value string_xspan(struct string *s, value mofs, value mmax,
                          struct string *filter,
                          value (*func)(const char *, const char *,
                                        const char *, long),
                          const struct prim_op *op)
{
  long ofs, max;
  CHECK_TYPES_OP(op,
                 s,      string,
                 mofs,   CT_STR_IDX(ofs, s, true),
                 mmax,   CT_RANGE(max, 0, LONG_MAX),
                 filter, string);
  const char *start = s->str + ofs;
  long slen = string_len(s);
  max += ofs;
  if (max > slen)
    max = slen;
  const char *end = s->str + max;
  return func(start, end, filter->str, string_len(filter));
}

TYPEDOP(string_span, 0, "`s0 `n0 `n1 `s1 -> `n2. Returns the number of"
        " characters, but at most `n1, in string `s0, starting at offset `n0,"
        " that consist entirely of characters in `s1.\n"
        "See also `string_cspan().",
        4, (struct string *s, value mofs, value mmax, struct string *accept),
        OP_LEAF | OP_NOESCAPE | OP_STR_READONLY | OP_CONST, "snns.n")
{
  return string_xspan(s, mofs, mmax, accept, string_span, THIS_OP);
}

TYPEDOP(string_cspan, 0, "`s0 `n0 `n1 `s1 -> `n2. Returns the number of"
        " characters, at most `n1, in string `s0 starting at offset `n0"
        " that consists entirely of characters not in `s1.\n"
        "See also `string_span().",
        4, (struct string *s, value mofs, value mmax, struct string *reject),
        OP_LEAF | OP_NOESCAPE | OP_STR_READONLY | OP_CONST, "snns.n")
{
  return string_xspan(s, mofs, mmax, reject, string_cspan, THIS_OP);
}

TYPEDOP(sdelete, "sdelete", "`n `s0 -> `s1. Return a copy of `s0 without any"
        " occurrence of character `n.",
        2, (value mch, struct string *src),
        OP_LEAF | OP_NOESCAPE | OP_STR_READONLY | OP_CONST, "ns.s")
{
  unsigned char c;
  CHECK_TYPES(mch, CT_RANGE(c, SCHAR_MIN, UCHAR_MAX),
              src, string);
  long slen = string_len(src);

  long count = 0;
  for (const char *sp = src->str, *const send = sp + slen;
       (sp = memchr(sp, c, send - sp));
       ++sp)
    ++count;

  /* count is now the number of occurrences of c */
  GCPRO(src);
  struct string *dst = alloc_empty_string(slen - count);
  UNGCPRO();

  char *dp = dst->str;
  for (const char *sp = src->str, *const send = sp + slen; sp < send; ++sp)
    {
      unsigned char x = *sp;
      if (x != c)
        *dp++ = x;
    }
  return dst;
}

TYPEDOP(string_fill, "string_fill!", "`s `n -> `s. Set all characters of `s to"
        " character `n",
	2, (struct string *str, value mc),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "sn.1")
{
  unsigned char c;
  CHECK_TYPES(str, string,
              mc,  CT_RANGE(c, SCHAR_MIN, UCHAR_MAX));

  ulong len = string_len(str);
  /* allow readonly for empty string */
  if (len == 0)
    return str;
  if (obj_readonlyp(&str->o))
    RUNTIME_ERROR(error_value_read_only, NULL);
  memset(str->str, c, len);
  return str;
}

TYPEDOP(ascii_to_html, 0, "`s0 -> `s0|`s1. Escapes HTML special characters."
        " May return either `s0, if `s0 doesn't contain any special chars,"
        " or a new string.",
	1, (struct string *s),
	OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "s.s")
{
  CHECK_TYPES(s, string);
  long l = string_len(s);
  long want = l;
  const char *s1 = s->str;
  for (int i = 0; i < l; ++i)
    switch (s1[i])
      {
	case '&': /* "&amp;"  */ want += 4; break;
	case '>': /* "&gt;"   */
	case '<': /* "&lt;"   */ want += 3; break;
	case '"': /* "&quot;" */ want += 5; break;
      }
  if (want == l)
    return s;

  if (want > MAX_STRING_SIZE)
    runtime_error(error_bad_value);

  GCPRO(s);
  struct string *newp = alloc_empty_string(want);
  UNGCPRO();

  s1 = s->str;
  char *s2 = newp->str;
  for (int i = 0; i < l; ++i)
    switch (s1[i])
      {
	case '&': memcpy(s2, "&amp;", 5);  s2 += 5; break;
	case '>': memcpy(s2, "&gt;", 4);   s2 += 4; break;
	case '<': memcpy(s2, "&lt;", 4);   s2 += 4; break;
	case '"': memcpy(s2, "&quot;", 6); s2 += 6; break;
	default: *s2++ = s1[i]; break;
      }
  assert(s2 == newp->str + want);

  return newp;
}

#ifndef NBSP
#  define NBSP "\240"
#endif

TYPEDOP(string_from_utf8, 0, "`s0 `n -> `s1. Returns the UTF-8 string `s0"
        " converted to an ISO" NBSP "8859-1 string. `n controls how conversion"
        " errors are handled:\n"
        "   0  \tcharacters that cannot be represented in ISO" NBSP "8859-1"
        " and incorrect UTF-8 codes cause a runtime error\n"
        "   1  \tcharacters that cannot be represented are translitterated"
        " if possible; incorrect codes cause a runtime error\n"
        "   2  \tcharacters that cannot be represented are translitterated"
        " if possible; incorrect codes are skipped\n"
        "   3  \tcharacters that cannot be represented and incorrect"
        " codes are skipped",
        2, (struct string *s, value mmode),
	OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "sn.s")
{
  int mode;
  CHECK_TYPES(s,     string,
              mmode, CT_RANGE(mode, 0, 3));
  const char *toenc = NULL;
  switch (mode)
    {
    case 0: toenc = "ISO-8859-1"; break;
    case 1: case 2: toenc = "ISO-8859-1//TRANSLIT"; break;
    case 3: toenc = "ISO-8859-1//IGNORE"; break;
    }
  if (toenc == NULL)
    runtime_error(error_bad_value);

  iconv_t cd = iconv_open(toenc, "UTF-8");
  if (cd == (iconv_t)-1)
    RUNTIME_ERROR(error_bad_value, NULL);

  struct oport *op = NULL;
  GCPRO(op, s);

  struct string *result;

  size_t inpos = 0;
  size_t inlen = string_len(s);
  for (;;)
    {
      char buf[4096];
      size_t r, oused;
      {
        char *instr = s->str + inpos;
        size_t olen = sizeof buf;
        char *ostr = buf;
        r = iconv(cd, &instr, &inlen, &ostr, &olen);
        oused = ostr - buf;
        inpos = instr - s->str;
      }
      if (r == (size_t)-1 && errno != E2BIG && inlen > 0)
        {
          if (mode >= 2)
            {
              --inlen;
              ++inpos;
            }
          else
            {
              iconv_close(cd);
              runtime_error(error_bad_value);
            }
        }
      if (op == NULL)
        {
          if (inlen == 0)
            {
              /* common (?) case; everything converted in one go */
              result = alloc_string_length(buf, oused);
              break;
            }
          op = make_string_oport();
        }
      opwrite(op, buf, oused);
      if (inlen == 0)
        {
          result = port_string(op, SIZE_MAX);
          break;
        }
    }
  UNGCPRO();
  iconv_close(cd);
  return result;
}

EXT_TYPEDOP(string_ref, 0, "`s `n1 -> `n2. Return the code (`n2) of the `n1'th"
            " character of `s. Negative `n1 are counted from the end of `s.",
	    2, (struct string *str, value c), (str, c),
	    OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_STR_READONLY, "sn.n")
{
  long idx;
  CHECK_TYPES(str, string,
              c,   CT_STR_IDX(idx, str, false));
  return makeint((unsigned char)str->str[idx]);
}

EXT_TYPEDOP(string_set, "string_set!", "`s `n1 `n2 -> `n3. Set the `n1'th"
            " character of `s to the character whose code is `n2.\n"
            "Negative `n1 are counted from the end of `s.\n"
            "The return value is the actual stored value, which is"
            " `n2 & 255.",
	    3, (struct string *str, value i, value mc), (str, i, mc),
	    OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "snn.n")
{
  long idx;
  long c;
  CHECK_TYPES(str, string,
              i,   CT_STR_IDX(idx, str, false),
              mc,  CT_INT(c));
  if (obj_readonlyp(&str->o))
    RUNTIME_ERROR(error_value_read_only, NULL);
  return makeint((unsigned char)(str->str[idx] = c));
}

static inline int canon_sign(long l)
{
  return l < 0 ? -1 : l > 0;
}

static void check_string_binop(struct string *s1, struct string *s2,
                               const struct prim_op *op)
{
  CHECK_TYPES_OP(op, s1, string, s2, string);
}

#define STRING_CMP(type, desc, canon)                                   \
static value string_n ## type ## cmp(struct string *s1,                 \
                                     struct string *s2, long lmax)      \
{                                                                       \
  long l1 = string_len(s1);                                             \
  if (l1 > lmax) l1 = lmax;                                             \
  long l2 = string_len(s2);                                             \
  if (l2 > lmax) l2 = lmax;                                             \
  const char *t1 = s1->str;                                             \
  const char *t2 = s2->str;                                             \
                                                                        \
  size_t minlen = l1 < l2 ? l1 : l2;                                    \
  if ((#type)[0] == 0)                                                  \
    {                                                                   \
      long r = memcmp(t1, t2, minlen);                                  \
      if (r == 0)                                                       \
        r = l1 - l2;                                                    \
      return makeint(canon_sign(r));                                    \
    }                                                                   \
                                                                        \
  for (int i = 0; ; ++i, ++t1, ++t2)                                    \
    {                                                                   \
      if (i == minlen) return makeint(canon_sign(l1 - l2));             \
      int diff = canon(*t1) - canon(*t2);                               \
      if (diff) return makeint(diff);                                   \
    }                                                                   \
}                                                                       \
                                                                        \
TYPEDOP(string_n ## type ## cmp, 0, "`s1 `s2 `n0 -> `n1. Compare at"    \
        " most `n0 characters of two strings" desc "."                  \
        " Returns 0 if `s1 = `s2, < 0 if `s1 < `s2"                     \
        " and > 0 if `s1 > `s2",                                        \
	3, (struct string *s1, struct string *s2, value nmax),          \
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE                              \
        | OP_STR_READONLY | OP_CONST,                                   \
        "ssn.n")                                                        \
{                                                                       \
  long lmax;                                                            \
  CHECK_TYPES(s1, string,                                               \
              s2, string,                                               \
              nmax, CT_INT(lmax));                                      \
  return string_n ## type ## cmp(s1, s2, lmax);                         \
}                                                                       \
                                                                        \
TYPEDOP(string_ ## type ## cmp, 0, "`s1 `s2 -> `n. Compare two"         \
        " strings" desc ". Returns 0 if `s1 = `s2, < 0 if `s1 < `s2"    \
        " and > 0 if `s1 > `s2",                                        \
	2, (struct string *s1, struct string *s2),                      \
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE                              \
        | OP_STR_READONLY | OP_CONST,                                   \
        "ss.n")                                                         \
{                                                                       \
  check_string_binop(s1, s2, THIS_OP);                                  \
  return string_n ## type ## cmp(s1, s2, LONG_MAX);                     \
}                                                                       \
                                                                        \
TYPEDOP(string_ ## type ## equalp, "string_" #type "equal?",            \
        "`s1 `s2 -> `b. Return true if `s1 and `s2 are equal" desc ".", \
        2, (struct string *s1, struct string *s2),                      \
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE                              \
        | OP_STR_READONLY | OP_CONST,                                   \
        "ss.n")                                                         \
{                                                                       \
  check_string_binop(s1, s2, THIS_OP);                                  \
  return makebool(string_n ## type ## cmp(s1, s2, LONG_MAX)             \
                  == makeint(0));                                       \
}

STRING_CMP(, , (unsigned char))
STRING_CMP(8i, " ignoring case", TO_8LOWER)
STRING_CMP(i, " ignoring accentuation and case", TO_7LOWER)

static value string_index(struct string *haystack, char needle, long ofs)
{
  const char *str = haystack->str;
  const char *found = memchr(str + ofs, needle, string_len(haystack) - ofs);
  if (found == NULL)
    return makeint(-1);
  return makeint(found - str);
}

TYPEDOP(string_index, 0,
        "`s `n1 -> `n2. Returns the index in `s of the first occurrence"
        " of the character `n1, or -1 if not found.",
        2, (struct string *haystack, value mneedle),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_STR_READONLY | OP_CONST,
        "sn.n")
{
  long needle;
  CHECK_TYPES(haystack, string,
              mneedle,  CT_INT(needle));
  return string_index(haystack, needle, 0);
}

TYPEDOP(string_index_offset, 0,
        "`s `n1 `n2 -> `n3. Returns the index in `s of the first occurrence,"
        " not before index `n2, of the character `n1, or -1 if not found.",
        3, (struct string *haystack, value mneedle, value mofs),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_STR_READONLY | OP_CONST,
        "snn.n")
{
  long ofs, needle;
  CHECK_TYPES(haystack, string,
              mneedle,  CT_INT(needle),
              mofs,     CT_STR_IDX(ofs, haystack, true));
  return string_index(haystack, needle, ofs);
}

/* find distance from the last character to its previous occurrence
   or length of string if none */
static size_t steplen(const char *s, size_t l)
{
  char c = s[l - 1];
  for (size_t step = 1; step < l; ++step)
    if (s[l - 1 - step] == c)
      return step;
  return l;
}

static size_t step8ilen(const char *s, size_t l)
{
  char c = TO_7LOWER(s[l - 1]);
  for (size_t step = 1; step < l; ++step)
    if (TO_7LOWER(s[l - 1 - step]) == c)
      return step;
  return l;
}

static int string_search(struct string *s1, struct string *s2,
                         long ofs,
                         int (*cmpfn)(const void *, const void *, size_t),
                         void *(*chrfn)(const void *, int, size_t),
                         size_t (*stepfn)(const char *, size_t))
{
  ulong l1 = string_len(s1);
  ulong l2 = string_len(s2);

  if (ofs < 0)
    ofs += l1;
  if (ofs < 0 || ofs > l1)
    runtime_error(error_bad_value);
  l1 -= ofs;

  /* Immediate termination conditions */
  if (l2 == 0) return 0;
  if (l2 > l1) return -1;

  const char *t1 = s1->str + ofs;
  const char *t2 = s2->str;

  size_t c2_step = stepfn(t2, l2);
  char lastc2 = t2[l2 - 1];
  size_t i = l2 - 1; /* No point in starting earlier */
  for (;;)
    {
      /* Search for lastc2 in t1 starting at i */
      const char *next_c2 = chrfn(t1 + i, lastc2, l1 - i);
      if (next_c2 == NULL)
        return -1;

      const char *check_start = next_c2 - (l2 - 1);
      if (cmpfn(check_start, t2, l2 - 1) == 0)
        return check_start - t1 + ofs;

      i += c2_step;
      if (i >= l1)
        return -1;
    }
}

int mudlle_string_isearch(struct string *haystack, struct string *needle)
{
  return string_search(haystack, needle, 0, mem8icmp, mem8ichr, step8ilen);
}

static value string_msearch(struct string *s1, struct string *s2,
                            long ofs,
                            int (*cmpfn)(const void *, const void *, size_t),
                            void *(*chrfn)(const void *, int, size_t),
                            size_t (*stepfn)(const char *, size_t))
{
  return makeint(string_search(s1, s2, ofs, cmpfn, chrfn, stepfn));
}

#define DEF_STRING_SEARCH(infix, cmpfn, chrfn, stepfn, doc)             \
TYPEDOP(string_ ## infix ## search, 0,                                  \
        "`s1 `s2 -> `n. Searches in string `s1 for string `s2" doc "."  \
        " Returns the first index in `s1 where `s2 was found,"          \
        " or -1 if not found.",                                         \
	2, (struct string *s1, struct string *s2),                      \
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_STR_READONLY | OP_CONST, \
        "ss.n")                                                         \
{                                                                       \
  CHECK_TYPES(s1, string,                                               \
              s2, string);                                              \
  return string_msearch(s1, s2, 0, cmpfn, chrfn, stepfn);               \
}

DEF_STRING_SEARCH(,  memcmp,   memchr,   steplen, "")
DEF_STRING_SEARCH(i, mem8icmp, mem8ichr, step8ilen,
                  " (case- and accentuation-insensitive)")

TYPEDOP(string_isearch_offset, 0,
        "`s1 `n0 `s2 -> `n1. Searches in string `s1, starting at offset"
        " `n0, for string `s2 (case- and accentuation-insensitive)."
        " Returns the first index in `s1 where `s2 was found,"
        " or -1 if not found.",
	3, (struct string *s1, value mofs, struct string *s2),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_STR_READONLY | OP_CONST,
        "sns.n")
{
  long ofs;
  CHECK_TYPES(s1, string,
              mofs, CT_INT(ofs),
              s2, string);
  return string_msearch(s1, s2, ofs, mem8icmp, mem8ichr, step8ilen);
}

TYPEDOP(substring, 0, "`s1 `n1 `n2 -> `s2. Extract substring of `s starting"
        " at `n1 of length `n2. The first character is numbered 0",
	3, (struct string *s, value start, value length),
	OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "snn.s")
{
  long first, size;
  CHECK_TYPES(s,      string,
              start,  CT_STR_IDX(first, s, true),
              length, CT_RANGE(size, 0, LONG_MAX));
  if (first + size > string_len(s))
    RUNTIME_ERROR(error_bad_index, NULL);

  GCPRO(s);
  struct string *newp = alloc_empty_string(size);
  UNGCPRO();
  memcpy(newp->str, s->str + first, size);

  return newp;
}

bool string_equalp(struct string *a, struct string *b)
{
  assert(TYPE(a, string) && TYPE(b, string));
  long la = string_len(a);
  return la == string_len(b) && memcmp(a->str, b->str, la) == 0;
}

VARTOP(concat_strings, 0, "`s0 `s1 ... -> `s. Returns the concatenated"
       " strings `s0, `s1, ... as a new string.",
       OP_STR_READONLY | OP_LEAF, "s*.s")
{
  long size = 0;
  for (int n = 0; n < nargs; ++n)
    {
      TYPEIS(args->data[n], string);
      size += string_len((struct string *)(args->data[n]));
    }
  if (size > MAX_STRING_SIZE)
    runtime_error(error_bad_value);
  GCPRO(args);
  struct string *res = alloc_empty_string(size);
  UNGCPRO();
  char *dest = res->str;
  for (int n = 0; n < nargs; ++n)
    {
      long l = string_len((struct string *)(args->data[n]));
      memcpy(dest, ((struct string *)(args->data[n]))->str, l);
      dest += l;
    }
  return res;
}

value concat_strings(struct string **strings, int count)
{
  enum runtime_error error = error_bad_type;
  long size = 0;
  for (int n = 0; n < count; ++n)
    {
      if (!TYPE(strings[n], string))
        goto got_error;
      size += string_len(strings[n]);
    }
  if (size > MAX_STRING_SIZE)
    {
      error = error_bad_value;
      goto got_error;
    }

  struct string *res = alloc_empty_string(size);
  char *dest = res->str;
  for (int n = 0; n < count; ++n)
    {
      long l = string_len(strings[n]);
      memcpy(dest, strings[n]->str, l);
      dest += l;
    }
  return res;

 got_error: ;
  struct vector *v = alloc_vector(count);
  memcpy(v->data, strings, count * sizeof *strings);
  primitive_runtime_error(error, &op_concat_strings, 1, v);
}

value string_append(struct string *s1, struct string *s2,
                    const struct prim_op *op)
{
  long l1 = string_len(s1);
  long l2 = string_len(s2);

  long nl = l1 + l2;
  if (nl > MAX_STRING_SIZE)
    primitive_runtime_error(error_bad_value, op, 2, s1, s2);

  GCPRO(s1, s2);
  struct string *newp = alloc_empty_string(nl);
  UNGCPRO();
  memcpy(newp->str, s1->str, l1);
  memcpy(newp->str + l1, s2->str, l2);

  return newp;
}

TYPEDOP(string_append, 0, "`s1 `s2 -> `s. Concatenate `s1 and `s2."
        " The resulting string must not have more than `MAX_STRING_SIZE"
        " characters.",
        2, (struct string *s1, struct string *s2),
        OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "ss.s")
{
  CHECK_TYPES(s1, string,
              s2, string);
  return string_append(s1, s2, THIS_OP);
}

TYPEDOP(split_words, 0, "`s -> `l. Split string `s into a list of"
        " space-separated words.\n"
        "Single- or double-quoted sequences of words are kept together.",
        1, (struct string *s),
        OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "s.l")
{
  CHECK_TYPES(s, string);

  long slen = string_len(s);
  struct list *l = NULL, *last = NULL;
  GCPRO(l, last, s);

  int idx = 0;

  for (;;)
    {
      while (idx < slen && s->str[idx] == ' ')
        ++idx;

      if (idx == slen)
        break;

      const char *endp;
      int end;
      if ((s->str[idx] == '\'' || s->str[idx] == '"') /* quoted words */
          && (endp = memchr(s->str + idx + 1, s->str[idx], slen - idx - 1)))
        end = endp - s->str + 1;
      else
        {
          end = idx + 1;
          while (end < slen && s->str[end] != ' ')
            ++end;
        }

      int len = end - idx;
      struct string *wrd = alloc_empty_string(len);
      memcpy(wrd->str, s->str + idx, len);

      idx = end;

      if (!l)
        l = last = alloc_list(wrd, NULL);
      else
        {
          value v = alloc_list(wrd, NULL);
          last->cdr = v;
          last = v;
        }
    }

  UNGCPRO();

  return l;
}

TYPEDOP(atoi, 0, "`s -> `n|`s. Converts the string `s into an integer.\n"
        "Returns `s if the conversion failed.\n"
        "Handles binary, octal, decimal, and hexadecimal notation.\n"
        "Equivalent to `atoi_base(`s, 0).",
	1, (struct string *s),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_STR_READONLY | OP_CONST,
        "s.[sn]")
{
  CHECK_TYPES(s, string);
  long n;
  if (!mudlle_strtolong(s->str, string_len(s), &n, 0, false))
    return s;
  return makeint(n);
}

TYPEDOP(atoi_base, 0, "`s `n0 -> `n1|`s. Converts the string `s into an"
        " integer.\n"
        "`n0 specifies the base, which must be between 2 and 36 inclusive,"
        " or the special value 0.\n"
        "Any leading whitespace is ignored. Then there may be an optional"
        " sign character (\"+\" or \"-\").\n"
        "For base 0, a following prefix decides the base: \"0x\" for"
        " hexadecimal (also allowed for base 16), \"0\" for octal,"
        " or \"0b\" for binary (also allowed for base 2). Any other digit"
        " means decimal base.\n"
        "Following the optional prefix, the rest of the number is"
        " interpreted according to the (possibly deduced) base.\n"
        "Returns `s if the conversion failed.",
	2, (struct string *s, value mbase),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_STR_READONLY | OP_CONST,
        "sn.[sn]")
{
  long base;
  CHECK_TYPES(s, string,
              mbase, CT_INT(base));
  if (base != 0 && (base < 2 || base > 26))
    RUNTIME_ERROR(error_bad_value, "base must be 0 or [2..25]");

  long n;
  if (!mudlle_strtolong(s->str, string_len(s), &n, base, false))
    return s;
  return makeint(n);
}

TYPEDOP(itoa, 0, "`n -> `s. Converts integer into string", 1, (value mn),
        OP_LEAF | OP_NOESCAPE | OP_CONST, "n.s")
{
  long n;
  CHECK_TYPES(mn, CT_INT(n));

  /* conservative limit: 3 bits per decimal digit, round up, one sign
     character, one zero character */
  char buf[(TAGGED_INT_BITS - 1 + 2) / 3 + 1 + 1];
  int c = sprintf(buf, "%ld", n);
  assert(c < sizeof buf);
  return make_readonly(alloc_string(buf));
}

TYPEDOP(isalpha, "calpha?", "`n -> `b. TRUE if `n is a letter (allowed in"
        " keywords)",
	1, (value n),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST, "n.n")
{
  return makebool(IS_8NAME(GETINT(n)));
}

TYPEDOP(isdigit, "cdigit?", "`n -> `b. TRUE if `n is a digit",
	1, (value n),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST, "n.n")
{
  return makebool(isdigit((unsigned char)GETINT(n)));
}

TYPEDOP(isxdigit, "cxdigit?", "`n -> `b. TRUE if `n is a hexadecimal digit",
	1, (value n),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST, "n.n")
{
  return makebool(isxdigit((unsigned char)GETINT(n)));
}

TYPEDOP(isprint, "cprint?", "`n -> `b. TRUE if `n is a printable character",
        1, (value n),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST, "n.n")
{
  return makebool(IS_8PRINT(GETINT(n)));
}

TYPEDOP(isspace, "cspace?", "`n -> `b. TRUE if `n is a white space character."
        " N.b., returns FALSE for non-breaking space (`NBSP).",
        1, (value n),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST, "n.n")
{
  /* not using IS_8SPACE() so any line-wrapping code that uses
     cspace?() to find potential line breaks keeps working */
  return makebool(isspace((unsigned char)GETINT(n)));
}

TYPEDOP(isupper, "cupper?", "`n -> `b. TRUE if `n is an upper case character",
        1, (value n),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST, "n.n")
{
  unsigned char c = GETINT(n);
  return makebool(TO_8LOWER(c) != c);
}

TYPEDOP(islower, "clower?", "`n -> `b. TRUE if `n is an lower case character",
        1, (value n),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST, "n.n")
{
  unsigned char c = GETINT(n);
  return makebool(TO_8UPPER(c) != c);
}

TYPEDOP(cupper, 0, "`n0 -> `n1. Return `n0's upper case variant", 1, (value n),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST, "n.n")
{
  return makeint(TO_8UPPER(GETINT(n)));
}

TYPEDOP(clower, 0, "`n0 -> `n1. Return `n0's lower case variant", 1, (value n),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST, "n.n")
{
  return makeint(TO_8LOWER(GETINT(n)));
}

TYPEDOP(cicmp, 0, "`n0 `n1 -> `n2. Compare characters `n0 and `n1 as"
        " `string_icmp() does. Returns -1, 0, or 1 if `n0 is less than,"
        " equal, or greater than `n1.",
        2, (value n0, value n1),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST, "nn.n")
{
  return makeint(TO_7LOWER(GETINT(n0)) - TO_7LOWER(GETINT(n1)));
}

TYPEDOP(c7bit, 0, "`n0 -> `n1. Return `n0's 7 bit variant", 1, (value n),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST, "n.n")
{
  return makeint(TO_7PRINT(GETINT(n)));
}



const unsigned char *get_iso88591_pcre_table(void)
{
#ifdef USE_PCRE
  static bool been_here = false;
  static const unsigned char *table;

  if (been_here)
    return table;

  been_here = true;
  char *olocale = strdup(setlocale(LC_ALL, NULL));
  if (setlocale(LC_ALL, "en_US.iso88591") != NULL)
    {
      table = pcre_maketables();
      setlocale(LC_ALL, olocale);
    }
  free(olocale);
  return table;
#else  /* ! USE_PCRE */
  return NULL;
#endif /* ! USE_PCRE */
}

TYPEDOP(is_regexp, "regexp?", "`x -> `b. Returns TRUE if `x is a"
        " regular expression, as created by `make_regexp()",
        1, (value re), OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(is_regexp(re));
}

TYPEDOP(fnmatch, 0, "`s0 `s1 `n -> `b. Returns true if the glob pattern `s0"
        " matches the string `s1 using flags in `n:\n"
        "  \t`FNM_NOESCAPE  \tdo not treat backslash (\\) as an escape"
        " character\n"
        "  \t`FNM_PATHNAME  \tperform a pathname match, where wildcards"
        " (* and ?) only match between slashes\n"
        "  \t`FNM_PERIOD    \tdo not let wildcards match leading periods",
        3, (struct string *pat, struct string *str, value mflags),
        OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_STR_READONLY | OP_CONST,
        "ssn.n")
{
  long flags;
  CHECK_TYPES(pat, string,
              str, string,
              mflags, CT_INT(flags));
  if (flags & ~(FNM_NOESCAPE | FNM_PATHNAME | FNM_PERIOD))
    RUNTIME_ERROR(error_bad_value, "invalid flags");

  /* zero bytes cannot match or be matched */
  if (string_len(pat) != strlen(pat->str)
      || string_len(str) != strlen(str->str))
    return makebool(false);

  return makebool(fnmatch(pat->str, str->str, flags) == 0);
}

#ifdef USE_PCRE

static struct string *regexp_str;

static void *regexp_malloc(size_t s)
{
  /*
   * This assert checks the assumption (which is more or less guaranteed
   * in the documentation of pcre) that pcre_malloc is called at most once
   * once per call to make_regexp.
   */
  assert(!regexp_str);
  regexp_str = (struct string *)allocate_string(type_string, s);
  return regexp_str->str;
}

static void regexp_free(void *p)
{
  /*
   * This assert checks the assumption (which is more or less guaranteed
   * in the documentation of pcre) that pcre_free is called at most once,
   * and only after pcre_malloc has been called.
   */
  assert(TYPE(regexp_str, string) && regexp_str->str == p);
  regexp_str = NULL;
}

TYPEDOP(make_regexp, 0, "`s `n -> `r. Create a matcher for the regular"
        " expression `s with flags `n.\n"
        "Returns cons(`errorstring, `erroroffset) on error.\n"
	"The following flags are supported:\n"
	"  \t`PCRE_7BIT       \tconvert pattern to its 7-bit equivalent\n"
	"  \t`PCRE_ANCHORED   \tforce pattern anchoring\n"
	"  \t`PCRE_CASELESS   \tdo caseless matching\n"
	"  \t`PCRE_DOLLAR_ENDONLY\n"
	"                  \t$ only matches end of string,"
	" and not newline\n"
	"  \t`PCRE_DOTALL     \t. matches anything, including newline\n"
	"  \t`PCRE_EXTENDED   \tignore whitespace and # comments\n"
	"  \t`PCRE_EXTRA      \tuse PCRE extra features"
	" (no idea what that means)\n"
	"  \t`PCRE_MULTILINE  \t^ and $ match at newlines\n"
	"  \t`PCRE_UNGREEDY   \tinvert greedyness of quantifiers",
	2, (struct string *pat, value mflags),
	OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "sn.[ok]")
{
  long flags;
  CHECK_TYPES(pat, string,
              mflags, CT_INT(flags));

  pcre *p;
  const char *errstr;
  int errofs;
  void *(*old_malloc)(size_t) = pcre_malloc;
  void (*old_free)(void *) = pcre_free;

  char *lpat = mudlle_string_dup(pat);
  size_t zpos = strlen(lpat);
  if (zpos != string_len(pat))
    {
      free(lpat);
      STATIC_STRING(bad_nul_str, "invalid NUL character");
      return alloc_list(GET_STATIC_STRING(bad_nul_str), makeint(zpos));
    }

  if (flags & PCRE_7BIT)
    {
      strto7print(lpat);
      flags &= ~PCRE_7BIT;
    }
  regexp_str = NULL;
  GCPRO(regexp_str);

  const unsigned char *table = get_iso88591_pcre_table();
  pcre_malloc = regexp_malloc;
  pcre_free = regexp_free;
  p = pcre_compile(lpat, flags, &errstr, &errofs, table);
  pcre_malloc = old_malloc;
  pcre_free = old_free;

  free(lpat);

  value result;
  if (p == NULL)
    result = alloc_list(alloc_string(errstr), makeint(errofs));
  else
    {
      struct mregexp *mregexp = (struct mregexp *)alloc_private(
        PRIVATE_REGEXP, 1);
      mregexp->re = make_readonly(regexp_str);
      result = make_readonly(mregexp);
      if (!check_immutable(result))
        abort();
    }

  UNGCPRO();
  return result;
}

static const pcre *get_regexp(value x)
{
  if (!is_regexp(x))
    runtime_error_message(error_bad_type, "expected regexp");
  struct mregexp *re = x;
  assert(TYPE(re->re, string));
  return (const pcre *)re->re->str;
}

TYPEDOP(regexp_exec, 0,
        "`r `s `n0 `n1 -> `v|`b. Tries to match the string `s, starting at"
	" character `n0 with regexp matcher `r and flags `n1.\n"
	"Returns `false if no match; otherwise, it returns a vector of"
	" submatches unless `PCRE_BOOLEAN is specified (see below).\n"
	"A submatch is either a string matched by the corresponding"
	" parenthesis group, or null if that group was not used.\n"
	"If `n1 & `PCRE_INDICES, submatches are instead represented as"
	" cons(`start, `length) or null.\n"
        "If `n1 & `PCRE_BOOLEAN, the result is instead a boolean saying"
        " whether a match was found.\n"
        "`PCRE_INDICES and `PCRE_BOOLEAN must not both be set.\n"
	"The following flags are supported:\n"
	"  \t`PCRE_7BIT      \tconvert the haystack to its 7-bit equivalent"
	" before matching\n"
	"  \t`PCRE_ANCHORED  \tmatch only at the first position\n"
        "  \t`PCRE_BOOLEAN   \tsee above\n"
	"  \t`PCRE_INDICES   \tsee above\n"
	"  \t`PCRE_NOTBOL    \t`s is not the beginning of a line\n"
	"  \t`PCRE_NOTEMPTY  \tan empty string is not a valid match\n"
	"  \t`PCRE_NOTEOL    \t`s is not the end of a line",
        4, (value mre, struct string *str, value msofs, value mflags),
	OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "osnn.[vn]")
{
  long flags, sofs;
  CHECK_TYPES(mre,    any,
              str,    string,
              msofs,  CT_STR_IDX(sofs, str, true),
              mflags, CT_INT(flags));
  const pcre *re = get_regexp(mre);

  int nsub;
  /* this should probably be an assertion */
  if (pcre_fullinfo(re, NULL, PCRE_INFO_CAPTURECOUNT, &nsub) != 0)
    runtime_error(error_bad_value);
  int olen = (nsub + 1) * 3;
  int ovec[olen];

  bool indices = flags & PCRE_INDICES;
  bool boolean = flags & PCRE_BOOLEAN;
  flags &= ~(PCRE_INDICES | PCRE_BOOLEAN);
  if (indices && boolean)
    runtime_error(error_bad_value);

  {
    const char *haystack = str->str;
    size_t slen = string_len(str);
    char *lstr = NULL;

    if (flags & PCRE_7BIT)
      {
        lstr = malloc(slen + 1);
        for (size_t idx = 0; idx <= slen; ++idx)
          lstr[idx] = TO_7PRINT(haystack[idx]);
        haystack = lstr;
        flags &= ~PCRE_7BIT;
      }

    /* n.b., requires reasonably new libpcre (older ones did not have
       the 'sofs' parameter) */
    int res = pcre_exec(re, NULL, haystack, slen, sofs, flags, ovec, olen);

    free(lstr);

    if (res == PCRE_ERROR_NOMATCH)
      return makebool(false);
    if (res < 0)
      runtime_error(error_bad_value);
    if (boolean)
      return makebool(true);
  }

  struct vector *v = NULL;
  GCPRO(v, str);
  v = alloc_vector(nsub + 1);

  for (int i = 0; i <= nsub; ++i)
    {
      int st = ovec[i * 2];
      if (st >= 0)
	{
	  int ln = ovec[i * 2 + 1] - st;
          if (indices)
            SET_VECTOR(v, i, alloc_list(makeint(st), makeint(ln)));
	  else
            {
              struct string *tmp = alloc_empty_string(ln);
              memcpy(tmp->str, str->str + st, ln);
              v->data[i] = tmp;
            }
	}
    }

  UNGCPRO();

  return v;
}

#endif /* USE_PCRE */

#ifdef HAVE_CRYPT
TYPEDOP(crypt, 0, "`s1 `s2 -> `s3. Encrypt `s1 using `s2 as salt",
	2, (struct string *s, struct string *salt),
	OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "ss.s")
{
  CHECK_TYPES(s,    string,
              salt, string);

  /* Use all the characters in the string, rather than the first 8. */
  char buffer[9] = { 0 };
  const char *p = s->str;
  for (int i = 0; *p; ++p, i = (i + 1) & 7)
    buffer[i] += *p;

  for (int i = 0; i < 8; ++i)
    if (!buffer[i])
      buffer[i] = ' ';

  return alloc_string(crypt(buffer, salt->str));
}
#endif	/* HAVE_CRYPT */

static void define_strings(void)
{
  const char host_type[] =
#ifdef __i386__
    "i386"
#elif defined __x86_64__
    "x86_64"
#else
#  error Fix me
#endif
    ;
  system_define("host_type", make_readonly(alloc_string(host_type)));
}

STATIC_STRING(static_obj_empty_string, "");
struct string *const static_empty_string
  = GET_STATIC_STRING(static_obj_empty_string);

void string_init(void)
{
  DEFINE(stringp);
  DEFINE(make_string);
  DEFINE(string_length);
  DEFINE(string_fill);
  DEFINE(string_ref);
  DEFINE(string_set);
  DEFINE(string_cmp);
  DEFINE(string_icmp);
  DEFINE(string_8icmp);
  DEFINE(string_ncmp);
  DEFINE(string_nicmp);
  DEFINE(string_n8icmp);
  DEFINE(string_equalp);
  DEFINE(string_iequalp);
  DEFINE(string_8iequalp);
  DEFINE(string_search);
  DEFINE(string_isearch);
  DEFINE(string_isearch_offset);
  DEFINE(string_index);
  DEFINE(string_index_offset);
  DEFINE(string_span);
  DEFINE(string_cspan);
  DEFINE(sdelete);
  DEFINE(substring);
  DEFINE(concat_strings);
  DEFINE(string_append);
  DEFINE(split_words);
  DEFINE(itoa);
  DEFINE(atoi);
  DEFINE(atoi_base);
  DEFINE(string_upcase);
  DEFINE(string_downcase);
  DEFINE(string_7bit);
  DEFINE(ascii_to_html);
  DEFINE(string_from_utf8);
  DEFINE(isalpha);
  DEFINE(isupper);
  DEFINE(islower);
  DEFINE(isdigit);
  DEFINE(isxdigit);
  DEFINE(isprint);
  DEFINE(isspace);
  DEFINE(cupper);
  DEFINE(clower);
  DEFINE(cicmp);
  DEFINE(c7bit);

  define_strings();

#define def(x) system_define(#x, makeint(x))
  DEFINE(fnmatch);
  def(FNM_NOESCAPE);
  def(FNM_PATHNAME);
  def(FNM_PERIOD);

  DEFINE(is_regexp);

#ifdef USE_PCRE

  DEFINE(make_regexp);
  DEFINE(regexp_exec);

  /* PCRE options */
  def(PCRE_7BIT);
  def(PCRE_ANCHORED);
  def(PCRE_BOOLEAN);
  def(PCRE_CASELESS);
  def(PCRE_DOLLAR_ENDONLY);
  def(PCRE_DOTALL);
  def(PCRE_EXTENDED);
  def(PCRE_EXTRA);
  def(PCRE_INDICES);
  def(PCRE_MULTILINE);
  def(PCRE_NOTBOL);
  def(PCRE_NOTEMPTY);
  def(PCRE_NOTEOL);
  def(PCRE_UNGREEDY);
#endif  /* USE_PCRE */

#ifdef HAVE_CRYPT
  DEFINE(crypt);
#endif

  system_define("MAX_STRING_SIZE", makeint(MAX_STRING_SIZE));
}
