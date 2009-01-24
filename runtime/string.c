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

#include <ctype.h>
#include <errno.h>
#include <iconv.h>
#include <locale.h>
#include <stdlib.h>
#include <string.h>

#include "runtime/runtime.h"
#include "stringops.h"
#include "print.h"
#include "charset.h"
#include "call.h"


#ifdef USE_PCRE
#  ifdef HAVE_PCRE_PCRE_H
#    include <pcre/pcre.h>
#  elif HAVE_PCRE_H
#    include <pcre.h>
#  else
#    error "Do not know where to find pcre.h"
#  endif
#endif

#if HAVE_CRYPT_H
#include <crypt.h>
#endif

#define PCRE_7BIT    ((MAX_TAGGED_INT >> 1) + 1) /* highest bit */
#define PCRE_INDICES (PCRE_7BIT >> 1)            /* second highest bit */

bool is_regexp(value _re)
{
  struct grecord *re = _re;
  return (TYPE(re, type_private)
          && re->data[0] == makeint(PRIVATE_REGEXP));
}

TYPEDOP(stringp, "string?", "`x -> `b. TRUE if `x is a string", 1, (value v),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(TYPE(v, type_string));
}

TYPEDOP(make_string, 0, "`n -> `s. Create a string of length `n",
        1, (value msize), OP_LEAF | OP_NOESCAPE, "n.s")
{
  int size = GETINT(msize);
  if (size < 0)
    runtime_error(error_bad_value);
  struct string *newp = alloc_empty_string(size);
  return newp;
}

TYPEDOP(string_length, 0, "`s -> `n. Return length of string",
        1, (struct string *str),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "s.n")
{
  TYPEIS(str, type_string);
  return makeint(string_len(str));
}

TYPEDOP(string_downcase, 0, "`s0 -> `s1. Returns a copy of `s0 with all"
        " characters lower case",
	1, (struct string *s),
	OP_LEAF | OP_NOESCAPE, "s.s")
{
  struct gcpro gcpro1;

  TYPEIS(s, type_string);
  GCPRO1(s);
  long l = string_len(s);
  struct string *newp = alloc_empty_string(l);
  UNGCPRO();

  char *s1 = s->str, *s2 = newp->str;
  while (l--)
    s2[l] = TO_8LOWER(s1[l]);

  return newp;
}  

TYPEDOP(string_upcase, 0, "`s0 -> `s1. Returns a copy of `s0 with all"
        " characters upper case",
	1, (struct string *s),
	OP_LEAF | OP_NOESCAPE, "s.s")
{
  struct gcpro gcpro1;

  TYPEIS(s, type_string);
  GCPRO1(s);
  long l = string_len(s);
  struct string *newp = alloc_empty_string(l);
  UNGCPRO();

  char *s1 = s->str, *s2 = newp->str;
  while (l--)
    s2[l] = TO_8UPPER(s1[l]);

  return newp;
}  


TYPEDOP(string_fill, "string_fill!", "`s `n -> `s. Set all characters of `s to"
        " character `n",
	2, (struct string *str, value c),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "sn.1")
{
  TYPEIS(str, type_string);
  memset(str->str, GETINT(c), string_len(str));
  return str;
}

TYPEDOP(string_7bit, 0, "`s0 -> `s1. Returns a copy of `s0 with all characters"
        " converted to printable 7 bit form",
	1, (struct string *s),
	OP_LEAF | OP_NOESCAPE, "s.s")
{
  struct gcpro gcpro1;

  TYPEIS(s, type_string);
  GCPRO1(s);
  long l = string_len(s);
  struct string *newp = alloc_empty_string(l);
  UNGCPRO();

  char *s1 = s->str, *s2 = newp->str;
  while (l--)
    s2[l] = TO_7PRINT(s1[l]);

  return newp;
}  

#ifndef NBSP
#  define NBSP "\240"
#endif

TYPEDOP(string_from_utf8, 0, "`s0 `n -> `s1. Returns the UTF-8 string `s0"
        " converted to an ISO" NBSP "8859-1 string. `n controls how conversion"
        " errors are handled:\r\n"
        "   0  \tcharacters that cannot be represented in ISO" NBSP "8859-1"
        " and incorrect UTF-8 codes cause a runtime error\r\n"
        "   1  \tcharacters that cannot be represented are translitterated"
        " if possible; incorrect codes cause a runtime error\r\n"
        "   2  \tcharacters that cannot be represented are translitterated"
        " if possible; incorrect codes are skipped\r\n"
        "   3  \tcharacters that cannot be represented and incorrect"
        " codes are skipped",
        2, (struct string *s, value mmode),
	OP_LEAF | OP_NOESCAPE, "sx.s")
{
  TYPEIS(s, type_string);

  int mode = GETINT(mmode);
  const char *toenc = NULL;
  switch (mode) {
  case 0: toenc = "ISO-8859-1"; break;
  case 1: case 2: toenc = "ISO-8859-1//TRANSLIT"; break;
  case 3: toenc = "ISO-8859-1//IGNORE"; break;
  }
  if (toenc == NULL)
    runtime_error(error_bad_value);

  char *localstr;
  LOCALSTR(localstr, s);

  iconv_t cd = iconv_open(toenc, "UTF-8");
  if (cd == (iconv_t)-1)
    runtime_error(error_bad_value);

  struct gcpro gcpro1;
  struct oport *op = NULL;
  GCPRO1(op);

  struct string *result;

  size_t inlen = string_len(s);
  for (;;) {
    char buf[4096], *ostr = buf;
    size_t olen = sizeof buf;
    size_t r = iconv(cd, &localstr, &inlen, &ostr, &olen);
    if (r == (size_t)-1 && errno != E2BIG && inlen > 0) {
      if (mode >= 2) {
        --inlen;
        ++localstr;
      } else {
        iconv_close(cd);
        runtime_error(error_bad_value);
      }
    }
    if (op == NULL) {
      if (inlen == 0) {
        /* common (?) case; everything converted in one go */
        result = alloc_string_length(buf, ostr - buf);
        break;
      }
      op = make_string_oport();
    }
    opwrite(op, buf, ostr - buf);
    if (inlen == 0) {
      result = port_string(op);
      break;
    }
  }
  UNGCPRO();
  iconv_close(cd);
  return result;
}  

EXT_TYPEDOP(string_ref, 0, "`s `n1 -> `n2. Return the code (`n2) of the `n1'th"
            " character of `s",
	    2, (struct string *str, value c),
	    OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "sn.n")
{
  TYPEIS(str, type_string);
  long idx = GETINT(c);
  if (idx < 0)
    idx += string_len(str);
  if (idx < 0 || idx >= string_len(str)) runtime_error(error_bad_index);
  return makeint((unsigned char)str->str[idx]);
}

EXT_TYPEDOP(string_set, "string_set!", "`s `n1 `n2 -> `n2. Set the `n1'th"
            " character of `s to the character whose code is `n2",
	    3, (struct string *str, value i, value mc),
	    OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "snn.n")
{
  TYPEIS(str, type_string);
  if (str->o.flags & OBJ_READONLY) runtime_error(error_value_read_only);
  long idx = GETINT(i);
  long c = GETINT(mc);
  if (idx < 0)
    idx += string_len(str);
  if (idx < 0 || idx >= string_len(str)) runtime_error(error_bad_index);

  return makeint((unsigned char)(str->str[idx] = c));
}

TYPEDOP(string_cmp, 0, "`s1 `s2 -> `n. Compare 2 strings."
        " Returns 0 if `s1 = `s2, < 0 if `s1 < `s2 and > 0 if `s1 > `s2",
	2, (struct string *s1, struct string *s2),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "ss.n")
{
  TYPEIS(s1, type_string);
  TYPEIS(s2, type_string);

  long l1 = string_len(s1);
  long l2 = string_len(s2);
  const char *t1 = s1->str;
  const char *t2 = s2->str;
  
  int i = 0;
  int res;
  do {
    if (i == l1) { res = i - l2; break; }
    if (i == l2) { res = 1; break; }
    if ((res = *t1++ - *t2++))
      break;
    i++;
  } while (1);
  return makeint(res);
}

TYPEDOP(string_8icmp, 0, "`s1 `s2 -> `n. Compare 2 strings ignoring case."
        " Returns 0 if `s1 = `s2, < 0 if `s1 < `s2 and > 0 if `s1 > `s2",
	2, (struct string *s1, struct string *s2),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "ss.n")
{
  TYPEIS(s1, type_string);
  TYPEIS(s2, type_string);

  long l1 = string_len(s1);
  long l2 = string_len(s2);
  const char *t1 = s1->str;
  const char *t2 = s2->str;
  
  int i = 0;
  int res;
  do {
    if (i == l1) { res = i - l2; break; }
    if (i == l2) { res = 1; break; }
    if ((res = TO_8LOWER(*t1) - TO_8LOWER(*t2)))
      break;
    t1++; t2++; i++;
  } while (1);
  return makeint(res);
}

TYPEDOP(string_icmp, 0, "`s1 `s2 -> `n. Compare 2 strings ignoring accentuation"
        " and case. Returns 0 if `s1 = `s2, < 0 if `s1 < `s2 and > 0"
        " if `s1 > `s2",
	2, (struct string *s1, struct string *s2),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "ss.n")
{
  ulong l1, l2, i;
  char *t1, *t2;
  int res;

  TYPEIS(s1, type_string);
  TYPEIS(s2, type_string);

  l1 = string_len(s1);
  l2 = string_len(s2);
  t1 = s1->str;
  t2 = s2->str;
  
  i = 0;
  do {
    if (i == l1) { res = i - l2; break; }
    if (i == l2) { res = 1; break; }
    if ((res = TO_7LOWER(*t1) - TO_7LOWER(*t2)))
      break;
    t1++; t2++; i++;
  } while (1);
  return makeint(res);
}

TYPEDOP(string_search, 0,
        "`s1 `s2 -> `n. Searches in string `s1 for string `s2."
        " Returns -1 if not found, index of first matching character"
        " otherwise.",
	2, (struct string *s1, struct string *s2),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "ss.n")
{
  ulong l1, l2, i, j, i1;
  char *t1, *t2, lastc2;

  TYPEIS(s1, type_string);
  TYPEIS(s2, type_string);

  l1 = string_len(s1);
  l2 = string_len(s2);

  /* Immediate termination conditions */
  if (l2 == 0) return makeint(0);
  if (l2 > l1) return makeint(-1);

  t1 = s1->str;
  t2 = s2->str;
  lastc2 = t2[l2 - 1];
  
  i = l2 - 1; /* No point in starting earlier */
  for (;;)
    {
      /* Search for lastc2 in t1 starting at i */
      while (t1[i] != lastc2)
	if (++i == l1) return makeint(-1);

      /* Check if rest of string matches */
      j = l2 - 1;
      i1 = i;
      do
	if (j == 0) return makeint(i1); /* match found at i1 */
      while (t2[--j] == t1[--i1]);

      /* No match. If we wanted better efficiency, we could skip over
	 more than one character here (depending on where the next to
	 last 'lastc2' is in s2.
	 Probably not worth the bother for short strings */
      if (++i == l1) return makeint(-1); /* Might be end of s1 */
    }
}

TYPEDOP(string_isearch, 0,
        "`s1 `s2 -> `n. Searches in string `s1 for string `s2"
        " (case- and accentuation-insensitive)."
        " Returns -1 if not found, index of first matching character"
        " otherwise.",
	2, (struct string *s1, struct string *s2),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "ss.n")
{
  ulong l1, l2, i, j, i1;
  char *t1, *t2, lastc2;

  TYPEIS(s1, type_string);
  TYPEIS(s2, type_string);

  l1 = string_len(s1);
  l2 = string_len(s2);

  /* Immediate termination conditions */
  if (l2 == 0) return makeint(0);
  if (l2 > l1) return makeint(-1);

  t1 = s1->str;
  t2 = s2->str;
  lastc2 = TO_7LOWER(t2[l2 - 1]);
  
  i = l2 - 1; /* No point in starting earlier */
  for (;;)
    {
      /* Search for lastc2 in t1 starting at i */
      while (TO_7LOWER(t1[i]) != lastc2)
	if (++i == l1) return makeint(-1);

      /* Check if rest of string matches */
      j = l2 - 1;
      i1 = i;
      do
	{
	  if (j == 0) return makeint(i1); /* match found at i1 */
	  --j; --i1;
	}
      while (TO_7LOWER(t2[j]) == TO_7LOWER(t1[i1]));

      /* No match. If we wanted better efficiency, we could skip over
	 more than one character here (depending on where the next to
	 last 'lastc2' is in s2.
	 Probably not worth the bother for short strings */
      if (++i == l1) return makeint(-1); /* Might be end of s1 */
    }
}

TYPEDOP(substring, 0, "`s1 `n1 `n2 -> `s2. Extract substring of `s starting"
        " at `n1 of length `n2. The first character is numbered 0",
	3, (struct string *s, value start, value length),
	OP_LEAF | OP_NOESCAPE, "snn.s")
{
  TYPEIS(s, type_string);

  long first = GETINT(start);
  if (first < 0)
    first += string_len(s);
  long size = GETINT(length);
  if (first < 0 || size < 0 || first + size > string_len(s))
    runtime_error(error_bad_index);

  struct gcpro gcpro1;
  GCPRO1(s);
  struct string *newp = alloc_empty_string(size);
  memcpy(newp->str, s->str + first, size);
  UNGCPRO();

  return newp;
}

value string_append(struct string *s1, struct string *s2)
{
  struct gcpro gcpro1, gcpro2;
  GCPRO2(s1, s2);

  long l1 = string_len(s1);
  long l2 = string_len(s2);

  struct string *newp = alloc_empty_string(l1 + l2);
  memcpy(newp->str, s1->str, l1);
  memcpy(newp->str + l1, s2->str, l2);
  UNGCPRO();

  return newp;
}

EXT_TYPEDOP(string_append, 0, "`s1 `s2 -> `s. Concatenate `s1 and `s2",
	    2, (struct string *s1, struct string *s2),
	    OP_LEAF | OP_NOESCAPE, "ss.s")
{
  TYPEIS(s1, type_string);
  TYPEIS(s2, type_string);
  return string_append(s1, s2);
}

TYPEDOP(split_words, 0, "`s -> `l. Split string `s into words in list `l",
	  1, (struct string *s),
	  OP_LEAF | OP_NOESCAPE, "s.l")
{
  struct list *l = NULL, *last = NULL;
  struct string *wrd;
  char missing, *endp;
  struct gcpro gcpro1, gcpro2, gcpro3;
  int idx;

  TYPEIS(s, type_string);

  GCPRO3(l, last, s);

  idx = 0;

  for (;;) {
    int end;

    while (s->str[idx] == ' ')
      ++idx;

    missing = 0;
    if ((s->str[idx] == '\'' || s->str[idx] == '"') /* quoted words */
        && (endp = strchr(s->str + idx + 1, s->str[idx])))
        end = endp - s->str + 1;
    else
      {
        end = idx;
        while (s->str[end] && s->str[end] != ' ')
          ++end;
    
        if (end == idx)
          break;
      }

    int len = end - idx + (missing != 0);
    wrd = alloc_empty_string(len);
    memcpy(wrd->str, s->str + idx, len);
    if (missing)
      wrd->str[len - 1] = missing;
    
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

TYPEDOP(atoi, 0, "`s -> `n|`s. Converts string into integer. Returns `s if"
        " conversion failed",
	1, (struct string *s),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "s.S")
{
  int n;

  TYPEIS(s, type_string);
  if (!mudlle_strtoint(s->str, &n))
    return s;
  else
    return makeint(n);
}

TYPEDOP(itoa, 0, "`n -> `s. Converts integer into string", 1, (value n),
	  OP_LEAF | OP_NOESCAPE, "n.s")
{
  char buf[16];

  sprintf(buf, "%ld", GETINT(n));
  return alloc_string(buf);
}

TYPEDOP(isalpha, "calpha?", "`n -> `b. TRUE if `n is a letter (allowed in"
        " keywords)",
	1, (value n),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "n.n")
{
  return makebool(IS_8NAME(GETINT(n)));
}

TYPEDOP(isdigit, "cdigit?", "`n -> `b. TRUE if `n is a digit",
	1, (value n),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "n.n")
{
  return makebool(isdigit((unsigned char)GETINT(n)));
}

TYPEDOP(isxdigit, "cxdigit?", "`n -> `b. TRUE if `n is a hexadecimal digit",
	1, (value n),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "n.n")
{
  return makebool(isxdigit((unsigned char)GETINT(n)));
}

TYPEDOP(isprint, "cprint?", "`n -> `b. TRUE if `n is a printable character",
        1, (value n),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "n.n")
{
  return makebool(IS_8PRINT(GETINT(n)));
}

TYPEDOP(isspace, "cspace?", "`n -> `b. TRUE if `n is a white space character."
        " N.b., returns FALSE for non-breaking space (`NBSP).",
        1, (value n),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "n.n")
{
  /* not using IS_8SPACE() so any line-wrapping code that uses
     cspace?() to find potential line breaks keeps working */
  return makebool(isspace((unsigned char)GETINT(n)));
}

TYPEDOP(isupper, "cupper?", "`n -> `b. TRUE if `n is an upper case character",
        1, (value n),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "n.n")
{
  unsigned char c = GETINT(n);
  return makebool(TO_8LOWER(c) != c);
}

TYPEDOP(islower, "clower?", "`n -> `b. TRUE if `n is an lower case character",
        1, (value n),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "n.n")
{
  unsigned char c = GETINT(n);
  return makebool(TO_8UPPER(c) != c);
}

TYPEDOP(cupper, 0, "`n0 -> `n1. Return `n0's upper case variant", 1, (value n),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "n.n")
{
  return makeint(TO_8UPPER(GETINT(n)));
}

TYPEDOP(clower, 0, "`n0 -> `n1. Return `n0's lower case variant", 1, (value n),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "n.n")
{
  return makeint(TO_8LOWER(GETINT(n)));
}

TYPEDOP(cicmp, 0, "`n0 `n1 -> `n2. Compare characters `n0 and `n1 as"
        " `string_icmp() does. Returns -1, 0, or 1 if `n0 is less than,"
        " equal, or greater than `n1.",
        2, (value n0, value n1),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "nn.n")
{
  return makeint(TO_7LOWER(GETINT(n0)) - TO_7LOWER(GETINT(n1)));
}

TYPEDOP(c7bit, 0, "`n0 -> `n1. Return `n0's 7 bit variant", 1, (value n),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "n.n")
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
  if (setlocale(LC_ALL, "en_US.iso88591") == NULL)
    return NULL;
  table = pcre_maketables();
  setlocale(LC_ALL, olocale);
  return table;
#else  /* ! USE_PCRE */
  return NULL;
#endif /* ! USE_PCRE */
}

TYPEDOP(is_regexp, "regexp?", "`x -> `b. Returns TRUE if `x is a"
        " regular expression, as created by `make_regexp()",
        1, (value re), OP_LEAF | OP_NOALLOC, "x.n")
{
  return makebool(is_regexp(re));
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
  assert(TYPE(regexp_str, type_string) && regexp_str->str == p);
  regexp_str = NULL;
}

TYPEDOP(make_regexp, 0, "`s `n -> `r. Create a matcher for the regular"
        " expression `s with flags `n."
        " Returns cons(`errorstring, `erroroffset) on error",
	2, (struct string *pat, value mflags),
	OP_LEAF | OP_NOESCAPE, "sn.[ok]")
{
  pcre *p;
  const char *errstr;
  int errofs;
  struct gcpro gcpro1;
  void *(*old_malloc)(size_t) = pcre_malloc;
  void (*old_free)(void *) = pcre_free;
  char *lpat;

  int flags = GETINT(mflags);
  TYPEIS(pat, type_string);

  LOCALSTR(lpat, pat);
  if (flags & PCRE_7BIT)
    {
      strto7print(lpat);
      flags &= ~PCRE_7BIT;
    }
  regexp_str = NULL;
  GCPRO1(regexp_str);

  const unsigned char *table = get_iso88591_pcre_table();
  pcre_malloc = regexp_malloc;
  pcre_free = regexp_free;
  p = pcre_compile(lpat, flags, &errstr, &errofs, table);
  pcre_malloc = old_malloc;
  pcre_free = old_free;

  value result;
  if (p == NULL)
    result = alloc_list(alloc_string(errstr), makeint(errofs));
  else
    {
      struct grecord *mregexp  = alloc_private(PRIVATE_REGEXP, 1);
      regexp_str->o.flags |= OBJ_READONLY;
      mregexp->data[1] = &regexp_str->o;
      mregexp->o.flags |= OBJ_READONLY;
      result = mregexp;
    }

  UNGCPRO();
  return result;
}

static const pcre *get_regexp(value x)
{
  if (!is_regexp(x))
    runtime_error(error_bad_type);

  struct grecord *re = x;
  struct string *restr = (struct string *)re->data[1];
  assert(TYPE(restr, type_string));
  return (const pcre *)restr->str;
}

#if ((PCRE_MAJOR * 100) + PCRE_MINOR) >= 206
#	define PCRE_START_OFFSET 0, 
#else
#	define PCRE_START_OFFSET
#endif

TYPEDOP(regexp_exec, 0,
        "`r `s `n -> `v. Tries to match the string `s with regexp"
	" matcher `r and flags `n. Returns a vector of submatches or false if"
	" no match. If `n & `PCRE_INDICES, return a vector of"
        " cons(`start, `length) for each submatch instead.",
        3, (struct string *mre, struct string *str, value mflags),
	OP_LEAF | OP_NOESCAPE, "osn.[vn]")
{
  struct gcpro gcpro1;

  int flags = GETINT(mflags);
  const pcre *re = get_regexp(mre);
  TYPEIS(str, type_string);

  int nsub = pcre_info(re, NULL, NULL);
  if (nsub < 0)
    runtime_error(error_bad_value);
  int olen = (nsub + 1) * 3;
  int ovec[olen];

  char *haystack = str->str;
  char *lstr = NULL;
  size_t slen = string_len(str);

  bool indices = flags & PCRE_INDICES;
  flags &= ~PCRE_INDICES;

  if (flags & PCRE_7BIT)
    {
      lstr = malloc(slen + 1);
      for (size_t idx = 0; idx <= slen; ++idx)
        lstr[idx] = TO_7PRINT(haystack[idx]);
      haystack = lstr;
      flags &= ~PCRE_7BIT;
    }

  int res = pcre_exec(re, NULL, haystack, slen,
                      PCRE_START_OFFSET
                      flags, ovec, olen);

  free(lstr);
  lstr = haystack = NULL;

  if (res == PCRE_ERROR_NOMATCH)
    return makebool(false);
  else if (res < 0)
    runtime_error(error_bad_value);

  struct vector *v = alloc_vector(nsub + 1);
  GCPRO1(v);

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

#if HAVE_CRYPT_H
TYPEDOP(crypt, 0, "`s1 `s2 -> `s3. Encrypt `s1 using `s2 as salt",
	2, (struct string *s, struct string *salt),
	OP_LEAF | OP_NOESCAPE, "ss.s")
{
  char buffer[9];
  int i;
  char *p;

  TYPEIS(s, type_string);
  TYPEIS(salt, type_string);

  /* Use all the characters in the string, rather than the first 8. */
  memset(buffer, 0, sizeof buffer);

  for (p = s->str, i = 0; *p; ++p, i = (i + 1) & 7)
    buffer[i] += *p;

  for (i = 0; i < 8; ++i)
    if (!buffer[i])
      buffer[i] = ' ';

  return alloc_string(crypt(buffer, salt->str));
}
#endif /* HAVE_CRYPT_H */

static void define_string(const char *name, const char *val)
{
  struct string *s = alloc_string(val);
  s->o.flags |= OBJ_READONLY;
  system_define(name, s);
}

static void define_strings(void)
{
  define_string("NL", "\n");
  define_string("CRLF", "\r\n");

  const char host_type[] =
#ifdef i386
    "i386"
#elif defined __x86_64__
    "x86_64"
#else
#  error Fix me
#endif
    ;
  define_string("host_type", host_type);
}

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
  DEFINE(string_search);
  DEFINE(string_isearch);
  DEFINE(substring);
  DEFINE(string_append);
  DEFINE(split_words);
  DEFINE(itoa);
  DEFINE(atoi);
  DEFINE(string_upcase);
  DEFINE(string_downcase);
  DEFINE(string_7bit);
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

  DEFINE(is_regexp);

#ifdef USE_PCRE

  DEFINE(make_regexp);
  DEFINE(regexp_exec);

#define def(x) system_define(#x, makeint(x))
  /* PCRE options */
  def(PCRE_7BIT);
  def(PCRE_ANCHORED);
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

#if HAVE_CRYPT_H
  DEFINE(crypt);
#endif
  
#undef def
}
