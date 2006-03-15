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

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include "runtime/runtime.h"
#include "stringops.h"
#include "print.h"
#include "ports.h"
#include "utils.charset.h"
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

TYPEDOP(stringp, "x -> b. TRUE if x is a string", 1, (value v),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(TYPE(v, type_string));
}

TYPEDOP(make_string, "n -> s. Create a string of length n", 1, (value size),
	OP_LEAF | OP_NOESCAPE, "n.s")
{
  struct string *newp;

  ISINT(size);
  if(intval(size) < 0)
    runtime_error(error_bad_value);
  newp = (struct string *)allocate_string(type_string, intval(size) + 1);
  newp->str[intval(size)] = '\0';
  
  return (newp);
}

TYPEDOP(string_length, "s -> n. Return length of string", 1, (struct string *str),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "s.n")
{
  TYPEIS(str, type_string);
  return (makeint(string_len(str)));
}

TYPEDOP(downcase, "s -> s. Returns a copy of s with all characters lower case",
	1, (struct string *s),
	OP_LEAF | OP_NOESCAPE, "s.s")
{
  struct gcpro gcpro1;
  struct string *newp;
  char *s1, *s2;

  TYPEIS(s, type_string);
  GCPRO1(s);
  newp = (struct string *)allocate_string(type_string, string_len(s) + 1);
  UNGCPRO();

  s1 = s->str; s2 = newp->str;
  while ((*s2++ = TO_8LOWER(*s1++)))
    ;

  return newp;
}  

TYPEDOP(upcase, "s -> s. Returns a copy of s with all characters upper case",
	1, (struct string *s),
	OP_LEAF | OP_NOESCAPE, "s.s")
{
  struct gcpro gcpro1;
  struct string *newp;
  char *s1, *s2;

  TYPEIS(s, type_string);
  GCPRO1(s);
  newp = (struct string *)allocate_string(type_string, string_len(s) + 1);
  UNGCPRO();

  s1 = s->str; s2 = newp->str;
  while ((*s2++ = TO_8UPPER(*s1++))) 
    ;

  return newp;
}  


TYPEDOP(string_fill, "s n -> s. Set all characters of s to character whose code is n",
	2, (struct string *str, value c),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "sn.1")
{
  TYPEIS(str, type_string);
  ISINT(c);

  memset(str->str, intval(c), string_len(str));
  return str;
}

TYPEDOP(strto7bit, "s -> s. Returns a copy of s with all characters converted to "
	"printable 7 bit form",
	1, (struct string *s),
	OP_LEAF | OP_NOESCAPE, "s.s")
{
  struct gcpro gcpro1;
  struct string *newp;
  char *s1, *s2;

  TYPEIS(s, type_string);
  GCPRO1(s);
  newp = (struct string *)allocate_string(type_string, string_len(s) + 1);
  UNGCPRO();

  s1 = s->str; s2 = newp->str;
  while ((*s2++ = TO_7PRINT(*s1++)))
    ;

  return newp;
}  

EXT_TYPEDOP(string_ref, "s n1 -> n2. Return the code (n2) of the n1'th character of s",
	    2, (struct string *str, value c),
	    OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "sn.n")
{
  long idx;

  TYPEIS(str, type_string);
  ISINT(c);

  idx = intval(c);
  if (idx < 0)
    idx += string_len(str);
  if (idx < 0 || idx >= string_len(str)) runtime_error(error_bad_index);
  return (makeint((unsigned char)str->str[idx]));
}

EXT_TYPEDOP(string_set, "s n1 n2 -> n2. Set the n1'th character of s to the "
	    "character whose code is n2",
	    3, (struct string *str, value i, value c),
	    OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "snn.n")
{
  long idx;

  TYPEIS(str, type_string);
  if (str->o.flags & OBJ_READONLY) runtime_error(error_value_read_only);
  ISINT(i);
  ISINT(c);

  idx = intval(i);
  if (idx < 0)
    idx += string_len(str);
  if (idx < 0 || idx >= string_len(str)) runtime_error(error_bad_index);

  return makeint((unsigned char)(str->str[idx] = intval(c)));
}

TYPEDOP(string_cmp, "s1 s2 -> n. Compare 2 strings. Returns 0 if s1 = s2, < 0 if s1 < s2 and > 0 if s1 > s2",
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
    if ((res = *t1++ - *t2++))
      break;
    i++;
  } while (1);
  return (makeint(res));
}

TYPEDOP(string_8icmp, "s1 s2 -> n. Compare 2 strings ignoring case.\n\
Returns 0 if s1 = s2, < 0 if s1 < s2 and > 0 if s1 > s2",
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
    if ((res = TO_8LOWER(*t1) - TO_8LOWER(*t2)))
      break;
    t1++; t2++; i++;
  } while (1);
  return (makeint(res));
}

TYPEDOP(string_icmp, "s1 s2 -> n. Compare 2 strings ignoring accentuation and case.\n\
Returns 0 if s1 = s2, < 0 if s1 < s2 and > 0 if s1 > s2",
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
  return (makeint(res));
}

TYPEDOP(string_search, "s1 s2 -> n. Searches in string s1 for string s2.\n\
Returns -1 if not found, index of first matching character otherwise.",
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

TYPEDOP(string_isearch, "s1 s2 -> n. Searches in string s1 for string s2 (case- and accentuation-insensitive).\n\
Returns -1 if not found, index of first matching character otherwise.",
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

TYPEDOP(substring, "s1 n1 n2 -> s2. Extract substring of s starting at n1 of length n2.\nThe first character is numbered 0",
	3, (struct string *s, value start, value length),
	OP_LEAF | OP_NOESCAPE, "snn.s")
{
  struct string *newp;
  long first, size;
  struct gcpro gcpro1;

  TYPEIS(s, type_string);
  ISINT(start);
  ISINT(length);
  GCPRO1(s);

  first = intval(start);
  if (first < 0)
    first += string_len(s);
  size = intval(length);
  if (first < 0 || size < 0 || first + size > string_len(s))
    runtime_error(error_bad_index);

  newp = (struct string *)allocate_string(type_string, size + 1);
  newp->str[size] = '\0';
  memcpy(newp->str, s->str + first, size);
  UNGCPRO();

  return (newp);
}

value string_append(struct string *s1, struct string *s2)
{
  struct string *newp;
  struct gcpro gcpro1, gcpro2;
  ulong l1, l2;

  GCPRO2(s1, s2);

  l1 = string_len(s1);
  l2 = string_len(s2);

  newp = (struct string *)allocate_string(type_string, l1 + l2 + 1);
  newp->str[l1 + l2] = '\0';
  memcpy(newp->str, s1->str, l1);
  memcpy(newp->str + l1, s2->str, l2);
  UNGCPRO();

  return (newp);
}

EXT_TYPEDOP(string_append, "s1 s2 -> s. Concatenate s1 and s2",
	    2, (struct string *s1, struct string *s2),
	    OP_LEAF | OP_NOESCAPE, "ss.s")
{
  TYPEIS(s1, type_string);
  TYPEIS(s2, type_string);
  return string_append(s1, s2);
}

TYPEDOP(split_words, "s -> l. Split string s into words in list l",
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
    int len, end;

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

    len = end - idx + (missing != 0);
    wrd = (struct string *)allocate_string(type_string, len + 1);
    memcpy(wrd->str, s->str + idx, len);
    if (missing)
      wrd->str[len - 1] = missing;
    wrd->str[len] = '\0';
    
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

TYPEDOP(atoi, "s -> n. Converts string into integer. Returns s if conversion failed",
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

TYPEDOP(itoa, "n -> s. Converts integer into string", 1, (value n),
	  OP_LEAF | OP_NOESCAPE, "n.s")
{
  char buf[16];

  sprintf(buf, "%ld", GETINT(n));
  return alloc_string(buf);
}

TYPEDOP(isalpha, "n -> b. TRUE if n is a letter (allowed in keywords)",
	1, (value n),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "n.n")
{
  return makebool(IS_8NAME(GETINT(n)));
}

TYPEDOP(isdigit, "n -> b. TRUE if n is a digit",
	1, (value n),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "n.n")
{
  return makebool(isdigit((unsigned char)GETINT(n)));
}

TYPEDOP(isxdigit, "n -> b. TRUE if n is a hexadecimal digit",
	1, (value n),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "n.n")
{
  return makebool(isxdigit((unsigned char)GETINT(n)));
}

TYPEDOP(isprint, "n -> b. TRUE if n is a printable character", 1, (value n),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "n.n")
{
  return makebool(IS_8PRINT(GETINT(n)));
}

TYPEDOP(isupper, "n -> b. TRUE if n is an upper case character", 1, (value n),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "n.n")
{
  unsigned char c = GETINT(n);
  return makebool(TO_8LOWER(c) != c);
}

TYPEDOP(islower, "n -> b. TRUE if n is an lower case character", 1, (value n),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "n.n")
{
  unsigned char c = GETINT(n);
  return makebool(TO_8UPPER(c) != c);
}

TYPEDOP(toupper, "n -> n. Return n's upper case variant", 1, (value n),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "n.n")
{
  return makeint(TO_8UPPER(GETINT(n)));
}

TYPEDOP(tolower, "n -> n. Return n's lower case variant", 1, (value n),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "n.n")
{
  return makeint(TO_8LOWER(GETINT(n)));
}

TYPEDOP(cto7bit, "n -> n. Return n's 7 bit variant", 1, (value n),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "n.n")
{
  return makeint(TO_7PRINT(GETINT(n)));
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

TYPEDOP(make_regexp, "s n -> r. Create a matcher for the regular expression "
	"s with flags n. Returns pair of errorstring . erroroffset on error",
	2, (struct string *pat, value flags),
	OP_LEAF | OP_NOESCAPE, "sn.x")
{
  pcre *p;
  const char *errstr;
  int errofs;
  struct gcpro gcpro1;
  void *(*old_malloc)(size_t) = pcre_malloc;
  void (*old_free)(void *) = pcre_free;
  char *lpat;

  ISINT(flags);
  TYPEIS(pat, type_string);

  LOCALSTR(lpat, pat);

  regexp_str = NULL;
  GCPRO1(regexp_str);

  pcre_malloc = regexp_malloc;
  pcre_free = regexp_free;
  p = pcre_compile(lpat, intval(flags), &errstr, &errofs, NULL);
  pcre_malloc = old_malloc;
  pcre_free = old_free;

  UNGCPRO();

  if (!p)
    return alloc_list(alloc_string(errstr), makeint(errofs));

  regexp_str->o.flags |= OBJ_READONLY;

  return regexp_str;
}

#if ((PCRE_MAJOR * 100) + PCRE_MINOR) >= 206
#	define PCRE_START_OFFSET 0, 
#else
#	define PCRE_START_OFFSET
#endif

TYPEDOP(regexp_exec, "r s n -> v. Tries to match the string s with regexp "
	"matcher r and flags n. Returns a vector of submatches or false if "
	"no match", 3, (struct string *re, struct string *str, value flags),
	OP_LEAF | OP_NOESCAPE, "ssn.x")
{
  int res, *ovec, olen, nsub, i;
  struct vector *v;
  struct gcpro gcpro1;

  ISINT(flags);
  TYPEIS(re, type_string);
  TYPEIS(str, type_string);

  nsub = pcre_info((const pcre *)re->str, NULL, NULL);
  if (nsub < 0)
    runtime_error(error_bad_value);
  olen = (nsub + 1) * 3;
  ovec = (int *)alloca(sizeof(int) * olen);

  res = pcre_exec((const pcre *)re->str, NULL, str->str, string_len(str), 
		  PCRE_START_OFFSET
		  intval(flags), ovec, olen);
  
  if (res == PCRE_ERROR_NOMATCH)
    return makeint(0);
  else if (res < 0)
    runtime_error(error_bad_value);

  v = alloc_vector(nsub + 1);
  GCPRO1(v);

  for(i = 0; i <= nsub; ++i)
    {
      int st = ovec[i * 2];
      if(st >= 0)
	{
	  int ln = ovec[i * 2 + 1] - st;
	  struct string *tmp;
	  tmp = (struct string *)allocate_string(type_string, ln + 1);
	  memcpy(tmp->str, str->str + st, ln);
	  tmp->str[ln] = 0;
	  v->data[i] = tmp;
	}
    }

  UNGCPRO();

  return v;
}

#endif /* USE_PCRE */

#if HAVE_CRYPT_H
TYPEDOP(crypt, "s1 s2 -> s. Encrypt s1 using s2 as salt",
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

static void define_strings(void)
{
  struct string *s = 0;
  struct gcpro gcpro1;

  GCPRO1(s);

  s = alloc_string("\n");
  s->o.flags |= OBJ_READONLY;
  system_define("NL", s);
  s = alloc_string("\r\n");
  s->o.flags |= OBJ_READONLY;
  system_define("CRLF", s);

  UNGCPRO();
}

void string_init(void)
{
  DEFINE("string?", stringp);
  DEFINE("make_string", make_string);
  DEFINE("string_length", string_length);
  DEFINE("string_fill!", string_fill);
  DEFINE("string_ref", string_ref);
  DEFINE("string_set!", string_set);
  DEFINE("string_cmp", string_cmp);
  DEFINE("string_icmp", string_icmp);
  DEFINE("string_8icmp", string_8icmp);
  DEFINE("string_search", string_search);
  DEFINE("string_isearch", string_isearch);
  DEFINE("substring", substring);
  DEFINE("string_append", string_append);
  DEFINE("split_words", split_words);
  DEFINE("itoa", itoa);
  DEFINE("atoi", atoi);
  DEFINE("string_upcase", upcase);
  DEFINE("string_downcase", downcase);
  DEFINE("string_7bit", strto7bit);
  DEFINE("calpha?", isalpha);
  DEFINE("cupper?", isupper);
  DEFINE("clower?", islower);
  DEFINE("cdigit?", isdigit);
  DEFINE("cxdigit?", isxdigit);
  DEFINE("cprint?", isprint);
  DEFINE("cupper", toupper);
  DEFINE("clower", tolower);
  DEFINE("c7bit", cto7bit);

  define_strings();

#define def(x) system_define(#x, makeint(x))

#ifdef USE_PCRE

  DEFINE("make_regexp", make_regexp);
  DEFINE("regexp_exec", regexp_exec);

  /* PCRE options */
  def(PCRE_CASELESS);
  def(PCRE_MULTILINE);
  def(PCRE_DOTALL);
  def(PCRE_EXTENDED);
  def(PCRE_ANCHORED);
  def(PCRE_DOLLAR_ENDONLY);
  def(PCRE_EXTRA);
  def(PCRE_NOTBOL);
  def(PCRE_NOTEOL);
  def(PCRE_UNGREEDY);

#endif
#if HAVE_CRYPT_H
  DEFINE("crypt", crypt);
#endif
  
#undef def
}
