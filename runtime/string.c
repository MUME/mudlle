/* $Log: string.c,v $
 * Revision 1.16  1995/08/23  20:21:32  arda
 * New primitives: string_upcase/downcase
 * Remove builtin /help command.
 * ?
 *
 * Revision 1.15  1994/10/09  06:44:21  arda
 * Libraries
 * Type inference
 * Many minor improvements
 *
 * Revision 1.14  1994/09/06  07:50:45  arda
 * Constant support: detect_immutability, global_set!, string_{i}search.
 *
 * Revision 1.13  1994/08/26  18:15:47  arda
 * Minor fixes
 *
 * Revision 1.12  1994/08/26  08:51:51  arda
 * Keep free block list for string ports.
 *
 * Revision 1.11  1994/08/16  19:17:18  arda
 * Added flags to primitives for better calling sequences.
 *
 * Revision 1.10  1994/03/08  01:50:58  arda
 * (MD) New Istari.
 *
 * Revision 1.9  1993/10/03  14:07:27  dgay
 * Bumper disun8 update.
 *
 * Revision 1.8  1993/05/02  07:38:21  un_mec
 * Owl: New output (mudlle ports).
 *
 * Revision 1.7  1993/04/25  19:50:41  un_mec
 * Owl: Miscellaneous changes.
 *      I HATE fixing bugs twice.
 *
 * Revision 1.6  1993/04/24  15:21:15  un_mec
 * Owl: Code cleanup.
 *
 * Revision 1.4  1993/03/29  09:25:54  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.3  1993/03/14  16:16:51  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.2  1993/02/14  00:40:16  un_mec
 * Owl: MUME III released:
 * - mudlle is now basically working. Lots of basic procedures still need
 * to be added.
 *
 * Revision 1.1  1992/12/27  21:42:24  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

static char rcsid[] = "$Id: string.c,v 1.16 1995/08/23 20:21:32 arda Exp $";

#include <string.h>
#include <ctype.h>
#include "runtime/runtime.h"
#include "stringops.h"
#include "print.h"
#include "ports.h"

TYPEDOP(stringp, "x -> b. TRUE if x is a string", 1, (value v),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(TYPE(v, type_string));
}

TYPEDOP(make_string, "n -> s. Create an empty string of length n", 1, (value size),
	OP_LEAF | OP_NOESCAPE, "n.s")
{
  struct string *new;

  ISINT(size);
  new = (struct string *)allocate_string(type_string, intval(size) + 1);
  new->str[intval(size)] = '\0';
  
  return (new);
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
  struct string *new;
  char *s1, *s2;

  TYPEIS(s, type_string);
  GCPRO1(s);
  new = (struct string *)allocate_string(type_string, string_len(s) + 1);
  UNGCPRO();

  s1 = s->str; s2 = new->str;
  while (*s2++ = tolower(*s1++)) ;

  return new;
}  

TYPEDOP(upcase, "s -> s. Returns a copy of s with all characters upper case",
	1, (struct string *s),
	OP_LEAF | OP_NOESCAPE, "s.s")
{
  struct gcpro gcpro1;
  struct string *new;
  char *s1, *s2;

  TYPEIS(s, type_string);
  GCPRO1(s);
  new = (struct string *)allocate_string(type_string, string_len(s) + 1);
  UNGCPRO();

  s1 = s->str; s2 = new->str;
  while (*s2++ = toupper(*s1++)) ;

  return new;
}  

TYPEDOP(string_fill, "s n -> . Set all characters of s to character whose code is n",
	2, (struct string *str, value c),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "sn.")
{
  TYPEIS(str, type_string);
  ISINT(c);

  memset(str->str, intval(c), string_len(str));
  undefined();
}

TYPEDOP(string_ref, "s n1 -> n2. Return the code (n2) of the n1'th character of s",
	2, (struct string *str, value c),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "sn.n")
{
  long index;

  TYPEIS(str, type_string);
  ISINT(c);

  index = intval(c);
  if (index < 0 || index >= string_len(str)) runtime_error(error_bad_index);
  return (makeint(str->str[index]));
}

TYPEDOP(string_set, "s n1 n2 -> n2. Set the n1'th character of s to the character whose code is n2",
	3, (struct string *str, value i, value c),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "snn.n")
{
  long index;

  TYPEIS(str, type_string);
  if (str->o.flags & OBJ_READONLY) runtime_error(error_value_read_only);
  ISINT(i);
  ISINT(c);

  index = intval(i);
  if (index < 0 || index >= string_len(str)) runtime_error(error_bad_index);
  str->str[index] = intval(c);

  return c;
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
    if (res = *t1++ - *t2++) break;
    i++;
  } while (1);
  return (makeint(res));
}

TYPEDOP(string_icmp, "s1 s2 -> n. Compare 2 strings ignoring case.\n\
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
    if (res = tolower(*t1) - tolower(*t2)) break;
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

TYPEDOP(string_isearch, "s1 s2 -> n. Searches in string s1 for string s2 (case insensitive).\n\
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
  lastc2 = tolower(t2[l2 - 1]);
  
  i = l2 - 1; /* No point in starting earlier */
  for (;;)
    {
      /* Search for lastc2 in t1 starting at i */
      while (tolower(t1[i]) != lastc2)
	if (++i == l1) return makeint(-1);

      /* Check if rest of string matches */
      j = l2 - 1;
      i1 = i;
      do
	{
	  if (j == 0) return makeint(i1); /* match found at i1 */
	  --j; --i1;
	}
      while (tolower(t2[j]) == tolower(t1[i1]));

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
  struct string *new;
  long first, size;
  struct gcpro gcpro1;

  TYPEIS(s, type_string);
  ISINT(start);
  ISINT(length);
  GCPRO1(s);

  first = intval(start);
  size = intval(length);
  if (first < 0 || size < 0 || first + size > string_len(s))
    runtime_error(error_bad_index);

  new = (struct string *)allocate_string(type_string, size + 1);
  new->str[size] = '\0';
  memcpy(new->str, s->str + first, size);
  UNGCPRO();

  return (new);
}

value string_append(struct string *s1, struct string *s2)
{
  struct string *new;
  struct gcpro gcpro1, gcpro2;
  ulong l1, l2;

  GCPRO2(s1, s2);

  l1 = string_len(s1);
  l2 = string_len(s2);

  new = (struct string *)allocate_string(type_string, l1 + l2 + 1);
  new->str[l1 + l2] = '\0';
  memcpy(new->str, s1->str, l1);
  memcpy(new->str + l1, s2->str, l2);
  UNGCPRO();

  return (new);
}

TYPEDOP(string_append, "s1 s2 -> s. Concatenate s1 and s2",
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
  struct string *word;
  int len;
  char *scan, *end, missing;
  struct gcpro gcpro1, gcpro2;

  TYPEIS(s, type_string);

  scan = s->str;
  GCPRO2(l, last);

  do {
    while (*scan == ' ') scan++;

    missing = 0;
    if (*scan == '\'' || *scan == '"') /* Quoted words */
      {
	end = scan + 1;
	while (*end && *end != *scan) end++;
	/* Be nice: add missing quote */
	if (!*end) missing = *scan;
	else end++;
      }
    else
      {
	end = scan;
	while (*end && *end != ' ') end++;

	if (end == scan) break;
      }

    len = end - scan + (missing != 0);
    word = (struct string *)allocate_string(type_string, len + 1);
    memcpy(word->str, scan, len);
    if (missing) word->str[len - 1] = missing;
    word->str[len] = '\0';
    
    scan = end;

    if (!l) l = last = alloc_list(word, NULL);
    else 
      {
	last->cdr = alloc_list(word, NULL);
	last = last->cdr;
      }
  } while (1);

  UNGCPRO();

  return (l);
}

TYPEDOP(atoi, "s -> n. Converts string into integer. Returns s if conversion failed",
	  1, (struct string *s),
	  OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "s.S")
{
  long n;

  TYPEIS(s, type_string);
  if (sscanf(s->str, "%ld", &n) == 1) return makeint(n);
  else return s;
}

TYPEDOP(itoa, "n -> s. Converts integer into string", 1, (value n),
	  OP_LEAF | OP_NOESCAPE, "n.s")
{
  char buf[16];

  ISINT(n);
  sprintf(buf, "%ld", intval(n));
  return alloc_string(buf);
}

VAROP(format, "s x1 x2 ... -> s. Formats string s with parameters x1, ...",
      OP_LEAF)
{
  int i;
  struct string *str;
  ulong l, spos;
  struct gcpro gcpro1, gcpro2, gcpro3;
  struct oport *p;

  if (nargs < 1) runtime_error(error_wrong_parameters);
  str = args->data[0];
  TYPEIS(str, type_string);
  GCPRO2(args, str);
  p = make_string_outputport();
  GCPRO(gcpro3, p);

  l = string_len(str);
  i = 1;
  spos = 0;
  while (spos < l)
    if (str->str[spos] == '%')
      {
	spos++;
	if (spos == l) runtime_error(error_bad_value);
	switch (str->str[spos])
	  {
	  default: runtime_error(error_bad_value);
	  case '%': pputc('%', p); break;
	  case 'c':
	    if (i >= nargs) runtime_error(error_wrong_parameters);
	    ISINT(args->data[i]);
	    pputc(intval(args->data[i++]), p);
	    break;
	  case 'n': pputs(EOL, p); break;
	  case 'p':
	    if (i >= nargs) runtime_error(error_wrong_parameters);
	    ISINT(args->data[i]);
	    if (intval(args->data[i++]) != 1) pputc('s', p);
	    break;
	  case 'P':
	    if (i >= nargs) runtime_error(error_wrong_parameters);
	    ISINT(args->data[i]);
	    if (intval(args->data[i++]) != 1) pputs("ies", p);
	    else pputc('y', p);
	    break;
	  case 's':
	    if (i >= nargs) runtime_error(error_wrong_parameters);
	    output_value(p, prt_display, args->data[i++]);
	    break;
	  case 'w':
	    if (i >= nargs) runtime_error(error_wrong_parameters);
	    output_value(p, prt_print, args->data[i++]);
	    break;
	  }
	spos++;
      }
    else
      {
	pputc(str->str[spos], p);
	spos++;
      }

  if (i != nargs) runtime_error(error_wrong_parameters);

  str = port_string(p);
  UNGCPRO();
  opclose(p);
  return str;
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
  DEFINE("string_search", string_search);
  DEFINE("string_isearch", string_isearch);
  DEFINE("substring", substring);
  DEFINE("string_append", string_append);
  DEFINE("split_words", split_words);
  DEFINE("itoa", itoa);
  DEFINE("atoi", atoi);
  DEFINE("format", format);
  DEFINE("string_upcase", upcase);
  DEFINE("string_downcase", downcase);
}
