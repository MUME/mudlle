/* $Log: utils.c,v $
 * Revision 1.9  1994/10/09  06:43:11  arda
 * Libraries
 * Type inference
 * Many minor improvements
 *
 * Revision 1.8  1994/09/16  13:07:15  arda
 * Rename protect to catch.
 * New protect/unprotect functions (like dynpro/undynpro).
 *
 * Revision 1.7  1994/02/11  09:59:30  dgay
 * Owl: -Wall
 *      new shared string handling
 *      configuration file
 *
 * Revision 1.6  1993/12/23  20:48:56  dgay
 * Owl: New alloc.c: semi-generational collector.
 *      Included Amiga makefile for convenience.
 *
 * Revision 1.5  1993/12/06  19:20:54  arda
 * divers CLI
 *
 * Revision 1.4  1993/11/27  11:29:12  arda
 * Owl: Major changes to affect.
 *      Save mudlle data with players & objects.
 *      Change skill format on disk.
 *      Other minor changes.
 *      Still needs full debugging.
 *
 * Revision 1.3  1993/08/15  21:00:34  un_mec
 * Owl: Overload [].
 *      Added xcalloc, xrealloc.
 *
 * Revision 1.2  1993/03/29  09:24:50  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.3  1993/03/14  16:15:18  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.1  1992/12/27  21:41:45  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

static char rcsid[] = "$Id: utils.c,v 1.9 1994/10/09 06:43:11 arda Exp $";

#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "mudlle.h"
#include "mudio.h"
#include "utils.h"
#ifdef MUME
#include "macro.h"
#endif

int erred;

void error(const char *msg, ...)
{
  va_list args;
  char err[4096];

  va_start(args, msg);
  vsprintf(err, msg, args);
  va_end(args);
  mflush(mudout);
  mprintf(muderr, "%s" EOL, err);
  erred = 1;
}

void warning(const char *msg, ...)
{
  va_list args;
  char err[4096];

  va_start(args, msg);
  vsprintf(err, msg, args);
  va_end(args);
  mflush(mudout);
  mprintf(muderr, "warning: %s" EOL, err);
}

#ifdef DEBUG_MEMORY
void *debug_xmalloc(const char *file, int line, int size)
{
  void *new = debug_malloc(file, line, size);

  if (!new)
    {
      fprintf(stderr, "No memory!\n");
      abort();
    }

  return new;
}

void *debug_xcalloc(const char *file, int line, int number, int size)
{
  void *new = debug_calloc(file, line, number, size);

  if (!new) abort();

  return new;
}

void *debug_xrealloc(const char *file, int line, void *old, int size)
{
  void *new = debug_realloc(file, line, old, size);

  if (!new) abort();

  return new;
}

char *debug_xstrdup(const char *file, int line, char *s)
{
  char *new = debug_xmalloc(file, line, strlen(s) + 1);

  return strcpy(new, s);
}
#else
void *xmalloc(int size)
{
  void *new = malloc(size);

  if (!new) abort();

  return new;
}

void *xcalloc(int number, int size)
{
  void *new = calloc(number, size);

  if (!new) abort();

  return new;
}

void *xrealloc(void *old, int size)
{
  void *new = realloc(old, size);

  if (!new) abort();

  return new;
}

char *xstrdup(const char *s)
{
  char *new = xmalloc(strlen(s) + 1);

  return strcpy(new, s);
}
#endif

char *strlwr(char *s)
{
  char *t = s;

  while (*t) { *t = tolower(*t); t++; }

  return s;
}

#ifndef HAVE_MEMMOVE
void memmove(char *to, const char *from, int n)
{
  if (to == from) return;

  if (to > from && to < from + n)
    {
      from += n;
      to += n;
      while (n--) *--to = *--from;
    }
  else if (from > to && from < to + n)
    {
      while (n--) *to++ = *from++;
    }
  else memcpy(to, from, n);
}

#endif
