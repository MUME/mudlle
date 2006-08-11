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

#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "mudlle.h"
#include "mudio.h"
#include "utils.h"
#  include "options.h"

extern int lineno;
extern const char *filename;

int erred;

void log_error(const char *msg, ...)
{
  va_list args;
  char err[4096];

  va_start(args, msg);
  vsprintf(err, msg, args);
  va_end(args);
  if (mudout) mflush(mudout);
  mprintf(muderr, "%s" EOL, err);
  if (muderr) mflush(muderr);
  erred = 1;
}

void compile_error(const char *msg, ...)
{
  va_list args;
  char err[4096];

  va_start(args, msg);
  vsprintf(err, msg, args);
  va_end(args);
  if (mudout) mflush(mudout);
  mprintf(muderr, "%s:%d: %s" EOL, filename, lineno, err);
  if (muderr) mflush(muderr);
  erred = 1;
}

static void vwarning(const char *fname, int line, const char *msg, va_list args)
{
  char err[4096];

  vsnprintf(err, sizeof err, msg, args);
  if (mudout) mflush(mudout);

  if (fname == NULL)
    mprintf(muderr, "warning: %s" EOL, err);
  else if (line > 0)
    mprintf(muderr, "%s:%d: warning: %s" EOL, fname, line, err);
  else
    mprintf(muderr, "%s: warning: %s" EOL, fname, err);

  if (muderr) mflush(muderr);
}


void warning(const char *msg, ...)
{
  va_list args;

  va_start(args, msg);
  vwarning(NULL, -1, msg, args);
  va_end(args);
}

void warning_line(const char *fname, int line, const char *msg, ...)
{
  va_list args;

  va_start(args, msg);
  vwarning(fname, line, msg, args);
  va_end(args);
}

#ifdef DEBUG_MEMORY
void *debug_xmalloc(const char *file, int line, int size)
{
  void *newp = debug_malloc(file, line, size);

  if (!newp)
    {
      fprintf(stderr, "No memory!\n");
      abort();
    }

  return newp;
}

void *debug_xcalloc(const char *file, int line, int number, int size)
{
  void *newp = debug_calloc(file, line, number, size);

  if (!newp) abort();

  return newp;
}

void *debug_xrealloc(const char *file, int line, void *old, int size)
{
  void *newp = debug_realloc(file, line, old, size);

  if (!newp) abort();

  return newp;
}

char *debug_xstrdup(const char *file, int line, const char *s)
{
  char *newp = debug_xmalloc(file, line, strlen(s) + 1);

  return strcpy(newp, s);
}
#else
void *xmalloc(int size)
{
  void *newp = malloc(size);

  if (!newp) abort();

  return newp;
}

void *xcalloc(int number, int size)
{
  void *newp = calloc(number, size);

  if (!newp) abort();

  return newp;
}

void *xrealloc(void *old, int size)
{
  void *newp = realloc(old, size);

  if (!newp) abort();

  return newp;
}

char *xstrdup(const char *s)
{
  char *newp = xmalloc(strlen(s) + 1);

  return strcpy(newp, s);
}
#endif

char *strlwr(char *s)
{
  char *t = s;

  while (*t) { *t = tolower(*t); t++; }

  return s;
}

#if (!HAVE_MEMMOVE)
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
