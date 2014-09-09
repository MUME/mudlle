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

#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "compile.h"
#include "context.h"
#include "lexer.h"
#include "ports.h"
#include "strbuf.h"
#include "utils.h"

bool use_nicename;
bool erred;

static void vlog_message(const char *fname, const char *nname, int line,
                         bool is_warning, const char *msg, va_list va)
{
  strbuf_t sb = SBNULL;

  if (fname != NULL)
    {
      bool use_fname = !use_nicename || nname == NULL;

      sb_printf(&sb, "%s:", use_fname ? fname : nname);
      if (line > 0)
        sb_printf(&sb, "%d:", line);
      sb_addc(&sb, ' ');
    }
  if (is_warning)
    sb_addstr(&sb, "warning: ");
  if (!use_nicename && fname != NULL && nname != NULL
      && strcmp(nname, fname) != 0)
    sb_printf(&sb, "[%s] ", nname);
  sb_vprintf(&sb, msg, va);
  if (mudout) pflush(mudout);
  pprintf(muderr, "%s\n", sb_str(&sb));
  if (muderr) pflush(muderr);
  sb_free(&sb);
  if (!is_warning)
    erred = true;
}

void log_error(const char *msg, ...)
{
  va_list args;
  va_start(args, msg);
  block body = this_mfile ? this_mfile->body : NULL;
  vlog_message(body ? body->filename : NULL,
               body ? body->nicename : NULL,
               yylineno, false, msg, args);
  va_end(args);
}

void compile_error(const char *msg, ...)
{
  va_list args;
  va_start(args, msg);
  vlog_message(lexer_filename, lexer_nicename, yylineno, false, msg, args);
  va_end(args);
}

static void vwarning(const char *fname, const char *nname, int line,
                     const char *msg, va_list args)
{
  vlog_message(fname, nname, line, true, msg, args);
}


void compile_warning(const char *msg, ...)
{
  va_list args;

  va_start(args, msg);
  vwarning(lexer_filename, lexer_nicename, yylineno, msg, args);
  va_end(args);
}

void warning_line(const char *fname, const char *nname, int line,
                  const char *msg, ...)
{
  va_list args;

  va_start(args, msg);
  vwarning(fname, nname, line, msg, args);
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

/* ofs is the offset to the 'next' field */
void *reverse_list_internal(void *l, size_t ofs)
{
  void *prev = NULL;
  while (l != NULL)
    {
      void **p = (void *)((char *)l + ofs);
      void *next = *p;
      *p = prev;
      prev = l;
      l = next;
    }
  return prev;
}
