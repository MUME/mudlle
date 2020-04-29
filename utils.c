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

#include "mudlle-config.h"

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#include "compile.h"
#include "context.h"
#include "lexer.h"
#include "ports.h"
#include "strbuf.h"
#include "tree.h"
#include "utils.h"

bool use_nicename;
bool erred;

static void vlog_message(const struct loc *loc, bool is_warning,
                         const char *msg, va_list va)
{
  struct strbuf sb = SBNULL;

  bool use_fname = !use_nicename;

  if (loc->fname == NULL)
    {
      abort();
    }
  else
    sb_printf(&sb, "%s:", use_fname ? loc->fname->path : loc->fname->nice);
  if (loc->line > 0)
    {
      sb_printf(&sb, "%d:", loc->line);
      if (loc->col > 0)
        sb_printf(&sb, "%d:", loc->col);
    }
  sb_addc(&sb, ' ');

  if (is_warning)
    sb_addstr(&sb, "warning: ");
  if (use_fname && strcmp(loc->fname->nice, loc->fname->path) != 0)
    sb_printf(&sb, "[%s] ", loc->fname->nice);
  sb_vprintf(&sb, msg, va);
  pflush(mudout);
  pprintf(muderr, "%s\n", sb_str(&sb));
  pflush(muderr);
  sb_free(&sb);
  if (!is_warning)
    erred = true;
}

void compile_error(const struct loc *loc, const char *msg, ...)
{
  va_list args;
  va_start(args, msg);
  vlog_message(loc, false, msg, args);
  va_end(args);
}

void compile_warning(const struct loc *loc, const char *msg, ...)
{
  va_list args;
  va_start(args, msg);
  vlog_message(loc, true, msg, args);
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
  assert(newp != NULL);
  return newp;
}

void *xrealloc(void *old, int size)
{
  void *newp = realloc(old, size);
  assert(newp != NULL);
  return newp;
}

char *xstrdup(const char *s)
{
  size_t size = strlen(s) + 1;
  char *newp = xmalloc(size);
  memcpy(newp, s, size);
  return newp;
}
#endif

/* ofs is the offset to the 'next' field */
void *internal_reverse_list(void *l, size_t ofs)
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

void ary_grow(struct ary *a)
{
  ary_set_size(a, a->size ? a->size * 2 : 32);
}

void ary_set_size(struct ary *a, size_t size)
{
  a->size = size;
  a->data = realloc(a->data, a->size * sizeof a->data[0]);
  if (a->used > a->size)
    a->used = a->size;
}

#ifndef __GNUC__
int popcountl(unsigned long u)
{
  static const uint8_t count[16] = {
    0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4
  };

  int n = 0;
  while (u > 0)
    {
      n += count[u & 15];
      u >>= 4;
    }
  return n;
}

/* count leading zeros */
int clz(int i)
{
  CASSERT_EXPR(sizeof (i) == sizeof (uint32_t));

  /* from https://en.wikipedia.org/wiki/Find_first_set */
  static const uint8_t count[16] = {
    4, 3, 2, 2, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0
  };

  unsigned n = 0;
  if ((i & 0xffff0000) == 0) {n += 16; i <<= 16;}
  if ((i & 0xff000000) == 0) {n +=  8; i <<=  8;}
  if ((i & 0xf0000000) == 0) {n +=  4; i <<=  4;}
  return n + count[(i >> (32 - 4)) & 15];
}
#endif
