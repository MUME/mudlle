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

#ifndef UTILS_H
#define UTILS_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#include "mudlle.h"

extern bool erred;
extern bool use_nicename;

#ifdef DEBUG_MEMORY
void *debug_xmalloc(const char *file, int line, int size);
void *debug_xcalloc(const char *file, int line, int number, int size);
void *debug_xrealloc(const char *file, int line, void *old, int size);
char *debug_xstrdup(const char *file, int line, const char *s);
#else
void *xmalloc(int size);
void *xcalloc(int number, int size);
void *xrealloc(void *old, int size);
char *xstrdup(const char *s);
#endif

void log_error(const char *msg, ...) FMT_PRINTF(1, 2);
void compile_error(const char *msg, ...) FMT_PRINTF(1, 2);
void compile_warning(const char *msg, ...) FMT_PRINTF(1, 2);
void warning_line(const char *fname, const char *nname, int line,
                  const char *msg, ...) FMT_PRINTF(4, 5);

/* The only standard tag ... */
#define TAG_END -1

void *reverse_list_internal(void *l);

/* Reverses list 'l' of type 'type *' and returns the new list.
   The first field in *l must be a pointer called 'next' to the next item. */
#define reverse_list(l, type)                             \
  ((void)((l) - (type *)0),                               \
   CASSERT_EXPR(offsetof(type, next) == 0),               \
   (void)(&(l) - &(l)->next),                             \
   (type *)reverse_list_internal(l))

#endif
