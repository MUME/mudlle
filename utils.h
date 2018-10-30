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

#include "mudlle-config.h"

#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>

#include "mudlle.h"

struct loc;

extern bool erred;
extern bool use_nicename;

#ifdef DEBUG_MEMORY
void *debug_xmalloc(const char *file, int line, int size);
void *debug_xrealloc(const char *file, int line, void *old, int size);
char *debug_xstrdup(const char *file, int line, const char *s);
#else
void *xmalloc(int size);
void *xrealloc(void *old, int size);
char *xstrdup(const char *s);
#endif

void log_error(const struct loc *loc, const char *msg, ...) FMT_PRINTF(2, 3);
void compile_error(const struct loc *loc, const char *msg, ...)
  FMT_PRINTF(2, 3);
void compile_warning(const struct loc *loc, const char *msg, ...)
  FMT_PRINTF(2, 3);
void warning_loc(const char *fname, const char *nname, const struct loc *loc,
                 const char *msg, ...) FMT_PRINTF(4, 5);

void *internal_reverse_list(void *l, size_t ofs);

/* Reverses list 'l' of type 'type *' and returns the new list.
   *l must have a pointer field called 'next', pointing to the next item. */
#define reverse_list_field(l, type, next)                       \
  (_Generic((l),       type *: (void)0),                        \
   _Generic((l)->next, type *: (void)0),                        \
   (type *)internal_reverse_list((l), offsetof(type, next)))

#define reverse_list(l, type) reverse_list_field(l, type, next)

static inline unsigned long get_stack_pointer(void)
{
  unsigned long r;
#ifdef __i386__
  asm("movl %%esp,%0" : "=rm" (r));
#elif defined __x86_64__
  asm("movq %%rsp,%0" : "=rm" (r));
#else
 #warning Improve this
  volatile int v;
  r = (unsigned long)&v;
#endif
  return r;
}

struct ary {
  size_t used, size;
  void **data;
};

#define ARY_FOREACH(ary, type, var)                     \
  for (type **__apos = (type **)(ary)->data,            \
         **const __aend = __apos + (ary)->used, *var;   \
       __apos < __aend ? (var = *__apos, true) : false; \
       ++__apos)

#define ARY_NULL { .used = 0 }

void ary_grow(struct ary *a);
void ary_set_size(struct ary *a, size_t size);

static inline void ary_add(struct ary *a, void *d)
{
  while (a->used >= a->size)
    ary_grow(a);
  a->data[a->used++] = d;
}

static inline void ary_empty(struct ary *a)
{
  a->used = 0;
}

static inline size_t ary_entries(struct ary *a)
{
  return a->used;
}

static inline void ary_free(struct ary *a)
{
  free(a->data);
  *a = (struct ary)ARY_NULL;
}

#ifdef __GNUC__
static inline int popcountl(unsigned long u)
{
  return __builtin_popcountl(u);
}

static inline int popcount(unsigned u)
{
  return __builtin_popcount(u);
}
#else
int popcountl(unsigned long u);

static inline int popcount(unsigned u)
{
  return popcountl(u);
}
#endif

#endif
