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

#ifndef UTILS_H
#define UTILS_H

#  include "options.h"

extern int erred;

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

char *strlwr(char *s);
void compile_error(const char *msg, ...);
void log_error(const char *msg, ...);
void warning(const char *msg, ...);
void warning_line(const char *fname, int line, const char *msg, ...);

#if (!HAVE_MEMMOVE)
void *memmove(void *dest, const void *src, size_t n);
#endif

/* The only standard tag ... */
#define TAG_END -1

#endif
