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

#ifndef COMPILE_H
#define COMPILE_H

#include <stdbool.h>

#include "types.h"

struct constant;
struct mfile;

extern struct component *component_undefined;
extern struct component *component_true, *component_false;

extern struct constant *constant_null;

void init_string_cache(void);
void free_string_cache(void);
struct string *scache_alloc_str_len(const char *str, size_t len);
struct string *scache_alloc_str(const char *str);

value make_constant(const struct constant *c);
value make_shared_string_constant(const struct constant *c,
                                  struct table *cache);
bool interpret(value *result, seclev_t seclev, int reload);
struct closure *compile_code(struct mfile *f, seclev_t seclev);

void compile_init(void);

extern struct mfile *this_mfile;

bool load_file(const char *fullname, const char *filename,
               const char *nicename, seclev_t seclev, bool reload);

#endif
