/*
 * Copyright (c) 1993-2012 David Gay and Gustav H�llberg
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

#ifndef PRINT_H
#define PRINT_H

#include "ports.h"

struct strbuf;

enum prt_level { prt_display, prt_write, prt_examine, prt_constant };

void output_value_cut(struct oport *f, enum prt_level level,
                      bool no_quote, value v, size_t maxlen);
void output_value(struct oport *f, enum prt_level level, value v);
bool print_constant(struct oport *f, value v, size_t maxlen, bool allow_gone);

void write_nul_string(struct oport *op, struct string *s);

void print_init(void);

void describe_fn(struct strbuf *sb, value v);

void sb_write_string(struct strbuf *sb, const char *str, size_t len);

void sb_add_seclevel(struct strbuf *sb, int lev);

#endif
