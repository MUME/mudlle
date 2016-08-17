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

/*
 * Support routines for different charsets
 */

#ifndef CHARSET_H
#define CHARSET_H

#include <stdbool.h>
#include <string.h>

#define IS_8SPACE(x) (latin1_char_class[(unsigned char) (x)] & 0x20)
#define IS_8NAME(x) (latin1_char_class[(unsigned char) (x)] & 1)
#define IS_8PRINT(x) (latin1_char_class[(unsigned char) (x)] & 2)
#define IS_8ALPHA(x) (latin1_char_class[(unsigned char) (x)] & 4)
#define IS_8ALPHANUM(x) (latin1_char_class[(unsigned char) (x)] & 0x14)
#define IS_8NOSPACE(x) ((latin1_char_class[(unsigned char) (x)] & 0x22) == 2)
#define IS_8OBJNAME(x) (latin1_char_class[(unsigned char) (x)] & 9)

#define TO_7PRINT(x) (latin1_to_ascii_print[(unsigned char) (x)])
#define TO_7LOWER(x) (latin1_to_ascii_icmp[(unsigned char) (x)])
#define TO_8LOWER(x) (latin1_to_lower[(unsigned char) (x)])
#define TO_8UPPER(x) (latin1_to_upper[(unsigned char) (x)])

extern const unsigned char latin1_to_ascii_print[256];
extern const unsigned char latin1_to_ascii_icmp[256];
extern const unsigned char latin1_char_class[256];
extern const unsigned char latin1_to_upper[256];
extern const unsigned char latin1_to_lower[256];

bool str_is8bit(const char *str);
int str8icmp(const char *s1, const char *s2);
int mem8icmp(const void *_s1, const void *_s2, size_t n);
int str8nicmp(const char *s1, const char *s2, int n);
void *mem8ichr(const void *_s, int _c, size_t n);
void strto7print(char *str);
void str8lwr(char *str);
void str7lwr(char *str);
char *str8cap(char *str);

int lookup_named_character(const char *name, size_t namelen);

#endif  /* CHARSET_H */
