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

#define __IS_CCLS(x, mask, cmp)                                 \
  ((latin1_char_class[(unsigned char)x] & mask) cmp)

#define IS_8SPACE(x)    __IS_CCLS((x), 0x20, != 0)
#define IS_8NAME(x)     __IS_CCLS((x),    1, != 0)
#define IS_8PRINT(x)    __IS_CCLS((x),    2, != 0)
#define IS_8ALPHA(x)    __IS_CCLS((x),    4, != 0)
#define IS_8ALPHANUM(x) __IS_CCLS((x), 0x14, != 0)
#define IS_8NOSPACE(x)  __IS_CCLS((x), 0x22, == 2)
#define IS_8OBJNAME(x)  __IS_CCLS((x),    9, != 0)

#define TO_7PRINT(x) (latin1_to_ascii_print[(unsigned char)(x)])
#define TO_7LOWER(x) (latin1_to_ascii_icmp[(unsigned char)(x)])
#define TO_8LOWER(x) (latin1_to_lower[(unsigned char)(x)])
#define TO_8UPPER(x) (latin1_to_upper[(unsigned char)(x)])

extern const unsigned char latin1_to_ascii_print[256];
extern const unsigned char latin1_to_ascii_icmp[256];
extern const unsigned char latin1_char_class[256];
extern const unsigned char latin1_to_upper[256];
extern const unsigned char latin1_to_lower[256];

/* functions with '7' in the name first convert to the 7-bit (accent-free)
   characters; 'i' ignores case; '8' preserves accents */
int str7icmp(const char *s1, const char *s2);
int mem7icmp(const void *_s1, const void *_s2, size_t n);
int str7nicmp(const char *s1, const char *s2, int n);
void *mem7ichr(const void *_s, int _c, size_t n);
void strto7print(char *str);
void str8lwr(char *str);
void str7lwr(char *str);

int lookup_named_character(const char *name, size_t namelen);

#define FOR_CHAR_ESCAPES(op, sep)               \
  op('\a', 'a') sep()                           \
  op('\b', 'b') sep()                           \
  op('\f', 'f') sep()                           \
  op('\n', 'n') sep()                           \
  op('\r', 'r') sep()                           \
  op('\t', 't') sep()                           \
  op('\v', 'v')

#endif  /* CHARSET_H */
