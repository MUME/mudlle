/*
 * Copyright (c) 1993-2004 David Gay and Gustav Hållberg
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

#ifndef UTILS_CHARSET_H
#define UTILS_CHARSET_H

#define CHARSET_UNKNOWN 0        /* we don't know yet - assume ASCII */
#define CHARSET_ASCII   1
#define CHARSET_LATIN1  2

typedef unsigned int charset_t;

#define IS_8NAME(x) (latin1_char_class[(unsigned char) (x)] & 1)
#define IS_8PRINT(x) (latin1_char_class[(unsigned char) (x)] & 2)
#define IS_8ALPHA(x) (latin1_char_class[(unsigned char) (x)] & 4)
#define IS_8ALPHANUM(x) (latin1_char_class[(unsigned char) (x)] & 20)
#define IS_8NOSPACE(x) (IS_8PRINT(x) && !isspace(x))
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

extern int str_is8bit(const char *str);
extern int str8icmp(const char *s1, const char *s2);
extern int str8nicmp(const char *s1, const char *s2, int n);
extern void strto7print(char *str);
extern void str8lwr(char *str);
extern void str7lwr(char *str);
extern char *str8cap(char *str);

#endif // UTILS_CHARSET_H
