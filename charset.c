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

#include <stdlib.h>

#include "charset.h"
#include "mudlle.h"

/*
 * conversion table for latin1 to 7-bit ascii (for output)
 */
const unsigned char latin1_to_ascii_print[256] = {
  0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
  0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
  0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
  0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f,
  ' ',  '!',  '"',  '#',  '$',  '%',  '&',  '\'',
  '(',  ')',  '*',  '+',  ',',  '-',  '.',  '/',
  '0',  '1',  '2',  '3',  '4',  '5',  '6',  '7',
  '8',  '9',  ':',  ';',  '<',  '=',  '>',  '?',
  '@',  'A',  'B',  'C',  'D',  'E',  'F',  'G',
  'H',  'I',  'J',  'K',  'L',  'M',  'N',  'O',
  'P',  'Q',  'R',  'S',  'T',  'U',  'V',  'W',
  'X',  'Y',  'Z',  '[',  '\\', ']',  '^',  '_',
  '`',  'a',  'b',  'c',  'd',  'e',  'f',  'g',
  'h',  'i',  'j',  'k',  'l',  'm',  'n',  'o',
  'p',  'q',  'r',  's',  't',  'u',  'v',  'w',
  'x',  'y',  'z',  '{',  '|',  '}',  '~',  0x7f,
  0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
  0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
  0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97,
  0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
  ' ',  '!',  'c',  'L',  '$',  'Y',  '|',  'P',
  '"',  'C',  'a',  '<',  ',',  '-',  'R',  '-',
  'd',  '+',  '2',  '3',  '\'', 'u',  'P',  '*',
  ',',  '1',  'o',  '>',  '4',  '2',  '3',  '?',
  'A',  'A',  'A',  'A',  'A',  'A',  'A',  'C',
  'E',  'E',  'E',  'E',  'I',  'I',  'I',  'I',
  'D',  'N',  'O',  'O',  'O',  'O',  'O',  '*',
  'O',  'U',  'U',  'U',  'U',  'Y',  'T',  's',
  'a',  'a',  'a',  'a',  'a',  'a',  'a',  'c',
  'e',  'e',  'e',  'e',  'i',  'i',  'i',  'i',
  'd',  'n',  'o',  'o',  'o',  'o',  'o',  '/',
  'o',  'u',  'u',  'u',  'u',  'y',  't',  'y'
};

/*
 * conversion table for latin1 to 7-bit lower case ascii (for comparisons)
 */
const unsigned char latin1_to_ascii_icmp[256] = {
  0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
  0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
  0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
  0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f,
  ' ',  '!',  '"',  '#',  '$',  '%',  '&',  '\'',
  '(',  ')',  '*',  '+',  ',',  '-',  '.',  '/',
  '0',  '1',  '2',  '3',  '4',  '5',  '6',  '7',
  '8',  '9',  ':',  ';',  '<',  '=',  '>',  '?',
  '@',  'a',  'b',  'c',  'd',  'e',  'f',  'g',
  'h',  'i',  'j',  'k',  'l',  'm',  'n',  'o',
  'p',  'q',  'r',  's',  't',  'u',  'v',  'w',
  'x',  'y',  'z',  '[',  '\\', ']',  '^',  '_',
  '`',  'a',  'b',  'c',  'd',  'e',  'f',  'g',
  'h',  'i',  'j',  'k',  'l',  'm',  'n',  'o',
  'p',  'q',  'r',  's',  't',  'u',  'v',  'w',
  'x',  'y',  'z',  '{',  '|',  '}',  '~',  0x7f,
  0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
  0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
  0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97,
  0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
  0xa0, '¡',  '¢',  '£',  '¤',  '¥',  '¦',  '§',
  '¨',  '©',  'ª',  '«',  '¬',  '­',  '®',  '¯',
  '°',  '±',  '²',  '³',  '´',  'µ',  '¶',  '·',
  '¸',  '¹',  'º',  '»',  '¼',  '½',  '¾',  '¿',
  'a',  'a',  'a',  'a',  'a',  'a',  'a',  'c',
  'e',  'e',  'e',  'e',  'i',  'i',  'i',  'i',
  'd',  'n',  'o',  'o',  'o',  'o',  'o',  '×',
  'o',  'u',  'u',  'u',  'u',  'y',  't',  's',
  'a',  'a',  'a',  'a',  'a',  'a',  'a',  'c',
  'e',  'e',  'e',  'e',  'i',  'i',  'i',  'i',
  'd',  'n',  'o',  'o',  'o',  'o',  'o',  '÷',
  'o',  'u',  'u',  'u',  'u',  'y',  't',  'y'
};

/*
 * character classification macros and bit table:
 *
 * IS_8NAME(x)  x is allowed in a name (not all letters are)
 * IS_8PRINT(x) x is a printable character (possibly space or NBSP)
 */
/* 0x01 -> allowed in all names
 * 0x02 -> printable
 * 0x04 -> alpha
 * 0x08 -> allowed in obj-names
 * 0x10 -> digit
 * 0x20 -> space
 */
const unsigned char latin1_char_class[256] = {
  /* 0x00-0x1f */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x20, 0x20, 0x20, 0x20, 0x20, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  /* SP - ? */
  0x22, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02,
  0x02, 0x02, 0x02, 0x02, 0x02, 0x0a, 0x02, 0x02, /* - allowed in objnames */
  0x12, 0x12, 0x12, 0x12, 0x12, 0x12, 0x12, 0x12, /* 0 - 7 */
  0x12, 0x12, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02,
  /* @ - _ */
  0x02, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07,
  0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07,
  0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07,
  0x07, 0x07, 0x07, 0x02, 0x02, 0x02, 0x02, 0x02,
  /* ` - DEL */
  0x02, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07,
  0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07,
  0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07,
  0x07, 0x07, 0x07, 0x02, 0x02, 0x02, 0x02, 0x00,
  /* 0x80 - 0x9f */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  /* 0xa0 - 0xbf */
  0x22, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02,
  0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02,
  0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02,
  0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02,
  /* 0xc0 - 0xdf */
  0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x06, 0x06,
  0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07,
  0x06, 0x06, 0x07, 0x07, 0x07, 0x07, 0x07, 0x02,
  0x06, 0x07, 0x07, 0x07, 0x07, 0x07, 0x06, 0x06,
  /* 0xe0 - 0xff */
  0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x06, 0x06,
  0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07,
  0x06, 0x06, 0x07, 0x07, 0x07, 0x07, 0x07, 0x02,
  0x06, 0x07, 0x07, 0x07, 0x07, 0x07, 0x06, 0x06
};

/*
 * convert latin1 to upper case latin1
 */
const unsigned char latin1_to_upper[256] = {
  0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
  0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
  0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
  0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f,
  ' ',  '!',  '"',  '#',  '$',  '%',  '&',  '\'',
  '(',  ')',  '*',  '+',  ',',  '-',  '.',  '/',
  '0',  '1',  '2',  '3',  '4',  '5',  '6',  '7',
  '8',  '9',  ':',  ';',  '<',  '=',  '>',  '?',
  '@',  'A',  'B',  'C',  'D',  'E',  'F',  'G',
  'H',  'I',  'J',  'K',  'L',  'M',  'N',  'O',
  'P',  'Q',  'R',  'S',  'T',  'U',  'V',  'W',
  'X',  'Y',  'Z',  '[',  '\\', ']',  '^',  '_',
  '`',  'A',  'B',  'C',  'D',  'E',  'F',  'G',
  'H',  'I',  'J',  'K',  'L',  'M',  'N',  'O',
  'P',  'Q',  'R',  'S',  'T',  'U',  'V',  'W',
  'X',  'Y',  'Z',  '{',  '|',  '}',  '~',  0x7f,
  0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
  0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
  0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97,
  0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
  0xa0, '¡',  '¢',  '£',  '¤',  '¥',  '¦',  '§',
  '¨',  '©',  'ª',  '«',  '¬',  '­',  '®',  '¯',
  '°',  '±',  '²',  '³',  '´',  'M',  '¶',  '·',
  '¸',  '¹',  'º',  '»',  '¼',  '½',  '¾',  '¿',
  'À',  'Á',  'Â',  'Ã',  'Ä',  'Å',  'Æ',  'Ç',
  'È',  'É',  'Ê',  'Ë',  'Ì',  'Í',  'Î',  'Ï',
  'Ð',  'Ñ',  'Ò',  'Ó',  'Ô',  'Õ',  'Ö',  '×',
  'Ø',  'Ù',  'Ú',  'Û',  'Ü',  'Ý',  'Þ',  'ß',
  'À',  'Á',  'Â',  'Ã',  'Ä',  'Å',  'Æ',  'Ç',
  'È',  'É',  'Ê',  'Ë',  'Ì',  'Í',  'Î',  'Ï',
  'Ð',  'Ñ',  'Ò',  'Ó',  'Ô',  'Õ',  'Ö',  '÷',
  'Ø',  'Ù',  'Ú',  'Û',  'Ü',  'Ý',  'Þ',  'Y'
};

/*
 * convert latin1 to lower case latin1
 */
const unsigned char latin1_to_lower[256] = {
  0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
  0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
  0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
  0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f,
  ' ',  '!',  '"',  '#',  '$',  '%',  '&',  '\'',
  '(',  ')',  '*',  '+',  ',',  '-',  '.',  '/',
  '0',  '1',  '2',  '3',  '4',  '5',  '6',  '7',
  '8',  '9',  ':',  ';',  '<',  '=',  '>',  '?',
  '@',  'a',  'b',  'c',  'd',  'e',  'f',  'g',
  'h',  'i',  'j',  'k',  'l',  'm',  'n',  'o',
  'p',  'q',  'r',  's',  't',  'u',  'v',  'w',
  'x',  'y',  'z',  '[',  '\\',  ']', '^',  '_',
  '`',  'a',  'b',  'c',  'd',  'e',  'f',  'g',
  'h',  'i',  'j',  'k',  'l',  'm',  'n',  'o',
  'p',  'q',  'r',  's',  't',  'u',  'v',  'w',
  'x',  'y',  'z',  '{',  '|',  '}',  '~',  0x7f,
  0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
  0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
  0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97,
  0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
  0xa0, '¡',  '¢',  '£',  '¤',  '¥',  '¦',  '§',
  '¨',  '©',  'ª',  '«',  '¬',  '­',  '®',  '¯',
  '°',  '±',  '²',  '³',  '´',  'µ',  '¶',  '·',
  '¸',  '¹',  'º',  '»',  '¼',  '½',  '¾',  '¿',
  'à',  'á',  'â',  'ã',  'ä',  'å',  'æ',  'ç',
  'è',  'é',  'ê',  'ë',  'ì',  'í',  'î',  'ï',
  'ð',  'ñ',  'ò',  'ó',  'ô',  'õ',  'ö',  '×',
  'ø',  'ù',  'ú',  'û',  'ü',  'ý',  'þ',  'ß',
  'à',  'á',  'â',  'ã',  'ä',  'å',  'æ',  'ç',
  'è',  'é',  'ê',  'ë',  'ì',  'í',  'î',  'ï',
  'ð',  'ñ',  'ò',  'ó',  'ô',  'õ',  'ö',  '÷',
  'ø',  'ù',  'ú',  'û',  'ü',  'ý',  'þ',  'ÿ'
};

/* all of the latin-1 character set 1 - 1:

   0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
   0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
   0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
   0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f,
   ' ',  '!',  '"',  '#',  '$',  '%',  '&',  '\'',
   '(',  ')',  '*',  '+',  ',',  '-',  '.',  '/',
   '0',  '1',  '2',  '3',  '4',  '5',  '6',  '7',
   '8',  '9',  ':',  ';',  '<',  '=',  '>',  '?',
   '@',  'A',  'B',  'C',  'D',  'E',  'F',  'G',
   'H',  'I',  'J',  'K',  'L',  'M',  'N',  'O',
   'P',  'Q',  'R',  'S',  'T',  'U',  'V',  'W',
   'X',  'Y',  'Z',  '[',  '\\',  ']',  '^',  '_',
   '`',  'a',  'b',  'c',  'd',  'e',  'f',  'g',
   'h',  'i',  'j',  'k',  'l',  'm',  'n',  'o',
   'p',  'q',  'r',  's',  't',  'u',  'v',  'w',
   'x',  'y',  'z',  '{',  '|',  '}',  '~',  0x7f,
   0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
   0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
   0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97,
   0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
   0xa0, '¡',  '¢',  '£',  '¤',  '¥',  '¦',  '§',
   '¨',  '©',  'ª',  '«',  '¬',  '­',  '®',  '¯',
   '°',  '±',  '²',  '³',  '´',  'µ',  '¶',  '·',
   '¸',  '¹',  'º',  '»',  '¼',  '½',  '¾',  '¿',
   'À',  'Á',  'Â',  'Ã',  'Ä',  'Å',  'Æ',  'Ç',
   'È',  'É',  'Ê',  'Ë',  'Ì',  'Í',  'Î',  'Ï',
   'Ð',  'Ñ',  'Ò',  'Ó',  'Ô',  'Õ',  'Ö',  '×',
   'Ø',  'Ù',  'Ú',  'Û',  'Ü',  'Ý',  'Þ',  'ß',
   'à',  'á',  'â',  'ã',  'ä',  'å',  'æ',  'ç',
   'è',  'é',  'ê',  'ë',  'ì',  'í',  'î',  'ï',
   'ð',  'ñ',  'ò',  'ó',  'ô',  'õ',  'ö',  '÷',
   'ø',  'ù',  'ú',  'û',  'ü',  'ý',  'þ',  'ÿ'
*/

bool str_is8bit(const char *str)
{
  while (*str)
    if (*str++ & 0x80)
      return true;

  return false;
}

int str8icmp(const char *s1, const char *s2)
{
  int c1, c2;

  while ((c1 = TO_7LOWER(*s1++)) == (c2 = TO_7LOWER(*s2++)) && c1)
    ;

  return (c1 < c2) ? -1 : (c1 != c2);
}

int mem8icmp(const void *_s1, const void *_s2, size_t n)
{
  const char *s1 = _s1, *s2 = _s2;
  int c1, c2;

  while (n-- > 0)
    if ((c1 = TO_7LOWER(*s1++)) != (c2 = TO_7LOWER(*s2++)))
      return (c1 < c2) ? -1 : 1;
  return 0;
}

int str8nicmp(const char *s1, const char *s2, int n)
{
  while (n-- > 0)
    {
      int c1 = TO_7LOWER(*s1++);
      int c2 = TO_7LOWER(*s2++);
      if (c1 != c2)
        return (c1 < c2) ? -1 : (c1 != c2);
      if (c1 == 0)
        return 0;
    }
  return 0;
}

void *mem8ichr(const void *_s, int _c, size_t n)
{
  char c = TO_7LOWER(_c);
  const char *s = _s;
  while (n > 0)
    {
      if (c == TO_7LOWER(*s))
        return (void *)s;
      ++s;
      --n;
    }
  return NULL;
}

void strto7print(char *str)
{
  while (*str)
    {
      *str = TO_7PRINT(*str);
      ++str;
    }
}

void str8lwr(char *str)
{
  while (*str)
    {
      *str = TO_8LOWER(*str);
      ++str;
    }
}

void str7lwr(char *str)
{
  while (*str)
    {
      *str = TO_7LOWER(*str);
      ++str;
    }
}

char *str8cap(char *str)
{
  *str = TO_8UPPER(*str);
  return str;
}

struct char_name {
  unsigned char c;
  const char *name;
};

/* must be kept ascendingly sorted by name */
#define FOR_ISO88591_CHARS(op)                                  \
  op('´', "ACUTE ACCENT"),                                      \
  op('&', "AMPERSAND"),                                         \
  op('\'',  "APOSTROPHE"),                                      \
  op('*', "ASTERISK"),                                          \
  op('¦', "BROKEN BAR"),                                        \
  op('¸', "CEDILLA"),                                           \
  op('¢', "CENT SIGN"),                                         \
  op('^', "CIRCUMFLEX ACCENT"),                                 \
  op(':', "COLON"),                                             \
  op(',', "COMMA"),                                             \
  op('@', "COMMERCIAL AT"),                                     \
  op('©', "COPYRIGHT SIGN"),                                    \
  op('¤', "CURRENCY SIGN"),                                     \
  op('°', "DEGREE SIGN"),                                       \
  op('¨', "DIAERESIS"),                                         \
  op('8', "DIGIT EIGHT"),                                       \
  op('5', "DIGIT FIVE"),                                        \
  op('4', "DIGIT FOUR"),                                        \
  op('9', "DIGIT NINE"),                                        \
  op('1', "DIGIT ONE"),                                         \
  op('7', "DIGIT SEVEN"),                                       \
  op('6', "DIGIT SIX"),                                         \
  op('3', "DIGIT THREE"),                                       \
  op('2', "DIGIT TWO"),                                         \
  op('0', "DIGIT ZERO"),                                        \
  op('÷', "DIVISION SIGN"),                                     \
  op('$', "DOLLAR SIGN"),                                       \
  op('=', "EQUALS SIGN"),                                       \
  op('!', "EXCLAMATION MARK"),                                  \
  op('ª', "FEMININE ORDINAL INDICATOR"),                        \
  op('.', "FULL STOP"),                                         \
  op('`', "GRAVE ACCENT"),                                      \
  op('>', "GREATER-THAN SIGN"),                                 \
  op('-', "HYPHEN-MINUS"),                                      \
  op('¡', "INVERTED EXCLAMATION MARK"),                         \
  op('¿', "INVERTED QUESTION MARK"),                            \
  op('A', "LATIN CAPITAL LETTER A"),                            \
  op('Á', "LATIN CAPITAL LETTER A WITH ACUTE"),                 \
  op('Â', "LATIN CAPITAL LETTER A WITH CIRCUMFLEX"),            \
  op('Ä', "LATIN CAPITAL LETTER A WITH DIAERESIS"),             \
  op('À', "LATIN CAPITAL LETTER A WITH GRAVE"),                 \
  op('Å', "LATIN CAPITAL LETTER A WITH RING ABOVE"),            \
  op('Ã', "LATIN CAPITAL LETTER A WITH TILDE"),                 \
  op('Æ', "LATIN CAPITAL LETTER AE"),                           \
  op('B', "LATIN CAPITAL LETTER B"),                            \
  op('C', "LATIN CAPITAL LETTER C"),                            \
  op('Ç', "LATIN CAPITAL LETTER C WITH CEDILLA"),               \
  op('D', "LATIN CAPITAL LETTER D"),                            \
  op('E', "LATIN CAPITAL LETTER E"),                            \
  op('É', "LATIN CAPITAL LETTER E WITH ACUTE"),                 \
  op('Ê', "LATIN CAPITAL LETTER E WITH CIRCUMFLEX"),            \
  op('Ë', "LATIN CAPITAL LETTER E WITH DIAERESIS"),             \
  op('È', "LATIN CAPITAL LETTER E WITH GRAVE"),                 \
  op('Ð', "LATIN CAPITAL LETTER ETH"),                          \
  op('F', "LATIN CAPITAL LETTER F"),                            \
  op('G', "LATIN CAPITAL LETTER G"),                            \
  op('H', "LATIN CAPITAL LETTER H"),                            \
  op('I', "LATIN CAPITAL LETTER I"),                            \
  op('Í', "LATIN CAPITAL LETTER I WITH ACUTE"),                 \
  op('Î', "LATIN CAPITAL LETTER I WITH CIRCUMFLEX"),            \
  op('Ï', "LATIN CAPITAL LETTER I WITH DIAERESIS"),             \
  op('Ì', "LATIN CAPITAL LETTER I WITH GRAVE"),                 \
  op('J', "LATIN CAPITAL LETTER J"),                            \
  op('K', "LATIN CAPITAL LETTER K"),                            \
  op('L', "LATIN CAPITAL LETTER L"),                            \
  op('M', "LATIN CAPITAL LETTER M"),                            \
  op('N', "LATIN CAPITAL LETTER N"),                            \
  op('Ñ', "LATIN CAPITAL LETTER N WITH TILDE"),                 \
  op('O', "LATIN CAPITAL LETTER O"),                            \
  op('Ó', "LATIN CAPITAL LETTER O WITH ACUTE"),                 \
  op('Ô', "LATIN CAPITAL LETTER O WITH CIRCUMFLEX"),            \
  op('Ö', "LATIN CAPITAL LETTER O WITH DIAERESIS"),             \
  op('Ò', "LATIN CAPITAL LETTER O WITH GRAVE"),                 \
  op('Ø', "LATIN CAPITAL LETTER O WITH STROKE"),                \
  op('Õ', "LATIN CAPITAL LETTER O WITH TILDE"),                 \
  op('P', "LATIN CAPITAL LETTER P"),                            \
  op('Q', "LATIN CAPITAL LETTER Q"),                            \
  op('R', "LATIN CAPITAL LETTER R"),                            \
  op('S', "LATIN CAPITAL LETTER S"),                            \
  op('T', "LATIN CAPITAL LETTER T"),                            \
  op('Þ', "LATIN CAPITAL LETTER THORN"),                        \
  op('U', "LATIN CAPITAL LETTER U"),                            \
  op('Ú', "LATIN CAPITAL LETTER U WITH ACUTE"),                 \
  op('Û', "LATIN CAPITAL LETTER U WITH CIRCUMFLEX"),            \
  op('Ü', "LATIN CAPITAL LETTER U WITH DIAERESIS"),             \
  op('Ù', "LATIN CAPITAL LETTER U WITH GRAVE"),                 \
  op('V', "LATIN CAPITAL LETTER V"),                            \
  op('W', "LATIN CAPITAL LETTER W"),                            \
  op('X', "LATIN CAPITAL LETTER X"),                            \
  op('Y', "LATIN CAPITAL LETTER Y"),                            \
  op('Ý', "LATIN CAPITAL LETTER Y WITH ACUTE"),                 \
  op('Z', "LATIN CAPITAL LETTER Z"),                            \
  op('a', "LATIN SMALL LETTER A"),                              \
  op('á', "LATIN SMALL LETTER A WITH ACUTE"),                   \
  op('â', "LATIN SMALL LETTER A WITH CIRCUMFLEX"),              \
  op('ä', "LATIN SMALL LETTER A WITH DIAERESIS"),               \
  op('à', "LATIN SMALL LETTER A WITH GRAVE"),                   \
  op('å', "LATIN SMALL LETTER A WITH RING ABOVE"),              \
  op('ã', "LATIN SMALL LETTER A WITH TILDE"),                   \
  op('æ', "LATIN SMALL LETTER AE"),                             \
  op('b', "LATIN SMALL LETTER B"),                              \
  op('c', "LATIN SMALL LETTER C"),                              \
  op('ç', "LATIN SMALL LETTER C WITH CEDILLA"),                 \
  op('d', "LATIN SMALL LETTER D"),                              \
  op('e', "LATIN SMALL LETTER E"),                              \
  op('é', "LATIN SMALL LETTER E WITH ACUTE"),                   \
  op('ê', "LATIN SMALL LETTER E WITH CIRCUMFLEX"),              \
  op('ë', "LATIN SMALL LETTER E WITH DIAERESIS"),               \
  op('è', "LATIN SMALL LETTER E WITH GRAVE"),                   \
  op('ð', "LATIN SMALL LETTER ETH"),                            \
  op('f', "LATIN SMALL LETTER F"),                              \
  op('g', "LATIN SMALL LETTER G"),                              \
  op('h', "LATIN SMALL LETTER H"),                              \
  op('i', "LATIN SMALL LETTER I"),                              \
  op('í', "LATIN SMALL LETTER I WITH ACUTE"),                   \
  op('î', "LATIN SMALL LETTER I WITH CIRCUMFLEX"),              \
  op('ï', "LATIN SMALL LETTER I WITH DIAERESIS"),               \
  op('ì', "LATIN SMALL LETTER I WITH GRAVE"),                   \
  op('j', "LATIN SMALL LETTER J"),                              \
  op('k', "LATIN SMALL LETTER K"),                              \
  op('l', "LATIN SMALL LETTER L"),                              \
  op('m', "LATIN SMALL LETTER M"),                              \
  op('n', "LATIN SMALL LETTER N"),                              \
  op('ñ', "LATIN SMALL LETTER N WITH TILDE"),                   \
  op('o', "LATIN SMALL LETTER O"),                              \
  op('ó', "LATIN SMALL LETTER O WITH ACUTE"),                   \
  op('ô', "LATIN SMALL LETTER O WITH CIRCUMFLEX"),              \
  op('ö', "LATIN SMALL LETTER O WITH DIAERESIS"),               \
  op('ò', "LATIN SMALL LETTER O WITH GRAVE"),                   \
  op('ø', "LATIN SMALL LETTER O WITH STROKE"),                  \
  op('õ', "LATIN SMALL LETTER O WITH TILDE"),                   \
  op('p', "LATIN SMALL LETTER P"),                              \
  op('q', "LATIN SMALL LETTER Q"),                              \
  op('r', "LATIN SMALL LETTER R"),                              \
  op('s', "LATIN SMALL LETTER S"),                              \
  op('ß', "LATIN SMALL LETTER SHARP S"),                        \
  op('t', "LATIN SMALL LETTER T"),                              \
  op('þ', "LATIN SMALL LETTER THORN"),                          \
  op('u', "LATIN SMALL LETTER U"),                              \
  op('ú', "LATIN SMALL LETTER U WITH ACUTE"),                   \
  op('û', "LATIN SMALL LETTER U WITH CIRCUMFLEX"),              \
  op('ü', "LATIN SMALL LETTER U WITH DIAERESIS"),               \
  op('ù', "LATIN SMALL LETTER U WITH GRAVE"),                   \
  op('v', "LATIN SMALL LETTER V"),                              \
  op('w', "LATIN SMALL LETTER W"),                              \
  op('x', "LATIN SMALL LETTER X"),                              \
  op('y', "LATIN SMALL LETTER Y"),                              \
  op('ý', "LATIN SMALL LETTER Y WITH ACUTE"),                   \
  op('ÿ', "LATIN SMALL LETTER Y WITH DIAERESIS"),               \
  op('z', "LATIN SMALL LETTER Z"),                              \
  op('{', "LEFT CURLY BRACKET"),                                \
  op('(', "LEFT PARENTHESIS"),                                  \
  op('[', "LEFT SQUARE BRACKET"),                               \
  op('«', "LEFT-POINTING DOUBLE ANGLE QUOTATION MARK"),         \
  op('<', "LESS-THAN SIGN"),                                    \
  op('_', "LOW LINE"),                                          \
  op('¯', "MACRON"),                                            \
  op('º', "MASCULINE ORDINAL INDICATOR"),                       \
  op('µ', "MICRO SIGN"),                                        \
  op('·', "MIDDLE DOT"),                                        \
  op('×', "MULTIPLICATION SIGN"),                               \
  op(0xa0, "NO-BREAK SPACE"),                                   \
  op('¬', "NOT SIGN"),                                          \
  op('#', "NUMBER SIGN"),                                       \
  op('%', "PERCENT SIGN"),                                      \
  op('¶', "PILCROW SIGN"),                                      \
  op('+', "PLUS SIGN"),                                         \
  op('±', "PLUS-MINUS SIGN"),                                   \
  op('£', "POUND SIGN"),                                        \
  op('?', "QUESTION MARK"),                                     \
  op('"', "QUOTATION MARK"),                                    \
  op('®', "REGISTERED SIGN"),                                   \
  op('\\', "REVERSE SOLIDUS"),                                  \
  op('}', "RIGHT CURLY BRACKET"),                               \
  op(')', "RIGHT PARENTHESIS"),                                 \
  op(']', "RIGHT SQUARE BRACKET"),                              \
  op('»', "RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK"),        \
  op('§', "SECTION SIGN"),                                      \
  op(';', "SEMICOLON"),                                         \
  op('­', "SOFT HYPHEN"),                                       \
  op('/', "SOLIDUS"),                                           \
  op(' ', "SPACE"),                                             \
  op('¹', "SUPERSCRIPT ONE"),                                   \
  op('³', "SUPERSCRIPT THREE"),                                 \
  op('²', "SUPERSCRIPT TWO"),                                   \
  op('~', "TILDE"),                                             \
  op('|', "VERTICAL LINE"),                                     \
  op('½', "VULGAR FRACTION ONE HALF"),                          \
  op('¼', "VULGAR FRACTION ONE QUARTER"),                       \
  op('¾', "VULGAR FRACTION THREE QUARTERS"),                    \
  op('¥', "YEN SIGN")

#define __CHAR(a, b) a
#define __NAME(a, b) b
static const char *const iso88591names[] = {
  FOR_ISO88591_CHARS(__NAME)
};
static const unsigned char iso88591chars[] = {
  FOR_ISO88591_CHARS(__CHAR)
};
#undef __CHAR
#undef __NAME

struct cmp_char_data {
  const char *name;
  size_t len;
};

static int cmp_char_name(const void *a, const void *b)
{
  const struct cmp_char_data *data = a;
  const char *const *name = b;
  size_t nlen = strlen(*name);
  size_t clen = data->len < nlen ? data->len : nlen;
  int r = memcmp(data->name, *name, clen);
  if (r != 0)
    return r;
  return clen < nlen ? -1 : clen == data->len ? 0 : 1;
}

int lookup_named_character(const char *name, size_t namelen)
{
  struct cmp_char_data cdata = {
    .name = name,
    .len  = namelen
  };
  const char *const *cname = bsearch(&cdata, iso88591names,
                                     VLENGTH(iso88591names),
                                     sizeof iso88591names[0],
                                     cmp_char_name);
  if (cname == NULL)
    return -1;
  return iso88591chars[cname - iso88591names];
}
