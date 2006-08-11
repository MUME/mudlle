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

/*
 * Support routines for different charsets
 */

#include "charset.h"

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
    'X',  'Y',  'Z',  '[',  '\\',  ']',  '^',  '_',
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
    'd',  '+',  '2',  '3',  '\'',  'u',  'P',  '*',
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
    'x',  'y',  'z',  '[',  '\\',  ']',  '^',  '_',
    '`',  'a',  'b',  'c',  'd',  'e',  'f',  'g',
    'h',  'i',  'j',  'k',  'l',  'm',  'n',  'o',
    'p',  'q',  'r',  's',  't',  'u',  'v',  'w',
    'x',  'y',  'z',  '{',  '|',  '}',  '~',  0x7f,
    0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
    0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
    0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97,
    0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
    0xa0,  '¡',  '¢',  '£',  '¤',  '¥',  '¦',  '§',
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
 */
const unsigned char latin1_char_class[256] = {
  /* 0x00-0x1f */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  /* SP - ? */
  0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02,
  0x02, 0x02, 0x02, 0x02, 0x02, 0x0A, 0x02, 0x02, /* - allowed in objnames */
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
  0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02,
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
  0x06, 0x07, 0x07, 0x07, 0x07, 0x07, 0x06, 0x00

  /* ÿ (0xff) isn't handled corrently, so make it forbidden */
};

/*
 * convert latin1 to upper case latin1
 */
const unsigned char latin1_to_upper[256] =
{
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
    '`',  'A',  'B',  'C',  'D',  'E',  'F',  'G',
    'H',  'I',  'J',  'K',  'L',  'M',  'N',  'O',
    'P',  'Q',  'R',  'S',  'T',  'U',  'V',  'W',
    'X',  'Y',  'Z',  '{',  '|',  '}',  '~',  0x7F,
    0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
    0x88, 0x89, 0x8A, 0x8B, 0x8C, 0x8D, 0x8E, 0x8F,
    0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97,
    0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
    ' ',  '¡',  '¢',  '£',  '¤',  '¥',  '¦',  '§',
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
const unsigned char latin1_to_lower[256] =
{
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
  ' ',  '¡',  '¢',  '£',  '¤',  '¥',  '¦',  '§',
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
    ' ',  '¡',  '¢',  '£',  '¤',  '¥',  '¦',  '§',
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

int str_is8bit(const char *str)
{
  while (*str)
    if (*str++ & 0x80)
      return 1;

  return 0;
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
