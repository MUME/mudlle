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

#ifndef PORTS_H
#define PORTS_H

#include <stdarg.h>
#include <stdio.h>
#include "mvalues.h"
#include "types.h"

struct oport;

/* Methods for the oport class */
struct oport_methods
{
  void (*close)(struct oport *p); /* Close method */
  void (*putch)(struct oport *p, char c); 
  void (*write)(struct oport *p, const char *data, int nchars);
  void (*swrite)(struct oport *p, struct string *s, int from, int nchars);
  void (*flush)(struct oport *p);
};

struct oport /* A generic output port */
{
  struct obj o;
  struct gtemp *methods;
  /* Each type of output port has specific information after this point */
};

value make_string_outputport(void);
/* Returns: A new string-type output port, with nothing in it.
*/

value make_string_7bit_outputport(void);
/* Returns: A new 7 bit string-type output port, with nothing in it.
*/

value make_file_outputport(FILE *f);
/* Returns: A new file-type output port on file f.
   Note: As there is no finalization, you are responsible for closing f,
     either by closing the port or by closing f.
     Also there is no report of any errors that may occur on f
*/

struct string *port_string(struct oport *p);
/* Returns: A mudlle string representing all the data send to port p.
   Requires: p be a string-type output port
*/

char *port_cstring(struct oport *p);
/* Returns: A C string representing all the data sent to port p.
     The caller is responsible for freeing it.
   Requires: p be a string-type output port
*/

int port_empty(struct oport *p);
/* Return: true if the port is empty
   Requires: p be a string-type output port
*/

void empty_string_oport(struct oport *_p);


void port_append(struct oport *p1, struct oport *p2);
/* Effects: The characters of port p2 are appended to the end of port p1.
   Modifies: p1
   Requires: p2 be a string-type output port
*/

/* C-like I/O routines for ports */
/* They check that the port is not null or closed */

#define opclose(op) \
  do { if ((op) && (op)->methods) (((struct oport_methods *)(op)->methods->external)->close((op))); } while (0)

#define pputc(c, op) \
    do { if ((op) && (op)->methods) (((struct oport_methods *)(op)->methods->external)->putch((op), (c))); } while (0)

#define opwrite(op, s, n) \
  do { if ((op) && (op)->methods) (((struct oport_methods *)(op)->methods->external)->write((op), (s), (n))); } while (0)

#define pswrite(op, s, f, n) \
  do { if ((op) && (op)->methods) (((struct oport_methods *)(op)->methods->external)->swrite((op), (s), (f), (n))); } while (0)

#define pputs_cst(s, op) \
  do { if ((op) && (op)->methods) (((struct oport_methods *)(op)->methods->external)->write((op), (s), (sizeof((s)) - 1))); } while (0)

#define pflush(op) \
  do { if ((op) && (op)->methods) (((struct oport_methods *)(op)->methods->external)->flush((op))); } while (0)

void pputs(const char *s, struct oport *p);
void pprintf(struct oport *p, const char *fmt, ...);

void vpprintf(struct oport *p, const char *fmt, va_list args);

struct string *msprintf(const char *s, ...);

size_t string_port_length(struct oport *oport);
int is_string_port(struct oport *oport);

/* integers are 31 bits long, in base 2 this makes 31 characters +
   sign + null byte + 1 for luck */
#define INTSTRLEN 34

char *int2str(char *str, int base, ulong n, int is_signed);
/* Requires: base be 2, 8, 10 or 16. str be at least INTSTRLEN characters long.
   Effects: Prints the ASCII representation of n in base base to the
     string str.
     If signed is TRUE, n is actually a signed long
   Returns: A pointer to the start of the result.
*/
char *int2str_wide(char *str, ulong n, int is_signed);

void ports_init(void);

#endif
