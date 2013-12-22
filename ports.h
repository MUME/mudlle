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

#ifndef PORTS_H
#define PORTS_H

#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#include "mvalues.h"

struct oport;

struct oport_stat {
  enum {
    oport_type_string,
    oport_type_file,
    oport_type_user,
  } type;
  size_t size;
};

/* Methods for the oport class */
struct oport_methods
{
  void (*close)(struct oport *p);
  void (*putch)(struct oport *p, int c, size_t n);
  void (*write)(struct oport *p, const char *data, size_t nchars);
  void (*swrite)(struct oport *p, struct string *s, size_t from, size_t nchars);
  void (*flush)(struct oport *p);
  void (*stat)(struct oport *p, struct oport_stat *buf);
};

struct oport /* A generic output port */
{
  struct obj o;
  struct gtemp *methods;
  /* Each type of output port has specific information after this point */
};

value make_string_oport(void);
/* Returns: A new string-type output port, with nothing in it.
*/

value make_file_oport(FILE *f);
/* Returns: A new file-type output port on file f.
   Note: As there is no finalization, you are responsible for closing f,
     either by closing the port or by closing f.
     Also there is no report of any errors that may occur on f
*/

typedef struct {
  void (*write)(const char *str, size_t len, value data);
  void (*stat)(struct oport_stat *buf, value data);
} line_oport_methods_t;
value make_line_oport(const line_oport_methods_t *methods, value data);

struct string *port_string(struct oport *p);
/* Returns: A mudlle string representing all the data send to port p.
   Requires: p be a string-type output port
*/

char *port_cstring(struct oport *p);
/* Returns: A C string representing all the data sent to port p.
     The caller is responsible for freeing it.
   Requires: p be a string-type output port
*/

bool port_is_empty(struct oport *p);
/* Return: true if the port is empty
   Requires: p be a string-type output port
*/

void empty_string_oport(struct oport *_p);

ssize_t string_port_search(struct oport *p, int c);

void port_append(struct oport *p1, struct oport *p2);
/* Effects: The characters of port p2 are appended to the end of port p1.
   Modifies: p1
   Requires: p2 be a string-type output port
*/

void port_append_substring(struct oport *p1, struct oport *p2,
                           size_t start, size_t length);
/* Effects: Starting at character start, length characters of port p2
     are appended to the end of port p1.
   Modifies: p1
   Requires: p2 be a string-type output port */

/* Call f(data, buf, len) for each data block in string port p, or
   until f() returns false */
void port_for_blocks(struct oport *p,
                     bool (*f)(void *data, const char *buf, size_t len),
                     void *data);

void port_append_escape(struct oport *p1, struct oport *p2, int esc);
/* Effects: The characters of port p2 are appended to the end of port
   p1, and all 'esc' characters are doubled.  Modifies: p1 Requires:
   p2 be a string-type output port
*/

/* C-like I/O routines for ports */
static inline const struct oport_methods *oport_methods(struct oport *op)
{
  return op && op->methods ? op->methods->external : NULL;
}

static inline void opclose(struct oport *op)
{
  const struct oport_methods *m = oport_methods(op);
  if (m) m->close(op);
}

static inline void pputnc(int c, size_t n, struct oport *op)
{
  const struct oport_methods *m = oport_methods(op);
  if (m) m->putch(op, c, n);
}

static inline void pputc(int c, struct oport *op)
{
  pputnc(c, 1, op);
}

static inline void opwrite(struct oport *op, const char *s, size_t nchars)
{
  const struct oport_methods *m = oport_methods(op);
  if (m) m->write(op, s, nchars);
}

static inline void pputs(const char *s, struct oport *op)
{
  opwrite(op, s, strlen(s));
}

static inline void opstat(struct oport *op, struct oport_stat *buf)
{
  const struct oport_methods *m = oport_methods(op);
  if (m) m->stat(op, buf);
}

static inline void pswrite(struct oport *op, struct string *s,
                           size_t from, size_t nchars)
{
  const struct oport_methods *m = oport_methods(op);
  if (m) m->swrite(op, s, from, nchars);
}

static inline void pflush(struct oport *op)
{
  const struct oport_methods *m = oport_methods(op);
  if (m) m->flush(op);
}

void pprintf(struct oport *p, const char *fmt, ...)
  FMT_PRINTF(2, 3);

void vpprintf(struct oport *p, const char *fmt, va_list args)
  FMT_PRINTF(2, 0);

struct string *msprintf(const char *s, ...)
  FMT_PRINTF(1, 2);

size_t string_port_length(struct oport *oport);
bool is_string_port(struct oport *oport);

/* integers are 31 bits long, in base 2 this makes 31 characters +
   sign + null byte + 1 for luck */
#define INTSTRLEN 34

char *int2str(char *str, int base, ulong n, bool is_signed);
/* Requires: base be 2, 8, 10 or 16. str be at least INTSTRLEN characters long.
   Effects: Prints the ASCII representation of n in base base to the
     string str.
     If signed is true, n is actually a signed long
   Returns: A pointer to the start of the result.
*/
char *int2str_wide(char *str, ulong n, bool is_signed);

void ports_init(void);

#endif
