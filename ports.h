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
  size_t size;
};

/* Methods for the oport class */
struct oport_methods
{
  void (*close)(struct oport *p);
  void (*putnc)(struct oport *p, int c, size_t n);
  void (*write)(struct oport *p, const char *data, size_t nchars);
  void (*swrite)(struct oport *p, struct string *s, size_t from,
                 size_t nchars);
  void (*flush)(struct oport *p);
  void (*stat)(struct oport *p, struct oport_stat *buf);
};

struct oport /* A generic output port */
{
  struct obj o;
  struct tagged_ptr methods;
  /* each type of output port has specific information after this point */
};

struct oport *alloc_oport(size_t nfields, const struct oport_methods *m);

struct oport *make_string_oport(void);
/* Returns: A new string-type output port, with nothing in it.
*/

struct oport *make_file_oport(FILE *f);
/* Returns: A new file-type output port on file f.
   Note: As there is no finalization, you are responsible for closing f,
     either by closing the port or by closing f.
     Also there is no report of any errors that may occur on f
*/

struct oport *make_capped_oport(struct oport *oport, size_t n);

bool capped_port_overflow(const struct oport *oport);

struct line_oport_methods {
  /* the contents of 'str' can mutate across calls to swrite() */
  void (*swrite)(struct string *str, size_t len, value data);
  void (*stat)(struct oport_stat *buf, value data);
};
/* call pflush(oport) when done printing to a line oport */
struct oport *make_line_oport(const struct line_oport_methods *methods,
                              value data);

struct oport *make_sink_oport(void);

struct string *port_string(struct oport *p, size_t maxlen);
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

void string_port_copy(char *s, struct oport *p, size_t maxlen);

void port_append(struct oport *p1, struct oport *p2);
/* Effects: The characters of port p2 are appended to the end of port p1.
   Modifies: p1
   Requires: p2 be a string-type output port
*/

/* Call f(data, buf, len) for each data block in string port p, or
   until f() returns false. Returns true iff all f() returned true. */
bool port_for_blocks(struct oport *p,
                     bool (*f)(void *data, struct string *str, size_t len),
                     void *data);

/* C-like I/O routines for ports */
static inline const struct oport_methods *oport_methods(struct oport *p)
{
  if (p == NULL)
    return NULL;
  return get_tagged_ptr(&p->methods);
}

static inline void set_oport_methods(struct oport *p,
                                     const struct oport_methods *m)
{
  set_tagged_ptr(&p->methods, (void *)m);
}

static inline void opclose(struct oport *op)
{
  const struct oport_methods *m = oport_methods(op);
  if (m) m->close(op);
}

static inline void pputnc(int c, size_t n, struct oport *op)
{
  const struct oport_methods *m = oport_methods(op);
  if (m) m->putnc(op, c, n);
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
  if (m)
    m->stat(op, buf);
  else
    *buf = (struct oport_stat){ .size = 0 };
}

static inline void pswrite_substring(struct oport *op, struct string *s,
                                     size_t from, size_t nchars)
{
  const struct oport_methods *m = oport_methods(op);
  if (m) m->swrite(op, s, from, nchars);
}

static inline void pswrite(struct oport *op, struct string *s)
{
  pswrite_substring(op, s, 0, string_len(s));
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

size_t string_port_length(struct oport *oport);
bool is_string_port(struct oport *oport);
bool is_file_port(struct oport *oport);

struct intstr {
  /* integer bits + sign + null */
  char s[CHAR_BIT * sizeof (unsigned long long) + 1 + 1];
};

/* These return a pointer into 'str'. 'base' must be in [2, 8, 10, 16] */
char *longtostr(struct intstr *str, unsigned base, long n);
char *ulongtostr(struct intstr *str, unsigned base, ulong n);
char *longtostr_wide(struct intstr *str, long n);
char *ulongtostr_wide(struct intstr *str, ulong n);

void ports_init(void);

/* printing to this port will output to the current 'mudout' port */
extern struct oport *mudout_port;

/* use with pprintf %s to capitalize the next conversion */
extern const char *const CAPITALIZE;

#endif
