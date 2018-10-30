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

#include "mudlle-config.h"

#include <ctype.h>
#include <errno.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sys/stat.h>
#include <sys/types.h>

#include "alloc.h"
#include "charset.h"
#include "context.h"
#include "error.h"
#include "ports.h"
#include "strbuf.h"
#include "utils.h"

#include "runtime/mudlle-string.h"


#ifdef LOCAL_MUDLLE_TYPES
#undef LOCAL_MUDLLE_TYPES
#define LOCAL_MUDLLE_TYPES                      \
  struct capped_oport *:       true,            \
  struct file_oport *:         true,            \
  struct line_oport *:         true,            \
  struct mudout_oport *:       true,            \
  struct sink_oport *:         true,            \
  struct string_oport_block *: true,
#endif

/* The various types of input & output ports */

static struct string_oport *string_oport_cache;
static int string_oport_cache_used;

struct oport *mudout_port;

#define STRING_BLOCK_SIZE 512	/* Size of each block */

struct string_oport_block /* A structure in which to accumulate output */
{
  struct obj o;
  struct string_oport_block *next;
  struct string *data;
};
/* sizeof(struct string_oport_block) should equal BLOCK_SIZE (see calloc.c)
   exactly, otherwise some memory will be wasted. */

struct string_oport /* Output to a string */
{
  struct oport p;
  struct string_oport_block *first, *current;
  value pos;
};

struct file_oport /* Output to a FILE * */
{
  struct oport p;
  struct tagged_ptr file;
};


static struct string_oport_block *free_blocks;

/* Creation & code for the various types of ports */
/* ---------------------------------------------- */

struct oport *alloc_oport(size_t nfields, const struct oport_methods *m)
{
  struct oport *p = (struct oport *)allocate_record(type_oport, nfields);
  set_oport_methods(p, m);
  return p;
}

#define ALLOC_OPORT(type)                               \
  (CHECK_MUDLLE_TYPE((struct type ## _oport *)0),       \
   (struct type ## _oport *)alloc_oport(                \
     grecord_fields(struct type ## _oport),             \
     &type ## _port_methods))

static struct string_oport *get_string_port(struct oport *p)
{
  assert(is_string_port(p));
  assert(!readonlyp(p));
  return (struct string_oport *)p;
}

static ulong port_length(struct string_oport *p)
{
  struct string_oport_block *current = p->first;
  ulong size;

  size = 0;
  while (current->next)
    {
      size += STRING_BLOCK_SIZE;
      current = current->next;
    }
  return size + intval(p->pos);
}

static struct string_oport_block *new_string_block(void)
{
  if (free_blocks)
    {
      struct string_oport_block *newp = free_blocks;
      assert(!readonlyp(newp));
      GCCHECK(newp);
      free_blocks = free_blocks->next;
      newp->next = NULL;
      return newp;
    }

  struct string_oport_block *newp;
  newp = (struct string_oport_block *)allocate_record(
    type_internal, grecord_fields(*newp));
  GCPRO(newp);
  struct string *s = (struct string *)allocate_string(
    type_internal, STRING_BLOCK_SIZE);
  UNGCPRO();
  newp->data = s;
  return newp;
}

void empty_string_oport(struct oport *_p)
{
  struct string_oport *p = get_string_port(_p);

  assert(p->first);
  assert(p->current);

  if (p->first != p->current)
    {
      p->current->next = free_blocks;
      free_blocks = p->first->next;
      p->first->next = NULL;
      p->current = p->first;
    }

  p->pos = makeint(0);
}

static void free_string_oport(struct string_oport *p)
{
  set_oport_methods(&p->p, NULL);

  for (struct string_oport_block *b = p->first; b; b = b->next)
    {
      GCCHECK(b);
      assert(!readonlyp(b));
    }

  /* Free data (add blocks to free block list) */
  p->current->next = free_blocks;
  free_blocks = p->first;
  p->first = p->current = NULL;
}

static void string_close(struct oport *_p)
{
  struct string_oport *p = get_string_port(_p);
  free_string_oport(p);

  if (string_oport_cache_used < 1024)
    {
      p->first = (void *)string_oport_cache;
      string_oport_cache = p;
      ++string_oport_cache_used;
    }
}

static void string_flush(struct oport *_p)
{
}

static void string_putnc(struct oport *_p, int c, size_t n)
{
  struct string_oport *p = (struct string_oport *)_p;
  assert(!readonlyp(p));
  struct string_oport_block *current = p->current;
  long pos = intval(p->pos);

  while (n > 0)
    {
      size_t left = STRING_BLOCK_SIZE - pos;
      size_t cnt = n < left ? n : left;
      memset(current->data->str + pos, c, cnt);
      n -= cnt;
      pos += cnt;
      if (n == 0)
        break;

      struct string_oport_block *blk;
      GCPRO(p, current);
      blk = new_string_block();
      UNGCPRO();
      p->current = current->next = blk;
      current = p->current;
      pos = 0;
    }
  p->pos = makeint(pos);
}

static void string_write(struct oport *_p, const char *data, size_t nchars)
{
  struct string_oport *p = (struct string_oport *)_p;
  assert(!readonlyp(p));
  struct string_oport_block *current = p->current;
  long pos = intval(p->pos);
  GCPRO(p, current);
  size_t fit;
  while ((fit = STRING_BLOCK_SIZE - pos) < nchars)
    {
      struct string_oport_block *blk = new_string_block();

      memcpy(current->data->str + pos, data, fit);
      p->current = current->next = blk;
      current = p->current;
      data += fit;
      nchars -= fit;
      pos = 0;
    }
  UNGCPRO();
  memcpy(current->data->str + pos, data, nchars);
  p->pos = makeint(pos + nchars);
}

static void string_swrite(struct oport *_p, struct string *s, size_t from,
                          size_t nchars)
{
  struct string_oport *p = (struct string_oport *)_p;
  assert(!readonlyp(p));
  struct string_oport_block *current = p->current;
  int fit;
  long pos = intval(p->pos);

  GCPRO(p, current, s);
  while ((fit = STRING_BLOCK_SIZE - pos) < nchars)
    {
      struct string_oport_block *blk = new_string_block();

      memcpy(current->data->str + pos, s->str + from, fit);
      p->current = current->next = blk;
      current = p->current;
      from += fit;
      nchars -= fit;
      pos = 0;
    }
  UNGCPRO();
  memcpy(current->data->str + pos, s->str + from, nchars);
  p->pos = makeint(pos + nchars);
}

static void string_stat(struct oport *oport, struct oport_stat *buf)
{
  struct string_oport *p = get_string_port(oport);
  *buf = (struct oport_stat){ .size = port_length(p) };
}

static const struct oport_methods string_port_methods = {
  .close  = string_close,
  .putnc  = string_putnc,
  .write  = string_write,
  .swrite = string_swrite,
  .flush  = string_flush,
  .stat   = string_stat,
};

static struct string_oport *new_string_port(void)
{
  if (string_oport_cache)
    {
      struct string_oport *p = string_oport_cache;
      string_oport_cache = (void *)p->first;
      --string_oport_cache_used;
      return p;
    }
  return ALLOC_OPORT(string);
}

static struct string_oport *init_string_oport(struct string_oport *p)
{
  assert(!readonlyp(p));
  GCPRO(p);
  set_oport_methods(&p->p, &string_port_methods);
  struct string_oport_block *blk = new_string_block();
  p->first = p->current = blk;
  p->pos = makeint(0);
  UNGCPRO();
  return p;
}

struct oport *make_string_oport(void)
{
  struct string_oport *p = new_string_port();
  return &init_string_oport(p)->p;
}

struct line_oport {
  struct string_oport soport;
  value prev_char;
  struct tagged_ptr line_methods; /* points to struct line_oport_methods */
  value line_handler_data;
};

static void line_port_close(struct oport *_p)
{
  struct line_oport *p = (struct line_oport *)_p;
  free_string_oport(&p->soport);
}

static void line_port_send(struct line_oport *p)
{
  size_t len = string_port_length(&p->soport.p);
  if (len == 0)
    return;
  struct string_oport_block *current = p->soport.first;
  const struct line_oport_methods *methods = get_tagged_ptr(&p->line_methods);
  GCPRO(p);
  if (current->next == NULL)
    methods->swrite(current->data, intval(p->soport.pos),
                    p->line_handler_data);
  else
    {
      struct string *s = port_string(&p->soport.p, SIZE_MAX);
      methods->swrite(s, string_len(s), p->line_handler_data);
    }
  UNGCPRO();
  p->prev_char = makeint(EOF);
  empty_string_oport(&p->soport.p);
}

static void line_port_putnc(struct oport *_p, int c, size_t n)
{
  struct line_oport *p = (struct line_oport *)_p;

  GCPRO(p);

  while (n > 0)
    {
      if (p->prev_char == makeint('\n') && c != '\r')
        line_port_send(p);

      bool flush_after = ((p->prev_char == makeint('\r') && c == '\n')
                          || (p->prev_char == makeint('\n') && c == '\r'));
      size_t cnt = flush_after ? 1 : n;
      string_putnc(_p, c, cnt);
      n -= cnt;
      if (flush_after)
        line_port_send(p);
      else
        p->prev_char = makeint(c);
    }

  UNGCPRO();
}

static void line_port_write(struct oport *_p, const char *data, size_t nchars)
{
  struct line_oport *p = (struct line_oport *)_p;
  GCPRO(p);

  for (;;)
    {
      if (nchars == 0)
        break;

      if ((p->prev_char == makeint('\r') && data[0] == '\n')
          || (p->prev_char == makeint('\n') && data[0] == '\r'))
        {
          line_port_putnc(_p, data[0], 1);
          ++data;
          if (!--nchars)
            break;
        }
      else if (p->prev_char == makeint('\n'))
        line_port_send(p);

      const char *nl = memchr(data, '\n', nchars);
      if (nl == NULL)
        {
          string_write(&p->soport.p, data, nchars);
          p->prev_char = makeint(data[nchars - 1]);
          break;
        }

      size_t n = nl - data + 1;
      string_write(&p->soport.p, data, n);
      if (n > 1 && data[n - 2] == '\r')
        line_port_send(p);
      else
        p->prev_char = makeint('\n');
      data += n;
      nchars -= n;
      if (!nchars)
        break;
    }

  UNGCPRO();
}

static void line_port_swrite(struct oport *_p, struct string *s, size_t from,
                             size_t nchars)
{
  char *buf = malloc(nchars);
  memcpy(buf, s->str + from, nchars);
  line_port_write(_p, buf, nchars);
  free(buf);
}

static void line_port_flush(struct oport *_p)
{
  struct line_oport *p = (struct line_oport *)_p;
  line_port_send(p);
}

static void line_port_stat(struct oport *_p, struct oport_stat *buf)
{
  struct line_oport *p = (struct line_oport *)_p;
  const struct line_oport_methods *methods = get_tagged_ptr(&p->line_methods);
  methods->stat(buf, p->line_handler_data);
  size_t slen = string_port_length(_p);
  if (slen > buf->size)
    *buf = (struct oport_stat){ .size = slen };
}

static const struct oport_methods line_port_methods = {
  .close  = line_port_close,
  .putnc  = line_port_putnc,
  .write  = line_port_write,
  .swrite = line_port_swrite,
  .flush  = line_port_flush,
  .stat   = line_port_stat,
};

struct oport *make_line_oport(const struct line_oport_methods *methods,
                              value data)
{
  GCPRO(data);
  struct line_oport *p = ALLOC_OPORT(line);
  p = (struct line_oport *)init_string_oport(&p->soport);
  set_oport_methods(&p->soport.p, &line_port_methods);
  p->prev_char = makeint(EOF);
  set_tagged_ptr(&p->line_methods, (void *)methods);
  p->line_handler_data = data;
  UNGCPRO();
  return &p->soport.p;
}

static void file_close(struct oport *_p)
{
  struct file_oport *p = (struct file_oport *)_p;
  FILE *f = get_tagged_ptr(&p->file);
  fclose(f);
  set_tagged_ptr(&p->file, NULL);
}

static void file_flush(struct oport *_p)
{
  struct file_oport *p = (struct file_oport *)_p;
  FILE *f = get_tagged_ptr(&p->file);
  fflush(f);
}

static void file_putnc(struct oport *_p, int c, size_t n)
{
  struct file_oport *p = (struct file_oport *)_p;
  FILE *f = get_tagged_ptr(&p->file);
  if (f)
    while (n-- > 0)
      putc(c, f);
}

static void file_write(struct oport *_p, const char *data, size_t nchars)
{
  struct file_oport *p = (struct file_oport *)_p;
  FILE *f = get_tagged_ptr(&p->file);
  if (f) fwrite(data, nchars, 1, f);
}

static void file_swrite(struct oport *_p, struct string *s, size_t from,
                        size_t nchars)
{
  struct file_oport *p = (struct file_oport *)_p;
  FILE *f = get_tagged_ptr(&p->file);
  if (f) fwrite(s->str + from, nchars, 1, f);
}

static void file_stat(struct oport *_p, struct oport_stat *buf)
{
  struct file_oport *p = (struct file_oport *)_p;
  FILE *f = get_tagged_ptr(&p->file);
  size_t size = 0;
  if (f)
    {
      /* we don't fflush, so the result may be incorrect */
      struct stat fbuf;
      if (fstat(fileno(f), &fbuf) == 0)
        size = fbuf.st_size;
    }
  *buf = (struct oport_stat){ .size = size };
}

struct sink_oport {
  struct oport p;
  value count;                  /* characters written */
};

static void sink_close(struct oport *_p)
{
}

static void sink_use(struct oport *_p, size_t n)
{
  struct sink_oport *p = (struct sink_oport *)_p;
  p->count = mudlle_iadd(p->count, n);
}

static void sink_putnc(struct oport *p, int c, size_t n)
{
  sink_use(p, n);
}

static void sink_write(struct oport *p, const char *data, size_t nchars)
{
  sink_use(p, nchars);
}

static void sink_swrite(struct oport *p, struct string *s, size_t from,
                        size_t nchars)
{
  sink_use(p, nchars);
}

static void sink_flush(struct oport *_p)
{
}

static void sink_stat(struct oport *_p, struct oport_stat *buf)
{
  struct sink_oport *p = (struct sink_oport *)_p;
  *buf = (struct oport_stat){ .size = intval(p->count) };
}

static const struct oport_methods sink_port_methods = {
  .close  = sink_close,
  .putnc  = sink_putnc,
  .write  = sink_write,
  .swrite = sink_swrite,
  .flush  = sink_flush,
  .stat   = sink_stat,
};

struct oport *make_sink_oport(void)
{
  struct sink_oport *p = ALLOC_OPORT(sink);
  p->count = makeint(0);
  return &p->p;
}

static const struct oport_methods file_port_methods = {
  .close  = file_close,
  .putnc  = file_putnc,
  .write  = file_write,
  .swrite = file_swrite,
  .flush  = file_flush,
  .stat   = file_stat,
};

struct oport *make_file_oport(FILE *f)
{
  struct file_oport *p = ALLOC_OPORT(file);
  set_tagged_ptr(&p->file, f);
  return &p->p;
}

bool port_is_empty(struct oport *_p)
/* Return: true if the port is empty
   Requires: p be a string-type output port
*/
{
  struct string_oport *p = get_string_port(_p);
  struct string_oport_block *current = p->first;

  return !current->next && intval(p->pos) == 0;
}

static void port_copy(char *s, struct string_oport *p, size_t maxlen)
{
  for (struct string_oport_block *current = p->first;
       maxlen > 0 && current;
       current = current->next)
    {
      bool last = current->next == NULL;
      size_t n = last ? intval(p->pos) : STRING_BLOCK_SIZE;
      if (n > maxlen)
        n = maxlen;
      memcpy(s, current->data->str, n);
      s += n;
      maxlen -= n;
    }
  *s = 0;
}

void string_port_copy(char *s, struct oport *p, size_t maxlen)
{
  port_copy(s, get_string_port(p), maxlen);
}

size_t string_port_length(struct oport *p)
{
  return port_length(get_string_port(p));
}

struct string *port_string(struct oport *_p, size_t maxlen)
{
  struct string_oport *p = get_string_port(_p);
  struct string *result;

  size_t l = port_length(p);
  if (l > maxlen)
    l = maxlen;
  if (l == 0)
    return static_empty_string;

  if (l > MAX_STRING_SIZE)
    runtime_error(error_bad_value);

  GCPRO(p);
  result = alloc_empty_string(l);
  UNGCPRO();

  port_copy(result->str, p, l);

  return result;
}

char *port_cstring(struct oport *_p)
{
  struct string_oport *p = get_string_port(_p);
  size_t size = port_length(p);

  char *s = xmalloc(size + 1);
  port_copy(s, p, size);

  /* replace NUL by spaces */
  const char *end = s + size;
  for (char *s2 = s; (s2 = memchr(s2, 0, end - s2)); )
    *s2++ = ' ';

  return s;
}

bool port_for_blocks(struct oport *_p,
                     bool (*f)(void *data, struct string *str, size_t len),
                     void *data)
{
  struct string_oport *p = get_string_port(_p);
  struct string_oport_block *current = p->first;
  long pos = intval(p->pos);

  bool result = true;

  GCPRO(current);
  while (current->next)
    {
      if (!f(data, current->data, STRING_BLOCK_SIZE))
        {
          result = false;
          goto done;
        }
      current = current->next;
    }
  f(data, current->data, pos);
 done:
  UNGCPRO();
  return result;
}

void port_append(struct oport *p1, struct oport *_p2)
/* Effects: The characters of port p2 are appended to the end of port p1.
   Modifies: p1
   Requires: p2 be a string-type output port
*/
{
  struct string_oport *p2 = get_string_port(_p2);
  struct string_oport_block *current = p2->first;
  long pos = intval(p2->pos);
  GCPRO(p1, current);
  while (current->next)
    {
      pswrite_substring(p1, current->data, 0, STRING_BLOCK_SIZE);
      current = current->next;
    }
  pswrite_substring(p1, current->data, 0, pos);
  UNGCPRO();
}

/* C I/O routines for use with the ports */
/* ------------------------------------- */

static const char basechars[16] = "0123456789abcdef";

static char *simple_uinttostr(struct intstr *str, unsigned base, unsigned u)
{
  char *pos = str->s + sizeof str->s;
  *--pos = '\0';
  do
    {
      *--pos = basechars[u % base];
      u /= base;
    }
  while (u > 0);
  return pos;
}

static char *simple_inttostr(struct intstr *str, unsigned base, int i)
{
  assert(i != INT_MIN);
  bool minus = i < 0;
  if (minus)
    i = -i;
  char *pos = simple_uinttostr(str, base, i);
  if (minus)
    *--pos = '-';
  return pos;
}

static char *internal_inttostr(struct intstr *str, unsigned base,
                               unsigned long long n,
                               bool is_signed, bool wide)
{
  char *pos = str->s + sizeof str->s;
  *--pos = '\0';

  int i = wide ? 3 : -1;
  bool minus = false;
  if (is_signed && (long long)n < 0)
    {
      minus = true;
      if ((long long)n <= -16)
	{
	  /* this is to take care of LLONG_MIN */
          lldiv_t q = lldiv((long long)n, base);
	  *--pos = basechars[-q.rem];
	  n = q.quot;
	  --i;
	}
      n = -(long long)n;
    }

  do
    {
      if (i == 0)
        {
          *--pos = ',';
          i = 2;
        }
      else
        --i;
      *--pos = basechars[n % base];
      n /= base;
    }
  while (n > 0);
  if (minus)
    *--pos = '-';

  return pos;
}

/* Requires: base be 2, 8, 10 or 16.
   Effects: Prints the ASCII representation of n in base base to str.
   Returns: A pointer to the start of the result.
*/
char *longtostr(struct intstr *str, unsigned base, long n)
{
  if (sizeof (long) == sizeof (int) && n != LONG_MIN)
    return simple_inttostr(str, base, n);
  return internal_inttostr(str, base, n, true, false);
}
char *ulongtostr(struct intstr *str, unsigned base, ulong n)
{
  if (sizeof (long) == sizeof (int))
    return simple_uinttostr(str, base, n);
  return internal_inttostr(str, base, n, false, false);
}
char *ulongtostr_wide(struct intstr *str, ulong n)
{
  return internal_inttostr(str, 10, n, false, true);
}
char *longtostr_wide(struct intstr *str, long n)
{
  return internal_inttostr(str, 10, n, true, true);
}

/* use with printf %s to signal that the next %s should be capitalized */
const char *const CAPITALIZE = "<capitalize>";

void vpprintf(struct oport *p, const char *fmt, va_list args)
{
  if (oport_methods(p) == NULL) return;

  struct intstr ibuf;

  GCPRO(p);

  struct strbuf sbfloat = SBNULL;

  bool cap = false;
  for (const char *percent, *add; (percent = strchr(fmt, '%')); )
    {
      enum {
        sz_char, sz_short, sz_int, sz_long, sz_llong
      } isize = sz_int;
      int width = 0;
      int prec = -1;
      bool minus = false, zero = false, hash = false, plus = false;
      bool space = false, widefmt = false;
      int addlen;
      char chararg;

      opwrite(p, fmt, percent - fmt);
      fmt = percent + 1;

      /* flags */
      for (;; ++fmt)
        {
          switch (*fmt)
            {
            case '-':  minus   = true; continue;
            case '+':  plus    = true; continue;
            case '0':  zero    = true; continue;
            case ' ':  space   = true; continue;
            case '#':  hash    = true; continue;
            case '\'': widefmt = true; continue;
            }
          break;
        }

      if (*fmt == '*')
	{
	  width = va_arg(args, int);
          if (width < 0)
            {
              minus = true;
              width = -width;
            }
	  ++fmt;
	}
      else
        for (unsigned char c; isdigit(c = *fmt); ++fmt)
          width = width * 10 + c - '0';

      if (*fmt == '.')
        {
          ++fmt;
          if (*fmt == '*')
            {
              prec = va_arg(args, int);
              if (prec < 0)
                prec = 0;
              ++fmt;
            }
          else
            {
              prec = 0;
              for (unsigned char c; isdigit(c = *fmt); ++fmt)
                prec = prec * 10 + c - '0';
            }
        }

      switch (*fmt)
        {
        case 'h':
          isize = sz_short;
          if (*++fmt == 'h')
            {
              isize = sz_char;
              ++fmt;
            }
          break;
        case 'l':
          isize = sz_long;
          if (*++fmt == 'l')
            {
              isize = sz_llong;
              ++fmt;
            }
          break;
#define ISIZE(t) _Generic((t)0,                 \
                          int:       sz_int,    \
                          long:      sz_long,   \
                          long long: sz_llong)
        case 'j': isize = ISIZE(intmax_t);  ++fmt; break;
        case 'z': isize = ISIZE(ssize_t);   ++fmt; break;
        case 't': isize = ISIZE(ptrdiff_t); ++fmt; break;
	}
#undef ISIZE

      unsigned char c = *fmt++;
      unsigned base = 10;
      unsigned predigits = 0;
      const char *prefix = "";
      switch (c)
	{
	case '%':
	  add = "%";
          addlen = 1;
          goto have_addlen;
	case 'o':
          base = 8;
          widefmt = false;
          if (hash)
            {
              prefix = "0";
              predigits = 1;
            }
          goto process_unsigned;
	case 'x':
          base = 16;
          widefmt = false;
          if (hash)
            prefix = "0x";
          goto process_unsigned;
	case 'u':
          {
          process_unsigned:
            space = plus = false;

            unsigned long long ull;
            unsigned u;
            switch (isize)
              {
              case sz_char:
                u = (unsigned char)va_arg(args, unsigned);
                break;
              case sz_short:
                u = (unsigned short)va_arg(args, unsigned);
                break;
              case sz_int: u = va_arg(args, unsigned); break;
              case sz_long:
                if (sizeof (long) > sizeof (int))
                  {
                    ull = va_arg(args, unsigned long);
                    goto generic_uint;
                  }
                u = va_arg(args, unsigned);
                break;
              case sz_llong:
                ull = va_arg(args, unsigned long long);
                goto generic_uint;
              }
            if (widefmt)
              {
                ull = u;
                goto generic_uint;
              }
            add = simple_uinttostr(&ibuf, base, u);
            goto add_int;
          generic_uint:
            add = internal_inttostr(&ibuf, base, ull, false, widefmt);
            goto add_int;
          }

	case 'd':
          {
            long long ll;
            int i;
            switch (isize)
              {
              case sz_char:  i = (signed char)va_arg(args, int); break;
              case sz_short: i = (signed short)va_arg(args, int); break;
              case sz_int:   i = va_arg(args, int); break;
              case sz_long:
                if (sizeof (long) > sizeof (int))
                  {
                    ll = va_arg(args, long);
                    goto generic_int;
                  }
                i = va_arg(args, long);
                break;
              case sz_llong:
                ll = va_arg(args, long long);
                goto generic_int;
              }
            if (widefmt || i == INT_MIN)
              {
                ll = i;
                goto generic_int;
              }
            add = simple_inttostr(&ibuf, 10, i);
            goto add_int;

          generic_int:
            add = internal_inttostr(&ibuf, 10, ll, true, widefmt);

          add_int:
            if (minus || prec >= 0)
              zero = false;

            bool neg = (*add == '-');
            if (neg)
              ++add;

            if (strcmp(add, "0") == 0)
              {
                if (!(base == 8 && hash))
                  add = "";
                prefix = "";
                predigits = 0;
              }

            if (prec < 0)
              prec = 1;

            /* 'ilen' is the number of digits */
            size_t ilen = strlen(add) + predigits;

            /* 'tlen' is the total number of characters to print
               before adjusting for 'width' */
            size_t tlen = ilen;
            if (neg || plus || space)
              ++tlen;
            tlen += strlen(prefix) - predigits;

            size_t zeros;
            if (zero)
              zeros = tlen < width ? width - tlen : 0;
            else
              zeros = ilen < prec ? prec - ilen : 0;

            tlen += zeros;
            size_t spaces = tlen < width ? width - tlen : 0;

            if (spaces && !minus)
              pputnc(' ', spaces, p);
            if (neg)
              pputc('-', p);
            else if (plus)
              pputc('+', p);
            else if (space)
              pputc(' ', p);
            pputs(prefix, p);
            pputnc('0', zeros, p);
            pputs(add, p);
            if (spaces && minus)
              pputnc(' ', spaces, p);

            continue;
          }

	case 's':
	  add = va_arg(args, const char *);
          if (add == CAPITALIZE)
            {
              cap = true;
              continue;
            }

	  if (!add) add = "(null)";
	  if (prec >= 0 && !memchr(add, 0, prec))
	    {
	      addlen = prec;
	      goto have_addlen;
	    }
	  break;
	case 'c':
	  chararg = va_arg(args, int);
	  add = &chararg;
          addlen = 1;
          goto have_addlen;
	case 'a':
	case 'e':
	case 'f':
	case 'g':
          {
            sb_empty(&sbfloat);
            sb_addc(&sbfloat, '%');
            if (hash)  sb_addc(&sbfloat, '#');
            if (zero)  sb_addc(&sbfloat, '0');
            if (minus) sb_addc(&sbfloat, '-');
            if (space) sb_addc(&sbfloat, ' ');
            if (plus)  sb_addc(&sbfloat, '+');
            if (width > 0) sb_printf(&sbfloat, "%d", width);
            if (prec >= 0) sb_printf(&sbfloat, ".%d", prec);
            sb_addc(&sbfloat, c);
            sb_addc(&sbfloat, 0);

            /* print the number after the formatting string;
               cf. special-case in sb_vprintf() */
            size_t fmtlen = sb_len(&sbfloat);
            sb_printf(&sbfloat, sb_str(&sbfloat), va_arg(args, double));

            add = sb_str(&sbfloat) + fmtlen;
            addlen = sb_len(&sbfloat) - fmtlen;
            goto have_addlen;
          }

	default:
          abort();
	}

      addlen = strlen(add);
    have_addlen: ;

      size_t npad = width > addlen ? width - addlen : 0;
      if (npad > 0 && !minus)
        pputnc(' ', npad, p);

      if (cap && addlen > 0)
	{
          pputc(TO_8UPPER(add[0]), p);
          opwrite(p, add + 1, addlen - 1);
	}
      else
	opwrite(p, add, addlen);

      cap = false;

      if (npad > 0 && minus)
        pputnc(' ', npad, p);
    }
  pputs(fmt, p);

  UNGCPRO();

  sb_free(&sbfloat);
}

void pprintf(struct oport *p, const char *fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  vpprintf(p, fmt, args);
  va_end(args);
}

bool is_file_port(struct oport *oport)
{
  const struct oport_methods *m = oport_methods(oport);
  return m == &file_port_methods;
}

bool is_string_port(struct oport *oport)
{
  const struct oport_methods *m = oport_methods(oport);
  return m == &string_port_methods || m == &line_port_methods;
}

static void mudout_close(struct oport *p)
{
}

static void mudout_putnc(struct oport *_p, int c, size_t n)
{
  pputnc(c, n, mudout);
}

static void mudout_write(struct oport *p, const char *data, size_t nchars)
{
  opwrite(mudout, data, nchars);
}

static void mudout_swrite(struct oport *p, struct string *s, size_t from,
                          size_t nchars)
{
  pswrite_substring(mudout, s, from, nchars);
}

static void mudout_flush(struct oport *p)
{
  pflush(mudout);
}

static void mudout_stat(struct oport *p, struct oport_stat *buf)
{
  opstat(mudout, buf);
}

static const struct oport_methods mudout_port_methods = {
  .close  = mudout_close,
  .putnc  = mudout_putnc,
  .write  = mudout_write,
  .swrite = mudout_swrite,
  .flush  = mudout_flush,
  .stat   = mudout_stat,
};

struct capped_oport {
  struct oport p;
  value left;                /* max characters to print; -1 signals overflow */
  struct oport *child;       /* where to print for real */
};

static void capped_close(struct oport *p)
{
  struct capped_oport *cport = (struct capped_oport *)p;
  cport->left = makeint(-1);
  cport->child = NULL;
}

static size_t cap_use(struct capped_oport *p, size_t n)
{
  long cap = intval(p->left);
  if (cap < 0)
    return 0;
  if (n > cap)
    {
      n = cap;
      cap = -1;
    }
  else
    cap -= n;
  p->left = makeint(cap);
  return n;
}

static void capped_putnc(struct oport *p, int c, size_t n)
{
  struct capped_oport *cport = (struct capped_oport *)p;
  n = cap_use(cport, n);
  pputnc(c, n, cport->child);
}

static void capped_write(struct oport *p, const char *data, size_t nchars)
{
  struct capped_oport *cport = (struct capped_oport *)p;
  nchars = cap_use(cport, nchars);
  opwrite(cport->child, data, nchars);
}

static void capped_swrite(struct oport *p, struct string *s, size_t from,
                          size_t nchars)
{
  struct capped_oport *cport = (struct capped_oport *)p;
  nchars = cap_use(cport, nchars);
  pswrite_substring(cport->child, s, from, nchars);
}

static void capped_flush(struct oport *p)
{
  struct capped_oport *cport = (struct capped_oport *)p;
  pflush(cport->child);
}

static void capped_stat(struct oport *p, struct oport_stat *buf)
{
  struct capped_oport *cport = (struct capped_oport *)p;
  opstat(cport->child, buf);
}

static const struct oport_methods capped_port_methods = {
  .close  = capped_close,
  .putnc  = capped_putnc,
  .write  = capped_write,
  .swrite = capped_swrite,
  .flush  = capped_flush,
  .stat   = capped_stat,
};

bool capped_port_overflow(const struct oport *p)
{
  struct capped_oport *cport = (struct capped_oport *)p;
  return intval(cport->left) < 0;
}

struct oport *make_capped_oport(struct oport *p, size_t n)
{
  GCPRO(p);
  struct capped_oport *cport = ALLOC_OPORT(capped);
  UNGCPRO();
  cport->left = (n > MAX_TAGGED_INT
                 ? makeint(MAX_TAGGED_INT)
                 : makeint(n));
  cport->child = p;
  return &cport->p;
}

void ports_init(void)
{
  staticpro(&free_blocks);
  staticpro(&string_oport_cache);

  /* mudout_port doesn't have any special fields */
  struct mudout_oport {
    struct oport p;
  };
  mudout_port = &ALLOC_OPORT(mudout)->p;
  staticpro(&mudout_port);
}
