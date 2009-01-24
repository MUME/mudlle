/*
 * Copyright (c) 1993-2006 David Gay and Gustav H�llberg
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

#include <stdarg.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>
#include "alloc.h"
#include "ports.h"
#include "utils.h"
#include "charset.h"


/* The various types of input & output ports */

static struct string_oport *string_oport_cache;
static int string_oport_cache_used;

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
  struct gtemp *file;
};


static struct string_oport_block *free_blocks;

/* Creation & code for the various types of ports */
/* ---------------------------------------------- */

static struct string_oport *get_string_port(struct oport *p)
{
  assert(is_string_port(p));
  return (struct string_oport *)p;
}

static struct string_oport_block *new_string_block(void)
{
  struct string_oport_block *newp;

  if (free_blocks) 
    {
      newp = free_blocks;
      GCCHECK(newp);
      free_blocks = free_blocks->next;
    }
  else
    {
      struct gcpro gcpro1;
      struct string *s;

      s = (struct string *)allocate_string(type_internal, STRING_BLOCK_SIZE);
      GCPRO1(s);
      newp = (struct string_oport_block *)allocate_record(type_internal, 2);
      UNGCPRO();
      newp->data = s;
      GCCHECK(newp);
    }

  newp->next = NULL;
  
  return newp;
}

void empty_string_oport(struct oport *_p)
{
  struct string_oport *p = get_string_port(_p);

  assert(is_string_port(_p));

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

static void free_string_oport(struct oport *_p)
{
  struct string_oport *p = get_string_port(_p);
  p->p.methods = NULL;

  for (struct string_oport_block *b = p->first; b; b = b->next)
    GCCHECK(b);

  /* Free data (add blocks to free block list) */
  p->current->next = free_blocks;
  free_blocks = p->first;
  p->first = p->current = NULL;
}

static void output_string_close(struct oport *_p)
{
  struct string_oport *p = get_string_port(_p);
  free_string_oport(_p);

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
     
static void string_putc(struct oport *_p, char c)
{
  struct string_oport *p = (struct string_oport *)_p;
  struct string_oport_block *current = p->current;
  long pos = intval(p->pos);
  
  if (pos == STRING_BLOCK_SIZE)
    {
      struct string_oport_block *blk;
      struct gcpro gcpro1, gcpro2;

      GCPRO2(p, current);
      blk = new_string_block();
      UNGCPRO();
      p->current = current->next = blk;
      current = p->current;
      p->pos = makeint(pos = 0);
    }
  current->data->str[pos++] = c;
  p->pos = makeint(pos);
}

static void string_write(struct oport *_p, const char *data, int nchars)
{
  struct string_oport *p = (struct string_oport *)_p;
  struct string_oport_block *current = p->current;
  int fit;
  long pos = intval(p->pos);
  struct gcpro gcpro1, gcpro2;

  GCPRO2(p, current);
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

static void string_swrite(struct oport *_p, struct string *s, int from,
                          int nchars)
{
  struct string_oport *p = (struct string_oport *)_p;
  struct string_oport_block *current = p->current;
  int fit;
  long pos = intval(p->pos);
  struct gcpro gcpro1, gcpro2, gcpro3;

  GCPRO2(p, current);
  GCPRO(gcpro3, s);
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

static const struct oport_methods string_port_methods = {
  .close  = output_string_close,
  .putch  = string_putc,
  .write  = string_write,
  .swrite = string_swrite,
  .flush  = string_flush
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
  return (struct string_oport *)allocate_record(type_outputport, 4);
}

static struct gtemp *mstring_port_methods, *mstring_7bit_port_methods;
static struct gtemp *mfile_port_methods, *mline_port_methods;

static struct string_oport *init_string_oport(struct string_oport *p)
{
  struct gcpro gcpro1;
  GCPRO1(p);
  p->p.methods = mstring_port_methods;
  struct string_oport_block *blk = new_string_block();
  p->first = p->current = blk;
  p->pos = makeint(0);
  UNGCPRO();
  return p;
}

value make_string_oport(void)
{
  struct string_oport *p = new_string_port();
  return init_string_oport(p);
}

static void string_7bit_putc(struct oport *_p, char c)
{
  string_putc(_p, TO_7PRINT(c));
}

static void string_7bit_write(struct oport *p, const char *data, int nchars)
{
  char buf[STRING_BLOCK_SIZE];
  struct gcpro gcpro1;

  GCPRO1(p);
  while (nchars)
    {
      int i, fit = sizeof buf < nchars ? sizeof buf : nchars;

      for (i = 0; i < fit; ++i)
	buf[i] = TO_7PRINT(*data++);
      string_write(p, buf, fit);
      nchars -= fit;
    }
  UNGCPRO();
}

static void string_7bit_swrite(struct oport *p, struct string *s, int from,
                               int nchars)
{
  char buf[STRING_BLOCK_SIZE];
  struct gcpro gcpro1, gcpro2;

  GCPRO2(p, s);
  while (nchars)
    {
      int i, fit = sizeof buf < nchars ? sizeof buf : nchars;

      for (i = 0; i < fit; ++i)
	buf[i] = TO_7PRINT(s->str[from + i]);
      string_write(p, buf, fit);
      nchars -= fit;
      from += fit;
    }
  UNGCPRO();
}

struct line_oport {
  struct string_oport soport;
  value prev_char;
  struct gtemp *line_handler;
  value line_handler_data;
};

static void port_copy(char *s, struct string_oport *p);

static void line_port_send(struct line_oport *p)
{
  line_handler_t line_handler = p->line_handler->external;

  size_t len = string_port_length((struct oport *)p);
  if (len == 0)
    return;
  char *line = malloc(len + 1);
  port_copy(line, &p->soport);
  line_handler(line, len, p->line_handler_data);
  free(line);
  empty_string_oport((struct oport *)p);
  p->prev_char = makeint(EOF);
}

static void line_port_putc(struct oport *_p, char c)
{
  struct line_oport *p = (struct line_oport *)_p;

  struct gcpro gcpro1;
  GCPRO1(p);

  if (p->prev_char == makeint('\n') && c != '\r')
    line_port_send(p);
  bool flush_after = p->prev_char == makeint('\r') && c == '\n';
  string_putc(_p, c);
  if (flush_after)
    line_port_send(p);
  else
    p->prev_char = makeint(c);

  UNGCPRO();
}

static void line_port_write(struct oport *_p, const char *data, int nchars)
{
  struct line_oport *p = (struct line_oport *)_p;
  struct gcpro gcpro1;
  GCPRO1(p);

  for (;;)
    {
      if (nchars == 0)
        break;

      if ((p->prev_char == makeint('\r') && data[0] == '\n')
          || (p->prev_char == makeint('\n') && data[0] == '\r'))
        {
          line_port_putc(_p, data[0]);
          ++data;
          if (!--nchars)
            break;
        }
      else if (p->prev_char == makeint('\n'))
        line_port_send(p);

      void *nl = memchr(data, '\n', nchars);
      if (nl == NULL)
        {
          string_write((struct oport *)p, data, nchars);
          p->prev_char = makeint(data[nchars - 1]);
          break;
        }

      size_t n = (const char *)nl - data + 1;
      string_write((struct oport *)p, data, n);
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

static void line_port_swrite(struct oport *_p, struct string *s, int from,
                             int nchars)
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

static const struct oport_methods line_port_methods = {
  .close  = free_string_oport,
  .putch  = line_port_putc,
  .write  = line_port_write,
  .swrite = line_port_swrite,
  .flush  = line_port_flush
};

value make_line_oport(line_handler_t line_handler, value data)
{
  struct line_oport *p
    = (struct line_oport *)allocate_record(type_outputport, 7);

  p = (struct line_oport *)init_string_oport(&p->soport);
  p->soport.p.methods = mline_port_methods;
  p->prev_char = makeint(EOF);
  p->line_handler_data = data;

  struct gcpro gcpro1;
  GCPRO1(p);
  struct gtemp *mh = allocate_temp(type_internal, line_handler);
  p->line_handler = mh;
  UNGCPRO();

  return p;
}

static const struct oport_methods string_7bit_port_methods = {
  .close  = output_string_close,
  .putch  = string_7bit_putc,
  .write  = string_7bit_write,
  .swrite = string_7bit_swrite,
  .flush  = string_flush
};

value make_string_7bit_outputport(void)
{
  struct string_oport *p = new_string_port();
  struct gcpro gcpro1;
  struct string_oport_block *blk;

  GCPRO1(p);
  p->p.methods = mstring_7bit_port_methods;
  blk = new_string_block();
  p->first = p->current = blk;
  p->pos = makeint(0);
  UNGCPRO();

  return p;
}

static void output_file_close(struct oport *_p)
{
  struct file_oport *p = (struct file_oport *)_p;
  FILE *f = p->file->external;

  fclose(f);
  p->file->external = NULL;
}

static void file_flush(struct oport *_p)
{
  struct file_oport *p = (struct file_oport *)_p;
  FILE *f = p->file->external;

  fflush(f);
}
     
static void file_putc(struct oport *_p, char c)
{
  struct file_oport *p = (struct file_oport *)_p;
  FILE *f = p->file->external;

  if (f) putc(c, f);
}

static void file_write(struct oport *_p, const char *data, int nchars)
{
  struct file_oport *p = (struct file_oport *)_p;
  FILE *f = p->file->external;

  if (f) fwrite(data, nchars, 1, f);
}

static void file_swrite(struct oport *_p, struct string *s, int from,
                        int nchars)
{
  struct file_oport *p = (struct file_oport *)_p;
  FILE *f = p->file->external;

  if (f) fwrite(s->str + from, nchars, 1, f);
}

static const struct oport_methods file_port_methods = {
  .close  = output_file_close,
  .putch  = file_putc,
  .write  = file_write,
  .swrite = file_swrite,
  .flush  = file_flush
};

value make_file_oport(FILE *f)
{
  struct file_oport *p = (struct file_oport *)allocate_record(type_outputport,
                                                              2);
  struct gcpro gcpro1;
  struct gtemp *mf;

  GCPRO1(p);
  p->p.methods = mfile_port_methods;
  mf = allocate_temp(type_internal, f);
  p->file = mf;
  UNGCPRO();

  return p;
}

int port_empty(struct oport *_p)
/* Return: true if the port is empty
   Requires: p be a string-type output port
*/
{
  struct string_oport *p = get_string_port(_p);
  struct string_oport_block *current = p->first;

  return !current->next && intval(p->pos) == 0;
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

static void port_copy(char *s, struct string_oport *p)
{
  struct string_oport_block *current = p->first;
  long pos = intval(p->pos);

  while (current->next)
    {
      memcpy(s, current->data->str, STRING_BLOCK_SIZE);
      s += STRING_BLOCK_SIZE;
      current = current->next;
    }
  memcpy(s, current->data->str, pos);
  s[pos] = '\0';
}

size_t string_port_length(struct oport *p)
{
  return port_length(get_string_port(p));
}

struct string *port_string(struct oport *_p)
{
  struct string_oport *p = get_string_port(_p);
  struct gcpro gcpro1;
  struct string *result;

  GCPRO1(p);
  result = alloc_empty_string(port_length(p));
  UNGCPRO();

  port_copy(result->str, p);

  return result;
}

char *port_cstring(struct oport *_p)
{
  struct string_oport *p = get_string_port(_p);
  char *s, *s2;
  size_t size = port_length(p);

  s = xmalloc(size + 1);
  port_copy(s, p);

  s2 = s;
  while ((s2 = memchr(s2, 0, size - (s2 - s))))
    *s2 = ' ';

  return s;
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
  struct gcpro gcpro1, gcpro2;

  GCPRO2(p1, current);
  while (current->next)
    {
      pswrite(p1, current->data, 0, STRING_BLOCK_SIZE);
      current = current->next;
    }
  pswrite(p1, current->data, 0, pos);
  UNGCPRO();
}

/* appends contents of string port '_p2' to 'p1', doubling any
   occurrences of 'esc' */
void port_append_escape(struct oport *p1, struct oport *_p2, int esc)
{
  struct string_oport *p2 = get_string_port(_p2);
  struct string_oport_block *current = p2->first;
  long pos = intval(p2->pos);
  struct gcpro gcpro1, gcpro2;

  GCPRO2(p1, current);
  while (current)
    {
      long start = 0;
      size_t size = current->next ? STRING_BLOCK_SIZE : pos;
      for (;;)
        {
          const char *pesc = memchr(current->data->str + start, esc,
                                    size - start);
          size_t l = pesc ? pesc - (current->data->str + start) + 1 : size - start;
          pswrite(p1, current->data, start, l);
          if (pesc == NULL)
            break;
          pputc(esc, p1);
          start += l;
        }
      current = current->next;
    }
  UNGCPRO();
}

/* searches for character 'c' in string port 'p'; returns the index of the
   first match, or -1 if none found */
ssize_t string_port_search(struct oport *p, int c)
{
  struct string_oport *sp = get_string_port(p);
  struct string_oport_block *current = sp->first;
  ssize_t result = 0;
  while (current)
    {
      size_t used = current->next ? STRING_BLOCK_SIZE : intval(sp->pos);
      const char *found = memchr(current->data->str, c, used);
      if (found)
        return result + (found - current->data->str);
      result += used;
      current = current->next;
    }
  return -1;
}


/* C I/O routines for use with the ports */
/* ------------------------------------- */

void pputs(const char *s, struct oport *p)
{
  opwrite(p, s, strlen(s));
}

static const char basechars[] = "0123456789abcdef";

char *int2str(char *str, int base, ulong n, bool is_signed)
/* Requires: base be 2, 8, 10 or 16. str be at least INTSTRLEN characters long.
   Effects: Prints the ASCII representation of n in base base to the
     string str.
     If is_signed is true, n is actually a long
   Returns: A pointer to the start of the result.
*/
{
  /* ints are 32 bits, the longest number will thus be
     32 digits (in binary) + 1(sign) characters long */
  char *pos = str + INTSTRLEN - 1;
  *--pos = '\0';

  bool minus;
  if (is_signed && (long)n < 0)
    {
      minus = true;
      if ((long)n <= -16)
	{
	  /* this is to take care of LONG_MIN */
	  *--pos = basechars[abs((long)n % base)];
	  n = (long)n / base;
	}
      n = -(long)n;
    }
  else minus = false;

  do {
    *--pos = basechars[n % base];
    n /= base;
  } while (n > 0);
  if (minus) *--pos = '-';

  return pos;
}

char *int2str_wide(char *str, ulong n, bool is_signed)
/* Requires: str be at least INTSTRLEN characters long.
   Effects: Prints the ASCII representation of n in base 10 with
     1000-separation by commas
     If is_signed is true, n is actually a long
   Returns: A pointer to the start of the result.
*/
{
  char *pos = str + INTSTRLEN - 1;
  *--pos = '\0';

  int i = 3;
  bool minus;
  if (is_signed && (long)n < 0)
    {
      minus = true;
      if (n <= -16)
	{
	  /* this is to take care of LONG_MIN */
	  *--pos = basechars[abs(n % 10)];
	  n /= 10;
	  --i;
	}
      n = -(long)n;
    }
  else minus = false;

  do {
    if (!i)
      {
	*--pos = ',';
	i = 3;
      }
    *--pos = basechars[n % 10];
    n /= 10;
    --i;
  } while (n > 0);
  if (minus) *--pos = '-';

  return pos;
}

void vpprintf(struct oport *p, const char *fmt, va_list args)
{
  const char *percent, *add = NULL;
  char buf[INTSTRLEN], padchar;
  struct gcpro gcpro1;

  if (!p || !p->methods) return;
  GCPRO1(p);

  while ((percent = strchr(fmt, '%')))
    {
      bool is_signed = true;
      bool longfmt = false;
      int fsize = 0;
      int fprec = -1;
      bool padright = false;
      bool cap = false;
      bool widefmt = false;
      int base = 10;
      int addlen;

        opwrite(p, fmt, percent - fmt);
      fmt = percent + 1;
      if (*fmt == '-')
	{
	  padright = true;
	  fmt++;
	}

      if (*fmt == '0')
	padchar = '0';
      else
	padchar = ' ';

      if (*fmt == '\'')
	{
	  widefmt = true;
	  fmt++;
	}

      if (*fmt == '*')
	{
	  fsize = va_arg(args, int);
          if (fsize < 0)
            {
              padright = true;
              fsize = -fsize;
            }
	  ++fmt;
	}
      else
	while (isdigit(*(unsigned char *)fmt))
	  fsize = fsize * 10 + *fmt++ - '0';

      if (*fmt == '.')
	{
	  fprec = 0;
	  if (*++fmt == '*')
	    {
	      fprec = va_arg(args, int);
              if (fprec < 0)
                fprec = 0;
	      ++fmt;
	    }
	  else
	    while (isdigit(*(unsigned char *)fmt))
	      fprec = fprec * 10 + *fmt++ - '0';
	}

      if (*fmt == 'l')
	{
          longfmt = true;
          ++fmt;
	}

      unsigned char c = *fmt++;
      switch (c)
	{
	case '%':
	  add = "%";
          addlen = 1;
          goto have_addlen;
	case 'o':
          base = 8;
          goto process_unsigned;
	case 'x':
          base = 16;
	case 'u':
        process_unsigned:
          is_signed = false;
          widefmt = false;
	case 'd': case 'i': {
          ulong ul;
          if (longfmt)
            if (is_signed)
              ul = va_arg(args, long);
            else
              ul = va_arg(args, unsigned long);
          else if (is_signed)
            ul = va_arg(args, int);
          else
            ul = va_arg(args, unsigned);

          if (widefmt)
            add = int2str_wide(buf, ul, is_signed);
          else
            add = int2str(buf, base, ul, is_signed);
	  break;
        }
	case 'S':
	  cap = true;
	case 's':
	  add = va_arg(args, const char *);
	  if (!add) add = "(null)";
	  if (fprec >= 0 && !memchr(add, 0, fprec))
	    {
	      addlen = fprec;
	      goto have_addlen;
	    }
	  break;
	case 'c':
	  add = buf;
	  buf[0] = va_arg(args, int);
          buf[1] = '\0';
          addlen = 1;
          goto have_addlen;
	case 'f':
	  if (fprec >= 0)
	    {
	      char *abuf = alloca(fprec + 16);
	      sprintf(abuf, "%.*f", fprec, va_arg(args, double));
	      add = abuf;
	    }
	  else
	    {
	      sprintf(buf, "%f", va_arg(args, double));
	      add = buf;
	    }
	  break;
	default: assert(0);
	}
      addlen = strlen(add);
    have_addlen:

      if (fsize > 0 && !padright)
	{
	  int i = fsize - addlen;

	  while (--i >= 0) pputc(padchar, p);
	}
      if (cap && addlen > 0)
	{
	    {
	      pputc(toupper(add[0]), p);
	      opwrite(p, add + 1, addlen - 1);
	    }
	}
      else
	opwrite(p, add, addlen);

      if (fsize > 0 && padright)
	{
	  int i = fsize - addlen;

	  while (--i >= 0) pputc(' ', p);
	}
    }
    pputs(fmt, p);

  UNGCPRO();
}

void pprintf(struct oport *p, const char *fmt, ...)
{
  va_list args;
  
  va_start(args, fmt);
  vpprintf(p, fmt, args);
  va_end(args);  
}

/* does a "mudlle sprintf", returning a newly allocated mudlle string
   with the result */
struct string *msprintf(const char *fmt, ...)
{
  struct oport *port = make_string_oport();

  struct gcpro gcpro1;
  GCPRO1(port);

  va_list args;
  va_start(args, fmt);
  vpprintf(port, fmt, args);
  va_end(args);

  UNGCPRO();

  struct string *result = port_string(port);
  opclose(port);

  return result;
}

bool is_string_port(struct oport *oport)
{
  return (oport->methods == mstring_port_methods
          || oport->methods == mstring_7bit_port_methods
          || oport->methods == mline_port_methods);
}

void ports_init(void)
{
  staticpro(&free_blocks);

  mstring_port_methods = allocate_temp(type_internal,
                                       (void *)&string_port_methods);
  staticpro(&mstring_port_methods);

  mstring_7bit_port_methods = allocate_temp(type_internal,
                                            (void *)&string_7bit_port_methods);
  staticpro(&mstring_7bit_port_methods);

  mfile_port_methods = allocate_temp(type_internal,
                                     (void *)&file_port_methods);
  staticpro(&mfile_port_methods);

  mline_port_methods = allocate_temp(type_internal,
                                     (void *)&line_port_methods);
  staticpro(&mline_port_methods);

  staticpro(&string_oport_cache);
}
