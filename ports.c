/* $Log: ports.c,v $
 * Revision 1.25  1995/04/29  20:05:22  arda
 * fix
 *
 * Revision 1.24  1995/03/06  20:28:46  arda
 * make server
 *
 * Revision 1.23  1995/02/12  18:20:04  arda
 * (CLI) Unknown...
 *
 * Revision 1.22  1995/01/21  17:47:46  arda
 * Cli mods
 *
 * Revision 1.21  1995/01/09  17:33:58  arda
 * First masun1 commit?
 *
 * Revision 1.20  1994/10/09  06:42:52  arda
 * Libraries
 * Type inference
 * Many minor improvements
 *
 * Revision 1.19  1994/08/26  08:51:43  arda
 * Keep free block list for string ports.
 *
 * Revision 1.18  1994/08/22  11:18:39  arda
 * Moved code allocation to ins.c
 * Changes for mudlle compiler in MUME.
 *
 * Revision 1.17  1994/08/16  19:16:13  arda
 * Mudlle compiler for sparc now fully functional (68k compiler now needs
 * updating for primitives).
 * Changes to allow Sparc trap's for runtime errors.
 * Also added flags to primitives for better calling sequences.
 *
 * Revision 1.14  1994/02/11  09:59:18  dgay
 * Owl: -Wall
 *      new shared string handling
 *      configuration file
 *
 * Revision 1.13  1994/01/02  15:50:14  arda
 * bug fix
 *
 * Revision 1.12  1993/11/27  17:24:11  arda
 * Owl: The promised bug fixes.
 *
 * Revision 1.11  1993/11/27  11:29:05  arda
 * Owl: Major changes to affect.
 *      Save mudlle data with players & objects.
 *      Change skill format on disk.
 *      Other minor changes.
 *      Still needs full debugging.
 *
 * Revision 1.10  1993/08/28  17:01:27  un_autre
 * SIZE OF ARMORS (CLI)
 *
 * Revision 1.9  1993/08/14  16:43:15  un_mec
 * Owl: Improved vpprintf
 *      New input system (with an input stack) => small changes to interact
 *      New identifier rules (lexer.l)
 *
 * Revision 1.8  1993/07/21  20:36:58  un_mec
 * Owl: Added &&, ||, optimised if.
 *      Added branches to the intermediate language.
 *      Separated destiniation language generation into ins module
 *      (with some peephole optimisation)
 *      Standalone version of mudlle (mkf, runtime/mkf, mudlle.c) added to CVS
 *
 * Revision 1.7  1993/06/28  21:04:07  un_mec
 * Last protocol bugs fixed :-) ?
 *
 * Revision 1.6  1993/06/28  19:12:08  un_autre
 * Bug fix of previous changes:
 *   - /num <x> <regexp>
 *   - improved cprintf (field width)
 *   - edit("person/file")
 *
 * Revision 1.5  1993/06/25  15:37:57  un_autre
 * *** empty log message ***
 *
 * Revision 1.4  1993/06/20  13:35:21  un_mec
 * Owl: edit protocol bug fix.
 *
 * Revision 1.3  1993/05/29  11:42:10  un_mec
 * Owl: MUME protocol added.
 *
 * Revision 1.2  1993/05/07  23:23:59  un_autre
 * *** empty log message ***
 *
 * Revision 1.1  1993/05/02  07:37:57  un_mec
 * Owl: New output (mudlle ports).
 *
 * Revision 1.2  1992/04/11  14:44:11  gay_d
 * Nothing important
 *
 * Revision 1.1  1992/02/20  17:48:08  gay_d
 * Initial revision
 * */

static char rcsid[] = "$Id: ports.c,v 1.25 1995/04/29 20:05:22 arda Exp $";

#include <assert.h>
#include <stdarg.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>
#include "mudlle.h"
#include "types.h"
#include "alloc.h"
#include "ports.h"
#include "utils.h"

#ifdef MUME
#include "handler.socket.h"
#include "../server/comm.h"
#endif

/* The various types of input & output ports */


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

static struct string_oport_block *free_blocks;

/* Creation & code for the various types of ports */
/* ---------------------------------------------- */

static void init_outputport(struct oport *p, struct oport_methods *m)
{
}

static struct string_oport_block *new_string_block(void)
{
  struct string_oport_block *new;

  if (free_blocks) 
    {
      new = free_blocks;
      free_blocks = free_blocks->next;
    }
  else
    {
      struct gcpro gcpro1;
      struct string *s;

      s = (struct string *)allocate_string(type_internal, STRING_BLOCK_SIZE);
      GCPRO1(s);
      new = (struct string_oport_block *)allocate_record(type_internal, 2);
      UNGCPRO();
      new->data = s;
    }

  new->next = NULL;
  
  return new;
}

static void output_string_close(struct oport *_p)
{
  struct string_oport *p = (struct string_oport *)_p;
  
  p->p.methods = NULL;

  /* Free data (add blocks to free block list) */
  p->current->next = free_blocks;
  free_blocks = p->first;
  p->first = p->current = NULL;
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
      struct gcpro gcpro1;

      GCPRO1(p);
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

static void string_swrite(struct oport *_p, struct string *s, int from, int nchars)
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

static struct oport_methods string_port_methods = {
  output_string_close,
  string_putc,
  string_write,
  string_swrite,
  string_flush
};

value make_string_outputport()
{
  struct string_oport *p = (struct string_oport *) allocate_record(type_outputport, 4);
  struct gcpro gcpro1;
  struct gtemp *m;
  struct string_oport_block *blk;

  GCPRO1(p);
  m = allocate_temp(type_internal, &string_port_methods);
  p->p.methods = m;
  blk = new_string_block();
  p->first = p->current = blk;
  p->pos = makeint(0);
  UNGCPRO();

  return p;
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

struct string *port_string(struct oport *_p)
{
  struct string_oport *p = (struct string_oport *)_p;
  struct gcpro gcpro1;
  struct string *result;

  GCPRO1(p);
  result = (struct string *)allocate_string(type_string, port_length(p) + 1);
  UNGCPRO();

  port_copy(result->str, p);

  return result;
}

char *port_cstring(struct oport *_p)
{
  struct string_oport *p = (struct string_oport *)_p;
  char *s;

  s = xmalloc(port_length(p) + 1);
  port_copy(s, p);

  return s;
}

void port_append(struct oport *p1, struct oport *_p2)
/* Effects: The characters of port p2 are appended to the end of port p1.
   Modifies: p1
   Requires: p2 be a string-type output port
*/
{
  struct string_oport *p2 = (struct string_oport *)_p2;
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

#ifdef MUME
int port_dump(struct descriptor_data *t)
/* Effects: Sends all data sent to port p to descriptor fd, except for
     the first 'from' characters.
   Returns: 0 if all output sent,
     -1 in case of an error (errno contains more information)
     the # of characters sent otherwise.
   Requires: p be a string-type output port
*/
{
  ulong sent = 0;
  int cnt, tosend;

  struct oport *_p = t->sending.obj;
  int fd = t->descriptor;
  ulong from = t->sendpos;

  struct string_oport *p = (struct string_oport *)_p;
  struct string_oport_block *current = p->first;

  while (current->next)
    {
      if (from >= STRING_BLOCK_SIZE)
	{
	  from -= STRING_BLOCK_SIZE;
	  current = current->next;
	}
      else
	{
	  tosend = STRING_BLOCK_SIZE - from;

	  cnt = server_mux_send(t,current->data->str + from, tosend);

	  if (cnt < 0)
	    if (errno == EWOULDBLOCK && sent)
	      return sent;	/* 0 is for send complete */
	    else
	      return -1;

	  sent += cnt;
	  if (cnt != tosend) return sent;
	  current = current->next;
	  from = 0;
	}
    }
  tosend = intval(p->pos) - from;
  if (tosend <= 0) return 0;	/* All done */

  cnt = server_mux_send(t,current->data->str + from, tosend);

  if (cnt == tosend) return 0;	/* All done */
  if (cnt < 0)
    if (errno == EWOULDBLOCK && sent)
      return sent;		/* 0 is for send complete */
    else
      return -1;

  return sent + cnt;
}
#endif

/* C I/O routines for use with the ports */
/* ------------------------------------- */

void pputs(const char *s, struct oport *p)
{
  pwrite(p, s, strlen(s));
}

static char basechars[16] = "0123456789abcdef";

char *int2str(char *str, int base, ulong n, int is_signed)
/* Requires: base be 2, 8, 10 or 16. str be at least INTSTRLEN characters long.
   Effects: Prints the ASCII representation of n in base base to the
     string str.
     If is_signed is TRUE, n is actually a long
   Returns: A pointer to the start of the result.
*/
{
  char *pos;
  int minus;

  /* ints are 32 bits, the longest number will thus be
     32 digits (in binary) + 1(sign) characters long */
  pos = str + INTSTRLEN - 1;
  *--pos = '\0';

  if (is_signed && (long)n < 0)
    {
      minus = TRUE;
      n = -(long)n;
    }
  else minus = FALSE;

  do {
    *--pos = basechars[n % base];
    n /= base;
  } while (n > 0);
  if (minus) *--pos = '-';

  return pos;
}

void vpprintf(struct oport *p, const char *fmt, va_list args)
{
  const char *percent, *add = NULL;
  char buf[INTSTRLEN];
  int longfmt, padright, fsize, addlen, cap;
  struct gcpro gcpro1;

  GCPRO1(p);
  while (percent = strchr(fmt, '%'))
    {
      pwrite(p, fmt, percent - fmt);
      fmt = percent + 1;
      longfmt = FALSE;
      fsize = 0;
      padright = FALSE;
      cap = FALSE;
      if (*fmt == '-')
	{
	  padright = TRUE;
	  fmt++;
	}
      while (isdigit(*fmt))
	{
	  fsize = fsize * 10 + *fmt - '0';
	  fmt++;
	}
      if (*fmt == 'l')
	{
	  longfmt = TRUE;
	  fmt++;
	}
      switch (*fmt)
	{
	case '%':
	  add = "%";
	  break;
	case 'd': case 'i':
	  if (longfmt)
	    add = int2str(buf, 10, va_arg(args, long), TRUE);
	  else
	    add = int2str(buf, 10, va_arg(args, int), TRUE);
	  break;
	case 'u':
	  if (longfmt)
	    add = int2str(buf, 10, va_arg(args, unsigned long), FALSE);
	  else
	    add = int2str(buf, 10, va_arg(args, unsigned int), FALSE);
	  break;
	case 'x':
	  if (longfmt)
	    add = int2str(buf, 16, va_arg(args, unsigned long), FALSE);
	  else
	    add = int2str(buf, 16, va_arg(args, unsigned int), FALSE);
	  break;
	case 'o':
	  if (longfmt)
	    add = int2str(buf, 8, va_arg(args, unsigned long), FALSE);
	  else
	    add = int2str(buf, 8, va_arg(args, unsigned int), FALSE);
	  break;
	case 'S':
	  cap = TRUE;
	case 's':
	  add = va_arg(args, const char *);
	  if (!add) add = "(null)";
	  break;
	case 'c':
	  add = buf;
	  buf[0] = va_arg(args, int); buf[1] = '\0';
	  break;
	case 'f':
	  sprintf(buf, "%f", va_arg(args, double));
	  add = buf;
	  break;
	default: assert(0);
	}
      fmt++;

      addlen = strlen(add);
      if (fsize > 0 && !padright)
	{
	  int i = fsize - addlen;

	  while (--i >= 0) pputc(' ', p);
	}
      if (cap && addlen > 0)
	{
	  pputc(toupper(add[0]), p);
	  pwrite(p, add + 1, addlen - 1);
	}
      else
	pwrite(p, add, addlen);
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

void ports_init(void)
{
  staticpro((value *)&free_blocks);
}
