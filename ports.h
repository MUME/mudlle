/* $Log: ports.h,v $
 * Revision 1.12  1995/07/15  15:24:36  arda
 * Context cleanup.
 * Remove GCDEBUG.
 *
 * Revision 1.11  1995/02/12  18:20:05  arda
 * (CLI) Unknown...
 *
 * Revision 1.10  1995/01/22  15:11:50  arda
 * Linux patches.
 *
 * Revision 1.9  1995/01/21  17:47:47  arda
 * Cli mods
 *
 * Revision 1.8  1994/08/26  18:15:42  arda
 * Minor fixes
 *
 * Revision 1.7  1994/08/26  08:51:44  arda
 * Keep free block list for string ports.
 *
 * Revision 1.6  1994/08/16  19:16:14  arda
 * Mudlle compiler for sparc now fully functional (68k compiler now needs
 * updating for primitives).
 * Changes to allow Sparc trap's for runtime errors.
 * Also added flags to primitives for better calling sequences.
 *
 * Revision 1.3  1993/11/27  11:29:07  arda
 * Owl: Major changes to affect.
 *      Save mudlle data with players & objects.
 *      Change skill format on disk.
 *      Other minor changes.
 *      Still needs full debugging.
 *
 * Revision 1.2  1993/05/29  11:42:13  un_mec
 * Owl: MUME protocol added.
 *
 * Revision 1.1  1993/05/02  07:37:59  un_mec
 * Owl: New output (mudlle ports).
 *
 * Revision 1.1  1992/02/20  17:58:09  gay_d
 * Initial revision
 * */

#ifndef PORTS_H
#define PORTS_H

#include <stdarg.h>
#include "mvalues.h"
#ifdef MUME
#include "struct.socket.h"
#endif

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

struct string *port_string(struct oport *p);
/* Returns: A mudlle string representing all the data send to port p.
   Requires: p be a string-type output port
*/

char *port_cstring(struct oport *p);
/* Returns: A C string representing all the data sent to port p.
     The caller is responsible for freeing it.
   Requires: p be a string-type output port
*/

#ifdef MUME
int port_dump(struct descriptor_data *t);
/* Effects: Sends all data sent to port p to descriptor fd, except for
     the first 'from' characters.
   Returns: 0 if all output sent,
     -1 in case of an error (errno contains more information)
     the # of characters sent otherwise.
   Requires: p be a string-type output port
*/
#endif

void port_append(struct oport *p1, struct oport *p2);
/* Effects: The characters of port p2 are appended to the end of port p1.
   Modifies: p1
   Requires: p2 be a string-type output port
*/

/* C-like I/O routines for ports */
/* These do not check the validity of the port (closed, gone, etc) */

#define opclose(op) \
  (((struct oport_methods *)(op)->methods->external)->close((op)))
#define pputc(c, op) \
  (((struct oport_methods *)(op)->methods->external)->putch((op), (c)))
#define pwrite(op, s, n) \
  (((struct oport_methods *)(op)->methods->external)->write((op), (s), (n)))
#define pswrite(op, s, f, n) \
  (((struct oport_methods *)(op)->methods->external)->swrite((op), (s), (f), (n)))
#define pputs_cst(s, op) \
  (((struct oport_methods *)(op)->methods->external)->write((op), (s), (sizeof((s)) - 1)))
#define pflush(op) \
  ((struct oport_methods *)((op)->methods->external)->flush((op)))

void pputs(const char *s, struct oport *p);
void pprintf(struct oport *p, const char *fmt, ...);
void vpprintf(struct oport *p, const char *fmt, va_list args);

/* integers are 31 bits long, in base 2 this makes 31 characters + sign + null byte + 1 for luck */
#define INTSTRLEN 34

char *int2str(char *str, int base, ulong n, int is_signed);
/* Requires: base be 2, 8, 10 or 16. str be at least INTSTRLEN characters long.
   Effects: Prints the ASCII representation of n in base base to the
     string str.
     If signed is TRUE, n is actually a signed long
   Returns: A pointer to the start of the result.
*/

void ports_init(void);

#endif
