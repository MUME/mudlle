/* $Log: io.c,v $
 * Revision 1.25  1995/07/15  15:25:01  arda
 * Context cleanup.
 * Remove GCDEBUG.
 *
 * Revision 1.24  1995/06/04  14:24:37  arda
 * Rename/move some files, misc. junk
 *
 * Revision 1.23  1995/04/29  20:05:32  arda
 * fix
 *
 * Revision 1.22  1995/01/22  15:11:58  arda
 * Linux patches.
 *
 * Revision 1.21  1994/10/09  06:44:09  arda
 * Libraries
 * Type inference
 * Many minor improvements
 *
 * Revision 1.20  1994/08/31  13:03:38  arda
 * Bug fixes (argh, no, new version of characters structures! (MD))
 *
 * Revision 1.19  1994/08/22  18:03:37  arda
 * Primitives for compiler.
 *
 * Revision 1.18  1994/08/17  10:19:56  arda
 * Improved make depend.
 * basic_load for compiler, select_reactor.
 *
 * Revision 1.17  1994/08/16  19:17:07  arda
 * Added flags to primitives for better calling sequences.
 *
 * Revision 1.14  1994/03/10  19:13:35  arda
 * Last version.
 *
 * Revision 1.13  1994/03/08  01:50:53  arda
 * (MD) New Istari.
 *
 * Revision 1.12  1993/12/07  22:10:49  arda
 * align on zones
 *
 * Revision 1.11  1993/05/02  13:03:06  un_mec
 * Owl: ARGH! Bugs.
 *
 * Revision 1.10  1993/04/24  15:21:07  un_mec
 * Owl: Code cleanup.
 *
 * Revision 1.8  1993/04/10  09:17:48  un_mec
 * Owl: Debug mudlle.
 *
 * Revision 1.7  1993/03/29  09:25:44  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.3  1993/03/14  16:16:41  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.5  1993/01/30  12:14:10  un_mec
 * Owl: Mudlle reactions installed, with loading and editing commands.
 * Also new: room commands, actions (only tell for now).
 *
 * Revision 1.4  1993/01/26  09:49:21  un_mec
 * Owl:
 * - Limit mudlle execution time (prevent infinite loops).
 * - Add mudlle reaction procedures.
 *
 * Revision 1.3  1993/01/11  16:15:42  un_mec
 * Run emacs with security installed. Users may only edit in
 * /home/mud/mume/lib/mudlle/<their name>/.
 * Arata and higher can edit any user's directory.
 * /mudlle can now be opened to all gods (on disun8 initially).
 *
 * Add read-only variables error message.
 *
 * Add some object ops.
 *
 * Revision 1.2  1992/12/30  14:11:58  un_mec
 * Owl:
 * Several changes:
 * - Variables don't have separate value & function cells, instead their are
 *   now 2 types: type_function & type_variable.
 * 	-> new functions store, recall. Removed store-xx, recall-xx.
 * - New types: list (Lisp style pair), vector (array)
 *
 * Revision 1.1  1992/12/27  21:42:18  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

static char rcsid[] = "$Id: io.c,v 1.25 1995/07/15 15:25:01 arda Exp $";

#include "runtime/runtime.h"
#include "print.h"
#include "utils.h"
#include "mparser.h"
#include "interpret.h"
#include <time.h>
#ifndef AMIGA
#include <sys/time.h>
#include <sys/resource.h>
#endif

OPERATION(print, "x -> . Print a representation of x", 1, (value v),
	  OP_LEAF | OP_NOESCAPE)
{
  mprint(mudout, prt_print, v);
  undefined();
}

OPERATION(newline, " -> . Print a newline", 0, (void),
	  OP_LEAF | OP_NOESCAPE)
{
  mputs(EOL, mudout);
  mflush(mudout);
  undefined();
}

OPERATION(display, "x -> . Display a representation of x", 1, (value v),
	  OP_LEAF | OP_NOESCAPE)
{
  mprint(mudout, prt_display, v);
  undefined();
}

OPERATION(examine, "x -> . Examine a representation of x", 1, (value v),
	  OP_LEAF | OP_NOESCAPE)
{
  mprint(mudout, prt_examine, v);
  undefined();
}

TYPEDOP(ctime,
	" -> n. Returns the number of milliseconds of cpu time (use difference only)",
	0, (void),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".n")
{
#ifdef AMIGA
  unsigned int clock[2];

  if (timer(clock)) runtime_error(error_bad_value);

  return (makeint(1000 * (clock[0] % 86400) + clock[1] / 1000));
#else
  struct rusage usage;

  getrusage(RUSAGE_SELF, &usage);
  return (makeint(1000 * usage.ru_utime.tv_sec + usage.ru_utime.tv_usec / 1000));
#endif
}

TYPEDOP(time,
	" -> n. Returns the number of seconds since the 1st of January 1970 GMT",
	0, (void),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".n")
{
  return makeint(time(0));
}

static value _mktime(value t, struct tm *(*convert)(const time_t *time))
{
  struct tm *tm;
  time_t timeval;
  struct vector *vtm;

  ISINT(t);
  timeval = intval(t);

  tm = convert(&timeval);
  vtm = alloc_vector(8);
  vtm->data[0] = makeint(tm->tm_sec);
  vtm->data[1] = makeint(tm->tm_min);
  vtm->data[2] = makeint(tm->tm_hour);
  vtm->data[3] = makeint(tm->tm_mday);
  vtm->data[4] = makeint(tm->tm_mon);
  vtm->data[5] = makeint(tm->tm_year);
  vtm->data[6] = makeint(tm->tm_wday);
  vtm->data[7] = makeint(tm->tm_yday);

  return vtm;
}

TYPEDOP(gmtime,
	"n -> v. Converts time in seconds to a vector of GMT time information",
	1, (value t),
	OP_LEAF | OP_NOESCAPE, "n.v")
{

  return _mktime(t, gmtime);
}

TYPEDOP(localtime,
	"n -> v. Converts time in seconds to a vector of local time information",
	1, (value t),
	OP_LEAF | OP_NOESCAPE, "n.v")
{

  return _mktime(t, localtime);
}

TYPEDOP(asctime,
       "v -> s. Makes a string representing a particular date, as returned by gmtime",
	1, (struct vector *vgmt),
	OP_LEAF | OP_NOESCAPE, "v.s")
{
  struct tm gmt;

  TYPEIS(vgmt, type_vector);
  if (vector_len(vgmt) < 8) runtime_error(error_bad_value);
  ISINT(vgmt->data[0]); gmt.tm_sec = intval(vgmt->data[0]);
  ISINT(vgmt->data[1]); gmt.tm_min = intval(vgmt->data[1]);
  ISINT(vgmt->data[2]); gmt.tm_hour = intval(vgmt->data[2]);
  ISINT(vgmt->data[3]); gmt.tm_mday = intval(vgmt->data[3]);
  ISINT(vgmt->data[4]); gmt.tm_mon = intval(vgmt->data[4]);
  ISINT(vgmt->data[5]); gmt.tm_year = intval(vgmt->data[5]);
  ISINT(vgmt->data[6]); gmt.tm_wday = intval(vgmt->data[6]);
  ISINT(vgmt->data[7]); gmt.tm_yday = intval(vgmt->data[7]);

  gmt.tm_isdst = FALSE;
#ifndef linux
  gmt.tm_zone = "GMT";
  gmt.tm_gmtoff = 0;
#endif

  return alloc_string(asctime(&gmt));
}

void io_init(void)
{
  DEFINE("write", print);
  DEFINE("display", display);
  DEFINE("examine", examine);
  DEFINE("newline", newline);
  DEFINE("ctime", ctime);
  DEFINE("time", time);
  DEFINE("asctime", asctime);
  DEFINE("gmtime", gmtime);
}
