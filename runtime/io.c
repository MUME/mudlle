/*
 * Copyright (c) 1993-2004 David Gay and Gustav Hållberg
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

#include "runtime/runtime.h"
#include "print.h"
#include "utils.h"
#include "mparser.h"
#include "interpret.h"
#include "call.h"
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
  if (mudout) mflush(mudout);
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
#elif defined(hpux)
  return makeint(0);
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

TYPEDOP(time_afterp,
	"n0 n1 -> b. Returns true if time n0 is after time n1",
	2, (value t0, value t1),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "nn.n")
{
  ISINT(t0); ISINT(t1);
  return makebool(uintval(t0) > uintval(t1));
}

static value _mktime(value t, struct tm *(*convert)(const time_t *time))
{
  struct tm *tm;
  time_t timeval;
  struct vector *vtm;

  ISINT(t);
  timeval = uintval(t);

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
#ifdef HAVE_TM_ZONE
  gmt.tm_zone = "GMT";
  gmt.tm_gmtoff = 0;
#endif

  return alloc_string(asctime(&gmt));
}

TYPEDOP(strftime,
       "s v -> s. Convert a gmtime vector into text, as specified by a strftime format string. Return zero on error.",
	2, (struct string *fmt, struct vector *vgmt),
	OP_LEAF | OP_NOESCAPE, "v.s")
{
#ifdef MAX_STRING_LENGTH
  char buffer[MAX_STRING_LENGTH];
#else
  char buffer[4096];
#endif

  struct tm gmt;

  TYPEIS(fmt, type_string);
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
#ifdef HAVE_TM_ZONE
  gmt.tm_zone = "GMT";
  gmt.tm_gmtoff = 0;
#endif

#ifdef MAX_STRING_LENGTH
  if (strftime(buffer, MAX_STRING_LENGTH, fmt->str, &gmt))
#else
  if (strftime(buffer, 4096, fmt->str, &gmt))
#endif
    return alloc_string(buffer);
  else
    return makeint(0);
}

OPERATION(with_output, "oport fn -> . Evaluates fn() with output sent to port.\n\
If p is not a port, just evaluates fn() (no error).\n\
Output is restored when done",
	  2, (value out, value code),
	  0)
{
  struct session_context newp;
  value result, data;
  Mio newout = mudout, newerr = muderr;

  callable(code, 0);
  if (TYPE(out, type_outputport)) newout = newerr = out;

  session_start(&newp, minlevel, muduser, newout, newerr);
  session_context->data = session_context->parent->data;
  result = mcatch_call0(code);
  data = session_context->data;
  session_end();
  session_context->data = data;

  if (exception_signal) /* Continue with exception handling */
    mthrow(exception_signal, exception_value);

  return result;
}

UNSAFEOP(with_output_file,
	 "s b f -> . Evaluates fn() with output appended to file s. "
	 "If b is false, send all call traces to the file; otherwise, "
	 "only send unhandled ones",
	 3, (struct string *file, value unhandled_only, value code),
	 0)
{
  struct session_context newp;
  value result, data;
  Mio oport;
  FILE *f;
  struct gcpro gcpro1;

  callable(code, 0);
  TYPEIS(file, type_string);

  f = fopen(file->str, "a+");
  if (f == NULL)
    runtime_error(error_bad_value);

  oport = make_file_outputport(f);
  GCPRO1(oport);

  if (!istrue(unhandled_only))
    add_call_trace(oport, 0);

  session_start(&newp, minlevel, muduser, oport, oport);
  session_context->data = session_context->parent->data;
  result = mcatch_call0(code);
  data = session_context->data;
  session_end();
  session_context->data = data;

  UNGCPRO();
  fclose(f);
  oport->methods = NULL;

  if (exception_signal) /* Continue with exception handling */
    mthrow(exception_signal, exception_value);

  return result;
}

static void pformat(struct oport *p, struct string *str,
		    struct vector *args, int i, int nargs)
{
  ulong l, spos;
  struct gcpro gcpro1, gcpro2, gcpro3;

  GCPRO3(args, str, p);

  l = string_len(str);
  spos = 0;
  while (spos < l)
    if (str->str[spos] == '%')
      {
	spos++;
	if (spos == l) runtime_error(error_bad_value);
	switch (str->str[spos])
	  {
	  default: runtime_error(error_bad_value);
	  case '%': pputc('%', p); break;
	  case 'c':
	    if (i >= nargs) runtime_error(error_wrong_parameters);
	    ISINT(args->data[i]);
	    pputc(intval(args->data[i++]), p);
	    break;
	  case 'n': pputs(EOL, p); break;
	  case 'p':
	    if (i >= nargs) runtime_error(error_wrong_parameters);
	    ISINT(args->data[i]);
	    if (intval(args->data[i++]) != 1) pputc('s', p);
	    break;
	  case 'P':
	    if (i >= nargs) runtime_error(error_wrong_parameters);
	    ISINT(args->data[i]);
	    if (intval(args->data[i++]) != 1) pputs("ies", p);
	    else pputc('y', p);
	    break;
	  case 's':
	    if (i >= nargs) runtime_error(error_wrong_parameters);
	    output_value(p, prt_display, args->data[i++]);
	    break;
	  case 'w':
	    if (i >= nargs) runtime_error(error_wrong_parameters);
	    output_value(p, prt_print, args->data[i++]);
	    break;
	  }
	spos++;
      }
    else
      {
	pputc(str->str[spos], p);
	spos++;
      }

  if (i != nargs) runtime_error(error_wrong_parameters);

  UNGCPRO();
}

VAROP(pformat, "oport s x1 x2 ... -> . Outputs formatted string s to port, with parameters x1, ... See format() for syntax",
      OP_LEAF)
{
  struct string *str;
  struct oport *p;

  if (nargs < 2) runtime_error(error_wrong_parameters);

  str = args->data[1];
  TYPEIS(str, type_string);

  p = args->data[0];
  TYPEIS(p, type_outputport);

  pformat(p, str, args, 2, nargs);

  undefined();
}

VAROP(format, 
      "s x1 x2 ... -> s. Formats string s with parameters x1, ..." EOL
      "Special entries are %x, where x can be:" EOL
      "  %   a % sign" EOL
      "  c   the character in the next parameter (an int)" EOL
      "  n   end of line" EOL
      "  p   if the next param is 1 \"\", else \"s\"" EOL
      "  P   if the next param is 1 \"y\", else \"ies\"" EOL
      "  s   a string repr. of the next param (like display)" EOL
      "  w   a string repr. of the next param (like write)",
      OP_LEAF)
{
  struct string *str;
  struct gcpro gcpro1;
  struct oport *p;

  if (nargs < 1) runtime_error(error_wrong_parameters);
  GCPRO1(args);
  p = make_string_outputport();
  UNGCPRO();
  str = args->data[0];
  TYPEIS(str, type_string);

  GCPRO1(p);
  pformat(p, str, args, 1, nargs);
  str = port_string(p);
  UNGCPRO();
  opclose(p);
  return str;
}

TYPEDOP(make_string_oport,
       " -> oport. Returns a new string output port.",
	0, (void),
	OP_LEAF, ".o")
{
  return make_string_outputport();
}

TYPEDOP(port_string,
       "oport -> s. Returns the contents of string port oport.",
	1, (struct oport *p),
	OP_LEAF, "o.s")
{
  TYPEIS(p, type_outputport);
  /* Warning: need to check that this is a string output port!
     But: the only externally visible output ports are of that kind,
     so not a problem so far. */

  return port_string(p);
}


OPERATION(add_call_trace_oport, "x b -> . Also send call traces to x (an oport or a character). If b is TRUE, only send those not handled otherwise.",
	  2, (value oport, value only_unhandled), OP_LEAF)
{
  struct gcpro gcpro1;

  if (!TYPE(oport, type_outputport) && !TYPE(oport, type_character))
    runtime_error(error_bad_type);

  GCPRO1(oport);
  remove_call_trace(oport);
  UNGCPRO();

  add_call_trace(oport, istrue(only_unhandled));
  undefined();
}

OPERATION(remove_call_trace_oport, "x -> . Stop sending call traces to x",
	  1, (value oport), OP_LEAF)
{
  if (!TYPE(oport, type_outputport) && !TYPE(oport, type_character))
    runtime_error(error_bad_type);

  remove_call_trace(oport);

  undefined();
}

void io_init(void)
{
  DEFINE("write", print);
  DEFINE("display", display);
  DEFINE("examine", examine);
  DEFINE("newline", newline);
  DEFINE("ctime", ctime);
  DEFINE("time", time);
  DEFINE("time_after?", time_afterp);
  DEFINE("asctime", asctime);
  DEFINE("strftime", strftime);
  DEFINE("gmtime", gmtime);
  DEFINE("localtime", localtime);
  DEFINE("with_output", with_output);
  DEFINE("with_output_file", with_output_file);
  DEFINE("make_string_oport", make_string_oport);
  DEFINE("port_string", port_string);
  DEFINE("pformat", pformat);
  DEFINE("format", format);

  DEFINE("add_call_trace_oport!", add_call_trace_oport);
  DEFINE("remove_call_trace_oport!", remove_call_trace_oport);

}
