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

#include <time.h>
#ifndef AMIGA
#  include <sys/time.h>
#  ifndef WIN32
#    include <sys/resource.h>
#  endif
#endif

#include "runtime/runtime.h"
#include "print.h"
#include "utils.h"
#include "mparser.h"
#include "interpret.h"
#include "call.h"
#include "io.h"


static struct oport *get_oport(struct oport *oport)
{
  if (TYPE(oport, type_outputport))
    return oport;
  return NULL;
}

TYPEDOP(print, "write", "`x -> . Print a representation of `x", 1, (value v),
        OP_LEAF | OP_NOESCAPE, "x.")
{
  mprint(mudout, prt_print, v);
  undefined();
}

TYPEDOP(newline, 0, " -> . Print a newline", 0, (void),
        OP_LEAF | OP_NOESCAPE, ".")
{
  mputs(EOL, mudout);
  if (mudout) mflush(mudout);
  undefined();
}

TYPEDOP(display, 0, "`x -> . Display a representation of `x", 1, (value v),
        OP_LEAF | OP_NOESCAPE, "x.")
{
  mprint(mudout, prt_display, v);
  undefined();
}

TYPEDOP(examine, 0, "`x -> . Examine a representation of `x", 1, (value v),
        OP_LEAF | OP_NOESCAPE, "x.")
{
  mprint(mudout, prt_examine, v);
  undefined();
}

#ifndef WIN32
TYPEDOP(ctime, 0, " -> `n. Returns the number of milliseconds of CPU"
        " time (use difference only)",
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
#endif


TYPEDOP(time, 0,
	" -> `n. Returns the number of seconds since the 1st of January"
        " 1970 GMT",
	0, (void),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".n")
{
  return makeint(time(NULL));
}

TYPEDOP(time_afterp, "time_after?",
	"`n0 `n1 -> `b. Returns true if time `n0 is after time `n1",
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

TYPEDOP(gmtime, 0,
	"`n -> `v. Converts time in seconds `n, as returned by `time(),"
        " to a vector of GMT time information"
        " [ `sec `min `hour `mday `mon `year `wday `yday]",
	1, (value t),
	OP_LEAF | OP_NOESCAPE, "n.v")
{

  return _mktime(t, gmtime);
}

TYPEDOP(localtime, 0,
	"`n -> `v. Converts time in seconds to a vector of local time"
	" information",
	1, (value t),
	OP_LEAF | OP_NOESCAPE, "n.v")
{

  return _mktime(t, localtime);
}

static void get_tm_struct(struct tm *tm, struct vector *v)
{
  TYPEIS(v, type_vector);

  if (vector_len(v) != time_fields)
    runtime_error(error_bad_value);

  tm->tm_sec  = GETINT(v->data[tm_sec]);
  tm->tm_min  = GETINT(v->data[tm_min]);
  tm->tm_hour = GETINT(v->data[tm_hour]);
  tm->tm_mday = GETINT(v->data[tm_mday]);
  tm->tm_mon  = GETINT(v->data[tm_mon]);
  tm->tm_year = GETINT(v->data[tm_year]);
  tm->tm_wday = GETINT(v->data[tm_wday]);
  tm->tm_yday = GETINT(v->data[tm_yday]);

  tm->tm_isdst = FALSE;
#ifdef HAVE_TM_ZONE
  tm->tm_zone = "GMT";
  tm->tm_gmtoff = 0;
#endif
}

TYPEDOP(asctime, 0,
       "`v -> `s. Makes a string representing a particular date, as returned"
	" by `gmtime(3). Cf. the `tm_xxx constants.",
	1, (struct vector *vgmt),
	OP_LEAF | OP_NOESCAPE, "v.s")
{
  struct tm gmt;
  get_tm_struct(&gmt, vgmt);
  return alloc_string(asctime(&gmt));
}

TYPEDOP(strftime, 0,
	"`s `v -> `s. Convert a gmtime vector into text, as specified by a"
	" `strftime(3) format string. Returns false on error.",
	2, (struct string *fmt, struct vector *vgmt),
	OP_LEAF | OP_NOESCAPE, "sv.S")
{
#ifdef MAX_STRING_LENGTH
  char buffer[MAX_STRING_LENGTH];
#else
  char buffer[4096];
#endif

  struct tm gmt;

  TYPEIS(fmt, type_string);
  get_tm_struct(&gmt, vgmt);
  
  if (strftime(buffer, sizeof buffer, fmt->str, &gmt))
    return alloc_string(buffer);

  return makeint(0);
}

TYPEDOP(with_output, 0, "`oport `f -> `x. Evaluates `f() with output sent"
        " to port."
        " If `p is not a port, just evaluates `f() (no error)."
        " Output is restored when done. Returns the result of `f()",
        2, (value out, value code),
        0, "xf.x")
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

UNSAFEOP(with_output_file, 0,
	 "`s `b `f -> `x. Evaluates `f() with output appended to file `s. "
	 "If `b is false, send all call traces to the file; otherwise, "
	 "only send unhandled ones. Returns the result of `f().",
	 3, (struct string *file, value unhandled_only, value code),
	 0)
{
  struct session_context newp;
  value result, data;
  Mio oport = NULL;
  FILE *f;
  struct gcpro gcpro1, gcpro2;

  callable(code, 0);
  TYPEIS(file, type_string);

  f = fopen(file->str, "a+");
  if (f == NULL)
    runtime_error(error_bad_value);

  GCPRO2(code, oport);
  oport = make_file_outputport(f);

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
  struct gcpro gcpro1, gcpro2, gcpro3;
  ulong l, spos;

  GCPRO3(args, str, p);

  l = string_len(str);
  spos = 0;
  while (spos < l)
    {
      char *percent = memchr(str->str + spos, '%', l - spos);

      if (percent == NULL)
        {
          pswrite(p, str, spos, l - spos);
          break;
        }

      pswrite(p, str, spos, percent - str->str - spos);

      spos = percent - str->str + 1;
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

  if (i != nargs) runtime_error(error_wrong_parameters);

  UNGCPRO();
}

static const typing pformat_tset = { "osx*.", NULL };

FULLOP(pformat, 0, 
       "`oport `s `x1 `x2 ... -> . Outputs formatted string `s to `oport,"
       " with parameters `x1, ... See `format() for syntax. Does nothing if"
       " `oport is not an output port.",
       -1, (struct vector *args, ulong nargs), 0, OP_LEAF, 
       pformat_tset, /* extern */)
{
  struct string *str;
  struct oport *p;
  struct gcpro gcpro1;

  if (nargs < 2) runtime_error(error_wrong_parameters);

  GCPRO1(args);
  p = get_oport(args->data[0]);
  UNGCPRO();
  if (p == NULL)
    undefined();

  str = args->data[1];
  TYPEIS(str, type_string);

  pformat(p, str, args, 2, nargs);

  undefined();
}

static const typing format_tset = { "sx*.s", NULL };

FULLOP(format, 0, 
      "`s `x1 `x2 ... -> `s. Formats string `s with parameters `x1, ..." EOL
      "Special entries are %x, where x can be:" EOL
      "  %   \ta % sign" EOL
      "  c   \tthe character in the next parameter (an int)" EOL
      "  n   \tend of line" EOL
      "  p   \tif the next param is 1 \"\", else \"s\"" EOL
      "  P   \tif the next param is 1 \"y\", else \"ies\"" EOL
      "  s   \ta string repr. of the next param (like display)" EOL
      "  w   \ta string repr. of the next param (like write)",
       -1, (struct vector *args, ulong nargs), 0, OP_LEAF, 
       format_tset, /* extern */)
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

OPERATION(pputc, 0, "`oport `n -> . Print character `n to output port `oport."
          " Does nothing if `oport is not an output port.",
          2, (struct oport *p, value mchar), OP_LEAF)
{
  p = get_oport(p);
  
  if (p == NULL)
    undefined();
  pputc(GETINT(mchar), p);
  undefined();
}

TYPEDOP(pprint, 0, "`oport `s -> . Print `s to output port `oport. Does nothing"
        " if `oport is not an output port.",
        2, (struct oport *p, struct string *s), OP_LEAF, "os.")
{
  struct gcpro gcpro1;

  TYPEIS(s, type_string);
  GCPRO1(s);
  p = get_oport(p);
  UNGCPRO();
  
  if (p == NULL)
    undefined();
  pswrite(p, s, 0, string_len(s));
  undefined();
}

TYPEDOP(pprint_substring, 0,
        "`oport `s `n0 `n1 -> . Print `n1 characters of `s, starting with `n0,"
        " to output port `oport. Does nothing if `oport is not an output"
        " port.",
        4, (struct oport *p, struct string *s, value mstart, value mlength),
        OP_LEAF, "osnn.")
{
  int start = GETINT(mstart), length = GETINT(mlength);
  struct gcpro gcpro1;

  TYPEIS(s, type_string);
  GCPRO1(s);
  p = get_oport(p);
  UNGCPRO();
  if (p == NULL)
    undefined();

  if (start < 0)
    start += string_len(s);
  if (start < 0 || length < 0 || start + length > string_len(s))
    runtime_error(error_bad_index);
  pswrite(p, s, start, length);
  undefined();
}

TYPEDOP(make_string_oport, 0,
       " -> `oport. Returns a new string output port.",
	0, (void),
	OP_LEAF, ".o")
{
  return make_string_outputport();
}

static void check_string_port(struct oport *p)
{
  TYPEIS(p, type_outputport);
  if (!is_string_port(p))
    runtime_error(error_bad_type);
}

TYPEDOP(port_string, 0,
       "`oport -> `s. Returns the contents of string port `oport.",
	1, (struct oport *p),
	OP_LEAF, "o.s")
{
  check_string_port(p);
  return port_string(p);
}

TYPEDOP(string_oport_length, 0,
       "`oport -> `n. Returns the number of characters in the string"
        " port `oport.",
	1, (struct oport *p),
	OP_LEAF, "o.n")
{
  check_string_port(p);
  return makeint(string_port_length(p));
}

OPERATION(port_empty, "port_empty!",
          "`oport -> . Empties the contents of string port `oport.",
          1, (struct oport *p),
          OP_LEAF)
{
  check_string_port(p);
  empty_string_oport(p);
  undefined();
}

OPERATION(add_call_trace_oport, "add_call_trace_oport!",
          "`x `b -> . Also send call traces to `x (an oport"
          " or a character). If `b is TRUE, only send those not handled"
          " otherwise.",
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

OPERATION(remove_call_trace_oport, "remove_call_trace_oport!",
          "`x -> . Stop sending call traces to `x",
	  1, (value oport), OP_LEAF)
{
  if (!TYPE(oport, type_outputport) && !TYPE(oport, type_character))
    runtime_error(error_bad_type);

  remove_call_trace(oport);

  undefined();
}

void io_init(void)
{
  DEFINE(print);
  DEFINE(display);
  DEFINE(examine);
  DEFINE(newline);
#ifndef WIN32
  DEFINE(ctime);
#endif
  DEFINE(time);
  DEFINE(time_afterp);
  DEFINE(asctime);
  DEFINE(strftime);
  DEFINE(gmtime);
  DEFINE(localtime);
  DEFINE(with_output);
  DEFINE(with_output_file);
  DEFINE(pputc);
  DEFINE(pprint);
  DEFINE(pprint_substring);
  DEFINE(make_string_oport);
  DEFINE(port_empty);
  DEFINE(port_string);
  DEFINE(string_oport_length);
  DEFINE(pformat);
  DEFINE(format);

  DEFINE(add_call_trace_oport);
  DEFINE(remove_call_trace_oport);

}
