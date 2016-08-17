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

#include <ctype.h>
#include <time.h>
#include <unistd.h>

#include <sys/time.h>
#include <sys/times.h>
#ifndef WIN32
#  include <sys/resource.h>
#endif

#include "../call.h"
#include "../context.h"
#include "../interpret.h"
#include "../mparser.h"
#include "../ports.h"
#include "../print.h"
#include "../strbuf.h"
#include "../utils.h"

#include "io.h"
#include "mudlle-float.h"
#include "runtime.h"


struct oport *get_oport(struct oport *oport)
{
  if (TYPE(oport, type_oport))
    return oport;
  return NULL;
}

TYPEDOP(newline, 0, "-> . Print a newline", 0, (void),
        OP_LEAF | OP_NOESCAPE, ".")
{
  pputc('\n', mudout);
  if (mudout) pflush(mudout);
  undefined();
}

static value pprint(struct oport *p, enum prt_level level, value v)
{
  GCPRO1(v);
  p = get_oport(p);
  if (p)
    output_value(p, level, false, v);
  UNGCPRO();
  undefined();
}

TYPEDOP(pdisplay, 0, "`oport `x -> . Print a representation of `x to"
        " `oport as `display() does."
        " Does nothing if `oport is not an output port.", 2,
        (struct oport *p, value x), OP_LEAF | OP_NOESCAPE | OP_STR_READONLY,
        "xx.")
{
  return pprint(p, prt_display, x);
}

TYPEDOP(pwrite, 0, "`oport `x -> . Print a representation of `x to"
        " `oport as `write() does."
        " Does nothing if `oport is not an output port.", 2,
        (struct oport *p, value x), OP_LEAF | OP_NOESCAPE | OP_STR_READONLY,
        "xx.")
{
  return pprint(p, prt_write, x);
}

TYPEDOP(pexamine, 0, "`oport `x -> . Print a representation of `x to"
        " `oport as `examine() does."
        " Does nothing if `oport is not an output port.", 2,
        (struct oport *p, value x), OP_LEAF | OP_NOESCAPE | OP_STR_READONLY,
        "xx.")
{
  return pprint(p, prt_examine, x);
}

TYPEDOP(write, 0, "`x -> . Print a representation of `x", 1, (value v),
        OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "x.")
{
  output_value(mudout, prt_write, false, v);
  if (mudout) pflush(mudout);
  undefined();
}

TYPEDOP(display, 0, "`x -> . Display a representation of `x", 1, (value v),
        OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "x.")
{
  output_value(mudout, prt_display, false, v);
  if (mudout) pflush(mudout);
  undefined();
}

TYPEDOP(examine, 0, "`x -> . Examine a representation of `x", 1, (value v),
        OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "x.")
{
  output_value(mudout, prt_examine, false, v);
  if (mudout) pflush(mudout);
  undefined();
}

#ifndef WIN32
static long clk_per_sec;

TYPEDOP(ctime, 0, "-> `n. Returns the number of milliseconds of CPU"
        " time (use difference only)",
	0, (void),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".n")
{
  struct tms tms;
  times(&tms);
  unsigned long long t = tms.tms_utime;
  t *= 1000;
  t /= clk_per_sec;
  return makeint((long)t);
}
#endif


TYPEDOP(time, 0,
	"-> `n. Returns the number of seconds since the 1st of January"
        " 1970 UTC. On 32-bit systems, negative values are used for values"
        " greater than `MAXINT (following Jan 10 13:37:03 2004 UTC).\n"
        "Cf. `time_after().",
	0, (void),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".n")
{
  return makeint(time(NULL));
}

static enum runtime_error get_msecs(value m, time_t *t)
{
  if (!integerp(m))
    return error_bad_type;
  long l = GETINT(m);
  if (l < 0)
    {
      /* negative values are treated as 31-bit overflows */
      l += 1UL << 31;
      if (l < 0)
        return error_bad_value;
    }
  *t = l;
  if (*t != l)
    return error_bad_value;
  return error_none;
}

TYPEDOP(time_afterp, "time_after?",
	"`n0 `n1 -> `b. Returns true if time `n0 is after time `n1, as"
        " returned from `time().",
	2, (value mt0, value mt1),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST, "nn.n")
{
  time_t t0, t1;
  enum runtime_error error = get_msecs(mt0, &t0);
  if (error != error_none)
    goto got_error;
  error = get_msecs(mt1, &t1);
  if (error != error_none)
    goto got_error;
  return makebool(t0 > t1);

 got_error:
  primitive_runtime_error(error, &op_time_afterp, 2, mt0, mt1);
}

static value make_tm(value t, struct tm *(*convert)(const time_t *time),
                     const struct primitive_ext *op)
{
  enum runtime_error error;
  if (!integerp(t))
    {
      error = error_bad_type;
      goto got_error;
    }

  time_t timeval;
  error = get_msecs(t, &timeval);
  if (error != error_none)
    goto got_error;

  struct tm *tm = convert(&timeval);
  if (tm == NULL)
    {
      error = error_bad_value;
      goto got_error;
    }

  struct vector *vtm = alloc_vector(time_fields);
  vtm->data[tm_sec]  = makeint(tm->tm_sec);
  vtm->data[tm_min]  = makeint(tm->tm_min);
  vtm->data[tm_hour] = makeint(tm->tm_hour);
  vtm->data[tm_mday] = makeint(tm->tm_mday);
  vtm->data[tm_mon]  = makeint(tm->tm_mon);
  vtm->data[tm_year] = makeint(tm->tm_year);
  vtm->data[tm_wday] = makeint(tm->tm_wday);
  vtm->data[tm_yday] = makeint(tm->tm_yday);
  return vtm;

 got_error:
  primitive_runtime_error(error, op, 1, t);
}

TYPEDOP(gmtime, 0,
	"`n -> `v. Converts time in seconds `n, as returned by `time(),"
        " to a vector of UTC time information, indexed by the `tm_xxx"
        " constants:\n"
        "  `tm_sec   \tseconds (0-59; 60 for leap seconds)\n"
        "  `tm_min   \tminutes (0-59)\n"
        "  `tm_hour  \thours (0-23)\n"
        "  `tm_mday  \tday of month (1-31)\n"
        "  `tm_mon   \tmonth (0-11 for Jan-Dec)\n"
        "  `tm_year  \tyear since 1900\n"
        "  `tm_wday  \tday of week (0-6 for Sun-Sat)\n"
        "  `tm_yday  \tday of year (0-365 where 0 is Jan 1)",
	1, (value t),
	OP_LEAF | OP_NOESCAPE, "n.v")
{

  return make_tm(t, gmtime, &op_gmtime);
}

TYPEDOP(localtime, 0,
	"`n -> `v. Converts time in seconds to a vector of local time"
	" information. See `gmtime() for the format of `v.",
	1, (value t),
	OP_LEAF | OP_NOESCAPE, "n.v")
{

  return make_tm(t, localtime, &op_localtime);
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

  tm->tm_isdst = false;
#ifdef HAVE_STRUCT_TM_TM_ZONE
  tm->tm_zone = (char *)"UTC";
  tm->tm_gmtoff = 0;
#endif
}

TYPEDOP(asctime, 0,
       "`v -> `s. Returns a string of format \"Wed Jun 30 21:49:08 1993\""
        " representing the time in `v as returned by `gmtime().\n"
        "Cf. the `tm_xxx constants.",
	1, (struct vector *vgmt),
	OP_LEAF | OP_NOESCAPE | OP_CONST, "v.s")
{
  struct tm gmt;
  get_tm_struct(&gmt, vgmt);
  char *s = asctime(&gmt);      /* can return NULL */
  size_t l = s ? strlen(s) : 0;
  if (l > 0 && s[l - 1] == '\n')
    --l;
  return make_readonly(alloc_string_length(s, l));
}

TYPEDOP(strftime, 0,
	"`s0 `v -> `s1. Convert the `gmtime() vector `v into a string, as"
        " specified by the Unix `strftime(3) format string `s0."
        " Returns false on error."
        " See `/mhelp `strftime for help on the format string.",
	2, (struct string *fmt, struct vector *vgmt),
	OP_LEAF | OP_NOESCAPE | OP_STR_READONLY | OP_CONST | OP_TRACE, "sv.S")
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
    return make_readonly(alloc_string(buffer));

  return makebool(false);
}

TYPEDOP(mktime, 0,
        "`v -> `n. Does the inverse of `gmtime(). Returns -1 on error.",
        1, (struct vector *vgmt),
        OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST, "v.n")
{
  struct tm gmt;
  get_tm_struct(&gmt, vgmt);
  time_t t = mktime(&gmt);
  if (t != -1)
    t -= timezone;
  return makeint(t);
}

TYPEDOP(with_output, 0, "`oport `f -> `x. Evaluates `f() with output"
        " sent to `oport.\n"
        "If `oport is not a port, just evaluates `f().\n"
        "Output is restored when done. Returns the result of `f().",
        2, (value out, value code),
        OP_APPLY, "xf.x")
{
  struct oport *newout = mudout, *newerr = muderr;

  callable(code, 0);
  if (TYPE(out, type_oport)) newout = newerr = out;

  if (newout == mudout && newerr == muderr)
    return call0(code);

  struct oport *oout = mudout, *oerr = muderr;
  GCPRO2(oout, oerr);
  mudout = newout;
  muderr = newerr;
  value result = mcatch_call0(NULL, code);
  UNGCPRO();
  mudout = oout;
  muderr = oerr;

  maybe_mrethrow();

  return result;
}

UNSAFETOP(with_output_file, 0,
          "`s `n `f -> `x. Evaluates `f() with output appended to file `s."
          " `n decides how call traces are handled:\n"
          "  0  \tall call traces are sent to the file\n"
          "  1  \tonly unhandled call traces are sent to the file\n"
          "  2  \tno call traces are sent to the file\n"
          "Returns the result of `f().",
          3, (struct string *file, value mode, value code),
          0, "sxf.x")
{
  callable(code, 0);
  TYPEIS(file, type_string);
  int ctmode = GETRANGE(mode, 0, 2);

  FILE *f = fopen(file->str, "a+");
  if (f == NULL)
    runtime_error(error_bad_value);

  struct oport *oport = NULL;
  {
    GCPRO2(code, oport);
    oport = make_file_oport(f);

    if (ctmode == 0)
      add_call_trace(oport, 0);
    UNGCPRO();
  }

  struct oport *oout = mudout, *oerr = muderr;
  GCPRO3(oout, oerr, oport);
  mudout = oport;
  if (ctmode != 2)
    muderr = oport;
  value result = mcatch_call0(NULL, code);
  UNGCPRO();
  mudout = oout;
  muderr = oerr;

  fclose(f);
  oport->methods = NULL;

  if (ctmode == 0)
    remove_call_trace(oport);

  maybe_mrethrow();

  return result;
}

static struct {
  struct strbuf sb;
  struct oport *oport;
} format_data = { .sb = SBNULL };

static bool is_single(value v)
{
  if (integerp(v))
    return intval(v) == 1;
#ifdef USE_GMP
  if (TYPE(v, type_bigint))
    {
      struct bigint *bi = v;
      check_bigint(bi);
      return mpz_cmp_si(bi->mpz, 1) == 0;
    }
#endif
  runtime_error(error_bad_type);
}

static void pformat(struct oport *p, struct string *str,
		    struct vector *args, int i, int nargs)
{
  ulong slen, spos;

  GCPRO3(args, str, p);

  slen = string_len(str);
  spos = 0;
  while (spos < slen)
    {
      size_t ppos;
      {
        char *percent = memchr(str->str + spos, '%', slen - spos);
        if (percent == NULL)
          {
            pswrite_substring(p, str, spos, slen - spos);
            break;
          }
        ppos = percent - str->str;
      }

      pswrite_substring(p, str, spos, ppos - spos);

      int conv;

      bool zero = false, minus = false, plus = false;
      bool hash = false, space = false;

      long prec = -1;
      ulong width = 0;

      {
        const char *s = str->str + ppos + 1, *strend = str->str + slen;

        /* look for flags */
        for (;; ++s)
          {
            if (s >= strend) goto bad_value;
            switch (*s)
              {
              case '0': zero  = true; continue;
              case '+': plus  = true; continue;
              case '-': minus = true; continue;
              case '#': hash  = true; continue;
              case ' ': space = true; continue;
              }
            break;
          }

        if (s >= strend) goto bad_value;

        if (*s == '*')
          {
            if (i >= nargs) runtime_error(error_wrong_parameters);
            long w = GETINT(args->data[i]); i++;
            if (w < 0)
              {
                minus = true;
                width = -w;
              }
            else
              width = w;
            if (++s >= strend) goto bad_value;
          }
        else
          while (isdigit(*s))
            {
              if (width >= (MAX_TAGGED_INT - 9) / 10)
                goto bad_value;
              width = width * 10 + *s - '0';
              if (++s >= strend) goto bad_value;
            }

        if (*s == '.')
          {
            if (++s >= strend) goto bad_value;
            if (*s == '*')
              {
                if (i >= nargs) runtime_error(error_wrong_parameters);
                long w = GETINT(args->data[i]); i++;
                if (w < 0)
                  prec = 0;
                else
                  prec = w;
                if (++s >= strend) goto bad_value;
              }
            else
              {
                prec = 0;
                while (isdigit(*s))
                  {
                    if (prec >= (MAX_TAGGED_INT - 9) / 10)
                      goto bad_value;
                    prec = prec * 10 + *s - '0';
                    if (++s >= strend) goto bad_value;
                  }
              }
          }
        spos = s - str->str + 1;
        conv = *s;
      }

      unsigned base, predigits;

      enum prt_level print_level;

      const char *prefix;
      struct strbuf *const sb = &format_data.sb; /* save some typing below */

      switch (conv)
        {
        default: goto bad_value;
        case '%': pputc('%', p); break;
        case 'n': pputc('\n', p); break;
        case 'p':
          if (i >= nargs) runtime_error(error_wrong_parameters);
          if (!is_single(args->data[i++]))
            pputc('s', p);
          break;
        case 'P':
          if (i >= nargs) runtime_error(error_wrong_parameters);
          if (is_single(args->data[i++]))
            pputc('y', p);
          else
            pputs("ies", p);
          break;
        case 'C':
        case 'c':
          if (i >= nargs) runtime_error(error_wrong_parameters);
          ISINT(args->data[i]);
          sb_empty(sb);
          {
            int c = intval(args->data[i++]);
            if (isupper(conv))
              c = TO_8UPPER(c);
            sb_addc(sb, c);
          }
          goto do_output;
        case 'b':
          base = 2;
          prefix = "0b";
          predigits = 0;
          goto do_int;
        case 'd':
          base = 10;
          prefix = "";
          predigits = 0;
          goto do_int;
        case 'o':
          base = 8;
          prefix = "0";
          predigits = 1;
          goto do_int;
        case 'x':
          base = 16;
          prefix = "0x";
          predigits = 0;
          goto do_int;

          {
          do_int:
            if (i >= nargs) runtime_error(error_wrong_parameters);
            value v = args->data[i++];

            if (minus || prec >= 0)
              zero = false;

            /* 'istr' is the absolute integer */
            const char *istr;

            sb_empty(sb);
            if (integerp(v))
              {
                long l = intval(v);
                if (base != 10)
                  {
                    space = plus = false;
                    l &= MAX_TAGGED_INT * 2 + 1;
                  }

                sb_setminsize(sb, INTSTRSIZE);
                sb_setlen(sb, INTSTRSIZE);
                istr = inttostr(sb_mutable_str(sb), base, l, true);
              }
#ifdef USE_GMP
            else if (TYPE(v, type_bigint))
              {
                struct bigint *bi = v;
                check_bigint(bi);
                size_t size = mpz_sizeinbase(bi->mpz, base) + 2;
                sb_setminsize(sb, size);
                sb_setlen(sb, size - 1);
                mpz_get_str(sb_mutable_str(sb), base, bi->mpz);
                istr = sb_str(sb);
              }
#endif
            else
              goto bad_value;

            bool neg = (*istr == '-');
            if (neg)
              ++istr;

            if (!hash || strcmp(istr, "0") == 0)
              {
                prefix = "";
                predigits = 0;
              }

            /* 'ilen' is the number of digits */
            size_t ilen = strlen(istr) + predigits;

            /* 'tlen' is the total number of characters to print
               before adjusting for 'width' */
            size_t tlen = ilen;
            if (neg || plus || space)
              ++tlen;
            tlen += strlen(prefix) - predigits;

            size_t zeros;
            if (zero)
              zeros = tlen < width ? width - tlen : 0;
            else if (prec >= 0)
              zeros = ilen < prec ? prec - ilen : 0;
            else
              zeros = 0;

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
            pputs(istr, p);
            if (spaces && minus)
              pputnc(' ', spaces, p);
            break;
          }
        case 's':
        case 'S':
          print_level = prt_display;
          goto do_string;
        case 'w':
        case 'W':
          {
            print_level = hash ? prt_examine : prt_write;
          do_string:
            if (i >= nargs) runtime_error(error_wrong_parameters);
            sb_empty(sb);
            if (prec >= 0)
              output_value_cut(format_data.oport,
                               print_level,
                               zero,
                               args->data[i++],
                               prec);
            else
              output_value(format_data.oport,
                           print_level,
                           zero,
                           args->data[i++]);
          do_output:
            if (prec >= 0 && sb_len(sb) > prec)
              sb_setlen(sb, prec);
            size_t spaces = sb_len(sb) < width ? width - sb_len(sb) : 0;
            if (spaces && !minus)
              pputnc(' ', spaces, p);
            if (sb_len(sb) == 0)
              ;
            else if (isupper(conv))
              {
                pputc(TO_8UPPER(sb_str(sb)[0]), p);
                opwrite(p, sb_str(sb) + 1, sb_len(sb) - 1);
              }
            else
              opwrite(p, sb_str(sb), sb_len(sb));
            sb_free(sb);
            if (spaces && minus)
              pputnc(' ', spaces, p);
            break;
          }
        case 'a':
        case 'e':
        case 'f':
        case 'g':
          if (i >= nargs) runtime_error(error_wrong_parameters);
          double d = floatval(args->data[i++]);
          sb_empty(sb);
          sb_addc(sb, '%');
          if (hash)
            sb_addc(sb, '#');
          if (zero)
            sb_addc(sb, '0');
          if (minus)
            sb_addc(sb, '-');
          if (space)
            sb_addc(sb, ' ');
          if (plus)
            sb_addc(sb, '+');
          if (width > 0)
            sb_printf(sb, "%lu", width);
          if (prec >= 0)
            {
              sb_addc(sb, '.');
              if (prec > 0)
                sb_printf(sb, "%ld", prec);
            }
          sb_addc(sb, conv);
          char *fmt = sb_detach(sb);
          sb_printf(sb, fmt, d);
          free(fmt);
          opwrite(p, sb_str(sb), sb_len(sb));
          break;
        }
    }

  if (i != nargs) runtime_error(error_wrong_parameters);

  UNGCPRO();
  sb_free(&format_data.sb);
  return;

 bad_value:
  sb_free(&format_data.sb);
  runtime_error(error_bad_value);
}


static struct string *sformat(struct string *fmt, struct vector *argv, int idx)
{
  TYPEIS(fmt, type_string);
  TYPEIS(argv, type_vector);

  int nargs = vector_len(argv);
  if (nargs < idx)
    runtime_error(error_wrong_parameters);

  struct oport *p;
  {
    GCPRO2(fmt, argv);
    p = make_string_oport();
    UNGCPRO();
  }

  struct string *str;
  {
    GCPRO1(p);
    pformat(p, fmt, argv, idx, nargs);
    str = port_string(p, (size_t)-1);
    UNGCPRO();
  }
  opclose(p);
  return make_readonly(str);
}

VARTOP(dformat, 0,
       "`s `x1 `x2 ... -> . Displays formatted string `s to the user"
       " with parameters `x1, ... See `format() for syntax. Equivalent"
       " to `display(`format(`s, `x1, ...)).",
       OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "sx*.")
{
  if (nargs < 1) runtime_error(error_wrong_parameters);
  TYPEIS(args->data[0], type_string);
  pformat(mudout, args->data[0], args, 1, nargs);
  if (mudout) pflush(mudout);
  undefined();
}

TYPEDOP(dvformat, 0, "`s `v -> . Displays formatted string `s with"
        " parameters in `v. See `format() for syntax. Equivalent to"
        " `display(`vformat(`s, `v)).",
        2, (struct string *fmt, struct vector *argv),
        OP_LEAF | OP_NOESCAPE | OP_STR_READONLY | OP_TRACE, "sv.")
{
  TYPEIS(fmt, type_string);
  TYPEIS(argv, type_vector);
  pformat(mudout, fmt, argv, 0, vector_len(argv));
  if (mudout) pflush(mudout);
  undefined();
}

VARTOP(pformat, 0,
       "`oport `s `x1 `x2 ... -> . Outputs formatted string `s to `oport,"
       " with parameters `x1, ... See `format() for syntax. Does nothing if"
       " `oport is not an output port.",
       OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "xsx*.")
{
  if (nargs < 2) runtime_error(error_wrong_parameters);

  TYPEIS(args->data[1], type_string);

  GCPRO1(args);
  struct oport *p = get_oport(args->data[0]);
  UNGCPRO();
  if (p != NULL)
    pformat(p, args->data[1], args, 2, nargs);

  undefined();
}

TYPEDOP(pvformat, 0, "`oport `s `v -> . Output formatted string `s0 with"
        " parameters in `v to `oport. See `format() for syntax. Does nothing"
        " if `oport is not an output port.",
        3, (struct oport *p, struct string *fmt, struct vector *argv),
        OP_LEAF | OP_NOESCAPE | OP_STR_READONLY | OP_TRACE, "xsv.")
{
  TYPEIS(fmt, type_string);
  TYPEIS(argv, type_vector);

  GCPRO2(fmt, argv);
  p = get_oport(p);
  UNGCPRO();
  if (p != NULL)
    pformat(p, fmt, argv, 0, vector_len(argv));
  undefined();
}

TYPEDOP(vformat, 0, "`s0 `v -> `s1. Formats string `s0 with"
        " parameters in `v. See `format() for syntax.",
        2, (struct string *fmt, struct vector *argv),
        OP_LEAF | OP_NOESCAPE | OP_STR_READONLY | OP_TRACE, "sv.s")
{
  return sformat(fmt, argv, 0);
}

VARTOP(sformat, 0,
       "`s0|`null `s1 `x1 `x2 ... -> `n. Formats into writable string `s0"
       " using format `s1 and arguments `x1 `x2 ... ; throws an error if `s0"
       " and `s1 are the same string, if `s0 is not writable, or if `s0 is"
       " too short for the formatted result.\n"
       "Return value `n is the number of characters written into `s0, or the"
       " required string length if `s0 is null. The terminating null character"
       " is not counted.\n"
       "Note that `n might be greater than `MAX_STRING_SIZE.\n"
       "If `s0 is a string, `sformat() will place a null character at"
       " `s0[`n] if and only if `n is less than `slength(`s0).\n"
       "See `format() for syntax.",
       OP_LEAF | OP_NOESCAPE, "[su]sx*.n")
{
  if (nargs < 2)
    runtime_error(error_wrong_parameters);

  TYPEIS(args, type_vector);

  struct string *s = args->data[0];
  if (s)
    {
      TYPEIS(s, type_string);
      if (readonlyp(s))
	runtime_error(error_value_read_only);
    }

  struct string *fmt = args->data[1];
  TYPEIS(fmt, type_string);

  if (s == fmt)
    runtime_error(error_bad_value);

  // REVISIT: it would be convenient to have a "struct string" oport so
  // we could format directly into args->data[0] instead of having to
  // use a strbuf oport.

  struct strbuf sb = SBNULL;
  GCPRO2(args, s);
  struct oport *p = s == NULL ? make_sink_oport() : make_strbuf_oport(&sb);
  pformat(p, args->data[1], args, 2, nargs);
  UNGCPRO();

  if (s == NULL)
    {
      struct oport_stat stat;
      opstat(p, &stat);
      return makeint(stat.size);
    }

  size_t plen = sb_len(&sb);
  if (string_len(s) < plen)
    {
      sb_free(&sb);
      runtime_error(error_bad_value);
    }

  memcpy(s->str, sb_str(&sb), plen + 1);
  sb_free(&sb);
  return makeint(plen);
}

static const typing format_tset = { "sx*.s", NULL };

FULLOP(format, 0,
       "`s0 `x1 `x2 ... -> `s1. Formats string `s0 with parameters `x1, ...\n"
       "The string `s0 can contain formatting directives starting with '%'"
       " followed by zero or more flag characters, an optional field width,"
       " an optional precision, and the conversion specifier.\n"
       "\n"
       "Flag for for `b, `o, `x, `e, `f, `g, `a, `w, and `W conversions:\n"
       "  `#   \tPrefix non-zero numbers with \"0b\" (`b conversion),"
       " \"0\" (`o conversion), or \"0x\" (`x conversion).\n"
       "      \tFor `a, `e, `f, and `g conversions, the result will always"
       " contains a decimal point.\n"
       "      \tFor `g conversions, trailing zeros are not removed from"
       " the result.\n"
       "      \tFor `w and `W conversions, use `examine style output"
       " instead of `write.\n"
       "Flags for `b, `o, `d, `x, `a, `e, `f, and `g conversions:\n"
       "  `0   \tZero-pad to fill up the field width.\n"
       "  ' ' \t(space) Add a space before positive signed numbers.\n"
       "  `+   \tForce a sign ('+' or '-') before signed numbers.\n"
       "Flags for `c, `C, `s, `S, `w, `W, `b, `o, `d, `x, `a, `e, `f, and `g"
       " conversions:\n"
       "  `-   \tLeft justify the result (default is right justified).\n"
       "Flag for `w and `W conversions:\n"
       "  `0   \tInhibit any leading apostrophe from compound values and"
       " write null as \"()\".\n"
       "\n"
       "Field width and precision are only used for the `c, `C, `s, `S,"
       " `w, `W, `b, `o, `d, `x, `a, `e, `f, and `g conversions.\n"
       "\n"
       "The field width can be either a positive integer or an asterisk ('*'),"
       " meaning to use the next parameter (an integer) as field width. If"
       " the parameter is negative, this is taken as the `- flag and the"
       " absolute value is used for the field width.\n"
       "The output will be padded with spaces or zeros as necessary to fill"
       " up to the field width.\n"
       "\n"
       "The precision is a period ('.') followed by either an optional"
       " positive integer or an asterisk ('*'),"
       " meaning to use the next parameter (an integer) as precision. If"
       " the parameter is negative, this is taken as a precision of zero.\n"
       "For conversions `b, `o, `d, and `x, the precision is the minimum"
       " number of digits to print, zero-padding as necessary.\n"
       "For conversions `s, `S, `w, and `W, the precision is the maximum"
       " number of characters to include.\n"
       "For conversions `a, `e and `f, the precision is the number of digits"
       " to print after the decimal point.\n"
       "For the `g conversion, the precision is the maximum number of"
       " significant digits.\n"
       "\n"
       "Available conversions:\n"
       "  `%   \tA % sign.\n"
       "  `c   \tThe character in the next parameter (an integer).\n"
       "  `C   \tLike `c but capitalize the character.\n"
       "  `n   \tEnd of line (equivalent to \"\\n\").\n"
       "  `p   \tIf the next parameter (integer or bigint) is 1 \"\";"
       " otherwise \"s\".\n"
       "  `P   \tIf the next parameter (integer or bigint) is 1 \"y\";"
       " otherwise \"ies\".\n"
       "\n"
       "  `s   \tA string representation of the next parameter"
       " (like `display).\n"
       "  `S   \tLike `s but capitalize the first character.\n"
       "  `w   \tA string representation of the next parameter"
       " (like `write or, with the `# flag, `examine).\n"
       "  `W   \tLike `w but capitalize the first character.\n"
       "\n"
       "  `b   \tThe next parameter (integer or bigint) converted to binary;"
       " an integer will be considered unsigned.\n"
       "  `o   \tThe next parameter (integer or bigint) converted to octal;"
       " an integer will be considered unsigned.\n"
       "  `d   \tThe next parameter (integer or bigint) converted to"
       " decimal.\n"
       "  `x   \tThe next parameter (integer or bigint) converted to"
       " hexadecimal; an integer will be considered unsigned.\n"
       "\n"
       "  `e   \tThe next parameter (a float, integer, or bigint) in"
       " \"[-]d.ddde<+->dd\" style.\n"
       "  `f   \tThe next parameter (a float, integer, or bigint) in"
       " \"[-]ddd.ddd\" style.\n"
       "  `g   \tThe next parameter as `e conversion if the exponent is"
       " less than -4 or greater than or equal to the precision; otherwise"
       " as `f conversion.\n"
       "  `a   \tThe next parameter (a float, integer, or bigint) in"
       " \"[-]0xh.hhhhp<+->d\" style.\n"
       , NVARARGS, (struct vector *args, ulong nargs), 0,
       OP_LEAF | OP_NOESCAPE | OP_STR_READONLY | OP_CONST,
       format_tset, static)
{
  if (nargs < 1) runtime_error(error_wrong_parameters);
  return sformat(args->data[0], args, 1);
}

TYPEDOP(pputnc, 0, "`oport `n0 `n1 -> . Print `n1 characters `n0 to"
        " output port `oport."
        " Does nothing if `oport is not an output port.",
        3, (struct oport *p, value mchar, value mcount),
        OP_LEAF | OP_NOESCAPE, "xnn.")
{
  long count = GETRANGE(mcount, 0, MAX_STRING_SIZE);
  long ch = GETINT(mchar);

  p = get_oport(p);
  if (p == NULL)
    undefined();

  pputnc(ch, count, p);
  undefined();
}

TYPEDOP(pputc, 0, "`oport `n -> . Print character `n to output port `oport."
        " Does nothing if `oport is not an output port.",
        2, (struct oport *p, value mchar), OP_LEAF | OP_NOESCAPE, "xn.")
{
  return code_pputnc(p, mchar, makeint(1));
}

TYPEDOP(pprint, 0, "`oport `s -> . Print `s to output port `oport."
        " Does nothing if `oport is not an output port.",
        2, (struct oport *p, struct string *s),
        OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "xs.")
{
  TYPEIS(s, type_string);
  GCPRO1(s);
  p = get_oport(p);
  UNGCPRO();

  if (p == NULL)
    undefined();
  pswrite(p, s);
  undefined();
}

TYPEDOP(pprint_substring, 0,
        "`oport `s `n0 `n1 -> . Print `n1 characters of `s to `oport,"
        " starting with character `n0."
        " Does nothing if `oport is not an output port.",
        4, (struct oport *p, struct string *s, value mstart, value mlength),
        OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "xsnn.")
{
  long start = GETINT(mstart), length = GETINT(mlength);
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
  pswrite_substring(p, s, start, length);
  undefined();
}

TYPEDOP(make_string_oport, 0,
       "-> `oport. Returns a new string output port.",
	0, (void),
	OP_LEAF | OP_NOESCAPE, ".o")
{
  return make_string_oport();
}

void check_string_port(struct oport *oport)
{
  TYPEIS(oport, type_oport);
  if (!is_string_port(oport))
    runtime_error(error_bad_type);
}

TYPEDOP(port_string, 0,
       "`oport -> `s. Returns the contents of string port `oport.\n"
        "Throws an error if `oport contains more than `MAX_STRING_LENGTH"
        " characters.",
	1, (struct oport *p),
	OP_LEAF | OP_NOESCAPE, "o.s")
{
  check_string_port(p);
  return port_string(p, (size_t)-1);
}

TYPEDOP(port_string_head, 0,
       "`oport `n -> `s. Returns at most `n leading characters from the"
        " contents of string port `oport.\n"
        "Throws an error if attempting to extract more than"
        " `MAX_STRING_LENGTH characters",
        2, (struct oport *p, value n),
	OP_LEAF | OP_NOESCAPE, "on.s")
{
  long maxlen = GETRANGE(n, 0, LONG_MAX);
  check_string_port(p);
  return port_string(p, maxlen);
}

TYPEDOP(port_copy, 0,
       "`oport `s `n0 `n1 -> `n2. Copies the first `n1 characters of string"
        " port `oport into the supplied string `s starting at position `n0.\n"
	"String `s must be writable and at least `n0 + `n1 bytes long;"
	" neither `n0 nor `n1 can be negative.\n"
        "Returns `n2 in range [0, `n1] as the number of bytes written.\n"
        "Note: `port_copy() will place a null character at `s[`n0 + `n2]"
	" if and only if `n0 + `n2 is less than slength(`s).\n",
	4, (struct oport *p, struct string *s, value mstart, value mlength),
	OP_LEAF | OP_NOESCAPE, "osnn.n")
{
  long start = GETRANGE(mstart, 0, LONG_MAX);
  long length = GETRANGE(mlength, 0, LONG_MAX);

  check_string_port(p);
  TYPEIS(s, type_string);

  if (readonlyp(s))
    runtime_error(error_value_read_only);

  size_t plen = string_port_length(p);
  size_t slen = string_len(s);

  if (slen < start + length)
    runtime_error(error_bad_value);

  size_t copied = (plen < length) ? plen : length;
  GCPRO1(s);
  string_port_copy(s->str + start, p, copied);
  UNGCPRO();

  assert(s->str[start + copied] == '\0');
  return makeint(copied);
}

TYPEDOP(string_oport_length, 0,
       "`oport -> `n. Returns the number of characters in the string"
        " port `oport.",
	1, (struct oport *p),
	OP_LEAF | OP_NOESCAPE, "o.n")
{
  check_string_port(p);
  return makeint(string_port_length(p));
}

TYPEDOP(port_empty, "port_empty!",
        "`oport -> . Empties the contents of string port `oport.",
        1, (struct oport *p),
        OP_LEAF | OP_NOESCAPE, "o.")
{
  check_string_port(p);
  empty_string_oport(p);
  undefined();
}

TYPEDOP(add_call_trace_oport, "add_call_trace_oport!",
        "`x `b -> . Also send call traces to `x (an oport"
        " or a character). If `b is TRUE, only send those not handled"
        " otherwise.",
        2, (value oport, value only_unhandled), OP_LEAF | OP_NOESCAPE, "ox.")
{
  if (!TYPE(oport, type_oport) && !TYPE(oport, type_character))
    runtime_error(error_bad_type);

  GCPRO1(oport);
  remove_call_trace(oport);
  UNGCPRO();

  add_call_trace(oport, istrue(only_unhandled));
  undefined();
}

TYPEDOP(remove_call_trace_oport, "remove_call_trace_oport!",
        "`x -> . Stop sending call traces to `x (an oport or a character)."
        " Cf. `add_call_trace_oport!().",
        1, (value oport), OP_LEAF | OP_NOESCAPE, "o.")
{
  if (!TYPE(oport, type_oport) && !TYPE(oport, type_character))
    runtime_error(error_bad_type);

  remove_call_trace(oport);

  undefined();
}

#undef stdout                   /* needed on OS X */
TYPEDOP(stdout, 0,
        "-> `port. Returns the standard output port.",
        0, (void), OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".o")
{
  return mudout;
}

void io_init(void)
{
  DEFINE(stdout);
  DEFINE(pdisplay);
  DEFINE(pwrite);
  DEFINE(pexamine);
  DEFINE(display);
  DEFINE(write);
  DEFINE(examine);
  DEFINE(newline);
#ifndef WIN32
  clk_per_sec = sysconf(_SC_CLK_TCK);
  if (clk_per_sec < 0)
    {
      perror("sysconf(_SC_CLK_TCK)");
      exit(1);
    }
  assert(clk_per_sec > 0);
  DEFINE(ctime);
#endif
  DEFINE(time);
  DEFINE(time_afterp);
  DEFINE(asctime);
  DEFINE(strftime);
  DEFINE(gmtime);
  DEFINE(localtime);
  DEFINE(mktime);
  DEFINE(with_output);
  DEFINE(with_output_file);
  DEFINE(pputc);
  DEFINE(pputnc);
  DEFINE(pprint);
  DEFINE(pprint_substring);
  DEFINE(make_string_oport);
  DEFINE(port_empty);
  DEFINE(port_string);
  DEFINE(port_string_head);
  DEFINE(port_copy);
  DEFINE(string_oport_length);
  DEFINE(pformat);
  DEFINE(pvformat);
  DEFINE(dformat);
  DEFINE(dvformat);
  DEFINE(format);
  DEFINE(sformat);
  DEFINE(vformat);

  DEFINE(add_call_trace_oport);
  DEFINE(remove_call_trace_oport);


  format_data.oport = make_strbuf_oport(&format_data.sb);
  staticpro(&format_data.oport);
}
