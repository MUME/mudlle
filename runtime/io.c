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

#include "../mudlle-config.h"

#include <ctype.h>
#include <time.h>
#include <unistd.h>

#include <sys/resource.h>

#include "io.h"
#include "mudlle-float.h"
#include "mudlle-string.h"
#include "prims.h"

#include "../call.h"
#include "../charset.h"
#include "../context.h"
#include "../interpret.h"
#include "../mparser.h"
#include "../ports.h"
#include "../print.h"
#include "../strbuf.h"
#include "../utils.h"


struct oport *get_oport(struct oport *oport)
{
  if (TYPE(oport, oport))
    return oport;
  return NULL;
}

TYPEDOP(newline, , "-> . Print a newline", (void),
        OP_LEAF | OP_NOESCAPE, ".")
{
  pputc('\n', mudout);
  pflush(mudout);
  undefined();
}

static value pprint(struct oport *p, enum prt_level level, value v)
{
  GCPRO(v);
  p = get_oport(p);
  UNGCPRO();
  if (p)
    output_value(p, level, v);
  undefined();
}

TYPEDOP(pdisplay, , "`oport `x -> . Print a representation of `x to"
        " `oport as `display() does."
        " Does nothing if `oport is not an output port.",
        (struct oport *p, value x), OP_LEAF | OP_NOESCAPE | OP_STR_READONLY,
        "xx.")
{
  return pprint(p, prt_display, x);
}

TYPEDOP(pwrite, , "`oport `x -> . Print a representation of `x to"
        " `oport as `write() does."
        " Does nothing if `oport is not an output port.",
        (struct oport *p, value x), OP_LEAF | OP_NOESCAPE | OP_STR_READONLY,
        "xx.")
{
  return pprint(p, prt_write, x);
}

TYPEDOP(pexamine, , "`oport `x -> . Print a representation of `x to"
        " `oport as `examine() does."
        " Does nothing if `oport is not an output port.",
        (struct oport *p, value x), OP_LEAF | OP_NOESCAPE | OP_STR_READONLY,
        "xx.")
{
  return pprint(p, prt_examine, x);
}

TYPEDOP(write, , "`x -> . Print a representation of `x to the standard"
        " oport. Cf. `read_constant() which almost does the reverse.",
        (value v),
        OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "x.")
{
  output_value(mudout, prt_write, v);
  pflush(mudout);
  undefined();
}

TYPEDOP(pwrite_constant, , "`oport `x `b -> . Print a representation of `x to"
        " `oport that can be read using `read_constant().\n"
        "If `b is true, replaces any non-serializable value with null.\n"
        "Throws an error if `x cannot be represented as a constant.\n"
        "Does nothing if `oport is not an output port.",
        (struct oport *p, value v, value replace_gone),
        OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "xxx.")
{
  GCPRO(v);
  p = get_oport(p);
  UNGCPRO();
  if (p != NULL && !print_constant(p, v, MAX_STRING_SIZE,
                                   istrue(replace_gone)))
    runtime_error(error_bad_value);
  undefined();
}

TYPEDOP(display, , "`x -> . Display a representation of `x to the standard"
        " oport.", (value v),
        OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "x.")
{
  output_value(mudout, prt_display, v);
  pflush(mudout);
  undefined();
}

TYPEDOP(examine, , "`x -> . Examine a representation of `x to the standard"
        " oport.", (value v),
        OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "x.")
{
  output_value(mudout, prt_examine, v);
  pflush(mudout);
  undefined();
}

TYPEDOP(ctime, , "-> `n. Returns the number of milliseconds of CPU"
        " time. Only use the difference between two calls.",
	(void),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".n")
{
  clock_t c = clock();
  if (c == (clock_t)-1)
    return makeint(-1);

  unsigned long long t = c;
  t *= 1000;
  t /= CLOCKS_PER_SEC;
  return makeint((long)t);
}


TYPEDOP(time, ,
	"-> `n. Returns the number of seconds since the 1st of January"
        " 1970" NBSP "UTC. On 32-bit systems, negative values are used for"
        " values greater than `MAXINT (following Jan" NBSP "10 13:37:03 2004"
        NBSP "UTC).\n"
        "Cf. `time_after?() and `time_diff().",
	(void),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".n")
{
  return makeint(time(NULL));
}

enum runtime_error ct_time_p(long l, const char **errmsg, time_t *dst)
{
  if (l < 0)
    {
      /* negative values are treated as 31-bit overflows */
      l += 1UL << 31;
      if (l < 0)
        {
          *errmsg = "negative time out of range";
          return error_bad_value;
        }
    }
  *dst = l;
  if (*dst != l)
    {
      *errmsg = "time out of range";
      return error_bad_value;
    }
  return error_none;
}

#define __CT_TIME_E(v, msg, dst) ct_time_p(v, msg, &dst)
#define CT_TIME(dst) CT_INT_P(dst, __CT_TIME_E)

TYPEDOP(time_diff, "time_diff",
	"`n0 `n1 -> `n2. Returns the number of seconds from time `n1 to"
        " time `n0 as returned by `time(), bounded to [`MININT..`MAXINT].\n"
        "Similar to `n0 - `n1, but handles negative values correctly on"
        " 64-bit systems.\n"
        "Cf. `time_after?().",
	(value mt0, value mt1),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST, "nn.n")
{
  time_t t0, t1;
  CHECK_TYPES(mt0, CT_TIME(t0),
              mt1, CT_TIME(t1));
  int64_t ll = (int64_t)t0 - (int64_t)t1;
  long l = (ll < MIN_TAGGED_INT ? MIN_TAGGED_INT
            : ll > MAX_TAGGED_INT ? MAX_TAGGED_INT
            : ll);
  return makeint(l);
}

TYPEDOP(time_afterp, "time_after?",
	"`n0 `n1 -> `b. Returns true if time `n0 is after time `n1, as"
        " returned from `time().\n"
        "Cf. `time_diff().",
	(value mt0, value mt1),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST, "nn.n")
{
  time_t t0, t1;
  CHECK_TYPES(mt0, CT_TIME(t0),
              mt1, CT_TIME(t1));
  return makebool(t0 > t1);
}

static value make_tm(time_t timeval, struct tm *(*convert)(const time_t *time),
                     const struct prim_op *op)
{
  enum runtime_error error;
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
  CASSERT(time_fields == 8);
  return vtm;

 got_error:
  primitive_runtime_error(error, op, 1, makeint(timeval));
}

TYPEDOP(gmtime, ,
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
	(value mt),
	OP_LEAF | OP_NOESCAPE, "n.v")
{
  time_t t;
  CHECK_TYPES(mt, CT_TIME(t));
  return make_tm(t, gmtime, THIS_OP);
}

TYPEDOP(localtime, ,
	"`n -> `v. Converts time in seconds to a vector of local time"
	" information. See `gmtime() for the format of `v.",
	(value mt),
	OP_LEAF | OP_NOESCAPE, "n.v")
{
  time_t t;
  CHECK_TYPES(mt, CT_TIME(t));
  return make_tm(t, localtime, THIS_OP);
}

static void get_tm_struct(struct tm *tm, struct vector *v)
{
  assert(TYPE(v, vector));

  if (vector_len(v) != time_fields)
    runtime_error(error_bad_value);

  *tm = (struct tm){
    .tm_sec    = GETRANGE(v->data[tm_sec], 0, 60),
    .tm_min    = GETRANGE(v->data[tm_min], 0, 59),
    .tm_hour   = GETRANGE(v->data[tm_hour], 0, 23),
    .tm_mday   = GETRANGE(v->data[tm_mday], 1, 31),
    .tm_mon    = GETRANGE(v->data[tm_mon], 0, 11),
    .tm_year   = GETINT(v->data[tm_year]),
    .tm_wday   = GETRANGE(v->data[tm_wday], 0, 6),
    .tm_yday   = GETRANGE(v->data[tm_yday], 0, 365),
    .tm_isdst  = false,
  };
}

TYPEDOP(asctime, ,
       "`v -> `s. Returns a string of format \"Wed Jun 30 21:49:08 1993\""
        " representing the time in `v as returned by `gmtime().\n"
        "Cf. the `tm_xxx constants.",
	(struct vector *vgmt),
	OP_LEAF | OP_NOESCAPE | OP_CONST, "v.s")
{
  CHECK_TYPES(vgmt, vector);

  struct tm gmt;
  get_tm_struct(&gmt, vgmt);
  const char *s = asctime(&gmt); /* can return NULL */
  size_t l = s ? strlen(s) : 0;
  if (l > 0 && s[l - 1] == '\n')
    --l;
  return make_readonly(alloc_string_length(s, l));
}

TYPEDOP(strftime, ,
	"`s0 `v -> `s1. Convert the `gmtime() vector `v into a string, as"
        " specified by the Unix `strftime(3) format string `s0."
        " Returns false on error."
        " See `/mhelp `strftime for help on the format string.",
	(struct string *fmt, struct vector *vgmt),
	OP_LEAF | OP_NOESCAPE | OP_STR_READONLY | OP_CONST | OP_TRACE,
        "sv.[sz]")
{
  CHECK_TYPES(fmt, string, vgmt, vector);

  struct tm gmt;
  get_tm_struct(&gmt, vgmt);

#ifdef MAX_STRING_LENGTH
  char buffer[MAX_STRING_LENGTH];
#else
  char buffer[4096];
#endif
  if (strftime(buffer, sizeof buffer, fmt->str, &gmt))
    return make_readonly(alloc_string(buffer));

  return makebool(false);
}

TYPEDOP(mktime, ,
        "`v -> `n. Does the inverse of `gmtime(). Returns -1 on error.",
        (struct vector *vgmt),
        OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_CONST, "v.n")
{
  CHECK_TYPES(vgmt, vector);

  struct tm gmt;
  get_tm_struct(&gmt, vgmt);

  time_t t = mktime(&gmt);
  if (t != -1)
    t -= timezone;
  return makeint(t);
}

TYPEDOP(with_output, , "`oport `f -> `x. Evaluates `f() with output"
        " sent to `oport.\n"
        "If `oport is not a port, just evaluates `f().\n"
        "Output is restored when done. Returns the result of `f().\n"
        "Cf. `stdout().",
        (value out, value code),
        OP_APPLY, "xf.x")
{
  CHECK_TYPES(out,  any,
              code, CT_CALLABLE(0));

  struct oport *newout = mudout, *newerr = muderr;

  if (TYPE(out, oport)) newout = newerr = out;

  if (newout == mudout && newerr == muderr)
    return call0(code);

  struct oport *oout = mudout, *oerr = muderr;
  GCPRO(oout, oerr);
  mudout = newout;
  muderr = newerr;
  value result = mcatch_call(NULL, code);
  UNGCPRO();
  mudout = oout;
  muderr = oerr;

  maybe_mrethrow();

  return result;
}

UNSAFEOP(with_output_file, ,
         "`s `n `f -> `x. Evaluates `f() with output appended to file `s."
         " `n decides how call traces are handled:\n"
         "  0  \tall call traces are sent to the file\n"
         "  1  \tonly unhandled call traces are sent to the file\n"
         "  2  \tno call traces are sent to the file\n"
         "Returns the result of `f().\n"
         "Cf. `stdout().",
         (struct string *file, value mode, value code),
         0, "sxf.x")
{
  int ctmode;
  CHECK_TYPES(file, string,
              mode, CT_RANGE(ctmode, 0, 2),
              code, CT_CALLABLE(0));

  FILE *f = fopen(file->str, "a+");
  if (f == NULL)
    RUNTIME_ERROR(error_bad_value, "failed to open file");

  struct oport *oport = NULL;
  {
    GCPRO(code, oport);
    oport = make_file_oport(f);

    if (ctmode == 0)
      add_call_trace(oport, 0);
    UNGCPRO();
  }

  struct oport *oout = mudout, *oerr = muderr;
  GCPRO(oout, oerr, oport);
  mudout = oport;
  if (ctmode != 2)
    muderr = oport;
  value result = mcatch_call(NULL, code);
  UNGCPRO();
  mudout = oout;
  muderr = oerr;

  fclose(f);
  set_oport_methods(oport, NULL);

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
  unsigned expected = TSET(integer);
  if (integerp(v))
    return intval(v) == 1;
#ifdef USE_GMP
  expected |= TSET(bigint);
  if (TYPE(v, bigint))
    {
      struct bigint *bi = v;
      check_bigint(bi);
      return mpz_cmp_si(bi->mpz, 1) == 0;
    }
#endif
  bad_typeset_error(v, expected);
}

#define MAX_FORMAT_LEN     65536

static noreturn void missing_format_param(void)
{
  runtime_error_message(error_wrong_parameters,
                        "not enough parameters for format string");
}

static noreturn void bad_format_value(const char *msg)
{
  sb_free(&format_data.sb);
  runtime_error_message(error_bad_value, msg);
}

static const char *get_int(const char *s, long *dst, const char *err)
{
  long r = 0;
  while (isdigit((unsigned char)*s))
    {
      if (r > MAX_TAGGED_INT / 10)
        bad_format_value(err);
      r *= 10;
      int n = *s++ - '0';
      if (r > MAX_TAGGED_INT - n)
        bad_format_value(err);
      r += n;
    }
  *dst = r;
  return s;
}

static void pformat(struct oport *p, struct string *str,
		    struct vector *args, int i, int nargs)
{
  ulong slen, spos;

  GCPRO(args, str, p);

  slen = string_len(str);
  spos = 0;
  while (spos < slen)
    {
      size_t ppos;
      {
        const char *percent = memchr(str->str + spos, '%', slen - spos);
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
            if (s >= strend)
              bad_format_value("invalid trailing format conversion");
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

        if (s >= strend)
          bad_format_value("invalid trailing format conversion");

        if (*s == '*')
          {
            if (i >= nargs)
              missing_format_param();
            long w = GETINT(args->data[i]); i++;
            if (w < 0)
              {
                minus = true;
                width = -w;
              }
            else
              width = w;
            ++s;
          }
        else
          {
            long l;
            s = get_int(s, &l, "field width out of range");
            width = l;
          }

        if (s >= strend)
          bad_format_value("invalid trailing format conversion");

        if (*s == '.')
          {
            if (++s >= strend)
              bad_format_value("invalid trailing format conversion");
            if (*s == '*')
              {
                if (i >= nargs)
                  missing_format_param();
                long w = GETINT(args->data[i]); i++;
                if (w < 0)
                  prec = -1;
                else
                  prec = w;
                ++s;
              }
            else
              s = get_int(s, &prec, "precision out of range");

            if (s >= strend)
              bad_format_value("invalid trailing format conversion");
          }
        spos = s - str->str + 1;
        conv = *s;
      }

      enum prt_level print_level;

      unsigned base, predigits = 0;
      const char *prefix = "";

      struct strbuf *const sb = &format_data.sb; /* save some typing below */
      struct intstr ibuf;

      switch (conv)
        {
        default:
          bad_format_value("unknown conversion");
        case '%': pputc('%', p); break;
        case 'n': pputc('\n', p); break;
        case 'p':
          if (i >= nargs)
            missing_format_param();
          if (!is_single(args->data[i++]))
            pputc('s', p);
          break;
        case 'P':
          if (i >= nargs)
            missing_format_param();
          if (is_single(args->data[i++]))
            pputc('y', p);
          else
            pputs("ies", p);
          break;
        case 'C':
        case 'c':
          {
            if (i >= nargs)
              missing_format_param();
            value mc = args->data[i++];
            char c = GETINT(mc);
            if (conv == 'C')
              {
                conv = 'c';
                c = TO_8UPPER(c);
              }

            if (width <= 1 && !hash)
              {
                /* common case */
                pputc(c, p);
                break;
              }

            sb_empty(sb);

            if (hash)
              {
                /* emit as character constant */
                sb_addc(sb, '?');
                if (!IS_8PRINT(c) || c == C_NBSP)
                  {
                    int esc = 0;
                    switch (c)
                      {
#define _E(escchr, chr) case escchr: esc = chr; break
                        FOR_CHAR_ESCAPES(_E, SEP_SEMI);
#undef _E
                      default:
                        sb_printf(sb, "\\%03o", (unsigned char)c);
                        goto do_output;
                      }
                    sb_printf(sb, "\\%c", esc);
                  }
                else
                  {
                    if (c == '\\' || IS_8SPACE(c) || strchr("(){}[]\"", c))
                      sb_addc(sb, '\\');
                    sb_addc(sb, c);
                  }
                goto do_output;
              }

            sb_addc(sb, c);
            prec = -1;          /* precision has no effect for %c */
            goto do_output;
          }
        case 'b':
          base = 2;
          if (hash)
            prefix = "0b";
          goto do_int;
        case 'd':
          base = 10;
          goto do_int;
        case 'o':
          base = 8;
          if (hash)
            {
              prefix = "0";
              predigits = 1;
            }
          goto do_int;
        case 'x':
          base = 16;
          if (hash)
            prefix = "0x";
          goto do_int;

          {
          do_int:
            if (i >= nargs)
              missing_format_param();
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
                    l &= MAX_TAGGED_UINT;
                  }

                istr = longtostr(&ibuf, base, l);
              }
#ifdef USE_GMP
            else if (TYPE(v, bigint))
              {
                struct bigint *bi = v;
                check_bigint(bi);
                size_t size = mpz_sizeinbase(bi->mpz, base) + 2;
                if (size > MAX_FORMAT_LEN)
                  bad_format_value("bigint out of range to format");
                sb_setminsize(sb, size);
                sb_setlen(sb, size - 1);
                mpz_get_str(sb_mutable_str(sb), base, bi->mpz);
                istr = sb_str(sb);
              }
#endif
            else
              bad_format_value("invalid parameter to integer conversion");

            bool neg = (*istr == '-');
            if (neg)
              ++istr;

            if (strcmp(istr, "0") == 0)
              {
                if (!(base == 8 && hash))
                  istr = "";
                prefix = "";
                predigits = 0;
              }

            if (prec < 0)
              prec = 1;

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
            else
              zeros = ilen < prec ? prec - ilen : 0;

            if (zeros > MAX_FORMAT_LEN || width > MAX_FORMAT_LEN)
              bad_format_value("integer too large to print");

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
            if (i >= nargs)
              missing_format_param();
            bool simple = width == 0 && !isupper(conv);
            struct oport *dest = p;
            if (!simple)
              {
                dest = format_data.oport;
                sb_empty(sb);
              }
            if (prec > MAX_FORMAT_LEN)
              bad_format_value("precision out of range");
            output_value_cut(dest,
                             print_level,
                             zero,
                             args->data[i++],
                             prec >= 0 ? prec : SIZE_MAX);
            if (simple)
              break;
          do_output:
            if (prec > MAX_FORMAT_LEN)
              bad_format_value("precision out of range");
            if (width > MAX_FORMAT_LEN)
              bad_format_value("field width out of range");
            if (prec >= 0)
              assert(sb_len(sb) <= prec);
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
          if (i >= nargs)
            missing_format_param();
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

          if (prec > MAX_FORMAT_LEN)
            bad_format_value("precision out of range");
          if (width > MAX_FORMAT_LEN)
            bad_format_value("field width out of range");

          char *fmt = sb_detach(sb);
          sb_printf(sb, fmt, d);
          free(fmt);
          opwrite(p, sb_str(sb), sb_len(sb));
          break;
        }
    }

  if (i != nargs)
    runtime_error_message(error_wrong_parameters,
                          "too many parameters for format string");

  UNGCPRO();
  sb_free(&format_data.sb);
  return;
}


static struct string *sformat(struct string *fmt, struct vector *argv, int idx)
{
  TYPEIS(fmt, string);
  TYPEIS(argv, vector);

  int nargs = vector_len(argv);
  if (nargs < idx)
    runtime_error(error_wrong_parameters);

  struct oport *p;
  {
    GCPRO(fmt, argv);
    p = make_string_oport();
    UNGCPRO();
  }

  struct string *str;
  {
    GCPRO(p);
    pformat(p, fmt, argv, idx, nargs);
    str = port_string(p, SIZE_MAX);
    UNGCPRO();
  }
  opclose(p);
  return make_readonly(str);
}

VAROP(dformat, ,
      "`s `x1 `x2 ... -> . Displays formatted string `s to the standard"
      " oport with parameters `x1, ... See `format() for syntax. Equivalent"
      " to `display(`format(`s, `x1, ...)).",
      OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "sx*.")
{
  if (nargs < 1) runtime_error(error_wrong_parameters);
  TYPEIS(args->data[0], string);
  pformat(mudout, args->data[0], args, 1, nargs);
  pflush(mudout);
  undefined();
}

TYPEDOP(dvformat, , "`s `v -> . Displays formatted string `s to the standard"
        " oport with parameters in `v."
        " See `format() for syntax. Equivalent to `display(`vformat(`s, `v)).",
        (struct string *fmt, struct vector *argv),
        OP_LEAF | OP_NOESCAPE | OP_STR_READONLY | OP_TRACE, "sv.")
{
  TYPEIS(fmt, string);
  TYPEIS(argv, vector);
  pformat(mudout, fmt, argv, 0, vector_len(argv));
  pflush(mudout);
  undefined();
}

VAROP(pformat, ,
      "`oport `s `x1 `x2 ... -> . Outputs formatted string `s to `oport,"
      " with parameters `x1, ... See `format() for syntax. Does nothing if"
      " `oport is not an output port.",
      OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "xsx*.")
{
  if (nargs < 2) runtime_error(error_wrong_parameters);

  TYPEIS(args->data[1], string);

  GCPRO(args);
  struct oport *p = get_oport(args->data[0]);
  UNGCPRO();
  if (p != NULL)
    pformat(p, args->data[1], args, 2, nargs);

  undefined();
}

TYPEDOP(pvformat, , "`oport `s `v -> . Output formatted string `s0 with"
        " parameters in `v to `oport. See `format() for syntax. Does nothing"
        " if `oport is not an output port.",
        (struct oport *p, struct string *fmt, struct vector *argv),
        OP_LEAF | OP_NOESCAPE | OP_STR_READONLY | OP_TRACE, "xsv.")
{
  GCPRO(fmt, argv);                     /* CT_OPT_OPORT may cause GC */
  CHECK_TYPES(p,    CT_OPT_OPORT,
              fmt,  string,
              argv, vector);
  UNGCPRO();
  if (p != NULL)
    pformat(p, fmt, argv, 0, vector_len(argv));
  undefined();
}

TYPEDOP(vformat, , "`s0 `v -> `s1. Formats string `s0 with"
        " parameters in `v. See `format() for syntax.",
        (struct string *fmt, struct vector *argv),
        OP_LEAF | OP_NOESCAPE | OP_STR_READONLY | OP_TRACE, "sv.s")
{
  return sformat(fmt, argv, 0);
}

VAROP(sformat, ,
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

  TYPEIS(args, vector);

  struct string *s = args->data[0];
  if (s)
    {
      TYPEIS(s, string);
      if (readonlyp(s))
	runtime_error(error_value_read_only);
    }

  struct string *fmt = args->data[1];
  TYPEIS(fmt, string);

  if (s == fmt)
    runtime_error(error_bad_value);

  // REVISIT: it would be convenient to have a "struct string" oport so
  // we could format directly into args->data[0] instead of having to
  // use a strbuf oport.

  struct strbuf sb = SBNULL;
  GCPRO(args, s);
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

#define EXTRA_CONVERSIONS ""

VAROP(format, ,
      "`s0 `x1 `x2 ... -> `s1. Formats string `s0 with parameters `x1, ...\n"
      "The string `s0 can contain formatting directives starting with '%'"
      " followed by zero or more flag characters, an optional field width,"
      " an optional precision, and the conversion specifier.\n"
      "\n"
      "Flag for for `b, `o, `x, `e, `f, `g, and `a conversions:\n"
      "  `#   \tPrefix non-zero numbers with \"0b\" (`b conversion),"
      " \"0\" (`o conversion), or \"0x\" (`x conversion).\n"
      "      \tFor `a, `e, `f, and `g conversions, the result will always"
      " contains a decimal point.\n"
      "      \tFor `g conversions, trailing zeros are not removed from"
      " the result.\n"
      "Flags for `b, `o, `d, `x, `a, `e, `f, and `g conversions:\n"
      "  `0   \tZero-pad to fill up the field width.\n"
      "  ' ' \t(space) Add a space before non-negative signed numbers.\n"
      "  `+   \tForce a sign ('+' or '-') before signed numbers.\n"
      "Flags for `c, `C, `s, `S, `w, `W, `b, `o, `d, `x, `a, `e, `f, and `g"
      " conversions:\n"
      "  `-   \tLeft justify the result (default is right justified).\n"
      "Flag for `c and `C conversions:\n"
      "  `#   \tWrite the character as a character constant (\"?x\").\n"
      "Flag for `s, `S, `w and `W conversions:\n"
      "  `0   \tInhibit any leading apostrophe from compound values and"
      " write null as \"()\".\n"
      "Flag for `w and `W conversions:\n"
      "  `#   \tUse `examine() style output instead of `write().\n"
      "\n"
      "Field width and precision are only used for the `c, `C, `s, `S,"
      " `w, `W, `b, `o, `d, `x, `a, `e, `f, and `g conversions.\n"
      "\n"
      "The field width can be either a non-negative integer or an asterisk"
      " ('*'), meaning to use the next parameter (an integer) as field"
      " width. If the parameter is negative, this is taken as the `- flag"
      " and the absolute value is used for the field width.\n"
      "The output will be padded with spaces or zeros as necessary to fill"
      " up to the field width.\n"
      "\n"
      "The precision is a period ('.') followed by either an optional"
      " non-negative integer or an asterisk ('*'),"
      " meaning to use the next parameter (an integer) as precision. If"
      " the parameter is negative, this is taken as a precision of zero.\n"
      "For conversions `b, `o, `d, and `x, the precision is the minimum"
      " number of digits to print, zero-padding as necessary. A precision"
      " of zero when printing zero means no output.\n"
      "For conversions `s, `S, `w, and `W, the precision is the maximum"
      " number of characters to include.\n"
      "For conversions `a, `e and `f, the precision is the number of digits"
      " to print after the decimal point.\n"
      "For the `g conversion, the precision is the maximum number of"
      " significant digits.\n"
      "\n"
      "Field width and precision can at most be"
      " " STRINGIFY(MAX_FORMAT_LEN) ".\n"
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
      " as output by `display().\n"
      "  `S   \tLike `s but capitalize the first character.\n"
      "  `w   \tA string representation of the next parameter"
      " as output by `write() or, with the `# flag, `examine(). Long strings"
      " will be truncated, unless a precision is specified.\n"
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
      " \"[-]0xh.hhhhp<+->d\" style." EXTRA_CONVERSIONS,
      OP_LEAF | OP_NOESCAPE | OP_STR_READONLY | OP_CONST,
      "sx*.s")
{
  if (nargs < 1) runtime_error(error_wrong_parameters);
  return sformat(args->data[0], args, 1);
}

TYPEDOP(pputnc, , "`oport `n0 `n1 -> . Print `n1 characters `n0 to"
        " output port `oport."
        " Does nothing if `oport is not an output port.",
        (struct oport *p, value mchar, value mcount),
        OP_LEAF | OP_NOESCAPE, "xnn.")
{
  long count;
  unsigned char c;
  CHECK_TYPES(p,      CT_OPT_OPORT,
              mchar,  CT_RANGE(c, SCHAR_MIN, UCHAR_MAX),
              mcount, CT_RANGE(count, 0, MAX_STRING_SIZE));
  pputnc(c, count, p);
  undefined();
}

TYPEDOP(pputc, , "`oport `n -> . Print character `n to output port `oport."
        " Does nothing if `oport is not an output port.",
        (struct oport *p, value mchar), OP_LEAF | OP_NOESCAPE, "xn.")
{
  unsigned char c;
  CHECK_TYPES(p,     CT_OPT_OPORT,
              mchar, CT_RANGE(c, SCHAR_MIN, UCHAR_MAX));
  pputc(c, p);
  undefined();
}

TYPEDOP(pprint, , "`oport `s -> . Print `s to output port `oport."
        " Does nothing if `oport is not an output port.",
        (struct oport *p, struct string *s),
        OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "xs.")
{
  GCPRO(s);                     /* CT_OPT_OPORT may cause GC */
  CHECK_TYPES(p, CT_OPT_OPORT,
              s, string);
  UNGCPRO();

  if (p == NULL)
    undefined();
  pswrite(p, s);
  undefined();
}

TYPEDOP(pprint_substring, ,
        "`oport `s `n0 `n1 -> . Print `n1 characters of `s to `oport,"
        " starting with character `n0."
        " Does nothing if `oport is not an output port.",
        (struct oport *p, struct string *s, value mstart, value mlength),
        OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "xsnn.")
{
  long start = GETINT(mstart), length = GETINT(mlength);
  GCPRO(s);                     /* CT_OPT_OPORT may cause GC */
  CHECK_TYPES(p,       CT_OPT_OPORT,
              s,       string,
              mstart,  CT_STR_IDX(start, s, true),
              mlength, CT_RANGE(length, 0, LONG_MAX));
  if (start + length > string_len(s))
    RUNTIME_ERROR(error_bad_index, NULL);
  UNGCPRO();
  pswrite_substring(p, s, start, length);
  undefined();
}

TYPEDOP(make_string_oport, ,
       "-> `oport. Returns a new string output port.",
	(void),
	OP_LEAF | OP_NOESCAPE, ".o")
{
  return make_string_oport();
}

TYPEDOP(port_string, ,
       "`oport -> `s. Returns the contents of string port `oport.\n"
        "Throws an error if `oport contains more than `MAX_STRING_SIZE"
        " characters.",
	(struct oport *p),
	OP_LEAF | OP_NOESCAPE, "o.s")
{
  CHECK_TYPES(p, CT_STR_OPORT);
  return port_string(p, SIZE_MAX);
}

TYPEDOP(port_string_head, ,
       "`oport `n -> `s. Returns at most `n leading characters from the"
        " contents of string port `oport.\n"
        "Throws an error if attempting to extract more than"
        " `MAX_STRING_SIZE characters",
        (struct oport *p, value n),
	OP_LEAF | OP_NOESCAPE, "on.s")
{
  long maxlen;
  CHECK_TYPES(p, CT_STR_OPORT,
              n, CT_RANGE(maxlen, 0, LONG_MAX));
  return port_string(p, maxlen);
}

TYPEDOP(port_copy, ,
       "`oport `s `n0 `n1 -> `n2. Copies the first `n1 characters of string"
        " port `oport into the supplied string `s starting at position `n0.\n"
	"String `s must be writable and at least `n0 + `n1 bytes long;"
	" neither `n0 nor `n1 can be negative.\n"
        "Returns `n2 in range [0, `n1] as the number of bytes written.\n"
        "Note: `port_copy() will place a null character at `s[`n0 + `n2]"
	" if and only if `n0 + `n2 is less than slength(`s).\n",
	(struct oport *p, struct string *s, value mstart, value mlength),
	OP_LEAF | OP_NOESCAPE, "osnn.n")
{
  long start, length;
  CHECK_TYPES(p,       CT_STR_OPORT,
              s,       string,
              mstart,  CT_RANGE(start, 0, LONG_MAX),
              mlength, CT_RANGE(length, 0, LONG_MAX));

  if (readonlyp(s))
    runtime_error(error_value_read_only);

  size_t plen = string_port_length(p);
  size_t slen = string_len(s);

  if (slen < start + length)
    runtime_error(error_bad_value);

  size_t copied = (plen < length) ? plen : length;
  GCPRO(s);
  string_port_copy(s->str + start, p, copied);
  UNGCPRO();

  assert(s->str[start + copied] == '\0');
  return makeint(copied);
}

TYPEDOP(port_append, ,
       "`oport0 `oport1 -> . Prints the contents of string port `oport1"
        " to port `oport0.",
	(struct oport *dst, struct oport *src),
	OP_LEAF | OP_NOESCAPE, "oo.")
{
  CHECK_TYPES(dst, oport,
              src, CT_STR_OPORT);
  port_append(dst, src);
  undefined();
}

TYPEDOP(string_oport_length, ,
       "`oport -> `n. Returns the number of characters in the string"
        " port `oport.",
	(struct oport *p),
	OP_LEAF | OP_NOESCAPE, "o.n")
{
  CHECK_TYPES(p, CT_STR_OPORT);
  return makeint(string_port_length(p));
}

TYPEDOP(port_empty, "port_empty!",
        "`oport -> . Empties the contents of string port `oport.",
        (struct oport *p),
        OP_LEAF | OP_NOESCAPE, "o.")
{
  CHECK_TYPES(p, CT_STR_OPORT);
  empty_string_oport(p);
  undefined();
}

TYPEDOP(add_call_trace_oport, "add_call_trace_oport!",
        "`x `b -> . Also send call traces to `x (an oport"
        " or a character). If `b is TRUE, only send those not handled"
        " otherwise.",
        (value oport, value only_unhandled), OP_LEAF | OP_NOESCAPE, "ox.")
{
  CHECK_TYPES(oport,          CT_TYPES(oport, character),
              only_unhandled, any);

  GCPRO(oport);
  remove_call_trace(oport);
  UNGCPRO();

  add_call_trace(oport, istrue(only_unhandled));
  undefined();
}

TYPEDOP(remove_call_trace_oport, "remove_call_trace_oport!",
        "`x -> . Stop sending call traces to `x (an oport or a character)."
        " Cf. `add_call_trace_oport!().",
        (value oport), OP_LEAF | OP_NOESCAPE, "o.")
{
  CHECK_TYPES(oport, CT_TYPES(oport, character));
  remove_call_trace(oport);
  undefined();
}

#undef stdout                   /* needed on OS X */
TYPEDOP(stdout, ,
        "-> `port. Returns the standard `display() output port.",
        (void), OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".o")
{
  return mudout_port;
}

void io_init(void)
{
  DEFINE(stdout);
  DEFINE(pdisplay);
  DEFINE(pwrite);
  DEFINE(pwrite_constant);
  DEFINE(pexamine);
  DEFINE(display);
  DEFINE(write);
  DEFINE(examine);
  DEFINE(newline);
  DEFINE(ctime);
  DEFINE(time);
  DEFINE(time_afterp);
  DEFINE(time_diff);
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
  DEFINE(port_append);
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
