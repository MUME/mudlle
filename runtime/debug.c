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
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sys/param.h>

#include "basic.h"
#include "debug.h"
#include "mudlle-string.h"
#include "runtime.h"

#include "../alloc.h"
#include "../context.h"
#include "../global.h"
#include "../interpret.h"
#include "../ports.h"
#include "../print.h"
#include "../strbuf.h"
#include "../table.h"
#include "../utils.h"


static void show_function(struct closure *c);

#  define HELP_PREFIX ""

SECTOP(help, HELP_PREFIX "help",
       "`f -> . Prints help on function `f", 1, (value v),
       1,
       OP_LEAF | OP_NOESCAPE, "x.")
{
  struct primitive *op = v; /* Only valid for type_{sec,prim,va} */
  if (TYPE(v, type_secure)
      || (TYPE(v, type_primitive) && op->op->flags & OP_FASTSEC))
    {
      pprintf(mudout, "Secure %d: %s", op->op->seclevel, op->op->help);
    }
  else if (TYPE(v, type_primitive) || TYPE(v, type_varargs))
    {
      pputs(op->op->help, mudout);
    }
  else if (TYPE(v, type_closure)) show_function(v);
  else pprintf(mudout, "This variable isn't executable\n");
  pputc('\n', mudout);
  undefined();
}

SECTOP(help_string, 0, "`f -> `s. Returns `f's help string, or null if none",
       1, (value v),
       1,
       OP_LEAF | OP_NOESCAPE, "f.[su]")
{
  if (is_any_primitive(v))
    {
      struct primitive *op = v;

      if (op->op->flags & OP_CONST)
        {
          size_t len = strlen(op->op->help);
          struct strbuf sb = sb_initf(
            "%s%s%s",
            op->op->help,
            (len == 0
             ? ""
             : op->op->help[len - 1] == '.' ? "\n" : ".\n"),
            "May be evaluated at compile-time.");
          struct string *s = alloc_string(sb_str(&sb));
          sb_free(&sb);
          return s;
        }
      return alloc_string(op->op->help);
    }

  TYPEIS(v, type_closure);
  struct closure *c = v;
  return c->code->help;
}

TYPEDOP(defined_in, 0, "`f -> `v. Returns information on where `f (any"
        " primitive, closure, code, or mcode) is defined as"
        " [`nicename, `lineno, `filename].",
        1, (value fn),
        OP_LEAF | OP_NOESCAPE, "[fo].v")
{
  struct vector *v;
  struct string *filename, *nicename;
  uword lineno;

  if (TYPE(fn, type_closure))
    fn = ((struct closure *)fn)->code;

  if (TYPE(fn, type_code) || TYPE(fn, type_mcode))
    {
      struct code *code = fn;
      nicename = code->nicename;
      filename = code->filename;
      lineno   = code->lineno;
    }
  else if (is_any_primitive(fn))
    {
      struct primitive *prim = fn;
      nicename = filename = alloc_string(prim->op->filename);
      lineno   = prim->op->lineno;
    }
  else
    runtime_error(error_bad_type);

  GCPRO1(filename);
  v = alloc_vector(3);
  UNGCPRO();
  v->data[0] = nicename;
  v->data[1] = makeint(lineno);
  v->data[2] = filename;

  return v;
}

UNSAFETOP(set_use_nicename, "set_use_nicename!",
          "`b -> . Set whether to (only) use nicenames for call traces and"
          " compiler messages. Cf. `use_nicename().",
          1, (value enable),
          OP_LEAF | OP_NOESCAPE | OP_NOALLOC, "x.")
{
  use_nicename = istrue(enable);
  undefined();
}

TYPEDOP(use_nicename, 0, "-> `b. True if (only) nicenames will be used"
        " in call traces and compiler messages. Cf. `set_use_nicename!().",
        0, (void),
        OP_LEAF | OP_NOESCAPE | OP_NOALLOC, ".n")
{
  return makebool(use_nicename);
}

UNSAFETOP(closure_variables, 0,
          "`f -> `v. Returns a vector of the closure variable values of"
          " function `f",
          1, (struct closure *fn),
          OP_LEAF | OP_NOESCAPE, "f.v")
{
  ulong nbvar, i;
  struct vector *res;

  TYPEIS(fn, type_closure);

  nbvar = (fn->o.size - offsetof(struct closure, variables)) / sizeof(value);

  GCPRO1(fn);
  res = alloc_vector(nbvar);
  for (i = 0; i < nbvar; ++i)
    res->data[i] = fn->variables[i];
  UNGCPRO();

  return res;
}

TYPEDOP(variable_value, 0, "`v -> `x. Returns the value in variable `v",
        1, (struct variable *v),
        OP_LEAF | OP_NOESCAPE, "o.x")
{
  TYPEIS(v, type_variable);
  return v->vvalue;
}

TYPEDOP(function_seclevel, 0, "`f -> `n. Returns the security level of the"
        " function `f",
	1, (value fn), OP_LEAF | OP_NOESCAPE, "f.n")
{
  if (is_any_primitive(fn))
    return makeint(((struct primitive *)fn)->op->seclevel);

  TYPEIS(fn, type_closure);
  struct closure *c = fn;
  return makeint(c->code->seclevel);
}

TYPEDOP(function_name, 0, "`f -> `s. Returns name of `f (any primitive,"
        " closure, code, or mcode) if available; false otherwise.",
	1, (value fn),
	OP_LEAF | OP_NOESCAPE, "[fo].[sz]")
{
  struct string *name = NULL;

  if (TYPE(fn, type_mcode) || TYPE(fn, type_code))
    {
      name = ((struct code *)fn)->varname;
      goto got_name;
    }

  if (is_any_primitive(fn))
    {
      struct primitive *op = fn;
      return alloc_string(op->op->name);
    }

  if (TYPE(fn, type_closure))
    {
      struct closure *c = fn;
      name = c->code->varname;
      goto got_name;
    }
  runtime_error(error_bad_type);

got_name:
  return name ? name : makebool(false);
}

static void show_function(struct closure *c)
{
  struct code *code = c->code;
  if (code->help) output_value(mudout, prt_display, false, code->help);
  else pputs("undocumented", mudout);
  pputs(" [", mudout);
  output_value(mudout, prt_display, false, code->nicename);
  pprintf(mudout, ":%d", code->lineno);
  pputc(']', mudout);
}

TYPEDOP(profile, 0, "`f -> `x. Returns profiling information for `f:"
        " cons(#`calls, #`instructions) for mudlle functions, #`calls for"
        " primitives.\n"
        "Profiling information is only updated by interpreted code.",
        1, (value fn),
        OP_LEAF | OP_NOESCAPE, "[fo].[kn]")
{
  if (TYPE(fn, type_closure)) fn = ((struct closure *)fn)->code;

  if (TYPE(fn, type_code))
    {
      struct icode *c = fn;
      return alloc_list(makeint(c->call_count),
                        makeint(c->instruction_count));
    }
  if (is_any_primitive(fn))
    return makeint(((struct primitive *)fn)->call_count);
  runtime_error(error_bad_type);
}

UNSAFETOP(dump_memory, 0, "-> . Dumps GC memory (for use by profiler)",
          0, (void), OP_LEAF | OP_NOESCAPE, ".")
{
  dump_memory();
  undefined();
}

SECTOP(apropos, HELP_PREFIX "apropos",
       "`s -> . Finds all global variables whose name contains"
       " the substring `s and prints them (with help)",
       1, (struct string *s),
       1,
       OP_LEAF | OP_NOESCAPE, "s.")
{
  TYPEIS(s, type_string);
  GCPRO1(s);
  for (int i = 0, envused = intval(environment->used); i < envused; ++i)
    {
      if (mudlle_string_isearch(GNAME(i), s) < 0)
        continue;

      output_value(mudout, prt_display, false, GNAME(i));
      pputs("\n  ", mudout);

      if (is_any_primitive(GVAR(i)))
        {
          struct primitive *op = GVAR(i);

          if (op->op->help)
            pprintf(mudout, "Primitive: %s\n", op->op->help);
          else
            pputs("Undocumented primitive\n", mudout);
        }
      else if (TYPE(GVAR(i), type_closure))
        {
          pputs("Function: ", mudout);
          show_function(GVAR(i));
          pputc('\n', mudout);
        }
      else
        pputs("Variable\n", mudout);
    }
  UNGCPRO();
  undefined();
}

static const typing quit_tset = { "n.", ".", NULL };
FULLOP(quit, 0, "[`n] -> . Exit mudlle, optionally with exit code `n.",
       NVARARGS, (struct vector *args, ulong nargs), 0, 0, quit_tset, static)
{
  int code;
  if (nargs == 0)
    code = EXIT_SUCCESS;
  else if (nargs == 1)
    code = GETINT(args->data[0]);
  else
    runtime_error(error_wrong_parameters);
  exit(code);
}

#ifdef GCSTATS
TYPEDOP(gcstats, 0, "-> `v. Returns GC statistics: vector(`minor_count,"
        " `major_count, `size, `usage_minor, `usage_major, last gc sizes,"
        " gen0 gc sizes, gen1 gc sizes). The sizes vectors are vectors"
        " of vector(`objects, `rosize, `rwsize).", 0, (void),
        OP_LEAF | OP_NOESCAPE, ".v")
{
  struct vector *last = NULL;
  struct vector *gen[2] = { 0 };
  GCPRO3(gen[0], gen[1], last);
  for (int g = 0; g < 2; ++g)
    gen[g] = alloc_vector(last_type);
  last = alloc_vector(last_type);
  for (int i = 0; i < last_type; i++)
    {
      struct vector *v = alloc_vector(2);
      v->data[0] = makeint(gcstats.l.types[i].nb);
      v->data[1] = makeint(gcstats.l.types[i].size);
      last->data[i] = v;
      for (int g = 0; g < 2; ++g)
        {
          v = alloc_vector(3);
          v->data[0] = makeint(gcstats.gen[g].types[i].nb);
          v->data[1] = makeint(gcstats.gen[g].types[i].rosize);
          v->data[2] = makeint(gcstats.gen[g].types[i].rwsize);
          gen[g]->data[i] = v;
        }
    }
  struct vector *v = alloc_vector(8);
  v->data[0] = makeint(gcstats.minor_count);
  v->data[1] = makeint(gcstats.major_count);
  v->data[2] = makeint(gcstats.size);
  v->data[3] = makeint(gcstats.usage_minor);
  v->data[4] = makeint(gcstats.usage_major);
  v->data[5] = last;
  v->data[6] = gen[0];
  v->data[7] = gen[1];

  UNGCPRO();

  return v;
}

  #define GCSTATS_LEVEL 1

SECTOP(reset_gcstats, "reset_gcstats!", "-> . Reset short GC statistics.",
        0, (void), GCSTATS_LEVEL,
        OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".")
{
  gcstats.a = GCSTATS_ALLOC_NULL;
  undefined();
}

TYPEDOP(short_gcstats, 0, "-> `v. Returns short GC statistics. `v[`n] is"
        " a vector(`objects, `bytes) allcated for type `n.", 0, (void),
        OP_LEAF | OP_NOESCAPE, ".v")
{
  struct gcstats stats = gcstats;
  struct vector *v = alloc_vector(last_type);
  GCPRO1(v);
  for (int i = 0; i < last_type; ++i)
    {
      struct vector *w = alloc_vector(2);
      w->data[0] = makeint(stats.a.types[i].nb);
      w->data[1] = makeint(stats.a.types[i].size);
      v->data[i] = w;
    }
  UNGCPRO();
  return v;
}

TYPEDOP(gc_generation, 0, "-> `n. Returns the number of garbage collections"
	" run so far (minor or major). Cf. `gc_cmp().",
	0, (void), OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".n")
{
  return makeint(gcstats.minor_count + gcstats.major_count);
}

#endif  /* GCSTATS */

TYPEDOP(gc_cmp, 0, "`x0 `x1 -> `n. Compares `x0 and `x1 as == does, and"
        " returns -1 if `x0 is less than `x1, 0 if they are equal, or 1 if `x0"
	" is greater than `x1. The results are only stable within"
	" one `gc_generation().",
	2, (value a, value b),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "xx.n")
{
  return makeint((long)a < (long)b ? -1 : (long)a != (long)b);
}

TYPEDOP(gc_hash, 0,
        "`x -> `n. Returns a non-negative hash number for object `x,"
        " which is valid while `gc_generation() is constant.",
	1, (value x),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makeint(case_symbol_hash_len((const char *)&x, sizeof x,
                                      MAX_TAGGED_INT + 1));
}

UNSAFETOP(garbage_collect, 0, "`n -> . Does a forced garbage collection,"
          " asserting room for `n bytes of allocations before another"
          " garbage collection has to be done.",
          1, (value n),
          OP_LEAF | OP_NOESCAPE, "n.")
{
  garbage_collect(GETINT(n));
  undefined();
}

void debug_init(void)
{
  DEFINE(garbage_collect);
  DEFINE(help);
  DEFINE(help_string);
  DEFINE(defined_in);
  DEFINE(set_use_nicename);
  DEFINE(use_nicename);
  DEFINE(closure_variables);
  DEFINE(variable_value);
  DEFINE(function_name);
  DEFINE(function_seclevel);
  DEFINE(profile);
  DEFINE(dump_memory);
  DEFINE(apropos);
  DEFINE(quit);
#ifdef GCSTATS
  DEFINE(gcstats);
  DEFINE(short_gcstats);
  DEFINE(reset_gcstats);
  DEFINE(gc_generation);
#endif  /* GCSTATS */
  DEFINE(gc_cmp);
  DEFINE(gc_hash);
}
