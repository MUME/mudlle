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
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sys/param.h>

#include "basic.h"
#include "check-types.h"
#include "debug.h"
#include "mudlle-string.h"
#include "prims.h"

#include "../alloc.h"
#include "../context.h"
#include "../global.h"
#include "../hash.h"
#include "../interpret.h"
#include "../ports.h"
#include "../print.h"
#include "../strbuf.h"
#include "../table.h"
#include "../utils.h"


static void show_function(struct closure *c);

#  define HELP_PREFIX ""

SECOP(help, HELP_PREFIX "help",
       "`f -> . Prints help on function `f", (value v),
       LVL_VALA, OP_LEAF | OP_NOESCAPE, "x.")
{
  struct primitive *op = v; /* Only valid for type_{sec,prim,va} */
  if (TYPE(v, secure)
      || (TYPE(v, primitive) && op->op->flags & OP_FASTSEC))
    {
      pprintf(mudout, "Secure %d: %s", op->op->seclevel, op->op->help);
    }
  else if (TYPE(v, primitive) || TYPE(v, varargs))
    {
      pputs(op->op->help, mudout);
    }
  else if (TYPE(v, closure)) show_function(v);
  else pprintf(mudout, "This variable isn't executable\n");
  pputc('\n', mudout);
  undefined();
}

SECOP(help_string, , "`f -> `s. Returns `f's help string, or null if none",
      (value v), LVL_VALA, OP_LEAF | OP_NOESCAPE, "f.[su]")
{
  CHECK_TYPES(v, CT_FUNCTION);

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

  assert(TYPE(v, closure));
  struct closure *c = v;
  return c->code->help;
}

TYPEDOP(defined_in, , "`f -> `v. Returns information on where `f (any"
        " primitive, closure, code, or mcode) is defined as"
        " [`nicename, `lineno, `filename, `column].",
        (value orig_fn),
        OP_LEAF | OP_NOESCAPE, "[fo].v")
{
  CHECK_TYPES(orig_fn, CT_TYPESET(TYPESET_FUNCTION
                                  | P(type_mcode) | P(type_code)));

  struct string *filename, *nicename;
  int column, lineno;

  value fn = orig_fn;
  if (TYPE(fn, closure))
    fn = ((struct closure *)fn)->code;

  if (TYPE(fn, code) || TYPE(fn, mcode))
    {
      struct code *code = fn;
      nicename = code->nicename;
      filename = code->filename;
      lineno   = code->lineno;
      column   = code->column;
    }
  else
    {
      assert(is_any_primitive(fn));
      struct primitive *prim = fn;
      lineno   = prim->op->lineno;
      nicename = filename = alloc_string(prim->op->filename);
      column   = 1;
    }

  GCPRO(filename, nicename);
  struct vector *v = alloc_vector(4);
  UNGCPRO();
  v->data[0] = nicename;
  v->data[1] = makeint(lineno);
  v->data[2] = filename;
  v->data[3] = makeint(column);

  return v;
}

UNSAFEOP(set_use_nicename, "set_use_nicename!",
          "`b -> . Set whether to (only) use nicenames for call traces and"
          " compiler messages. Cf. `use_nicename().",
          (value enable),
          OP_LEAF | OP_NOESCAPE | OP_NOALLOC, "x.")
{
  use_nicename = istrue(enable);
  undefined();
}

TYPEDOP(use_nicename, , "-> `b. True if (only) nicenames will be used"
        " in call traces and compiler messages. Cf. `set_use_nicename!().",
        (void),
        OP_LEAF | OP_NOESCAPE | OP_NOALLOC, ".n")
{
  return makebool(use_nicename);
}

UNSAFEOP(closure_variables, ,
         "`f -> `v. Returns a vector of the closure variable values of"
         " function `f",
         (struct closure *fn),
         OP_LEAF | OP_NOESCAPE, "f.v")
{
  CHECK_TYPES(fn, closure);
  ulong nbvar = ((fn->o.size - offsetof(struct closure, variables))
                 / sizeof (value));

  GCPRO(fn);
  struct vector *res = alloc_vector(nbvar);
  for (ulong i = 0; i < nbvar; ++i)
    res->data[i] = fn->variables[i];
  UNGCPRO();

  return res;
}

TYPEDOP(variable_value, , "`v -> `x. Returns the value in variable `v",
        (struct variable *v),
        OP_LEAF | OP_NOESCAPE, "o.x")
{
  CHECK_TYPES(v, variable);
  return v->vvalue;
}

TYPEDOP(function_seclevel, , "`f -> `n. Returns the security level of the"
        " function `f",
	(value fn), OP_LEAF | OP_NOESCAPE, "f.n")
{
  CHECK_TYPES(fn, CT_FUNCTION);

  if (is_any_primitive(fn))
    return makeint(((struct primitive *)fn)->op->seclevel);

  assert(TYPE(fn, closure));
  struct closure *c = fn;
  return makeint(c->code->seclevel);
}

TYPEDOP(function_name, , "`f -> `s. Returns name of `f (any primitive,"
        " closure, code, or mcode) if available; false otherwise.",
	(value fn),
	OP_LEAF | OP_NOESCAPE, "[fo].[sz]")
{
  CHECK_TYPES(fn, CT_TYPESET(TYPESET_FUNCTION
                             | P(type_mcode) | P(type_code)));

  if (is_any_primitive(fn))
    return ((struct primitive *)fn)->op->name;

  struct string *name;
  if (TYPE(fn, mcode) || TYPE(fn, code))
    name = ((struct code *)fn)->varname;
  else
    {
      assert(TYPE(fn, closure));
      struct closure *c = fn;
      name = c->code->varname;
    }

  return name ? name : makebool(false);
}

static void show_function(struct closure *c)
{
  struct code *code = c->code;
  if (code->help)
    pswrite(mudout, code->help);
  else
    pputs("undocumented", mudout);
  pputs(" [", mudout);
  pswrite(mudout, code->nicename);
  pprintf(mudout, ":%d", code->lineno);
  pputc(']', mudout);
}

TYPEDOP(profile, , "`f -> `x. Returns profiling information for `f:"
        " cons(#`calls, #`instructions) for mudlle functions, #`calls for"
        " primitives.",
        (value fn),
        OP_LEAF | OP_NOESCAPE, "[fo].[kn]")
{
  CHECK_TYPES(fn, CT_TYPESET(TYPESET_FUNCTION | TSET(code) | TSET(mcode)));

  if (is_any_primitive(fn))
    {
#ifdef PROFILE_CALL_COUNT
      return makeint(*((struct primitive *)fn)->op->call_count);
#else
      return makeint(0);
#endif
    }

  if (TYPE(fn, closure))
    fn = ((struct closure *)fn)->code;

  ulong size;
  if (TYPE(fn, mcode))
    {
#ifdef NOCOMPILER
      abort();
#else
      struct mcode *c = fn;
      size = c->code_length;
#endif
    }
  else
    {
      assert(TYPE(fn, code));
      struct icode *c = fn;
      size = c->instruction_count;
    }

  unsigned call_count = 0;
#ifdef PROFILE_CALL_COUNT
  struct code *c = fn;
  call_count = c->call_count;
#endif
  return alloc_list(makeint(call_count), makeint(size));
}

SECOP(apropos, HELP_PREFIX "apropos",
      "`s -> . Finds all global variables whose name contains"
      " the substring `s and prints them (with help)",
      (struct string *s), LVL_VALA, OP_LEAF | OP_NOESCAPE, "s.")
{
  CHECK_TYPES(s, string);

  GCPRO(s);
  for (int i = 0, envused = intval(environment->used); i < envused; ++i)
    {
      if (mudlle_string_isearch(GNAME(i), s) < 0)
        continue;

      pswrite(mudout, GNAME(i));
      pputs("\n  ", mudout);

      if (is_any_primitive(GVAR(i)))
        {
          struct primitive *op = GVAR(i);

          if (op->op->help)
            pprintf(mudout, "Primitive: %s\n", op->op->help);
          else
            pputs("Undocumented primitive\n", mudout);
        }
      else if (TYPE(GVAR(i), closure))
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

VAROP(quit, , "[`n] -> . Exit mudlle, optionally with exit code `n.",
       0, ("n.", "."))
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
TYPEDOP(gcstats, , "-> `v. Returns GC statistics: vector(`minor_count,"
        " `major_count, `size, `usage_minor, `usage_major, last gc sizes,"
        " gen0 gc sizes, gen1 gc sizes). The sizes vectors are vectors"
        " of vector(`objects, `rosize, `rwsize).", (void),
        OP_LEAF | OP_NOESCAPE, ".v")
{
  struct vector *last = NULL;
  struct vector *gen[2] = { 0 };
  GCPRO(gen[0], gen[1], last);
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

SECOP(reset_gcstats, "reset_gcstats!", "-> . Reset short GC statistics.",
      (void), LVL_VALA,
      OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".")
{
  gcstats.a = GCSTATS_ALLOC_NULL;
  undefined();
}

TYPEDOP(short_gcstats, , "-> `v. Returns short GC statistics. `v[`n] is"
        " a vector(`objects, `bytes) allcated for type `n.", (void),
        OP_LEAF | OP_NOESCAPE, ".v")
{
  struct gcstats stats = gcstats;
  struct vector *v = alloc_vector(last_type);
  GCPRO(v);
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

#endif  /* GCSTATS */

TYPEDOP(gc_generation, , "-> `n. Returns the number of garbage collections"
	" run so far (minor or major). Cf. `gc_cmp().",
	(void), OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".n")
{
  return makeint(gcstats.minor_count + gcstats.major_count);
}

TYPEDOP(gc_cmp, , "`x0 `x1 -> `n. Compares `x0 and `x1 as == does, and"
        " returns -1 if `x0 is less than `x1, 0 if they are equal, or 1 if `x0"
	" is greater than `x1. The results are only stable within"
	" one `gc_generation().",
	(value a, value b),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "xx.n")
{
  return makeint((long)a < (long)b ? -1 : (long)a != (long)b);
}

TYPEDOP(gc_hash, ,
        "`x -> `n. Returns a non-negative hash number for object `x,"
        " which is valid while `gc_generation() is constant.",
	(value x),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makeint(symbol_nhash((const char *)&x, sizeof x,
                              TAGGED_INT_BITS - 1));
}

UNSAFEOP(garbage_collect, , "`n -> . Does a forced garbage collection,"
         " asserting room for `n bytes of allocations before another"
         " garbage collection has to be done.",
         (value n),
         OP_LEAF | OP_NOESCAPE, "n.")
{
  garbage_collect(GETRANGE(n, 0, LONG_MAX));
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
  DEFINE(apropos);
  DEFINE(quit);
#ifdef GCSTATS
  DEFINE(gcstats);
  DEFINE(short_gcstats);
  DEFINE(reset_gcstats);
#endif  /* GCSTATS */
  DEFINE(gc_generation);
  DEFINE(gc_cmp);
  DEFINE(gc_hash);
}
