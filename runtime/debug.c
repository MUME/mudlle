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

#include <ctype.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/param.h>
#include <unistd.h>
#include <string.h>

#include "runtime/runtime.h"
#include "runtime/basic.h"
#include "global.h"
#include "alloc.h"
#include "interpret.h"
#include "context.h"


static void show_function(struct closure *c);

#  define HELP_PREFIX ""

TYPEDOP(help, HELP_PREFIX "help",
        "`f -> . Prints help on function `f", 1, (value v),
        OP_LEAF, "x.")
{
  if (TYPE(v, type_primitive) || TYPE(v, type_varargs))
    {
      struct primitive *op = v;

      mputs(op->op->help, mudout);
    }
  else if (TYPE(v, type_secure))
    {
      struct primitive *op = v;

      mprintf(mudout, "Secure %d: %s", op->op->seclevel, op->op->help);
    }
  else if (TYPE(v, type_closure)) show_function(v);
  else mprintf(mudout, "This variable isn't executable" EOL);
  mputs(EOL, mudout);
  undefined();
}

TYPEDOP(help_string, 0, "`f -> `s. Returns `f's help string, or null if none",
	1, (value v),
	OP_LEAF | OP_NOESCAPE, "f.[su]")
{
  if (TYPE(v, type_primitive) || TYPE(v, type_varargs) || TYPE(v, type_secure))
    {
      struct primitive *op = v;

      return alloc_string(op->op->help);
    }
  if (TYPE(v, type_closure)) 
    {
      struct closure *c = v;

      if (c->code->o.type == type_code)
        return c->code->help;
      if (c->code->o.type == type_mcode)
        return ((struct mcode *)c->code)->help;
      return alloc_string(((struct primitive *)c->code)->op->help);
    }
  runtime_error(error_bad_type);
  NOTREACHED;
}

TYPEDOP(defined_in, 0, "`f -> `v. Returns information on where `f is defined"
        " [`filename, `lineno].",
        1, (value fn),
        OP_LEAF | OP_NOESCAPE, "[fo].v")
{
  struct gcpro gcpro1;
  struct vector *v;
  struct string *filename;
  uword lineno;

  if (TYPE(fn, type_closure)) 
    fn = ((struct closure *)fn)->code;

  if (TYPE(fn, type_code))
    {
      struct code *code = fn;
      filename = code->filename;
      lineno   = code->lineno;
    }
  else if (TYPE(fn, type_mcode))
    {
      struct mcode *mcode = fn;
      filename = mcode->filename;
      lineno   = mcode->lineno;
    }
  else if (TYPE(fn, type_primitive) || TYPE(fn, type_varargs) ||
	   TYPE(fn, type_secure))
    {
      struct primitive *prim = fn;
      filename = alloc_string(prim->op->filename);
      lineno   = prim->op->lineno;
    }
  else
    runtime_error(error_bad_type);

  GCPRO1(filename);
  v = alloc_vector(2);
  UNGCPRO();
  v->data[0] = filename;
  v->data[1] = makeint(lineno);

  return v;
}

UNSAFEOP(closure_variables, 0,
	 "`f -> `v. Returns a vector of the closure variable values of"
         " function `f",
	 1, (struct closure *fn),
	 OP_LEAF | OP_NOESCAPE)
{
  ulong nbvar, i;
  struct gcpro gcpro1;
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
        OP_LEAF, "o.x")
{
  TYPEIS(v, type_variable);
  return v->vvalue;
}

TYPEDOP(function_seclevel, 0, "`f -> `n. Returns the security level of the"
        " function `f",
	1, (value fn), OP_LEAF | OP_NOESCAPE, "f.n")
{
  if (TYPE(fn, type_primitive) || TYPE(fn, type_varargs))
    return makeint(0);

  if (TYPE(fn, type_secure))
    return makeint(((struct primitive *)fn)->op->seclevel);

  if (TYPE(fn, type_closure))
    {
      struct closure *c = fn;
      if (TYPE(c->code, type_code))
	return makeint(((struct code *)c->code)->seclevel);
      if (TYPE(c->code, type_mcode))
	return makeint(((struct mcode *)c->code)->seclevel);
    }

  runtime_error(error_bad_type);
  NOTREACHED;
}

TYPEDOP(function_name, 0, "`f -> `s. Returns name of `f if available, false"
        " otherwise",
	1, (value fn),
	OP_LEAF | OP_NOESCAPE, "[fo].S")
{
  struct string *name = NULL;

  if (TYPE(fn, type_mcode))
    {
      name = ((struct mcode *)fn)->varname;
      goto got_name;
    }

  if (TYPE(fn, type_code))
    {
      name = ((struct code *)fn)->varname;
      goto got_name;
    }

  if (TYPE(fn, type_primitive) || TYPE(fn, type_varargs) ||
      TYPE(fn, type_secure))
    {
      struct primitive *op = fn;
      return alloc_string(op->op->name);
    }

  if (TYPE(fn, type_closure)) 
    {
      struct closure *c = fn;

      if (c->code->o.type == type_code) 
	name = c->code->varname;
      else if (c->code->o.type == type_mcode)
	{
	  struct mcode *code = (struct mcode *)c->code;

	  name = code->varname;
	}

      goto got_name;
    }
  runtime_error(error_bad_type);
  NOTREACHED;

got_name:
  return name ? name : makebool(false);
}

static void show_function(struct closure *c)
{
  if (c->code->o.type == type_mcode)
    {
      struct mcode *code = (struct mcode *)c->code;

      if (code->help) mprint(mudout, prt_display, code->help);
      else mputs("undocumented", mudout);
      mputs(" [", mudout);
      mprint(mudout, prt_display, code->filename);
      mprintf(mudout, ":%d", code->lineno);
      mputs("]", mudout);
    }
  else if (c->code->o.type == type_code)
    {
      struct code *code = c->code;

      if (code->help) mprint(mudout, prt_display, code->help);
      else mputs("undocumented", mudout);
      mputs(" [", mudout);
      mprint(mudout, prt_display, code->filename);
      mprintf(mudout, ":%d", code->lineno);
      mputs("]", mudout);
    }
  else code_help(c->code);
}

TYPEDOP(profile, 0, "`f -> `x. Returns profiling information for `f:"
        " cons(#`calls, #`instructions) for mudlle functions, #`calls for"
        " primitives",
        1, (value fn),
        OP_LEAF, "[fo].[kn]")
{
  if (TYPE(fn, type_closure)) fn = ((struct closure *)fn)->code;

  if (TYPE(fn, type_code))
    {
      struct code *c = fn;
      return alloc_list(makeint(c->call_count),
                        makeint(c->instruction_count));
    }
  if (TYPE(fn, type_primitive)
      || TYPE(fn, type_secure)
      || TYPE(fn, type_varargs))
    return makeint(((struct primitive *)fn)->call_count);
  runtime_error(error_bad_type);
  NOTREACHED;
}

UNSAFEOP(dump_memory, 0, " -> . Dumps GC memory (for use by profiler)",
	 0, (void),
	 OP_LEAF)
{
  dump_memory();
  undefined();
}

static int instr(char *s1, char *in)
{
  while (*in)
    {
      char *s = s1, *ins = in;

      while (*s && tolower(*s) == tolower(*ins)) { s++; ins++; }
      if (!*s) return true;
      in++;
    }
  return false;
}

TYPEDOP(apropos, HELP_PREFIX "apropos",
        "`s -> . Finds all global variables whose name contains"
        " the substring `s and prints them (with help)",
        1, (struct string *s),
        OP_LEAF, "s.")
{
  struct list *globals;
  struct gcpro gcpro1, gcpro2;

  TYPEIS(s, type_string);

  GCPRO1(s);
  globals = global_list();
  GCPRO(gcpro2, globals);
  while (globals)
    {
      struct symbol *sym = globals->car;

      if (instr(s->str, sym->name->str))
	{
	  value v = GVAR(intval(sym->data));
	  struct gcpro gcpro3;

	  GCPRO(gcpro3, v);
	  mprint(mudout, prt_display, sym->name);
	  mputs(EOL "  ", mudout);
	  if (TYPE(v, type_primitive)
              || TYPE(v, type_secure)
              || TYPE(v, type_varargs))
	    {
	      struct primitive *op = v;

	      if (op->op->help) 
		mprintf(mudout, "Primitive: %s" EOL, op->op->help);
	      else mputs("Undocumented primitive" EOL, mudout);
	    }
	  else if (TYPE(v, type_closure))
	    {
	      mputs("Function: ", mudout);
	      show_function(v);
	      mputs(EOL, mudout);
	    }
	  else mprintf(mudout, "Variable" EOL);
	  UNGCPRO1(gcpro3);
	  
	}
      globals = globals->cdr;
    }
  UNGCPRO();
  undefined();
}

TYPEDOP(debug, 0, "`n -> . Set debug level (0 = no debug)", 1, (value c),
        OP_LEAF | OP_NOALLOC, "n.")
{
  debug_level = GETINT(c);
  undefined();
}

static const typing quit_tset = { "n.", ".", NULL };
FULLOP(quit, 0, "[`n] -> . Exit mudlle, optionally with exit code `n.",
       -1, (struct vector *args, ulong nargs), 0, 0, quit_tset, static)
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
TYPEDOP(gcstats, 0, " -> `v. Returns GC statistics: vector(`minor_count,"
        " `major_count, `size, `usage_minor, `usage_major, last gc sizes,"
        " gen0 gc sizes, gen1 gc sizes). The sizes vectors are as"
        " that of `short_gcstats()", 0, (void), OP_LEAF, ".v")
{
  struct gcstats stats;
  struct vector *gen0, *gen1 = NULL, *last = NULL, *v;
  int i;
  struct gcpro gcpro1, gcpro2, gcpro3;

  stats = gcstats;

  gen0 = alloc_vector(2 * last_type);
  GCPRO3(gen0, gen1, last);
  gen1 = alloc_vector(2 * last_type);
  last = alloc_vector(2 * last_type);
  for (i = 0; i < last_type; i++)
    {
      last->data[2 * i] = makeint(stats.lnb[i]);
      last->data[2 * i + 1] = makeint(stats.lsizes[i]);
      gen0->data[2 * i] = makeint(stats.g0nb[i]);
      gen0->data[2 * i + 1] = makeint(stats.g0sizes[i]);
      gen1->data[2 * i] = makeint(stats.g1nb[i]);
      gen1->data[2 * i + 1] = makeint(stats.g1sizes[i]);
    }
  v = alloc_vector(8);
  v->data[0] = makeint(stats.minor_count);
  v->data[1] = makeint(stats.major_count);
  v->data[2] = makeint(stats.size);
  v->data[3] = makeint(stats.usage_minor);
  v->data[4] = makeint(stats.usage_major);
  v->data[5] = last;
  v->data[6] = gen0;
  v->data[7] = gen1;

  UNGCPRO();

  return v;
}

OPERATION(reset_gcstats, "reset_gcstats!", " -> . Reset short GC statistics",
          0, (void),
	  OP_LEAF | OP_NOALLOC)
{
  memset(gcstats.anb, 0, sizeof gcstats.anb);
  memset(gcstats.asizes, 0, sizeof gcstats.asizes);
  undefined();
}

OPERATION(short_gcstats, 0, " -> `v. Returns short GC statistics. `v[2*`n] is"
          " the number of allocations for type `n, and `v[2*`n+1] is total"
          " bytes of allocation for type `n", 0, (void),
	  OP_LEAF)
{
  struct gcstats stats = gcstats;
  struct vector *v = alloc_vector(2 * last_type);
  int i;

  for (i = 0; i < last_type; ++i)
    {
      v->data[2 * i] = makeint(stats.anb[i]);
      v->data[2 * i + 1] = makeint(stats.asizes[i]);
    }

  return v;
}

TYPEDOP(gc_generation, 0, "-> `n. Returns the number of garbage collections"
	" run so far (minor or major). Cf. `gc_cmp().",
	0, (void), OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".n")
{
  return makeint(gcstats.minor_count + gcstats.major_count);
}

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
  unsigned long v = (unsigned long)x;

#define ROL(x, n) (((x) << (n)) | ((x) >> (sizeof (x) * 8 - (n))))
  v = v ^ ROL(v, 3) ^ ROL(v, 7) ^ ROL(v, 13) ^ ROL(v, 23);
#undef ROL

  return makeint(v & MAX_TAGGED_INT);
}

#endif

UNSAFEOP(garbage_collect, 0, "`n -> . Does a forced garbage collection,"
	 " asserting room for `n bytes of allocations before another"
	 " garbage collection has to be done.",
	 1, (value n),
	 OP_LEAF)
{
  ISINT(n);

  garbage_collect(intval(n));
  undefined();
}

void debug_init(void)
{
  DEFINE(garbage_collect);
  DEFINE(help);
  DEFINE(help_string);
  DEFINE(defined_in);
  DEFINE(closure_variables);
  DEFINE(variable_value);
  DEFINE(function_name);
  DEFINE(function_seclevel);
  DEFINE(profile);
  DEFINE(dump_memory);
  DEFINE(apropos);
  DEFINE(debug);
  DEFINE(quit);
#ifdef GCSTATS
  DEFINE(gcstats);
  DEFINE(short_gcstats);
  DEFINE(reset_gcstats);
  DEFINE(gc_generation);
  DEFINE(gc_cmp);
  DEFINE(gc_hash);
#endif
}
