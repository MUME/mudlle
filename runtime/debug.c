/*
 * Copyright (c) 1993-1999 David Gay and Gustav Hållberg
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

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/param.h>
#include <ctype.h>
#include "runtime/runtime.h"
#include "runtime/basic.h"
#include "mudio.h"
#include "global.h"
#include "alloc.h"
#include "interpret.h"

static void show_function(struct closure *c);

OPERATION(help, "fn -> . Provides help on function fn", 1, (value v),
	  OP_LEAF)
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

TYPEDOP(help_string, "fn -> s. Returns fn's help string, or false if none",
	1, (value v),
	OP_LEAF | OP_NOESCAPE, "f.S")
{
  if (TYPE(v, type_primitive) || TYPE(v, type_varargs) || TYPE(v, type_secure))
    {
      struct primitive *op = v;

      return alloc_string(op->op->help);
    }
  if (TYPE(v, type_closure)) 
    {
      struct closure *c = v;

      if (c->code->o.type == type_code) return c->code->help;
      if (c->code->o.type == type_mcode) return ((struct mcode *)c->code)->help;
      return alloc_string(((struct primitive *)c->code)->op->help);
    }
  runtime_error(error_bad_type);
  NOTREACHED;
}

TYPEDOP(defined_in, "fn -> v. Returns information on where fn is defined (filename, lineno). Returns false for primitives",
	1, (value fn),
	OP_LEAF | OP_NOESCAPE, "f.v")
{
  if (TYPE(fn, type_primitive) || TYPE(fn, type_varargs) ||
      TYPE(fn, type_secure))
    return makebool(FALSE);
  if (TYPE(fn, type_closure)) 
    {
      struct closure *c = fn;
      struct vector *v;
      struct string *filename;
      uword lineno;
      struct gcpro gcpro1;

      if (c->code->o.type == type_code) 
	{
	  filename = c->code->filename;
	  lineno = c->code->lineno;
	}
      else if (c->code->o.type == type_mcode)
	{
	  struct mcode *code = (struct mcode *)c->code;

	  filename = code->filename;
	  lineno = code->lineno;
	}
      else
	return makebool(FALSE);

      GCPRO1(filename);
      v = alloc_vector(2);
      UNGCPRO();
      v->data[0] = filename;
      v->data[1] = makeint(lineno);

      return v;
    }
  runtime_error(error_bad_type);
  NOTREACHED;
}

TYPEDOP(function_name, "fn -> s. Returns name of fn if available, false otherwise",
	1, (value fn),
	OP_LEAF | OP_NOESCAPE, "f.x")
{
  if (TYPE(fn, type_primitive) || TYPE(fn, type_varargs) ||
      TYPE(fn, type_secure))
    {
      struct primitive *op = fn;

      return alloc_string(op->op->name);
    }
  if (TYPE(fn, type_closure)) 
    {
      struct string *name = NULL;
      struct closure *c = fn;

      if (c->code->o.type == type_code) 
	name = c->code->varname;
      else if (c->code->o.type == type_mcode)
	{
	  struct mcode *code = (struct mcode *)c->code;

	  name = code->varname;
	}

      if (name)
	return name;
      else
	return makebool(FALSE);
    }
  runtime_error(error_bad_type);
  NOTREACHED;
}

static void show_function(struct closure *c)
{
  if (c->code->o.type == type_mcode)
    {
      struct mcode *code = (struct mcode *)c->code;

      if (code->help) mprint(mudout, prt_display, code->help);
      else mputs("undocumented", mudout);
      mputs(" {", mudout);
      mprint(mudout, prt_display, code->filename);
      mprintf(mudout, ";%d", code->lineno);
      mputs("}", mudout);
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

OPERATION(profile, "fn -> x. Returns profiling information for function fn: \n\
(#calls #instructions) for mudlle functions,\n\
#calls for primitives",
	  1, (value fn),
	  OP_LEAF)
{
  struct gcpro gcpro1;
  struct list *tmp;

  if (TYPE(fn, type_closure)) fn = ((struct closure *)fn)->code;

  if (TYPE(fn, type_code))
    {
      struct code *c = fn;

      GCPRO1(c);
      tmp = alloc_list(makeint(c->instruction_count), NULL);
      tmp = alloc_list(makeint(c->call_count), tmp);
      UNGCPRO();

      return tmp;
    }
  else if (TYPE(fn, type_primitive) || TYPE(fn, type_secure) || TYPE(fn, type_varargs))
    return makeint(((struct primitive *)fn)->call_count);
  else runtime_error(error_bad_type);
  NOTREACHED;
}

UNSAFEOP(dump_memory, " -> . Dumps GC memory (for use by profiler)",
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
      if (!*s) return TRUE;
      in++;
    }
  return FALSE;
}

OPERATION(apropos, "s -> . Finds all global variables whose name contains substring s and prints them (with help)",
	  1, (struct string *s),
	  OP_LEAF)
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
	  if (TYPE(v, type_primitive) || TYPE(v, type_secure) || TYPE(v, type_varargs))
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

OPERATION(debug, "n -> . Set debug level (0 = no debug)", 1, (value c),
	  OP_LEAF | OP_NOALLOC)
{
  ISINT(c);
  debug_level = intval(c);
  undefined();
}

OPERATION(quit, " -> . Exit mudlle", 0, (void),
	  0)
{
  exit(0);
}

#ifdef GCSTATS
OPERATION(gcstats, " -> l. Returns GC statistics", 0, (void),
	  OP_LEAF)
{
  struct gcstats stats;
  struct vector *gen0, *gen1, *last, *v;
  int i;
  struct gcpro gcpro1, gcpro2, gcpro3;

  stats = gcstats;

  gen0 = alloc_vector(2 * last_type);
  GCPRO(gcpro1, gen0);
  gen1 = alloc_vector(2 * last_type);
  GCPRO(gcpro2, gen1);
  last = alloc_vector(2 * last_type);
  GCPRO(gcpro3, last);
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

OPERATION(reset_gcstats, " -> . Reset short GC statistics", 0, (void),
	  OP_LEAF | OP_NOALLOC)
{
  memset(gcstats.anb, 0, sizeof gcstats.anb);
  memset(gcstats.asizes, 0, sizeof gcstats.asizes);
  undefined();
}

OPERATION(short_gcstats, " -> l. Returns short GC statistics", 0, (void),
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

#endif

UNSAFEOP(garbage_collect, "n -> . Does a forced garbage collection",
	 1, (value n),
	 OP_LEAF)
{
  ISINT(n);

  garbage_collect(intval(n));
  undefined();
}

void debug_init(void)
{
  DEFINE("garbage_collect", garbage_collect);
  DEFINE("help", help);
  DEFINE("help_string", help_string);
  DEFINE("defined_in", defined_in);
  DEFINE("function_name", function_name);
  DEFINE("profile", profile);
  DEFINE("dump_memory", dump_memory);
  DEFINE("apropos", apropos);
  DEFINE("debug", debug);
  DEFINE("quit", quit);
#ifdef GCSTATS
  DEFINE("gcstats", gcstats);
  DEFINE("short_gcstats", short_gcstats);
  DEFINE("reset_gcstats!", reset_gcstats);
#endif
}
