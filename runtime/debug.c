/* $Log: debug.c,v $
 * Revision 1.4  1995/07/16  09:17:05  arda
 * Add GCSTATS option.
 * Misc bug fixes.
 *
 * Revision 1.3  1995/07/15  15:24:55  arda
 * Context cleanup.
 * Remove GCDEBUG.
 *
 * Revision 1.2  1994/10/09  15:14:32  arda
 * mudlle libraries.
 *
 * Revision 1.1  1994/10/09  07:18:17  arda
 * Add/Remove files
 * */

static char rcsid[] = "$Id: debug.c,v 1.4 1995/07/16 09:17:05 arda Exp $";

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

#ifdef MUME
#include "interact.h"
#include "struct.char.h"
#include "frontend.h"
#endif

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
      mprintf(mudout, ":%d", code->lineno);
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
	  struct gcpro gcpro1;

	  GCPRO1(v);
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
	  UNGCPRO();
	  
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

#ifndef MUME
OPERATION(quit, " -> . Exit mudlle", 0, (void),
	  0)
{
  exit(0);
}
#endif

#ifdef MUME
OPERATION(with_output, "p fn -> . Evaluates fn() with output sent to player p.\n\
If p is not a player, just evaluates fn() (no error).\n\
Output is restored when done",
	  2, (struct character *out, value code),
	  0)
{
  struct session_context new;
  value result;
  Mio newout = mudout, newerr = muderr;

  callable(code, 0);
  if (TYPE(out, type_character)) newout = newerr = out->ch;

  session_start(&new, minlevel, muduser, newout, newerr);
  result = catch_call0(code);
  session_end();

  if (exception_signal) /* Continue with exception handling */
    throw(exception_signal, exception_value);

  return result;
}
#endif

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
  DEFINE("profile", profile);
  DEFINE("dump_memory", dump_memory);
  DEFINE("apropos", apropos);
  DEFINE("debug", debug);

#ifdef MUME
  DEFINE("with_output", with_output);
#else
  DEFINE("quit", quit);
#endif
#ifdef GCSTATS
  DEFINE("gcstats", gcstats);
#endif
}
