/* $Log: print.c,v $
 * Revision 1.25  1995/04/29  20:05:23  arda
 * fix
 *
 * Revision 1.24  1994/10/09  06:42:53  arda
 * Libraries
 * Type inference
 * Many minor improvements
 *
 * Revision 1.23  1994/09/03  13:37:27  arda
 * Changed display of objects (use short descr, not name).
 *
 * Revision 1.22  1994/08/31  13:03:25  arda
 * Bug fixes (argh, no, new version of characters structures! (MD))
 *
 * Revision 1.21  1994/08/29  13:17:31  arda
 * Contagious immutability.
 * Global array of values instead of variables.
 * Direct recursion.
 *
 * Revision 1.20  1994/08/16  19:16:15  arda
 * Mudlle compiler for sparc now fully functional (68k compiler now needs
 * updating for primitives).
 * Changes to allow Sparc trap's for runtime errors.
 * Also added flags to primitives for better calling sequences.
 *
 * Revision 1.16  1994/03/23  14:31:29  arda
 * *** empty log message ***
 *
 * Revision 1.15  1994/02/24  08:33:02  arda
 * Owl: New error messages.
 *
 * Revision 1.14  1994/02/12  17:24:58  arda
 * Owl: Better code generated.
 *
 * Revision 1.13  1994/02/03  19:21:46  arda
 * nothing special(2)
 *
 * Revision 1.12  1994/01/08  12:49:51  dgay
 * Owl: Improved code generation for blocks (they are not implemented
 * as 0 argument functions anymore, they are folded into the current
 * function instead).
 *
 * Revision 1.11  1993/11/27  11:29:09  arda
 * Owl: Major changes to affect.
 *      Save mudlle data with players & objects.
 *      Change skill format on disk.
 *      Other minor changes.
 *      Still needs full debugging.
 *
 * Revision 1.10  1993/10/03  14:07:14  dgay
 * Bumper disun8 update.
 *
 * Revision 1.9  1993/07/21  20:37:00  un_mec
 * Owl: Added &&, ||, optimised if.
 *      Added branches to the intermediate language.
 *      Separated destiniation language generation into ins module
 *      (with some peephole optimisation)
 *      Standalone version of mudlle (mkf, runtime/mkf, mudlle.c) added to CVS
 *
 * Revision 1.8  1993/05/02  13:02:54  un_mec
 * Owl: ARGH! Bugs.
 *
 * Revision 1.7  1993/05/02  07:38:00  un_mec
 * Owl: New output (mudlle ports).
 *
 * Revision 1.6  1993/04/25  19:50:25  un_mec
 * Owl: Miscellaneous changes.
 *      I HATE fixing bugs twice.
 *
 * Revision 1.5  1993/03/29  09:24:22  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.4  1993/03/17  12:49:52  dgay
 * Fixed GC of help strings in code blocks.
 * Added security features.
 *
 * Revision 1.3  1993/03/14  16:14:45  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.3  1993/01/08  23:57:08  un_mec
 * Owl: Allow characters and objects to appear in mudlle.
 *
 * Revision 1.2  1992/12/30  14:10:51  un_mec
 * Owl:
 * Several changes:
 * - Variables don't have separate value & function cells, instead their are
 *   now 2 types: type_function & type_variable.
 * - print_value: New types (list, vector), printing rationalised.
 * - New type: list (Lisp style pair)
 * - lexer.l: Debug read_from_string
 * - debug_level & DEBUG macro provided to help debugging.
 *
 * Revision 1.1  1992/12/27  21:41:30  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

static char rcsid[] = "$Id: print.c,v 1.25 1995/04/29 20:05:23 arda Exp $";

#include <string.h>
#include <stddef.h>
#include <alloca.h>
#include <string.h>
#include <setjmp.h>
#include <stdlib.h>

#include "mudlle.h"
#include "print.h"
#include "types.h"
#include "table.h"
#include "objenv.h"
#include "code.h"
#ifdef MUME
#include "def.time.h"
#include "def.char.h"
#include "def.obj.h"
#include "struct.time.h"
#include "struct.char.h"
#include "struct.obj.h"
#include "macro.h"
#endif

#define MAX_PRINT_COUNT 400

static int prt_count;
jmp_buf print_complex;

static unsigned char writable_chars[256 / 8];
#define set_writable(c, ok) \
  do { if (ok) writable_chars[(c) >> 3] |= 1 << ((c) & 7); \
       else writable_chars[(c) >> 3] &= ~(1 << ((c) & 7)); } while(0)
#define writable(c) (writable_chars[(c) >> 3] & (1 << ((c) & 7)))

static void _print_value(struct oport *f, prt_level level, value v);

static void write_string(struct oport *p, prt_level level, struct string *print)
{
  ulong l = string_len(print);

  if (level == prt_display)
    pswrite(p, print, 0, l);
  else
    {
      struct gcpro gcpro1;
      unsigned char *str = (unsigned char *)alloca(l + 1);
      unsigned char *endstr;

      memcpy((char *)str, print->str, l + 1);
      GCPRO1(p);
      /* The NULL byte at the end doesn't count */
      endstr = str + l;

      pputc('"', p);
      while (str < endstr)
	{
	  unsigned char *pos = str;

	  while (pos < endstr && writable(*pos)) pos++;
	  pwrite(p, (char *)str, pos - str);
	  if (pos < endstr)	/* We stopped for a \ */
	    {
	      pputc('\\', p);
	      switch (*pos)
		{
		case '\\': case '"': pputc(*pos, p); break;
		case '\n': pputc('n', p); break;
		case '\t': pputc('t', p); break;
		case '\f': pputc('f', p); break;
		default: pprintf(p, "%o", *pos); break;
		}
	      str = pos + 1;
	    }
	  else str = pos;
	}
      pputc('"', p);
      UNGCPRO();
    }
}

static int write_instruction(struct oport *f, instruction *i)
{
  ubyte byte1, byte2;
  ubyte op;
  instruction *old_i = i;
  static char *brname[] = { "", "(loop)", "(nz)", "(z)" };
  static char *builtin_names[] = { 
    "eq", "neq", "gt", "lt", "le", "ge", "ref", "set",
    "add", "sub", "bitand", "bitor", "not" };

#define insubyte() (*i++)
#define insbyte() ((byte)insubyte())
#define insuword() (byte1 = *i++, byte2 = *i++, (byte1 << 8) + byte2)
#define insword() ((word)insuword())

  op = insubyte();

  if (op >= op_recall && op <= op_closure_var + closure_var)
    {
      static char *opname[] = { "recall", "assign", "closure var" };
      static char *classname[] = { "local", "closure", "global" };

      if ((op - op_recall) %3 == global_var)
	pprintf(f, "%s[%s] %lu\n", opname[(op - op_recall) / 3],
		classname[(op - op_recall) % 3], insuword());
      else
	pprintf(f, "%s[%s] %lu\n", opname[(op - op_recall) / 3],
		classname[(op - op_recall) % 3], insubyte());
    }
  else if (op >= op_builtin_eq && op <= op_builtin_not)
    pprintf(f, "builtin_%s\n", builtin_names[op - op_builtin_eq]);
  else if (op >= op_typecheck && op < op_typecheck + last_synthetic_type)
    pprintf(f, "typecheck %d\n", op - op_typecheck);
  else switch (op)
    {
    case op_return: pprintf(f, "return\n"); break;
    case op_constant1: pprintf(f, "constant %u\n", insubyte()); break;
    case op_constant2: pprintf(f, "constant %u\n", insuword()); break;
    case op_integer1: pprintf(f, "integer1 %d\n", insbyte()); break;
    case op_integer2: pprintf(f, "integer2 %d\n", insword()); break;
    case op_closure: pprintf(f, "closure %u\n", insubyte()); break;
    case op_closure_code1: pprintf(f, "closure code %u\n", insubyte()); break;
    case op_closure_code2: pprintf(f, "closure code %u\n", insuword()); break;
    case op_execute: pprintf(f, "execute %u\n", insubyte()); break;
    case op_execute_global1: pprintf(f, "execute[global %u] 1\n", insuword()); break;
    case op_execute_global2: pprintf(f, "execute[global %u] 2\n", insuword()); break;
    case op_execute_primitive1: pprintf(f, "execute[primitive %u] 1\n", insuword()); break;
    case op_execute_primitive2: pprintf(f, "execute[primitive %u] 2\n", insuword()); break;
    case op_argcheck: pprintf(f, "argcheck %u\n", insubyte()); break;
    case op_varargs: pprintf(f, "varargs\n"); break;
    case op_discard: pprintf(f, "discard\n"); break;
    case op_pop_n: pprintf(f, "pop %u\n", insubyte()); break;
    case op_exit_n: pprintf(f, "exit %u\n", insubyte()); break;
    case op_branch1: case op_branch_z1: case op_branch_nz1: case op_loop1:
      pprintf(f, "branch%s %d\n", brname[(op - op_branch1) / 2], insbyte());
      break;
    case op_branch2: case op_branch_z2: case op_branch_nz2: case op_loop2:
      pprintf(f, "wbranch%s %d\n", brname[(op - op_branch1) / 2], insword());
      break;
    case op_clear_local:
      pprintf(f, "clear[local] %lu\n", insubyte());
      break;
    default: pprintf(f, "Opcode %d\n", op); break;
    }
  return i - old_i;
}

static void write_code(struct oport *f, struct code *c)
{
  instruction *ins;
  ulong nbins, i;
  struct gcpro gcpro1, gcpro2;

  GCPRO2(f, c);
  ins = (instruction *)((char *)c + offsetof(struct code, constants[c->nb_constants]));
  nbins = (instruction *)((char *)c + c->o.size) - ins;
  pprintf(f, "Code %lu bytes:\n", nbins);
  i = 0;
  while (i < nbins)
    {
      ins = (instruction *)((char *)c + offsetof(struct code, constants[c->nb_constants]));
      i += write_instruction(f, ins + i);
    }

  pprintf(f, "\n%u locals, %u stack, seclevel %u, %u constants:\n",
	  c->nb_locals, c->stkdepth, c->seclevel, c->nb_constants);
  for (i = 0; i < c->nb_constants; i++)
    {
      pprintf(f, "%lu: ", i);
      _print_value(f, prt_examine, c->constants[i]);
      pprintf(f, "\n");
    }
  UNGCPRO();
}

static void write_closure(struct oport *f, struct closure *c)
{
  ulong nbvar = (c->o.size - offsetof(struct closure, variables)) / sizeof(value), i;
  struct gcpro gcpro1, gcpro2;

  GCPRO2(f, c);
  pprintf(f, "Closure, code is\n");
  _print_value(f, prt_examine, c->code);
  pprintf(f, "\nand %lu variables are\n", nbvar);

  for (i = 0; i < nbvar; i++) 
    {
      pprintf(f, "%lu: ", i);
      _print_value(f, prt_examine, c->variables[i]);
      pprintf(f, "\n");
    }
  UNGCPRO();
}

static void write_vector(struct oport *f, prt_level level, struct vector *v)
{
  ulong len = vector_len(v), i;
  struct gcpro gcpro1, gcpro2;

  GCPRO2(f, v);
  pprintf(f, "[");
  for (i = 0; i < len; i++)
    {
      pputc(' ', f);
      _print_value(f, level, v->data[i]);
    }
  pprintf(f, " ]");
  UNGCPRO();
}

static void write_list(struct oport *f, prt_level level, struct list *v)
{
  struct gcpro gcpro1, gcpro2;

  GCPRO2(f, v);
  pprintf(f, "(");
  do {
    _print_value(f, level, v->car);
    if (!TYPE(v->cdr, type_pair)) break;
    pputc(' ', f);
    v = v->cdr;
  } while (1);
  
  if (v->cdr)
    {
      pputs(" . ", f);
      _print_value(f, level, v->cdr);
    }
  pprintf(f, ")");
  UNGCPRO();
}

static void write_character(struct oport *f, prt_level level, struct character *c)
{
#ifdef MUME
  struct char_data *ch = c->ch;

  if (level == prt_display) pputs(CHAR_SHORT(ch), f);
  else if (IS_NPC(ch)) pprintf(f, "{NPC %s}", NPC_SHORT(ch));
  else pprintf(f, "{player %s}", PC_NAME(ch));
#endif
}

static void write_object(struct oport *f, prt_level level, struct object *c)
{
#ifdef MUME
  if (level == prt_display && c->obj->short_description)
    pputs(c->obj->short_description, f);
  else pprintf(f, "{object %s}", c->obj->name);
#endif
}

static void write_integer(struct oport *f, long v)
{
  char buf[INTSTRLEN];

  pputs(int2str(buf, 10, (ulong)v, TRUE), f);
}

static void _print_value(struct oport *f, prt_level level, value v)
{
  static char *typename[last_type] = {
    "code", "closure", "variable", "internal", "primitive", "varargs", "secure",
    "integer", "string", "vector", "list", "symbol", "table", "private",
    "object", "character", "gone", "output-port", "mcode" };
  static char visible_at[][last_type] = {
    /* Display */ { 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0 },
    /* Print */   { 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0 },
    /* Examine */ { 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0 } };
  struct gcpro gcpro1, gcpro2;

  if (prt_count++ > MAX_PRINT_COUNT) longjmp(print_complex, 0);

  if (integerp(v)) write_integer(f, intval(v));
  else if (!v) pprintf(f, "null");
  else
    {
      struct obj *obj = v;

      assert(obj->type < last_type);
      if (!visible_at[level][obj->type])
	pprintf(f, "{%s}", typename[obj->type]);
      else
	switch (obj->type)
	  {
	  default: assert(0);
	  case type_string: write_string(f, level, v); break;
	  case type_symbol:
	    {
	      struct symbol *sym = v;

	      GCPRO2(f, sym);
	      pprintf(f, "<");
	      write_string(f, level, sym->name);
	      pprintf(f, ",");
	      _print_value(f, level, sym->data);
	      pprintf(f, ">");
	      UNGCPRO();
	      break;
	    }
	  case type_variable:
	    {
	      struct variable *var = v;

	      GCPRO2(f, var);
	      pprintf(f, "variable = "); _print_value(f, level, var->value);
	      UNGCPRO();
	      break;
	    }
	  case type_code: write_code(f, v); break;
	  case type_closure: write_closure(f, v); break;
	  /*case type_table: write_table(f, v); break;*/
	  case type_pair: write_list(f, level, v); break;
	  case type_vector: write_vector(f, level, v); break;
	  case type_character: write_character(f, level, v); break;
	  case type_object: write_object(f, level, v); break;
	  }
    }
}

void output_value(struct oport *f, prt_level level, value v)
{
  /* Optimise common cases (avoid complexity check overhead) */
  if (integerp(v)) write_integer(f, intval(v));
  else if (!v) pputs_cst("null", f);
  else if (((struct obj *)v)->type == type_string) write_string(f, level, v);
  else
    {
      struct gcpro *old_gcpro = gcpro;

      if (setjmp(print_complex)) /* exit when problems */
	{
	  gcpro = old_gcpro;
	  pputs_cst("<complex>", f);
	}
      else
	{
	  struct gcpro gcpro1, gcpro2, gcpro3;
	  struct oport *p;

	  GCPRO2(f, v);
	  p = make_string_outputport();
	  GCPRO(gcpro3, p);
	  prt_count = 0;
	  _print_value(p, level, v);
	  UNGCPRO();
	  port_append(f, p);
	  opclose(p);
	}
    }
}

#include "mudio.h"
#ifdef MUME

void mprint(Mio f, prt_level level, value v)
{
  struct gcpro gcpro1;
  struct oport *p;

  GCPRO1(v);
  p = char_output(f);
  if (p) output_value(p, level, v);
  UNGCPRO();
}

#else

void mprint(Mio f, prt_level level, value v)
{
  struct gcpro *old_gcpro = gcpro;

  if (setjmp(print_complex)) /* exit when problems */
    {
      gcpro = old_gcpro;
      mputs("<complex>", f);
    }
  else
    {
      struct gcpro gcpro1, gcpro2;
      struct oport *p;
      char *s;

      GCPRO1(v);
      p = make_string_outputport();
      GCPRO(gcpro2, p);
      prt_count = 0;
      _print_value(p, level, v);
      s = port_cstring(p);
      opclose(p);
      UNGCPRO();
      mputs(s, f);
      free(s);
    }
}

#endif

void print_init(void)
{
  unsigned int c;

  for (c = 32; c < 127; c++) set_writable(c, TRUE);
  set_writable('"', FALSE);
  set_writable('\\', FALSE);
  for (c = 160; c < 256; c++) set_writable(c, TRUE); 
}
