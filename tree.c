/* $Log: tree.c,v $
 * Revision 1.9  1994/10/09  06:43:05  arda
 * Libraries
 * Type inference
 * Many minor improvements
 *
 * Revision 1.8  1994/08/16  19:16:24  arda
 * Mudlle compiler for sparc now fully functional (68k compiler now needs
 * updating for primitives).
 * Changes to allow Sparc trap's for runtime errors.
 * Also added flags to primitives for better calling sequences.
 *
 * Revision 1.5  1994/02/24  08:33:07  arda
 * Owl: New error messages.
 *
 * Revision 1.4  1994/01/29  19:50:38  dgay
 * Owl: add file & line information to functions.
 *
 * Revision 1.3  1993/03/29  09:24:39  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.4  1993/03/17  12:49:59  dgay
 * Fixed GC of help strings in code blocks.
 * Added security features.
 *
 * Revision 1.3  1993/03/14  16:15:04  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.1  1992/12/27  21:41:39  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

static char rcsid[] = "$Id: tree.c,v 1.9 1994/10/09 06:43:05 arda Exp $";

#include <stdarg.h>
#include "mudlle.h"
#include "tree.h"
#include "compile.h"

file new_file(int class, const char *name, vlist imports, vlist defines, vlist reads,
	      vlist writes, block body)
{
  file new = allocate(memory, sizeof *new);

  new->class = class;
  new->name = name;
  new->imports = imports;
  new->defines = defines;
  new->reads = reads;
  new->writes = writes;
  new->body = body;

  return new;
}

function new_function(mtype type, const char *help, vlist args, component value,
		      int lineno, const char *filename)
{
  function new = allocate(memory, sizeof *new);

  new->type = type;
  new->help = help;
  new->args = args;
  new->varargs = FALSE;
  new->value = value;
  new->lineno = lineno;
  new->filename = filename;
  new->varname = NULL;

  return new;
}

function new_vfunction(mtype type, const char *help, const char *arg, component value,
		       int lineno, const char *filename)
{
  function new = allocate(memory, sizeof *new);

  new->type = type;
  new->help = help;
  /* using type_vector implies a useless type check */
  new->args = new_vlist(arg, stype_any, NULL);
  new->varargs = TRUE;
  new->value = value;
  new->lineno = lineno;
  new->filename = filename;
  new->varname = NULL;

  return new;
}

block new_codeblock(vlist locals, clist sequence)
{
  block new = allocate(memory, sizeof *new);

  new->locals = locals;
  new->sequence = sequence;

  return new;
}

clist new_clist(component c, clist next)
{
  clist new = allocate(memory, sizeof *new);

  new->next = next;
  new->c = c;

  return new;
}

cstlist new_cstlist(constant cst, cstlist next)
{
  cstlist new = allocate(memory, sizeof *new);

  new->next = next;
  new->cst = cst;

  return new;
}

vlist new_vlist(const char *var, mtype type, vlist next)
{
  vlist new = allocate(memory, sizeof *new);

  new->next = next;
  new->var = var;
  new->type = type;

  return new;
}

constant new_constant(int class, ...)
{
  va_list args;
  constant new = allocate(memory, sizeof *new);

  new->class = class;
  va_start(args, class);
  switch (class)
    {
    case cst_int:
      new->u.integer = va_arg(args, int);
      break;
    case cst_string:
      new->u.string = va_arg(args, const char *);
      break;
    case cst_list: case cst_array:
      new->u.constants = va_arg(args, cstlist);
      break;
    default: assert(0);
    }
  va_end(args);
  return new;
}

static clist make_clist(va_list args)
{
  int count;
  clist first = NULL, *scan = &first;

  for (count = va_arg(args, int); count > 0; count--)
    {
      *scan = new_clist(va_arg(args, component), NULL);
      scan = &(*scan)->next;
    }
  return first;
}
  
component new_component(int class, ...)
{
  va_list args;
  component new = allocate(memory, sizeof *new);

  new->class = class;
  va_start(args, class);
  switch (class)
    {
    case c_assign: 
      new->u.assign.symbol = va_arg(args, const char *);
      new->u.assign.value = va_arg(args, component);
      break;
    case c_recall:
      new->u.recall = va_arg(args, const char *);
      break;
    case c_constant:
      new->u.constant = va_arg(args, constant);
      break;
    case c_closure:
      new->u.closure = va_arg(args, function);
      break;
    case c_block:
      new->u.block = va_arg(args, block);
      break;
    case c_execute:
      new->u.execute = va_arg(args, clist);
      break;
    case c_builtin:
      new->u.builtin.fn = va_arg(args, unsigned int);
      new->u.builtin.args = make_clist(args);
      break;
    case c_labeled: case c_exit:
      new->u.labeled.name = va_arg(args, const char *);
      new->u.labeled.expression = va_arg(args, component);
      break;
    default: assert(0);
    }
  va_end(args);
  return new;
}

clist append_clist(clist l1, clist l2)
{
  clist last;

  if (!l1) return l2;
  if (!l2) return l1;

  for (last = l1; last->next; last = last->next) ;
  last->next = l2;

  return l1;
}

clist reverse_clist(clist l)
{
  clist prev = NULL;

  while (l)
    {
      clist next = l->next;
      
      l->next = prev;
      prev = l;
      l = next;
    }
  return prev;
}

cstlist reverse_cstlist(cstlist l)
{
  cstlist prev = NULL;

  while (l)
    {
      cstlist next = l->next;
      
      l->next = prev;
      prev = l;
      l = next;
    }
  return prev;
}

vlist append_vlist(vlist l1, vlist l2)
{
  vlist last;

  if (!l1) return l2;
  if (!l2) return l1;

  for (last = l1; last->next; last = last->next) ;
  last->next = l2;

  return l1;
}

vlist reverse_vlist(vlist l)
{
  vlist prev = NULL;

  while (l)
    {
      vlist next = l->next;
      
      l->next = prev;
      prev = l;
      l = next;
    }
  return prev;
}

/* Make a mudlle rep of a parse tree */
static value mudlle_parse_component(component c);

static value mudlle_vlist(vlist vars)
{
  value l = NULL;
  struct gcpro gcpro1;
  struct string *s;

  vars = reverse_vlist(vars);
  GCPRO1(l);
  while (vars)
    {
      value t;

      s = alloc_string(vars->var);
      t = alloc_list(s, makeint(vars->type));
      l = alloc_list(t, l);
      vars = vars->next;
    }
  UNGCPRO();
  return l;
}

static value mudlle_clist(clist exprs)
{
  value l = NULL;
  struct gcpro gcpro1;
  value c;

  exprs = reverse_clist(exprs);
  GCPRO1(l);
  while (exprs)
    {
      c = mudlle_parse_component(exprs->c);
      l = alloc_list(c, l);
      exprs = exprs->next;
    }
  UNGCPRO();
  return l;
}

static value mudlle_parse_component(component c)
{
  struct vector *mc;
  static char msize[] = { 2, 1, 1, 7, 1, 2, 2, 2, 2 };
  struct gcpro gcpro1;
  struct string *sym;
  value val;
  function f;

  mc = alloc_vector(msize[c->class] + 1);
  mc->data[0] = makeint(c->class);
  GCPRO1(mc);

  switch (c->class)
    {
    case c_assign:
      sym = alloc_string(c->u.assign.symbol);
      mc->data[1] = sym;
      val = mudlle_parse_component(c->u.assign.value);
      mc->data[2] = val;
      break;

    case c_recall:
      sym = alloc_string(c->u.recall);
      mc->data[1] = sym;
      break;

    case c_constant:
      val = make_constant(c->u.constant);
      mc->data[1] = val;
      break;

    case c_closure:
      f = c->u.closure;
      mc->data[1] = makeint(f->type);
      val = f->help ? alloc_string(f->help) : NULL;
      mc->data[2] = val;
      val = mudlle_vlist(reverse_vlist(f->args));
      mc->data[3] = val;
      mc->data[4] = makeint(f->varargs);
      val = mudlle_parse_component(f->value);
      mc->data[5] = val;
      mc->data[6] = makeint(f->lineno);
      val = make_filename(f->filename);
      mc->data[7] = val;
      break;

    case c_execute:
      val = mudlle_clist(c->u.execute);
      mc->data[1] = val;
      break;

    case c_builtin:
      mc->data[1] = makeint(c->u.builtin.fn);
      val = mudlle_clist(c->u.builtin.args);
      mc->data[2] = val;
      break;

    case c_block:
      val = mudlle_vlist(c->u.block->locals);
      mc->data[1] = val;
      val = mudlle_clist(c->u.block->sequence);
      mc->data[2] = val;
      break;

    case c_labeled: case c_exit:
      if (c->u.labeled.name) val = alloc_string(c->u.labeled.name);
      else val = NULL;
      mc->data[1] = val;
      val = mudlle_parse_component(c->u.labeled.expression);
      mc->data[2] = val;
      break;
      
    default:
      assert(0);
    }

  UNGCPRO();
  return mc;
}

value mudlle_parse(file f)
{
  struct vector *file = alloc_vector(7);
  struct gcpro gcpro1;
  value tmp;

  GCPRO1(file);
  file->data[0] = makeint(f->class);
  tmp = f->name ? alloc_string(f->name) : makebool(FALSE);
  file->data[1] = tmp;
  tmp = mudlle_vlist(f->imports);
  file->data[2] = tmp;
  tmp = mudlle_vlist(f->defines);
  file->data[3] = tmp;
  tmp = mudlle_vlist(f->reads);
  file->data[4] = tmp;
  tmp = mudlle_vlist(f->writes);
  file->data[5] = tmp;
  tmp = mudlle_parse_component(new_component(c_block, f->body));
  file->data[6] = tmp;
  UNGCPRO();

  return file;
}

#ifdef PRINT_CODE
static void print_constant(FILE *f, constant c);

static void print_list(FILE *f, cstlist l)
{
  int first = TRUE;

  while (l)
    {
      if (!first) fprintf(f, " ");
      first = FALSE;
      print_constant(f, l->cst);
      l = l->next;
    }
}

static void print_vlist(FILE *f, vlist l)
{
  int first = TRUE;

  while (l)
    {
      if (!first) fprintf(f, ", ");
      first = FALSE;
      if (l->type != stype_any) fprintf(f, "%d ", l->type);
      fputs(l->var, f);
      l = l->next;
    }
}

static void print_constant(FILE *f, constant c)
{
  switch (c->class)
    {
    case cst_int:
      fprintf(f, "%d", c->u.integer);
      break;
    case cst_string:
      fprintf(f, "\"%s\"" , c->u.string);
      break;
    case cst_list:
      fprintf(f, "(");
      print_list(f, c->u.constants);
      fprintf(f, ")");
      break;
    case cst_array:
      fprintf(f, "#(");
      print_list(f, c->u.constants);
      fprintf(f, ")");
      break;
    default: assert(0);
    }
}

static void print_component(FILE *f, component c);

static void print_block(FILE *f, block c)
{
  vlist vars = c->locals;
  clist sequence = c->sequence;

  fprintf(f, "[ ");
  if (vars)
    {
      print_vlist(f, vars);
      fprintf(f, "| ");
    }
  while (sequence)
    {
      print_component(f, sequence->c);
      fprintf(f, " ");
      sequence = sequence->next;
    }
  fprintf(f, "]");
}

static void print_clist(FILE *f, clist sequence)
{
  while (sequence)
    {
      fprintf(f, ", ");
      print_component(f, sequence->c);
      sequence = sequence->next;
    }
}

static void print_function(FILE *f, function fn)
{
  if (fn->help) fprintf(f, "fn \"%s\" (", fn->help);
  else fprintf(f, "fn (");
  print_vlist(f, fn->args);
  fprintf(f, ") ");
  print_component(f, fn->value);
}

static void print_component(FILE *f, component c)
{
  switch (c->class)
    {
    case c_assign:
      fprintf(f, "%s=", c->u.assign.symbol);
      print_component(f, c->u.assign.value);
      break;
    case c_recall:
      fprintf(f, "%s", c->u.recall);
      break;
    case c_execute:
      fprintf(f, "exec(");
      print_component(f, c->u.execute->c);
      print_clist(f, c->u.execute->next);
      fprintf(f, ")");
      break;
    case c_builtin:
      fprintf(f, "builtin(%d", c->u.builtin.fn);
      print_clist(f, c->u.builtin.args);
      fprintf(f, ")");
      break;
    case c_constant:
      print_constant(f, c->u.constant);
      break;
    case c_closure:
      print_function(f, c->u.closure);
      break;
    case c_block:
      print_block(f, c->u.block);
      break;
    case c_labeled:
      fprintf(f, "<%s>", c->u.labeled.name);
      print_component(f, c->u.labeled.expression);
      break;
    case c_exit:
      if (c->u.labeled.name) fprintf(f, "exit(<%s>,", c->u.labeled.name);
      else fprintf(f, "exit(");
      print_component(f, c->u.labeled.expression);
      fprintf(f, ")");
      break;
    default: assert(0);
    }
}

void print_file(FILE *out, file f)
{
  static const char *fnames[] = { "", "module", "library" };

  fputs(fnames[f->class], out);
  if (f->name) fprintf(out, " %s\n", f->name);
  if (f->imports)
    {
      fprintf(out, "imports "); 
      print_vlist(out, f->imports);
      fprintf(out, "\n");
    }
  if (f->defines)
    {
      fprintf(out, "defines "); 
      print_vlist(out, f->defines);
      fprintf(out, "\n");
    }
  if (f->reads)
    {
      fprintf(out, "reads "); 
      print_vlist(out, f->reads);
      fprintf(out, "\n");
    }
  if (f->writes)
    {
      fprintf(out, "writes "); 
      print_vlist(out, f->writes);
      fprintf(out, "\n");
    }
  print_component(out, new_component(c_block, f->body));
}

#endif

