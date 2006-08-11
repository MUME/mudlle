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

#include <stdarg.h>
#include <string.h>
#include "mudlle.h"
#include "tree.h"
#include "compile.h"
#include "error.h"
#include "utils.h"
#include "env.h"
#include "charset.h"

mfile new_file(block_t heap, enum file_class vclass, const char *name,
	       vlist imports, vlist defines, vlist reads, vlist writes,
	       block body, int lineno)
{
  mfile newp = allocate(heap, sizeof *newp);

  newp->vclass = vclass;
  newp->name = name;
  newp->imports = imports;
  newp->defines = defines;
  newp->reads = reads;
  newp->writes = writes;
  newp->body = body;
  newp->lineno = lineno;

  return newp;
}

function new_function(block_t heap, mtype type, str_and_len_t help, vlist args,
		      component avalue, int lineno, const char *filename)
{
  function newp = allocate(heap, sizeof *newp);

  newp->type = type;
  newp->help = help;
  newp->args = args;
  newp->varargs = FALSE;
  newp->value = avalue;
  newp->lineno = lineno;
  newp->filename = filename;
  newp->varname = NULL;

  return newp;
}

function new_vfunction(block_t heap, mtype type, str_and_len_t help,
		       const char *arg, component avalue,
		       int lineno, const char *filename)
{
  function newp = allocate(heap, sizeof *newp);

  newp->type = type;
  newp->help = help;
  /* using type_vector implies a useless type check */
  newp->args = new_vlist(heap, arg, stype_any, NULL);
  newp->varargs = TRUE;
  newp->value = avalue;
  newp->lineno = lineno;
  newp->filename = filename;
  newp->varname = NULL;

  return newp;
}

block new_codeblock(block_t heap, vlist locals, clist sequence, 
		    const char *filename, int lineno)
{
  block newp = allocate(heap, sizeof *newp);

  newp->locals = locals;
  newp->sequence = sequence;
  newp->filename = filename;
  newp->lineno = lineno;

  return newp;
}

clist new_clist(block_t heap, component c, clist next)
{
  clist newp = allocate(heap, sizeof *newp);

  newp->next = next;
  newp->c = c;

  return newp;
}

cstlist new_cstlist(block_t heap, constant cst, cstlist next)
{
  cstlist newp = allocate(heap, sizeof *newp);

  newp->next = next;
  newp->cst = cst;

  return newp;
}

vlist new_vlist(block_t heap, const char *var, mtype type, vlist next)
{
  vlist newp = allocate(heap, sizeof *newp);

  newp->next = next;
  newp->var = var;
  newp->type = type;
  newp->was_read = newp->was_written = 0;

  return newp;
}

cstpair new_cstpair(block_t heap, constant cst1, constant cst2)
{
  cstpair newp = allocate(heap, sizeof *newp);

  newp->cst1 = cst1;
  newp->cst2 = cst2;
  
  return newp;
}

constant new_constant(block_t heap, enum constant_class vclass, ...)
{
  va_list args;
  constant newp = allocate(heap, sizeof *newp);

  newp->vclass = vclass;
  va_start(args, vclass);
  switch (vclass)
    {
    case cst_int:
      newp->u.integer = va_arg(args, int);
      break;
    case cst_string:
      newp->u.string = va_arg(args, str_and_len_t);
      break;
    case cst_list: case cst_array: case cst_table:
      newp->u.constants = va_arg(args, cstlist);
      break;
    case cst_float:
      newp->u.mudlle_float = va_arg(args, double);
      break;
    case cst_bigint:
      newp->u.bigint_str = va_arg(args, const char *);
      break;
    case cst_symbol:
      newp->u.constpair = va_arg(args, cstpair);
      break;
    default: assert(0);
    }
  va_end(args);
  return newp;
}

str_and_len_t *cstlist_has_symbol(cstlist list, str_and_len_t needle)
{
  while (list)
    {
      constant car = list->cst, str;
      assert(car->vclass == cst_symbol);
      str = car->u.constpair->cst1;
      assert(str->vclass == cst_string);
      if (needle.len == str->u.string.len
          && mem8icmp(needle.str, str->u.string.str, needle.len) == 0)
	return &str->u.string;
      list = list->next;
    }
  return NULL;
}
  
static clist make_clist(block_t heap, va_list args)
{
  int count;
  clist first = NULL, *scan = &first;

  for (count = va_arg(args, int); count > 0; count--)
    {
      *scan = new_clist(heap, va_arg(args, component), NULL);
      scan = &(*scan)->next;
    }
  return first;
}

component new_component(block_t heap, enum component_class vclass, ...)
{
  va_list args;
  component newp = allocate(heap, sizeof *newp);

  newp->vclass = vclass;
  newp->lineno = 0;

  va_start(args, vclass);
  switch (vclass)
    {
    case c_assign: 
      newp->u.assign.symbol = va_arg(args, const char *);
      newp->u.assign.value = va_arg(args, component);
      break;
    case c_recall:
      newp->u.recall = va_arg(args, const char *);
      break;
    case c_constant:
      newp->u.cst = va_arg(args, constant);
      break;
    case c_closure:
      newp->u.closure = va_arg(args, function);
      break;
    case c_block:
      newp->u.blk = va_arg(args, block);
      break;
    case c_execute:
      newp->u.execute = va_arg(args, clist);
      break;
    case c_builtin:
      newp->u.builtin.fn = va_arg(args, unsigned int);
      newp->u.builtin.args = make_clist(heap, args);
      break;
    case c_labeled: case c_exit:
      newp->u.labeled.name = va_arg(args, const char *);
      newp->u.labeled.expression = va_arg(args, component);
      break;
    default: assert(0);
    }
  va_end(args);
  return newp;
}

pattern new_pattern_constant(block_t heap, constant c)
{
  pattern ap = allocate(heap, sizeof *ap);
  ap->vclass = pat_const;
  ap->lineno = 0;
  ap->u.constval = c;
  return ap;
}

pattern new_pattern_expression(block_t heap, component c)
{
  pattern ap = allocate(heap, sizeof *ap);
  ap->vclass = pat_expr;
  ap->lineno = c->lineno;
  ap->u.expr = c;
  return ap;
}

pattern new_pattern_sink(block_t heap)
{
  pattern ap = allocate(heap, sizeof *ap);
  ap->lineno = 0;
  ap->vclass = pat_sink;
  return ap;
}

pattern new_pattern_symbol(block_t heap, const char *sym, mtype type)
{
  pattern ap = allocate(heap, sizeof *ap);
  ap->vclass = pat_symbol;
  ap->lineno = 0;
  ap->u.sym.name = sym;
  ap->u.sym.type = type;
  return ap;
}

pattern new_pattern_compound(block_t heap, 
			     enum pattern_class class,
			     patternlist list,
			     int ellipsis)
{
  pattern ap = allocate(heap, sizeof *ap);
  ap->vclass = class;
  ap->lineno = 0;
  ap->u.l.patlist = list;
  ap->u.l.ellipsis = ellipsis;
  return ap;
}

patternlist new_pattern_list(block_t heap, 
			     pattern pat,
			     patternlist tail)
{
  patternlist apl = allocate(heap, sizeof *apl);
  apl->next = tail;
  apl->pat = pat;
  return apl;
}

matchnodelist new_match_list(block_t heap, matchnode node, matchnodelist tail)
{
  matchnodelist ml = allocate(heap, sizeof *ml);
  ml->next = tail;
  ml->match = node;
  return ml;
}

matchnode new_match_node(block_t heap, pattern pat, component cond, 
			 component e)
{
  matchnode nd = allocate(heap, sizeof *nd);
  nd->pattern = pat;
  nd->expression = e;
  nd->condition = cond;
  return nd;
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
#define B 2
  struct vector *mc;
  static const char msize[] = { 2, 1, 1, 7, 1, 2, 2, 2, 2 };
  struct gcpro gcpro1;
  struct string *sym;
  value val;
  function f;

  mc = alloc_vector(msize[c->vclass] + B);
  mc->data[0] = makeint(c->vclass);
  mc->data[1] = makeint(c->lineno);
  GCPRO1(mc);

  switch (c->vclass)
    {
    case c_assign:
      sym = alloc_string(c->u.assign.symbol);
      mc->data[B + 0] = sym;
      val = mudlle_parse_component(c->u.assign.value);
      mc->data[B + 1] = val;
      break;

    case c_recall:
      sym = alloc_string(c->u.recall);
      mc->data[B + 0] = sym;
      break;

    case c_constant:
      val = make_constant(c->u.cst);
      mc->data[B + 0] = val;
      break;

    case c_closure:
      f = c->u.closure;
      mc->data[B + 0] = makeint(f->type);
      val = f->help.len ? alloc_string_length(f->help.str, f->help.len) : NULL;
      mc->data[B + 1] = val;
      val = mudlle_vlist(reverse_vlist(f->args));
      mc->data[B + 2] = val;
      mc->data[B + 3] = makeint(f->varargs);
      val = mudlle_parse_component(f->value);
      mc->data[B + 4] = val;
      mc->data[B + 5] = makeint(f->lineno);
      val = make_filename(f->filename);
      mc->data[B + 6] = val;
      break;

    case c_execute:
      val = mudlle_clist(c->u.execute);
      mc->data[B + 0] = val;
      break;

    case c_builtin:
      mc->data[B + 0] = makeint(c->u.builtin.fn);
      val = mudlle_clist(c->u.builtin.args);
      mc->data[B + 1] = val;
      break;

    case c_block:
      val = mudlle_vlist(c->u.blk->locals);
      mc->data[B + 0] = val;
      val = mudlle_clist(c->u.blk->sequence);
      mc->data[B + 1] = val;
      break;

    case c_labeled: case c_exit:
      if (c->u.labeled.name) val = alloc_string(c->u.labeled.name);
      else val = NULL;
      mc->data[B + 0] = val;
      val = mudlle_parse_component(c->u.labeled.expression);
      mc->data[B + 1] = val;
      break;
      
    default:
      assert(0);
    }

  UNGCPRO();
  return mc;
}

value mudlle_parse(block_t heap, mfile f)
{
  struct vector *file = alloc_vector(8);
  struct gcpro gcpro1;

  GCPRO1(file);

  component cbody = new_component(heap, c_block, f->body);
  cbody->lineno = f->lineno;

  SET_VECTOR(file, 0, makeint(f->vclass));
  SET_VECTOR(file, 1, f->name ? alloc_string(f->name) : makebool(FALSE));
  SET_VECTOR(file, 2, mudlle_vlist(f->imports));
  SET_VECTOR(file, 3, mudlle_vlist(f->defines));
  SET_VECTOR(file, 4, mudlle_vlist(f->reads));
  SET_VECTOR(file, 5, mudlle_vlist(f->writes));
  SET_VECTOR(file, 6, mudlle_parse_component(cbody));
  SET_VECTOR(file, 7, alloc_string(f->body->filename));
  UNGCPRO();

  return file;
}

#ifdef PRINT_CODE
static void print_constant(FILE *f, constant c);

static void print_list(FILE *f, cstlist l, int has_tail)
{
  int first = TRUE;

  while (l)
    {
      if (!first) fprintf(f, " ");
      print_constant(f, l->cst);
      if (first) fprintf(f, " .");
      first = FALSE;
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
  switch (c->vclass)
    {
    case cst_int:
      fprintf(f, "%d", c->u.integer);
      break;
    case cst_string:
      fprintf(f, "\"%s\"" , c->u.string.str);
      break;
    case cst_float:
      fprintf(f, "%f", c->u.mudlle_float);
      break;
    case cst_list:
      fprintf(f, "(");
      print_list(f, c->u.constants, 1);
      fprintf(f, ")");
      break;
    case cst_array:
      fprintf(f, "#(");
      print_list(f, c->u.constants, 0);
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
  if (fn->help.len) fprintf(f, "fn \"%s\" (", fn->help.str);
  else fprintf(f, "fn (");
  print_vlist(f, fn->args);
  fprintf(f, ") ");
  print_component(f, fn->value);
}

static void print_component(FILE *f, component c)
{
  switch (c->vclass)
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
      print_constant(f, c->u.cst);
      break;
    case c_closure:
      print_function(f, c->u.closure);
      break;
    case c_block:
      print_block(f, c->u.blk);
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

void print_file(FILE *out, mfile f)
{
  static const char *const fnames[] = { "", "module", "library" };

  fputs(fnames[f->vclass], out);
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
  {
    block_t oops = new_block();

    print_component(out, new_component(oops, c_block, f->body));
    free_block(oops);
  }
}

#endif

static char *heap_allocate_string(block_t heap, const char *s)
{
  char *r = allocate(heap, strlen(s) + 1);
  strcpy(r, s);
  return r;
}

static block_t build_heap;

static int apc_symcount;
static vlist apc_symbols;

static clist build_clist(int n, ...)
{
  va_list args;
  clist res = NULL;

  va_start(args, n);
  while (n-- > 0)
    res = new_clist(build_heap, va_arg(args, component), res);
  va_end(args);

  return reverse_clist(res);
}

static component build_int_component(long n)
{
  return new_component(build_heap, c_constant, 
		       new_constant(build_heap, cst_int, n));
}

static component build_string_component(const char *s) UNUSED;

static component build_string_component(const char *s)
{
  str_and_len_t sl;
  sl.str = (char *)s;
  sl.len = strlen(s);
  return new_component(build_heap, c_constant,
		       new_constant(build_heap, cst_string, sl));
}

static component build_assign(const char *var, component val)
{
  return new_component(build_heap, c_assign, var, val);
}

static component build_recall(const char *var)
{
  return new_component(build_heap, c_recall, var);
}

static component build_exec(component f, int n, ...)
{
  va_list args;
  clist res = new_clist(build_heap, f, NULL);

  va_start(args, n);
  while (n--)
    res = new_clist(build_heap, va_arg(args, component), res);
  va_end(args);

  return new_component(build_heap, c_execute, reverse_clist(res));
}

static vlist build_vlist(int n, ...)
{
  va_list args;
  vlist res = NULL;

  va_start(args, n);
  while (n--)
    {
      const char *s = va_arg(args, const char *);
      mtype type = va_arg(args, mtype);
      res = new_vlist(build_heap, s, type, res);
    }

  va_end(args);
  return res;
}

static component build_logic_and(component e1, component e2)
{
  if (e2 == NULL || e2 == component_true)
    return e1;
  else if (e1 == NULL || e1 == component_true)
    return e2;
  else
    return new_component(build_heap, c_builtin, b_sc_and, 2, e1, e2);
}

static component build_binop(int op, component e1, component e2)
{
  return new_component(build_heap, c_builtin, op, 2, e1, e2);
}

static component build_unop(int op, component e)
{
  return new_component(build_heap, c_builtin, op, 1, e);
}

static component build_codeblock(vlist vl, clist code)
{
  return new_component(build_heap, c_block, 
		       new_codeblock(build_heap, vl, code, NULL, -1));
}

static component build_reference(component x, component idx) UNUSED;

static component build_reference(component x, component idx)
{
  return new_component(build_heap, c_builtin, b_ref, 2, x, idx);
}

static component build_const_comparison(constant cst, component e)
{
  switch (cst->vclass) {
  case cst_list:
    /* we only want NULLs here - fallthrough */
    assert(cst->u.constants == NULL);
  case cst_int:
    return build_binop(b_eq, e,
		       new_component(build_heap, c_constant, cst));
  default:
    return build_exec(build_recall("=>"), 2, 
		      new_component(build_heap, c_constant, cst), e);
  }
}

static component build_typecheck(component e, mtype type)
{
  const char *f;

  switch (type) {
  case type_integer:   f = ":integer?";   break;
  case type_string:    f = ":string?";    break;
  case type_vector:    f = ":vector?";    break;
  case type_pair:      f = ":pair?";      break;
  case type_symbol:    f = ":symbol?";    break;
  case type_table:     f = ":table?";     break;
  case type_object:    f = ":object?";    break;
  case type_character: f = ":character?"; break;
  case type_gone:      f = ":gone?";      break;
  case type_float:     f = ":float?";     break;
  case type_bigint:    f = ":bigint?";    break;
  case type_null: 
    return build_binop(b_eq, e,
		       new_component(build_heap, c_constant, 
				     new_constant(build_heap, cst_list, NULL)));
  case stype_none:
    return component_false;
  case stype_any:
    return component_true;
  default:
    assert(0);
  }
  return build_exec(build_recall(f), 1, e);
}

static component build_match_block(pattern pat, component e,
				   int level)
{
  component res;

  switch (pat->vclass) {
  case pat_sink:
    return component_true;
  case pat_symbol:
    {
      vlist sym;
      
      for (sym = apc_symbols; sym; sym = sym->next)
	if (strcasecmp(sym->var, pat->u.sym.name) == 0)
	  {
	    log_error("repeated variable name in match pattern (%s)", 
		      pat->u.sym.name);
	    return NULL;
	  }

      apc_symbols = new_vlist(build_heap, pat->u.sym.name, stype_any, 
			      apc_symbols);

      res = build_codeblock
	(NULL,
	 build_clist(2,
		     build_assign(pat->u.sym.name, e),
		     build_typecheck(build_recall(pat->u.sym.name),
				     pat->u.sym.type)));
      break;
    }
  case pat_const:
    res = build_const_comparison(pat->u.constval, e);
    break;
  case pat_array:
    {
      /*
       *  [
       *    | tmp |
       *    tmp = <expression>;
       *    (vector?(tmp) &&
       *     vector_length(tmp) == vector_length(<pattern>) &&
       *     tmp[0] == <pattern>[0] &&
       *          :
       *     tmp[N] == <pattern>[N])
       *  ]
       */

      clist code;
      component check = NULL;
      int vlen, n;
      patternlist apl = pat->u.l.patlist;

      char buf[16], *tmpname;
      
      sprintf(buf, "$%d", level);
      tmpname = heap_allocate_string(build_heap, buf);

      code = build_clist(1, build_assign(tmpname, e));

      for (vlen = 0, apl = pat->u.l.patlist; apl; apl = apl->next)
	++vlen;

      for (n = vlen, apl = pat->u.l.patlist; apl; apl = apl->next)
	{
	  component c;

	  --n;
	  c = build_match_block
	    (apl->pat, 
	     new_component(build_heap, c_builtin, b_ref, 2,
			   build_recall(tmpname),
			   build_int_component(n)),
	     level + 1);

          check = build_logic_and(c, check);
	}
      
      if (!(pat->u.l.ellipsis && vlen == 0))
	check = build_logic_and
	  (build_binop
	   (pat->u.l.ellipsis ? b_ge : b_eq,
	    build_exec(build_recall(GLOBAL_ENV_PREFIX "vector_length"), 1,
		       build_recall(tmpname)),
	    build_int_component(vlen)),
	   check);

      check = build_logic_and
	(build_exec(build_recall(GLOBAL_ENV_PREFIX "vector?"), 1,
		    build_recall(tmpname)),
	 check);

      code = new_clist(build_heap, check, code);

      res = build_codeblock(build_vlist(1, tmpname, stype_any),
			    reverse_clist(code));
      break;
    }
  case pat_list:
    {
      patternlist apl = pat->u.l.patlist;
      component check, getcdr;
      clist code;
      char buf[16], *tmpname;
      int first = TRUE;

      if (pat->u.l.patlist == NULL)
	{
	  res = build_const_comparison(new_constant(build_heap, cst_list, NULL), e);
	  break;
	}

      /*
       *  [
       *    | tmp |
       *    tmp = <expression>;
       *    pair?(tmp) &&
       *    car(tmp) == car(<pattern>) &&
       *      [
       *        tmp = cdr(tmp); <pattern> = cdr(<pattern>);
       *        pair?(tmp) &&
       *        car(tmp) == car(<pattern>) &&
       *          :
       *          [                              \  this is done at
       *            cdr(tmp) == cdr(<pattern>);  +- the last pair
       *          ]                              /
       *      ]
       *  ]
       */
      sprintf(buf, "$%d", level);
      tmpname = heap_allocate_string(build_heap, buf);
      
      code = build_clist(1, build_assign(tmpname, e));
      
      /* $tmp = cdr($tmp) */
      getcdr = build_exec(build_recall(GLOBAL_ENV_PREFIX "cdr"), 1, 
			  build_recall(tmpname));
      getcdr->lineno = pat->lineno;
      
      /* this will go last: compare the tail */
      if (apl->pat == NULL)
	check = build_const_comparison
	  (new_constant(build_heap, cst_list, NULL), getcdr);
      else
	check = build_match_block(apl->pat, getcdr, level + 1);
      
      for (apl = apl->next; apl; apl = apl->next)
	{
	  component c = build_logic_and
            (build_exec(build_recall(GLOBAL_ENV_PREFIX "pair?"), 1, 
                        build_recall(tmpname)),
             build_match_block
             (apl->pat, 
              build_exec(build_recall(GLOBAL_ENV_PREFIX "car"), 1, 
                         build_recall(tmpname)),
	      level + 1));
	  
	  if (first)
	    first = FALSE;
	  else
	    {
	      component movecdr;
	      getcdr = build_exec(build_recall(GLOBAL_ENV_PREFIX "cdr"), 1, 
				  build_recall(tmpname));
	      movecdr = build_assign(tmpname, getcdr);
	      check = build_codeblock(NULL, build_clist(2, movecdr, check));
	    }
	  check = build_logic_and(c, check);
	  check->lineno = apl->pat->lineno;
	}
      
      code = new_clist(build_heap, check, code);
      
      res = build_codeblock(build_vlist(1, tmpname, stype_any),
			    reverse_clist(code));
      break;
    }
  case pat_expr:
    res = build_exec(build_recall("=>"), 2, pat->u.expr, e);
    break;
  default:
    assert(0);
  }
  res->lineno = pat->lineno;
  return res;
}

static component build_error(int error)
{
  return build_exec(build_recall(GLOBAL_ENV_PREFIX "error"), 1, 
		    build_int_component(error));
}

component new_pattern_component(block_t heap, pattern pat, component e)
{
  component res;

  build_heap = heap;
  apc_symcount = 0;
  apc_symbols = NULL;

  /* 
   *  Warning: if the match fails, this might leave only some of the variables 
   *  in the pattern filled. But it's a feature, right?
   */
  res = new_component(build_heap, c_builtin, b_if, 2,
		      new_component(build_heap, c_builtin, b_not, 1,
                                    build_match_block(pat, e, 0)),
		      build_error(error_no_match));
  res->lineno = pat->lineno;
  return res;
}

component new_for_component(block_t heap, vlist vars,
                            component einit,
                            component eexit,
                            component eloop,
                            component e)
{
  /*
   *    <break> [
   *      <vars>
   *      <einit>
   *      loop
   *        [
   *          if (!<eexit) exit<break> 42;
   *          <continue> <e>;
   *          <eloop>;
   *        ]
   *    ]
   */
  clist code = NULL;
  component c;

  build_heap = heap;

  if (eloop)
    code = new_clist(build_heap, eloop, code);

  code = new_clist(build_heap,
                   new_component(build_heap, c_labeled, "continue", e),
                   code);

  if (eexit)
    {
      c = new_component(build_heap, c_builtin, b_if, 2,
                        new_component(build_heap, c_builtin, b_not, 1, eexit),
                        new_component(build_heap, c_exit,
                                      "break",
                                      component_undefined));
      code = new_clist(build_heap, c, code);
    }

  c = build_codeblock(NULL, code);
  c = new_component(build_heap, c_builtin, b_loop, 1, c);

  code = new_clist(build_heap, c, NULL);
  if (einit)
    code = new_clist(build_heap, einit, code);

  return new_component(build_heap, c_labeled, "break",
                       build_codeblock(vars, code));
}

component new_match_component(block_t heap, component e, matchnodelist matches)
{
  vlist vl;
  clist code;
  build_heap = heap;

  /*
   *   <$match> [
   *     | $exp |
   *     
   *     $exp = <match-expression>
   *     [                                       \
   *       | <pattern-variables> |                |
   *       if (<pattern-match> [&& <condition>])  +- repeat for each match node
   *         exit<$match> <pattern-expression>;   |
   *     ]                                       /
   *     false
   *   ]
   */

  vl = build_vlist(1, "$exp", stype_any);

  code = build_clist(1, component_false);
  for (; matches; matches = matches->next)
    {
      component matchcode;

      apc_symcount = 0;
      apc_symbols = NULL;
      matchcode = build_match_block(matches->match->pattern,
				    build_recall("$exp"), 0);
      matchcode = build_logic_and(matchcode, matches->match->condition);
      matchcode = new_component(build_heap, c_builtin, b_if, 2,
				matchcode,
				new_component(build_heap, c_exit, "$match",
					      matches->match->expression));

      code = new_clist(build_heap,
		       build_codeblock(apc_symbols, build_clist(1, matchcode)),
		       code);
    }

  code = new_clist(build_heap, build_assign("$exp", e), code);

  return new_component(build_heap, c_labeled, "$match", 
		       build_codeblock(vl, code));
}

component new_xor_component(block_t heap, component e0, component e1)
{
  vlist vl = new_vlist(heap, "$xor", stype_any, NULL);
  clist cl;
  build_heap = heap;

  /* 
   *  [
   *    | $xor |
   *    $xor = !<exp1>;
   *    if (<exp0>) $xor else !$xor;
   *  ]
   */

  cl = new_clist(heap,
		 new_component(heap, c_assign, "$xor",
			       build_unop(b_not, e1)),
		 NULL);
  cl = new_clist(heap,
		 new_component(heap, c_builtin,
			       b_ifelse, 3, e0,
			       build_recall("$xor"),
			       build_unop(b_not, build_recall("$xor"))),
		 cl);
    
  return new_component(heap, c_block,
		       new_codeblock(heap,
				     vl,
				     reverse_clist(cl),
				     NULL, -1));
}

component new_postfix_inc_component(block_t heap, const char *var, int op)
{
  vlist vl = new_vlist(heap, "$tmp", stype_any, NULL);
  clist cl;
  build_heap = heap;

  /* 
   *  [
   *    | $tmp |
   *    $tmp = <var>;
   *    <var> = $tmp + 1;
   *    $tmp;
   *  ]
   */

  cl = new_clist(heap,
		 new_component(heap, c_assign, "$tmp",
			       build_recall(var)),
		 NULL);
  cl = new_clist(heap,
		 new_component(heap, c_assign, 
			       var, 
			       build_binop(op,
					   build_recall("$tmp"),
					   new_component(heap, c_constant,
							 new_constant(heap, cst_int, 1)))),
		 cl);
  cl = new_clist(heap,
		 build_recall("$tmp"),
		 cl);
  return new_component(heap, c_block,
		       new_codeblock(heap,
				     vl,
				     reverse_clist(cl),
				     NULL, -1));
}
