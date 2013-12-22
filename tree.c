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

#include <stdarg.h>
#include <string.h>

#include "alloc.h"
#include "charset.h"
#include "compile.h"
#include "env.h"
#include "error.h"
#include "tree.h"
#include "utils.h"

#define GEP GLOBAL_ENV_PREFIX

const char *const builtin_op_names[] = {
  "sc_or", "sc_and", "eq", "ne", "lt", "le", "gt", "ge",
  "bitor", "bitxor", "bitand", "shift_left", "shift_right",
  "add", "subtract", "multiply", "divide", "remainder", "negate",
  "not", "bitnot", "ifelse", "if", "while", "loop", "ref", "set",
  "cons", NULL,
  /* these are only used by the parser and compiler */
  "xor"
};
CASSERT_VLEN(builtin_op_names, very_last_builtin);

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

function new_function(block_t heap, unsigned typeset, str_and_len_t help,
                      vlist args, component avalue, int lineno,
                      const char *filename, const char *nicename)
{
  function newp = allocate(heap, sizeof *newp);

  newp->typeset = typeset;
  newp->help = help;
  newp->args = args;
  newp->varargs = false;
  newp->value = avalue;
  newp->lineno = lineno;
  newp->filename = filename;
  newp->nicename = nicename;
  newp->varname = NULL;

  return newp;
}

function new_vfunction(block_t heap, unsigned typeset, str_and_len_t help,
		       const char *arg, component avalue,
		       int lineno, const char *filename,
                       const char *nicename)
{
  function newp = allocate(heap, sizeof *newp);

  newp->typeset = typeset;
  newp->help = help;
  /* using type_vector implies a useless type check */
  newp->args = new_vlist(heap, arg, TYPESET_ANY, lineno, NULL);
  newp->varargs = true;
  newp->value = avalue;
  newp->lineno = lineno;
  newp->filename = filename;
  newp->nicename = nicename;
  newp->varname = NULL;

  return newp;
}

block new_codeblock(block_t heap, vlist locals, clist sequence,
		    const char *filename, const char *nicename, int lineno)
{
  block newp = allocate(heap, sizeof *newp);

  newp->locals = locals;
  newp->sequence = sequence;
  newp->filename = filename;
  newp->nicename = nicename;
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

vlist new_vlist(block_t heap, const char *var, unsigned typeset, int lineno,
                vlist next)
{
  vlist newp = allocate(heap, sizeof *newp);

  newp->next = next;
  newp->var = var;
  newp->typeset = typeset;
  newp->was_read = newp->was_written = false;
  newp->lineno = lineno;

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

str_and_len_t *cstlist_find_symbol(cstlist list, str_and_len_t needle)
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

static clist make_clist(block_t heap, int count, va_list args)
{
  clist first = NULL, *scan = &first;

  while (count-- > 0)
    {
      *scan = new_clist(heap, va_arg(args, component), NULL);
      scan = &(*scan)->next;
    }
  return first;
}

component new_component(block_t heap, int lineno,
                        enum component_class vclass, ...)
{
  va_list args;
  component newp = allocate(heap, sizeof *newp);

  newp->vclass = vclass;
  newp->lineno = lineno;

  va_start(args, vclass);
  switch (vclass)
    {
    case c_assign:
      newp->u.assign.symbol = va_arg(args, const char *);
      newp->u.assign.value = va_arg(args, component);
      break;
    case c_vref:
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
      newp->u.builtin.fn = va_arg(args, enum builtin_op);
      int nargs = va_arg(args, int);
      newp->u.builtin.args = make_clist(heap, nargs, args);
      break;
    case c_labeled: case c_exit:
      newp->u.labeled.name = va_arg(args, const char *);
      newp->u.labeled.expression = va_arg(args, component);
      break;
    default: abort();
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

pattern new_pattern_symbol(block_t heap, const char *sym, mtype type,
                           int lineno)
{
  pattern ap = allocate(heap, sizeof *ap);
  ap->vclass = pat_symbol;
  ap->lineno = lineno;
  ap->u.sym.name = sym;
  ap->u.sym.type = type;
  return ap;
}

pattern new_pattern_compound(block_t heap,
			     enum pattern_class class,
			     patternlist list,
			     bool ellipsis)
{
  assert(class == pat_list || class == pat_array);

  if (ellipsis)
    goto not_const;

  for (patternlist p = list; p != NULL; p = p->next)
    if (p->pat && p->pat->vclass != pat_const)
      goto not_const;

  cstlist cl = NULL;
  for (patternlist p = list; p != NULL; p = p->next)
    cl = new_cstlist(heap, p->pat ? p->pat->u.constval : NULL, cl);
  cl = reverse_cstlist(cl);
  return new_pattern_constant(
    heap,
    new_constant(heap, class == pat_list ? cst_list : cst_array, cl));

 not_const: ;
  pattern ap = allocate(heap, sizeof *ap);
  ap->lineno = 0;
  ap->vclass = class;
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
			 component e, const char *filename, int lineno)
{
  matchnode nd = allocate(heap, sizeof *nd);
  nd->pattern = pat;
  nd->expression = e;
  nd->condition = cond;
  nd->filename = filename;
  nd->lineno = lineno;
  return nd;
}

static inline vlist reverse_vlist(vlist l)
{
  return reverse_list(l, struct _vlist);
}

/* Make a mudlle rep of a parse tree */
static value mudlle_parse_component(component c);

static value mudlle_vlist(vlist vars)
{
  value l = NULL;
  struct string *s = NULL;

  GCPRO2(l, s);
  for (vars = reverse_vlist(vars); vars; vars = vars->next)
    {
      s = alloc_string(vars->var);
      struct vector *v = alloc_vector(3);
      v->data[0] = s;
      v->data[1] = makeint(vars->typeset);
      v->data[2] = makeint(vars->lineno);
      l = alloc_list(v, l);
    }
  UNGCPRO();
  return l;
}

static value mudlle_clist(clist exprs)
{
  value l = NULL;
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
  static const char msize[] = { 2, 1, 1, 8, 1, 2, 2, 2, 2, 1 };
  CASSERT_VLEN(msize, component_classes);

  struct vector *mc = alloc_vector(msize[c->vclass] + 2);
  mc->data[0] = makeint(c->vclass);
  mc->data[1] = makeint(c->lineno);

#define SET(n, value) SET_VECTOR(mc, 2 + (n), (value))

  GCPRO1(mc);

  switch (c->vclass)
    {
    case c_assign:
      SET(0, alloc_string(c->u.assign.symbol));
      SET(1, mudlle_parse_component(c->u.assign.value));
      break;

    case c_vref:
    case c_recall:
      SET(0, alloc_string(c->u.recall));
      break;

    case c_constant:
      SET(0, make_constant(c->u.cst));
      break;

    case c_closure: {
      function f = c->u.closure;
      SET(0, makeint(f->typeset));
      SET(1, (f->help.len
              ? alloc_string_length(f->help.str, f->help.len)
              : NULL));
      SET(2, mudlle_vlist(reverse_vlist(f->args)));
      SET(3, makeint(f->varargs));
      SET(4, mudlle_parse_component(f->value));
      SET(5, makeint(f->lineno));
      SET(6, make_filename(f->filename));
      SET(7, make_filename(f->nicename));
      break;
    }

    case c_execute:
      SET(0, mudlle_clist(c->u.execute));
      break;

    case c_builtin:
      SET(0, makeint(c->u.builtin.fn));
      SET(1, mudlle_clist(c->u.builtin.args));
      break;

    case c_block:
      SET(0, mudlle_vlist(c->u.blk->locals));
      SET(1, mudlle_clist(c->u.blk->sequence));
      break;

    case c_labeled: case c_exit:
      SET(0, (c->u.labeled.name
              ? alloc_string(c->u.labeled.name)
              : NULL));
      SET(1, mudlle_parse_component(c->u.labeled.expression));
      break;

    default:
      abort();
    }

  UNGCPRO();
  return mc;

#undef SET
}

value mudlle_parse(block_t heap, mfile f)
{
  struct vector *file = alloc_vector(9);
  GCPRO1(file);

  component cbody = new_component(heap, f->lineno, c_block, f->body);

  SET_VECTOR(file, 0, makeint(f->vclass));
  SET_VECTOR(file, 1, f->name ? alloc_string(f->name) : makebool(false));
  SET_VECTOR(file, 2, mudlle_vlist(f->imports));
  SET_VECTOR(file, 3, mudlle_vlist(f->defines));
  SET_VECTOR(file, 4, mudlle_vlist(f->reads));
  SET_VECTOR(file, 5, mudlle_vlist(f->writes));
  SET_VECTOR(file, 6, mudlle_parse_component(cbody));
  SET_VECTOR(file, 7, alloc_string(f->body->filename));
  SET_VECTOR(file, 8, alloc_string(f->body->nicename));
  UNGCPRO();

  return file;
}

#ifdef PRINT_CODE
static void print_constant(FILE *f, constant c);

static void print_list(FILE *f, cstlist l, int has_tail)
{
  bool first = true;

  while (l)
    {
      if (!first) fprintf(f, " ");
      if (l->cst == NULL)
        fprintf(f, "<0>");
      else
        print_constant(f, l->cst);
      if (first) fprintf(f, " .");
      first = false;
      l = l->next;
    }
}

static void print_vlist(FILE *f, vlist l)
{
  int first = true;

  while (l)
    {
      if (!first) fprintf(f, ", ");
      first = false;
      if (l->typeset != TYPESET_ANY) fprintf(f, "%#x ", l->typeset);
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
  const char *prefix = "";
  while (sequence)
    {
      fputs(prefix, f);
      print_component(f, sequence->c);
      sequence = sequence->next;
      prefix = ", ";
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
      fputs(", ", f);
      print_clist(f, c->u.execute->next);
      fprintf(f, ")");
      break;
    case c_builtin:
      fprintf(f, "b_%s(", builtin_op_names[c->u.builtin.fn]);
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

void print_mudlle_file(FILE *out, mfile f)
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

    print_component(out, new_component(oops, f->lineno, c_block, f->body));
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
    {
      component c = va_arg(args, component);
      if (c == NULL)
        continue;
      res = new_clist(build_heap, c, res);
    }
  va_end(args);

  return reverse_clist(res);
}

component new_int_component(block_t heap, long n)
{
  return new_component(heap, 0, c_constant, new_constant(heap, cst_int, n));
}

static component build_int_component(long n)
{
  return new_int_component(build_heap, n);
}

static component build_string_component(const char *s) UNUSED;

static component build_string_component(const char *s)
{
  str_and_len_t sl = {
    .str = (char *)s,
    .len = strlen(s)
  };
  return new_component(build_heap, 0, c_constant,
		       new_constant(build_heap, cst_string, sl));
}

static component build_assign(int lineno, const char *var, component val)
{
  return new_component(build_heap, lineno, c_assign, var, val);
}

static component build_recall(const char *var)
{
  return new_component(build_heap, 0, c_recall, var);
}

static component build_exec(component f, int n, ...)
{
  va_list args;
  clist res = new_clist(build_heap, f, NULL);

  va_start(args, n);
  while (n--)
    res = new_clist(build_heap, va_arg(args, component), res);
  va_end(args);

  return new_component(build_heap, 0, c_execute, reverse_clist(res));
}

/* args are name, typeset, line, name, typeset, line, ... */
static vlist build_vlist(int n, ...)
{
  va_list args;
  vlist res = NULL;

  va_start(args, n);
  while (n--)
    {
      const char *s = va_arg(args, const char *);
      unsigned typeset = va_arg(args, unsigned);
      int lineno = va_arg(args, int);
      if (s == NULL)
        continue;
      res = new_vlist(build_heap, s, typeset, lineno, res);
    }

  va_end(args);
  return res;
}

component new_binop_component(block_t heap, int lineno, enum builtin_op op,
                              component e1, component e2)
{
  if (op == b_xor)
    return new_xor_component(heap, lineno, e1, e2);
  assert(op >= 0 && op < last_builtin);
  return new_component(heap, lineno, c_builtin, op, 2, e1, e2);
}

static component build_if(component cond, component ctrue, component cfalse)
{
  return new_component(build_heap, 0, c_builtin,
                       cfalse ? b_ifelse : b_if,
                       cfalse ? 3 : 2,
                       cond, ctrue, cfalse);
}

static component build_exit(const char *name, component c)
{
  return new_component(build_heap, 0, c_exit, name, c);
}

static component build_binop(enum builtin_op op, component e1, component e2)
{
  return new_binop_component(build_heap, 0, op, e1, e2);
}

static component build_unop(enum builtin_op op, component e)
{
  return new_component(build_heap, 0, c_builtin, op, 1, e);
}

static component build_logic_and(component e1, component e2)
{
  assert(e1 || e2);
  if (e2 == NULL || e2 == component_true)
    return e1;
  if (e1 == NULL || e1 == component_true)
    return e2;
  return build_binop(b_sc_and, e1, e2);
}

static component build_codeblock(vlist vl, clist code)
{
  return new_component(build_heap, 0, c_block,
		       new_codeblock(build_heap, vl, code, NULL, NULL, -1));
}

static component build_const_comparison(constant cst, component e)
{
  switch (cst->vclass) {
  case cst_int:
  simple:
    return build_binop(b_eq, e,
		       new_component(build_heap, 0, c_constant, cst));
  case cst_list:
    if (cst->u.constants == NULL)
      goto simple;
    /* fallthrough */
  default:
    return build_exec(build_recall(GEP "equal?"), 2,
		      new_component(build_heap, 0, c_constant, cst), e);
  }
}

static component build_typecheck(component e, mtype type)
{
  const char *f;

  switch (type) {
  case type_integer:   f = GEP "integer?";   break;
  case type_string:    f = GEP "string?";    break;
  case type_vector:    f = GEP "vector?";    break;
  case type_pair:      f = GEP "pair?";      break;
  case type_symbol:    f = GEP "symbol?";    break;
  case type_table:     f = GEP "table?";     break;
  case type_object:    f = GEP "object?";    break;
  case type_character: f = GEP "character?"; break;
  case type_gone:      f = GEP "gone?";      break;
  case type_float:     f = GEP "float?";     break;
  case type_bigint:    f = GEP "bigint?";    break;
  case type_null:
    return build_binop(b_eq, e,
		       new_component(build_heap, 0, c_constant,
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

/* returns true if this block can fail to match */
static bool build_match_block(pattern pat, component e,
                              int level, component *result)
{
  *result = NULL;
  bool can_fail = true;

  switch (pat->vclass) {
  case pat_sink:
    return false;
  case pat_symbol:
    {
      for (vlist sym = apc_symbols; sym; sym = sym->next)
	if (strcasecmp(sym->var, pat->u.sym.name) == 0)
	  {
	    compile_error("repeated variable name in match pattern (%s)",
                          pat->u.sym.name);
	    return false;
	  }

      apc_symbols = new_vlist(
        build_heap, pat->u.sym.name, TYPESET_ANY, pat->lineno, apc_symbols);

      component ca = build_assign(0, pat->u.sym.name, e);
      if (pat->u.sym.type == stype_any)
        {
          *result = ca;
          can_fail = false;
        }
      else
        *result = build_codeblock(
          NULL,
          build_clist(2, ca,
                      build_typecheck(build_recall(pat->u.sym.name),
                                      pat->u.sym.type)));
      break;
    }
  case pat_const:
    *result = build_const_comparison(pat->u.constval, e);
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
      int vlen, n;

      char buf[16], *tmpname;

      sprintf(buf, "$%d", level);
      tmpname = heap_allocate_string(build_heap, buf);

      code = build_clist(1, build_assign(0, tmpname, e));

      patternlist apl;
      for (vlen = 0, apl = pat->u.l.patlist; apl; apl = apl->next)
	++vlen;

      struct {
        component c;
        bool can_fail;
      } checks[vlen];

      for (n = vlen, apl = pat->u.l.patlist; apl; apl = apl->next)
	{
	  --n;

	  component c;
	  bool f = build_match_block(
	    apl->pat, build_binop(b_ref,
                                  build_recall(tmpname),
                                  build_int_component(n)),
            level + 1, &c);

          checks[n].c = c;
          checks[n].can_fail = f;
	}

      component check = build_exec(build_recall(GEP "vector?"), 1,
                                   build_recall(tmpname));
      if (!(pat->u.l.ellipsis && vlen == 0))
	check = build_logic_and(
          check,
          build_binop(
            pat->u.l.ellipsis ? b_ge : b_eq,
	    build_exec(build_recall(GEP "vector_length"), 1,
		       build_recall(tmpname)),
	    build_int_component(vlen)));

      bool used_next = false;
      for (;;)
        {
          while (n < vlen && checks[n].can_fail)
            check = build_logic_and(check, checks[n++].c);
          if (n < vlen)
            {
              check = build_if(build_unop(b_not, check),
                               build_exit("next", component_false),
                               NULL);
              used_next = true;
            }
          code = new_clist(build_heap, check, code);
          if (n == vlen)
            break;
          check = NULL;
          while (n < vlen && !checks[n].can_fail)
            {
              component c = checks[n++].c;
              if (c != NULL)
                code = new_clist(build_heap, c, code);
            }
          if (n == vlen)
            {
              code = new_clist(build_heap, component_true, code);
              break;
            }
        }

      component c = build_codeblock(
        build_vlist(1, tmpname, TYPESET_ANY, pat->lineno),
        reverse_clist(code));
      if (used_next)
        c = new_component(build_heap, 0, c_labeled, "next", c);
      *result = c;

      break;
    }
  case pat_list:
    {
      patternlist apl = pat->u.l.patlist;
      component check, getcdr;
      clist code;
      char buf[16], *tmpname;
      int first = true;

      if (pat->u.l.patlist == NULL)
	{
	  *result = build_const_comparison(
            new_constant(build_heap, cst_list, NULL), e);
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

      code = build_clist(1, build_assign(0, tmpname, e));

      /* $tmp = cdr($tmp) */
      getcdr = build_exec(build_recall(GEP "cdr"), 1,
			  build_recall(tmpname));
      getcdr->lineno = pat->lineno;

      bool prev_can_fail = true;
      /* this will go last: compare the tail */
      if (apl->pat == NULL)
	check = build_const_comparison(
          new_constant(build_heap, cst_list, NULL), getcdr);
      else
        prev_can_fail = build_match_block(apl->pat, getcdr,
                                          level + 1, &check);

      for (apl = apl->next; apl; apl = apl->next)
	{
          component cpair = build_exec(
            build_recall(GEP "pair?"), 1,
            build_recall(tmpname));

	  component c;
          if (build_match_block(
                apl->pat,
                build_exec(build_recall(GEP "car"), 1,
                           build_recall(tmpname)),
                level + 1, &c))
            c = build_logic_and(cpair, c);
          else if (c == NULL)
            c = cpair;
          else
            c = build_if(cpair,
                         build_codeblock(
                           NULL, build_clist(2, c, component_true)),
                         component_false);

	  if (first)
	    first = false;
	  else
	    {
	      getcdr = build_exec(build_recall(GEP "cdr"), 1,
				  build_recall(tmpname));
	      component movecdr = build_assign(0, tmpname, getcdr);
	      check = build_codeblock(NULL, build_clist(2, movecdr, check));
	    }
          if (prev_can_fail)
            check = build_logic_and(c, check);
          else if (check == NULL)
            check = c;
          else
            check = build_if(c,
                             build_codeblock(
                               NULL,
                               build_clist(2, check, component_true)),
                             component_false);
          prev_can_fail = true;
	  check->lineno = apl->pat->lineno;
	}

      code = new_clist(build_heap, check, code);

      *result = build_codeblock(
        build_vlist(1, tmpname, TYPESET_ANY, pat->lineno),
        reverse_clist(code));
      break;
    }
  case pat_expr:
    *result = build_exec(build_recall(GEP "equal?"),
                         2, pat->u.expr, e);
    break;
  default:
    assert(0);
  }
  (*result)->lineno = pat->lineno;
  return can_fail;
}

static component build_error(runtime_errors error)
{
  return build_exec(build_recall(GEP "error"), 1,
		    build_int_component(error));
}

component new_pattern_component(block_t heap, pattern pat, component e)
{
  build_heap = heap;
  apc_symcount = 0;
  apc_symbols = NULL;

  /* Warning: if the match fails, this might leave only some of the variables
   * in the pattern filled. But it's a feature, right? */
  component mc;
  build_match_block(pat, e, 0, &mc);
  component res = build_if(build_unop(b_not, mc),
                           build_error(error_no_match), NULL);
  res->lineno = pat->lineno;
  return res;
}

component new_dereference(block_t heap, int lineno, component e)
{
  build_heap = heap;
  component c = build_exec(build_recall(GEP "dereference"),
                           1, e);
  c->lineno = lineno;
  return c;
}

/* return dereferenced value or NULL */
static component is_dereference(component e)
{
  if (e->vclass != c_execute)
    return NULL;
  clist cl = e->u.execute;
  if (cl == NULL || cl->next == NULL || cl->next->next != NULL)
    return NULL;
  component f = cl->c;
  if (f->vclass != c_recall)
    return NULL;
  if (strcmp(f->u.recall, GEP "dereference") != 0)
    return NULL;
  return cl->next->c;
}

component new_reference(block_t heap, int lineno, component e)
{
  build_heap = heap;

  if (e->vclass == c_builtin && e->u.builtin.fn == b_ref)
    {
      clist args = e->u.builtin.args;
      return build_exec(build_recall(GEP "make_ref"), 2,
                        args->c, args->next->c);

    }

  if (e->vclass == c_recall)
    return new_component(heap, lineno, c_vref, e->u.recall);

  if (e->vclass == c_execute)
    {
      clist args = e->u.execute;
      component f = args->c;
      args = args->next;
      if (f->vclass != c_recall
          || args == NULL
          || args->next != NULL)
        goto error;
      const char *name = f->u.recall;
      if (strncasecmp(name, GEP, strlen(GEP)) == 0)
        name += strlen(GEP);

      if (strcasecmp(name, "symbol_get") == 0)
        return build_exec(build_recall(GEP "make_symbol_ref"),
                          1, args->c);
      int idx;
      if (strcasecmp(name, "car") == 0)
        idx = 0;
      else if (strcasecmp(name, "cdr") == 0)
        idx = 1;
      else
        goto error;
      return build_exec(build_recall(GEP "make_pair_ref"),
                        2, args->c, build_int_component(idx));
    }

 error:
  compile_error("cannot create a reference to that");
  return NULL;
}

static component build_set_ref(component e0, component e1)
{
  return build_exec(build_recall(GEP "set_ref!"), 2, e0, e1);
}

component new_for_component(block_t heap, vlist vars,
                            component einit,
                            component eexit,
                            component eloop,
                            component e,
                            const char *filename, int lineno)
{
  /*
   *  <break> [
   *    <vars>
   *    <einit>
   *    loop
   *      [
   *        if (!<eexit>) exit<break> 42;
   *        <continue> <e>;
   *        <eloop>;
   *      ]
   *  ]
   */
  clist code = NULL;

  build_heap = heap;

  if (eloop)
    code = new_clist(build_heap, eloop, code);

  code = new_clist(build_heap,
                   new_component(build_heap, e->lineno, c_labeled,
                                 "continue", e),
                   code);

  if (eexit)
    {
      component c = build_if(build_unop(b_not, eexit),
                             build_exit("break", component_undefined),
                             NULL);
      code = new_clist(build_heap, c, code);
    }

  component c = build_codeblock(NULL, code);
  c = build_unop(b_loop, c);

  code = new_clist(build_heap, c, NULL);
  if (einit)
    code = new_clist(build_heap, einit, code);

  c = build_codeblock(vars, code);
  c->u.blk->filename = filename;
  c->u.blk->lineno = lineno;
  return new_component(build_heap, c->lineno, c_labeled, "break", c);
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

  vl = build_vlist(1, "$exp", TYPESET_ANY, e->lineno);

  code = build_clist(1, component_false);
  for (; matches; matches = matches->next)
    {
      apc_symcount = 0;
      apc_symbols = NULL;
      component matchcode;
      bool can_fail = build_match_block(matches->match->pattern,
                                        build_recall("$exp"), 0,
                                        &matchcode);
      component cexit = build_exit("$match", matches->match->expression);
      if (can_fail)
        matchcode = build_logic_and(matchcode, matches->match->condition);
      else if (matches->match->condition)
        {
          matchcode = build_codeblock(
            NULL, build_clist(2, matchcode, matches->match->condition));
          can_fail = true;
        }

      clist cl;
      if (can_fail)
        cl = build_clist(1, build_if(matchcode, cexit, NULL));
      else
        cl = build_clist(2, matchcode, cexit);

      component cblock = build_codeblock(apc_symbols, cl);
      cblock->u.blk->filename = matches->match->filename;
      cblock->u.blk->lineno = matches->match->lineno;
      code = new_clist(build_heap, cblock, code);
    }

  code = new_clist(build_heap, build_assign(0, "$exp", e), code);

  return new_component(build_heap, 0, c_labeled, "$match",
		       build_codeblock(vl, code));
}

component new_xor_component(block_t heap, int lineno, component e0,
                            component e1)
{
  build_heap = heap;
  return build_binop(b_bitxor, build_unop(b_not, e0), build_unop(b_not, e1));
}

static component comp_id(component e)
{
  return e;
}

static component comp_set_tmp(component e)
{
  return build_assign(0, "$tmp", e);
}

component new_assign_expression(block_t heap, component e0, enum builtin_op op,
                                component e1, bool postfix, int lineno)
{
  build_heap = heap;

  if (op == b_invalid)
    {
      if (e0->vclass == c_builtin && e0->u.builtin.fn == b_ref)
        {
          clist args = e0->u.builtin.args;
          return new_component(heap, lineno, c_builtin, b_set, 3,
                               args->c, args->next->c, e1);
        }
      if (e0->vclass == c_recall)
        return build_assign(lineno, e0->u.recall, e1);
      component r = is_dereference(e0);
      if (r == NULL)
        goto error;
      return build_set_ref(r, e1);
    }

  component (*set_tmp)(component) = postfix ? comp_set_tmp : comp_id;
  component ret = postfix ? build_recall("$tmp") : NULL;
  const char *tmp_name = postfix ? "$tmp" : NULL;

  if (e0->vclass == c_builtin && e0->u.builtin.fn == b_ref)
    {
      clist args = e0->u.builtin.args;
      vlist vl = build_vlist(3,
                             tmp_name, TYPESET_ANY, e0->lineno,
                             "$ref", TYPESET_ANY, args->c->lineno,
                             "$exp", TYPESET_ANY, args->next->c->lineno);
      clist cl = build_clist(
        4,
        build_assign(lineno, "$exp", args->c),
        build_assign(lineno, "$ref", args->next->c),
        new_component(heap, lineno, c_builtin, b_set, 3,
                      build_recall("$exp"),
                      build_recall("$ref"),
                      build_binop(op,
                                  set_tmp(build_binop(b_ref,
                                                      build_recall("$exp"),
                                                      build_recall("$ref"))),
                                  e1)),
        ret);
      return build_codeblock(vl, cl);
    }

  if (e0->vclass == c_recall)
    {
      vlist vl = build_vlist(1, tmp_name, TYPESET_ANY, e0->lineno);
      clist cl = build_clist(
        2,
        build_assign(lineno, e0->u.recall, build_binop(op, set_tmp(e0), e1)),
        ret);
      /* optimize for common case */
      if (vl == NULL && cl->next == NULL)
        return cl->c;
      return build_codeblock(vl, cl);
    }

  component r = is_dereference(e0);
  if (r != NULL)
    {
      vlist vl = build_vlist(2,
                             tmp_name, TYPESET_ANY, e0->lineno,
                             "$ref", TYPESET_ANY, e0->lineno);
      clist cl = build_clist(
        3,
        build_assign(lineno, "$ref", r),
        build_set_ref(
          build_recall("$ref"),
          build_binop(op,
                      set_tmp(new_dereference(heap, lineno,
                                              build_recall("$ref"))),
                      e1)),
        ret);
      return build_codeblock(vl, cl);
    }

 error:
  compile_error("can only assign to variables or references");
  return NULL;
}
