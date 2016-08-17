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
#include "calloc.h"
#include "charset.h"
#include "compile.h"
#include "error.h"
#include "global.h"
#include "mvalues.h"
#include "table.h"
#include "tree.h"
#include "utils.h"

#define GEP GLOBAL_ENV_PREFIX

static struct alloc_block *build_heap;

static struct component *build_exec(struct component *f, int n, ...);
static struct component *build_recall(const char *var);
static struct component *build_constant(struct constant *cst);

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

struct mfile *new_file(struct alloc_block *heap, enum file_class vclass,
                       const char *name,
                       struct vlist *requires, struct vlist *defines,
                       struct vlist *reads, struct vlist *writes,
                       struct vlist *statics, struct block *body, int lineno)
{
  struct mfile *newp = allocate(heap, sizeof *newp);
  *newp = (struct mfile){
    .vclass   = vclass,
    .name     = name,
    .requires = requires,
    .defines  = defines,
    .reads    = reads,
    .writes   = writes,
    .statics  = statics,
    .body     = body,
    .lineno   = lineno
  };

  return newp;
}

struct function *new_function(struct alloc_block *heap, unsigned typeset,
                              struct str_and_len help, struct vlist *args,
                              struct component *avalue, int lineno,
                              const char *filename, const char *nicename)
{
  struct function *newp = allocate(heap, sizeof *newp);

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

struct function *new_vfunction(struct alloc_block *heap, unsigned typeset,
                               struct str_and_len help, const char *arg,
                               struct component *avalue,
                               int lineno, const char *filename,
                               const char *nicename)
{
  struct function *newp = allocate(heap, sizeof *newp);

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

struct block *new_codeblock(struct alloc_block *heap, struct vlist *locals,
                            struct clist *sequence, const char *filename,
                            const char *nicename, int lineno)
{
  struct block *newp = allocate(heap, sizeof *newp);

  newp->locals = locals;
  newp->sequence = sequence;
  newp->filename = filename;
  newp->nicename = nicename;
  newp->lineno = lineno;
  newp->statics = false;

  return newp;
}

struct clist *new_clist(struct alloc_block *heap, struct component *c,
                        struct clist *next)
{
  assert(c != NULL);

  struct clist *newp = allocate(heap, sizeof *newp);

  newp->next = next;
  newp->c = c;

  return newp;
}

struct cstlist *new_cstlist(struct alloc_block *heap, struct constant *cst,
                            struct cstlist *next)
{
  struct cstlist *newp = allocate(heap, sizeof *newp);

  newp->next = next;
  newp->cst = cst;

  return newp;
}

bool cstlist_has_len(struct cstlist *list, ulong l)
{
  for (;; list = list->next, --l)
    {
      if (l == 0)
        return true;
      if (list == NULL)
        return false;
    }
}

struct vlist *new_vlist(struct alloc_block *heap, const char *var,
                        unsigned typeset, int lineno, struct vlist *next)
{
  struct vlist *newp = allocate(heap, sizeof *newp);

  newp->next = next;
  newp->var = var;
  newp->typeset = typeset;
  newp->was_read = newp->was_written = false;
  newp->lineno = lineno;

  return newp;
}

struct cstpair *new_cstpair(struct alloc_block *heap, struct constant *cst1,
                            struct constant *cst2)
{
  struct cstpair *newp = allocate(heap, sizeof *newp);
  *newp = (struct cstpair){ .cst1 = cst1, .cst2 = cst2 };
  return newp;
}

struct constant *new_int_constant(struct alloc_block *heap, long l)
{
  return new_constant(heap, cst_int, l);
}

struct constant *new_constant(struct alloc_block *heap,
                              enum constant_class vclass, ...)
{
  va_list args;
  struct constant *newp = allocate(heap, sizeof *newp);

  newp->vclass = vclass;
  va_start(args, vclass);
  switch (vclass)
    {
    case cst_int:
      newp->u.integer = va_arg(args, long);
      break;
    case cst_string:
      newp->u.string = va_arg(args, struct str_and_len);
      break;
    case cst_list:
      {
        newp->u.constants = va_arg(args, struct cstlist *);
        struct cstlist *clhead = newp->u.constants;
        struct cstlist **clp = &newp->u.constants;
        while (*clp && ((*clp)->cst == NULL
                        || (*clp)->cst->vclass != cst_expression))
          clp = &(*clp)->next;
        struct cstlist *cltail = *clp;
        if (cltail == NULL)
          break;

        if (clp == &newp->u.constants)
          {
            /* the first expression is the list tail */
            newp = clhead->cst;
            cltail = cltail->next;
          }
        else if (clp == &newp->u.constants->next)
          {
            /* the first expression is the last element before the tail */
            if (newp->u.constants->cst)
              newp = newp->u.constants->cst;
            else
              newp->u.constants = NULL;
          }
        else
          {
            /* the first expression is here, so truncate the tail */
            *clp = NULL;
          }

        build_heap = heap;

        struct component *l = build_constant(newp);
        for (; cltail; cltail = cltail->next)
          {
            struct component *c = build_constant(cltail->cst);
            l = build_exec(build_recall(GEP "pcons"), 2, c, l);
          }
        newp = allocate(heap, sizeof *newp);
        newp->vclass = cst_expression;
        newp->u.expression = l;
        break;
      }
    case cst_array: case cst_table: case cst_ctable:
      {
        newp->u.constants = va_arg(args, struct cstlist *);
        bool dynamic = false;
        for (struct cstlist *cl = newp->u.constants; cl; cl = cl->next)
          if (cl->cst->vclass == cst_expression)
            {
              dynamic = true;
              break;
            }
        if (!dynamic)
          break;

        build_heap = heap;

        struct clist *cargs = NULL;
        for (struct cstlist *cl = newp->u.constants; cl; cl = cl->next)
          cargs = new_clist(heap, build_constant(cl->cst), cargs);
        cargs = new_clist(heap, build_recall(GEP "sequence"), cargs);
        struct component *c = new_component(heap, 0, c_execute, cargs);
        if (vclass != cst_array)
          {
            const char *builder = (vclass == cst_table
                                   ? GEP "vector_to_ptable"
                                   : GEP "vector_to_pctable");
            c = build_exec(build_recall(builder), 1, c);
          }
        newp->vclass = cst_expression;
        newp->u.expression = c;
        break;
      }
    case cst_float:
      newp->u.mudlle_float = va_arg(args, double);
      break;
    case cst_bigint:
      newp->u.bigint = va_arg(args, struct bigint_const *);
      break;
    case cst_symbol:
      {
        newp->u.constpair = va_arg(args, struct cstpair *);
        if (newp->u.constpair->cst1->vclass != cst_expression
            && newp->u.constpair->cst2->vclass != cst_expression)
          break;

        build_heap = heap;

        struct component *c = build_exec(
          build_recall(GEP "make_psymbol"), 2,
          build_constant(newp->u.constpair->cst1),
          build_constant(newp->u.constpair->cst2));
        newp->vclass = cst_expression;
        newp->u.expression = c;
        break;
      }
    case cst_expression:
      newp->u.expression = va_arg(args, struct component *);
      break;
    default: abort();
    }
  va_end(args);
  return newp;
}

bool cstlist_find_symbol_clash(struct cstlist *list, bool ctable,
                               struct str_and_len **s0,
                               struct str_and_len **s1)
{
  ulong cnt = 0;
  for (struct cstlist *l = list; l; l = l->next)
    {
      struct constant *car = list->cst;
      if (car->vclass == cst_expression)
        continue;
      ++cnt;
    }
  if (cnt < 2)
    return false;

  ulong hsize = table_good_size(cnt);
  assert((hsize & (hsize - 1)) == 0);

  struct hnode {
    struct hnode *next;
    struct str_and_len *e;
  } **hash = calloc(hsize, sizeof *hash);

  ulong (*hashfn)(const char *name, size_t len, ulong size)
    = ctable ? case_symbol_hash_len : symbol_hash_len;
  int (*compare)(const void *a, const void *b, size_t n)
    = ctable ? memcmp : mem8icmp;

  bool result = false;
  for (; list; list = list->next)
    {
      struct constant *car = list->cst;
      if (car->vclass == cst_expression)
        continue;
      assert(car->vclass == cst_symbol);
      struct constant *str = car->u.constpair->cst1;
      assert(str->vclass == cst_string);
      ulong hent = hashfn(str->u.string.str, str->u.string.len, hsize);
      ulong slen = str->u.string.len;
      for (struct hnode *n = hash[hent]; n; n = n->next)
        if (n->e->len == slen
            && compare(n->e->str, str->u.string.str, slen) == 0)
          {
            *s0 = &str->u.string;
            *s1 = n->e;
            result = true;
            goto done;
          }
      struct hnode *n = malloc(sizeof *n);
      *n = (struct hnode){ .next = hash[hent], .e = &str->u.string };
      hash[hent] = n;
    }

 done:
  for (ulong l = 0; l < hsize; ++l)
    for (struct hnode *n = hash[l], *next; n; n = next)
      {
        next = n->next;
        free(n);
      }
  free(hash);
  return result;
}

static struct clist *make_clist(struct alloc_block *heap, int count,
                                va_list args)
{
  struct clist *first = NULL, **scan = &first;

  while (count-- > 0)
    {
      *scan = new_clist(heap, va_arg(args, struct component *), NULL);
      scan = &(*scan)->next;
    }
  return first;
}

struct component *new_component(struct alloc_block *heap, int lineno,
                        enum component_class vclass, ...)
{
  va_list args;
  struct component *newp = allocate(heap, sizeof *newp);

  newp->vclass = vclass;
  newp->lineno = lineno;

  va_start(args, vclass);
  switch (vclass)
    {
    case c_assign:
      newp->u.assign.symbol = va_arg(args, const char *);
      newp->u.assign.value = va_arg(args, struct component *);
      break;
    case c_vref:
    case c_recall:
      newp->u.recall = va_arg(args, const char *);
      break;
    case c_constant:
      {
        struct constant *cst = va_arg(args, struct constant *);
        if (cst->vclass == cst_expression)
          newp = cst->u.expression;
        else
          newp->u.cst = cst;
        break;
      }
    case c_closure:
      newp->u.closure = va_arg(args, struct function *);
      break;
    case c_block:
      newp->u.blk = va_arg(args, struct block *);
      break;
    case c_execute:
      newp->u.execute = va_arg(args, struct clist *);
      break;
    case c_builtin:
      newp->u.builtin.fn = va_arg(args, int); /* enum builtin_op */
      int nargs = va_arg(args, int);
      newp->u.builtin.args = make_clist(heap, nargs, args);
      break;
    case c_labeled: case c_exit:
      newp->u.labeled.name = va_arg(args, const char *);
      newp->u.labeled.expression = va_arg(args, struct component *);
      break;
    default: abort();
    }
  va_end(args);
  return newp;
}

struct pattern *new_pattern_constant(struct alloc_block *heap,
                                     struct constant *c, int lineno)
{
  struct pattern *ap = allocate(heap, sizeof *ap);
  *ap = (struct pattern){
    .vclass = pat_const,
    .lineno = lineno,
    .u.constval = c
  };
  return ap;
}

struct pattern *new_pattern_symbol(struct alloc_block *heap,
                                   struct pattern *sym,
                                   struct pattern *val, int lineno)
{
  struct pattern *ap = allocate(heap, sizeof *ap);
  *ap = (struct pattern){
    .vclass = pat_symbol,
    .lineno = lineno,
    .u.sym = {
      .name = sym,
      .val  = val
    }
  };
  return ap;
}

struct pattern *new_pattern_expression(struct alloc_block *heap,
                                       struct component *c)
{
  struct pattern *ap = allocate(heap, sizeof *ap);
  *ap = (struct pattern){
    .vclass = pat_expr,
    .lineno = c->lineno,
    .u.expr = c
  };
  return ap;
}

struct pattern *new_pattern_sink(struct alloc_block *heap)
{
  struct pattern *ap = allocate(heap, sizeof *ap);
  *ap = (struct pattern){
    .vclass = pat_sink,
    .lineno = 0,
  };
  return ap;
}

struct pattern *new_pattern_variable(struct alloc_block *heap, const char *sym,
                                     enum mudlle_type type, int lineno)
{
  struct pattern *ap = allocate(heap, sizeof *ap);
  *ap = (struct pattern){
    .vclass = pat_variable,
    .lineno = lineno,
    .u.var = {
      .name = sym,
      .type = type
    }
  };
  return ap;
}

struct pattern *new_pattern_compound(struct alloc_block *heap,
                                     enum pattern_class class,
                                     struct pattern_list *list,
                                     bool ellipsis,
                                     int lineno)
{
  assert(class == pat_list || class == pat_array);

  if (ellipsis)
    goto not_const;

  for (struct pattern_list *p = list; p != NULL; p = p->next)
    if (p->pat && p->pat->vclass != pat_const)
      goto not_const;

  struct cstlist *cl = NULL;
  for (struct pattern_list *p = list; p != NULL; p = p->next)
    cl = new_cstlist(heap, p->pat ? p->pat->u.constval : NULL, cl);
  cl = reverse_cstlist(cl);
  return new_pattern_constant(
    heap,
    new_constant(heap, class == pat_list ? cst_list : cst_array, cl),
    lineno);

 not_const: ;
  struct pattern *ap = allocate(heap, sizeof *ap);
  *ap = (struct pattern){
    .vclass = class,
    .lineno = lineno,
    .u.l = {
      .patlist = list,
      .ellipsis = ellipsis
    }
  };
  return ap;
}

struct pattern_list *new_pattern_list(struct alloc_block *heap,
			     struct pattern *pat,
			     struct pattern_list *tail)
{
  struct pattern_list *apl = allocate(heap, sizeof *apl);
  apl->next = tail;
  apl->pat = pat;
  return apl;
}

struct match_node_list *new_match_list(struct alloc_block *heap,
                                       struct match_node *node,
                                       struct match_node_list *tail)
{
  struct match_node_list *ml = allocate(heap, sizeof *ml);
  ml->next = tail;
  ml->match = node;
  return ml;
}

struct match_node *new_match_node(struct alloc_block *heap,
                                  struct pattern *pat, struct component *cond,
                                  struct component *e, const char *filename,
                                  int lineno)
{
  struct match_node *nd = allocate(heap, sizeof *nd);
  nd->pattern = pat;
  nd->expression = e;
  nd->condition = cond;
  nd->filename = filename;
  nd->lineno = lineno;
  return nd;
}

static inline struct vlist *reverse_vlist(struct vlist *l)
{
  return reverse_list(l, struct vlist);
}

/* Make a mudlle rep of a parse tree */
static value mudlle_parse_component(struct component *c);

static value mudlle_vlist(struct vlist *vars)
{
  value l = NULL;
  struct string *s = NULL;

  GCPRO2(l, s);
  for (vars = reverse_vlist(vars); vars; vars = vars->next)
    {
      s = make_readonly(alloc_string(vars->var));
      struct vector *v = alloc_vector(3);
      v->data[0] = s;
      v->data[1] = makeint(vars->typeset);
      v->data[2] = makeint(vars->lineno);
      l = alloc_list(v, l);
    }
  UNGCPRO();
  return l;
}

static value mudlle_clist(struct clist *exprs)
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

static value mudlle_parse_component(struct component *c)
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
      SET(0, make_readonly(alloc_string(c->u.assign.symbol)));
      SET(1, mudlle_parse_component(c->u.assign.value));
      break;

    case c_vref:
    case c_recall:
      SET(0, make_readonly(alloc_string(c->u.recall)));
      break;

    case c_constant:
      SET(0, make_constant(c->u.cst));
      break;

    case c_closure: {
      struct function *f = c->u.closure;
      SET(0, makeint(f->typeset));
      SET(1, (f->help.len
              ? make_readonly(alloc_string_length(f->help.str, f->help.len))
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
              ? make_readonly(alloc_string(c->u.labeled.name))
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

value mudlle_parse(struct alloc_block *heap, struct mfile *f)
{
  struct vector *file = alloc_vector(parser_module_fields);
  GCPRO1(file);

  struct component *cbody = new_component(heap, f->lineno, c_block, f->body);

  SET_VECTOR(file, m_class,    makeint(f->vclass));
  SET_VECTOR(file, m_name,     (f->name
                                ? make_readonly(alloc_string(f->name))
                                : makebool(false)));
  SET_VECTOR(file, m_requires,  mudlle_vlist(f->requires));
  SET_VECTOR(file, m_defines,  mudlle_vlist(f->defines));
  SET_VECTOR(file, m_reads,    mudlle_vlist(f->reads));
  SET_VECTOR(file, m_writes,   mudlle_vlist(f->writes));
  SET_VECTOR(file, m_statics,  mudlle_vlist(f->statics));
  SET_VECTOR(file, m_body,     mudlle_parse_component(cbody));
  SET_VECTOR(file, m_filename, make_readonly(alloc_string(f->body->filename)));
  SET_VECTOR(file, m_nicename, make_readonly(alloc_string(f->body->nicename)));
  UNGCPRO();

  return file;
}

#ifdef PRINT_CODE
static void print_constant(FILE *f, struct constant *c);

static void print_list(FILE *f, struct cstlist *head, struct constant *tail)
{
  head = reverse_cstlist(head);
  const char *prefix = "";
  for (struct cstlist *l = head; l; l = l->next)
    {
      fputs(prefix, f);
      print_constant(f, l->cst);
      prefix = " ";
    }
  head = reverse_cstlist(head);
  if (tail)
    {
      fputs(" . ", f);
      print_constant(f, tail);
    }
}

static void print_vlist(FILE *f, struct vlist *l)
{
  int first = true;

  while (l)
    {
      if (!first) fprintf(f, ", ");
      first = false;
      unsigned t = l->typeset;
      if (t != TYPESET_ANY)
	{
	  fputc('{', f);
	  const char *prefix = "";
	  for (unsigned n = 0; t; ++n, t >>= 1)
	    if (t & 1)
	      {
		fputs(prefix, f);
		prefix = ",";
		fputs(mudlle_type_names[n], f);
	      }
	  fputs("} ", f);
	}
      fputs(l->var, f);
      l = l->next;
    }
}

static void print_component(FILE *f, struct component *c);

static void print_constant(FILE *f, struct constant *c)
{
  switch (c->vclass)
    {
    case cst_int:
      fprintf(f, "%ld", c->u.integer);
      break;
    case cst_string:
      fprintf(f, "\"%.*s\"" , (int)c->u.string.len, c->u.string.str);
      break;
    case cst_float:
      fprintf(f, "%f", c->u.mudlle_float);
      break;
    case cst_list:
      fprintf(f, "(");
      if (c->u.constants)
        print_list(f, c->u.constants->next, c->u.constants->cst);
      fprintf(f, ")");
      break;
    case cst_array:
      fprintf(f, "[");
      print_list(f, c->u.constants, NULL);
      fprintf(f, "]");
      break;
    case cst_table:
    case cst_ctable:
      fprintf(f, "{%s", c->vclass == cst_table ? "" : "c ");
      print_list(f, c->u.constants, NULL);
      fprintf(f, "}");
      break;
    case cst_symbol:
      fputs("<", f);
      print_constant(f, c->u.constpair->cst1);
      fputs("=", f);
      print_constant(f, c->u.constpair->cst2);
      fputs(">", f);
      break;
    case cst_expression:
      fprintf(f, ",(");
      print_component(f, c->u.expression);
      fprintf(f, ")");
      break;
    default:
      abort();
    }
}

static void print_block(FILE *f, struct block *c)
{
  struct vlist *vars = c->locals;
  struct clist *sequence = c->sequence;

  fputs("[ ", f);
  if (vars)
    {
      fputs("|", f);
      print_vlist(f, vars);
      fputs("| ", f);
    }
  while (sequence)
    {
      print_component(f, sequence->c);
      fputc(' ', f);
      sequence = sequence->next;
    }
  fputc(']', f);
}

static void print_clist(FILE *f, struct clist *sequence)
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

static void print_function(FILE *f, struct function *fn)
{
  fputs("fn ", f);
  if (fn->help.len > 0)
    fprintf(f, "\"%.*s\" ", (int)fn->help.len, fn->help.str);
  fputs("(", f);
  print_vlist(f, fn->args);
  fputs(") ", f);
  print_component(f, fn->value);
}

static void print_component(FILE *f, struct component *c)
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
    default: abort();
    }
}

void print_mudlle_file(FILE *out, struct mfile *f)
{
  static const char *const fnames[] = { "", "module", "library" };

  fputs(fnames[f->vclass], out);
  if (f->name) fprintf(out, " %s\n", f->name);
  if (f->requires)
    {
      fprintf(out, "requires ");
      print_vlist(out, f->requires);
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
    struct alloc_block *oops = new_block();

    print_component(out, new_component(oops, f->lineno, c_block, f->body));
    free_block(oops);
  }
}

#endif

static struct clist *build_clist(int n, ...)
{
  va_list args;
  struct clist *res = NULL;

  va_start(args, n);
  while (n-- > 0)
    {
      struct component *c = va_arg(args, struct component *);
      if (c == NULL)
        continue;
      res = new_clist(build_heap, c, res);
    }
  va_end(args);

  return reverse_clist(res);
}

struct component *new_int_component(struct alloc_block *heap, long n)
{
  return new_component(heap, 0, c_constant, new_int_constant(heap, n));
}

static struct component *build_int_component(long n)
{
  return new_int_component(build_heap, n);
}

static struct component *build_string_component(const char *s) UNUSED;

static struct component *build_string_component(const char *s)
{
  struct str_and_len sl = {
    .str = (char *)s,
    .len = strlen(s)
  };
  return new_component(build_heap, 0, c_constant,
		       new_constant(build_heap, cst_string, sl));
}

static struct component *build_assign(int lineno, const char *var,
                                      struct component *val)
{
  assert(var != NULL);
  return new_component(build_heap, lineno, c_assign, var, val);
}

static struct component *build_recall(const char *var)
{
  return new_component(build_heap, 0, c_recall, var);
}

static struct component *build_exec(struct component *f, int n, ...)
{
  va_list args;
  struct clist *res = new_clist(build_heap, f, NULL);

  va_start(args, n);
  while (n--)
    res = new_clist(build_heap, va_arg(args, struct component *), res);
  va_end(args);

  return new_component(build_heap, 0, c_execute, reverse_clist(res));
}

/* args are name, typeset, line, name, typeset, line, ... */
static struct vlist *build_vlist(int n, ...)
{
  va_list args;
  struct vlist *res = NULL;

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

struct component *new_binop_component(struct alloc_block *heap, int lineno,
                                      enum builtin_op op,
                                      struct component *e1,
                                      struct component *e2)
{
  if (op == b_xor)
    return new_xor_component(heap, lineno, e1, e2);
  assert(op >= 0 && op < last_builtin);
  return new_component(heap, lineno, c_builtin, op, 2, e1, e2);
}

static struct component *build_unop(enum builtin_op op, struct component *e)
{
  return new_component(build_heap, 0, c_builtin, op, 1, e);
}

static struct component *build_if(struct component *cond,
                                  struct component *ctrue,
                                  struct component *cfalse)
{
  return new_component(build_heap, 0, c_builtin,
                       cfalse ? b_ifelse : b_if,
                       cfalse ? 3 : 2,
                       cond, ctrue, cfalse);
}

static struct component *build_unless(struct component *cond,
                                      struct component *otherwise)
{
  return build_if(build_unop(b_not, cond), otherwise, NULL);
}

static struct component *build_exit(const char *name, struct component *c)
{
  return new_component(build_heap, 0, c_exit, name, c);
}

static struct component *build_binop(enum builtin_op op, struct component *e1,
                                     struct component *e2)
{
  return new_binop_component(build_heap, 0, op, e1, e2);
}

static struct component *build_codeblock(struct vlist *vl, struct clist *code)
{
  return new_component(build_heap, 0, c_block,
		       new_codeblock(build_heap, vl, code, NULL, NULL, -1));
}

static struct component *build_constant(struct constant *cst)
{
  return new_component(build_heap, 0, c_constant, cst);
}

static struct component *build_const_not_equal(struct constant *cst,
                                               struct component *e)
{
  switch (cst->vclass) {
  case cst_int:
  simple:
    return build_binop(b_ne, e, build_constant(cst));
  case cst_list:
    if (cst->u.constants == NULL)
      goto simple;
    /* fallthrough */
  default:
    return build_unop(
      b_not,
      build_exec(build_recall(GEP "equal?"), 2, build_constant(cst), e));
  }
}

static struct component *build_typecheck(struct component *e,
                                         enum mudlle_type type)
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
    return build_binop(
      b_eq, e,
      build_constant(new_constant(build_heap, cst_list, NULL)));
  case stype_none:
    return component_false;
  case stype_any:
    return component_true;
  default:
    abort();
  }
  return build_exec(build_recall(f), 1, e);
}

static struct component *build_error(enum runtime_error error)
{
  return build_exec(build_recall(GEP "error"), 1,
		    build_int_component(error));
}

/* true if 'c' is a recall of a single-assignment variable */
static bool is_safe_recall(struct component *c)
{
  return c->vclass == c_recall && c->u.recall[0] == '$';
}

static struct component *make_local_var(struct vlist **locals, int level,
                                        int lineno)
{
  char buf[16];
  sprintf(buf, "$%d", level);
  const char *tmpname = heap_allocate_string(build_heap, buf);
  *locals = new_vlist(build_heap, tmpname, TYPESET_ANY,
                      lineno, *locals);
  return build_recall(tmpname);
}

static struct component *make_safe_copy(struct component *e,
                                        struct vlist **locals,
                                        struct component **aexpr,
                                        int level, int lineno)
{
  if (is_safe_recall(e))
    return e;

  struct component *recall = make_local_var(locals, level, lineno);
  assert(recall->vclass == c_recall);
  *aexpr = build_assign(lineno, recall->u.recall, e);
  return recall;
}

static struct component *build_match_block(
  struct pattern *pat, struct component *e, int level, struct vlist **psymbols,
  struct component *(*err)(void *data), void *err_data);

static struct component *build_symbol_name_check(
  struct pattern *pat, struct component *e, int level, struct vlist **psymbols,
  struct component *(*err)(void *data), void *err_data)
{
  switch (pat->vclass)
    {
    case pat_sink:
      return NULL;
    case pat_variable:
      return build_match_block(pat, e, level, psymbols, err, err_data);
    case pat_expr:
      {
        struct vlist *locals = NULL;
        struct component *lassign = NULL;
        struct component *nexp = make_safe_copy(pat->u.expr, &locals, &lassign,
                                        level, pat->lineno);
        return build_codeblock(
          locals,
          build_clist(
            2,
            lassign,
            build_unless(
              build_binop(
                b_sc_and,
                build_typecheck(nexp, type_string),
                build_exec(build_recall(GEP "string_iequal?"), 2,
                           nexp, e)),
              err(err_data))));
      }
    case pat_const:
      {
        struct constant *cst = pat->u.constval;
        if (cst->vclass == cst_string)
          return build_unless(
            build_exec(build_recall(GEP "string_iequal?"), 2,
                       build_constant(cst), e),
            err(err_data));
        /* fallthrough */
      }
    case pat_list:
    case pat_array:
    case pat_symbol:
      compile_error("symbol names must be strings");
      return NULL;
    }
  abort();
}

static struct component *build_match_block(
  struct pattern *pat, struct component *e, int level, struct vlist **psymbols,
  struct component *(*err)(void *data), void *err_data)
{
  struct component *result;

  switch (pat->vclass) {
  case pat_sink:
    return NULL;
  case pat_variable:
    {
      for (struct vlist *sym = *psymbols; sym; sym = sym->next)
	if (strcasecmp(sym->var, pat->u.var.name) == 0)
	  {
	    compile_error("repeated variable name in match pattern (%s)",
                          pat->u.var.name);
	    return NULL;
	  }

      *psymbols = new_vlist(
        build_heap, pat->u.var.name, TYPESET_ANY, pat->lineno, *psymbols);

      result = build_assign(0, pat->u.var.name, e);
      if (pat->u.var.type != stype_any)
        result = build_codeblock(
          NULL,
          build_clist(
            2, result,
            build_unless(build_typecheck(build_recall(pat->u.var.name),
                                         pat->u.var.type),
                         err(err_data))));
      break;
    }
  case pat_const:
    result = build_if(build_const_not_equal(pat->u.constval, e),
                      err(err_data), NULL);
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

      struct component *eassign = NULL;
      struct vlist *vlocals = NULL;
      struct component *elocal = make_safe_copy(e, &vlocals, &eassign, level,
                                        pat->lineno);

      int vlen = 0;
      for (struct pattern_list *apl = pat->u.l.patlist; apl; apl = apl->next)
        ++vlen;

      struct component *lc = NULL;
      if (!pat->u.l.ellipsis || vlen > 0)
        {
          lc = build_if(
            build_binop(
              pat->u.l.ellipsis ? b_lt : b_ne,
              build_exec(build_recall(GEP "vector_length"), 1, elocal),
              build_int_component(vlen)),
            err(err_data),
            NULL);
          lc->lineno = pat->lineno;
        }

      struct component *tc = build_unless(
        build_exec(build_recall(GEP "vector?"), 1, elocal),
        err(err_data));
      tc->lineno = pat->lineno;

      struct clist *code = NULL;
      int n = vlen;
      for (struct pattern_list *apl = pat->u.l.patlist; apl; apl = apl->next)
	{
          --n;
	  struct component *c = build_match_block(
	    apl->pat, build_binop(b_ref, elocal, build_int_component(n)),
            level + 1, psymbols, err, err_data);
          if (c != NULL)
            code = new_clist(build_heap, c, code);
	}

      if (lc != NULL)
        code = new_clist(build_heap, lc, code);
      code = new_clist(build_heap, tc, code);
      if (eassign != NULL)
        code = new_clist(build_heap, eassign, code);

      result = build_codeblock(vlocals, code);
      break;
    }
  case pat_list:
    {
      if (pat->u.l.patlist == NULL)
	{
	  result = build_if(
            build_const_not_equal(
              new_constant(build_heap, cst_list, NULL), e),
            err(err_data), NULL);
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

      for (struct pattern_list *apl = pat->u.l.patlist->next;
           apl;
           apl = apl->next)
        ++level;
      int nlevel = level;

      struct clist *code = NULL;
      struct vlist *locals = NULL;
      struct component *exp = NULL;
      for (struct pattern_list *apl = pat->u.l.patlist->next;
           apl;
           apl = apl->next)
	{
          struct component *texp = (apl->next == NULL && is_safe_recall(e)
                            ? e
                            : make_local_var(&locals, --level, pat->lineno));

          if (exp == NULL)
            {
              struct component *getcdr = build_exec(build_recall(GEP "cdr"), 1,
                                            texp);
              struct component *c =
                (pat->u.l.patlist->pat == NULL
                 ? build_if(build_const_not_equal(
                              new_constant(build_heap, cst_list, NULL),
                              getcdr),
                            err(err_data), NULL)
                 : build_match_block(pat->u.l.patlist->pat, getcdr,
                                     nlevel, psymbols, err, err_data));
              if (c)
                code = new_clist(build_heap, c, code);
            }
          else
            {
              code = new_clist(
                build_heap,
                build_assign(0, exp->u.recall,
                             build_exec(build_recall(GEP "cdr"), 1, texp)),
                code);
            }
          exp = texp;

          struct component *mc = build_match_block(
            apl->pat,
            build_exec(build_recall(GEP "car"), 1, exp),
            nlevel, psymbols, err, err_data);
          if (mc != NULL)
            code = new_clist(build_heap, mc, code);

          struct component *pc = build_unless(
            build_exec(build_recall(GEP "pair?"), 1, exp),
            err(err_data));
          pc->lineno = apl->pat->lineno;
          code = new_clist(build_heap, pc, code);
        }

      if (!is_safe_recall(e))
        code = new_clist(build_heap, build_assign(0, exp->u.recall, e), code);

      result = build_codeblock(reverse_vlist(locals), code);
      break;
    }
  case pat_expr:
    result = build_unless(build_exec(build_recall(GEP "equal?"),
                                     2, pat->u.expr, e),
                          err(err_data));
    break;
  case pat_symbol:
    {
      struct vlist *locals = NULL;
      struct component *aexp = NULL;
      e = make_safe_copy(e, &locals, &aexp, level, pat->lineno);
      result = build_codeblock(
        locals,
        build_clist(
          4,
          aexp,
          build_unless(build_typecheck(e, type_symbol), err(err_data)),
          build_symbol_name_check(
            pat->u.sym.name,
            build_exec(build_recall(GEP "symbol_name"), 1, e),
            level + 1,
            psymbols, err, err_data),
          build_match_block(
            pat->u.sym.val,
            build_exec(build_recall(GEP "symbol_get"), 1, e),
            level + 2,
            psymbols, err, err_data)));
      break;
    }
  default:
    abort();
  }
  result->lineno = pat->lineno;
  return result;
}

static struct component *no_match_error(void *data)
{
  return build_error(error_no_match);
}

struct component *new_pattern_component(struct alloc_block *heap,
                                        struct pattern *pat,
                                        struct component *e)
{
  build_heap = heap;
  struct vlist *psymbols = NULL;
  /* Warning: if the match fails, this might leave only some of the variables
   * in the pattern filled. But it's a feature, right? */
  return build_codeblock(
    NULL,
    build_clist(
      2,
      build_match_block(pat, e, 0, &psymbols, no_match_error, NULL),
      component_undefined));
}

struct component *new_dereference(struct alloc_block *heap, int lineno,
                                  struct component *e)
{
  build_heap = heap;
  struct component *c = build_exec(build_recall(GEP "dereference"), 1, e);
  c->lineno = lineno;
  return c;
}

/* return dereferenced value or NULL */
static struct component *is_dereference(struct component *e)
{
  if (e->vclass != c_execute)
    return NULL;
  struct clist *cl = e->u.execute;
  if (cl == NULL || cl->next == NULL || cl->next->next != NULL)
    return NULL;
  struct component *f = cl->c;
  if (f->vclass != c_recall)
    return NULL;
  if (strcmp(f->u.recall, GEP "dereference") != 0)
    return NULL;
  return cl->next->c;
}

struct component *new_reference(struct alloc_block *heap, int lineno,
                                struct component *e)
{
  build_heap = heap;

  if (e->vclass == c_builtin && e->u.builtin.fn == b_ref)
    {
      struct clist *args = e->u.builtin.args;
      return build_exec(build_recall(GEP "make_ref"), 2,
                        args->c, args->next->c);

    }

  if (e->vclass == c_recall)
    return new_component(heap, lineno, c_vref, e->u.recall);

  if (e->vclass == c_execute)
    {
      struct clist *args = e->u.execute;
      struct component *f = args->c;
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

static struct component *build_set_ref(struct component *e0,
                                       struct component *e1)
{
  return build_exec(build_recall(GEP "set_ref!"), 2, e0, e1);
}

struct component *new_for_component(struct alloc_block *heap,
                                    struct vlist *vars,
                                    struct component *einit,
                                    struct component *eexit,
                                    struct component *eloop,
                                    struct component *e,
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
  struct clist *code = NULL;

  build_heap = heap;

  if (eloop)
    code = new_clist(build_heap, eloop, code);

  code = new_clist(build_heap,
                   new_component(build_heap, e->lineno, c_labeled,
                                 "continue", e),
                   code);

  if (eexit)
    {
      struct component *c = build_if(build_unop(b_not, eexit),
                             build_exit("break", component_undefined),
                             NULL);
      code = new_clist(build_heap, c, code);
    }

  struct component *c = build_codeblock(NULL, code);
  c = build_unop(b_loop, c);

  code = new_clist(build_heap, c, NULL);
  if (einit)
    code = new_clist(build_heap, einit, code);

  c = build_codeblock(vars, code);
  c->u.blk->filename = filename;
  c->u.blk->lineno = lineno;
  return new_component(build_heap, c->lineno, c_labeled, "break", c);
}

static struct component *next_error(void *count)
{
  ++*(int *)count;
  return build_exit("$next", component_undefined);
}

struct component *new_match_component(struct alloc_block *heap,
                                      struct component *e,
                                      struct match_node_list *matches)
{
  build_heap = heap;

  /*
   *   <$match> [
   *     | $exp |
   *
   *     $exp = <match-expression>
   *     <$next> [                               \
   *       | <pattern-variables> |                |
   *       if (<pattern-match> [&& <condition>])  +- repeat for each match node
   *         exit<$match> <pattern-expression>;   |
   *     ]                                       /
   *     false
   *   ]
   */

  struct vlist *vl = build_vlist(1, "$exp", TYPESET_ANY, e->lineno);
  struct clist *code = build_clist(1, component_false);
  for (; matches; matches = matches->next)
    {
      struct vlist *psymbols = NULL;
      int next_count = 0;
      struct component *matchcode = build_match_block(matches->match->pattern,
                                              build_recall("$exp"), 0,
                                              &psymbols,
                                              next_error, &next_count);
      struct component *cexit
        = build_exit("$match", matches->match->expression);

      struct clist *cl = build_clist(
        3,
        matchcode,
        (matches->match->condition
         ? build_unless(matches->match->condition, next_error(&next_count))
         : NULL),
        cexit);

      struct component *cblock = build_codeblock(reverse_vlist(psymbols), cl);
      cblock->u.blk->filename = matches->match->filename;
      cblock->u.blk->lineno = matches->match->lineno;

      if (next_count)
        cblock = new_component(build_heap, 0, c_labeled, "$next", cblock);

      code = new_clist(build_heap, cblock, code);
    }

  code = new_clist(build_heap, build_assign(0, "$exp", e), code);

  return new_component(build_heap, 0, c_labeled, "$match",
		       build_codeblock(vl, code));
}

struct component *new_xor_component(struct alloc_block *heap, int lineno,
                                    struct component *e0,
                                    struct component *e1)
{
  build_heap = heap;
  return build_binop(b_bitxor, build_unop(b_not, e0), build_unop(b_not, e1));
}

static struct component *comp_id(struct component *e)
{
  return e;
}

static struct component *comp_set_tmp(struct component *e)
{
  return build_assign(0, "$tmp", e);
}

struct component *new_assign_expression(struct alloc_block *heap,
                                        struct component *e0,
                                        enum builtin_op op,
                                        struct component *e1, bool postfix,
                                        int lineno)
{
  build_heap = heap;

  if (op == b_invalid)
    {
      if (e0->vclass == c_builtin && e0->u.builtin.fn == b_ref)
        {
          struct clist *args = e0->u.builtin.args;
          return new_component(heap, lineno, c_builtin, b_set, 3,
                               args->c, args->next->c, e1);
        }
      if (e0->vclass == c_recall)
        return build_assign(lineno, e0->u.recall, e1);
      struct component *r = is_dereference(e0);
      if (r == NULL)
        goto error;
      return build_set_ref(r, e1);
    }

  struct component *(*set_tmp)(struct component *)
    = postfix ? comp_set_tmp : comp_id;
  struct component *ret = postfix ? build_recall("$tmp") : NULL;
  const char *tmp_name = postfix ? "$tmp" : NULL;

  if (e0->vclass == c_builtin && e0->u.builtin.fn == b_ref)
    {
      struct clist *args = e0->u.builtin.args;
      struct vlist *vl = build_vlist(3,
                             tmp_name, TYPESET_ANY, e0->lineno,
                             "$ref", TYPESET_ANY, args->c->lineno,
                             "$exp", TYPESET_ANY, args->next->c->lineno);
      struct clist *cl = build_clist(
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
      struct vlist *vl = build_vlist(1, tmp_name, TYPESET_ANY, e0->lineno);
      struct clist *cl = build_clist(
        2,
        build_assign(lineno, e0->u.recall, build_binop(op, set_tmp(e0), e1)),
        ret);
      /* optimize for common case */
      if (vl == NULL && cl->next == NULL)
        return cl->c;
      return build_codeblock(vl, cl);
    }

  struct component *r = is_dereference(e0);
  if (r != NULL)
    {
      struct vlist *vl = build_vlist(2,
                             tmp_name, TYPESET_ANY, e0->lineno,
                             "$ref", TYPESET_ANY, e0->lineno);
      struct clist *cl = build_clist(
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

struct block *new_toplevel_codeblock(struct alloc_block *heap,
                                     struct vlist *statics,
                                     struct block *body)
{
  if (statics == NULL)
    return body;

  build_heap = heap;
  struct clist *cl = build_clist(1, new_component(heap, body->lineno,
                                                  c_block, body));
  body = new_codeblock(build_heap, statics, cl, body->filename, body->nicename,
                       body->lineno);
  body->statics = true;
  return body;
}

void str_and_len_dup(struct str_and_len *dest, const struct str_and_len *src)
{
  dest->len = src->len;
  dest->str = xmalloc(src->len);
  memcpy(dest->str, src->str, src->len);
}
