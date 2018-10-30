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
#include "lexer.h"
#include "mvalues.h"
#include "table.h"
#include "tree.h"
#include "utils.h"

#define GEP GLOBAL_ENV_PREFIX

static struct alloc_block *build_heap;

#define PUSH_BUILD_HEAP(heap)                                   \
  (void)0;                                                      \
  struct alloc_block *const old_build_heap = build_heap;        \
  struct alloc_block *const my_build_heap  = (heap);            \
  build_heap = my_build_heap

#define POP_BUILD_HEAP()                        \
  assert(build_heap == my_build_heap);          \
  build_heap = old_build_heap

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

static bool is_global_var(const char *name)
{
  return strncmp(name, GEP, strlen(GEP)) == 0;
}

struct mfile *new_file(struct alloc_block *heap, enum file_class vclass,
                       const char *name,
                       struct vlist *requires, struct vlist *defines,
                       struct vlist *reads, struct vlist *writes,
                       struct vlist *statics, struct block *body,
                       const struct loc *loc)
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
    .loc      = *loc,
  };

  return newp;
}

struct function *new_fn(struct alloc_block *heap, struct component *avalue,
                        const struct loc *loc, const char *filename,
                        const char *nicename)
{
  struct function *newp = allocate(heap, sizeof *newp);
  *newp = (struct function){
    .typeset  = TYPESET_ANY,
    .value    = avalue,
    .filename = filename,
    .nicename = nicename,
    .loc      = *loc
  };
  return newp;
}

struct block *new_codeblock(struct alloc_block *heap, struct vlist *locals,
                            struct clist *sequence, const char *filename,
                            const char *nicename,
                            const struct loc *loc)
{
  struct block *newp = allocate(heap, sizeof *newp);
  *newp = (struct block){
    .locals   = locals,
    .sequence = sequence,
    .filename = filename,
    .nicename = nicename,
    .loc      = *loc,
    .statics  = false
  };

  return newp;
}

struct clist *new_clist(struct alloc_block *heap, struct component *c,
                        struct clist *next)
{
  assert(c != NULL);

  struct clist *newp = allocate(heap, sizeof *newp);
  *newp = (struct clist){
    .next = next,
    .c    = c
  };
  return newp;
}

struct cstlist *new_cstlist(struct alloc_block *heap, struct constant *cst,
                            struct cstlist *next)
{
  struct cstlist *newp = allocate(heap, sizeof *newp);
  *newp = (struct cstlist){
    .next = next,
    .cst  = cst
  };
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
                        unsigned typeset, const struct loc *loc,
                        struct vlist *next)
{
  struct vlist *newp = allocate(heap, sizeof *newp);
  *newp = (struct vlist){
    .next        = next,
    .var         = var,
    .typeset     = typeset,
    .was_read    = false,
    .was_written = false,
    .loc         = *loc
  };
  return newp;
}

struct vlist *vlist_find(struct vlist *l, const char *s)
{
  for (; l; l = l->next)
    if (strcasecmp(l->var, s) == 0)
      return l;
  return NULL;
}

unsigned vlist_length(struct vlist *l)
{
  unsigned n = 0;
  for (; l; l = l->next)
    ++n;
  return n;
}

static struct constant *alloc_const(struct alloc_block *heap,
                                    enum constant_class vclass)
{
  struct constant *newp = allocate(heap, sizeof *newp);
  *newp = (struct constant){ .vclass = vclass };
  return newp;
}

struct cstpair *new_cstpair(struct alloc_block *heap, struct constant *cst1,
                            struct constant *cst2)
{
  struct cstpair *newp = allocate(heap, sizeof *newp);
  *newp = (struct cstpair){ .cst1 = cst1, .cst2 = cst2 };
  return newp;
}

struct constant *new_expr_constant(struct alloc_block *heap,
                                   struct component *e)
{
  struct constant *newp = alloc_const(heap, cst_expression);
  newp->u.expression = e;
  return newp;
}

struct constant *new_int_constant(struct alloc_block *heap, long l)
{
  struct constant *newp = alloc_const(heap, cst_int);
  newp->u.integer = l;
  return newp;
}

struct constant *new_string_constant(struct alloc_block *heap,
                                     const struct str_and_len *str)
{
  struct constant *newp = alloc_const(heap, cst_string);
  newp->u.string = *str;
  return newp;
}

struct constant *new_symbol_constant(struct alloc_block *heap,
                                     struct cstpair *pair)
{
  if (pair->cst1->vclass == cst_expression
      || pair->cst2->vclass == cst_expression)
    {
      PUSH_BUILD_HEAP(heap);
      struct component *c = build_exec(
        build_recall(GEP "make_psymbol"), 2,
        build_constant(pair->cst1),
        build_constant(pair->cst2));
      POP_BUILD_HEAP();
      return new_expr_constant(heap, c);
    }

  assert(pair->cst1->vclass == cst_string);

  struct constant *newp = alloc_const(heap, cst_symbol);
  newp->u.constpair = pair;
  return newp;
}

struct constant *new_float_constant(struct alloc_block *heap, double d)
{
  struct constant *newp = alloc_const(heap, cst_float);
  newp->u.mudlle_float = d;
  return newp;
}

struct constant *new_bigint_constant(struct alloc_block *heap,
                                     struct bigint_const *bi)
{
  struct constant *newp = alloc_const(heap, cst_bigint);
  newp->u.bigint = bi;
  return newp;
}

struct constant *new_list_constant(struct alloc_block *heap,
                                   struct cstlist *lst)
{
  struct constant *newp = alloc_const(heap, cst_list);
  newp->u.constants = lst;

  struct cstlist *clhead = newp->u.constants;
  struct cstlist **clp = &newp->u.constants;
  while (*clp && ((*clp)->cst == NULL
                  || (*clp)->cst->vclass != cst_expression))
    clp = &(*clp)->next;
  struct cstlist *cltail = *clp;
  if (cltail == NULL)
    return newp;

  if (clp == &newp->u.constants)
    {
      /* the first expression is the list tail */
      newp = clhead->cst;
      assert(newp != NULL);
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

  PUSH_BUILD_HEAP(heap);
  struct component *l = build_constant(newp);
  for (; cltail; cltail = cltail->next)
    {
      struct component *c = build_constant(cltail->cst);
      l = build_exec(build_recall(GEP "pcons"), 2, c, l);
    }
  POP_BUILD_HEAP();

  newp = alloc_const(heap, cst_expression);
  newp->u.expression = l;
  return newp;
}

static struct constant *new_sequence_constant(
  struct alloc_block *heap, enum constant_class vclass, struct cstlist *lst)
{
  assert(vclass == cst_array || vclass == cst_table || vclass == cst_ctable);

  for (struct cstlist *cl = lst; cl; cl = cl->next)
    if (cl->cst->vclass == cst_expression)
      goto dynamic;

  {
    struct constant *newp = alloc_const(heap, vclass);
    newp->u.constants = lst;
    return newp;
  }

 dynamic:
  PUSH_BUILD_HEAP(heap);
  struct clist *cargs = NULL;
  for (struct cstlist *cl = lst; cl; cl = cl->next)
    cargs = new_clist(heap, build_constant(cl->cst), cargs);
  cargs = new_clist(heap, build_recall(GEP "sequence"), cargs);
  struct component *c = new_execute_component(heap, NO_LOC, cargs);
  if (vclass != cst_array)
    {
      const char *builder = (vclass == cst_table
                             ? GEP "vector_to_ptable"
                             : GEP "vector_to_pctable");
      c = build_exec(build_recall(builder), 1, c);
    }
  POP_BUILD_HEAP();

  struct constant *newp = alloc_const(heap, cst_expression);
  newp->u.expression = c;
  return newp;
}

struct constant *new_array_constant(struct alloc_block *heap,
                                    struct cstlist *lst)
{
  return new_sequence_constant(heap, cst_array, lst);
}

struct constant *new_table_constant(struct alloc_block *heap,
                                    enum constant_class vclass,
                                    struct cstlist *lst)
{
  assert(vclass == cst_table || vclass == cst_ctable);
  return new_sequence_constant(heap, vclass, lst);
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

static struct component *alloc_component(struct alloc_block *heap,
                                         const struct loc *loc,
                                         enum component_class vclass)
{
  struct component *newp = allocate(heap, sizeof *newp);
  *newp = (struct component){
    .vclass = vclass,
    .loc    = *loc
  };
  return newp;
}

static struct component *new_assign_component(
  struct alloc_block *heap, const struct loc *loc, const char *sym,
  struct component *e)
{
  assert(sym != NULL);
  struct component *c = alloc_component(heap, loc, c_assign);
  c->u.assign.symbol = sym;
  c->u.assign.value = e;
  return c;
}

struct component *new_recall_component(
  struct alloc_block *heap, const struct loc *loc, const char *sym)
{
  assert(sym != NULL);
  struct component *c = alloc_component(heap, loc, c_recall);
  c->u.recall = sym;
  return c;
}

static struct component *new_vref_component(
  struct alloc_block *heap, const struct loc *loc, const char *sym)
{
  assert(sym != NULL);
  struct component *c = alloc_component(heap, loc, c_vref);
  c->u.recall = sym;
  return c;
}

struct component *new_const_component(
  struct alloc_block *heap, const struct loc *loc, struct constant *cst)
{
  if (cst->vclass == cst_expression)
    return cst->u.expression;

  struct component *c = alloc_component(heap, loc, c_constant);
  c->u.cst = cst;
  return c;
}

struct component *new_closure_component(
  struct alloc_block *heap, const struct loc *loc, struct function *f)
{
  struct component *c = alloc_component(heap, loc, c_closure);
  c->u.closure = f;
  return c;
}

struct component *new_block_component(
  struct alloc_block *heap, const struct loc *loc, struct block *blk)
{
  struct component *c = alloc_component(heap, loc, c_block);
  c->u.blk = blk;
  return c;
}

struct component *new_labeled_component(
  struct alloc_block *heap, const struct loc *loc, const char *name,
  struct component *comp)
{
  struct component *c = alloc_component(heap, loc, c_labeled);
  c->u.labeled.name = name;
  c->u.labeled.expression = comp;
  return c;
}

struct component *new_exit_component(
  struct alloc_block *heap, const struct loc *loc, const char *name,
  struct component *comp)
{
  struct component *c = alloc_component(heap, loc, c_exit);
  c->u.labeled.name = name;
  c->u.labeled.expression = comp;
  return c;
}

struct component *new_execute_component(
  struct alloc_block *heap, const struct loc *loc, struct clist *clist)
{
  struct component *c = alloc_component(heap, loc, c_execute);
  c->u.execute = clist;
  return c;
}

struct component *new_unop_component(
  struct alloc_block *heap, const struct loc *loc, enum builtin_op op,
  struct component *e)
{
  struct component *newp = alloc_component(heap, loc, c_builtin);
  newp->u.builtin.fn = op;
  newp->u.builtin.args = new_clist(heap, e, NULL);
  return newp;
}

struct pattern *new_pattern_constant(struct alloc_block *heap,
                                     struct constant *c, const struct loc *loc)
{
  struct pattern *ap = allocate(heap, sizeof *ap);
  *ap = (struct pattern){
    .vclass = pat_const,
    .loc    = *loc,
    .u.constval = c
  };
  return ap;
}

struct pattern *new_pattern_symbol(struct alloc_block *heap,
                                   struct pattern *sym,
                                   struct pattern *val, const struct loc *loc)
{
  struct pattern *ap = allocate(heap, sizeof *ap);
  *ap = (struct pattern){
    .vclass = pat_symbol,
    .loc    = *loc,
    .u.sym = {
      .name = sym,
      .val  = val
    }
  };
  return ap;
}

struct pattern *new_pattern_or(struct alloc_block *heap,
                               struct pattern *lhs,
                               struct pattern *rhs,
                               const struct loc *loc)
{
  struct pattern *ap = allocate(heap, sizeof *ap);
  *ap = (struct pattern){
    .vclass = pat_or,
    .loc    = *loc,
    .u.or = {
      .lhs = lhs,
      .rhs = rhs
    }
  };
  return ap;
}

struct pattern *new_pattern_and_expr(struct alloc_block *heap,
                                     struct pattern *p,
                                     struct component *c)
{
  struct pattern *ap = allocate(heap, sizeof *ap);
  *ap = (struct pattern){
    .vclass = pat_and,
    .loc    = c->loc,
    .u.and = {
      .pat  = p,
      .cond = c
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
    .loc    = c->loc,
    .u.expr = c
  };
  return ap;
}

struct pattern *new_pattern_sink(struct alloc_block *heap)
{
  struct pattern *ap = allocate(heap, sizeof *ap);
  *ap = (struct pattern){
    .vclass = pat_sink,
    .loc    = *NO_LOC,
  };
  return ap;
}

struct pattern *new_pattern_variable(struct alloc_block *heap, const char *sym,
                                     unsigned typeset, const struct loc *loc)
{
  struct pattern *ap = allocate(heap, sizeof *ap);
  *ap = (struct pattern){
    .vclass = pat_variable,
    .loc    = *loc,
    .u.var = {
      .name    = sym,
      .typeset = typeset
    }
  };
  return ap;
}

static bool patlist_is_const(struct pattern_list *list)
{
  for (struct pattern_list *p = list; p != NULL; p = p->next)
    if (p->pat && p->pat->vclass != pat_const)
      return false;
  return true;
}

static struct cstlist *patlist_to_cstlist(struct alloc_block *heap,
                                          struct pattern_list *list)
{
  struct cstlist *cl = NULL;
  for (struct pattern_list *p = list; p != NULL; p = p->next)
    cl = new_cstlist(heap, p->pat ? p->pat->u.constval : NULL, cl);
  return reverse_cstlist(cl);
}

struct pattern *new_array_pattern(struct alloc_block *heap,
                                  struct pattern_list *list,
                                  bool ellipsis,
                                  struct pattern_list *tail,
                                  const struct loc *loc)
{
  if (tail != NULL)
    assert(ellipsis);

  if (ellipsis || !patlist_is_const(list))
    goto not_const;

  struct cstlist *cl = patlist_to_cstlist(heap, list);
  return new_pattern_constant(heap, new_array_constant(heap, cl), loc);

 not_const: ;
  struct pattern *ap = allocate(heap, sizeof *ap);
  *ap = (struct pattern){
    .vclass = pat_array,
    .loc    = *loc,
    .u.ary = {
      .patlist  = list,
      .pattail  = tail,
      .ellipsis = ellipsis
    }
  };
  return ap;
}

struct pattern *new_list_pattern(struct alloc_block *heap,
                                 struct pattern_list *list,
                                 const struct loc *loc)
{
  if (!patlist_is_const(list))
    goto not_const;

  struct cstlist *cl = patlist_to_cstlist(heap, list);
  return new_pattern_constant(heap, new_list_constant(heap, cl), loc);

 not_const: ;
  struct pattern *ap = allocate(heap, sizeof *ap);
  *ap = (struct pattern){
    .vclass = pat_list,
    .loc    = *loc,
    .u.lst  = list,
  };
  return ap;
}

struct pattern_list *new_pattern_list(struct alloc_block *heap,
                                      struct pattern *pat,
                                      struct pattern_list *tail)
{
  struct pattern_list *apl = allocate(heap, sizeof *apl);
  *apl = (struct pattern_list){
    .next = tail,
    .pat  = pat
  };
  return apl;
}

struct match_node_list *new_match_list(struct alloc_block *heap,
                                       struct match_node *node,
                                       struct match_node_list *tail)
{
  struct match_node_list *ml = allocate(heap, sizeof *ml);
  *ml = (struct match_node_list){
    .next  = tail,
    .match = node
  };
  return ml;
}

struct match_node *new_match_node(struct alloc_block *heap,
                                  struct pattern *pat, struct component *e,
                                  const char *filename, const struct loc *loc)
{
  struct match_node *nd = allocate(heap, sizeof *nd);
  *nd = (struct match_node){
    .pattern    = pat,
    .expression = e,
    .filename   = filename,
    .loc        = *loc
  };
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
  struct vector *v = NULL;

  GCPRO(l, s, v);
  for (vars = reverse_vlist(vars); vars; vars = vars->next)
    {
      s = scache_alloc_str(vars->var);
      v = alloc_vector(3);
      SET_VECTOR(v, 0, s);
      SET_VECTOR(v, 1, makeint(vars->typeset));
      SET_VECTOR(v, 2, alloc_list(makeint(vars->loc.line),
                                  makeint(vars->loc.col)));
      l = alloc_list(v, l);
    }
  UNGCPRO();
  return l;
}

static value mudlle_clist(struct clist *exprs)
{
  value l = NULL;
  GCPRO(l);
  for (exprs = reverse_clist(exprs); exprs; exprs = exprs->next)
    {
      value c = mudlle_parse_component(exprs->c);
      l = alloc_list(c, l);
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

#define SET(n, value) SET_VECTOR(mc, 2 + (n), (value))

  GCPRO(mc);
  SET_VECTOR(mc, 1, alloc_list(makeint(c->loc.line), makeint(c->loc.col)));

  switch (c->vclass)
    {
    case c_assign:
      SET(0, scache_alloc_str(c->u.assign.symbol));
      SET(1, mudlle_parse_component(c->u.assign.value));
      break;

    case c_vref:
    case c_recall:
      SET(0, scache_alloc_str(c->u.recall));
      break;

    case c_constant:
      SET(0, make_constant(c->u.cst));
      break;

    case c_closure: {
      struct function *f = c->u.closure;
      SET(0, makeint(f->typeset));
      SET(1, (f->help.len
              ? scache_alloc_str_len(f->help.str, f->help.len)
              : NULL));
      SET(2, mudlle_vlist(reverse_vlist(f->args)));
      SET(3, makeint(f->varargs));
      SET(4, mudlle_parse_component(f->value));
      SET(5, scache_alloc_str(f->filename));
      SET(6, scache_alloc_str(f->nicename));
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
              ? scache_alloc_str(c->u.labeled.name)
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
  init_string_cache();

  struct vector *file = alloc_vector(parser_module_fields);
  GCPRO(file);

  struct component *cbody = new_block_component(heap, &f->loc, f->body);

  SET_VECTOR(file, m_class,    makeint(f->vclass));
  SET_VECTOR(file, m_name,     (f->name
                                ? scache_alloc_str(f->name)
                                : makebool(false)));
  SET_VECTOR(file, m_requires, mudlle_vlist(f->requires));
  SET_VECTOR(file, m_defines,  mudlle_vlist(f->defines));
  SET_VECTOR(file, m_reads,    mudlle_vlist(f->reads));
  SET_VECTOR(file, m_writes,   mudlle_vlist(f->writes));
  SET_VECTOR(file, m_statics,  mudlle_vlist(f->statics));
  SET_VECTOR(file, m_body,     mudlle_parse_component(cbody));
  SET_VECTOR(file, m_filename, scache_alloc_str(f->body->filename));
  SET_VECTOR(file, m_nicename, scache_alloc_str(f->body->nicename));

  UNGCPRO();

  free_string_cache();

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
    print_component(out, new_block_component(oops, &f->loc, f->body));
    free_block(oops);
  }
}

#endif

static inline struct component *set_loc(
  const struct loc *loc, struct component *c)
{
  c->loc = *loc;
  return c;
}

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
  return new_const_component(heap, NO_LOC, new_int_constant(heap, n));
}

static struct component *build_int_component(long n)
{
  return new_int_component(build_heap, n);
}

static struct component *build_string_component(const char *s) UNUSED;

static struct component *build_string_component(const char *s)
{
  return new_const_component(
    build_heap, NO_LOC,
    new_string_constant(build_heap,
                        &(const struct str_and_len){
                          .str = (char *)s, .len = strlen(s) }));
}

static struct component *build_assign(const struct loc *loc, const char *var,
                                      struct component *val)
{
  return new_assign_component(build_heap, loc, var, val);
}

static struct component *build_recall(const char *var)
{
  return new_recall_component(build_heap, NO_LOC, var);
}

static struct component *build_exec(struct component *f, int n, ...)
{
  va_list args;
  struct clist *res = new_clist(build_heap, f, NULL);

  va_start(args, n);
  while (n--)
    res = new_clist(build_heap, va_arg(args, struct component *), res);
  va_end(args);

  return new_execute_component(build_heap, NO_LOC, reverse_clist(res));
}

struct mvar {
  const char *name;
  const struct loc *loc;
};

#define MVAR(n, l) &(const struct mvar){ .name = (n), .loc = (l) }

/* args are name, typeset, line, name, typeset, line, ... */
static struct vlist *build_vlist(size_t n, ...)
{
  va_list args;
  va_start(args, n);
  struct vlist *res = NULL;
  for (size_t i = 0; i < n; ++i)
    {
      const struct mvar *mvar = va_arg(args, const struct mvar *);
      if (mvar->name == NULL)
        continue;
      res = new_vlist(build_heap, mvar->name, TYPESET_ANY, mvar->loc, res);
    }
  va_end(args);
  return res;
}

struct component *new_binop_component(struct alloc_block *heap,
                                      const struct loc *loc,
                                      enum builtin_op op,
                                      struct component *e1,
                                      struct component *e2)
{
  if (op == b_xor)
    {
      op = b_bitxor;
      e1 = new_unop_component(heap, loc, b_not, e1);
      e2 = new_unop_component(heap, loc, b_not, e2);
    }

  assert(op >= 0 && op < last_builtin);

  struct component *newp = alloc_component(heap, loc, c_builtin);
  newp->u.builtin.fn = op;
  newp->u.builtin.args = new_clist(heap, e1, new_clist(heap, e2, NULL));
  return newp;
}

struct component *new_ternop_component(struct alloc_block *heap,
                                       const struct loc *loc,
                                       enum builtin_op op,
                                       struct component *e1,
                                       struct component *e2,
                                       struct component *e3)
{
  struct component *newp = alloc_component(heap, loc, c_builtin);
  newp->u.builtin.fn = op;
  newp->u.builtin.args = new_clist(
    heap, e1, new_clist(heap, e2, new_clist(heap, e3, NULL)));
  return newp;
}

static struct component *build_unop(enum builtin_op op, struct component *e)
{
  return new_unop_component(build_heap, NO_LOC, op, e);
}

static struct component *build_if(struct component *cond,
                                  struct component *ctrue,
                                  struct component *cfalse)
{
  if (cfalse == NULL)
    return new_binop_component(build_heap, NO_LOC, b_if, cond, ctrue);

  return new_ternop_component(build_heap, NO_LOC, b_ifelse,
                              cond, ctrue, cfalse);
}

static struct component *build_unless(struct component *cond,
                                      struct component *otherwise)
{
  return build_if(build_unop(b_not, cond), otherwise, NULL);
}

static struct component *build_exit(const char *name, struct component *c)
{
  return new_exit_component(build_heap, NO_LOC, name, c);
}

static struct component *build_binop(enum builtin_op op, struct component *e1,
                                     struct component *e2)
{
  return new_binop_component(build_heap, NO_LOC, op, e1, e2);
}

static struct component *build_codeblock(struct vlist *vl, struct clist *code)
{
  return new_block_component(
    build_heap, NO_LOC,
    new_codeblock(build_heap, vl, code, NULL, NULL, NO_LOC));
}

static struct component *build_constant(struct constant *cst)
{
  return new_const_component(build_heap, NO_LOC, cst);
}

static struct component *build_const_not_equal(const struct loc *loc,
                                               struct constant *cst,
                                               struct component *e)
{
  switch (cst->vclass)
    {
    case cst_int:
    simple:
      return set_loc(loc, build_binop(b_ne, e, build_constant(cst)));
    case cst_list:
      if (cst->u.constants == NULL)
        goto simple;
      /* fallthrough */
    default:
      return set_loc(
        loc,
        build_unop(b_not,
                   set_loc(loc,
                           build_exec(build_recall(GEP "equal?"),
                                      2, build_constant(cst), e))));
    }
}

static struct component *build_typecheck(const struct loc *loc,
                                         struct component *e,
                                         enum mudlle_type type)
{
  const char *f;

  switch (type)
    {
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
        build_constant(constant_null));
    case stype_none:
      return component_false;
    case stype_any:
      return component_true;
    default:
      abort();
    }
  return set_loc(loc, build_exec(build_recall(f), 1, e));
}

static struct component *build_error(const struct loc *loc,
                                     enum runtime_error error)
{
  return set_loc(loc, build_exec(build_recall(GEP "error"), 1,
                                 build_int_component(error)));
}

/* true if 'c' is a recall of a single-assignment variable */
static bool is_safe_recall(struct component *c)
{
  return c->vclass == c_recall && c->u.recall[0] == '$';
}

static struct component *make_local_var(struct vlist **locals, int level,
                                        const struct loc *loc)
{
  char buf[16];
  sprintf(buf, "$%d", level);
  const char *tmpname = heap_allocate_string(build_heap, buf);
  *locals = new_vlist(build_heap, tmpname, TYPESET_ANY, loc, *locals);
  return build_recall(tmpname);
}

static struct component *make_safe_copy(struct component *e,
                                        struct vlist **locals,
                                        struct component **aexpr,
                                        int level, const struct loc *loc)
{
  if (is_safe_recall(e))
    return e;

  struct component *recall = make_local_var(locals, level, loc);
  assert(recall->vclass == c_recall);
  *aexpr = build_assign(loc, recall->u.recall, e);
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
    case pat_and:
    case pat_or:
    case pat_variable:
      return build_match_block(pat, e, level, psymbols, err, err_data);
    case pat_expr:
      {
        struct vlist *locals = NULL;
        struct component *lassign = NULL;
        struct component *nexp = make_safe_copy(pat->u.expr, &locals, &lassign,
                                                level, &pat->loc);
        return build_codeblock(
          locals,
          build_clist(
            2,
            lassign,
            build_unless(
              build_binop(
                b_sc_and,
                build_typecheck(&pat->loc, nexp, type_string),
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
      compile_error(&pat->loc, "symbol names must be strings");
      return NULL;
    }
  abort();
}

static int patlist_len(struct pattern_list *l)
{
  int n = 0;
  for (; l != NULL; l = l->next)
    ++n;
  return n;
}

static struct clist *build_patarray_match(
  struct clist *code, struct component *elocal, struct vlist **psymbols,
  struct pattern_list *l, int idx, int level,
  struct component *(*err)(void *data), void *err_data)
{
  for (; l != NULL; l = l->next)
    {
      --idx;
      struct component *c = build_match_block(
        l->pat, build_binop(b_ref, elocal, build_int_component(idx)),
        level + 1, psymbols, err, err_data);
      if (c != NULL)
        code = new_clist(build_heap, c, code);
    }
  return code;
}

static struct component *build_match_array_block(
  struct pattern *pat, struct component *e, int level, struct vlist **psymbols,
  struct component *(*err)(void *data), void *err_data)
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
                                            &pat->loc);

  int vlen = patlist_len(pat->u.ary.patlist);
  int tlen = patlist_len(pat->u.ary.pattail);

  struct component *lc = NULL;
  if (!pat->u.ary.ellipsis || (vlen + tlen) > 0)
    lc = set_loc(
      &pat->loc,
      build_if(
        build_binop(
          pat->u.ary.ellipsis ? b_lt : b_ne,
          build_exec(build_recall(GEP "vector_length"), 1, elocal),
          build_int_component(vlen + tlen)),
        err(err_data),
        NULL));

  struct component *tc = set_loc(
    &pat->loc,
    build_unless(
      build_exec(build_recall(GEP "vector?"), 1, elocal),
      err(err_data)));

  struct clist *code = build_patarray_match(
    NULL, elocal, psymbols, pat->u.ary.pattail, 0, level, err, err_data);
  code = build_patarray_match(
    code, elocal, psymbols, pat->u.ary.patlist, vlen, level, err,
    err_data);

  if (lc != NULL)
    code = new_clist(build_heap, lc, code);
  code = new_clist(build_heap, tc, code);
  if (eassign != NULL)
    code = new_clist(build_heap, eassign, code);

  return build_codeblock(vlocals, code);
}

static struct component *build_match_list_block(
  struct pattern *pat, struct component *e, int level, struct vlist **psymbols,
  struct component *(*err)(void *data), void *err_data)
{
  if (pat->u.lst == NULL)
    return set_loc(&pat->loc,
                   build_if(build_const_not_equal(&pat->loc, constant_null, e),
                            err(err_data), NULL));

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

  /* the last pair has both car/cdr entries */
  for (struct pattern_list *apl = pat->u.lst->next; apl; apl = apl->next)
    ++level;
  int nlevel = level;

  struct clist *code = NULL;
  struct vlist *locals = NULL;
  struct component *exp = NULL;
  for (struct pattern_list *apl = pat->u.lst->next; ; )
    {
      struct component *texp = (apl->next == NULL && is_safe_recall(e)
                                ? e
                                : make_local_var(&locals, --level,
                                                 &pat->loc));

      if (exp == NULL)
        {
          struct component *getcdr = build_exec(build_recall(GEP "cdr"), 1,
                                                texp);
          struct component *c =
            (pat->u.lst->pat == NULL
             ? build_if(build_const_not_equal(&pat->loc, constant_null,
                                              getcdr),
                        err(err_data), NULL)
             : build_match_block(pat->u.lst->pat, getcdr,
                                 nlevel, psymbols, err, err_data));
          if (c)
            code = new_clist(build_heap, c, code);
        }
      else
        {
          code = new_clist(
            build_heap,
            build_assign(NO_LOC, exp->u.recall,
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

      struct component *pc = set_loc(
        &apl->pat->loc,
        build_unless(
          build_exec(build_recall(GEP "pair?"), 1, exp),
          err(err_data)));
      code = new_clist(build_heap, pc, code);

      apl = apl->next;
      if (apl == NULL)
        break;
    }

  if (!is_safe_recall(e))
    code = new_clist(build_heap, build_assign(NO_LOC, exp->u.recall, e),
                     code);

  return build_codeblock(reverse_vlist(locals), code);
}

static struct component *build_match_variable_block(
  struct pattern *pat, struct component *e, int level, struct vlist **psymbols,
  struct component *(*err)(void *data), void *err_data)
{
  assert(pat->u.var.typeset == TYPESET_ANY);

  if (vlist_find(*psymbols, pat->u.var.name))
    {
      compile_error(&pat->loc,
                    "repeated variable name in match pattern (%s)",
                    pat->u.var.name);
      return NULL;
    }

  if (!is_global_var(pat->u.var.name))
    *psymbols = new_vlist(
      build_heap, pat->u.var.name, TYPESET_ANY, &pat->loc,
      *psymbols);

  return build_assign(&pat->loc, pat->u.var.name, e);
}

static struct component *build_match_symbol_block(
  struct pattern *pat, struct component *e, int level, struct vlist **psymbols,
  struct component *(*err)(void *data), void *err_data)
{
  struct vlist *locals = NULL;
  struct component *aexp = NULL;
  e = make_safe_copy(e, &locals, &aexp, level, &pat->loc);
  return build_codeblock(
    locals,
    build_clist(
      4,
      aexp,
      build_unless(build_typecheck(&pat->loc, e, type_symbol),
                   err(err_data)),
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
}

static struct component *next_error(void *count)
{
  ++*(int *)count;
  return build_exit("$next", component_undefined);
}

static struct component *build_match_or_block(
  struct pattern *pat, struct component *e, int level, struct vlist **psymbols,
  struct component *(*err)(void *data), void *err_data)
{
  struct vlist *locals = NULL;
  struct component *aexp = NULL;
  e = make_safe_copy(e, &locals, &aexp, level, &pat->loc);

  struct vlist *orig_psyms = *psymbols;

  int lhs_count = 0;
  struct component *lhs_comp = build_match_block(
    pat->u.or.lhs, e, level + 1,
    psymbols, next_error, &lhs_count);

  struct component *rhs_comp = NULL;
  if (lhs_count == 0)
    {
      /* lhs always succeeds */
      compile_warning(&pat->u.or.rhs->loc, "pattern not reachable");
    }
  else
    {
      /* temporarily restore used symbols to allow the same symbol being
         defined in lhs || rhs */
      struct vlist *lhs_psyms = *psymbols;
      *psymbols = orig_psyms;

      rhs_comp = build_match_block(
        pat->u.or.rhs, e, level + 1,
        psymbols, err, err_data);

      struct vlist *pl = *psymbols;
      *psymbols = lhs_psyms;

      for (struct vlist *next; pl != orig_psyms; pl = next)
        {
          next = pl->next;
          if (!vlist_find(*psymbols, pl->var))
            {
              pl->next = *psymbols;
              *psymbols = pl;
            }
          pl = next;
        }
    }

  return new_labeled_component(
    build_heap, &pat->loc,
    "$found", build_codeblock(
      locals,
      build_clist(
        3,
        aexp,
        new_labeled_component(
          build_heap, &pat->loc,
          "$next", build_codeblock(
            NULL,
            build_clist(
              2,
              lhs_comp,
              build_exit("$found", build_constant(constant_null))))),
        rhs_comp)));
}

static struct component *build_match_expr_block(
  struct pattern *pat, struct component *e, int level, struct vlist **psymbols,
  struct component *(*err)(void *data), void *err_data)
{
  return set_loc(
    &pat->loc,
    build_unless(set_loc(&pat->loc,
                         build_exec(build_recall(GEP "equal?"),
                                    2, pat->u.expr, e)),
                 err(err_data)));
}

static struct component *build_match_and_block(
  struct pattern *pat, struct component *e, int level, struct vlist **psymbols,
  struct component *(*err)(void *data), void *err_data)
{
  return build_codeblock(
    NULL,
    build_clist(
      2,
      build_match_block(pat->u.and.pat, e, level,
                        psymbols, err, err_data),
      build_unless(pat->u.and.cond, err(err_data))));
}

static struct component *build_match_const_block(
  struct pattern *pat, struct component *e, int level, struct vlist **psymbols,
  struct component *(*err)(void *data), void *err_data)
{
  return set_loc(
    &pat->loc,
    build_if(build_const_not_equal(&pat->loc, pat->u.constval, e),
             err(err_data), NULL));
}

static struct component *build_match_sink_block(
  struct pattern *pat, struct component *e, int level, struct vlist **psymbols,
  struct component *(*err)(void *data), void *err_data)
{
  return NULL;
}

typedef struct component *(*build_match_block_fn)(
  struct pattern *pat, struct component *e, int level, struct vlist **psymbols,
  struct component *(*err)(void *data), void *err_data);

static struct component *build_match_block(
  struct pattern *pat, struct component *e, int level, struct vlist **psymbols,
  struct component *(*err)(void *data), void *err_data)
{
  static const build_match_block_fn fns[] = {
    [pat_const]    = &build_match_const_block,
    [pat_list]     = &build_match_list_block,
    [pat_array]    = &build_match_array_block,
    [pat_symbol]   = &build_match_symbol_block,
    [pat_variable] = &build_match_variable_block,
    [pat_sink]     = &build_match_sink_block,
    [pat_expr]     = &build_match_expr_block,
    [pat_and]      = &build_match_and_block,
    [pat_or]       = &build_match_or_block,
  };
  return fns[pat->vclass](pat, e, level, psymbols, err, err_data);
}

static enum mudlle_type constant_type(struct constant *c)
{
  switch (c->vclass)
    {
    case cst_string:     return type_string;
    case cst_list:       return c->u.constants == NULL ? type_null : type_pair;
    case cst_array:      return type_vector;
    case cst_int:        return type_integer;
    case cst_float:      return type_float;
    case cst_bigint:     return type_bigint;
    case cst_ctable:     return type_table;
    case cst_table:      return type_table;
    case cst_symbol:     return type_symbol;
    case cst_expression: return stype_any;
    }
  abort();
}

static unsigned match_block_typeset(struct pattern *pat)
{
  switch (pat->vclass)
    {
    case pat_const:
      return P(constant_type(pat->u.constval));
    case pat_list:
      return pat->u.lst == NULL ? P(type_null) : P(type_pair);
    case pat_array:
      return P(type_vector);
    case pat_symbol:
      return P(type_symbol);
    case pat_and:
      return match_block_typeset(pat->u.and.pat);
    case pat_or:
      return (match_block_typeset(pat->u.or.lhs)
              | match_block_typeset(pat->u.or.rhs));
    case pat_variable:
    case pat_sink:
    case pat_expr:
      return TYPESET_ANY;
    }
  abort();
}

static struct component *no_match_error(void *data)
{
  const struct loc *loc = data;
  return build_error(loc, error_no_match);
}

struct component *new_pattern_component(struct alloc_block *heap,
                                        struct pattern *pat,
                                        struct component *e)
{
  struct vlist *psymbols = NULL;

  /* Warning: if the match fails, this might leave only some of the variables
   * in the pattern filled. But it's a feature, right? */
  PUSH_BUILD_HEAP(heap);
  struct component *c = build_codeblock(
    NULL,
    build_clist(
      2,
      build_match_block(pat, e, 0, &psymbols,
                        no_match_error, &pat->loc),
      component_undefined));
  POP_BUILD_HEAP();

  return c;
}

struct component *new_dereference(
  struct alloc_block *heap, const struct loc *loc, struct component *e)
{
  PUSH_BUILD_HEAP(heap);
  struct component *c = set_loc(
    loc, build_exec(build_recall(GEP "dereference"), 1, e));
  POP_BUILD_HEAP();
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

struct component *new_reference(struct alloc_block *heap,
                                const struct loc *loc,
                                struct component *e)
{
  if (e->vclass == c_recall)
    return new_vref_component(heap, loc, e->u.recall);

  struct component *result = NULL;

  if (e->vclass == c_builtin && e->u.builtin.fn == b_ref)
    {
      struct clist *args = e->u.builtin.args;
      PUSH_BUILD_HEAP(heap);
      result = build_exec(build_recall(GEP "make_ref"), 2,
                          args->c, args->next->c);
      POP_BUILD_HEAP();
      return result;
    }

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
      if (is_global_var(name))
        name += strlen(GEP);

      if (strcasecmp(name, "symbol_get") == 0)
        {
          PUSH_BUILD_HEAP(heap);
          result = build_exec(build_recall(GEP "make_symbol_ref"),
                              1, args->c);
          POP_BUILD_HEAP();
          return result;
        }

      int idx;
      if (strcasecmp(name, "car") == 0)
        idx = 0;
      else if (strcasecmp(name, "cdr") == 0)
        idx = 1;
      else
        goto error;
      PUSH_BUILD_HEAP(heap);
      result = build_exec(build_recall(GEP "make_pair_ref"),
                          2, args->c, build_int_component(idx));
      POP_BUILD_HEAP();
      return result;
    }

 error:
  compile_error(loc, "cannot create a reference to that");
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
                                    const char *filename,
                                    const struct loc *loc)
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

  PUSH_BUILD_HEAP(heap);

  if (eloop)
    code = new_clist(build_heap, eloop, code);

  code = new_clist(build_heap,
                   new_labeled_component(build_heap, &e->loc,
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
  c = new_unop_component(build_heap, loc, b_loop, c);

  code = new_clist(build_heap, c, NULL);
  if (einit)
    code = new_clist(build_heap, einit, code);

  c = build_codeblock(vars, code);
  c->u.blk->filename = filename;
  c->u.blk->loc = *loc;

  POP_BUILD_HEAP();

  return new_labeled_component(heap, &c->loc, "break", c);
}

struct component *new_match_component(struct alloc_block *heap,
                                      struct component *e,
                                      struct match_node_list *matches)
{
  /*
   *   <$match> [
   *     | $exp |
   *
   *     $exp = <match-expression>
   *     <$next> [                               \
   *       | <pattern-variables> |                |
   *       if (<pattern-match>)                   +- repeat for each match node
   *         exit<$match> <pattern-expression>;   |
   *     ]                                       /
   *     false
   *   ]
   */

  PUSH_BUILD_HEAP(heap);

  struct vlist *vl = build_vlist(1, MVAR("$exp", &e->loc));
  struct clist *code = build_clist(1, component_false);
  for (; matches; matches = matches->next)
    {
      struct vlist *psymbols = NULL;
      int next_count = 0;
      struct component *matchcode = build_match_block(
        matches->match->pattern, build_recall("$exp"), 0,
        &psymbols, next_error, &next_count);
      struct component *cexit = set_loc(
        &matches->match->loc,
        build_exit("$match", matches->match->expression));

      struct clist *cl = build_clist(2, matchcode, cexit);

      struct component *cblock = build_codeblock(reverse_vlist(psymbols), cl);
      cblock->u.blk->filename = matches->match->filename;
      cblock->u.blk->loc = matches->match->loc;

      if (next_count)
        cblock = new_labeled_component(build_heap, NO_LOC, "$next", cblock);

      code = new_clist(build_heap, cblock, code);
    }

  code = new_clist(build_heap, build_assign(NO_LOC, "$exp", e), code);

  struct component *result = new_labeled_component(
    build_heap, NO_LOC, "$match", build_codeblock(vl, code));
  POP_BUILD_HEAP();
  return result;
}

static struct component *comp_id(struct component *e)
{
  return e;
}

static struct component *comp_set_tmp(struct component *e)
{
  return build_assign(NO_LOC, "$tmp", e);
}

/* for +=, |=, ++, etc. */
struct component *new_assign_modify_expr(struct alloc_block *heap,
                                         struct component *e0,
                                         enum builtin_op op,
                                         struct component *e1, bool postfix,
                                         bool assignment,
                                         const struct loc *loc)
{
  struct component *result = NULL;

  PUSH_BUILD_HEAP(heap);

  struct component *(*set_tmp)(struct component *)
    = postfix ? comp_set_tmp : comp_id;
  struct component *ret = postfix ? build_recall("$tmp") : NULL;
  const char *tmp_name = postfix ? "$tmp" : NULL;

  if (e0->vclass == c_builtin && e0->u.builtin.fn == b_ref)
    {
      struct clist *args = e0->u.builtin.args;
      struct component *lexp = args->c, *lref = args->next->c;
      struct vlist *vl;
      struct clist *cl;
      if (lref->vclass == c_constant && lref->u.cst->vclass == cst_string)
        {
          vl = build_vlist(
            2,
            MVAR(tmp_name, &e0->loc),
            MVAR("$sym", &lref->loc));
          cl = build_clist(
            3,
            build_assign(loc, "$sym",
                         build_exec(build_recall(GEP "symbol_ref"), 2,
                                    lexp, lref)),
            set_loc(
              loc,
              build_exec(
                build_recall(GEP "symbol_set!"), 2,
                build_recall("$sym"),
                build_binop(op,
                            set_tmp(build_exec(
                                      build_recall(GEP "symbol_get"), 1,
                                      build_recall("$sym"))),
                            e1))),
            ret);
        }
      else
        {
          vl = build_vlist(
            3,
            MVAR(tmp_name, &e0->loc),
            MVAR("$ref", &lref->loc),
            MVAR("$exp", &lexp->loc));
          cl = build_clist(
            4,
            build_assign(loc, "$exp", lexp),
            build_assign(loc, "$ref", lref),
            new_ternop_component(
              heap, loc, b_set,
              build_recall("$exp"),
              build_recall("$ref"),
              build_binop(op,
                          set_tmp(build_binop(b_ref,
                                              build_recall("$exp"),
                                              build_recall("$ref"))),
                          e1)),
            ret);
        }
      result = build_codeblock(vl, cl);
      goto done;
    }

  if (e0->vclass == c_recall)
    {
      struct vlist *vl = build_vlist(
        1, MVAR(tmp_name, &e0->loc));
      struct clist *cl = build_clist(
        2,
        build_assign(loc, e0->u.recall, build_binop(op, set_tmp(e0), e1)),
        ret);
      /* optimize for common case */
      if (vl == NULL && cl->next == NULL)
        result = cl->c;
      else
        result = build_codeblock(vl, cl);
      goto done;
    }

  struct component *r = is_dereference(e0);
  if (r == NULL)
    {
      compile_error(&e0->loc,
                    "can only %s variables or references",
                    (assignment
                     ? "assign to"
                     : (op == b_add ? "increment" : "decrement")));
      goto done;
    }

  struct vlist *vl = build_vlist(
    2,
    MVAR(tmp_name, &e0->loc),
    MVAR("$ref", &e0->loc));
  struct clist *cl = build_clist(
    3,
    build_assign(loc, "$ref", r),
    build_set_ref(
      build_recall("$ref"),
      build_binop(op,
                  set_tmp(new_dereference(heap, loc,
                                          build_recall("$ref"))),
                  e1)),
    ret);
  result = build_codeblock(vl, cl);

 done:
  POP_BUILD_HEAP();
  return result;
}

struct component *new_assign_expression(struct alloc_block *heap,
                                        struct component *e0,
                                        struct component *e1,
                                        const struct loc *loc,
                                        const struct loc *dstloc)
{
  if (e0->vclass == c_builtin && e0->u.builtin.fn == b_ref)
    {
      struct clist *args = e0->u.builtin.args;
      return new_ternop_component(heap, loc, b_set,
                                  args->c, args->next->c, e1);
    }

  PUSH_BUILD_HEAP(heap);

  struct component *result = NULL;

  if (e0->vclass == c_recall)
    {
      if (e1->vclass == c_closure)
        e1->loc = *dstloc;
      result = build_assign(loc, e0->u.recall, e1);
    }
  else
    {
      struct component *r = is_dereference(e0);
      if (r == NULL)
        compile_error(&e0->loc, "can only assign to variables or references");
      else
        result = build_set_ref(r, e1);
    }

  POP_BUILD_HEAP();

  return result;
}

bool set_function_args(struct alloc_block *heap,
                       struct function *func,
                       struct pattern_list *args)
{
  PUSH_BUILD_HEAP(heap);

  bool result = false;

  /* dummy list head for function argument names */
  struct vlist *arglocals = new_vlist(heap, "$arglocals", TYPESET_ANY,
                                      NO_LOC, NULL);
  struct vlist *locals = arglocals;

  struct clist *clist = NULL;
  int argn = 0;
  for (struct pattern_list *arg = args; arg; arg = arg->next, ++argn)
    {
      struct pattern *pat = arg->pat;
      if (pat->vclass == pat_variable
          && !is_global_var(pat->u.var.name))
        {
          if (vlist_find(locals, pat->u.var.name))
            {
              compile_error(&pat->loc, "duplicate argument name %s",
                            pat->u.var.name);
              goto done;
            }
          arglocals->next = new_vlist(heap, pat->u.var.name,
                                      pat->u.var.typeset,
                                      &pat->loc,
                                      arglocals->next);
          func->args = new_vlist(heap, pat->u.var.name, pat->u.var.typeset,
                                 &pat->loc, func->args);
          continue;
        }

      char buf[16];
      sprintf(buf, "$arg%d", argn);
      const char *argname = heap_allocate_string(heap, buf);

      func->args = new_vlist(heap, argname, match_block_typeset(pat),
                             &pat->loc, func->args);
      struct component *c = build_match_block(
        pat, build_recall(argname), 0, &locals, no_match_error,
        &pat->loc);
      if (erred)
        goto done;

      if (c != NULL)
        clist = new_clist(heap, c, clist);
    }

  if (locals != arglocals || clist != NULL)
    {
      for (struct vlist **l = &locals; ; l = &(*l)->next)
        if (*l == arglocals)
          {
            *l = NULL;
            break;
          }

      clist = new_clist(heap, func->value, clist);
      clist = reverse_clist(clist);
      func->value = build_codeblock(locals, clist);
    }
  result = true;

 done:
  POP_BUILD_HEAP();
  return result;
}

struct block *new_toplevel_codeblock(struct alloc_block *heap,
                                     struct vlist *statics,
                                     struct block *body)
{
  if (statics == NULL)
    return body;

  PUSH_BUILD_HEAP(heap);
  struct clist *cl = build_clist(
    1, new_block_component(heap, &body->loc, body));
  POP_BUILD_HEAP();

  body = new_codeblock(heap, statics, cl, body->filename, body->nicename,
                       &body->loc);
  body->statics = true;
  return body;
}

void str_and_len_dup(struct str_and_len *dest, const struct str_and_len *src)
{
  dest->len = src->len;
  dest->str = xmalloc(src->len);
  memcpy(dest->str, src->str, src->len);
}
