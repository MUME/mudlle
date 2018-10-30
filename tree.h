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

#ifndef TREE_H
#define TREE_H

#include <stdio.h>

#include "types.h"
#include "utils.h"

extern struct alloc_block *parser_memory;

#define NO_LOC &(const struct loc){ .line = -1, .col = -1 }

#define LINE_LOC(l) &(const struct loc){ .line = (l), .col = -1 }

#define LLOC_LOC(lloc) &(const struct loc){     \
  .line = (lloc).first_line,                    \
  .col = (lloc).first_column                    \
}

struct loc {
  int line, col;
};

struct str_and_len {
  size_t len;
  char *str;			/* not NUL-terminated */
};

struct vlist {
  struct vlist *next;
  const char *var;
  unsigned typeset;
  bool was_read, was_written;
  struct loc loc;
};

struct clist {
  struct clist *next;
  struct component *c;
};

struct cstlist {
  struct cstlist *next;
  struct constant *cst;
};

struct cstpair {
  struct constant *cst1, *cst2;
};

struct function {
  unsigned typeset;		/* Return type(s) */
  struct str_and_len help;
  struct vlist *args;           /* Stored in reverse order! */
  bool varargs;			/* true if accepts arg vector;
                                   implies vlength(vargs) == 1 */
  struct component *value;
  struct loc loc;               /* Location of "fn" keyword */
  const char *filename;         /* Name on disk */
  const char *nicename;         /* Pretty-printed file name */
  const char *varname;		/* Name of variable in which function is
                                   stored */
};

struct block {
  struct vlist *locals;
  struct clist *sequence;
  const char *filename;
  const char *nicename;
  struct loc loc;
  bool statics;                 /* true if 'locals' are in fact statics */
};

enum constant_class {
  cst_int, cst_string, cst_list, cst_array, cst_float, cst_bigint, cst_table,
  cst_ctable, cst_symbol, cst_expression
};

struct bigint_const {
  int base;
  bool neg;
  char str[];
};

struct constant {
  enum constant_class vclass;
  union {
    long integer;
    struct str_and_len string;
    double mudlle_float;
    struct bigint_const *bigint;
    struct cstlist *constants;  /* stored in reverse order; tail element
                                   is first for vclass cst_list */
    struct cstpair *constpair;
    struct component *expression; /* not an actual constant at all */
  } u;
};

struct pattern_list {
  struct pattern_list *next;
  struct pattern *pat;
};

enum pattern_class {
  pat_const, pat_list, pat_array, pat_symbol, pat_variable, pat_sink, pat_expr,
  pat_and, pat_or
};

struct pattern {
  enum pattern_class vclass;
  struct loc loc;
  union {
    struct constant *constval;
    struct component *expr;
    struct {
      struct pattern_list *patlist, *pattail;
      bool ellipsis;
    } ary;
    struct pattern_list *lst;
    struct {
      const char *name;
      unsigned typeset;
    } var;
    struct {
      struct pattern *name;
      struct pattern *val;
    } sym;
    struct {
      struct pattern *lhs;
      struct pattern *rhs;
    } or;
    struct {
      struct pattern *pat;
      struct component *cond;
    } and;
  } u;
};

struct match_node {
  const char *filename;
  struct loc loc;
  struct pattern *pattern;
  struct component *expression;
};

struct match_node_list {
  struct match_node_list *next;
  struct match_node *match;
};

enum builtin_op {
  b_invalid = -1,
  b_sc_or, b_sc_and, b_eq, b_ne, b_lt, b_ge, b_le, b_gt,
  b_bitor, b_bitxor, b_bitand, b_shift_left, b_shift_right,
  b_add, b_subtract, b_multiply, b_divide, b_remainder, b_negate,
  b_not, b_bitnot, b_ifelse, b_if, b_while, b_loop, b_ref, b_set,
  b_cons, last_builtin,
  /* these are only used by the parser and compiler */
  b_xor,
  very_last_builtin
};

extern const char *const builtin_op_names[];

#define FOR_COMPONENT_CLASSES(op)                               \
  op(assign) op(recall) op(constant) op(closure) op(execute)    \
  op(builtin) op(block) op(labeled) op(exit) op(vref)

enum component_class {
#define __CDEF(name) c_ ## name,
  FOR_COMPONENT_CLASSES(__CDEF)
#undef __CDEF
  component_classes
};

#define FOR_PARSER_MODULE_FIELDS(op)                            \
  op(class) op(name) op(requires) op(defines) op(reads)         \
  op(writes) op(statics) op(body) op(filename) op(nicename)

enum parser_module_field {
#define __MDEF(name) m_ ## name,
  FOR_PARSER_MODULE_FIELDS(__MDEF)
#undef __MDEF
  parser_module_fields
};

struct component {
  enum component_class vclass;
  struct loc loc;
  union {
    struct {
      const char *symbol;
      struct component *value;
    } assign;
    const char *recall;
    struct constant *cst;
    struct function *closure;
    struct clist *execute;		/* 1st element is fn, rest are args */
    struct {
      enum builtin_op fn;
      struct clist *args;
    } builtin;
    struct block *blk;
    struct {
      const char *name;
      struct component *expression;
    } labeled; /* also for exit */
  } u;
};

/* these are manually exported to mudlle from support.c */
enum file_class { f_plain, f_module, f_library };

struct mfile {
  enum file_class vclass;
  const char *name;
  struct vlist *requires, *defines, *reads, *writes, *statics;
  struct block *body;
  struct loc loc;
};

struct mfile *new_file(struct alloc_block *heap, enum file_class vclass,
                       const char *name,
                       struct vlist *requires, struct vlist *defines,
                       struct vlist *reads, struct vlist *writes,
                       struct vlist *statics, struct block *body,
                       const struct loc *loc);
struct function *new_fn(struct alloc_block *heap, struct component *avalue,
                        const struct loc *loc, const char *filename,
                        const char *nicename);
struct block *new_codeblock(struct alloc_block *heap, struct vlist *locals,
                            struct clist *sequence, const char *filename,
                            const char *nicename, const struct loc *loc);
struct block *new_toplevel_codeblock(struct alloc_block *heap,
                                     struct vlist *statics,
                                     struct block *body);
struct clist *new_clist(struct alloc_block *heap, struct component *c,
                        struct clist *next);
struct cstpair *new_cstpair(struct alloc_block *heap, struct constant *cst1,
                    struct constant *cst2);
struct cstlist *new_cstlist(struct alloc_block *heap, struct constant *cst,
                            struct cstlist *next);
bool cstlist_find_symbol_clash(struct cstlist *list, bool ctable,
                               struct str_and_len **s0,
                               struct str_and_len **s1);
bool cstlist_has_len(struct cstlist *list, ulong len);
struct vlist *new_vlist(struct alloc_block *heap, const char *var,
                        unsigned typeset, const struct loc *loc,
                        struct vlist *next);
struct vlist *vlist_find(struct vlist *l, const char *s);
unsigned vlist_length(struct vlist *l);

struct constant *new_expr_constant(struct alloc_block *heap,
                                   struct component *e);
struct constant *new_int_constant(struct alloc_block *heap, long l);
struct constant *new_string_constant(struct alloc_block *heap,
                                     const struct str_and_len *str);
struct constant *new_symbol_constant(struct alloc_block *heap,
                                     struct cstpair *pair);
struct constant *new_float_constant(struct alloc_block *heap, double d);
struct constant *new_bigint_constant(struct alloc_block *heap,
                                     struct bigint_const *bi);
struct constant *new_list_constant(struct alloc_block *heap,
                                   struct cstlist *lst);
struct constant *new_array_constant(struct alloc_block *heap,
                                    struct cstlist *lst);
struct constant *new_table_constant(struct alloc_block *heap,
                                    enum constant_class vclass,
                                    struct cstlist *lst);
struct component *new_const_component(struct alloc_block *heap,
                                      const struct loc *loc,
                                      struct constant *cst);
struct component *new_recall_component(struct alloc_block *heap,
                                       const struct loc *loc,
                                       const char *sym);
struct component *new_closure_component(
  struct alloc_block *heap, const struct loc *loc, struct function *f);
struct component *new_block_component(
  struct alloc_block *heap, const struct loc *loc, struct block *blk);
struct component *new_execute_component(
  struct alloc_block *heap, const struct loc *loc, struct clist *clist);

struct component *new_labeled_component(
  struct alloc_block *heap, const struct loc *loc, const char *name,
  struct component *comp);
struct component *new_exit_component(
  struct alloc_block *heap, const struct loc *loc, const char *name,
  struct component *comp);

struct component *new_unop_component(
  struct alloc_block *heap, const struct loc *loc, enum builtin_op op,
  struct component *e);
struct component *new_binop_component(
  struct alloc_block *heap, const struct loc *loc, enum builtin_op op,
  struct component *e1, struct component *e2);
struct component *new_ternop_component(
  struct alloc_block *heap, const struct loc *loc, enum builtin_op op,
  struct component *e1, struct component *e2, struct component *e3);

struct component *new_int_component(struct alloc_block *heap, long n);

struct component *new_pattern_component(struct alloc_block *heap,
                                        struct pattern *pat,
                                        struct component *e);
struct pattern *new_pattern_or(struct alloc_block *heap,
                               struct pattern *lhs,
                               struct pattern *rhs,
                               const struct loc *loc);
struct pattern *new_pattern_constant(struct alloc_block *heap,
                                     struct constant *c,
                                     const struct loc *loc);
struct pattern *new_pattern_expression(struct alloc_block *heap,
                                       struct component *c);
struct pattern *new_pattern_and_expr(struct alloc_block *heap,
                                     struct pattern *p,
                                     struct component *c);
struct pattern *new_pattern_variable(struct alloc_block *heap, const char *sym,
                                     unsigned typeset, const struct loc *loc);
struct pattern *new_pattern_symbol(struct alloc_block *heap,
                                   struct pattern *name, struct pattern *val,
                                   const struct loc *loc);
struct pattern *new_array_pattern(struct alloc_block *heap,
                                  struct pattern_list *list,
                                  bool ellipsis,
                                  struct pattern_list *tail,
                                  const struct loc *loc);
struct pattern *new_list_pattern(struct alloc_block *heap,
                                 struct pattern_list *list,
                                 const struct loc *loc);
struct pattern *new_pattern_sink(struct alloc_block *heap);
struct pattern_list *new_pattern_list(struct alloc_block *heap,
                                      struct pattern *pat,
                                      struct pattern_list *tail);

struct component *new_match_component(struct alloc_block *heap,
                                      struct component *e,
                                      struct match_node_list *matches);
struct component *new_for_component(struct alloc_block *heap,
                                    struct vlist *vars,
                                    struct component *einit,
                                    struct component *eexit,
                                    struct component *eloop,
                                    struct component *e,
                                    const char *filename,
                                    const struct loc *loc);
struct match_node_list *new_match_list(struct alloc_block *heap,
                                       struct match_node *node,
                                       struct match_node_list *tail);
struct match_node *new_match_node(struct alloc_block *heap,
                                  struct pattern *pat, struct component *e,
                                  const char *filename, const struct loc *loc);

struct component *new_reference(struct alloc_block *heap,
                                const struct loc *loc,
                                struct component *e);
struct component *new_dereference(struct alloc_block *heap,
                                  const struct loc *loc,
                                  struct component *e);

struct component *new_assign_modify_expr(struct alloc_block *heap,
                                         struct component *e0,
                                         enum builtin_op op,
                                         struct component *e1, bool postfix,
                                         bool assignment,
                                         const struct loc *loc);
struct component *new_assign_expression(struct alloc_block *heap,
                                        struct component *e0,
                                        struct component *e1,
                                        const struct loc *loc,
                                        const struct loc *dstloc);

bool set_function_args(struct alloc_block *heap,
                       struct function *func,
                       struct pattern_list *args);

/* Creates a deep copy of src into dest. The str member is malloc()ed. */
void str_and_len_dup(struct str_and_len *dest, const struct str_and_len *src);

#ifdef PRINT_CODE
void print_mudlle_file(FILE *out, struct mfile *f);
#endif

static inline struct clist *reverse_clist(struct clist *l)
{
  return reverse_list(l, struct clist);
}

static inline struct cstlist *reverse_cstlist(struct cstlist *l)
{
  return reverse_list(l, struct cstlist);
}

value mudlle_parse(struct alloc_block *heap, struct mfile *f);

#endif /* TREE_H */
