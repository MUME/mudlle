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

struct str_and_len {
  size_t len;
  char *str;			/* not NUL-terminated */
};

struct vlist {
  struct vlist *next;
  const char *var;
  unsigned typeset;
  bool was_read, was_written;
  int lineno;
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
  int lineno;
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
  int lineno;
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
  pat_const, pat_list, pat_array, pat_symbol, pat_variable, pat_sink, pat_expr
};

struct pattern {
  enum pattern_class vclass;
  int lineno;
  union {
    struct constant *constval;
    struct component *expr;
    struct {
      struct pattern_list *patlist;
      bool ellipsis;
    } l;
    struct {
      const char *name;
      enum mudlle_type type;
    } var;
    struct {
      struct pattern *name;
      struct pattern *val;
    } sym;
  } u;
};

struct match_node {
  const char *filename;
  int lineno;
  struct pattern *pattern;
  struct component *expression, *condition;
};

struct match_node_list {
  struct match_node_list *next;
  struct match_node *match;
};

enum builtin_op {
  b_invalid = -1,
  b_sc_or, b_sc_and, b_eq, b_ne, b_lt, b_le, b_gt, b_ge,
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
  int lineno;
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
  int lineno;
};

struct mfile *new_file(struct alloc_block *heap, enum file_class vclass,
                       const char *name,
                       struct vlist *requires, struct vlist *defines,
                       struct vlist *reads, struct vlist *writes,
                       struct vlist *statics, struct block *body, int lineno);
struct function *new_function(struct alloc_block *heap, unsigned typeset,
                              struct str_and_len help, struct vlist *args,
                              struct component *val, int lineno,
                              const char *filename, const char *nicename);
struct function *new_vfunction(struct alloc_block *heap, unsigned typeset,
                               struct str_and_len help, const char *arg,
                               struct component *val, int lineno,
                               const char *filename, const char *anicename);
struct block *new_codeblock(struct alloc_block *heap, struct vlist *locals,
                            struct clist *sequence, const char *filename,
                            const char *nicename, int lineno);
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
                        unsigned typeset, int lineno, struct vlist *next);
struct constant *new_constant(struct alloc_block *heap,
                              enum constant_class vclass, ...);
struct constant *new_int_constant(struct alloc_block *heap, long l);
struct component *new_component(struct alloc_block *heap, int lineno,
                                enum component_class vclass, ...);
struct component *new_binop_component(struct alloc_block *heap, int lineno,
                                      enum builtin_op op, struct component *e1,
                                      struct component *e2);

struct component *new_int_component(struct alloc_block *heap, long n);

struct component *new_pattern_component(struct alloc_block *heap,
                                        struct pattern *pat,
                                        struct component *e);
struct pattern *new_pattern_constant(struct alloc_block *heap,
                                     struct constant *c, int lineno);
struct pattern *new_pattern_expression(struct alloc_block *heap,
                                       struct component *c);
struct pattern *new_pattern_variable(struct alloc_block *heap, const char *sym,
                                     enum mudlle_type type, int lineno);
struct pattern *new_pattern_symbol(struct alloc_block *heap,
                                   struct pattern *name, struct pattern *val,
                                   int lineno);
struct pattern *new_pattern_compound(struct alloc_block *heap,
                                     enum pattern_class class,
                                     struct pattern_list *list, bool ellipsis,
                                     int lineno);
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
                                    const char *filename, int lineno);
struct match_node_list *new_match_list(struct alloc_block *heap,
                                       struct match_node *node,
                                       struct match_node_list *tail);
struct match_node *new_match_node(struct alloc_block *heap,
                                  struct pattern *pat,
                                  struct component *cond, struct component *e,
                                  const char *filename, int lineno);

struct component *new_xor_component(struct alloc_block *heap, int lineno,
                                    struct component *e0,
                                    struct component *e1);
struct component *new_reference(struct alloc_block *heap, int lineno,
                                struct component *e);
struct component *new_dereference(struct alloc_block *heap, int lineno,
                                  struct component *e);
struct component *new_assign_expression(struct alloc_block *heap,
                                        struct component *e0,
                                        enum builtin_op op,
                                        struct component *e1, bool postfix,
                                        int lineno);

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
