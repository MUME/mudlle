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
#include "calloc.h"
#include "types.h"
#include "utils.h"

extern block_t parser_memory;

typedef struct _component *component;
typedef struct _constant *constant;
typedef struct _pattern *pattern;

typedef struct {
  size_t len;
  char *str;
} str_and_len_t;

typedef struct _vlist {
  struct _vlist *next;
  const char *var;
  unsigned typeset;
  bool was_read, was_written;
  int lineno;
} *vlist;

typedef struct _clist {
  struct _clist *next;
  component c;
} *clist;

typedef struct _cstlist {
  struct _cstlist *next;
  constant cst;
} *cstlist;

typedef struct _cstpair {
  constant cst1, cst2;
} *cstpair;

typedef struct {
  unsigned typeset;		/* Return type(s) */
  str_and_len_t help;
  vlist args;			/* Stored in reverse order! */
  bool varargs;			/* true if accepts arg vector;
                                   implies vlength(vargs) == 1 */
  component value;
  int lineno;
  const char *filename;         /* Name on disk */
  const char *nicename;         /* Pretty-printed file name */
  const char *varname;		/* Name of variable in which function is
                                   stored */
} *function;

typedef struct {
  vlist locals;
  clist sequence;
  const char *filename;
  const char *nicename;
  int lineno;
  bool statics;                 /* true if 'locals' are in fact statics */
} *block;

enum constant_class {
  cst_int, cst_string, cst_list, cst_array, cst_float, cst_bigint, cst_table,
  cst_symbol, cst_expression
};

struct _constant {
  enum constant_class vclass;
  union {
    int integer;
    str_and_len_t string;
    double mudlle_float;
    const char *bigint_str;
    cstlist constants;          /* stored in reverse order; tail element is
                                   first for vclass cst_list */
    cstpair constpair;
    component expression;       /* not an actual constant at all */
  } u;
};

typedef struct _patternlist {
  struct _patternlist *next;
  pattern pat;
} *patternlist;

enum pattern_class {
  pat_const, pat_list, pat_array, pat_symbol, pat_sink,
  pat_expr
};

struct _pattern {
  enum pattern_class vclass;
  int lineno;
  union {
    constant constval;
    component expr;
    struct {
      patternlist patlist;
      bool ellipsis;
    } l;
    struct {
      const char *name;
      mtype type;
    } sym;
  } u;
};

typedef struct _matchnode {
  const char *filename;
  int lineno;
  pattern pattern;
  component expression, condition;
} *matchnode;

typedef struct _matchnodelist {
  struct _matchnodelist *next;
  matchnode match;
} *matchnodelist;

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
  op(class) op(name) op(imports) op(defines) op(reads)          \
  op(writes) op(statics) op(body) op(filename) op(nicename)

enum parser_module_field {
#define __MDEF(name) m_ ## name,
  FOR_PARSER_MODULE_FIELDS(__MDEF)
#undef __MDEF
  parser_module_fields
};

struct _component {
  enum component_class vclass;
  int lineno;
  union {
    struct {
      const char *symbol;
      component value;
    } assign;
    const char *recall;
    constant cst;
    function closure;
    clist execute;		/* 1st element is fn, rest are args */
    struct {
      enum builtin_op fn;
      clist args;
    } builtin;
    block blk;
    struct {
      const char *name;
      component expression;
    } labeled; /* also for exit */
  } u;
};

/* these are manually exported to mudlle from support.c */
enum file_class { f_plain, f_module, f_library };

typedef struct {
  enum file_class vclass;
  const char *name;
  vlist imports, defines, reads, writes, statics;
  block body;
  int lineno;
} *mfile;

mfile new_file(block_t heap, enum file_class vclass, const char *name,
	       vlist imports, vlist defines, vlist reads, vlist writes,
               vlist statics, block body, int lineno);
function new_function(block_t heap, unsigned typeset, str_and_len_t help,
                      vlist args, component val, int lineno,
                      const char *filename, const char *nicename);
function new_vfunction(block_t heap, unsigned typeset, str_and_len_t help,
		       const char *arg, component val,
		       int lineno, const char *filename,
                       const char *anicename);
block new_codeblock(block_t heap, vlist locals, clist sequence,
		    const char *filename, const char *nicename, int lineno);
block new_toplevel_codeblock(block_t heap, vlist statics, block body);
clist new_clist(block_t heap, component c, clist next);
cstpair new_cstpair(block_t heap, constant cst1, constant cst2);
cstlist new_cstlist(block_t heap, constant cst, cstlist next);
str_and_len_t *cstlist_find_symbol(cstlist list, str_and_len_t needle);
vlist new_vlist(block_t heap, const char *var, unsigned typeset, int lineno,
                vlist next);
constant new_constant(block_t heap, enum constant_class vclass, ...);
component new_component(block_t heap, int lineno, enum component_class vclass,
                        ...);
component new_binop_component(block_t heap, int lineno, enum builtin_op op,
                              component e1, component e2);

component new_int_component(block_t heap, long n);

component new_pattern_component(block_t heap, pattern pat, component e);
pattern new_pattern_constant(block_t heap, constant c, int lineno);
pattern new_pattern_expression(block_t heap, component c);
pattern new_pattern_symbol(block_t heap, const char *sym, mtype type,
                           int lineno);
pattern new_pattern_compound(block_t heap, enum pattern_class class,
			     patternlist list, bool ellipsis, int lineno);
pattern new_pattern_sink(block_t heap);
patternlist new_pattern_list(block_t heap, pattern pat, patternlist tail);

component new_match_component(block_t heap, component e,
			      matchnodelist matches);
component new_for_component(block_t heap, vlist vars,
                            component einit,
                            component eexit,
                            component eloop,
                            component e,
                            const char *filename, int lineno);
matchnodelist new_match_list(block_t heap, matchnode node, matchnodelist tail);
matchnode new_match_node(block_t heap, pattern pat, component cond,
			 component e, const char *filename, int lineno);

component new_xor_component(block_t heap, int lineno, component e0,
                            component e1);
component new_reference(block_t heap, int lineno, component e);
component new_dereference(block_t heap, int lineno, component e);
component new_assign_expression(block_t heap, component e0, enum builtin_op op,
                                component e1, bool postfix, int lineno);

/* Creates a deep copy of src into dest. The str member is xmalloc()ed. */
void str_and_len_dup(str_and_len_t *dest, const str_and_len_t *src);

#ifdef PRINT_CODE
void print_mudlle_file(FILE *out, mfile f);
#endif

static inline clist reverse_clist(clist l)
{
  return reverse_list(l, struct _clist);
}

static inline cstlist reverse_cstlist(cstlist l)
{
  return reverse_list(l, struct _cstlist);
}

value mudlle_parse(block_t heap, mfile f);

#endif /* TREE_H */
