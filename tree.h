/*
 * Copyright (c) 1993-1999 David Gay and Gustav Hållberg
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

extern block_t parser_memory;

typedef struct _component *component;
typedef struct _constant *constant;
typedef struct _pattern *pattern;

typedef struct _vlist {
  struct _vlist *next;
  const char *var;
  mtype type;
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
  mtype type;			/* Return type */
  const char *help;
  vlist args;			/* Stored in reverse order! */
  int varargs;			/* TRUE if accepts arg vector (==> length(vargs)=1) */
  component value;
  int lineno;
  const char *filename;
  const char *varname;		/* Name of variable in which function is stored */
} *function;

typedef struct {
  vlist locals;
  clist sequence;
} *block;

enum constant_class {
  cst_int, cst_string, cst_list, cst_array, cst_float, cst_bigint, cst_table,
  cst_symbol
};

struct _constant {
  enum constant_class vclass;
  union {
    int integer;
    const char *string;
    double mudlle_float;
    const char *bigint_str;
    cstlist constants;	/* Stored in reverse order ... */
    cstpair constpair;
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
  union {
    constant constval;
    component expr;
    struct {
      patternlist patlist;
      int ellipsis;
    } l;
    struct {
      const char *name;
      mtype type;
    } sym;
  } u;
};

typedef struct _matchnode {
  pattern pattern;
  component expression, condition;
} *matchnode;

typedef struct _matchnodelist {
  struct _matchnodelist *next;
  matchnode match;
} *matchnodelist;

enum {
  b_or, b_and, b_sc_or, b_sc_and, b_eq, b_ne, b_lt, b_le, b_gt, b_ge,
  b_bitor, b_bitxor, b_bitand, b_shift_left, b_shift_right,
  b_add, b_subtract, b_multiply, b_divide, b_remainder, b_negate,
  b_not, b_bitnot, b_ifelse, b_if, b_while, b_loop, b_ref, b_set,
  b_cons, last_builtin, 
  /* this one is only used by the parser */
  b_xor
};

enum component_class {
  c_assign, c_recall, c_constant, c_closure, c_execute, c_builtin, c_block,
  c_labeled, c_exit
};

struct _component {
  enum component_class vclass;
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
      unsigned int fn;
      clist args;
    } builtin;
    block blk;
    struct {
      const char *name;
      component expression;
    } labeled; /* also for exit */
  } u;
};

enum file_class { f_plain, f_module, f_library };

typedef struct {
  enum file_class vclass;
  const char *name;
  vlist imports;
  vlist defines;
  vlist reads;
  vlist writes;
  block body;
} *mfile;

mfile new_file(block_t heap, enum file_class vclass, const char *name,
	       vlist imports, vlist defines, vlist reads, vlist writes,
	       block body);
function new_function(block_t heap, mtype type, const char *help, vlist args,
		      component val, int lineno, const char *filename);
function new_vfunction(block_t heap, mtype type, const char *help,
		       const char *arg, component val,
		       int lineno, const char *filename);
block new_codeblock(block_t heap, vlist locals, clist sequence);
clist new_clist(block_t heap, component c, clist next);
cstpair new_cstpair(block_t heap, constant cst1, constant cst2);
cstlist new_cstlist(block_t heap, constant cst, cstlist next);
vlist new_vlist(block_t heap, const char *var, mtype type, vlist next);
constant new_constant(block_t heap, enum constant_class vclass, ...);
component new_component(block_t heap, enum component_class vclass, ...);

component new_pattern_component(block_t heap, pattern pat, component e);
pattern new_pattern_constant(block_t heap, constant c);
pattern new_pattern_expression(block_t heap, component c);
pattern new_pattern_symbol(block_t heap, const char *sym, mtype type);
pattern new_pattern_compound(block_t heap, enum pattern_class class,
			     patternlist list, int ellipsis);
pattern new_pattern_sink(block_t heap);
patternlist new_pattern_list(block_t heap, pattern pat, patternlist tail);

component new_match_component(block_t heap, component e, 
			      matchnodelist matches);
matchnodelist new_match_list(block_t heap, matchnode node, matchnodelist tail);
matchnode new_match_node(block_t heap, pattern pat, component cond, 
			 component e);

component new_xor_component(block_t heap, component e0, component e1);
component new_postfix_inc_component(block_t heap, const char *var, int op);

#ifdef PRINT_CODE
void print_file(FILE *out, mfile f);
#endif

clist append_clist(clist l1, clist l2);
clist reverse_clist(clist l);
cstlist reverse_cstlist(cstlist l);
vlist append_vlist(vlist l1, vlist l2);
vlist reverse_vlist(vlist l);

value mudlle_parse(block_t heap, mfile f);

#endif
