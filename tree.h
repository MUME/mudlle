/* $Log: tree.h,v $
 * Revision 1.11  1994/10/09  06:43:06  arda
 * Libraries
 * Type inference
 * Many minor improvements
 *
 * Revision 1.10  1994/08/16  19:16:25  arda
 * Mudlle compiler for sparc now fully functional (68k compiler now needs
 * updating for primitives).
 * Changes to allow Sparc trap's for runtime errors.
 * Also added flags to primitives for better calling sequences.
 *
 * Revision 1.7  1994/02/24  08:33:09  arda
 * Owl: New error messages.
 *
 * Revision 1.6  1994/01/29  19:50:39  dgay
 * Owl: add file & line information to functions.
 *
 * Revision 1.5  1993/08/15  21:00:32  un_mec
 * Owl: Overload [].
 *      Added xcalloc, xrealloc.
 *
 * Revision 1.4  1993/07/21  20:37:02  un_mec
 * Owl: Added &&, ||, optimised if.
 *      Added branches to the intermediate language.
 *      Separated destiniation language generation into ins module
 *      (with some peephole optimisation)
 *      Standalone version of mudlle (mkf, runtime/mkf, mudlle.c) added to CVS
 *
 * Revision 1.3  1993/03/29  09:24:44  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.3  1993/03/14  16:15:08  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.1  1992/12/27  21:41:40  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

#ifndef TREE_H
#define TREE_H

#include <stdio.h>

typedef struct component *component;
typedef struct constant *constant;

typedef struct vlist {
  struct vlist *next;
  const char *var;
  mtype type;
} *vlist;

typedef struct clist {
  struct clist *next;
  component c;
} *clist;

typedef struct cstlist {
  struct cstlist *next;
  constant cst;
} *cstlist;

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

struct constant {
  enum { cst_int, cst_string, cst_list, cst_array } class;
  union {
    int integer;
    const char *string;
    struct cstlist *constants;	/* Stored in reverse order ... */
  } u;
};

enum {
  b_or, b_and, b_sc_or, b_sc_and, b_eq, b_ne, b_lt, b_le, b_gt, b_ge,
  b_bitor, b_bitxor, b_bitand, b_shift_left, b_shift_right,
  b_add, b_subtract, b_multiply, b_divide, b_remainder, b_negate,
  b_not, b_bitnot, b_ifelse, b_if, b_while, b_loop, b_ref, b_set,
  b_cons, last_builtin
};

struct component {
  enum { c_assign, c_recall, c_constant, c_closure, c_execute, c_builtin, c_block,
	 c_labeled, c_exit }
    class;
  union {
    struct {
      const char *symbol;
      component value;
    } assign;
    const char *recall;
    constant constant;
    function closure;
    clist execute;		/* 1st element is fn, rest are args */
    struct {
      unsigned int fn;
      clist args;
    } builtin;
    block block;
    struct {
      const char *name;
      component expression;
    } labeled; /* also for exit */
  } u;
};

typedef struct {
  enum { f_plain, f_module, f_library } class;
  const char *name;
  vlist imports;
  vlist defines;
  vlist reads;
  vlist writes;
  block body;
} *file;

file new_file(int class, const char *name, vlist imports, vlist defines, vlist reads,
	      vlist writes, block body);
function new_function(mtype type, const char *help, vlist args, component value,
		      int lineno, const char *filename);
function new_vfunction(mtype type, const char *help, const char *arg, component value,
		       int lineno, const char *filename);
block new_codeblock(vlist locals, clist sequence);
clist new_clist(component c, clist next);
cstlist new_cstlist(constant cst, cstlist next);
vlist new_vlist(const char *var, mtype type, vlist next);
constant new_constant(int class, ...);
component new_component(int class, ...);
#ifdef PRINT_CODE
void print_file(FILE *out, file f);
#endif

clist append_clist(clist l1, clist l2);
clist reverse_clist(clist l);
cstlist reverse_cstlist(cstlist l);
vlist append_vlist(vlist l1, vlist l2);
vlist reverse_vlist(vlist l);

value mudlle_parse(file f);

#endif
