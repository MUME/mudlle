#ifndef BUILTINS_H
#define BUILTINS_H

/* The machine code primitives */

void interpreter_invoke(void);
/* Enter the interpreter from a machine code function */

/* Builtins for operations */
void bsubtract(void), bor(void), band(void), bleq(void), 
  blne(void), bllt(void), blle(void), blgt(void), blge(void), bbitor(void), 
  bbitxor(void), bbitand(void), bshift_left(void), bshift_right(void), 
  badd(void), bmultiply(void), bdivide(void), bremainder(void), bnegate(void), 
  bnot(void), bbitnot(void), bref(void),
  bcar(void), bcdr(void), bwglobal(void);

/* Compiler support builtins (machine specific) */
#ifdef AMIGA
value __asm mc_invoke(register __d1 struct closure *c,
		      register __d0 uword argcount,
		      register __d2 value arg1,
		      register __d3 value arg2,
		      register __d4 value arg3,
		      register __d5 value extra);

void bbadargs(void), bcall(void), balloc(void), bcompare(void), balloc_readonly(void),
  bcons(void), berror(void), bvarargs(void);
#endif

#ifdef sparc
void bcleargc(void), bcall(void), balloc_variable(void), bcompare(void),
  bcall_primitive(void), bcall_primitive_leaf(void), balloc_cons(void),
  bcall_primitive_leaf_noalloc(void), balloc_closure(void), bvarargs(void),
  balloc_cons_l0(void);

value mc_invoke(value arg1, value arg2, value arg3, value arg4,
		value arg5, struct closure *c, int nargs);

value mc_invoke_vector(value arg1, value arg2, value arg3, value arg4,
		       value arg5, struct closure *c, int nargs,
		       struct vector *extra, int from);

void flush_windows(void);

#endif

#endif
