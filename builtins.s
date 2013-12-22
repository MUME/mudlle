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

	.seg	"text"

#include "options.h"

#ifdef SunOS
#define N(x) _/**/x
#endif

#define N(x) x

/* Important constants ... */
#define true 3
#define false 1
#define null 0

/* Register assignments */
#define arg0 l0
#define arg1 l1
#define xcountr g1
#define globals g2
#define scratch g3
#define argcount g3
#define scratch2 g4
#define closure i5
#define closure_in o5

/* Traps */
#define trap_offset 16

/* Runtime errors */
#define error_bad_function 0
#define error_stack_underflow 1
#define error_bad_type 2
#define error_divide_by_zero 3
#define error_bad_index 4
#define error_bad_value 5
#define error_variable_read_only 6
#define error_loop 7
#define error_recurse 8
#define error_wrong_parameters 9
#define error_security_violation 10
#define error_value_read_only 11
#define error_user_interrupt 12

/* Types */
#define type_code 0
#define type_closure 1
#define type_variable 2
#define type_internal 3
#define type_primitive 4
#define type_varargs 5
#define type_secure 6
#define type_integer 7
#define type_string 8
#define type_vector 9
#define type_list 10
#define type_symbol 11
#define type_table 12
#define type_private 13
#define type_object 14
#define type_character 15
#define type_gone 16
#define type_outputport 17
#define type_mcode 18
#define last_type 19

/* Garbage types */
#define garbage_string 0
#define garbage_record 1
#define garbage_code 2
#define garbage_forwarded 3
#define garbage_permanent 4
#define garbage_temp 5
#define garbage_mcode 6

/* Flags */
#define flag_readonly 1
#define flag_immutable 2

/* Basic object structure */
#ifdef GCDEBUG
#define object_offset 12	/* Size of object header */
#define object_gen 8		/* Object generation */
#else
#define object_offset 8		/* Size of object header */
#endif
#define object_type 5		/* Type of object */
#define object_size 0		/* Size of object */
#define object_info 4		/* Object information (type, flags, etc) */
#define object_flags 6		/* Flags of object */

/* Offsets of fields in various types */
#define pair_size (object_offset+8)
#define env_ovalues 8		/* values field in environment */
#define mcode_seclevel 0	/* seclevel in mcode */
#define function_offset 32	/* start of code in mcode object */
#define primitive_op 4		/* primitive_ext in primitive */
#define primop_op 8		/* C function in primitive_ext */
#define primop_nargs 12		/* nargs in primitive_ext */
#define primop_seclevel 20	/* seclevel in primitive_ext */

/* Stack frame information */
#define argstart 68
#define minframe 96

/* ccontext format */
#define cc_frame_start 0
#define cc_frame_end 4

/* call_stack structure def */
#define cs_next 0
#define cs_type 4
#define cs_compiled_SIZE 8 /* size for call_compiled */

#define call_compiled 2

/* External symbols used */
	.global	N(env_values)
	.global N(ccontext)
	.global	N(mvars)
	.global	N(xcount)
	.global	N(code_string_append)
	.global	N(code_ref)
	.global	N(posgen0)
	.global	N(startgen0)
	.global	N(minorgen)
	.global	N(majorgen)
	.global	N(garbage_collect)
	.global	N(seclevel)
	.global	N(interpreter_start)

/* Call C function, preserving global state, etc (needs l0) */

#define CCALL_LEAF_NOALLOC(fn) \
	call	fn; \
	mov	%xcountr,%l0; \
	set	N(env_values),%globals; \
	mov	%l0,%xcountr; \
	ld	[%globals],%globals;

#define CCALL_LEAF(fn) \
	set	N(ccontext),%l0; \
	st	%sp,[%l0+cc_frame_end]; \
	call	fn; \
	mov	%xcountr,%l0; \
	set	N(env_values),%globals; \
	mov	%l0,%xcountr; \
	ld	[%globals],%globals;

#define CCALL(fn) \
	set	N(ccontext),%l0; \
	st	%sp,[%l0+cc_frame_end]; \
	set	N(seclevel),%globals; \
	set	DEFAULT_SECLEVEL,%l0; \
	sth	%l0,[%globals]; \
	set	N(xcount),%l0; \
	call	fn; \
	st	%xcountr,[%l0]; \
	set	N(env_values),%globals; \
	set	N(xcount),%l0; \
	ld	[%globals],%globals; \
	ld	[%l0],%xcountr;

#define CCALL_SECURE(fn) \
	set	N(ccontext),%l0; \
	st	%sp,[%l0+cc_frame_end]; \
	ld	[%closure+object_offset],%l0; \
	set	N(seclevel),%globals; \
	lduh	[%l0+object_offset+mcode_seclevel],%l0; \
	sth	%l0,[%globals]; \
	set	N(xcount),%l0; \
	call	fn; \
	st	%xcountr,[%l0]; \
	set	N(env_values),%globals; \
	set	N(xcount),%l0; \
	ld	[%globals],%globals; \
	ld	[%l0],%xcountr;

/* Simple integer manipulation macros */
#define SETINT(reg) \
	bset	1,%reg;		/* set integer type bit */

#define CLEARINT(reg) \
	bclr	1,%reg;		/* clear integer type bit */

#define INTVAL(reg) \
	sra	%reg,1,%reg;	/* Make into normal integer */

#define INTVALTO(reg1, reg2) \
	sra	%reg1,1,%reg2;	/* Make into normal integer */

#define MAKEINT(reg) \
	add	%reg,%reg,%reg; \
	SETINT(reg)

#define MAKEINTTO(reg1, reg2) \
	add	%reg1,%reg1,%reg2; \
	bset	1,%reg2;

#define ISINT(reg) \
	btst	1,%reg;		/* Is reg1 an integer ? */

#define ISNULL(reg) \
	cmp	null,%reg;

#ifdef GCDEBUG_CHECK
#define GCCHECK(reg) \
	cmp	null,%reg; \
	be	6f; \
	ISINT(reg) \
	bne	6f; \
	nop; \
	set	N(xcount),%scratch2; \
	st	%xcountr,[%scratch2]; \
	ld	[%reg+object_gen],%scratch2; \
	btst	1,%scratch2; \
	be	8f; \
	nop; \
	set	N(minorgen),%xcountr; \
	ld	[%xcountr],%xcountr; \
	ba,a	7f; \
8:	set	N(majorgen),%xcountr; \
	ld	[%xcountr],%xcountr; \
7:	cmp	%xcountr,%scratch2; \
	be	6f; \
	mov	%o7,%scratch2; \
	call	N(abort); \
	nop; \
6:	set	N(xcount),%xcountr; \
	ld	[%xcountr],%xcountr;
#else
#define GCCHECK(reg)
#endif


	.global	N(abort)

/* Boolean builtins: and, or, not */

	.global	N(bor)

N(bor):	or	%arg0,%arg1,%arg0
	cmp	%arg0,false
	bne,a	1f
	or	%g0,true,%arg0
1:	retl
	nop


	.global	N(band)

N(band):	cmp	%arg0,false
	set	true,%arg0
	be,a	1f
	or	%g0,false,%arg0
1:	cmp	%arg1,false
	be,a	2f
	or	%g0,false,%arg0
2:	retl
	nop

	.global	N(bnot)

N(bnot):	cmp	%arg0,false
	set	true,%arg0
	bne,a	1f
	or	%g0,false,%arg0
1:	retl
	nop

/* Comparisons: ==, != */

	.global	N(bleq)

N(bleq):	cmp	%arg0,%arg1
	set	false,%arg0
	be,a	1f
	or	%g0,true,%arg0
1:	retl
	nop

	.global	N(blne)

N(blne):	cmp	%arg0,%arg1
	set	false,%arg0
	bne,a	1f
	or	%g0,true,%arg0
1:	retl
	nop


/* Simple integer operations: <, <=, >, >=, |, &, ^, <<, >>, -, *, /, %, ~,
   - (unary), + (int) */

#define INT1ARG \
	ISINT(arg0); \
	te	error_bad_type+trap_offset;

#define INT2ARGS \
	ISINT(arg0); \
	te	error_bad_type+trap_offset; \
	ISINT(arg1); \
	te	error_bad_type+trap_offset;

#define RELOP(name, btest) \
	.global	name; \
name:	INT2ARGS \
	cmp	%arg0,%arg1; \
	set	false,%arg0; \
	btest,a	1f; \
	or	%g0,true,%arg0; \
1:	retl; \
	nop;

RELOP(N(bllt), bl)
RELOP(N(blle), ble)
RELOP(N(blgt), bg)
RELOP(N(blge), bge)

	.global	N(bbitand)

N(bbitand):
	INT2ARGS
	retl
	and	%arg0,%arg1,%arg0

	.global	N(bbitor)

N(bbitor):
	INT2ARGS
	retl
	or	%arg0,%arg1,%arg0

	.global	N(bbitxor)

N(bbitxor):
	INT2ARGS
	xor	%arg0,%arg1,%arg0
	retl
	SETINT(arg0)

	.global	N(bshift_left)

N(bshift_left):
	INT2ARGS
	INTVAL(arg1)
	CLEARINT(arg0)
	sll	%arg0,%arg1,%arg0
	retl
	SETINT(arg0)

	.global N(bshift_right)

N(bshift_right):
	INT2ARGS
	INTVAL(arg1)
	sra	%arg0,%arg1,%arg0
	retl
	SETINT(arg0)

	.global	N(bsubtract)

N(bsubtract):
	INT2ARGS
	CLEARINT(arg1)
	retl
	sub	%arg0,%arg1,%arg0

	.global	N(bmultiply)

N(bmultiply):
	INT2ARGS
	CLEARINT(arg0)
	INTVAL(arg1)
	smul	%arg0,%arg1,%arg0
	retl
	SETINT(arg0)

	.global	N(bdivide)

N(bdivide):
	INT2ARGS
	INTVAL(arg1)
	cmp	%arg1,0
	te	error_divide_by_zero+trap_offset
	CLEARINT(arg0)
	sra	%arg0,31,%scratch
	wr	%scratch,%g0,%y
	sdiv	%arg0,%arg1,%arg0
	retl
	SETINT(arg0)

	.global	N(bremainder)

N(bremainder):
	INT2ARGS
	INTVAL(arg1)
	cmp	%arg1,0
	te	error_divide_by_zero+trap_offset
	INTVAL(arg0)
	sra	%arg0,31,%scratch
	wr	%scratch,%g0,%y

	sdiv	%arg0,%arg1,%scratch
	smul	%scratch,%arg1,%arg1
	sub	%arg0,%arg1,%arg0

	sll	%arg0,1,%arg0
	retl
	SETINT(arg0)

	.global	N(bnegate)

N(bnegate):
	INT1ARG
	neg	%arg0
	retl
	add	%arg0,2,%arg0

	.global	N(bbitnot)

N(bbitnot):
	INT1ARG
	retl
	xnor	%arg0,1,%arg0


/* Call C code: ref, + */

	.global	N(badd)

N(badd):	ISINT(arg0)
	be	cadd
	ISINT(arg1)
	be	cadd

	/* Integer addition */
	CLEARINT(arg1)
	retl
	add	%arg0,%arg1,%arg0

cadd:	/* Call string_append */

	/* %arg0, %arg1 not being %on regs makes life complicated */
	mov	%arg0,%globals
	mov	%arg1,%scratch
	save	%sp,-minframe,%sp
	call	clear_frame
	nop
	mov	%globals,%o0
	mov	%scratch,%o1

	CCALL_LEAF(N(code_string_append))
	ret
	restore	%o0,0,%arg0

	.global	N(bref)

N(bref):	ISINT(arg0)
	bne	cref
	nop
	lduh	[%arg0+object_info],%scratch
	cmp	%scratch,type_vector | garbage_record << 8
	be	vectorref
	cmp	%scratch,type_string | garbage_string << 8
	be	stringref
	nop

cref:	mov	%arg0,%globals
	mov	%arg1,%scratch
	save	%sp,-minframe,%sp
	call	clear_frame
	nop
	mov	%globals,%o0
	mov	%scratch,%o1

	CCALL_LEAF(N(code_ref))
	ret
	restore	%o0,0,%arg0

vectorref:
	ISINT(arg1)
	te	error_bad_type+trap_offset
	addcc	%arg1,%arg1,%arg1
	tneg	error_bad_index+trap_offset
	ld	[%arg0+object_size],%scratch
	add	%arg1,object_offset-2,%arg1
	cmp	%arg1,%scratch
	tge	error_bad_index+trap_offset
	retl
	ld	[%arg0+%arg1],%arg0

stringref:
	ISINT(arg1)
	te	error_bad_type+trap_offset
	INTVAL(arg1)
	tst	%arg1
	tneg	error_bad_index+trap_offset
	ld	[%arg0+object_size],%scratch
	add	%arg1,object_offset+1,%arg1
	cmp	%arg1,%scratch
	tge	error_bad_index+trap_offset
	sub	%arg1,1,%arg1
	ldub	[%arg0+%arg1],%arg0
	sll	%arg0,1,%arg0
	retl
	SETINT(arg0)


/* List ops: cons(.), car, cdr */

	.global	N(balloc_cons)

	/* arg0 *is* available */
N(balloc_cons):
	set	N(posgen0),%arg0
	ld	[%arg0],%arg1
	set	N(startgen0),%scratch
	ld	[%scratch],%scratch
	sub	%arg1,pair_size,%arg1
	cmp	%arg1,%scratch
	blu	consgc

	! success, init pair
	st	%arg1,[%arg0]

	set	pair_size,%scratch
	st	%scratch,[%arg1+object_size]
	sethi	%hi(garbage_record << 24 | type_list << 16),%scratch
#ifdef GCDEBUG
	st	%scratch,[%arg1+object_info]
	set	N(minorgen),%scratch
	ld	[%scratch],%scratch
	retl
	st	%scratch,[%arg1+object_gen]
#else
	retl
	st	%scratch,[%arg1+object_info]
#endif

consgc: save	%sp,-minframe,%sp
	call	clear_frame
	nop
	mov	pair_size,%o0
	CCALL_LEAF(N(garbage_collect))
	ba	N(balloc_cons)
	restore

	.global	N(bcar)

N(bcar):	lduh	[%arg0+object_info],%scratch
	cmp	%scratch,type_list | garbage_record << 8
	tne	error_bad_type+trap_offset
	retl
	ld	[%arg0+object_offset],%arg0

	.global	N(bcdr)

N(bcdr):	lduh	[%arg0+object_info],%scratch
	cmp	%scratch,type_list | garbage_record << 8
	tne	error_bad_type+trap_offset
	retl
	ld	[%arg0+object_offset+4],%arg0

/* Special ops: bcompare, bcleargc, balloc */

	.global	N(bwglobal)

N(bwglobal):
	set	N(mvars),%scratch
	ld	[%scratch],%scratch
	ld	[%scratch+%arg0],%scratch
	ISINT(scratch)
	retl
	te	error_variable_read_only+trap_offset

	.global	N(bcompare)

N(bcompare):
	INT2ARGS
	retl
	subcc	%arg0,%arg1,%g0

	.global	N(bcleargc)

/* Clear the allocated stack frame (between %sp+argstart+5*4 and %fp) */

N(bcleargc):
	tne	error_wrong_parameters+trap_offset

	clr	[%sp+88]
	add	%sp,96,%scratch
1:	cmp	%scratch,%fp
	bgeu	2f
	clr	[%scratch-4]	! Clears %sp+92 on first iteration
	clr	[%scratch+0]
	clr	[%scratch+4]
	clr	[%scratch+8]
	ba	1b
	add	%scratch,16,%scratch

	/* Clear new regs in window */
	/* Check minlevel security (seclevel should be >= minlevel) */
	/* Check for infinite loops (xcountr) */
2:	clr	%l2
	ld	[%closure+object_offset],%l0
	clr	%l3
	set	N(minlevel),%l1
	clr	%l4
	lduh	[%l0+object_offset+mcode_seclevel],%l0
	clr	%l5
	subcc	%xcountr,1,%xcountr
	clr	%l6
	te	error_loop+trap_offset
	clr	%l7
	lduh	[%l1],%l1
	clr	%o0
	clr	%o1
	clr	%o2
	clr	%o3
	clr	%o4
	clr	%o5

	cmp	%l0,%l1
	retl
	tlu	error_security_violation+trap_offset

/* Clear a frame and primop */
/* (In preparation for a call to a C primitive as part of
   a builtin op) */
clear_frame:
	add	%sp,argstart+4*4,%l2
1:	add	%l2,4,%l2
	cmp	%l2,%fp
	blu,a	1b
	clr	[%l2]

	/* Clear new regs in window */
	clr	%l1	/* l1 contains mudlle return address in other cases */
	clr	%l2
	clr	%l3
	clr	%l4
	clr	%l5
	clr	%l6
	clr	%l7
	clr	%o0
	clr	%o1
	clr	%o2
	clr	%o3
	clr	%o4
	retl
	clr	%o5

	.global	N(balloc_variable)

N(balloc_variable):
	set	N(posgen0),%arg0
	ld	[%arg0],%arg0
	set	N(startgen0),%scratch
	ld	[%scratch],%scratch
	mov	object_offset + 4, %arg1	! Variable size
	sub	%arg0,%arg1,%arg0
	cmp	%arg0,%scratch
	blu	vgc

	! success, init
	set	N(posgen0),%scratch
	st	%arg0,[%scratch]

	st	%arg1,[%arg0+object_size]
#ifdef GCDEBUG
	set	N(minorgen),%scratch
	ld	[%scratch],%scratch
	sethi	%hi(garbage_record << 24 | type_variable << 16),%arg1
	st	%arg1,[%arg0+object_info]
	retl
	st	%scratch,[%arg0+object_gen]
#else
	sethi	%hi(garbage_record << 24 | type_variable << 16),%arg1
	retl
	st	%arg1,[%arg0+object_info]
#endif

vgc:	save	%sp,-minframe,%sp
	call	clear_frame
	nop
	mov	object_offset + 4,%o0
	CCALL_LEAF(N(garbage_collect))
	ba	N(balloc_variable)
	restore


	.global	N(balloc_closure)

/* size of closure is in arg1 */
N(balloc_closure):
	set	N(posgen0),%arg0
	ld	[%arg0],%arg0
	set	N(startgen0),%scratch
	ld	[%scratch],%scratch
	sub	%arg0,%arg1,%arg0
	cmp	%arg0,%scratch
	blu	cgc

	! success, init
	set	N(posgen0),%scratch
	st	%arg0,[%scratch]

	st	%arg1,[%arg0+object_size]
#ifdef GCDEBUG
	set	N(minorgen),%scratch
	ld	[%scratch],%scratch
	set	garbage_record << 24 | type_closure << 16 | flag_readonly,%arg1
	st	%arg1,[%arg0+object_info]
	retl
	st	%scratch,[%arg0+object_gen]
#else
	set	garbage_record << 24 | type_closure << 16 | flag_readonly,%arg1
	retl
	st	%arg1,[%arg0+object_info]
#endif

cgc:	mov	%arg1,%scratch
	save	%sp,-minframe,%sp
	call	clear_frame
	nop
	mov	%scratch,%o0
	CCALL_LEAF(N(garbage_collect))
	ba	N(balloc_closure)
	restore


/* Interface operations:
    bcall: any call from machine language
    interpreter_invoke: machine language -> interpreter
    mc_invoke: C code -> machine language
*/

	.global	N(bcall)

/* Call using standard sparc conventions, except that
     o5 is the called closure
     the 6th argument is on the stack
*/
N(bcall):	lduh	[%closure_in+object_info],%arg0
	cmp	%arg0,type_closure | garbage_record << 8
	be,a	call_closure
	ld	[%closure_in+object_offset],%arg0

	GCCHECK(o0)
	GCCHECK(o1)
	GCCHECK(o2)
	GCCHECK(o3)
	GCCHECK(o4)
	GCCHECK(o5)

	cmp	%arg0,type_primitive | garbage_permanent << 8
	be,a	call_primitive
	ld	[%closure_in+object_offset+primitive_op],%closure_in

	cmp	%arg0,type_secure | garbage_permanent << 8
	be,a	call_secure
	ld	[%closure_in+object_offset+primitive_op],%closure_in

	cmp	%arg0,type_varargs | garbage_permanent << 8
	tne	error_bad_function+trap_offset

call_varargs:
	save	%sp,-minframe,%sp
	call	N(bvarargs)
	nop
	mov	%l1,%i1
	restore
	ld	[%closure_in+object_offset+primitive_op],%scratch
	mov	%o7,%l1
	ld	[%scratch+primop_op],%scratch		! Fetch C function
	CCALL(%scratch)
	clr	%o1
	clr	%o2
	clr	%o3
	clr	%o4
	jmpl	%l1+8,%g0
	clr	%o5


call_secure:
	/* Check security level */
	ld	[%closure+object_offset],%arg1
	lduh	[%closure_in+primop_seclevel],%arg0
	cmp	DEFAULT_SECLEVEL,%arg0
	tlu	error_security_violation+trap_offset

call_primitive:
	/* Check arg count */
	lduh	[%closure_in+primop_nargs],%arg0
	cmp	%arg0,%argcount
	tne	%g0+error_wrong_parameters+trap_offset

	mov	%o7,%l1
	ld	[%closure_in+primop_op],%scratch	! Fetch actual C function
	/* Fetch 6th argument from stack */
	ld	[%sp+argstart+5*4],%o5
	CCALL(%scratch)
	clr	%o1
	clr	%o2
	clr	%o3
	clr	%o4
	jmpl	%l1+8,%g0
	clr	%o5

call_closure:
	jmpl	%arg0+object_offset+function_offset,%g0
	nop


	.global	N(bvarargs)

/* Build varargs vector & count */
N(bvarargs):
	/* in: argcount: argument count, i0-i4+stack(fp): arguments */
	/* returns: i0: arg vector, i1: arg count */
	/* uses lx, ix, ox, but does not require them to be clean */
	sll	%argcount,2,%l1
	add	%l1,object_offset,%l1

	/* Allocate l1 bytes for argument vector */
valloc:	set	N(posgen0),%l0
	ld	[%l0],%l2
	set	N(startgen0),%l3
	ld	[%l3],%l3
	sub	%l2,%l1,%l2
	cmp	%l2,%l3
	blu	vargc

	/* success */
	st	%l2,[%l0]

	/* init arg vector header */
	st	%l1,[%l2+object_size]
	sethi	%hi(garbage_record << 24 | type_vector << 16),%l3
	st	%l3,[%l2+object_info]
#ifdef GCDEBUG
	set	N(minorgen),%l3
	ld	[%l3],%l3
	st	%l3,[%l2+object_gen]
#endif

	/* Copy arguments to vector */
	sub	%l1,object_offset,%l1
	srl	%l1,2,%l1
	cmp	0,%l1
	be	vargs0
	cmp	%l1,1
	be	vargs1
	cmp	%l1,2
	be	vargs2
	cmp	%l1,3
	be	vargs3
	cmp	%l1,4
	be	vargs4
	cmp	%l1,5
	be	vargs5
	nop

	/* Copy remaining arguments from stack */
	sll	%l1,2,%l0
	add	%l2,object_offset,%l3
	add	%fp,argstart,%l4

1:	sub	%l0,4,%l0
	ld	[%l4+%l0],%l5
	cmp	%l0,5*4
	bne	1b
	st	%l5,[%l3+%l0]

vargs5:	st	%i4,[%l2+object_offset+4*4]
vargs4:	st	%i3,[%l2+object_offset+3*4]
vargs3:	st	%i2,[%l2+object_offset+2*4]
vargs2:	st	%i1,[%l2+object_offset+1*4]
vargs1:	st	%i0,[%l2+object_offset]
vargs0: retl
	mov	%l2,%i0

vargc:	mov	%o7,%l0
	call	clear_frame
	mov	%l1,%scratch
	mov	%l0,%l1		! l1 is forwarded in this frame !
	mov	%scratch,%o0
	add	%scratch,%scratch,%l2
	SETINT(l2)
	CCALL_LEAF(N(garbage_collect))
	mov	%l1,%o7
	ba	valloc
	sra	%l2,1,%l1

	.global	N(bcall_secure)

N(bcall_secure):
	ld	[%closure_in+object_offset+primitive_op],%closure_in
	/* Check security level */
	ld	[%closure+object_offset],%arg1
	lduh	[%closure_in+primop_seclevel],%arg0
	lduh	[%arg1+object_offset+mcode_seclevel],%arg1
	cmp	%arg1,%arg0
	tlu	error_security_violation+trap_offset
	/* Check arg count */
	lduh	[%closure_in+primop_nargs],%arg0
	cmp	%arg0,%argcount
	tne	%g0+error_wrong_parameters+trap_offset

	mov	%o7,%l1
	ld	[%closure_in+primop_op],%scratch	! Fetch actual C function
	/* Fetch 6th argument from stack */
	ld	[%sp+argstart+5*4],%o5
	CCALL_SECURE(%scratch)
	clr	%o1
	clr	%o2
	clr	%o3
	clr	%o4
	jmpl	%l1+8,%g0
	clr	%o5

	.global	N(bcall_varargs)

N(bcall_varargs):
	save	%sp,-minframe,%sp
	call	N(bvarargs)
	nop
	mov	%l1,%i1
	restore
	ld	[%closure_in+object_offset+primitive_op],%scratch
	mov	%o7,%l1
	ld	[%scratch+primop_op],%scratch		! Fetch C function
	CCALL_SECURE(%scratch)
	clr	%o1
	clr	%o2
	clr	%o3
	clr	%o4
	jmpl	%l1+8,%g0
	clr	%o5

	.global	N(bcall_primitive)

/* Call using standard sparc conventions
   %scratch is the primitive containing the called function
*/
N(bcall_primitive):
	mov	%o7,%l1
	CCALL_SECURE(%scratch)
	clr	%o1
	clr	%o2
	clr	%o3
	clr	%o4
	jmpl	%l1+8,%g0
	clr	%o5

	.global	N(bcall_primitive_leaf)

/* Call using standard sparc conventions
     %scratch is the primitive containing the called function
     It is a "leaf" primitive (calls no mudlle functions)
*/
N(bcall_primitive_leaf):
	mov	%o7,%l1
	CCALL_LEAF(%scratch)
	clr	%o1
	clr	%o2
	clr	%o3
	clr	%o4
	jmpl	%l1+8,%g0
	clr	%o5

	.global	N(bcall_primitive_leaf_noalloc)

/* Call using standard sparc conventions
     %scratch is the primitive containing the called function
     It is a "leaf" primitive (calls no mudlle functions) and does no
     allocation.
*/
N(bcall_primitive_leaf_noalloc):
	mov	%o7,%l1
	/* Can't use CCALL_LEAF_NOALLOC because of error messages */
	CCALL_LEAF(%scratch)
	clr	%o1
	clr	%o2
	clr	%o3
	clr	%o4
	jmpl	%l1+8,%g0
	clr	%o5

/* Assumes call_stack entry address is fp-cs_compiled_SIZE. Uses scratch, l0, l1 */
#define PUSH_CALL_STACK \
	sub	%fp,cs_compiled_SIZE,%l0; \
	set	N(call_stack),%scratch; \
	ld	[%scratch],%l1; \
	st	%l0,[%scratch]; \
	st	%l1,[%l0+cs_next]; \
	set	call_compiled,%l1; \
	st	%l1,[%l0+cs_type]


/* Assumes call_stack entry address is fp-cs_compiled_SIZE. Uses scratch */
#define POP_CALL_STACK \
	ld	[%fp-cs_compiled_SIZE+cs_next],%l0; \
	set	N(call_stack),%scratch; \
	st	%l0,[%scratch]

	.global	N(mc_invoke)

/* We receive args0-4 in o0-o4, closure in o5, argcount on stack */
N(mc_invoke):
	save	%sp,-((minframe+cs_compiled_SIZE+15)&~15),%sp

	PUSH_CALL_STACK
	/* Frame information */
	set	N(ccontext),%scratch
	ld	[%scratch++cc_frame_start],%l3
	ld	[%scratch+cc_frame_end],%l2
	st	%sp,[%scratch]

	/* Clear 6th argument, and unused mem */
	clr	[%sp+argstart+5*4]
	clr	[%sp+argstart+6*4]
	clr	[%sp+argstart+7*4]
	clr	[%sp+argstart+8*4]

	ld	[%closure+object_offset],%arg0
	set	N(env_values),%globals
	mov	%i0,%o0
	ld	[%globals],%globals
	mov	%i1,%o1
	ld	[%fp+argstart+6*4],%argcount	! Fetch argcount
	mov	%i2,%o2
	set	N(xcount),%xcountr
	mov	%i3,%o3
	ld	[%xcountr],%xcountr
	mov	%i4,%o4
	call	%arg0+object_offset+function_offset
	mov	%i5,%o5

	/* Restore frame information */
	set	N(ccontext),%scratch
	st	%l3,[%scratch+cc_frame_start]
	st	%l2,[%scratch+cc_frame_end]
	POP_CALL_STACK

	ret
	restore	%o0,0,%o0

	.global	N(mc_invoke_vector)

/* We receive args0-4 in o0-o4, closure in o5, argcount on stack,
   extra arguments vector, start offset in this vector.
   argcount is >= 6 */
N(mc_invoke_vector):
	/* Calculate frame size */
	ld	[%sp+argstart+6*4],%globals	! Fetch argcount
	sll	%globals,2,%scratch
	add	%scratch,argstart+cs_compiled_SIZE+15,%scratch
	andn	%scratch,15,%scratch
	neg	%scratch

	save	%sp,%scratch,%sp

	PUSH_CALL_STACK
	/* Clear the last 12 "mudlle" bytes of the frame
	   (may be unused argument slots because of rounding, so
	   won't be initialised below, so will confuse the GC) */
	clr	[%fp-12]
	clr	[%fp-16]
	clr	[%fp-20]

	/* Frame information */
	set	N(ccontext),%scratch
	ld	[%scratch+cc_frame_start],%l3
	ld	[%scratch+cc_frame_end],%l2
	st	%sp,[%scratch]

	/* Transfer arguments (from the 6th, to argstart + 5*4) */
	ld	[%fp+argstart+7*4],%l5
	ld	[%fp+argstart+8*4],%l6
	sll	%l6,2,%l6
	add	%l6,object_offset,%l6
	add	%l5,%l6,%l5		! l5 now points to start of extra args
	add	%sp,argstart+5*4,%l4	! l4 now points to stack frame for args

	mov	%globals,%argcount
	sub	%globals,5,%globals
	sll	%globals,2,%globals
1:	subcc	%globals,4,%globals
	ld	[%l5+%globals],%l6
	bne	1b
	st	%l6,[%l4+%globals]

	ld	[%closure+object_offset],%arg0
	set	N(env_values),%globals
	mov	%i0,%o0
	ld	[%globals],%globals
	mov	%i1,%o1
	mov	%i2,%o2
	set	N(xcount),%xcountr
	mov	%i3,%o3
	ld	[%xcountr],%xcountr
	mov	%i4,%o4
	call	%arg0+object_offset+function_offset
	mov	%i5,%o5

	/* Restore frame information */
	set	N(ccontext),%scratch
	st	%l3,[%scratch+cc_frame_start]
	st	%l2,[%scratch+cc_frame_end]
	POP_CALL_STACK

	ret
	restore	%o0,0,%o0

	.global	N(interpreter_invoke)

N(interpreter_invoke):
	/* Save argcount */
	set	N(interpret_nargs),%l1
	st	%argcount,[%l1]

	set	N(interpret_closure),%l1
	st	%o5,[%l1]
	/* Fetch 6th argument from stack */
	ld	[%sp+argstart+5*4],%o5
	mov	%o7,%l1
	CCALL(N(interpreter_start))
	clr	%o1
	clr	%o2
	clr	%o3
	clr	%o4
	jmpl	%l1+8,%g0
	clr	%o5

	.global	N(flush_icache)

N(flush_icache):
	sub	%o1,%o0,%o1	/* %o1 = nbytes */
	and	%o1,0x1F,%o2	/* m <- (nbytes % (32-1)) >> 2 (use %o2 for m) */
	srl	%o2,2,%o2
	srl	%o1,5,%o1	/* i <- (nbytes >> 5) */
/* FLUSH4 implements: if (m > 0) { FLUSH addr; addr += 4; m--;} else goto L_test */
#define FLUSH4					\
		tst	%o2;			\
		ble	L_test;			\
		nop;				\
		iflush	%o0;			\
		inc	4,%o0;			\
		dec	1,%o2
	FLUSH4
	FLUSH4
	FLUSH4
	FLUSH4
	FLUSH4
	FLUSH4
	FLUSH4
				/* addr is 32-byte aligned here */
L_test:
	tst	%o1
	be	L_exit
	nop
L_loop:				/* flush 32 bytes per iteration */
	iflush	%o0
	iflush	%o0+8
	iflush	%o0+16
	iflush	%o0+24
	deccc	1,%o1		/* if (--i > 0) goto L_loop */
	bg	L_loop
	inc	32,%o0		/* addr += 32 (delay slot) */
L_exit:
	retl
	nop

	.global	N(flush_windows)

N(flush_windows):
	save	%sp,-minframe,%sp
	save	%sp,-minframe,%sp
	save	%sp,-minframe,%sp
	save	%sp,-minframe,%sp
	save	%sp,-minframe,%sp
	save	%sp,-minframe,%sp
	save	%sp,-minframe,%sp
	save	%sp,-minframe,%sp
	save	%sp,-minframe,%sp
	save	%sp,-minframe,%sp
	save	%sp,-minframe,%sp
	save	%sp,-minframe,%sp
	save	%sp,-minframe,%sp
	save	%sp,-minframe,%sp
	save	%sp,-minframe,%sp
	save	%sp,-minframe,%sp
	save	%sp,-minframe,%sp
	save	%sp,-minframe,%sp
	save	%sp,-minframe,%sp
	save	%sp,-minframe,%sp
	save	%sp,-minframe,%sp
	save	%sp,-minframe,%sp
	save	%sp,-minframe,%sp
	save	%sp,-minframe,%sp
	save	%sp,-minframe,%sp
	save	%sp,-minframe,%sp
	save	%sp,-minframe,%sp
	save	%sp,-minframe,%sp
	save	%sp,-minframe,%sp
	save	%sp,-minframe,%sp
	save	%sp,-minframe,%sp
	save	%sp,-minframe,%sp
	restore
	restore
	restore
	restore
	restore
	restore
	restore
	restore
	restore
	restore
	restore
	restore
	restore
	restore
	restore
	restore
	restore
	restore
	restore
	restore
	restore
	restore
	restore
	restore
	restore
	restore
	restore
	restore
	restore
	restore
	restore
	ret
	restore
