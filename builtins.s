	.seg	"text"

#include "options.h"

/* Important constants ... */
#define true 3
#define false 1
#define null 0

/* Register assignments */
#define arg0 l0
#define arg1 l1
#define xcount g1
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
#define env_values 8		/* values field in environment */
#define mcode_seclevel 0	/* seclevel in mcode */
#define function_offset 32	/* start of code in mcode object */
#define primitive_op 4		/* primitive_ext in primitive */
#define primop_op 8		/* C function in primitive_ext */
#define primop_nargs 12		/* nargs in primitive_ext */
#define primop_seclevel 20	/* seclevel in primitive_ext */

/* Stack frame information */
#define argstart 68
#define minframe 96

/* External symbols used */
	.global	_frame_start
	.global	_frame_end
	.global	_env_values
	.global	_mvars
	.global	_xcount
	.global	_code_string_append
	.global	_code_ref
	.global	_posgen0
	.global	_startgen0
	.global	_minorgen
	.global	_majorgen
	.global	_garbage_collect
	.global	_seclevel
	.global	_interpreter_start

/* Call C function, preserving global state, etc (needs l0) */

#define CCALL_LEAF_NOALLOC(fn) \
	call	fn; \
	mov	%xcount,%l0; \
	set	_env_values,%globals; \
	mov	%l0,%xcount; \
	ld	[%globals],%globals;
			
#define CCALL_LEAF(fn) \
	set	_frame_end,%l0; \
	st	%sp,[%l0]; \
	call	fn; \
	mov	%xcount,%l0; \
	set	_env_values,%globals; \
	mov	%l0,%xcount; \
	ld	[%globals],%globals;
			
#define CCALL(fn) \
	set	_frame_end,%l0; \
	st	%sp,[%l0]; \
	ld	[%closure+object_offset],%l0; \
	set	_seclevel,%globals; \
	lduh	[%l0+object_offset+mcode_seclevel],%l0; \
	sth	%l0,[%globals]; \
	set	_xcount,%l0; \
	call	fn; \
	st	%xcount,[%l0]; \
	set	_env_values,%globals; \
	set	_xcount,%l0; \
	ld	[%globals],%globals; \
	ld	[%l0],%xcount;
			
/* Simple integer manipulation macros */
#define SETINT(reg) \
	bset	1,%reg;		/* set integer type bit */

#define CLEARINT(reg) \
	bclr	1,%reg;		/* clear integer type bit */

#define INTVAL(reg) \
	sra	%reg,1,%reg;	/* Make into normal integer */

#define INTVALTO(reg1, reg2) \
	sra	%reg1,1,%reg2;

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
	set	_xcount,%scratch2; \
	st	%xcount,[%scratch2]; \
	ld	[%reg+object_gen],%scratch2; \
	btst	1,%scratch2; \
	be	8f; \
	nop; \
	set	_minorgen,%xcount; \
	ld	[%xcount],%xcount; \
	ba,a	7f; \
8:	set	_majorgen,%xcount; \
	ld	[%xcount],%xcount; \
7:	cmp	%xcount,%scratch2; \
	be	6f; \
	mov	%o7,%scratch2; \
	call	_abort; \
	nop; \
6:	set	_xcount,%xcount; \
	ld	[%xcount],%xcount;
#else
#define GCCHECK(reg)
#endif
	

	.global	_abort
	
/* Boolean builtins: and, or, not */

	.global	_bor

_bor:	or	%arg0,%arg1,%arg0
	cmp	%arg0,false
	bne,a	1f
	or	%g0,true,%arg0
1:	retl
	nop


	.global	_band

_band:	cmp	%arg0,false
	set	true,%arg0
	be,a	1f
	or	%g0,false,%arg0
1:	cmp	%arg1,false
	be,a	2f
	or	%g0,false,%arg0
2:	retl
	nop

	.global	_bnot

_bnot:	cmp	%arg0,false
	set	true,%arg0
	bne,a	1f
	or	%g0,false,%arg0
1:	retl
	nop

/* Comparisons: ==, != */

	.global	_bleq

_bleq:	cmp	%arg0,%arg1
	set	false,%arg0
	be,a	1f
	or	%g0,true,%arg0
1:	retl
	nop

	.global	_blne

_blne:	cmp	%arg0,%arg1
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

RELOP(_bllt, bl)
RELOP(_blle, ble)
RELOP(_blgt, bg)
RELOP(_blge, bge)

	.global	_bbitand

_bbitand:
	INT2ARGS
	retl
	and	%arg0,%arg1,%arg0

	.global	_bbitor

_bbitor:
	INT2ARGS
	retl
	or	%arg0,%arg1,%arg0

	.global	_bbitxor

_bbitxor:
	INT2ARGS
	xor	%arg0,%arg1,%arg0
	retl
	SETINT(arg0)

	.global	_bshift_left

_bshift_left:
	INT2ARGS
	INTVAL(arg1)
	CLEARINT(arg0)
	sll	%arg0,%arg1,%arg0
	retl
	SETINT(arg0)

	.global _bshift_right

_bshift_right:
	INT2ARGS
	INTVAL(arg1)
	sra	%arg0,%arg1,%arg0
	retl
	SETINT(arg0)

	.global	_bsubtract

_bsubtract:
	INT2ARGS
	CLEARINT(arg1)
	retl
	sub	%arg0,%arg1,%arg0

	.global	_bmultiply

_bmultiply:
	INT2ARGS
	CLEARINT(arg0)
	INTVAL(arg1)
	smul	%arg0,%arg1,%arg0
	retl
	SETINT(arg0)

	.global	_bdivide

_bdivide:
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

	.global	_bremainder

_bremainder:
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

	.global	_bnegate

_bnegate:
	INT1ARG
	neg	%arg0
	retl
	add	%arg0,2,%arg0

	.global	_bbitnot

_bbitnot:
	INT1ARG
	retl
	xnor	%arg0,1,%arg0
	

/* Call C code: ref, + */

	.global	_badd

_badd:	ISINT(arg0)
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

	CCALL_LEAF(_code_string_append)
	ret
	restore	%o0,0,%arg0

	.global	_bref
	
_bref:	ISINT(arg0)
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

	CCALL_LEAF(_code_ref)
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

	.global	_balloc_cons
	.global	_balloc_cons_l0

/* Note: arg0 is not available as it may contain something to be stored in the
   cons */
_balloc_cons_l0:
	set	_posgen0,%arg1
	ld	[%arg1],%arg1
	set	_startgen0,%scratch
	ld	[%scratch],%scratch
	sub	%arg1,pair_size,%arg1
	cmp	%arg1,%scratch
	blu	consgc_l0

	! success, init pair
	set	_posgen0,%scratch
	st	%arg1,[%scratch]

	set	pair_size,%scratch
	st	%scratch,[%arg1+object_size]
	sethi	%hi(garbage_record << 24 | type_list << 16),%scratch
#ifdef GCDEBUG
	st	%scratch,[%arg1+object_info]
	set	_minorgen,%scratch
	ld	[%scratch],%scratch
	retl
	st	%scratch,[%arg1+object_gen]
#else
	retl
	st	%scratch,[%arg1+object_info]
#endif


consgc_l0:
	mov	%arg0,%globals
	save	%sp,-minframe,%sp
	call	clear_frame
	nop
	mov	%globals,%l2
	mov	pair_size,%o0
	CCALL_LEAF(_garbage_collect)
	ba	_balloc_cons_l0
	restore	%l2,0,%arg0

	/* arg0 *is* available */
_balloc_cons:
	set	_posgen0,%arg0
	ld	[%arg0],%arg1
	set	_startgen0,%scratch
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
	set	_minorgen,%scratch
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
	CCALL_LEAF(_garbage_collect)
	ba	_balloc_cons
	restore

	.global	_bcar

_bcar:	lduh	[%arg0+object_info],%scratch
	cmp	%scratch,type_list | garbage_record << 8
	tne	error_bad_type+trap_offset
	retl
	ld	[%arg0+object_offset],%arg0

	.global	_bcdr

_bcdr:	lduh	[%arg0+object_info],%scratch
	cmp	%scratch,type_list | garbage_record << 8
	tne	error_bad_type+trap_offset
	retl
	ld	[%arg0+object_offset+4],%arg0

/* Special ops: bcompare, bcleargc, balloc */

	.global	_bwglobal

_bwglobal:
	set	_mvars,%scratch
	ld	[%scratch],%scratch
	ld	[%scratch+%arg0],%scratch
	ISINT(scratch)
	retl
	te	error_variable_read_only+trap_offset

	.global	_bcompare

_bcompare:
	INT2ARGS
	retl
	subcc	%arg0,%arg1,%g0

	.global	_bcleargc

/* Clear the allocated stack frame (between %sp+argstart+5*4 and %fp) */

_bcleargc:
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
	/* Check for infinite loops (xcount) */
2:	clr	%l2
	ld	[%closure+object_offset],%l0
	clr	%l3
	set	_minlevel,%l1
	clr	%l4
	lduh	[%l0+object_offset+mcode_seclevel],%l0
	clr	%l5
	subcc	%xcount,1,%xcount
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

	.global	_balloc_variable
	
_balloc_variable:
	set	_posgen0,%arg0
	ld	[%arg0],%arg0
	set	_startgen0,%scratch
	ld	[%scratch],%scratch
	mov	object_offset + 4, %arg1	! Variable size
	sub	%arg0,%arg1,%arg0
	cmp	%arg0,%scratch
	blu	vgc

	! success, init
	set	_posgen0,%scratch
	st	%arg0,[%scratch]

	st	%arg1,[%arg0+object_size]
#ifdef GCDEBUG
	set	_minorgen,%scratch
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
	CCALL_LEAF(_garbage_collect)
	ba	_balloc_variable
	restore


	.global	_balloc_closure
	
/* size of closure is in arg1 */
_balloc_closure:
	set	_posgen0,%arg0
	ld	[%arg0],%arg0
	set	_startgen0,%scratch
	ld	[%scratch],%scratch
	sub	%arg0,%arg1,%arg0
	cmp	%arg0,%scratch
	blu	cgc

	! success, init
	set	_posgen0,%scratch
	st	%arg0,[%scratch]

	st	%arg1,[%arg0+object_size]
#ifdef GCDEBUG
	set	_minorgen,%scratch
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
	CCALL_LEAF(_garbage_collect)
	ba	_balloc_closure
	restore


/* Interface operations:
    bcall: any call from machine language
    interpreter_invoke: machine language -> interpreter
    mc_invoke: C code -> machine language
*/

	.global	_bcall

/* Call using standard sparc conventions, except that
     o5 is the called closure
     the 6th argument is on the stack
*/
_bcall:	lduh	[%closure_in+object_info],%arg0
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
	call	_bvarargs
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
	lduh	[%arg1+object_offset+mcode_seclevel],%arg1
	cmp	%arg1,%arg0
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


	.global	_bvarargs

/* Build varargs vector & count */
_bvarargs:
	/* in: argcount: argument count, i0-i4+stack(fp): arguments */
	/* returns: i0: arg vector, i1: arg count */
	/* uses lx, ix, ox, but does not require them to be clean */
	sll	%argcount,2,%l1
	add	%l1,object_offset,%l1

	/* Allocate l1 bytes for argument vector */
valloc:	set	_posgen0,%l0
	ld	[%l0],%l2
	set	_startgen0,%l3
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
	set	_minorgen,%l3
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

mov	%arg1,%scratch
	save	%sp,-minframe,%sp

vargc:	mov	%o7,%l0
	call	clear_frame
	mov	%l1,%scratch
	mov	%l0,%l1		! l1 is forwarded in this frame !
	mov	%scratch,%o0
	CCALL_LEAF(_garbage_collect)
	mov	%l1,%o7
	ba	valloc
	mov	%o0,%l1

	.global	_bcall_primitive

/* Call using standard sparc conventions
   %scratch is the primitive containing the called function
*/
_bcall_primitive:
	mov	%o7,%l1
	CCALL(%scratch)
	clr	%o1
	clr	%o2
	clr	%o3
	clr	%o4
	jmpl	%l1+8,%g0
	clr	%o5

	.global	_bcall_primitive_leaf

/* Call using standard sparc conventions
     %scratch is the primitive containing the called function
     It is a "leaf" primitive (calls no mudlle functions)
*/
_bcall_primitive_leaf:
	mov	%o7,%l1
	CCALL_LEAF(%scratch)
	clr	%o1
	clr	%o2
	clr	%o3
	clr	%o4
	jmpl	%l1+8,%g0
	clr	%o5

	.global	_bcall_primitive_leaf_noalloc

/* Call using standard sparc conventions
     %scratch is the primitive containing the called function
     It is a "leaf" primitive (calls no mudlle functions) and does no
     allocation.
*/
_bcall_primitive_leaf_noalloc:
	mov	%o7,%l1
	CCALL_LEAF_NOALLOC(%scratch)
	clr	%o1
	clr	%o2
	clr	%o3
	clr	%o4
	jmpl	%l1+8,%g0
	clr	%o5

	.global	_mc_invoke

/* We receive args0-4 in o0-o4, closure in o5, argcount on stack */
_mc_invoke:
	save	%sp,-minframe,%sp

	/* Frame information */
	set	_frame_start,%scratch
	ld	[%scratch],%l3
	set	_frame_end,%l2
	ld	[%l2],%l2
	st	%sp,[%scratch]

	/* Clear 6th argument */
	clr	[%sp+argstart+5*4]
	/* And 7th (minframe has space for 2 args as it is rounded
	   upto a multiple of 16 */
	clr	[%sp+argstart+6*4]
	
	ld	[%closure+object_offset],%arg0
	set	_env_values,%globals
	mov	%i0,%o0
	ld	[%globals],%globals
	mov	%i1,%o1
	ld	[%fp+argstart+6*4],%argcount	! Fetch argcount
	mov	%i2,%o2
	set	_xcount,%xcount
	mov	%i3,%o3
	ld	[%xcount],%xcount
	mov	%i4,%o4
	call	%arg0+object_offset+function_offset
	mov	%i5,%o5

	/* Restore frame information */
	set	_frame_start,%scratch
	st	%l3,[%scratch]
	set	_frame_end,%scratch
	st	%l2,[%scratch]

	ret
	restore	%o0,0,%o0

	.global	_mc_invoke_vector

/* We receive args0-4 in o0-o4, closure in o5, argcount on stack,
   extra arguments vector, start offset in this vector.
   argcount is >= 6 */
_mc_invoke_vector:
	/* Calculate frame size */
	ld	[%sp+argstart+6*4],%globals	! Fetch argcount
	sll	%globals,2,%scratch
	add	%scratch,argstart+15,%scratch
	andn	%scratch,15,%scratch
	neg	%scratch

	save	%sp,%scratch,%sp

	/* Clear the last 12 bytes of the frame
	   (may be unused argument slots because of rounding, so
	   won't be initialised below, so will confuse the GC) */
	clr	[%fp-4]
	clr	[%fp-8]
	clr	[%fp-12]

	/* Frame information */
	set	_frame_start,%scratch
	ld	[%scratch],%l3
	set	_frame_end,%l2
	ld	[%l2],%l2
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
	set	_env_values,%globals
	mov	%i0,%o0
	ld	[%globals],%globals
	mov	%i1,%o1
	mov	%i2,%o2
	set	_xcount,%xcount
	mov	%i3,%o3
	ld	[%xcount],%xcount
	mov	%i4,%o4
	call	%arg0+object_offset+function_offset
	mov	%i5,%o5

	/* Restore frame information */
	set	_frame_start,%scratch
	st	%l3,[%scratch]
	set	_frame_end,%scratch
	st	%l2,[%scratch]

	ret
	restore	%o0,0,%o0

	.global	_interpreter_invoke

_interpreter_invoke:
	/* Save argcount */
	set	_interpret_nargs,%l1
	st	%argcount,[%l1]

	set	_interpret_closure,%l1
	st	%o5,[%l1]
	/* Fetch 6th argument from stack */
	ld	[%sp+argstart+5*4],%o5
	mov	%o7,%l1
	CCALL(_interpreter_start)
	clr	%o1
	clr	%o2
	clr	%o3
	clr	%o4
	jmpl	%l1+8,%g0
	clr	%o5

	.global	_flush_icache

_flush_icache:
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

	.global	_flush_windows

_flush_windows:
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
		