/*
 * Copyright (c) 1993-2004 David Gay and Gustav Hållberg
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

/* Warning: don't change order of ops without looking at runtime.c
   (catchsegv).
*/

#include "options.h"

.section	.rodata

#ifndef NOCOMPILER
	
#define N(x) x
	
/* Important constants ... */
#define true 3
#define false 1
#define null 0
#define undefined 85

/* Register assignments */
#define arg0 %eax
#define arg1 %ecx
#define globals %esi
#define argcount %eax
#define argcountw %ax
#define closure %ebx
#define closure_in %edx

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
#define function_offset 36	/* start of code in mcode object */
#define primitive_op 4		/* primitive_ext in primitive */
#define primop_op 8		/* C function in primitive_ext */
#define primop_nargs 12		/* nargs in primitive_ext */
#define primop_seclevel 20	/* seclevel in primitive_ext */

/* Stack frame information */
#define argstart 8

/* ccontext structure def */
#define cc_frame_start 0
#define cc_frame_end_sp 4
#define cc_frame_end_bp 8
#define cc_callee 12
#define cc_caller 20
#define cc_retadr 28
#define cc_SIZE 32

/* call_stack structure def */
#define cs_next 0
#define cs_type 4
#define cs_compiled_SIZE 8 /* size for call_compiled */

#define call_compiled 2

/* External symbols used */
	.globl	N(ccontext)
	.globl	N(env_values)
	.globl	N(mvars)
	.globl	N(xcount)
	.globl	N(code_string_append)
	.globl	N(code_ref)
	.globl	N(posgen0)
	.globl	N(startgen0)
	.globl	N(minorgen)
	.globl	N(majorgen)
	.globl	N(garbage_collect)
	.globl	N(seclevel)
	.globl	N(interpreter_start)
	.globl	N(call_stack)

#ifdef GCSTATS
	.global N(gcstats)

#define GCSTAT_ADD(type, size)					\
	incl	N(gcstats) + 4 * (5 + 22 * 6 + type);		\
	add	size,N(gcstats) + 4 * (5 + 22 * 7 + type)

#else  /* !GCSTATS */
#define GCSTAT_ADD(type, size)
#endif /* !GCSTATS */

	
/* Call C function (CCALL_SECURE needs %eax) */

#define END_FRAME_NOPC \
	mov	%esp,N(ccontext)+cc_frame_end_sp; \
	mov	%ebp,N(ccontext)+cc_frame_end_bp

#define END_FRAME_PC(n) \
	lea	4*(1+n)(%esp),%eax; \
	mov	%eax,N(ccontext)+cc_frame_end_sp; \
	mov	%ebp,N(ccontext)+cc_frame_end_bp

#define END_FRAME_PC_ecx(n) \
	lea	4*(1+n)(%esp),%ecx; \
	mov	%ecx,N(ccontext)+cc_frame_end_sp; \
	mov	%ebp,N(ccontext)+cc_frame_end_bp

#define CCALL_LEAF(fn) \
	call	fn
			
#define CCALL(fn) \
	movw	$DEFAULT_SECLEVEL,N(seclevel); \
	call	fn

			
#define CCALL_SECURE(fn) \
	mov	object_offset(closure),%eax; \
	mov	object_offset+mcode_seclevel(%eax),%ax; \
	mov	%ax,N(seclevel); \
	call	fn

/* Various ways of preserving the registers:
   SAVE_CALLEE/RESTORE_CALLEE: preserve value of callee saved registers
     only (caller saved are cleared in RESTORE)
   SAVE_CALLER/RESTORE_CALLER: preserve value of callee and caller registers
   CLEAR_CALLER: preserve no registers, just clear caller saved on return
     (assumes that called fn is NOALLOC)
*/
#define SAVE_CALLEE \
	mov	%ebx,N(ccontext)+cc_callee; \
	mov	%esi,N(ccontext)+cc_callee+4
#define RESTORE_CALLEE \
	mov	N(ccontext)+cc_callee,%ebx; \
	mov	N(ccontext)+cc_callee+4,%esi; \
	CLEAR_CALLER

#define SAVE_CALLER \
	mov	%ebx,N(ccontext)+cc_callee; \
	mov	%esi,N(ccontext)+cc_callee+4; \
	mov	%edx,N(ccontext)+cc_caller; \
	mov	%edi,N(ccontext)+cc_caller+4
#define RESTORE_CALLER \
	mov	N(ccontext)+cc_callee,%ebx; \
	mov	N(ccontext)+cc_callee+4,%esi; \
	mov	N(ccontext)+cc_caller,%edx; \
	mov	N(ccontext)+cc_caller+4,%edi

#define CLEAR_CALLER \
	xor	%edx,%edx; \
	xor	%edi,%edi

#define SAVE_RETADR \
	pop	%eax; \
	mov	%eax,N(ccontext)+cc_retadr

#define RESTORE_RETADR \
	push	N(ccontext)+cc_retadr
			
/* Simple integer manipulation macros */
#define SETINT(x) \
	bts	$0,x		/* set integer type bit */

#define CLEARINT(x) \
	btr	$0,x		/* clear integer type bit */

#define INTVAL(x) \
	sar	$1,x		/* Make into normal integer */

#define MAKEINT(x) \
	add	x,x; \
	SETINT(x)

#define ISINT(x) \
	testl	$1,x		/* Is x an integer ? */

#define ISNULL(x) \
	test	x,x		/* Is x NULL (registers only) */

#define ISUNALIGNED(x) \
	testl	$3,x

#define ARG0_IS_READ_WRITE				\
	testl	$flag_readonly,object_flags(arg0);	\
	jne	E(error_value_read_only)

#ifdef GCDEBUG_CHECK
#define GCCHECK(reg) \
	cmp	null,%reg; \
	be	6f; \
	ISINT(reg) \
	bne	6f; \
	nop; \
	set	N(xcount),%scratch2; \
	st	%xcountr,[%scratch2); \
	ld	[%reg+object_gen),%scratch2; \
	btst	1,%scratch2; \
	be	8f; \
	nop; \
	set	N(minorgen),%xcountr; \
	ld	[%xcountr),%xcountr; \
	ba,a	7f; \
8:	set	N(majorgen),%xcountr; \
	ld	[%xcountr),%xcountr; \
7:	cmp	%xcountr,%scratch2; \
	be	6f; \
	mov	%o7,%scratch2; \
	call	N(abort); \
	nop; \
6:	set	N(xcount),%xcountr; \
	ld	[%xcountr),%xcountr;
#else
#define GCCHECK(reg)
#endif
	
#define CHECK_LOOP				\
	decl	N(xcount);			\
	je	E(error_loop);			\
	cmpl    (N(mudlle_stack_limit)),%esp;	\
	jb	E(error_recurse)

	.globl	N(abort)

#define E(n) N(b ## n)
	
/* Simple integer operations: +, /, % */

#define INT1ARG \
	ISINT(arg0); \
	je	E(error_bad_type)

#define INT2ARGS \
	ISINT(arg0); \
	je	E(error_bad_type)
	ISINT(arg1); \
	je	E(error_bad_type)

	.globl	N(bmultiply)

N(bmultiply):
	btr	$0,arg0
	jnc	E(error_bad_type)
	sar	$1,arg1
	jnc	E(error_bad_type)
	push	%edx
	imul	arg1		/* Implicit arg is %eax = arg0 */
	SETINT(arg0)
	pop	%edx
	ret

	.globl	N(bdivide)

N(bdivide):
	sar	$1,arg1
	jnc	E(error_bad_type)
	jz	E(error_divide_by_zero)
	btr	$0,arg0
	jnc	E(error_bad_type)
	push	%edx
	cdq
	idiv	arg1
	SETINT(arg0)
	pop	%edx
	ret

	.globl	N(bremainder)

N(bremainder):
	sar	$1,arg0
	jnc	E(error_bad_type)
	sar	$1,arg1
	jnc	E(error_bad_type)
	jz	E(error_divide_by_zero)
	push	%edx
	cdq
	idiv	arg1
	mov	%edx,arg0
	MAKEINT(arg0)
	pop	%edx
	ret

	.globl	N(bshift_left)

N(bshift_left):
	sar	$1,arg1
	jnc	E(error_bad_type)
	btr	$0,arg0
	jnc	E(error_bad_type)
	shl	%cl,arg0
	SETINT(arg0)
	ret
	
	.globl	N(bshift_right)

N(bshift_right):
	sar	$1,arg1
	jnc	E(error_bad_type)
	testl	$1,arg0
	je	E(error_bad_type)
	sar	%cl,arg0
	SETINT(arg0)
	ret


/* Call C code: ref, + */

	.globl	N(badd)

N(badd):	
	ISINT(arg0)
	je	cadd
	ISINT(arg1)
	je	cadd

	/* Integer addition */
	CLEARINT(arg1)
	add	arg1,arg0
	ret

cadd:	/* Call string_append */
	SAVE_CALLER
	push	arg1
	push	arg0
	END_FRAME_PC(2)
	CCALL_LEAF(N(code_string_append))
	add	$8,%esp
	RESTORE_CALLER
	ret

	.globl	N(bref)
	
N(bref):	
	ISINT(arg0)
	jne	cref

	cmpb	$type_vector,object_type(arg0)
	je	vectorref
	cmpb	$type_string,object_type(arg0)
	je	stringref

cref:	/* Call ref - only need to save %edx (ref doesn't alloc) */
	mov	%edx,N(ccontext)+cc_caller
	push	arg1
	push	arg0
	END_FRAME_PC(2)
	CCALL_LEAF(N(code_ref))
	add	$8,%esp
	mov	N(ccontext)+cc_caller,%edx
	ret

vectorref:
	ISINT(arg1)
	je	E(error_bad_type)
	add	arg1,arg1
	js	E(error_bad_index)
	add	$object_offset-2,arg1
	cmp	object_size(arg0),arg1
	jae	E(error_bad_index)
	mov	(arg0,arg1),arg0
	ret

stringref:
	ISINT(arg1)
	je	E(error_bad_type)
	INTVAL(arg1)
	js	E(error_bad_index)
	add	$object_offset+1,arg1
	cmp	object_size(arg0),arg1
	jae	E(error_bad_index)
	movzbl	-1(arg0,arg1),arg0
	MAKEINT(arg0)
	ret

	.globl	N(bset)
	
N(bset):	
	mov	4(%esp),%edx
	ISINT(arg0)
	jne	cset

	cmpb	$type_vector,object_type(arg0)
	je	vectorset
	cmpb	$type_string,object_type(arg0)
	je	stringset

cset:	/* Call set! */
	SAVE_CALLEE
	push	%edx
	push	arg1
	push	arg0
	END_FRAME_PC(3)
	CCALL_LEAF(N(code_set))
	add	$12,%esp
	RESTORE_CALLEE
	ret

vectorset:
	ISINT(arg1)
	je	E(error_bad_type)
	add	arg1,arg1
	js	E(error_bad_index)
	ARG0_IS_READ_WRITE
	add	$object_offset-2,arg1
	cmp	object_size(arg0),arg1
	jae	E(error_bad_index)
	mov	%edx,(arg0,arg1)
	mov	%edx,arg0
	ret

stringset:
	INTVAL(%edx)
	ISINT(arg1)
	je	E(error_bad_type)
	INTVAL(arg1)
	js	E(error_bad_index)
	ARG0_IS_READ_WRITE
	add	$object_offset+1,arg1
	cmp	object_size(arg0),arg1
	jae	E(error_bad_index)
	mov	%dl,-1(arg0,arg1)
	MAKEINT(%edx)
	mov	%edx,arg0
	ret

/* Special ops: bcleargc, bwglobal */

	.globl	N(bwglobal)

N(bwglobal):
	sub	globals,arg0
	mov	N(mvars),arg1
	ISINT((arg1,arg0))
	je	E(error_variable_read_only)
	ret

/* Preambles:
   bcleargc: clear stack between sp and bp, check argcount and seclevel
   bvarargs: clear stack between sp and bp, check seclevel and build
     vector of arguments
   Register usage:
     in: argcount(eax), closure_in(edx)
     out: closure_in(edx) unchanged
          arg vector in arg0(eax) for bvarargs
	  edi=0
     scratch: eax, ecx, edi
*/

	.globl	N(bcleargc)

/* Clear the allocated stack frame (between caller's %esp and %ebp) */

N(bcleargc):
	jne	E(error_wrong_parameters)
	CHECK_LOOP
	mov	object_offset(closure_in),%eax
	movw	object_offset+mcode_seclevel(%eax),%ax
	cmpw	N(minlevel),%ax
	jb	E(error_security_violation)

	/* Clear stack frame */
	xor	%eax,%eax
	lea	4(%esp),%edi
	mov	%ebp,%ecx
	sub	%edi,%ecx
	sar	$2,%ecx
	rep
	stosl
	xor	%edi,%edi
	ret

	.globl	N(bcleargc0)

N(bcleargc0):
	jne	E(error_wrong_parameters)
	CHECK_LOOP
	mov	object_offset(closure_in),%eax
	movw	object_offset+mcode_seclevel(%eax),%ax
	cmpw	N(minlevel),%ax
	jb	E(error_security_violation)
	ret

	.globl	N(bcleargc1)

N(bcleargc1):
	jne	E(error_wrong_parameters)
	CHECK_LOOP
	mov	object_offset(closure_in),%eax
	movw	object_offset+mcode_seclevel(%eax),%ax
	cmpw	N(minlevel),%ax
	jb	E(error_security_violation)
	mov	closure_in,4(%esp)
	ret

	.globl	N(bcleargc2)

N(bcleargc2):
	jne	E(error_wrong_parameters)
	CHECK_LOOP
	mov	object_offset(closure_in),%eax
	movw	object_offset+mcode_seclevel(%eax),%ax
	cmpw	N(minlevel),%ax
	jb	E(error_security_violation)
	mov	closure_in,4(%esp)
	mov	closure_in,8(%esp)
	ret

	.globl	N(bvarargs)

/* Build varargs vector & count */
N(bvarargs):
	CHECK_LOOP
	mov	object_offset(closure_in),%ecx
	movw	object_offset+mcode_seclevel(%ecx),%cx
	cmpw	N(minlevel),%cx
	jb	E(error_security_violation)

	/* in: argcount: argument count, arguments on stack */
	/* returns vector in %eax */
	push	argcount

	/* Clear stack frame */
	xor	%eax,%eax
	lea	8(%esp),%edi
	mov	%ebp,%ecx
	sub	%edi,%ecx
	sar	$2,%ecx
	rep
	stosl

	pop	%ecx
	
	/* Allocate argument vector */
valloc:	lea	object_offset(,%ecx,4),%edi
	mov	N(posgen0),arg0
	sub	%edi,arg0
	cmp	N(startgen0),arg0
	jb	vargc
	/* success */
	mov	arg0,N(posgen0)
	mov	%edi,object_size(arg0)
	GCSTAT_ADD(type_vector, %edi)
#ifdef GCDEBUG
	mov	N(minorgen),%edi
	mov	%edi,object_gen(arg0)
#endif
	movl	$garbage_record | type_vector << 8,object_info(arg0)
	
	/* Copy %ecx args from bp+argstart */
	push	%esi
	lea	argstart(%ebp),%esi
	lea	object_offset(arg0),%edi
	rep
	movsl

	pop	%esi

	xor	%edi,%edi
	ret

vargc:	END_FRAME_PC(0)
	push	%ecx
	push	%edi
	xor	%edi,%edi
	SAVE_CALLER
	CCALL_LEAF(N(garbage_collect))
	RESTORE_CALLER
	add	$4,%esp
	pop	%ecx
	jmp	valloc


/* Storage allocation operations */

	/* Alloc variable cell, return in arg1 */
	.globl	N(balloc_variable)
	
N(balloc_variable):
	mov	N(posgen0),arg1
	sub	$object_offset + 4,arg1
	cmp	N(startgen0),arg1
	jb	vgc
	/* success */
	mov	arg1,N(posgen0)
	movl	$object_offset + 4,object_size(arg1)
	GCSTAT_ADD(type_variable, $object_offset + 4)
#ifdef GCDEBUG
	mov	N(minorgen),arg0
	mov	arg0,object_gen(arg1)
#endif
	movl	$garbage_record | type_variable << 8,object_info(arg1)
	ret

vgc:	SAVE_CALLER
	END_FRAME_PC(0)
	push	$object_offset + 4
	CCALL_LEAF(N(garbage_collect))
	add	$4,%esp
	RESTORE_CALLER
	jmp	N(balloc_variable)


	.globl	N(balloc_closure)
	
	/* size of closure is in arg1. Return result in arg1 */
N(balloc_closure):
	mov	N(posgen0),arg0
	sub	arg1,arg0
	cmp	N(startgen0),arg0
	jb	cgc
	/* success */
	mov	arg0,N(posgen0)
	mov	arg1,object_size(arg0)
	GCSTAT_ADD(type_closure, arg1)
#ifdef GCDEBUG
	mov	N(minorgen),arg1
	mov	arg1,object_gen(arg0)
#endif
	movl	$garbage_record | type_closure << 8 | flag_readonly << 16,object_info(arg0)
	mov	arg0,arg1
	ret

cgc:	SAVE_CALLER
	END_FRAME_PC(0)
	push	arg1
	push	arg1
	CCALL_LEAF(N(garbage_collect))
	add	$4,%esp
	pop	arg1
	RESTORE_CALLER
	jmp	N(balloc_closure)

	/* Allocate cons cell, return in arg1 */
	.globl	N(balloc_cons)

N(balloc_cons):
	mov	N(posgen0),arg1
	sub	$pair_size,arg1
	cmp	N(startgen0),arg1
	jb	consgc
	/* success */
	mov	arg1,N(posgen0)
	movl	$pair_size,object_size(arg1)
	GCSTAT_ADD(type_list, $pair_size)
#ifdef GCDEBUG
	mov	N(minorgen),arg0
	mov	arg0,object_gen(arg1)
#endif
	movl	$garbage_record | type_list << 8,object_info(arg1)
	ret

consgc:	SAVE_CALLER
	END_FRAME_PC(0)
	pushl	$pair_size
	CCALL_LEAF(N(garbage_collect))
	add	$4,%esp
	RESTORE_CALLER
	jmp	N(balloc_cons)

/* Interface operations:
    bcall: any call from machine language
    interpreter_invoke: machine language -> interpreter
    mc_invoke: C code -> machine language
*/

	.globl	N(bcall)

/* Call using standard x86 conventions
   closure_in contains called fn
*/
N(bcall):
	ISUNALIGNED(closure_in)
	jne	E(error_bad_function)

	cmpb	$type_closure,object_type(closure_in)
	je	call_closure

	cmpb	$type_primitive,object_type(closure_in)
	je	call_primitive

	cmpb	$type_secure,object_type(closure_in)
	je	E(error_security_violation) /* have to call these via bcall_secure */

	cmpb	$type_varargs,object_type(closure_in)
	jne	E(error_bad_function)

call_varargs:
	SAVE_CALLEE
	mov	object_offset+primitive_op(closure_in),closure_in
	mov	primop_op(closure_in),closure_in /* Fetch actual C function */
	END_FRAME_PC_ecx(0)
	push	argcount
	call	makevarargs
	push	%eax

	CCALL(*closure_in)
	RESTORE_CALLEE
	add	$8,%esp
	ret

call_primitive:
	/* Check arg count */
	mov	object_offset+primitive_op(closure_in),closure_in
	cmpw	primop_nargs(closure_in),%ax
	jne	E(error_wrong_parameters)

	SAVE_RETADR
	END_FRAME_NOPC
	mov	primop_op(closure_in),closure_in /* Fetch actual C function */
	SAVE_CALLEE
	CCALL(*closure_in)
	RESTORE_CALLEE
	RESTORE_RETADR
	ret

call_closure:
	mov	object_offset(closure_in),%ecx
	add	$object_offset+function_offset,%ecx
	jmp	*%ecx


/* Build varargs vector & count */
makevarargs: /* preserves closure, closure_in, globals (ie ebx, edx, esi) */
	/* in: argcount: argument count, arguments on stack */
	/* returns vector in %eax */
	push	%edx
	lea	object_offset(,argcount,4),%edx

	/* Allocate argcount bytes for argument vector */
valloc2:
	mov	N(posgen0),arg0
	sub	%edx,arg0
	cmp	N(startgen0),arg0
	jb	vargc2
	/* success */
	mov	arg0,N(posgen0)
	mov	%edx,object_size(arg0)
	GCSTAT_ADD(type_vector, %edx)
#ifdef GCDEBUG
	mov	N(minorgen),%ecx
	mov	%ecx,object_gen(arg0)
#endif
	movl	$garbage_record | type_vector << 8,object_info(arg0)

	/* Copy args from sp+16 */
	lea	-object_offset(%edx),%ecx
	shr	$2,%ecx
	mov	%esi,%edx
	lea	16(%esp),%esi
	lea	object_offset(arg0),%edi
	rep
	movsl
	mov	%edx,%esi
	pop	%edx
	ret

vargc2: push	%edx
	push	%edx
	CCALL_LEAF(N(garbage_collect))
	add	$4,%esp
	RESTORE_CALLEE
	pop	%edx
	jmp	valloc2

	.globl	N(bcall_secure)
	
N(bcall_secure):
	mov	object_offset+primitive_op(closure_in),closure_in
	/* Check security level */
	mov	object_offset(closure),%ecx
	movw	primop_seclevel(closure_in),%di
	cmpw	%di,object_offset+mcode_seclevel(%ecx)
	jb	E(error_security_violation)
	/* Check arg count */
	cmpw	argcountw,primop_nargs(closure_in)
	jne	E(error_wrong_parameters)

	mov	primop_op(closure_in),closure_in /* Fetch actual C function */

	SAVE_RETADR
	END_FRAME_NOPC
	SAVE_CALLEE
	CCALL_SECURE(*closure_in)
	RESTORE_CALLEE
	RESTORE_RETADR
	ret

	.globl	N(bcall_varargs)

N(bcall_varargs):
	SAVE_CALLEE
	mov	object_offset+primitive_op(closure_in),closure_in
	mov	primop_op(closure_in),closure_in /* Fetch actual C function */
	END_FRAME_PC_ecx(0)
	push	argcount
	call	makevarargs
	push	%eax

	CCALL_SECURE(*closure_in)
	RESTORE_CALLEE
	add	$8,%esp
	ret

	.globl	N(interpreter_invoke)

N(interpreter_invoke):
	mov	argcount,N(interpret_nargs)
	SAVE_RETADR
	push	closure_in
	END_FRAME_NOPC
	SAVE_CALLEE
	CCALL(N(interpreter_start))
	add	$4,%esp
	RESTORE_CALLEE
	RESTORE_RETADR
	ret

#define START_INVOKE \
	push	%ebp; \
	mov	%esp,%ebp; \
	push	%ebx; \
	push	%esi; \
	push	%edi; \
	sub	$cc_SIZE+cs_compiled_SIZE,%esp; \
	SAVE_CCONTEXT; \
	PUSH_CALL_STACK

#define END_INVOKE \
	RESTORE_CCONTEXT; \
	POP_CALL_STACK; \
	lea	-12(%ebp),%esp; \
	pop	%edi; \
	pop	%esi; \
	pop	%ebx; \
	pop	%ebp; \
	ret

#define SAVE_CCONTEXT \
	lea	N(ccontext),%esi; \
	mov	%esp,%edi; \
	mov	$cc_SIZE >> 2,%ecx; \
	rep; \
	movsl; \
	mov	%ebp,N(ccontext)+cc_frame_start

#define RESTORE_CCONTEXT \
	lea	-(12+cc_SIZE+cs_compiled_SIZE)(%ebp),%esi; \
	lea	N(ccontext),%edi; \
	mov	$cc_SIZE >> 2,%ecx; \
	rep; \
	movsl

/* Assumes call_stack entry address in edi */
#define PUSH_CALL_STACK \
	mov	$call_stack,%eax; \
	mov	(%eax),%ebx; \
	mov	%ebx,cs_next(%edi); \
	movl	$call_compiled,cs_type(%edi); \
	mov	%edi,(%eax)


/* Assumes call_stack entry address in esi */
#define POP_CALL_STACK \
	mov	cs_next(%esi),%ecx; \
	mov	%ecx,call_stack

#define PUSH_VECTORARGS \
	mov	object_size(%esi),%eax; \
	sub	$object_offset,%eax; \
	sub	%eax,%esp; \
	sar	$1,%eax; \
	sar	$1,%eax; \
	mov	%eax,%ecx; \
	add	$object_offset,%esi; \
	mov	%esp,%edi; \
	rep; \
	movsl

#define CALL_CLOSURE \
	xor	globals, globals; \
	xor	closure, closure; \
	CLEAR_CALLER; \
	mov	8(%ebp),closure_in; \
	mov	object_offset(closure_in),%ecx; \
	add	$object_offset+function_offset,%ecx; \
	call	*%ecx

	.globl	N(invoke0)
N(invoke0):
	START_INVOKE
	xor	%eax,%eax
	CALL_CLOSURE
	END_INVOKE
	
	.globl	N(invoke1)
N(invoke1):
	START_INVOKE
	mov	$1,%eax
	push	12(%ebp)
	CALL_CLOSURE
	END_INVOKE
	
	.globl	N(invoke2)
N(invoke2):
	START_INVOKE
	mov	$2,%eax
	push	16(%ebp)
	push	12(%ebp)
	CALL_CLOSURE
	END_INVOKE
	
	.globl	N(invoke3)
N(invoke3):
	START_INVOKE
	mov	$3,%eax
	push	20(%ebp)
	push	16(%ebp)
	push	12(%ebp)
	CALL_CLOSURE
	END_INVOKE
	
	.globl	N(invoke4)
N(invoke4):
	START_INVOKE
	mov	$4,%eax
	push	24(%ebp)
	push	20(%ebp)
	push	16(%ebp)
	push	12(%ebp)
	CALL_CLOSURE
	END_INVOKE
	
	.globl	N(invoke1plus)
N(invoke1plus):
	START_INVOKE
	mov	16(%ebp),%esi
	PUSH_VECTORARGS
	inc	%eax
	push	12(%ebp)
	CALL_CLOSURE
	END_INVOKE
	
	.globl	N(invoke)
N(invoke):
	START_INVOKE
	mov	12(%ebp),%esi
	PUSH_VECTORARGS
	CALL_CLOSURE
	END_INVOKE
	

	/* Put errorstubs last so branch prediction will predict correctly
	 * (ppro and most others predic contitional jumps forwards as not
	 * taken)
	 */
		
#define ERRORSTUB(n) \
	.globl	N(b ## n); \
N(b ## n):; \
	END_FRAME_PC(0); \
	push	$n; \
	CCALL_LEAF(N(runtime_error))

ERRORSTUB(error_bad_function)
ERRORSTUB(error_stack_underflow)
ERRORSTUB(error_bad_type)
ERRORSTUB(error_divide_by_zero)
ERRORSTUB(error_bad_index)
ERRORSTUB(error_bad_value)
ERRORSTUB(error_variable_read_only)
ERRORSTUB(error_loop)
ERRORSTUB(error_recurse)
ERRORSTUB(error_wrong_parameters)
ERRORSTUB(error_security_violation)
ERRORSTUB(error_value_read_only)
ERRORSTUB(error_user_interrupt)

#endif
