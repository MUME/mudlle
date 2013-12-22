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

/* Warning: don't change order of ops without looking at runtime.c
   (catchsegv).
*/

#include "options.h"

#  include "x86consts.h"

#ifdef __MACH__
.section	__TEXT,__text,regular,pure_instructions
#else
.section        .text
#endif

#ifndef NOCOMPILER

#ifdef __MACH__
#define N(x) _ ## x
#else
#define N(x) x
#endif

/* Important constants ... */
#define true 3
#define false 1
#define null 0
#define undefined ((42 << 1) | 1)

/* Register assignments */
#define arg0 %eax
#define arg0b %al
#define arg1 %ecx
#define arg1b %cl
#define globals %esi
#define argcount %eax
#define argcountw %ax
#define closure %ebx
#define closure_in %edx
#define closure_inb %dl

/* Stack frame information */
#define argstart 8

#ifdef GCSTATS
#define GCSTAT_ADD(type, size)						\
	addl	$1,N(gcstats) + 4 * (5 + last_type * 6 + type);		\
	addl	size,N(gcstats) + 4 * (5 + last_type * 7 + type)

#else  /* !GCSTATS */
#define GCSTAT_ADD(type, size)
#endif /* !GCSTATS */


/* End mudlle value stack frame ending before N values and PC. */
#define END_FRAME_PC(n) \
	lea	4*(1+n)(%esp),%eax; \
	mov	%eax,N(ccontext)+cc_frame_end_sp; \
	mov	%ebp,N(ccontext)+cc_frame_end_bp

/* End mudlle frame ending at last frame (at push %ebp). Uses
   'scratch' as scratch register. */
#define END_PREVIOUS_FRAME_REG(scratch) \
        xor     scratch,scratch; \
        mov     scratch,N(ccontext)+cc_frame_end_bp; \
        mov     %ebp,N(ccontext)+cc_frame_end_sp

#define END_PREVIOUS_FRAME END_PREVIOUS_FRAME_REG(arg0)

#define CCALL_LEAF(fn) \
	call	fn

#define CCALL(fn) \
	movw	$DEFAULT_SECLEVEL,N(internal_seclevel); \
	call	fn

#define CCALL_SECURE(fn) \
	mov	object_offset(closure),%eax; \
	mov	object_offset+mcode_seclevel(%eax),%ax; \
	mov	%ax,N(internal_seclevel); \
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


/* Push the current function's input arguments on the stack.
     in: argcount
     clobbers: esi, edi, ecx
 */
#define REPUSH_ARGS \
        /* %esp -= 4 * nargs */ \
        mov     argcount,%ecx; \
	neg     %ecx; \
	lea     (%esp,%ecx,4),%esp; \
	/* Copy argcount args from ebp+argstart to esp */ \
	mov     argcount,%ecx; \
	lea	argstart(%ebp),%esi; \
	mov     %esp,%edi; \
        rep; \
        movsl

/* Simple integer manipulation macros */
#define SETINT(x) \
	or	$1,x		/* set integer type bit */

#define CLEARINT(x) \
	and	$~1,x		/* clear integer type bit */

#define INTVAL(x) \
	sar	$1,x		/* Make into normal integer */

#define MAKEINT(x) \
        lea     1(x,x),x

#define ISINT(x) \
	test	$1,x		/* Is x an integer ? */

#define ISINTB(x) \
	testb	$1,x		/* Is x an integer ? */

#define ISNULL(x) \
	test	x,x		/* Is x NULL (registers only) */

#define ISUNALIGNED(x) \
	test	$3,x

#define IS_READ_WRITE(x) \
        testb   $OBJ_READONLY,object_flags(x)

#define CHECK_LOOP				\
	subl	$1,N(xcount);			\
	je	E(error_loop);			\
	cmpl    (N(mudlle_stack_limit)),%esp;	\
	jb	E(error_recurse)

#define E(n) N(b ## n)

/* Simple integer operations: +, /, % */

	.globl	N(bmultiply)

N(bmultiply):
	btr	$0,arg0
	jnc	1f
	sar	$1,arg1
	jnc     0f
	push	%edx
	imul	arg1,arg0
	SETINT(arg0)
	pop	%edx
	ret

0:      /* restore argument values */
        SETINT(arg0)
	add     arg1,arg1
1:	mov     $N(code_multiply),closure
        jmp     call_binary_primitive

	.globl	N(bdivide)

N(bdivide):
	sar	$1,arg0
	jnc	0f
	sar	$1,arg1
	jnc	1f
	jz	2f
	push	%edx
	cdq
	idiv	arg1
	MAKEINT(arg0)
	pop	%edx
	ret

	/* restore arguments */
0:      add     arg0,arg0
        jmp     4f
1:      add     arg1,arg1
	jmp     3f
2:      MAKEINT(arg1)
3:      MAKEINT(arg0)
4:      mov     $N(code_divide),closure
        jmp     call_binary_primitive

	.globl	N(bremainder)

N(bremainder):
	sar	$1,arg0
	jnc	0f
	sar	$1,arg1
	jnc	1f
	jz	2f
	push	%edx
	cdq
	idiv	arg1
	mov     %edx,arg0
	MAKEINT(arg0)
	pop	%edx
	ret

	/* restore arguments */
0:      add     arg0,arg0
        jmp     4f
1:      add     arg1,arg1
	jmp     3f
2:      MAKEINT(arg1)
3:      MAKEINT(arg0)
4:      mov     $N(code_remainder),closure
        jmp     call_binary_primitive

	.globl	N(bshift_left)

N(bshift_left):
	sar	$1,arg1
	jnc	0f
	btr	$0,arg0
	jnc	1f
	shl	%cl,arg0
	SETINT(arg0)
	ret

0:      add     arg1,arg1
        jmp     2f
1:      MAKEINT(arg1)
2:      mov     $N(code_shift_left),closure
        jmp     call_binary_primitive

	.globl	N(bshift_right)

N(bshift_right):
	sar	$1,arg1
	jnc	0f
	testl	$1,arg0
	je	1f
	sar	%cl,arg0
	SETINT(arg0)
	ret

0:      add     arg1,arg1
        jmp     2f
1:      MAKEINT(arg1)
2:      mov     $N(code_shift_right),closure
        jmp     call_binary_primitive

call_binary_primitive:
	/* used to call primitive (in 'closure') to trigger errors
	   with call trace */
	push    %ebp
        mov     %esp,%ebp
        push    arg1
        push    arg0
        END_PREVIOUS_FRAME
        CCALL_LEAF(*closure)
        /* should never get here */
	add	$8,%esp
        CCALL_LEAF(N(abort))

/* Call C code: ref, + */

	.globl	N(badd)

N(badd):
	ISINT(arg0b)
	je	cadd
	ISINT(arg1b)
	je	cadd

	/* Integer addition */
	lea     -1(arg0,arg1),arg0
	ret

cadd:	/* Call plus primitive for string append */
	push    %ebp
        mov     %esp,%ebp
	SAVE_CALLER
	push	arg1
	push	arg0
	END_PREVIOUS_FRAME
	CCALL_LEAF(N(code_plus))
	add	$8,%esp
	RESTORE_CALLER
	leave
	ret

	.globl	N(btypeof)

N(btypeof):
        ISNULL(arg0)
        mov	$(2 * type_null) + 1,arg1
        je	0f

        ISINT(arg0b)
        mov	$(2 * type_integer) + 1,arg1
        jne	0f

        movzbl	object_type(arg0),arg1
        MAKEINT(arg1)

0:	mov	arg1,arg0
        ret

	.globl	N(bref)

N(bref):
	ISINT(arg0b)
	jne	cref

	cmpb	$type_vector,object_type(arg0)
	je	vectorref
	cmpb	$type_string,object_type(arg0)
	je	stringref

cref:	/* Call ref - only need to save %edx (ref doesn't alloc) */
        push    %ebp
        mov     %esp,%ebp
	mov	%edx,N(ccontext)+cc_caller
	push	arg1
	push	arg0
	END_PREVIOUS_FRAME
	CCALL_LEAF(N(code_ref))
	add	$8,%esp
	mov	N(ccontext)+cc_caller,%edx
	leave
	ret

vectorref:
	ISINT(arg1b)
	je	1f
	push    arg1
	add	arg1,arg1
	jo	0f
	jns	vecnosign

	add	object_size(arg0),arg1
	sub	$object_offset,arg1
	js	0f

vecnosign:
	add	$object_offset-2,arg1
	cmp	object_size(arg0),arg1
	jae	0f
	mov	(arg0,arg1),arg0
	add     $4,%esp            /* pop arg1 */
	ret

stringref:
	ISINT(arg1b)
	je	cref
	push    arg1
	INTVAL(arg1)
	jns	strnosign

	add	object_size(arg0),arg1
	sub	$object_offset+1,arg1
	js	0f

strnosign:
	add	$object_offset+1,arg1
	cmp	object_size(arg0),arg1
	jae	0f
	movzbl	-1(arg0,arg1),arg0
	MAKEINT(arg0)
	add     $4,%esp            /* pop arg1 */
	ret

	/* vectorref and stringref errors come here */
0:      pop     arg1
        jmp     cref

	.globl	N(bset)

N(bset):
	mov	4(%esp),%edx
	ISINT(arg0b)
	jne	cset

	cmpb	$type_vector,object_type(arg0)
	je	vectorset
	cmpb	$type_string,object_type(arg0)
	je	stringset

cset:	/* Call set! */
        push    %ebp
        mov     %esp,%ebp
	SAVE_CALLEE
	push	%edx
	push	arg1
	push	arg0
	END_PREVIOUS_FRAME
	CCALL_LEAF(N(code_set))
	add	$12,%esp
	RESTORE_CALLEE
        leave
	ret

vectorset:
	ISINT(arg1b)
	je	cset
	IS_READ_WRITE(arg0)
        jne     cset

	push    arg1
	add	arg1,arg1
	jo	0f
	jns	vecsetnosign

	add	object_size(arg0),arg1
	sub	$object_offset,arg1
	js	0f

vecsetnosign:
	add	$object_offset-2,arg1
	cmp	object_size(arg0),arg1
	jae	0f
	mov	%edx,(arg0,arg1)
	mov	%edx,arg0
	add     $4,%esp            /* pop arg1 */
	ret

	/* restore arg1 before calling cset for stack traces */
0:      pop     arg1
	jmp     cset

stringset:
	INTVAL(%edx)
	jnc	0f
	IS_READ_WRITE(arg0)
        jne     1f
	push    arg1
	INTVAL(arg1)
	jnc	2f
	jns	strsetnosign

	add	object_size(arg0),arg1
	sub	$object_offset+1,arg1
	js	2f

strsetnosign:
	add	$object_offset+1,arg1
	cmp	object_size(arg0),arg1
	jae	2f
	mov	%dl,-1(arg0,arg1)
	movzbl	%dl,arg0
	MAKEINT(arg0)
	add     $4,%esp         /* pop arg1 */
	ret

	/* restore args before calling cset for stack traces */
0:      add     %edx,%edx
        jmp     cset
2:      pop     arg1
1:      MAKEINT(%edx)
        jmp     cset

/* Special ops: bcleargc, bwglobal */

	.globl	N(bwglobal)

N(bwglobal):
	sub	globals,arg0
	mov	N(mvars),arg1
	ISINTB((arg1,arg0))
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
	jb	valloc_gc
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

valloc_gc:
	push    %ebp
        mov     %esp,%ebp
	push	%ecx
	push	%edi
	xor	%edi,%edi
	SAVE_CALLER
        END_PREVIOUS_FRAME
	CCALL_LEAF(N(garbage_collect))
	RESTORE_CALLER
	/* add	$4,%esp */
	movl	-4(%ebp),%ecx
	leave
	jmp	valloc


/* Storage allocation operations */

	/* Alloc variable cell, return in arg1 */
	.globl	N(balloc_variable)

N(balloc_variable):
	mov	N(posgen0),arg1
	sub	$object_offset + 4,arg1
	cmp	N(startgen0),arg1
	jb	alloc_variable_gc
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

alloc_variable_gc:
	push    %ebp
        mov     %esp,%ebp
	SAVE_CALLER
	push	$object_offset + 4
        END_PREVIOUS_FRAME
	CCALL_LEAF(N(garbage_collect))
	/* add	$4,%esp */
	RESTORE_CALLER
	leave
	jmp	N(balloc_variable)


	.globl	N(balloc_closure)

	/* size of closure is in arg1. Return result in arg1 */
N(balloc_closure):
	mov	N(posgen0),arg0
	sub	arg1,arg0
	cmp	N(startgen0),arg0
	jb	alloc_closure_gc
	/* success */
	mov	arg0,N(posgen0)
	mov	arg1,object_size(arg0)
	GCSTAT_ADD(type_closure, arg1)
#ifdef GCDEBUG
	mov	N(minorgen),arg1
	mov	arg1,object_gen(arg0)
#endif
	movl	$garbage_record | type_closure << 8 | OBJ_READONLY << 16, \
                  object_info(arg0)
	mov	arg0,arg1
	ret

alloc_closure_gc:
	push    %ebp
        mov     %esp,%ebp
	SAVE_CALLER
	push	arg1
	push	arg1
        END_PREVIOUS_FRAME
	CCALL_LEAF(N(garbage_collect))
	/* add	$4,%esp */
	movl	-4(%ebp),arg1
	RESTORE_CALLER
	leave
	jmp	N(balloc_closure)

	/* Allocate cons cell, return in arg1 */
	.globl	N(balloc_cons)

N(balloc_cons):
	mov	N(posgen0),arg1
	sub	$pair_size,arg1
	cmp	N(startgen0),arg1
	jb	alloc_cons_gc
	/* success */
	mov	arg1,N(posgen0)
	movl	$pair_size,object_size(arg1)
	GCSTAT_ADD(type_pair, $pair_size)
#ifdef GCDEBUG
	mov	N(minorgen),arg0
	mov	arg0,object_gen(arg1)
#endif
	movl	$garbage_record | type_pair << 8,object_info(arg1)
	ret

alloc_cons_gc:
	push    %ebp
        mov     %esp,%ebp
	SAVE_CALLER
	pushl	$pair_size
        END_PREVIOUS_FRAME
	CCALL_LEAF(N(garbage_collect))
	/* add	$4,%esp */
	RESTORE_CALLER
	leave
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
	ISUNALIGNED(closure_inb)
	jne	E(error_bad_function)

	movw	$DEFAULT_SECLEVEL,N(internal_seclevel)

	cmpb	$type_closure,object_type(closure_in)
	je	call_closure

	cmpb	$type_primitive,object_type(closure_in)
	je	call_primitive

	cmpb	$type_secure,object_type(closure_in)
	je	call_secure

	cmpb	$type_varargs,object_type(closure_in)
	jne	E(error_bad_function)

call_varargs:
	push    %ebp
        mov     %esp,%ebp

	SAVE_CALLEE
        /* Fetch actual C function */
	mov	object_offset+primitive_op(closure_in),closure_in
	mov	primop_op(closure_in),closure_in

	push    argcount        /* not used by makevarargs */
        END_PREVIOUS_FRAME_REG(%ecx)
	call	makevarargs
	push	%eax

	CCALL(*closure_in)
	RESTORE_CALLEE

        leave
	ret

call_secure:
	mov	object_offset+primitive_op(closure_in),%ecx
	movw	primop_seclevel(%ecx),%di
	cmpw	%di,N(internal_seclevel)
	jb	E(error_security_violation)
	/* fallthrough */

call_primitive:
	/* Check arg count */
	mov	object_offset+primitive_op(closure_in),closure_in
	cmpw	primop_nargs(closure_in),%ax
	jne	E(error_wrong_parameters)
        /* fallthrough; this label is here to un-confuse gdb */

call_primitive_tail:
	push    %ebp
        mov     %esp,%ebp

        /* Fetch actual C function */
	mov	primop_op(closure_in),closure_in

	SAVE_CALLEE
	REPUSH_ARGS
        END_PREVIOUS_FRAME
	CCALL(*closure_in)
	RESTORE_CALLEE

	leave
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
vararg_alloc:
	mov	N(posgen0),arg0
	sub	%edx,arg0
	cmp	N(startgen0),arg0
	jb	vararg_alloc_gc
	/* success */
	mov	arg0,N(posgen0)
	mov	%edx,object_size(arg0)
	GCSTAT_ADD(type_vector, %edx)
#ifdef GCDEBUG
	mov	N(minorgen),%ecx
	mov	%ecx,object_gen(arg0)
#endif
	movl	$garbage_record | type_vector << 8,object_info(arg0)

	/* Copy args from sp+20 (edx, pc, argcount, ebp, pc, args...) */
	lea	-object_offset(%edx),%ecx
	shr	$2,%ecx
	mov	%esi,%edx
	lea	20(%esp),%esi
	lea	object_offset(arg0),%edi
	rep
	movsl
	mov	%edx,%esi
	pop	%edx
	ret

vararg_alloc_gc:
	push    %ebp
        mov     %esp,%ebp
        push	%edx
	push	%edx
	CCALL_LEAF(N(garbage_collect))
	/* add	$4,%esp */
	RESTORE_CALLEE
	movl	-4(%ebp),%edx
	leave
	jmp	vararg_alloc

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
        /* fallthrough; this label is here to un-confuse gdb */

bcall_secure_tail:
	push    %ebp
	mov     %esp,%ebp

	mov	primop_op(closure_in),closure_in
	SAVE_CALLEE
	REPUSH_ARGS
        END_PREVIOUS_FRAME_REG(%ecx)
        CCALL_SECURE(*closure_in)
	RESTORE_CALLEE

	leave
	ret

	.globl	N(bcall_varargs)

N(bcall_varargs):
	push    %ebp
        mov     %esp,%ebp
	SAVE_CALLEE
	push    argcount        /* not used by makevarargs */
        END_PREVIOUS_FRAME_REG(%ecx)
	call	makevarargs
	push	%eax
	CCALL_SECURE(*closure_in)
	RESTORE_CALLEE
	leave
	ret

	.globl	N(bapply_varargs)

N(bapply_varargs):
        /* call varargs primitive in arg1 (%ecx) with arguments in
	   vector arg0 (%eax) */
	push    %ebp
        mov     %esp,%ebp

        push    arg0
        push	arg1

        SAVE_CALLEE
        END_PREVIOUS_FRAME_REG(%ecx)
        CCALL_SECURE(N(apply_vararg))
        RESTORE_CALLEE
        leave
        ret

	.globl	N(interpreter_invoke)

N(interpreter_invoke):
	push    %ebp
        mov     %esp,%ebp

	lea     argstart(%ebp),%ecx
        push    %ecx
	push    argcount
	push	closure_in

        SAVE_CALLEE
	END_PREVIOUS_FRAME
	CCALL(N(interpreter_start))
	RESTORE_CALLEE
	leave
	ret

#define START_INVOKE \
	push	%ebp; \
	mov	%esp,%ebp; \
	push	%ebx; \
	push	%esi; \
	push	%edi; \
	sub	$cc_SIZE+cs_u,%esp; \
	SAVE_CCONTEXT; \
	PUSH_CALL_STACK

#define END_INVOKE \
	RESTORE_CCONTEXT; \
	POP_CALL_STACK; \
	mov	-12(%ebp),%edi; \
	mov	-8(%ebp),%esi; \
	mov	-4(%ebp),%ebx; \
	leave; \
	ret

#define SAVE_CCONTEXT \
	mov	$N(ccontext),%esi; \
	mov	%esp,%edi; \
	mov	$cc_SIZE >> 2,%ecx; \
	rep; \
	movsl; \
	mov	%ebp,N(ccontext)+cc_frame_start

#define RESTORE_CCONTEXT \
	lea	-(12+cc_SIZE+cs_u)(%ebp),%esi; \
	mov	$N(ccontext),%edi; \
	mov	$cc_SIZE >> 2,%ecx; \
	rep; \
	movsl

/* Assumes call_stack entry address in edi */
#define PUSH_CALL_STACK \
	mov	$N(call_stack),%eax; \
	mov	(%eax),%ebx; \
	mov	%ebx,cs_next(%edi); \
	movl	$call_compiled,cs_type(%edi); \
	mov	%edi,(%eax)

/* Assumes call_stack entry address in %esi */
#define POP_CALL_STACK \
	mov	cs_next(%esi),%ecx; \
	mov	%ecx,N(call_stack)

/* Assumes %esi points to the vector. Leaves nargs in %eax */
#define PUSH_VECTORARGS \
	mov	object_size(%esi),%eax; \
	sub	$object_offset,%eax; \
	sub	%eax,%esp; \
	sar	$2,%eax; \
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

	.globl	N(invoke5)
N(invoke5):
	START_INVOKE
	mov	$5,%eax
	push	28(%ebp)
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
	add	$1,%eax
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
ERRORSTUB(error_no_match)
ERRORSTUB(error_compile)
ERRORSTUB(error_abort)

#endif
