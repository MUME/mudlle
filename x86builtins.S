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

#include "mudlle-config.h"
#include "x86.h"

#  include "x86consts.h"

#ifdef __MACH__
.section	__TEXT,__text,regular,pure_instructions
#else
.section	.text
#endif

#ifndef NOCOMPILER

#ifdef __MACH__
 #define N(x) _ ## x
 #define SETSIZE(x)
#else
 #define N(x) x
 #define SETSIZE(x) .size x, . - x
#endif

#define GLABEL(n) .globl N(n) ; N(n)

#define FALIGN 16
#define GFUNC(n) .align FALIGN ; GLABEL(n): ; .cfi_startproc
#define GEND(n) .cfi_endproc ; SETSIZE(N(n))
#define LFUNC(n) .align FALIGN ; n: ; .cfi_startproc
#define LEND(n) .cfi_endproc ; SETSIZE(n)

#define WORD_BITS 2		/* bits per word size */
#define WORD_SIZE 4

/* some very incomplete CFI information included */
#define FUNC_ENTER					\
	push %ebp;					\
	.cfi_def_cfa_offset (2 * WORD_SIZE);		\
	.cfi_offset %ebp, -(2 * WORD_SIZE);		\
	mov %esp,%ebp;					\
	.cfi_def_cfa_register %ebp

#define FUNC_RETURN					\
	leave;						\
	.cfi_def_cfa %ebp, WORD_SIZE;			\
	ret

#define FUNC_NORETURN /* nothing to see here */

/* use these for no-prologue push/pop */
#define NO_FP_PUSH(reg)					\
	push reg;					\
	.cfi_adjust_cfa_offset WORD_SIZE
#define NO_FP_POP(reg)					\
	pop reg;					\
	.cfi_adjust_cfa_offset -WORD_SIZE
#define NO_FP_POPN(n)					\
	add $(n) * WORD_SIZE,%esp;			\
	.cfi_adjust_cfa_offset -(n * WORD_SIZE)

GLABEL(builtin_start):

#define ICST(n) $(2 * (n) | 1)

/* Register usage for mudlle
 *   eax: scratch, argument count, function result, 1st arg to builtins
 *   ecx: scratch, 2nd arg to builtins
 *   ebx: callee-saved, (opt.) closure
 *   esi: callee-saved, (opt.) globals
 *   edx: caller-save, closure-in
 *   edi: caller-save; callee-save for non-mudlle
 *   ebp: frame pointer
 *   esp: stack pointer
 *
 * The non-scratch registers must hold valid mudlle values when a GC
 * might happen. SAVE_CALLE{R,E} store them where the garbage collector
 * can update them as necessary.
 *
 * The stack preceding the current (END_PREVIOUS_FRAME) or previous
 * (END_PARENT_FRAME) stack frame (as pointed to by %ebp) will be
 * updated by the garbage collector as well. */

/* Register assignments */
#define arg0	    %eax
#define arg0b	    %al
#define arg1	    %ecx
#define arg1b	    %cl
#define globals	    %esi
#define argcount    %eax
#define argcountw   %ax
#define closure	    %ebx
#define closureb    %bl
#define closure_in  %edx
#define closure_inb %dl

/* Stack frame information */
#define argstart (2 * WORD_SIZE)
#define argn(n)	 (argstart + WORD_SIZE * (n))(%ebp)

#ifdef GCSTATS
#define GCSTAT_ADD(type, size)						\
	addl	$1,(N(gcstats) + gcstats_alloc				\
		    + gcstats_alloc_size * type + gcstats_alloc_nb);	\
	addl	size,(N(gcstats) + gcstats_alloc			\
		      + gcstats_alloc_size * type + gcstats_alloc_sz)

#else  /* !GCSTATS */
#define GCSTAT_ADD(type, size)
#endif /* !GCSTATS */


/* End mudlle value stack frame ending before N values and PC. */
/* #define END_FRAME_PC(n) \ */
/*	lea	WORD_SIZE*(1+n)(%esp),%eax; \ */
/*	mov	%eax,N(ccontext)+cc_frame_end_sp; \ */
/*	mov	%ebp,N(ccontext)+cc_frame_end_bp */

/* End mudlle frame ending at last frame (at push %ebp). Uses
   'scratch' as scratch register. */
#define END_PREVIOUS_FRAME_REG(scratch)			\
	xor	scratch,scratch;			\
	mov	scratch,N(ccontext)+cc_frame_end_bp;	\
	mov	%ebp,N(ccontext)+cc_frame_end_sp

#define END_PARENT_FRAME(scratch)			\
	movl	$1,scratch;				\
	mov	scratch,N(ccontext)+cc_frame_end_bp;	\
	mov	%ebp,N(ccontext)+cc_frame_end_sp

#define END_PREVIOUS_FRAME END_PREVIOUS_FRAME_REG(arg0)

#define CCALL_LEAF(fn) \
	call	fn

/* Various ways of preserving the registers:
   SAVE_CALLEE/RESTORE_CALLEE: preserve value of callee saved registers
     only (caller saved are cleared in RESTORE)
   SAVE_CALLER/RESTORE_CALLER: preserve value of callee and caller registers
   CLEAR_CALLER: preserve no registers, just clear caller saved on return
     (assumes that called fn is NOALLOC)
*/
#define __SAVE_CALLEE(n, reg) mov %reg,N(ccontext)+cc_callee_ ## reg
#define __RESTORE_CALLEE(n, reg) mov N(ccontext)+cc_callee_ ## reg,%reg
#define SAVE_CALLEE				\
	FOR_CALLEE_SAVE(__SAVE_CALLEE, ;)
#define RESTORE_CALLEE				\
	FOR_CALLEE_SAVE(__RESTORE_CALLEE, ;);	\
	CLEAR_CALLER

#define __SAVE_CALLER(n, reg) mov %reg,N(ccontext)+cc_caller_ ## reg
#define __RESTORE_CALLER(n, reg) mov N(ccontext)+cc_caller_ ## reg,%reg
#define SAVE_CALLER				\
	SAVE_CALLEE;				\
	FOR_CALLER_SAVE(__SAVE_CALLER, ;)
#define RESTORE_CALLER				\
	FOR_CALLEE_SAVE(__RESTORE_CALLEE, ;);	\
	FOR_CALLER_SAVE(__RESTORE_CALLER, ;)

#define CLEAR_CALLER				\
	xor	%edx,%edx;			\
	xor	%edi,%edi

/* Push the current function's input arguments on the stack.
     in: argcount
     clobbers: esi, edi, ecx
 */
#define REPUSH_ARGS						\
	/* %esp -= WORD_SIZE * nargs */				\
	mov	argcount,%ecx;					\
	neg	%ecx;						\
	lea	(%esp,%ecx,WORD_SIZE),%esp;			\
	/* Copy argcount args from ebp+argstart to esp */	\
	mov	argcount,%ecx;					\
	lea	argstart(%ebp),%esi;				\
	mov	%esp,%edi;					\
	rep;							\
	movsl

/* Simple integer manipulation macros */
#define SETINT(x) \
	or	$1,x		/* set integer type bit */

#define INTVAL(x) \
	sar	$1,x		/* Make into normal integer */

#define MAKEINTTO(x,y) \
	lea	1(x,x),y	/* Make into mudlle integer */
#define MAKEINT(x) MAKEINTTO(x, x)

#define ISINT(x) \
	test	$1,x		/* Not-equal if x is an integer */

#define ISNULL(x) \
	test	x,x		/* Equal if x is NULL */

#define IS_READ_WRITE(x) \
	/* Equal if x is read/write */ \
	testb	$OBJ_READONLY,object_flags(x)

#define CHECK_LOOP()					\
	subl	$1,N(xcount);				\
	je	E(early_error_loop);			\
	cmpl	(N(mudlle_stack_limit)),%esp;		\
	jb	E(early_error_recurse);			\
	cmpw	N(minlevel),%cx;			\
	jb	E(early_error_security_violation)

#define E(n) N(b ## n)

/* Simple integer operations: +, /, % */

GFUNC(bmultiply)
	btr	$0,arg0
	jnc	1f
	sar	$1,arg1
	jnc	0f

	imul	arg1,arg0
	SETINT(arg0)
	ret

0:	/* restore argument values */
	SETINT(arg0)
	add	arg1,arg1
1:	mov	$N(code_multiply),closure
	jmp	call_binary_primitive
GEND(bmultiply)

GFUNC(bdivide)
	sar	$1,arg0
	jnc	0f
	sar	$1,arg1
	jnc	1f
	jz	2f
	NO_FP_PUSH(%edx)
	cdq
	idiv	arg1
	MAKEINT(arg0)
	NO_FP_POP(%edx)
	ret

	/* restore arguments */
0:	add	arg0,arg0
	jmp	4f
1:	add	arg1,arg1
	jmp	3f
2:	MAKEINT(arg1)
3:	MAKEINT(arg0)
4:	mov	$N(code_divide),closure
	jmp	call_binary_primitive
GEND(bdivide)

GFUNC(bremainder)
	sar	$1,arg0
	jnc	0f
	sar	$1,arg1
	jnc	1f
	jz	2f
	NO_FP_PUSH(%edx)
	cdq
	idiv	arg1
	mov	%edx,arg0
	MAKEINT(arg0)
	NO_FP_POP(%edx)
	ret

	/* restore arguments */
0:	add	arg0,arg0
	jmp	4f
1:	add	arg1,arg1
	jmp	3f
2:	MAKEINT(arg1)
3:	MAKEINT(arg0)
4:	mov	$N(code_remainder),closure
	jmp	call_binary_primitive
GEND(bremainder)

GFUNC(bshift_left)
	sar	$1,arg1
	jnc	0f
	btr	$0,arg0
	jnc	1f
	shl	%cl,arg0
	SETINT(arg0)
	ret

0:	add	arg1,arg1
	jmp	2f
1:	MAKEINT(arg1)
2:	mov	$N(code_shift_left),closure
	jmp	call_binary_primitive
GEND(bshift_left)

GFUNC(bshift_right)
	sar	$1,arg1
	jnc	0f
	testl	$1,arg0
	je	1f
	sar	%cl,arg0
	SETINT(arg0)
	ret

0:	add	arg1,arg1
	jmp	2f
1:	MAKEINT(arg1)
2:	mov	$N(code_shift_right),closure
	jmp	call_binary_primitive
GEND(bshift_right)

LFUNC(call_binary_primitive)
	/* used to call primitive (in 'closure') to trigger errors
	   with call trace */
	FUNC_ENTER
	push	arg1
	push	arg0
	END_PREVIOUS_FRAME
	CCALL_LEAF(*closure)
	/* should never get here */
	add	$2 * WORD_SIZE,%esp
	CCALL_LEAF(N(abort))
LEND(call_binary_primitive)

/* Call C code: ref, + */

GFUNC(badd)
	ISINT(arg0b)
	je	cadd
	ISINT(arg1b)
	je	cadd

	/* Integer addition */
	lea	-1(arg0,arg1),arg0
	ret

cadd:	/* Call plus primitive for string append */
	FUNC_ENTER
	SAVE_CALLER
	push	arg1
	push	arg0
	END_PREVIOUS_FRAME
	CCALL_LEAF(N(code_plus))
	/* add	$2 * WORD_SIZE,%esp */
	RESTORE_CALLER
	FUNC_RETURN
GEND(badd)

GFUNC(bconcat) /* concatenate argcount (eax) strings on stack */
	FUNC_ENTER
	SAVE_CALLER
	push	arg0
	lea	argstart(%ebp),arg0
	push	arg0
	END_PREVIOUS_FRAME
	CCALL_LEAF(N(concat_strings))
	/* add	$2 * WORD_SIZE,%esp */
	RESTORE_CALLER
	FUNC_RETURN
GEND(bconcat)

GFUNC(btypeof)
	ISNULL(arg0)
	mov	ICST(type_null),arg1
	je	0f

	ISINT(arg0b)
	mov	ICST(type_integer),arg1
	jne	0f

	movzbl	object_type(arg0),arg1
	MAKEINT(arg1)

0:	mov	arg1,arg0
	ret
GEND(btypeof)

/* carry flag = arg0[arg1 >> 3] & (arg1 & 7) */
GFUNC(bbitref)
	NO_FP_PUSH(arg1)
	sar	$4,arg1		/* arg1 = byte offset */
	js	.Lbbitref_error

	add	$object_offset+1,arg1
	cmp	object_size(arg0),arg1
	.cfi_remember_state	/* for .Lbbitref_error */
	jae	.Lbbitref_error

	movzbl	-1(arg0,arg1),arg0
	NO_FP_POP(arg1)
	shr	arg1
	and	$7,arg1
	bt	arg1,arg0
	ret

.Lbbitref_error:
	.cfi_restore_state
	NO_FP_POP(arg1)
	mov	$N(code_bit_setp),closure
	jmp	call_binary_primitive
GEND(bbitref)

	/* arg0 = arg0[arg1] */
GFUNC(bref)
	ISINT(arg0b)
	jne	cref

	cmpb	$type_string,object_type(arg0)
	je	.Lstringref
	cmpb	$type_vector,object_type(arg0)
	jne	cref

	/* vector ref */
	ISINT(arg1b)
	je	cref
	NO_FP_PUSH(arg1)
	.cfi_remember_state	/* for .Lbref_error */
	add	arg1,arg1
	jo	.Lbref_error
	jns	.Lvecnosign

	add	object_size(arg0),arg1
	sub	$object_offset,arg1
	js	.Lbref_error

.Lvecnosign:
	add	$object_offset-2,arg1
	cmp	object_size(arg0),arg1
	jae	.Lbref_error
	mov	(arg0,arg1),arg0
	NO_FP_POPN(1)	/* pop arg1 */
	ret

.Lstringref:
	ISINT(arg1b)
	je	cref
	NO_FP_PUSH(arg1)
	INTVAL(arg1)
	jns	.Lstrnosign

	add	object_size(arg0),arg1
	sub	$object_offset+1,arg1
	js	.Lbref_error

.Lstrnosign:
	add	$object_offset+1,arg1
	cmp	object_size(arg0),arg1
	jae	.Lbref_error
	movzbl	-1(arg0,arg1),arg0
	MAKEINT(arg0)
	NO_FP_POPN(1)	/* pop arg1 */
	ret

	/* vectorref and stringref errors come here */
.Lbref_error:
	.cfi_restore_state
	NO_FP_POP(arg1)
	jmp	cref
GEND(bref)

LFUNC(cref)	/* Call ref - only need to save %edx (ref doesn't alloc) */
	FUNC_ENTER
	mov	%edx,N(ccontext)+cc_caller_edx
	push	arg1
	push	arg0
	END_PREVIOUS_FRAME
	CCALL_LEAF(N(code_ref))
	/* add	$2 * WORD_SIZE,%esp */
	mov	N(ccontext)+cc_caller_edx,%edx
	FUNC_RETURN
LEND(cref)

	/* arg0 = symbol for arg0[arg1] */
GFUNC(bsymbol_ref)
	FUNC_ENTER
	SAVE_CALLER
	push	arg1
	push	arg0
	END_PREVIOUS_FRAME
	CCALL_LEAF(N(code_symbol_ref))
	/* add	$2 * WORD_SIZE,%esp */
	RESTORE_CALLER
	FUNC_RETURN
GEND(bsymbol_ref)

	/* arg0 = arg0[arg1] = %edx */
GFUNC(bset)
	ISINT(arg0b)
	jne	cset

	cmpb	$type_string,object_type(arg0)
	je	.Lstringset
	cmpb	$type_vector,object_type(arg0)
	jne	cset

	/* vector set */
	ISINT(arg1b)
	je	cset
	IS_READ_WRITE(arg0)
	jne	cset

	NO_FP_PUSH(arg1)
	.cfi_remember_state	/* for .Lvecset_error */
	add	arg1,arg1
	jo	.Lvecset_error
	jns	.Lvecsetnosign

	add	object_size(arg0),arg1
	sub	$object_offset,arg1
	js	.Lvecset_error

.Lvecsetnosign:
	add	$object_offset-2,arg1
	cmp	object_size(arg0),arg1
	jae	.Lvecset_error
	mov	%edx,(arg0,arg1)
	mov	%edx,arg0
	NO_FP_POPN(1)		/* pop arg1 */
	ret

	/* restore arg1 before calling cset for stack traces */
.Lvecset_error:
	.cfi_restore_state
	NO_FP_POP(arg1)
	jmp	cset

.Lstringset:
	INTVAL(%edx)
	jnc	.Lstrset_error_noint
	IS_READ_WRITE(arg0)
	jne	.Lstrset_error_readonly
	NO_FP_PUSH(arg1)
	.cfi_remember_state	/* for .Lstrset_error */
	INTVAL(arg1)
	jnc	.Lstrset_error
	jns	.Lstrsetnosign

	add	object_size(arg0),arg1
	sub	$object_offset+1,arg1
	js	.Lstrset_error

.Lstrsetnosign:
	add	$object_offset+1,arg1
	cmp	object_size(arg0),arg1
	jae	.Lstrset_error
	mov	%dl,-1(arg0,arg1)
	movzbl	%dl,arg0
	MAKEINT(arg0)
	xor	%edx,%edx	/* make sure it's a valid mudlle value */
	NO_FP_POPN(1)		/* pop arg1 */
	ret

	/* restore args before calling cset for stack traces */
.Lstrset_error_noint:
	add	%edx,%edx
	jmp	cset

.Lstrset_error:
	.cfi_restore_state
	NO_FP_POP(arg1)
.Lstrset_error_readonly:
	MAKEINT(%edx)
	jmp	cset
GEND(bset)

LFUNC(cset)			/* Call set! */
	FUNC_ENTER
	SAVE_CALLEE
	push	%edx
	push	arg1
	push	arg0
	END_PREVIOUS_FRAME
	CCALL_LEAF(N(code_setb))
	/* add	$3 * WORD_SIZE,%esp */
	RESTORE_CALLEE
	FUNC_RETURN
LEND(cset)

/* Special ops: bcleargc, bwglobal, brglobal */

/* arg0: written value; arg1: goffset written to */
GFUNC(bwglobal)
	FUNC_ENTER

	push	arg1		/* goffset arg */
	push	arg0		/* written value */
	END_PREVIOUS_FRAME
	call	N(check_global_write)
	/* add $2 * WORD_SIZE,%esp */

	CLEAR_CALLER

	FUNC_RETURN
GEND(bwglobal)

/* arg0: goffset read from */
GFUNC(brglobal)
	FUNC_ENTER
	END_PREVIOUS_FRAME

	push	arg0
	call	N(check_global_read)
	/* add $WORD_SIZE,%esp */

	CLEAR_CALLER

	FUNC_RETURN
GEND(brglobal)

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

/* Clear the allocated stack frame (between caller's %esp and %ebp) */

GFUNC(bcleargc)
	jne	E(early_error_wrong_parameters)
	CHECK_LOOP()

	/* Clear stack frame */
	xor	%eax,%eax
	lea	WORD_SIZE(%esp),%edi
	mov	%ebp,%ecx
	sub	%edi,%ecx
	sar	$WORD_BITS,%ecx
	rep
	stosl
	xor	%edi,%edi
	ret
GEND(bcleargc)

GFUNC(bcleargc4)
	movl	$0,(4 * WORD_SIZE)(%esp)
GLABEL(bcleargc3):
	movl	$0,(3 * WORD_SIZE)(%esp)
GLABEL(bcleargc2):
	movl	$0,(2 * WORD_SIZE)(%esp)
GLABEL(bcleargc1):
	movl	$0,(1 * WORD_SIZE)(%esp)
GLABEL(bcleargc0):
	jne	E(early_error_wrong_parameters)
	CHECK_LOOP()
	ret
GEND(bcleargc0)

/* Build varargs vector & count */
GFUNC(bvarargs)
	CHECK_LOOP()

	/* in: argcount: argument count, arguments on stack */
	/* returns vector in %eax */
	NO_FP_PUSH(argcount)

	/* Clear stack frame */
	xor	%eax,%eax
	lea	(2 * WORD_SIZE)(%esp),%edi
	mov	%ebp,%ecx
	sub	%edi,%ecx
	sar	$WORD_BITS,%ecx
	rep
	stosl

	NO_FP_POP(%eax)			/* argcount */
	MAKEINTTO(%eax, %edi)		/* mudlle int argcount */

	/* Allocate argument vector */
	lea	object_offset(,%eax,WORD_SIZE),%eax /* object size */
	GCSTAT_ADD(type_vector, %eax)
	call	alloc_bytes
	mov	arg1,arg0

	/* New object now in arg0 */
	movl	$garbage_record | type_vector << 8,object_info(arg0)

	/* Copy args from bp+argstart */
	movl	%edi,%ecx
	INTVAL(%ecx)			/* argcount */

	NO_FP_PUSH(%esi)
	lea	argstart(%ebp),%esi
	lea	object_offset(arg0),%edi
	rep
	movsl
	NO_FP_POP(%esi)

	xor	%edi,%edi		/* must hold valid mudlle value */
	ret
GEND(bvarargs)

/* Storage allocation operations */

/* Allocate arg0 bytes and return in arg1. The caller's stack frame
   will be GC-protected. Preserves mudlle values in all the non-scratch
   registers. */
LFUNC(alloc_bytes)
	cmp	$MAX_MUDLLE_OBJECT_SIZE,arg0
	ja	.Lalloc_bytes_fail

	mov	N(posgen0),arg1
	sub	arg0,arg1
	cmp	N(startgen0),arg1
	jb	.Lalloc_bytes_gc
	mov	arg1,N(posgen0)
	mov	arg0,object_size(arg1)
#ifdef GCDEBUG
	mov	N(minorgen),arg0
	mov	arg0,object_gen(arg1)
#endif
	ret

.Lalloc_bytes_gc:
	FUNC_ENTER
	SAVE_CALLER
	push	arg0
	push	arg0
	END_PARENT_FRAME(arg0)
	CCALL_LEAF(N(garbage_collect))
	/* add	$WORD_SIZE,%esp */
	movl	-WORD_SIZE(%ebp),arg0
	RESTORE_CALLER
	leave
	jmp	alloc_bytes
	FUNC_NORETURN

.Lalloc_bytes_fail:
	FUNC_ENTER
	push	arg0
	END_PARENT_FRAME(arg0)
	CCALL_LEAF(N(abort))
	FUNC_NORETURN
LEND(alloc_bytes)

	/* Alloc variable cell, return in arg1 */
GFUNC(balloc_variable)
	GCSTAT_ADD(type_variable, $variable_size)
	movl	$variable_size,arg0
	call	alloc_bytes
	movl	$garbage_record | type_variable << 8,object_info(arg1)
	ret
GEND(balloc_variable)

	/* Size of closure is in arg0, return result in arg1 */
GFUNC(balloc_closure)
	GCSTAT_ADD(type_closure, arg0)
	call	alloc_bytes
	movl	$garbage_record | type_closure << 8 | OBJ_READONLY << 16, \
		  object_info(arg1)
	ret
GEND(balloc_closure)

	/* Allocate cons cell, return in arg1 */
LFUNC(alloc_cons)
	GCSTAT_ADD(type_pair, $pair_size)
	movl	$pair_size,arg0
	call	alloc_bytes
	movl	$garbage_record | type_pair << 8,object_info(arg1)
	ret
LEND(alloc_cons)

	/* arg0 = cons(arg1, arg0) */
GFUNC(bcons)
	FUNC_ENTER

	push	arg1
	push	arg0
	call	alloc_cons	/* cons cell in arg1 */
	movl	(%esp),arg0
	movl	arg0,pair_cdr_offset(arg1)
	movl	WORD_SIZE(%esp),arg0
	movl	arg0,pair_car_offset(arg1)
	movl	arg1,arg0

	FUNC_RETURN
GEND(bcons)

	/* arg0 = pcons(arg1, arg0) */
GFUNC(bpcons)
	FUNC_ENTER

	push	closure
	push	arg1
	push	arg0
	call	alloc_cons	/* cons cell in arg1 */

	movb	$OBJ_IMMUTABLE,closureb

	movl	(%esp),arg0	/* load arg0: the cdr */
	movl	arg0,pair_cdr_offset(arg1)
	ISNULL(arg0)
	je	0f
	ISINT(arg0b)
	jne	0f
	andb	object_flags(arg0),closureb

0:	movl	WORD_SIZE(%esp),arg0	/* load arg1: the car */
	movl	arg0,pair_car_offset(arg1)
	ISNULL(arg0)
	je	1f
	ISINT(arg0b)
	jne	1f
	andb	object_flags(arg0),closureb

1:	orb	$OBJ_READONLY,closureb
	movb	closureb,object_flags(arg1)

	movl	arg1,arg0

	movl	-WORD_SIZE(%ebp),closure
	FUNC_RETURN
GEND(bpcons)

	/* Allocate vector of arg0 bytes, return in arg1 */
GFUNC(balloc_vector)
	GCSTAT_ADD(type_vector, arg0)
	call	alloc_bytes
	movl	$garbage_record | type_vector << 8, object_info(arg1)
	ret
GEND(balloc_vector)

GFUNC(bsave_caller)
	SAVE_CALLEE
	lea	WORD_SIZE(%esp),%eax
	movl	%eax,N(ccontext)+cc_frame_end_sp
	movl	%ebp,N(ccontext)+cc_frame_end_bp
	ret
GEND(bsave_caller)

GFUNC(bsave_caller_noalloc)
	lea	WORD_SIZE(%esp),%eax
	movl	%eax,N(ccontext)+cc_frame_end_sp
	movl	%ebp,N(ccontext)+cc_frame_end_bp
	ret
GEND(bsave_caller_noalloc)

GFUNC(brestore_caller)
	RESTORE_CALLEE
	ret
GEND(brestore_caller)

/* Interface operations:
    bcall: any call from machine language
    interpreter_invoke: machine language -> interpreter
    mc_invoke: C code -> machine language
*/

/* Call using standard x86 conventions
   closure_in contains called fn
*/
GFUNC(bcall)
	ISNULL(closure_in)
	je	bcall_error_bad_function

	ISINT(closure_inb)
	jne	bcall_error_bad_function

	movw	$DEFAULT_SECLEVEL,N(internal_seclevel)

	cmpb	$type_closure,object_type(closure_in)
	je	call_closure

	cmpb	$type_primitive,object_type(closure_in)
	je	call_primitive

	cmpb	$type_secure,object_type(closure_in)
	je	call_secure

	cmpb	$type_varargs,object_type(closure_in)
	jne	bcall_error_bad_function
	/* fallthrough */

call_varargs:
	FUNC_ENTER

	SAVE_CALLEE
	END_PREVIOUS_FRAME_REG(%ecx)

	/* Fetch actual C function */
	mov	primitive_op(closure_in),closure_in
	mov	primop_op(closure_in),closure_in

	/* For call traces; read from handle_primitive_frame() in error.c */
	push	$N(bcall_primitive_tail)
	push	closure_in
	push	argcount

	lea	argstart(%ebp),%ecx
	push	%ecx
	push	argcount
	push	closure_in
	CCALL_LEAF(N(builtin_call_vararg))
	/* add	$3 * WORD_SIZE,%esp */

	RESTORE_CALLEE
	FUNC_RETURN

call_closure:
	mov	closure_code_offset(closure_in),%ecx
	add	$mcode_code_offset,%ecx
	jmp	*%ecx

call_secure:
	mov	primitive_op(closure_in),%ecx
	movw	primop_seclevel(%ecx),%di
	cmpw	%di,N(internal_seclevel)
	jb	bcall_error_security_violation
	mov	N(maxseclevel),%ecx
	INTVAL(%ecx)
	cmpw	%di,%cx
	jb	bcall_error_security_violation
	/* fallthrough */

call_primitive:
	/* Check arg count */
	mov	primitive_op(closure_in),%ecx
	cmpw	primop_nargs(%ecx),%ax
	jne	bcall_error_wrong_parameters
	/* fallthrough; this label is here to un-confuse gdb */
GEND(bcall)

GFUNC(bcall_primitive_tail)
	FUNC_ENTER

	/* Fetch actual C function */
	mov	primop_op(%ecx),closure_in

	/* For call traces; read from handle_primitive_frame() in error.c */
	push	$N(bcall_primitive_tail)
	push	closure_in
	push	argcount

	SAVE_CALLEE
	REPUSH_ARGS
	END_PREVIOUS_FRAME
	CCALL_LEAF(*closure_in)
	RESTORE_CALLEE

	FUNC_RETURN
GEND(bcall_primitive_tail)

	/* input: argcnt in %eax, seclev in %cx, secure in %edx */
GFUNC(bcall_secure)
	mov	primitive_op(closure_in),closure_in
	/* Check the immediate caller's security level */
	cmpw	primop_seclevel(closure_in),%cx
	jb	E(error_security_violation)
	movw	%cx,N(internal_seclevel)
	/* Check the session's security level */
	mov	N(maxseclevel),%ecx
	INTVAL(%ecx)
	cmpw	primop_seclevel(closure_in),%cx
	jb	E(error_security_violation)
	/* Check arg count */
	cmpw	argcountw,primop_nargs(closure_in)
	jne	E(error_wrong_parameters)

	FUNC_ENTER

	mov	primop_op(closure_in),closure_in
	SAVE_CALLEE
	REPUSH_ARGS
	END_PREVIOUS_FRAME_REG(%ecx)
	CCALL_LEAF(*closure_in)
	RESTORE_CALLEE

	FUNC_RETURN
GEND(bcall_secure)

	/* call varargs primitive in %edx (closure) with %eax
	   (argcount) arguments from the stack, at seclevel in %cx */
GFUNC(bcall_varargs)
	FUNC_ENTER

	movw	%cx,N(internal_seclevel)

	SAVE_CALLEE
	END_PREVIOUS_FRAME_REG(%ecx)

	lea	argstart(%ebp),%ecx
	push	%ecx
	push	argcount
	push	%edx
	CCALL_LEAF(N(builtin_call_vararg))
	/* add	$3 * WORD_SIZE,%esp */

	RESTORE_CALLEE
	FUNC_RETURN
GEND(bcall_varargs)

GFUNC(bapply_varargs)
	/* call varargs primitive in closure (%edx) with arguments in
	   vector on stack at seclevel %cx */
	FUNC_ENTER

	movw	%cx,N(internal_seclevel)
	push	argn(0)
	push	closure_in

	SAVE_CALLEE
	END_PREVIOUS_FRAME_REG(%ecx)
	CCALL_LEAF(N(builtin_apply_vararg))
	RESTORE_CALLEE
	FUNC_RETURN
GEND(bapply_varargs)

	/* interpret the closure in closure_in with argcount arguments
	   on the stack */
GFUNC(interpreter_invoke)
	FUNC_ENTER

	lea	argstart(%ebp),%ecx
	push	%ecx
	push	argcount
	push	closure_in

	SAVE_CALLEE
	END_PREVIOUS_FRAME
	CCALL_LEAF(N(builtin_call_interpreter))
	/* add	$3 * WORD_SIZE,%esp */
	RESTORE_CALLEE
	FUNC_RETURN
GEND(interpreter_invoke)

#define INVOKE_STACK_SIZE (cc_SIZE + cs_SIZE + 3 * WORD_SIZE)

#define START_INVOKE				\
	FUNC_ENTER;				\
	sub	$INVOKE_STACK_SIZE,%esp;	\
	/* preserve native callee-save */	\
	mov	%ebx,-(1 * WORD_SIZE)(%ebp);	\
	mov	%esi,-(2 * WORD_SIZE)(%ebp);	\
	mov	%edi,-(3 * WORD_SIZE)(%ebp);	\
	SAVE_CCONTEXT;				\
	PUSH_CALL_STACK

#define END_INVOKE				\
	RESTORE_CCONTEXT;			\
	POP_CALL_STACK;				\
	/* restore native callee-save */	\
	mov	-(3 * WORD_SIZE)(%ebp),%edi;	\
	mov	-(2 * WORD_SIZE)(%ebp),%esi;	\
	mov	-(1 * WORD_SIZE)(%ebp),%ebx;	\
	FUNC_RETURN

#define SAVE_CCONTEXT				\
	mov	$N(ccontext),%esi;		\
	mov	%esp,%edi;			\
	mov	$cc_SIZE >> 2,%ecx;		\
	rep;					\
	movsl;					\
	mov	%ebp,N(ccontext)+cc_frame_start

#define RESTORE_CCONTEXT				\
	lea	-INVOKE_STACK_SIZE(%ebp),%esi;		\
	mov	$N(ccontext),%edi;			\
	mov	$cc_SIZE >> 2,%ecx;			\
	rep;						\
	movsl

/* Assumes call_stack entry address in edi */
#define PUSH_CALL_STACK				\
	mov	$N(call_stack),%eax;		\
	mov	(%eax),%ebx;			\
	mov	%ebx,cs_next(%edi);		\
	movl	$call_compiled,cs_type(%edi);	\
	mov	%edi,(%eax)

/* Assumes call_stack entry address in %esi */
#define POP_CALL_STACK				\
	mov	cs_next(%esi),%ecx;		\
	mov	%ecx,N(call_stack)

/* Assumes %esi points to the vector. Leaves nargs in %eax */
#define PUSH_VECTORARGS				\
	mov	object_size(%esi),%eax;		\
	sub	$object_offset,%eax;		\
	sub	%eax,%esp;			\
	sar	$WORD_BITS,%eax;		\
	mov	%eax,%ecx;			\
	add	$object_offset,%esi;		\
	mov	%esp,%edi;			\
	rep;					\
	movsl

GFUNC(invoke1plus)
	START_INVOKE
	mov	argn(2),%esi
	PUSH_VECTORARGS
	add	$1,%eax
	push	argn(1)
	jmp	0f
GEND(invoke1plus)

GFUNC(invoke)
	START_INVOKE
	mov	argn(1),%esi
	PUSH_VECTORARGS
	jmp	0f
GEND(invoke)

GFUNC(invoke0)
	START_INVOKE
	xor	%eax,%eax
	jmp	0f
GEND(invoke0)

GFUNC(invoke1)
	START_INVOKE
	mov	$1,%eax
	jmp	1f
GEND(invoke1)

GFUNC(invoke2)
	START_INVOKE
	mov	$2,%eax
	jmp	2f
GEND(invoke2)

GFUNC(invoke3)
	START_INVOKE
	mov	$3,%eax
	jmp	3f
GEND(invoke3)

GFUNC(invoke4)
	START_INVOKE
	mov	$4,%eax
	jmp	4f
GEND(invoke4)

GFUNC(invoke5)
	START_INVOKE
	mov	$5,%eax
	push	argn(5)
4:	push	argn(4)
3:	push	argn(3)
2:	push	argn(2)
1:	push	argn(1)
0:
	xor	globals, globals
	xor	closure, closure
	CLEAR_CALLER
	mov	argn(0),closure_in
	mov	closure_code_offset(closure_in),%ecx
	add	$mcode_code_offset,%ecx
	call	*%ecx

	END_INVOKE
GEND(invoke5)


	/* Put errorstubs last so branch prediction will predict correctly
	 * (ppro and most others predic contitional jumps forwards as not
	 * taken)
	 */

#define BCALLERRORSTUB(n)			\
LFUNC(bcall_ ## n);				\
	mov	$n,closure;			\
	jmp	bcall_error;			\
LEND(bcall_ ## n)

BCALLERRORSTUB(error_bad_function)
BCALLERRORSTUB(error_security_violation)
BCALLERRORSTUB(error_wrong_parameters)

LFUNC(bcall_error)
	FUNC_ENTER

	END_PREVIOUS_FRAME_REG(%ecx)

	lea	argstart(%ebp),%ecx
	push	%ecx		/* pointer to arguments */
	push	argcount	/* number of arguments */
	push	closure_in	/* called function */
	push	closure		/* error number */
	CCALL_LEAF(N(bad_call_error))
LEND(bcall_error)

/* arg0 is the mudlle value (of bad type), and arg1 is the expected typeset */
GFUNC(btype_error)
	FUNC_ENTER

	push	arg1
	push	arg0
	END_PREVIOUS_FRAME_REG(%ecx)
	CCALL_LEAF(N(bad_typeset_error))
GEND(btype_error)

LFUNC(berror)
	FUNC_ENTER
	END_PREVIOUS_FRAME_REG(%ecx)
	push	closure		/* error number */
	CCALL_LEAF(N(runtime_error))
LEND(berror)

#define ERRORSTUB(n)				\
GFUNC(b ## n);					\
	mov	$n,closure;			\
	jmp	berror;				\
GEND(b ## n)

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

LFUNC(bearly_error)
	FUNC_ENTER
	END_PREVIOUS_FRAME_REG(%ecx)
	push	argcount
	push	closure		/* error number */
	CCALL_LEAF(N(compiled_early_runtime_error))
	FUNC_NORETURN
LEND(bearly_error)

/* argcount must be set correctly here */
#define EARLYERRORSTUB(n)			\
GFUNC(bearly_ ## n);				\
	mov	$n,closure;			\
	jmp	bearly_error;			\
GEND(bearly_ ## n)

EARLYERRORSTUB(error_loop)
EARLYERRORSTUB(error_recurse)
EARLYERRORSTUB(error_security_violation)
EARLYERRORSTUB(error_wrong_parameters)

GLABEL(builtin_end):

#endif
