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

#define FALIGN 16
#define GFUNC(n) .align FALIGN ; .globl N(n) ; N(n)
#define LFUNC(n) .align FALIGN ; n

#define ICST(n) $(2 * (n) | 1)

/* Register assignments */
#define arg0        %eax
#define arg0b       %al
#define arg1        %ecx
#define arg1b       %cl
#define globals     %esi
#define argcount    %eax
#define argcountw   %ax
#define closure     %ebx
#define closure_in  %edx
#define closure_inb %dl

/* Stack frame information */
#define argstart 8

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
/* 	lea	4*(1+n)(%esp),%eax; \ */
/* 	mov	%eax,N(ccontext)+cc_frame_end_sp; \ */
/* 	mov	%ebp,N(ccontext)+cc_frame_end_bp */

/* End mudlle frame ending at last frame (at push %ebp). Uses
   'scratch' as scratch register. */
#define END_PREVIOUS_FRAME_REG(scratch)                 \
        xor     scratch,scratch;                        \
        mov     scratch,N(ccontext)+cc_frame_end_bp;    \
        mov     %ebp,N(ccontext)+cc_frame_end_sp

#define END_PARENT_FRAME(scratch)                       \
        movl    $1,scratch;                             \
        mov     scratch,N(ccontext)+cc_frame_end_bp;    \
        mov     %ebp,N(ccontext)+cc_frame_end_sp

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
#define SAVE_CALLEE                             \
	mov	%ebx,N(ccontext)+cc_callee;     \
	mov	%esi,N(ccontext)+cc_callee+4
#define RESTORE_CALLEE                          \
	mov	N(ccontext)+cc_callee,%ebx;     \
	mov	N(ccontext)+cc_callee+4,%esi;   \
	CLEAR_CALLER

#define SAVE_CALLER                             \
	mov	%ebx,N(ccontext)+cc_callee;     \
	mov	%esi,N(ccontext)+cc_callee+4;   \
	mov	%edx,N(ccontext)+cc_caller;     \
	mov	%edi,N(ccontext)+cc_caller+4
#define RESTORE_CALLER                          \
	mov	N(ccontext)+cc_callee,%ebx;     \
	mov	N(ccontext)+cc_callee+4,%esi;   \
	mov	N(ccontext)+cc_caller,%edx;     \
	mov	N(ccontext)+cc_caller+4,%edi

#define CLEAR_CALLER                            \
	movl    $0,%edx;                        \
	movl    $0,%edi

/* Push the current function's input arguments on the stack.
     in: argcount
     clobbers: esi, edi, ecx
 */
#define REPUSH_ARGS                                             \
        /* %esp -= 4 * nargs */                                 \
        mov     argcount,%ecx;                                  \
	neg     %ecx;                                           \
	lea     (%esp,%ecx,4),%esp;                             \
	/* Copy argcount args from ebp+argstart to esp */       \
	mov     argcount,%ecx;                                  \
	lea	argstart(%ebp),%esi;                            \
	mov     %esp,%edi;                                      \
        rep;                                                    \
        movsl

/* Simple integer manipulation macros */
#define SETINT(x) \
	or	$1,x		/* set integer type bit */

#define INTVAL(x) \
	sar	$1,x		/* Make into normal integer */

#define MAKEINT(x) \
        lea     1(x,x),x

#define ISINT(x) \
	test	$1,x		/* Is x an integer ? */

#define ISNULL(x) \
	test	x,x		/* Is x NULL (registers only) */

#define IS_READ_WRITE(x) \
        testb   $OBJ_READONLY,object_flags(x)

#define CHECK_LOOP()                                    \
	subl	$1,N(xcount);                           \
	je	E(early_error_loop);                    \
	cmpl    (N(mudlle_stack_limit)),%esp;           \
	jb	E(early_error_recurse);                 \
	cmpw	N(minlevel),%cx;                        \
	jb	E(early_error_security_violation)

#define E(n) N(b ## n)

/* Simple integer operations: +, /, % */

GFUNC(bmultiply):
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

GFUNC(bdivide):
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

GFUNC(bremainder):
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

GFUNC(bshift_left):
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

GFUNC(bshift_right):
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

GFUNC(badd):
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
	/* add	$8,%esp */
	RESTORE_CALLER
	leave
	ret

GFUNC(bconcat): /* concatenate argcount (eax) strings on stack */
        push    %ebp
        mov     %esp,%ebp
        SAVE_CALLER
	push    arg0
	lea     8(%ebp),arg0
        push    arg0
	END_PREVIOUS_FRAME
	CCALL_LEAF(N(concat_strings))
	/* add	$8,%esp */
        RESTORE_CALLER
	leave
        ret

GFUNC(btypeof):
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

	/* arg0 = arg0[arg1] */
GFUNC(bref):
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
	/* add	$8,%esp */
	mov	N(ccontext)+cc_caller,%edx
	leave
	ret

vectorref:
	ISINT(arg1b)
	je	cref
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

	/* arg0 = arg0[arg1] = %edx */
GFUNC(bset):
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
	CCALL_LEAF(N(code_setb))
	/* add	$12,%esp */
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
	xor     %edx,%edx       /* make sure it's a valid mudlle value */
	add     $4,%esp         /* pop arg1 */
	ret

	/* restore args before calling cset for stack traces */
0:      add     %edx,%edx
        jmp     cset
2:      pop     arg1
1:      MAKEINT(%edx)
        jmp     cset

/* Special ops: bcleargc, bwglobal, brglobal */

/* arg0: written value; arg1: goffset written to */
GFUNC(bwglobal):
	push    %ebp
	mov     %esp,%ebp

	push	arg0            /* written value */
	push    arg1            /* goffset arg */
	END_PREVIOUS_FRAME
	call	N(check_global_write)
	/* add $8,%esp */

	CLEAR_CALLER

	leave
	ret

/* arg0: goffset read from */
GFUNC(brglobal):
	push    %ebp
	mov     %esp,%ebp
	END_PREVIOUS_FRAME

	push	arg1
	call	N(check_global_read)
	/* add $4,%esp */

	CLEAR_CALLER

	leave
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

/* Clear the allocated stack frame (between caller's %esp and %ebp) */

GFUNC(bcleargc):
	jne	E(early_error_wrong_parameters)
	CHECK_LOOP()

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

GFUNC(bcleargc4):
	mov	closure_in,16(%esp)
GFUNC(bcleargc3):
	mov	closure_in,12(%esp)
GFUNC(bcleargc2):
	mov	closure_in,8(%esp)
GFUNC(bcleargc1):
	mov	closure_in,4(%esp)
GFUNC(bcleargc0):
	jne	E(early_error_wrong_parameters)
	CHECK_LOOP()
	ret

/* Build varargs vector & count */
GFUNC(bvarargs):
	CHECK_LOOP()

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

	/* allocate arg0 bytes and return in arg1 */
LFUNC(alloc_bytes):
       	mov	N(posgen0),arg1
	sub	arg0,arg1
	cmp	N(startgen0),arg1
	jb      alloc_bytes_gc
	mov     arg1,N(posgen0)
	mov     arg0,object_size(arg1)
#ifdef GCDEBUG
	mov	N(minorgen),arg0
	mov	arg0,object_gen(arg1)
#endif
        ret

alloc_bytes_gc:
        push    %ebp
        mov     %esp,%ebp
	SAVE_CALLER
	push	arg0
	push	arg0
        END_PARENT_FRAME(arg0)
	CCALL_LEAF(N(garbage_collect))
	/* add	$4,%esp */
	movl	-4(%ebp),arg0
	RESTORE_CALLER
	leave
	jmp	alloc_bytes

	/* Alloc variable cell, return in arg1 */
GFUNC(balloc_variable):
	GCSTAT_ADD(type_variable, $variable_size)
	movl    $variable_size,arg0
	call    alloc_bytes
	movl	$garbage_record | type_variable << 8,object_info(arg1)
	ret

	/* Size of closure is in arg0, return result in arg1 */
GFUNC(balloc_closure):
	GCSTAT_ADD(type_closure, arg0)
	call    alloc_bytes
	movl	$garbage_record | type_closure << 8 | OBJ_READONLY << 16, \
                  object_info(arg1)
	ret

	/* Allocate cons cell, return in arg1 */
GFUNC(balloc_cons):
	GCSTAT_ADD(type_pair, $pair_size)
	movl    $pair_size,arg0
	call    alloc_bytes
	movl	$garbage_record | type_pair << 8,object_info(arg1)
	ret

	/* Allocate vector of arg0 bytes, return in arg1 */
GFUNC(balloc_vector):
	GCSTAT_ADD(type_vector, arg0)
	call    alloc_bytes
	movl	$garbage_record | type_vector << 8, object_info(arg1)
	ret

GFUNC(bsave_caller):
        lea     4(%esp),%eax
        movl    %eax,N(ccontext)+cc_frame_end_sp
        movl    %ebp,N(ccontext)+cc_frame_end_bp
        movl    closure,N(ccontext)+cc_callee
        movl    globals,N(ccontext)+cc_callee + 4
	ret

GFUNC(bsave_caller_noalloc):
        lea     4(%esp),%eax
        movl    %eax,N(ccontext)+cc_frame_end_sp
        movl    %ebp,N(ccontext)+cc_frame_end_bp
	ret

GFUNC(brestore_caller):
        movl    N(ccontext)+cc_callee,closure
        movl    N(ccontext)+cc_callee + 4,globals
	/* could have gone bad if a primitive caused GC */
	CLEAR_CALLER
        ret

/* Interface operations:
    bcall: any call from machine language
    interpreter_invoke: machine language -> interpreter
    mc_invoke: C code -> machine language
*/

/* Call using standard x86 conventions
   closure_in contains called fn
*/
GFUNC(bcall):
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
	push    %ebp
        mov     %esp,%ebp

	SAVE_CALLEE
        /* Fetch actual C function */
	mov	object_offset+primitive_op(closure_in),closure_in
	mov	primop_op(closure_in),closure_in

	/* For call traces; read from handle_primitive_frame() in error.c */
	push    $N(bcall_primitive_tail)
        push    closure_in
        push	argcount

	push    argcount        /* not popped by makevarargs */
        END_PREVIOUS_FRAME_REG(%ecx)
	call	makevarargs
	push	%eax

	CCALL_LEAF(*closure_in)
	RESTORE_CALLEE

        leave
	ret

call_secure:
	mov	object_offset+primitive_op(closure_in),%ecx
	movw	primop_seclevel(%ecx),%di
	cmpw	%di,N(internal_seclevel)
	jb	bcall_error_security_violation
	mov     N(maxseclevel),%ecx
	INTVAL(%ecx)
	cmpw    %di,%cx
	jb	bcall_error_security_violation
	/* fallthrough */

call_primitive:
	/* Check arg count */
	mov	object_offset+primitive_op(closure_in),%ecx
	cmpw	primop_nargs(%ecx),%ax
	jne	bcall_error_wrong_parameters
        /* fallthrough; this label is here to un-confuse gdb */

GFUNC(bcall_primitive_tail):
	push    %ebp
        mov     %esp,%ebp

        /* Fetch actual C function */
	mov	primop_op(%ecx),closure_in

	/* For call traces; read from handle_primitive_frame() in error.c */
	push    $N(bcall_primitive_tail)
        push    closure_in
        push    argcount

	SAVE_CALLEE
	REPUSH_ARGS
        END_PREVIOUS_FRAME
	CCALL_LEAF(*closure_in)
	RESTORE_CALLEE

	leave
	ret

call_closure:
	mov	object_offset(closure_in),%ecx
	add	$object_offset+function_offset,%ecx
	jmp	*%ecx

/* Build varargs vector & count */
makevarargs: /* preserves closure, closure_in, globals (ie ebx, edx, esi) */
	/* in: argcount: argument count, arguments on stack before ebp */
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

	/* Copy args from bp+8 (edx, pc, argcount, ..., ebp, pc, args...) */
	mov     8(%esp),%ecx     /* argcount */
	mov	%esi,%edx
	lea	8(%ebp),%esi
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

	/* input: argcnt in %eax, seclev in %cx, secure in %edx */
GFUNC(bcall_secure):
	mov	object_offset+primitive_op(closure_in),closure_in
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
        /* fallthrough; this label is here to un-confuse gdb */

bcall_secure_tail:
	push    %ebp
	mov     %esp,%ebp

	mov	primop_op(closure_in),closure_in
	SAVE_CALLEE
	REPUSH_ARGS
        END_PREVIOUS_FRAME_REG(%ecx)
        CCALL_LEAF(*closure_in)
	RESTORE_CALLEE

	leave
	ret

	/* call varargs primitive in %edx (closure) with %eax
  	   (argcount) arguments from the stack, at seclevel in %cx */
GFUNC(bcall_varargs):
	push    %ebp
        mov     %esp,%ebp
	movw    %cx,N(internal_seclevel)
	SAVE_CALLEE
	push    argcount        /* not popped by makevarargs */
        END_PREVIOUS_FRAME_REG(%ecx)
	call	makevarargs
	push	%eax
	CCALL_LEAF(*closure_in)
	RESTORE_CALLEE
	leave
	ret

GFUNC(bapply_varargs):
        /* call varargs primitive in closure (%edx) with arguments in
	   vector arg0 (%eax) at seclevel %cx */
	push    %ebp
        mov     %esp,%ebp

	movw    %cx,N(internal_seclevel)
        push    arg0
        push	closure_in

        SAVE_CALLEE
        END_PREVIOUS_FRAME_REG(%ecx)
        CCALL_LEAF(N(apply_vararg))
        RESTORE_CALLEE
        leave
        ret

	/* interpret the closure in closure_in with argcount arguments
	   on the stack */
GFUNC(interpreter_invoke):
	push    %ebp
        mov     %esp,%ebp

	lea     argstart(%ebp),%ecx
        push    %ecx
	push    argcount
	push	closure_in

        SAVE_CALLEE
	END_PREVIOUS_FRAME
	CCALL_LEAF(N(interpreter_start))
	/* add	$12,%esp */
	RESTORE_CALLEE
	leave
	ret

#define START_INVOKE                            \
	push	%ebp;                           \
	mov	%esp,%ebp;                      \
	push	%ebx;                           \
	push	%esi;                           \
	push	%edi;                           \
	sub	$cc_SIZE + cs_SIZE,%esp;        \
	SAVE_CCONTEXT;                          \
	PUSH_CALL_STACK

#define END_INVOKE                              \
	RESTORE_CCONTEXT;                       \
	POP_CALL_STACK;                         \
	mov	-12(%ebp),%edi;                 \
	mov	-8(%ebp),%esi;                  \
	mov	-4(%ebp),%ebx;                  \
	leave;                                  \
	ret

#define SAVE_CCONTEXT                           \
	mov	$N(ccontext),%esi;              \
	mov	%esp,%edi;                      \
	mov	$cc_SIZE >> 2,%ecx;             \
	rep;                                    \
	movsl;                                  \
	mov	%ebp,N(ccontext)+cc_frame_start

#define RESTORE_CCONTEXT                                \
	lea	-(12 + cc_SIZE + cs_SIZE)(%ebp),%esi;   \
	mov	$N(ccontext),%edi;                      \
	mov	$cc_SIZE >> 2,%ecx;                     \
	rep;                                            \
	movsl

/* Assumes call_stack entry address in edi */
#define PUSH_CALL_STACK                         \
	mov	$N(call_stack),%eax;            \
	mov	(%eax),%ebx;                    \
	mov	%ebx,cs_next(%edi);             \
	movl	$call_compiled,cs_type(%edi);   \
	mov	%edi,(%eax)

/* Assumes call_stack entry address in %esi */
#define POP_CALL_STACK                          \
	mov	cs_next(%esi),%ecx;             \
	mov	%ecx,N(call_stack)

/* Assumes %esi points to the vector. Leaves nargs in %eax */
#define PUSH_VECTORARGS                         \
	mov	object_size(%esi),%eax;         \
	sub	$object_offset,%eax;            \
	sub	%eax,%esp;                      \
	sar	$2,%eax;                        \
	mov	%eax,%ecx;                      \
	add	$object_offset,%esi;            \
	mov	%esp,%edi;                      \
	rep;                                    \
	movsl

#define CALL_CLOSURE                                    \
	xor	globals, globals;                       \
	xor	closure, closure;                       \
	CLEAR_CALLER;                                   \
	mov	8(%ebp),closure_in;                     \
	mov	object_offset(closure_in),%ecx;         \
	add	$object_offset+function_offset,%ecx;    \
	call	*%ecx

GFUNC(invoke0):
	START_INVOKE
	xor	%eax,%eax
	CALL_CLOSURE
	END_INVOKE

GFUNC(invoke1):
	START_INVOKE
	mov	$1,%eax
	push	12(%ebp)
	CALL_CLOSURE
	END_INVOKE

GFUNC(invoke2):
	START_INVOKE
	mov	$2,%eax
	push	16(%ebp)
	push	12(%ebp)
	CALL_CLOSURE
	END_INVOKE

GFUNC(invoke3):
	START_INVOKE
	mov	$3,%eax
	push	20(%ebp)
	push	16(%ebp)
	push	12(%ebp)
	CALL_CLOSURE
	END_INVOKE

GFUNC(invoke4):
	START_INVOKE
	mov	$4,%eax
	push	24(%ebp)
	push	20(%ebp)
	push	16(%ebp)
	push	12(%ebp)
	CALL_CLOSURE
	END_INVOKE

GFUNC(invoke5):
	START_INVOKE
	mov	$5,%eax
	push	28(%ebp)
	push	24(%ebp)
	push	20(%ebp)
	push	16(%ebp)
	push	12(%ebp)
	CALL_CLOSURE
	END_INVOKE

GFUNC(invoke1plus):
	START_INVOKE
	mov	16(%ebp),%esi
	PUSH_VECTORARGS
	add	$1,%eax
	push	12(%ebp)
	CALL_CLOSURE
	END_INVOKE

GFUNC(invoke):
	START_INVOKE
	mov	12(%ebp),%esi
	PUSH_VECTORARGS
	CALL_CLOSURE
	END_INVOKE


	/* Put errorstubs last so branch prediction will predict correctly
	 * (ppro and most others predic contitional jumps forwards as not
	 * taken)
	 */

GFUNC(bcall_error):
	push    %ebp
        mov     %esp,%ebp

	END_PREVIOUS_FRAME_REG(%ecx)

	/* these are here for handle_primitive_frame() in error.c */
	push    $N(bcall_error)
	push    closure_in      /* called function */
	push    argcount        /* number of arguments */

	push    closure         /* error number */
	CCALL_LEAF(N(runtime_error))

#define BCALLERRORSTUB(n)                       \
bcall_ ## n:;                                   \
        mov     $n,closure;                     \
        jmp     N(bcall_error)

BCALLERRORSTUB(error_bad_function)
BCALLERRORSTUB(error_security_violation)
BCALLERRORSTUB(error_wrong_parameters)

berror:
	push    %ebp
	mov     %esp,%ebp
	END_PREVIOUS_FRAME_REG(%ecx)
	push	closure         /* error number */
	CCALL_LEAF(N(runtime_error))

#define ERRORSTUB(n)                            \
GFUNC(b ## n):;                                 \
	mov	$n,closure;                     \
        jmp     berror

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

bearly_error:
	push    %ebp
	mov     %esp,%ebp
	END_PREVIOUS_FRAME_REG(%ecx)
	push    argcount
	push	closure         /* error number */
	CCALL_LEAF(N(compiled_early_runtime_error))

/* argcount must be set correctly here */
#define EARLYERRORSTUB(n)                       \
GFUNC(bearly_ ## n):;                           \
	mov     $n,closure;                     \
        jmp     bearly_error

EARLYERRORSTUB(error_loop)
EARLYERRORSTUB(error_recurse)
EARLYERRORSTUB(error_security_violation)
EARLYERRORSTUB(error_wrong_parameters)

#endif
