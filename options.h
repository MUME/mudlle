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

#ifndef OPTIONS_H
#define OPTIONS_H

/* Mudlle configuration */
/* This files contains only #defines used for both C and assembly code */


/* Must also define stricmp as needed */

#ifdef __MACH__
#  define nosiglongjmp longjmp
#  define nosigsetjmp setjmp
#endif

#ifdef sparc
#  ifdef __SVR4
#    define stricmp strcasecmp
#  endif
#endif

#ifdef hpux
#  define stricmp strcasecmp
#endif

#ifdef __sgi
#  define stricmp strcasecmp
#endif

#ifdef i386
#  define stricmp strcasecmp
#  define GCQDEBUG
#  define GCDEBUG_CHECK
#endif

#ifdef linux
#  define HAVE_ALLOCA_H
#endif

#ifndef PATH_MAX
#  define PATH_MAX 1024
#endif

/* GC configuration, basic parameters */
/* More parameters are found in alloc.h (and some logic in alloc.c). */
#define INITIAL_BLOCKSIZE (128*1024)
#define DEF_SAVE_SIZE (64*1024)

#define GLOBAL_SIZE 512
#define DEFAULT_SECLEVEL 0
#define MUDLLE_INTERRUPT
#define PRINT_CODE


#ifdef sparc
#  define USE_CCONTEXT
#  ifdef __SVR4
#    define __EXTENSIONS__
#    define HAVE_ULONG
#    define nosigsetjmp setjmp
#    define nosiglongjmp longjmp
#  else
#    define HAVE_ALLOCA_H
#    define nosigsetjmp _setjmp
#    define nosiglongjmp _longjmp
#    define HAVE_STRUCT_TM_TM_ZONE
#  endif
#endif /* sparc */

#ifdef hpux
#  define NOCOMPILER
#  define nosigsetjmp setjmp
#  define nosiglongjmp longjmp
#endif /* hpux */

#ifdef __sgi
#  include <sys/bsd_types.h>
#  define HAVE_ULONG
#  define NOCOMPILER
#  define HAVE_ALLOCA_H
#  define nosigsetjmp setjmp
#  define nosiglongjmp longjmp
#endif /* __sgi */

#ifdef linux
#  ifndef __ASSEMBLER__
#    include <sys/types.h>
#  endif
#  define HAVE_ULONG
#  define nosigsetjmp _setjmp
#  define nosiglongjmp _longjmp
#  if defined(i386) || defined(sparc)
#    define USE_CCONTEXT
#  else
#    define NOCOMPILER
#  endif
#endif /* linux */

#ifdef __CYGWIN__
#  define HAVE_ALLOCA_H
#  define NOCOMPILER
#  define nosigsetjmp setjmp
#  define nosiglongjmp longjmp
#endif /* __CYGWIN__ */

#ifdef WIN32
#  define NOCOMPILER
#  define nosigsetjmp setjmp
#  define nosiglongjmp longjmp
#  define htonl __ntohl
#  define ntohl __ntohl
#  define ntohs __ntohs
#  define htons __ntohs
#endif /* WIN32 */

#ifdef AMIGA
#  define HAVE_ALLOCA_H
#  define HAVE_STRUCT_TM_TM_ZONE
#  define nosigsetjmp setjmp
#  define nosiglongjmp longjmp
#endif /* AMIGA */

#ifdef __MACH__
#  define USE_CCONTEXT
#  define HAVE_CRYPT 1
#endif

/* Execution limits */

#define MAX_CALLS 100000       /* Max # of calls executed / interpret */
#define MAX_RECURSION 10000    /* Max # recursion depth / interpret */

/* max # of faster calls (machine code) */
#  define MAX_FAST_CALLS 0

#endif /* OPTIONS_H */
