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


#ifdef __MACH__
#  define nosiglongjmp longjmp
#  define nosigsetjmp setjmp
#endif

#if defined i386
#  define GCQDEBUG
#  define GCDEBUG_CHECK
#  undef WORDS_BIGENDIAN
#elif defined __x86_64__
#  undef WORDS_BIGENDIAN
#else
#  error "Unsupported architecture"
#endif

#ifdef linux
#  define HAVE_ALLOCA_H
#endif

#ifdef __CYGWIN__
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

#define DEFAULT_SECLEVEL 0 /* Seclevel when not calling type_secure/.. fns */
#define MIN_SECLEVEL 0     /* Minimum valid seclevel */
#define SECLEVEL_GLOBALS 1 /* Maxseclevel required to mess with globals */
#define LEGACY_SECLEVEL 1  /* Maxseclevel is used if seclevel < LEGACY */
#define MAX_SECLEVEL 1     /* Maximum valid seclevel */

#define MUDLLE_INTERRUPT
#define PRINT_CODE


#ifdef linux
#  ifndef __ASSEMBLER__
#    include <sys/types.h>
#  endif
#  define HAVE_ULONG
#  define nosigsetjmp _setjmp
#  define nosiglongjmp _longjmp
#  if defined(i386)
#    define USE_CCONTEXT
#  else
#    define NOCOMPILER
#  endif
#endif /* linux */

#ifdef __CYGWIN__
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

#ifdef __MACH__
#  if defined i386
#    define USE_CCONTEXT
#  else
#    define NOCOMPILER
#  endif
#  define HAVE_CRYPT 1
#endif

#undef HAVE_STATIC_ASSERT
#if __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 6)
#  define HAVE_STATIC_ASSERT 1
#elif defined __has_extension
#  if __has_extension(c_static_assert)
#    define HAVE_STATIC_ASSERT 1
#  endif
#endif

/* Execution limits */

#define MAX_RECURSION 10000    /* Max # recursion depth / interpret */

#define INTERPRETED_LOOP_COST 10 /* Relative cost of an interpreted loop */
#  define MAX_LOOP_COUNT MAX_TAGGED_INT

#endif /* OPTIONS_H */
