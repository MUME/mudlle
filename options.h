/* $Log: options.h,v $
 * Revision 1.2  1995/07/16  09:16:52  arda
 * Add GCSTATS option.
 * Misc bug fixes.
 *
 * Revision 1.1  1995/07/15  15:49:29  arda
 * New files, missing from previous commit.
 *
 */

#ifndef OPTIONS_H
#define OPTIONS_H

/* Mudlle configuration */
/* This files contains only #define's, it is used for both C and assembly code */

#ifndef MUME
/* The MUME options are in ../config.h */

#ifdef sparc
#define PRINT_CODE
#define INLINE
#define NORETURN volatile
#define INTERRUPT
#endif

#ifdef linux
#define stricmp strcasecmp
#define HAVE_MEMMOVE
#define PRINT_CODE
#define INLINE
#define NORETURN volatile
#define GCDEBUG
#define GCDEBUG_CHECK
#define PRINT_CODE
#define INTERRUPT
#endif

#ifdef AMIGA
#define PRINT_CODE
#define INLINE __inline
#define NORETURN
#define HAVE_MEMMOVE
#define PRINT_CODE
#define INTERRUPT
#endif

#define PATH_MAX 1024

/* GC configuration, basic parameters */
/* More parameters are found in alloc.h (and some logic in alloc.c). */

#define INITIAL_BLOCKSIZE (128*1024)
#define DEF_SAVE_SIZE (64*1024)
#define GLOBAL_SIZE 512

#endif

/* Execution limits */

#define MAX_CALLS 10000		/* Max # of calls executed / interpret */

#define MAX_FAST_CALLS 100000	/* Max # of faster calls (machine code) */


#endif
