/* $Log: interpret.h,v $
 * Revision 1.17  1995/07/15  15:24:27  arda
 * Context cleanup.
 * Remove GCDEBUG.
 *
 * Revision 1.16  1994/10/09  06:42:20  arda
 * Libraries
 * Type inference
 * Many minor improvements
 *
 * Revision 1.15  1994/09/16  13:07:13  arda
 * Rename protect to catch.
 * New protect/unprotect functions (like dynpro/undynpro).
 *
 * Revision 1.14  1994/09/15  19:46:41  arda
 * Performance improvements:
 *   setjmp -> _setjmp (setjmp is horrendously slow)
 *   cold_protect
 * reset_limits split from reset_interpreter
 * fix division of negative numbers
 * Add ?\{n,r,t}
 * gc_size returns "mutable" size
 *
 * Revision 1.13  1994/08/22  11:18:34  arda
 * Moved code allocation to ins.c
 * Changes for mudlle compiler in MUME.
 *
 * Revision 1.12  1994/08/16  19:16:02  arda
 * Mudlle compiler for sparc now fully functional (68k compiler now needs
 * updating for primitives).
 * Changes to allow Sparc trap's for runtime errors.
 * Also added flags to primitives for better calling sequences.
 *
 * Revision 1.9  1994/03/08  01:50:39  arda
 * (MD) New Istari.
 *
 * Revision 1.8  1994/02/11  09:58:59  dgay
 * Owl: -Wall
 *      new shared string handling
 *      configuration file
 *
 * Revision 1.7  1994/02/03  19:21:35  arda
 * nothing special(2)
 *
 * Revision 1.6  1993/05/02  13:02:41  un_mec
 * Owl: ARGH! Bugs.
 *
 * Revision 1.5  1993/03/29  09:24:05  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.3  1993/03/14  16:14:23  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.3  1993/02/14  21:09:07  un_mec
 * Owl: Fix code limits.
 *
 * Revision 1.2  1993/01/30  12:13:37  un_mec
 * Owl: Mudlle reactions installed, with loading and editing commands.
 *
 * Revision 1.1  1992/12/27  21:41:14  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

#ifndef INTERPRET_H
#define INTERPRET_H

void do_interpret(struct closure *c, int nargs);
void interpret_init(void);

#endif
