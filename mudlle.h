/* $Log: mudlle.h,v $
 * Revision 1.19  1995/07/15  15:24:35  arda
 * Context cleanup.
 * Remove GCDEBUG.
 *
 * Revision 1.18  1995/03/06  20:28:44  arda
 * make server
 *
 * Revision 1.17  1995/01/22  15:12:43  arda
 * Oops, 10 megs for block size.
 *
 * Revision 1.16  1995/01/22  15:11:47  arda
 * Linux patches.
 *
 * Revision 1.15  1994/11/15  17:34:44  arda
 * Backup
 *
 * Revision 1.14  1994/10/01  17:13:47  arda
 * asdf
 *
 * Revision 1.13  1994/09/24  17:55:39  arda
 * Some undocumented changes
 *
 * Revision 1.12  1994/09/10  19:11:43  arda
 * *** empty log message ***
 *
 * Revision 1.11  1994/08/22  18:03:03  arda
 * Minor fixes.
 *
 * Revision 1.10  1994/08/16  19:16:08  arda
 * Mudlle compiler for sparc now fully functional (68k compiler now needs
 * updating for primitives).
 * Changes to allow Sparc trap's for runtime errors.
 * Also added flags to primitives for better calling sequences.
 *
 * Revision 1.7  1994/02/11  09:59:09  dgay
 * Owl: -Wall
 *      new shared string handling
 *      configuration file
 *
 * Revision 1.6  1993/07/21  20:36:54  un_mec
 * Owl: Added &&, ||, optimised if.
 *      Added branches to the intermediate language.
 *      Separated destiniation language generation into ins module
 *      (with some peephole optimisation)
 *      Standalone version of mudlle (mkf, runtime/mkf, mudlle.c) added to CVS
 *
 * Revision 1.5  1993/05/02  07:37:49  un_mec
 * Owl: New output (mudlle ports).
 *
 * Revision 1.4  1993/04/24  15:20:42  un_mec
 * Owl: Code cleanup.
 *
 * Revision 1.3  1993/03/29  09:24:13  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.3  1993/03/14  16:14:32  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.2  1992/12/30  14:10:48  un_mec
 * Owl:
 * Several changes:
 * - Variables don't have separate value & function cells, instead their are
 *   now 2 types: type_function & type_variable.
 * - print_value: New types (list, vector), printing rationalised.
 * - New type: list (Lisp style pair)
 * - lexer.l: Debug read_from_string
 * - debug_level & DEBUG macro provided to help debugging.
 *
 * Revision 1.1  1992/12/27  21:41:20  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

#ifndef MUDLLE_H
#define MUDLLE_H

/* Generally useful declarations for the mudlle (pronounced muddle)
   interpreter */

#ifndef MUME
#ifdef linux
#include <sys/types.h>
#else
typedef unsigned long ulong;
#endif
typedef signed short word;
typedef unsigned short uword;
typedef signed char byte;
typedef unsigned char ubyte;
#define NEW_STRCPY strcpy
#endif

#include <assert.h>
#include "options.h"
#ifdef MUME
#include "def.main.h"
#endif
#include "context.h"
#include "mudio.h"
#include "calloc.h"
#include "alloc.h"

extern block_t memory;		/* Use this for all allocations during
				   compilation. It will be freed at the end */

extern int debug_level;

#define DEBUG(n, stmt) do { if (debug_level >= n) stmt; } while (0)

#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE 1
#endif

#endif
