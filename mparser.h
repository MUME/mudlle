/* $Log: mparser.h,v $
 * Revision 1.1  1995/06/04  14:24:30  arda
 * Rename/move some files, misc. junk
 *
 * Revision 1.7  1994/10/09  06:42:45  arda
 * Libraries
 * Type inference
 * Many minor improvements
 *
 * Revision 1.6  1994/08/16  19:16:10  arda
 * Mudlle compiler for sparc now fully functional (68k compiler now needs
 * updating for primitives).
 * Changes to allow Sparc trap's for runtime errors.
 * Also added flags to primitives for better calling sequences.
 *
 * Revision 1.3  1993/03/29  09:24:18  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.3  1993/03/14  16:14:42  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.1  1992/12/27  21:41:27  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

#ifndef PARSER_H
#define PARSER_H

#include "tree.h"
#include "tokens.h"

int yyparse(void);
void parser_init(void);

extern file parsed_code;

#endif
