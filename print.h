/* $Log: print.h,v $
 * Revision 1.6  1995/01/22  15:11:51  arda
 * Linux patches.
 *
 * Revision 1.5  1993/05/02  13:02:55  un_mec
 * Owl: ARGH! Bugs.
 *
 * Revision 1.4  1993/05/02  07:38:03  un_mec
 * Owl: New output (mudlle ports).
 *
 * Revision 1.3  1993/03/29  09:24:24  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.3  1993/03/14  16:14:48  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.2  1992/12/30  14:10:53  un_mec
 * Owl:
 * Several changes:
 * - Variables don't have separate value & function cells, instead their are
 *   now 2 types: type_function & type_variable.
 * - print_value: New types (list, vector), printing rationalised.
 * - New type: list (Lisp style pair)
 * - lexer.l: Debug read_from_string
 * - debug_level & DEBUG macro provided to help debugging.
 *
 * Revision 1.1  1992/12/27  21:41:31  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

#ifndef PRINT_H
#define PRINT_H

#include "ports.h"
#include "mvalues.h"

typedef enum { prt_display, prt_print, prt_examine } prt_level;

void output_value(struct oport *f, prt_level level, value v);
void print_init(void);

#endif
