/* $Log: stack.h,v $
 * Revision 1.7  1995/07/15  15:24:38  arda
 * Context cleanup.
 * Remove GCDEBUG.
 *
 * Revision 1.6  1995/01/22  15:11:52  arda
 * Linux patches.
 *
 * Revision 1.5  1993/05/02  07:38:06  un_mec
 * Owl: New output (mudlle ports).
 *
 * Revision 1.4  1993/03/29  09:24:28  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.3  1993/03/14  16:14:55  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.2  1992/12/30  14:10:56  un_mec
 * Owl:
 * Several changes:
 * - Variables don't have separate value & function cells, instead their are
 *   now 2 types: type_function & type_variable.
 * - print_value: New types (list, vector), printing rationalised.
 * - New type: list (Lisp style pair)
 * - lexer.l: Debug read_from_string
 * - debug_level & DEBUG macro provided to help debugging.
 *
 * Revision 1.1  1992/12/27  21:41:34  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

#ifndef STACK_H
#define STACK_H

#include "mudio.h"
#include "mvalues.h"

extern struct env *stack;

void stack_init(void);
void stack_clear(void);
#define stack_reserve(n) env_reserve(stack, (n))
/* Effect: Insures that stack has n free spaces.
     The stack depth can increase by n without the GC being called.
*/

value stack_pop(void);
void stack_push(value v);

value stack_get(ulong index);
void stack_set(ulong index, value v);
ulong stack_depth(void);

void print_stack(struct oport *f);

#endif
