/* $Log: stack.c,v $
 * Revision 1.10  1995/07/15  15:24:37  arda
 * Context cleanup.
 * Remove GCDEBUG.
 *
 * Revision 1.9  1994/02/24  08:33:06  arda
 * Owl: New error messages.
 *
 * Revision 1.8  1994/02/03  22:50:02  dgay
 * Owl: C closures.
 *
 * Revision 1.7  1993/11/21  14:47:42  arda
 * Miscellaneous
 *
 * Revision 1.6  1993/05/02  07:38:04  un_mec
 * Owl: New output (mudlle ports).
 *
 * Revision 1.5  1993/04/22  18:58:51  un_autre
 * (MD) & Owl. Bug fixes. /player fixes. EVER_WHINER flag. saving_spells adjusted.
 *
 * Revision 1.4  1993/03/29  09:24:26  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.3  1993/03/14  16:14:52  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.2  1992/12/30  14:10:55  un_mec
 * Owl:
 * Several changes:
 * - Variables don't have separate value & function cells, instead their are
 *   now 2 types: type_function & type_variable.
 * - print_value: New types (list, vector), printing rationalised.
 * - New type: list (Lisp style pair)
 * - lexer.l: Debug read_from_string
 * - debug_level & DEBUG macro provided to help debugging.
 *
 * Revision 1.1  1992/12/27  21:41:33  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

static char rcsid[] = "$Id: stack.c,v 1.10 1995/07/15 15:24:37 arda Exp $";

#include <string.h>
#include "runtime/runtime.h"
#include "objenv.h"
#include "print.h"

#define STACK_SIZE 256
#define LARGE_STACK 1024

struct env *stack;

/* Note: This stack never shrinks except when cleared.
   This shouldn't be changed */

void stack_init(void)
{
  stack_clear();
  staticpro((value *)&stack);
}

void stack_clear(void)
{
  if (!stack || intval(stack->used) > LARGE_STACK) /* Excessively large */
    stack = alloc_env(STACK_SIZE);
  else
    stack->used = makeint(0);
}

value stack_pop(void)
{
  ulong used = intval(stack->used);

  if (used == 0) runtime_error(error_stack_underflow);
  stack->used = (value)((long)stack->used - 2);
  return stack->values->data[used - 1];
}

void stack_push(value v)
{
  GCCHECK(v);
  ENV_ADD_ENTRY(stack, v);
}

value stack_get(ulong index)
{
  ulong used = intval(stack->used);

  if (used <= index) runtime_error(error_stack_underflow);
  return stack->values->data[used - index - 1];
}

void stack_set(ulong index, value v)
{
  ulong used = intval(stack->used);

  GCCHECK(v);
  if (used <= index) runtime_error(error_stack_underflow);
  stack->values->data[used - index - 1] = v;
}

ulong stack_depth(void)
{
  return (ulong)intval(stack->used);
}

void print_stack(struct oport *f)
{
  ulong used = intval(stack->used), i;

  pprintf(f, "Stack is:\n");
  for (i = 0; i < used; i++)
    {
      pprintf(f, "%lu: ", used - i - 1);
      output_value(f, prt_print, stack->values->data[i]);
      pprintf(f, "\n");
    }
  pprintf(f, "\n");
}
