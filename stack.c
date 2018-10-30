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

#include <string.h>

#include "alloc.h"
#include "error.h"
#include "objenv.h"
#include "print.h"
#include "stack.h"

#define STACK_SIZE 256
#define LARGE_STACK 1024

struct env *stack;

/* Note: This stack never shrinks except when cleared.
   This shouldn't be changed */

void stack_init(void)
{
  stack_clear();
  staticpro(&stack);
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
  stack->used = mudlle_iadd(stack->used, -1);
  return stack->values->data[used - 1];
}

void stack_push(value v)
{
  GCCHECK(v);
  env_add_entry(stack, v);
}

value stack_get(ulong aindex)
{
  ulong used = intval(stack->used);

  if (used <= aindex) runtime_error(error_stack_underflow);
  return stack->values->data[used - aindex - 1];
}

ulong stack_depth(void)
{
  return (ulong)intval(stack->used);
}
