/* $Log: env.c,v $
 * Revision 1.6  1994/10/09  06:41:58  arda
 * Libraries
 * Type inference
 * Many minor improvements
 *
 * Revision 1.5  1994/02/11  09:58:45  dgay
 * Owl: -Wall
 *      new shared string handling
 *      configuration file
 *
 * Revision 1.4  1994/02/03  19:21:29  arda
 * nothing special(2)
 *
 * Revision 1.3  1994/01/08  12:49:43  dgay
 * Owl: Improved code generation for blocks (they are not implemented
 * as 0 argument functions anymore, they are folded into the current
 * function instead).
 *
 * Revision 1.2  1993/03/29  09:23:46  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.3  1993/03/14  16:14:06  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.1  1992/12/27  21:41:03  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

static char rcsid[] = "$Id: env.c,v 1.6 1994/10/09 06:41:58 arda Exp $";

#include <string.h>
#include "mudlle.h"
#include "tree.h"
#include "env.h"
#include "ins.h"
#include "global.h"

struct locals_list
{
  struct locals_list *next;
  uword index;
  vlist locals;
};

struct env_stack
{
  struct env_stack *next, *prev;
  struct locals_list *locals;
  uword size, max_size;		/* Current & max length of locals */
  varlist closure;
};

static struct env_stack *env_stack;

static varlist new_varlist(variable_class class, ulong offset, varlist next)
{
  varlist new = allocate(memory, sizeof *new);

  new->next = next;
  new->class = class;
  new->offset = offset;

  return new;
}

static struct locals_list *new_locals_list(vlist vars, uword index,
					   struct locals_list *next)
{
  struct locals_list *new = allocate(memory, sizeof *new);

  new->next = next;
  new->index = index;
  new->locals = vars;

  return new;
}

uword vlist_length(vlist scan)
{
  uword nlocals = 0;

  for (; scan; scan = scan->next) nlocals++;

  return nlocals;
}

void env_reset(void)
{
  env_stack = NULL;
}

void env_push(vlist locals)
{
  struct env_stack *new = allocate(memory, sizeof *new);

  new->next = env_stack;
  new->prev = NULL;
  new->size = new->max_size = vlist_length(locals);
  if (locals) new->locals = new_locals_list(locals, 0, NULL);
  else new->locals = NULL;
  new->closure = NULL;
  if (env_stack) env_stack->prev = new;
  env_stack = new;
}

varlist env_pop(uword *nb_locals)
{
  varlist closure = env_stack->closure;

  *nb_locals = env_stack->max_size;
  env_stack = env_stack->next;
  if (env_stack) env_stack->prev = NULL;
  return closure;
}

void env_block_push(vlist locals, fncode fn)
{
  uword nsize, i, last_set;

  /* Add locals */
  env_stack->locals = new_locals_list(locals, env_stack->size, env_stack->locals);

  /* Update size info, clears vars if necessary */
  nsize = env_stack->size + vlist_length(locals);
  last_set = nsize;
  if (env_stack->max_size < nsize)
    {
      last_set = env_stack->max_size;
      env_stack->max_size = nsize;
    }
  for (i = env_stack->size; i < last_set; i++) ins1(op_clear_local, i, fn);
  env_stack->size = nsize;
}

void env_block_pop(void)
{
  /* Cannot share variables as some of them may escape */
  /* Think about this (most variables can be shared, and the
     variable cells always can be) */
  /*env_stack->size -= vlist_length(env_stack->locals->locals);*/
  env_stack->locals = env_stack->locals->next;
}

variable_class env_close(struct env_stack *env, ulong pos, ulong *offset)
/* Effects: Adds local variable pos of environment env to all closures
     below it in the env_stack.
   Returns: local_var if env is the last environment on the stack,
     closure_var otherwise.
     *offset is the offset in the closure or local variables at which
     the local variable <env,pos> can be found by the function whose
     environement is env_stack.
*/
{
  struct env_stack *subenv;
  variable_class class = local_var;

  /* Add <env,pos> to all environments below env */
  for (subenv = env->prev; subenv; subenv = subenv->prev)
    {
      varlist *closure;
      ulong coffset;
      int found = FALSE;

      /* Is <class,pos> already in closure ? */
      for (coffset = 0, closure = &subenv->closure; *closure;
	   coffset++, closure = &(*closure)->next)
	if (class == (*closure)->class && pos == (*closure)->offset) /* Yes ! */
	  {
	    found = TRUE;
	    break;
	  }
      if (!found)
	/* Add variable to closure, at end */
	*closure = new_varlist(class, pos, NULL);

      /* Copy reference to this closure position into <class,pos> */
      /* This is how the variable will be named in the next closure */
      class = closure_var;
      pos = coffset;
    }
  *offset = pos;
  return class;
}

variable_class env_lookup(const char *name, ulong *offset)
{
  struct env_stack *env;

  for (env = env_stack; env; env = env->next)
    {
      /* Look for variable in environment env */
      vlist vars;
      struct locals_list *scope;
      ulong pos;

      for (scope = env->locals; scope; scope = scope->next)
	for (pos = scope->index, vars = scope->locals; vars; pos++, vars = vars->next)
	  if (strcmp(name, vars->var) == 0)
	    return env_close(env, pos, offset);
    }

  /* Not found, is global */
  *offset = global_lookup(name);
  return global_var;
}
