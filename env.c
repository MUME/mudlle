/*
 * Copyright (c) 1993-2006 David Gay and Gustav Hållberg
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
#include "env.h"
#include "global.h"

struct locals_list
{
  struct locals_list *next;
  uword index;
  vlist locals;
};

struct env_stack
{
  fncode fn;
  struct env_stack *next, *prev;
  struct locals_list *locals;
  uword size, max_size;		/* Current & max length of locals */
  varlist closure;
};

static struct env_stack *env_stack;

static varlist new_varlist(block_t heap, variable_class vclass,
			   ulong offset, varlist next)
{
  varlist newp = allocate(heap, sizeof *newp);

  newp->next = next;
  newp->vclass = vclass;
  newp->offset = offset;

  return newp;
}

static struct locals_list *new_locals_list(block_t heap, vlist vars, uword idx,
					   struct locals_list *next)
{
  struct locals_list *newp = allocate(heap, sizeof *newp);

  newp->next = next;
  newp->index = idx;
  newp->locals = vars;

  return newp;
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

void env_push(vlist locals, fncode fn)
{
  struct env_stack *newp = allocate(fnmemory(fn), sizeof *newp);

  newp->fn = fn;
  newp->next = env_stack;
  newp->prev = NULL;
  newp->size = newp->max_size = vlist_length(locals);
  if (locals) newp->locals = new_locals_list(fnmemory(fn), locals, 0, NULL);
  else newp->locals = NULL;
  newp->closure = NULL;
  if (env_stack) env_stack->prev = newp;
  env_stack = newp;
}

varlist env_pop(uword *nb_locals)
{
  varlist closure = env_stack->closure;

  *nb_locals = env_stack->max_size;
  env_stack = env_stack->next;
  if (env_stack) env_stack->prev = NULL;
  return closure;
}

void env_block_push(vlist locals)
{
  uword nsize, i, last_set;

  /* Add locals */
  env_stack->locals = new_locals_list(fnmemory(env_stack->fn), locals,
				      env_stack->size, env_stack->locals);

  /* Update size info, clears vars if necessary */
  nsize = env_stack->size + vlist_length(locals);
  last_set = nsize;
  if (env_stack->max_size < nsize)
    {
      last_set = env_stack->max_size;
      env_stack->max_size = nsize;
    }
  for (i = env_stack->size; i < last_set; i++) ins1(op_clear_local, i,
						    env_stack->fn);
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

static variable_class env_close(struct env_stack *env, ulong pos, ulong *offset)
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
  variable_class vclass = local_var;

  /* Add <env,pos> to all environments below env */
  for (subenv = env->prev; subenv; subenv = subenv->prev)
    {
      varlist *closure;
      ulong coffset;
      int found = false;

      /* Is <class,pos> already in closure ? */
      for (coffset = 0, closure = &subenv->closure; *closure;
	   coffset++, closure = &(*closure)->next)
	if (vclass == (*closure)->vclass && pos == (*closure)->offset) /* Yes ! */
	  {
	    found = true;
	    break;
	  }
      if (!found)
	/* Add variable to closure, at end */
	*closure = new_varlist(fnmemory(subenv->fn), vclass, pos, NULL);

      /* Copy reference to this closure position into <class,pos> */
      /* This is how the variable will be named in the next closure */
      vclass = closure_var;
      pos = coffset;
    }
  *offset = pos;
  return vclass;
}

variable_class env_lookup(const char *name, ulong *offset,
			  int do_read, int do_write)
{
  if (strncasecmp(name, GLOBAL_ENV_PREFIX, strlen(GLOBAL_ENV_PREFIX)) == 0)
    name += strlen(GLOBAL_ENV_PREFIX);
  else
    for (struct env_stack *env = env_stack; env; env = env->next)
      {
	/* Look for variable in environment env */
	for (struct locals_list *scope = env->locals;
             scope;
             scope = scope->next)
          {
            ulong pos = scope->index;
            for (vlist vars = scope->locals; vars; pos++, vars = vars->next)
              if (strcasecmp(name, vars->var) == 0)
                {
                  if (do_read)
                    vars->was_read = true;
                  if (do_write)
                    vars->was_written = true;
                  return env_close(env, pos, offset);
                }
          }
      }
  
  /* Not found, is global */
  *offset = global_lookup(name);
  return global_var;
}
