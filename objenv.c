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
#include <stddef.h>
#include "mudlle.h"
#include "mudio.h"
#include "types.h"
#include "objenv.h"
#include "alloc.h"
#include "print.h"

struct env *alloc_env(ulong size)
/* Returns: A new environment, of initial size size, initialised to NULL.
*/
{
  struct gcpro gcpro1;
  struct env *newp = (struct env *)allocate_record(type_internal, 3);
  value tmp;

  GCPRO1(newp);
  tmp = alloc_vector(size);
  newp->values = tmp;
  UNGCPRO();
  newp->used = makeint(0);
  newp->size = makeint(size);

  return newp;
}

void env_reserve(struct env *env, ulong n)
/* Effects: Makes sure that env has n free entries
*/
{
  ulong used = intval(env->used);
  ulong size = intval(env->size);

  if (used + n > size)
    {
      ulong newsize = 2 * size + n;
      struct vector *newp;
      struct gcpro gcpro1;

      GCPRO1(env);
      newp = alloc_vector(newsize);
      memcpy(newp->data, env->values->data, size * sizeof(value));
      env->values = newp;
      env->size = makeint(newsize);
      UNGCPRO();
    }
}

ulong env_add_entry(struct env *env, value v)
/* Effects: Adds a new value to env, initialised to v.
   Returns: The index of the new value.
   Modifies: env.
*/
{
  ulong used = intval(env->used);
  struct gcpro gcpro1, gcpro2;

  GCCHECK(v); GCCHECK(env);
  GCPRO2(v, env);
  env_reserve(env, 1);
  UNGCPRO();
  env->values->data[used] = v;
  env->used = (value)((long)env->used + 2);

  return used;
}

void print_env(struct oport *f, struct env *env)
{
  ulong used = intval(env->used), i;
  struct gcpro gcpro1;

  GCPRO1(env);
  for (i = 0; i < used; i++)
    {
      pprintf(f, "%lu: ", i);
      output_value(f, prt_examine, env->values->data[i]);
      pprintf(f, "\n");
    }
  UNGCPRO();
}
