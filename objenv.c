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
#include <stddef.h>

#include "alloc.h"
#include "objenv.h"
#include "mvalues.h"

struct env *alloc_env(ulong size)
/* Returns: A new environment, of initial size size, initialised to NULL.
*/
{
  struct env *newp = (struct env *)allocate_record(
    type_internal, grecord_fields(*newp));
  GCPRO(newp);
  struct vector *v = alloc_vector(size);
  UNGCPRO();
  newp->values = v;
  newp->used = makeint(0);

  return newp;
}

void env_reserve(struct env *env, ulong n)
/* Effects: Makes sure that env has n free entries
*/
{
  ulong used = intval(env->used);
  ulong size = vector_len(env->values);

  if (used + n > size)
    {
      ulong newsize = 2 * size + n;
      GCPRO(env);
      struct vector *newp = alloc_vector(newsize);
      UNGCPRO();
      memcpy(newp->data, env->values->data, size * sizeof (value));
      env->values = newp;
    }
}

ulong env_grow_and_add_entry(struct env *env, value v)
/* Effects: Adds a new value to env, initialised to v.
   Returns: The index of the new value.
   Modifies: env.
*/
{
  ulong used = intval(env->used);

  GCCHECK(v); GCCHECK(env);
  GCPRO(v, env);
  env_reserve(env, 1);
  UNGCPRO();
  env->values->data[used] = v;
  env->used = mudlle_iadd(env->used, 1);

  return used;
}

#if 0
void print_env(struct oport *f, struct env *env)
{
  ulong used = intval(env->used), i;
  GCPRO(env);
  for (i = 0; i < used; i++)
    {
      pprintf(f, "%lu: ", i);
      output_value(f, prt_examine, env->values->data[i]);
      pprintf(f, "\n");
    }
  UNGCPRO();
}
#endif  /* 0 */
