/* $Log: objenv.c,v $
 * Revision 1.8  1993/12/31  09:18:36  arda
 * Owl: Minor allocation bug.
 *
 * Revision 1.7  1993/11/27  11:29:04  arda
 * Owl: Major changes to affect.
 *      Save mudlle data with players & objects.
 *      Change skill format on disk.
 *      Other minor changes.
 *      Still needs full debugging.
 *
 * Revision 1.6  1993/05/02  13:02:52  un_mec
 * Owl: ARGH! Bugs.
 *
 * Revision 1.5  1993/05/02  07:37:53  un_mec
 * Owl: New output (mudlle ports).
 *
 * Revision 1.4  1993/04/22  18:58:49  un_autre
 * (MD) & Owl. Bug fixes. /player fixes. EVER_WHINER flag. saving_spells adjusted.
 *
 * Revision 1.3  1993/03/29  09:24:15  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.3  1993/03/14  16:14:35  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.2  1992/12/30  14:10:50  un_mec
 * Owl:
 * Several changes:
 * - Variables don't have separate value & function cells, instead their are
 *   now 2 types: type_function & type_variable.
 * - print_value: New types (list, vector), printing rationalised.
 * - New type: list (Lisp style pair)
 * - lexer.l: Debug read_from_string
 * - debug_level & DEBUG macro provided to help debugging.
 *
 * Revision 1.1  1992/12/27  21:41:21  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

static char rcsid[] = "$Id: objenv.c,v 1.8 1993/12/31 09:18:36 arda Exp $";

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
  struct env *new = (struct env *)allocate_record(type_internal, 3);
  value tmp;

  GCPRO1(new);
  tmp = alloc_vector(size);
  new->values = tmp;
  UNGCPRO();
  new->used = makeint(0);
  new->size = makeint(size);

  return new;
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
      struct vector *new;
      struct gcpro gcpro1;

      GCPRO1(env);
      new = alloc_vector(newsize);
      memcpy(new->data, env->values->data, size * sizeof(value));
      env->values = new;
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
