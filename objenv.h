/* $Log: objenv.h,v $
 * Revision 1.5  1995/01/22  15:11:49  arda
 * Linux patches.
 *
 * Revision 1.4  1993/05/02  07:37:55  un_mec
 * Owl: New output (mudlle ports).
 *
 * Revision 1.3  1993/03/29  09:24:16  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.3  1993/03/14  16:14:39  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.1  1992/12/27  21:41:24  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

#ifndef OBJENV_H
#define OBJENV_H

#include "mudio.h"
#include "mvalues.h"

struct env			/* Is a record */
{
  struct obj o;
  value used;			/* # of elements used as opposed to allocated */
  value size;
  struct vector *values;
};

struct env *alloc_env(ulong size);
/* Returns: A new environment, of initial size size.
   Requires: size <= 2^30
*/

void env_reserve(struct env *env, ulong n);
/* Effects: Makes sure that env has n free entries
*/

ulong env_add_entry(struct env *env, value v);
/* Effects: Adds a new value to env, initialised to v.
   Returns: The index of the new value.
   Modifies: env.
   Requires: table contain less than 2^30 entries.
*/

#define ENV_ADD_ENTRY(env, v) \
  do { \
    if ((long)(env)->used >= (long)(env)->size) env_add_entry((env), (v)); \
    else \
      { \
	(env)->values->data[intval((env)->used)] = (v); \
	(env)->used = (value)((long)(env)->used + 2); \
      } \
  } while(0)

void print_env(struct oport *f, struct env *env);

#endif
