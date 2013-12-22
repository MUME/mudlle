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

#ifndef OBJENV_H
#define OBJENV_H

#include "types.h"

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

#endif
