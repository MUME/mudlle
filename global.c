/*
 * Copyright (c) 1993-1999 David Gay and Gustav Hållberg
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
#include "mudlle.h"
#include "global.h"
#include "objenv.h"
#include "table.h"
#include "alloc.h"
#include "types.h"
#include "error.h"
#include "module.h"

struct env *environment;
struct vector *env_values;
struct table *global;
struct vector *mvars;

void global_init(void)
/* Effects: Initialises the global environment before use.
*/
{
  environment = alloc_env(GLOBAL_SIZE);
  staticpro((value *)&environment);
  env_values = environment->values;
  staticpro((value *)&env_values);
  global = alloc_table(GLOBAL_SIZE);
  staticpro((value *)&global);
  mvars = alloc_vector(GLOBAL_SIZE);
  staticpro((value *)&mvars);
}

static ulong global_add(struct string *name, value val)
{
  struct symbol *pos;
  struct gcpro gcpro1, gcpro2;
  ulong aindex, old_size;
  struct vector *old_values = env_values;

  GCCHECK(val);

  GCPRO2(name, old_values);
  old_size = intval(environment->size);
  aindex = env_add_entry(environment, val);
  if (intval(environment->size) != old_size) /* Increase mvars too */
    {
      struct vector *new_mvars = alloc_vector(intval(environment->size));

      memcpy(new_mvars->data, mvars->data, mvars->o.size - sizeof(struct obj));
      mvars = new_mvars;

#ifdef i386
      /* This is evil, but the alternative is to lose a scarce, callee-save
	 register */
      patch_globals_stack(old_values, environment->values);
#endif
    }
  env_values = environment->values;
  UNGCPRO();
  mvars->data[aindex] = makeint(var_normal);
  pos = table_add_fast(global, name, makeint(aindex));
  pos->o.flags |= OBJ_READONLY; /* index of global vars never changes */

  return aindex;
}

ulong global_lookup(const char *name)
/* Returns: the index for global variable name in environment.
     If name doesn't exist yet, it is created with a variable
     whose value is NULL.
   Modifies: environment
*/
{
  struct symbol *pos;

  if (table_lookup(global, name, &pos)) return (ulong)intval(pos->data);

  return global_add(alloc_string(name), NULL);
}

ulong mglobal_lookup(struct string *name)
/* Returns: the index for global variable name in environment.
     If name doesn't exist yet, it is created with a variable
     whose value is NULL.
   Modifies: environment
*/
{
  struct symbol *pos;
  struct string *tname;
  struct gcpro gcpro1;

  if (table_lookup(global, name->str, &pos)) return (ulong)intval(pos->data);

  GCPRO1(name);
  tname = (struct string *)allocate_string(type_string, string_len(name) + 1);
  strcpy(tname->str, name->str);
  UNGCPRO();

  return global_add(tname, NULL);
}

struct list *global_list(void)
/* Returns: List of symbols representing all the global variables.
     The value cell of each symbol contains the variables number
*/
{
  return table_list(global);
}
