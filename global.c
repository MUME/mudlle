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
#include "global.h"
#include "table.h"
#include "alloc.h"
#include "error.h"
#include "module.h"
#include "context.h"

#include "runtime/support.h"

struct env *environment;
struct vector *env_values;
/* map from global name to its index in mvars, env_values, and
   global_names */
struct table *global;
/* variable type and ownership: NULL is var_system_write; type_string
   is owning module; and integer is var_xxx (writable)*/
struct vector *mvars;
struct vector *global_names;

void global_init(void)
/* Effects: Initialises the global environment before use.
*/
{
  environment = alloc_env(GLOBAL_SIZE);
  staticpro(&environment);
  env_values = environment->values;
  staticpro(&env_values);
  global = alloc_table(GLOBAL_SIZE);
  staticpro(&global);
  mvars = alloc_vector(GLOBAL_SIZE);
  staticpro(&mvars);
  global_names = alloc_vector(GLOBAL_SIZE);
  staticpro(&global_names);
}

static ulong global_add(struct string *name, value val)
{
  GCCHECK(val);

  size_t nlen = string_len(name);
  assert(nlen >= 1 && nlen <= MAX_VARIABLE_LENGTH);
  assert(obj_readonlyp(&name->o));

  struct vector *old_values = env_values;
  GCPRO2(name, old_values);
  long old_size = intval(environment->size);
  long aindex = env_add_entry(environment, val);
  long new_size = intval(environment->size);
  if (new_size != old_size) /* Increase mvars too */
    {
      struct vector *vec = alloc_vector(new_size);

      memcpy(vec->data, mvars->data, mvars->o.size - sizeof (struct obj));
      mvars = vec;

      vec = alloc_vector(new_size);

      memcpy(vec->data, global_names->data,
             global_names->o.size - sizeof (struct obj));
      global_names = vec;

#if defined(i386) && !defined(NOCOMPILER)
      /* This is evil, but the alternative is to lose a scarce, callee-save
	 register */
      patch_globals_stack(old_values, environment->values);
#endif
    }
  env_values = environment->values;
  UNGCPRO();
  mvars->data[aindex] = makeint(var_normal);
  global_names->data[aindex] = name;
  make_readonly(table_add_fast(global, name, makeint(aindex)));

  return aindex;
}

ulong global_lookup(const char *name)
/* Returns: the index for global variable name in environment.
     If name doesn't exist yet, it is created with a variable
     whose value is NULL.
   Modifies: environment
*/
{
  ulong ofs;
  if (global_exists(name, &ofs))
    return ofs;

  struct string *mname = make_readonly(alloc_string(name));
  return global_add(mname, NULL);
}

bool global_exists(const char *name, ulong *ofs)
{
  struct symbol *pos = table_lookup(global, name);
  if (pos == NULL)
    return false;
  *ofs = intval(pos->data);
  return true;
}

ulong mglobal_lookup(struct string *name)
/* Returns: the index for global variable name in environment.
     If name doesn't exist yet, it is created with a variable
     whose value is NULL.
   Modifies: environment
*/
{
  struct symbol *pos = table_mlookup(global, name);
  if (pos)
    return intval(pos->data);

  struct string *tname;
  if (obj_readonlyp(&name->o))
    tname = name;
  else
    tname = make_readonly(mudlle_string_copy(name));

  return global_add(tname, NULL);
}

/* We can't rely on seclevel being set (not all code is calling secures),
 * so for the purpose of check_global_*() we'll approximate with maxseclevel
 * instead.
 * As a result, a V+ session calling M code will enable that M code to mess
 * with globals.
 */
static inline int seclevel_for_globals(void)
{
  return intval(maxseclevel);
}

void check_global_write(ulong goffset, value val)
{
  /* called from mudlle; must not allocate if it returns */
  assert(goffset < vector_len(global_names));

  if (GMUTABLE(goffset))
    return;

  if (GCONSTANT(goffset))
    global_runtime_error(error_variable_read_only, true, goffset, val);

  if (seclevel_for_globals() < SECLEVEL_GLOBALS)
    global_runtime_error(error_security_violation, true, goffset, val);
}

void check_global_read(ulong goffset)
{
  /* called from mudlle; must not allocate if it returns */
  if (seclevel_for_globals() >= SECLEVEL_GLOBALS)
    return;

  assert(goffset < vector_len(global_names));

  /* Using module_vstatus() instead of immutablep() because:
   * - not all globals are born immutable (the GC makes them)
   * - it nudges people to make libraries instead of modules
   * - (Dain has a better feeling about module_vstatus())
   *
   * Caveat: sometimes non-immutable defines slip through.
   */
  struct string *module_name = NULL;
  enum vstatus status = module_vstatus(goffset, &module_name);
  if (status != var_system_mutable && status != var_system_write
      && status != var_module)
    global_runtime_error(error_security_violation, false, goffset, NULL);
}
