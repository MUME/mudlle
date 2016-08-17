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
#include "call.h"
#include "global.h"
#include "module.h"
#include "table.h"
#include "mvalues.h"

/* Module states automaton:

            -> unloaded	<-
           /      |       \
	   |      |	   \
	   |   loading	   |
	   |    /   \ 	   |
	   \   /     \	  /
	    error   loaded
	              |
	              |
	          protected

*/

enum {
  vmodule_status,
  vmodule_seclev,
  vmodule_size
};
struct table *module_data;

static ulong load_library;

enum module_status module_status(const char *name)
/* Returns: Status of module name:
     module_unloaded: module has never been loaded, or has been unloaded
     module_loaded: module loaded successfully
     module_error: attempt to load module led to error
*/
{
  struct symbol *sym = table_lookup(module_data, name);
  if (sym == NULL)
    return module_unloaded;
  return intval(((struct vector *)sym->data)->data[vmodule_status]);
}

int module_seclevel(const char *name)
{
  struct symbol *sym = table_lookup(module_data, name);
  if (sym == NULL)
    return -1;

  return intval(((struct vector *)sym->data)->data[vmodule_seclev]);
}

void module_set(const char *name, enum module_status status, int seclev)
/* Effects: Sets module status
*/
{
  struct symbol *sym = table_lookup(module_data, name);
  if (sym)
    {
      struct vector *v = sym->data;
      v->data[vmodule_status] = makeint(status);
      v->data[vmodule_seclev] = makeint(seclev);
      return;
    }

  assert(strlen(name) <= MAX_MODULE_NAME_LENGHT);

  struct vector *v = alloc_vector(vmodule_size);
  v->data[vmodule_status] = makeint(status);
  v->data[vmodule_seclev] = makeint(seclev);

  table_set(module_data, name, v);
}

bool module_unload(const char *name)
/* Effects: Removes all knowledge about module 'name' (eg prior to
     reloading it)
     module_status(name) will return module_unloaded if this operation is
     successful
     Sets to null all variables that belonged to name, and resets their status
     to var_normal
   Returns: false if name was protected or loading
*/
{
  enum module_status status = module_status(name);

  if (status != module_unloaded)
    {
      if (status == module_loading || status == module_protected)
        return false;

      ulong gsize = intval(environment->used);
      for (ulong i = 0; i < gsize; i++)
	{
	  struct string *v = mvars->data[i];

	  /* Unset module vars */
	  if (pointerp(v) && strcasecmp(name, v->str) == 0)
	    {
	      mvars->data[i] = makeint(var_normal);
	      GVAR(i) = NULL;
	    }
	}
      /* Safe even if name comes from a mudlle string,
	 because we know that entry name already exists */
      module_set(name, module_unloaded, 0);
    }

  return true;
}

enum module_status module_load(const char *name)
/* Effects: Attempts to load module name by calling mudlle hook
     Error/warning messages are sent to muderr
     Sets erred to true in case of error
     Updates module status
   Modifies: erred
   Requires: module_status(name) == module_unloaded
   Returns: New module status
*/
{
  if (callablep(GVAR(load_library), 1))
    {
      struct string *mname = alloc_string(name);

      mcatch_call1("call-load-library", GVAR(load_library), mname);
    }
  return module_status(name);
}

enum module_status module_require(const char *name)
/* Effects: Does module_load(name) if module_status(name) == module_unloaded
     Other effects as in module_load
*/
{
  enum module_status status = module_status(name);
  if (status == module_unloaded)
    status = module_load(name);

  return status;
}

enum vstatus module_vstatus(long n, struct string **name)
/* Returns: status of global variable n:
     var_normal: normal global variable, no writes
     var_write: global variable which is written
     var_module: defined symbol of a module
       module name is stored in *name
   Modifies: name
   Requires: n be a valid global variable offset
*/
{
  struct string *v = mvars->data[n];

  if (integerp(v)) return intval(v);
  if (v == NULL) return var_system_write;

  *name = v;
  return var_module;
}

bool module_vset(long n, enum vstatus status, struct string *name)
/* Effects: Sets status of global variable n to status.
     name is the module name for status var_module
   Returns: true if successful, false if the change is impossible
     (i.e., status was already var_module or var_system_{mutable,write})
*/
{
  assert(status >= 0 && status <= var_system_mutable);
  assert((name != NULL) == (status == var_module));

  if (GCONSTANT(n) || mvars->data[n] == makeint(var_system_mutable))
    return false;

  if (status == var_module)
    mvars->data[n] = name;
  else if (status == var_system_write)
    mvars->data[n] = NULL;
  else
    mvars->data[n] = makeint(status);

  return true;
}

void module_init(void)
/* Initialise this module */
{
  module_data = alloc_table(DEF_TABLE_SIZE);
  staticpro(&module_data);
  load_library = global_lookup("load_library");
}
