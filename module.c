/* $Log: module.c,v $
 * Revision 1.2  1995/07/15  15:24:31  arda
 * Context cleanup.
 * Remove GCDEBUG.
 *
 * Revision 1.1  1994/10/09  06:42:35  arda
 * Libraries
 * Type inference
 * Many minor improvements
 * */

#include <string.h>
#include "mudlle.h"
#include "table.h"
#include "global.h"
#include "alloc.h"
#include "module.h"
#include "call.h"

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

struct table *modules;
static ulong load_library;

int module_status(const char *name)
/* Returns: Status of module name:
     module_unloaded: module has never been loaded, or has been unloaded
     module_loaded: module loaded successfully
     module_error: attempt to load module led to error
*/
{
  struct symbol *sym;

  if (!table_lookup(modules, name, &sym)) return module_unloaded;
  return intval(sym->data);
}

void module_set(const char *name, int status)
/* Effects: Sets module status
*/
{
  table_set(modules, name, makeint(status));
}

int module_unload(const char *name)
/* Effects: Removes all knowledge about module 'name' (eg prior to reloading it)
     module_status(name) will return module_unloaded if this operation is
     successful
     Sets to null all variables that belonged to name, and resets their status
     to var_normal
   Returns: FALSE if name was protected or loading
*/
{
  int status = module_status(name);

  if (status != module_unloaded)
    {
      ulong gsize = intval(environment->used), i;

      if (status == module_loading || status == module_protected) return FALSE;

      for (i = 0; i < gsize; i++)
	{
	  struct string *v = mvars->data[i];

	  /* Unset module vars */
	  if (!integerp(v) && stricmp(name, v->str) == 0)
	    {
	      mvars->data[i] = makeint(var_normal);
	      GVAR(i) = NULL;
	    }
	}
      /* Safe even if name comes from a mudlle string,
	 because we know that entry name already exists */
      module_set(name, module_unloaded);
    }

  return TRUE;
}

int module_load(const char *name)
/* Effects: Attempts to load module name by calling mudlle hook
     Error/warning messages are sent to muderr
     Sets erred to TRUE in case of error
     Updates module status
   Modifies: erred
   Requires: module_status(name) == module_unloaded
   Returns: New module status
*/
{
  if (callablep(GVAR(load_library), 1))
    {
      struct string *mname = alloc_string(name);

      catch_call1(GVAR(load_library), mname);
    }
  return module_status(name);
}

int module_require(const char *name)
/* Effects: Does module_load(name) if module_status(name) == module_unloaded
     Other effects as in module_load
*/
{
  int status;

  if ((status = module_status(name)) == module_unloaded)
    status = module_load(name);

  return status;
}

int module_vstatus(long n, struct string **name)
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

  *name = v;
  return var_module;
}

int module_vset(long n, int status, struct string *name)
/* Effects: Sets status of global variable n to status.
     name is the module name for status var_module
   Returns: TRUE if successful, FALSE if the change is impossible
     (ie status was already var_module)
*/
{
  if (!integerp(mvars->data[n])) return FALSE;

  if (status == var_module)
    mvars->data[n] = name;
  else
    mvars->data[n] = makeint(status);

  return TRUE;
}

void module_init(void)
/* Initialise this module */
{
  modules = alloc_table(DEF_TABLE_SIZE);
  staticpro((value *)&modules);
  load_library = global_lookup("load_library");
}
