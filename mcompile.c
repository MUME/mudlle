/* $Log: mcompile.c,v $
 * Revision 1.3  1995/07/15  15:24:29  arda
 * Context cleanup.
 * Remove GCDEBUG.
 *
 * Revision 1.2  1994/11/08  09:23:17  arda
 * Protected modules are implicitly imported [fix].
 *
 * Revision 1.1  1994/10/09  06:42:29  arda
 * Libraries
 * Type inference
 * Many minor improvements
 * */
static char rcsid[] = "$Id: mcompile.c,v 1.3 1995/07/15 15:24:29 arda Exp $";

#include "mudlle.h"
#include "tree.h"
#include "types.h"
#include "code.h"
#include "ins.h"
#include "global.h"
#include "calloc.h"
#include "utils.h"
#include "module.h"

#include <string.h>
#include <stdlib.h>

/* A list of global variable indexes */
typedef struct glist {
  struct glist *next;
  ulong n;
} *glist;

static glist new_glist(ulong n, glist next)
{
  glist new = allocate(memory, sizeof *new);

  new->next = next;
  new->n = n;

  return new;
}

static int in_glist(ulong n, glist l)
{
  for (; l; l = l->next)
    if (n == l->n) return TRUE;

  return FALSE;
}

static glist readable;
static int all_readable;
static glist writable;
static int all_writable;
static glist definable;
static struct string *this_module;

/* A list of imported modules */
typedef struct mlist { 
  struct mlist *next;
  const char *name;
  int status;
} *mlist;

static mlist imported_modules;

static mlist new_mlist(const char *name, int status, mlist next)
{
  mlist new = allocate(memory, sizeof *new);

  new->next = next;
  new->name = name;
  new->status = status;

  return new;
}

static int imported(const char *name)
/* Returns: status of name in imported_modules, module_unloaded if absent
*/
{
  mlist mods;

  for (mods = imported_modules; mods; mods = mods->next)
    if (stricmp(mods->name, name) == 0) return mods->status;

  return module_unloaded;
}

int mstart(file f)
/* Effects: Start processing module f:
     - unload f
     - load required modules
     - change status of variables of f (defines, writes)
     - setup information for mrecall/massign/mexecute

     Sends error/warning messages.
   Returns: TRUE if compilation can proceed
*/
{
  vlist mods, reads, writes, defines;
  int all_loaded = TRUE;
  mlist modules = NULL;

  if (f->name)
    {
      if (!module_unload(f->name)) return FALSE;
      module_set(f->name, module_loading);
    }

  /* Load all modules */
  for (mods = f->imports; mods; mods = mods->next)
    {
      int mstatus = module_require(mods->var);

      if (mstatus < module_loaded)
	{
	  if (mstatus == module_loading)
	    error("loop in requires of %s", mods->var);
	  else
	    warning("failed to load %s", mods->var);
	  all_loaded = FALSE;
	}
      modules = new_mlist(mods->var, mstatus, modules);
    }

  all_writable = f->class == f_plain;
  all_readable = f->class == f_plain || !all_loaded;
  readable = writable = definable = NULL;
  if (f->name) 
    {
      this_module = alloc_string(f->name);
      this_module->o.flags |= OBJ_READONLY;
    }
  else
    this_module = NULL;
  imported_modules = modules;

  /* Change status of variables */
  for (defines = f->defines; defines; defines = defines->next)
    {
      ulong n = global_lookup(defines->var);
      struct string *omod;
      int ostatus = module_vstatus(n, &omod);

      if (!module_vset(n, var_module, this_module))
	error("cannot define %s: belongs to module %s", defines->var, omod->str);
      else if (ostatus == var_write)
	warning("%s was writable", defines->var);

      definable = new_glist(n, definable);
    }
      
  for (writes = f->writes; writes; writes = writes->next)
    {
      ulong n = global_lookup(writes->var);

      if (!module_vset(n, var_write, NULL))
	{
	  struct string *belongs;

	  module_vstatus(n, &belongs);
	  error("cannot write %s: belongs to module %s", writes->var, belongs->str);
	}

      writable = new_glist(n, writable);
    }
      
  for (reads = f->reads; reads; reads = reads->next)
    readable = new_glist(global_lookup(reads->var), readable);

  return TRUE;
}

void mrecall(ulong n, const char *name, fncode fn)
/* Effects: Generate code to recall variable n
*/
{
  struct string *mod;
  int status = module_vstatus(n, &mod);

  if (!in_glist(n, definable) &&
      !in_glist(n, readable) && !in_glist(n, writable))
    if (status == var_module)
      {
	/* Implicitly import protected modules */
	if (module_status(mod->str) == module_protected)
	  {
	    if (immutablep(GVAR(n))) /* Use value */
	      {
		ins_constant(GVAR(n), fn);
		return;
	      }
	  }
	else if (!all_readable && imported(mod->str) == module_unloaded)
	  error("read of global %s (module %s)", name, mod->str);
      }
    else if (!all_readable)
      error("read of global %s", name);

  ins2(op_recall + global_var, n, fn);
}

void mexecute(ulong n, const char *name, int count, fncode fn)
/* Effects: Generates code to call function in variable n, with count
     arguments
*/
{
  struct string *mod;
  int status = module_vstatus(n, &mod);

  if (!in_glist(n, definable) &&
      !in_glist(n, readable) && !in_glist(n, writable))
    if (status == var_module)
      {
	/* Implicitly import protected modules */
	if (module_status(mod->str) == module_protected)
	  {
	    value gvar = GVAR(n);

	    if (TYPE(gvar, type_primitive) &&
		count >= 1 && count <= 2 &&
		((struct primitive *)gvar)->op->nargs == count)
	      {
		if (count == 1) ins2(op_execute_primitive1, n, fn);
		else ins2(op_execute_primitive2, n, fn);
		return;
	      }
	  }
	else if (!all_readable && imported(mod->str) == module_unloaded)
	  error("read of global %s (module %s)", name, mod->str);
      }
    else if (!all_readable)
      error("read of global %s", name);

  if (count == 1)
    ins2(op_execute_global1, n, fn);
  else if (count == 2)
    ins2(op_execute_global2, n, fn);
  else
    {
      /* Could have an op_execute_global */
      ins2(op_recall + global_var, n, fn);
      ins1(op_execute, count, fn);
    }
}

void massign(ulong n, const char *name, int toplevel, fncode fn)
/* Effects: Generate code to assign to variable n
*/
{
  struct string *mod;
  int status = module_vstatus(n, &mod);

  if (status == var_module)
    if (mod == this_module && toplevel) 
      /* defined here */
      ins2(op_define, n, fn);
    else
      error("write of global %s (module %s)", name, mod->str);
  else if (all_writable || in_glist(n, writable))
    {
      ins2(op_assign + global_var, n, fn);
      if (status != var_write) module_vset(n, var_write, NULL);
    }
  else
    error("write of global %s", name);
}

void mcompile_init(void)
{
  staticpro((value *)&this_module);
}
