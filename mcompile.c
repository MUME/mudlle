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
typedef struct _glist {
  struct _glist *next;
  ulong n;
  int used;
} *glist;

static glist new_glist(block_t heap, ulong n, glist next)
{
  glist newp = allocate(heap, sizeof *newp);

  newp->next = next;
  newp->n = n;
  newp->used = 0;

  return newp;
}

static int in_glist(ulong n, glist l, int do_mark)
{
  for (; l; l = l->next)
    if (n == l->n)
      {
	if (do_mark)
	  l->used = 1;
	return TRUE;
      }

  return FALSE;
}

static glist readable;
static int all_readable;
static glist writable;
static int all_writable;
static glist definable;
static struct string *this_module;

/* A list of imported modules */
typedef struct _mlist { 
  struct _mlist *next;
  const char *name;
  int status;
  int used;
} *mlist;

static mlist imported_modules;

static mlist new_mlist(block_t heap, const char *name, int status, mlist next)
{
  mlist newp = allocate(heap, sizeof *newp);

  newp->next = next;
  newp->name = name;
  newp->status = status;
  newp->used = 0;

  return newp;
}

static int imported(const char *name, int do_mark)
/* Returns: status of name in imported_modules, module_unloaded if absent
*/
{
  mlist mods;

  for (mods = imported_modules; mods; mods = mods->next)
    if (stricmp(mods->name, name) == 0) 
      {
	if (do_mark)
	  mods->used = 1;
	return mods->status;
      }

  return module_unloaded;
}

int mstart(block_t heap, mfile f, int seclev)
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
  mlist lmodules = NULL;

  if (f->name)
    {
      if (module_status(f->name) == module_loaded &&
	  module_seclevel(f->name) > seclev)
	return FALSE;
	
      if (!module_unload(f->name))
	return FALSE;

      module_set(f->name, module_loading, seclev);
    }

  /* Load all modules */
  for (mods = f->imports; mods; mods = mods->next)
    {
      int mstatus = module_require(mods->var);

      if (mstatus < module_loaded)
	{
	  if (mstatus == module_loading)
	    log_error("loop in requires of %s", mods->var);
	  else
	    log_error("failed to load %s", mods->var);
	  if (f->name)
	    module_set(f->name, module_error, seclev);
	  return FALSE;
	}
      lmodules = new_mlist(heap, mods->var, mstatus, lmodules);
    }

  all_writable = f->vclass == f_plain;
  all_readable = f->vclass == f_plain;
  readable = writable = definable = NULL;
  if (f->name) 
    {
      this_module = alloc_string(f->name);
      this_module->o.flags |= OBJ_READONLY;
    }
  else
    this_module = NULL;
  imported_modules = lmodules;

  /* Change status of variables */
  for (defines = f->defines; defines; defines = defines->next)
    {
      ulong n = global_lookup(defines->var);
      struct string *omod;
      int ostatus = module_vstatus(n, &omod);

      if (!module_vset(n, var_module, this_module))
	log_error("cannot define %s: belongs to module %s", defines->var, omod->str);
      else if (ostatus == var_write)
	warning("%s was writable", defines->var);

      definable = new_glist(heap, n, definable);
    }
      
  for (writes = f->writes; writes; writes = writes->next)
    {
      ulong n = global_lookup(writes->var);

      if (!module_vset(n, var_write, NULL))
	{
	  struct string *belongs;

	  module_vstatus(n, &belongs);
	  log_error("cannot write %s: belongs to module %s", writes->var, belongs->str);
	}

      writable = new_glist(heap, n, writable);
    }
      
  for (reads = f->reads; reads; reads = reads->next)
    readable = new_glist(heap, global_lookup(reads->var), readable);

  return TRUE;
}

void mrecall(ulong n, const char *name, fncode fn)
/* Effects: Generate code to recall variable n
*/
{
  struct string *mod;
  int status = module_vstatus(n, &mod);

  if (!in_glist(n, definable, 0) &&
      !in_glist(n, readable, 1) && !in_glist(n, writable, 0)) {
    if (status == var_module)
      {
	/* Implicitly import protected modules */
	if (module_status(mod->str) == module_protected)
	  {
	    imported(mod->str, 1);
	    if (immutablep(GVAR(n))) /* Use value */
	      {
		ins_constant(GVAR(n), fn);
		return;
	      }
	  }
	else if (!all_readable && imported(mod->str, 1) == module_unloaded)
	  log_error("read of global %s (module %s)", name, mod->str);
      }
    else if (!all_readable)
      log_error("read of global %s", name);
  }

  ins2(op_recall + global_var, n, fn);
}

void mexecute(ulong n, const char *name, int count, fncode fn)
/* Effects: Generates code to call function in variable n, with count
     arguments
*/
{
  struct string *mod;
  int status = module_vstatus(n, &mod);

  if (!in_glist(n, definable, 0) &&
      !in_glist(n, readable, 1) && !in_glist(n, writable, 0)) {
    if (status == var_module)
      {
	/* Implicitly import protected modules */
	if (module_status(mod->str) == module_protected)
	  {
	    value gvar = GVAR(n);

	    imported(mod->str, 1);

	    if (TYPE(gvar, type_primitive))
	      {
		if (count >= 1 && count <= 2 &&
		    ((struct primitive *)gvar)->op->nargs == count)
		  {
		    if (count == 1) ins2(op_execute_primitive1, n, fn);
		    else ins2(op_execute_primitive2, n, fn);
		  }
		else
		  {
		    /* Could merge, but can't be bothered... */
		    ins2(op_recall + global_var, n, fn);
		    ins1(op_execute_primitive, count, fn);
		  }
		return;
	      }

	    if (TYPE(gvar, type_secure))
	      {
		/* Could merge, but can't be bothered... */
		ins2(op_recall + global_var, n, fn);
		ins1(op_execute_secure, count, fn);
		return;
	      }

	    if (TYPE(gvar, type_varargs))
	      {
		/* Could merge, but can't be bothered... */
		ins2(op_recall + global_var, n, fn);
		ins1(op_execute_varargs, count, fn);
		return;
	      }
	  }
	else if (!all_readable && imported(mod->str, 1) == module_unloaded)
	  log_error("read of global %s (module %s)", name, mod->str);
      }
    else if (!all_readable)
      log_error("read of global %s", name);
  }

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

void massign(ulong n, const char *name, fncode fn)
/* Effects: Generate code to assign to variable n
*/
{
  struct string *mod;
  int status = module_vstatus(n, &mod);

  if (status == var_module)
    if (mod == this_module && fntoplevel(fn))
      {
	if (!in_glist(n, definable, 1))
	  assert(0);

	/* defined here */
	ins2(op_define, n, fn);
      }
    else
      log_error("write of global %s (module %s)", name, mod->str);
  else if (all_writable || in_glist(n, writable, 1))
    {
      ins2(op_assign + global_var, n, fn);
      if (status != var_write) module_vset(n, var_write, NULL);
    }
  else
    log_error("write of global %s", name);
}

void mwarn_module(const char *name)
{
  glist gl;

  for (mlist ml = imported_modules; ml; ml = ml->next)
    if (!ml->used)
      warning_line(name, 0, "symbols from required module %s were never used",
                   ml->name);

  for (gl = readable; gl; gl = gl->next)
    if (!gl->used)
      warning_line(name, 0, "readable variable %s was never read",
                   GNAME(gl->n)->str);

  for (gl = writable; gl; gl = gl->next)
    if (!gl->used)
      warning_line(name, 0, "writable variable %s was never written",
                   GNAME(gl->n)->str);

  for (gl = definable; gl; gl = gl->next)
    if (!gl->used)
      warning_line(name, 0, "definable variable %s was never defined",
                   GNAME(gl->n)->str);
}

void mcompile_init(void)
{
  staticpro((value *)&this_module);
}
