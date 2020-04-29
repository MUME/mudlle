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

#include "mudlle-config.h"

#include <stdlib.h>
#include <string.h>

#include "alloc.h"
#include "calloc.h"
#include "code.h"
#include "global.h"
#include "ins.h"
#include "lexer.h"
#include "mcompile.h"
#include "module.h"
#include "mvalues.h"
#include "print.h"
#include "strbuf.h"
#include "tree.h"
#include "utils.h"


/* A list of global variable indexes */
struct glist {
  struct glist *next;
  ulong n;
  bool used;
  struct loc loc;
};

static struct glist *new_glist(struct alloc_block *heap, ulong n,
                               const struct loc *loc, struct glist *next)
{
  struct glist *newp = allocate(heap, sizeof *newp);
  *newp = (struct glist){
    .next = next,
    .n    = n,
    .used = false,
    .loc  = *loc
  };

  return newp;
}

static bool in_glist(ulong n, struct glist *l, bool do_mark)
{
  for (; l; l = l->next)
    if (n == l->n)
      {
	if (do_mark)
	  l->used = true;
	return true;
      }

  return false;
}

static struct glist *readable;
static bool all_readable;
static struct glist *writable;
static bool all_writable;
static struct glist *definable;
static struct string *this_module;

/* A list of imported modules */
struct mlist {
  struct mlist *next;
  struct vlist *var;
  enum module_status status;
  bool used;
};

static struct mlist *imported_modules;

static struct mlist *new_mlist(struct alloc_block *heap, struct vlist *var,
                               enum module_status status, struct mlist *next)
{
  struct mlist *newp = allocate(heap, sizeof *newp);
  *newp = (struct mlist){
    .next   = next,
    .var    = var,
    .status = status,
    .used   = false
  };

  return newp;
}

static enum module_status imported(const char *name, int do_mark)
/* Returns: status of name in imported_modules, module_unloaded if absent
*/
{
  for (struct mlist *mods = imported_modules; mods; mods = mods->next)
    if (strcasecmp(mods->var->var, name) == 0)
      {
	if (do_mark)
	  mods->used = 1;
	return mods->status;
      }

  return module_unloaded;
}

static struct mfile *current_mfile;

bool mstart(struct alloc_block *heap, struct mfile *f, seclev_t seclev)
/* Effects: Start processing module f:
     - unload f
     - load required modules
     - change status of variables of f (defines, writes)
     - setup information for mrecall/massign/mexecute

     Sends error/warning messages.
   Returns: true if compilation can proceed
*/
{
  assert(current_mfile == NULL);

  erred = false;

  struct mlist *lmodules = NULL;

  if (f->name)
    {
      if (module_status(f->name) == module_loaded
          && module_seclevel(f->name) > seclev)
	return false;

      if (!module_unload(f->name))
	return false;

      module_set(f->name, module_loading, seclev);
    }

  /* Load all modules */
  for (struct vlist *mods = f->requires; mods; mods = mods->next)
    {
      enum module_status mstatus = module_require(mods->var);

      if (mstatus < module_loaded)
	{
	  if (mstatus == module_loading)
	    compile_error(&mods->loc, "loop in requires of %s", mods->var);
	  else
	    compile_error(&mods->loc, "failed to load %s", mods->var);
          goto early_errout;
	}
      lmodules = new_mlist(heap, mods, mstatus, lmodules);
    }

  current_mfile = f;

  all_writable = f->vclass == f_plain;
  all_readable = f->vclass == f_plain;
  readable = writable = definable = NULL;
  if (f->name)
    this_module = make_readonly(alloc_string(f->name));
  else
    this_module = NULL;
  imported_modules = lmodules;

  /* Change status of variables */
  for (struct vlist *defines = f->defines; defines; defines = defines->next)
    {
      ulong n = global_lookup(defines->var);
      struct string *omod;
      enum vstatus ostatus = module_vstatus(n, &omod);

      if (ostatus == var_system_write)
        compile_error(&defines->loc,
                  "cannot define %s: cannot be written from mudlle",
                  defines->var);
      else if ((ostatus == var_write && seclev < SECLEVEL_GLOBALS)
               || ostatus == var_system_write)
        compile_error(&defines->loc,
                  "cannot define %s: exists and is writable", defines->var);
      else if (!module_vset(n, var_module, this_module))
        compile_error(&defines->loc,
                  "cannot define %s: belongs to module %s", defines->var,
                  omod->str);
      else if (ostatus == var_write)
        compile_warning(&defines->loc, "%s was writable", defines->var);

      definable = new_glist(heap, n, &defines->loc, definable);
    }

  for (struct vlist *writes = f->writes; writes; writes = writes->next)
    {
      ulong n = global_lookup(writes->var);

      if (in_glist(n, definable, false))
        compile_error(&writes->loc, "cannot write and define %s", writes->var);

      if (!module_vset(n, var_write, NULL))
	{
	  struct string *belongs;
	  enum vstatus vs = module_vstatus(n, &belongs);
	  switch (vs)
	    {
	    case var_system_write:
	      compile_error(&writes->loc,
                        "cannot write %s from mudlle", writes->var);
	      break;
	    case var_system_mutable:
              compile_warning(&writes->loc, "%s is always writable",
                              writes->var);
              break;
	    case var_module:
	      assert(TYPE(belongs, string));
	      compile_error(&writes->loc,
                        "cannot write %s: belongs to module %s", writes->var,
			belongs->str);
	      break;
            case var_normal:
	    case var_write:
              abort();
	    }
          continue;
	}

      writable = new_glist(heap, n, &f->loc, writable);
    }

  for (struct vlist *reads = f->reads; reads; reads = reads->next)
    {
      ulong n = global_lookup(reads->var);

      if (in_glist(n, definable, false))
        compile_error(&reads->loc, "cannot read and define %s", reads->var);

      readable = new_glist(heap, n, &reads->loc, readable);
    }

  for (struct vlist *statics = f->statics; statics; statics = statics->next)
    {
      ulong n;
      if (!global_exists(statics->var, &n))
        continue;

      if (in_glist(n, definable, false))
        compile_error(&statics->loc, "cannot define static %s", statics->var);
      if (in_glist(n, writable, false))
        compile_error(&statics->loc, "cannot write static %s", statics->var);
      if (in_glist(n, readable, false))
        compile_error(&statics->loc, "cannot read static %s", statics->var);
    }

  if (erred)
    goto errout;
  return true;

 errout:
  mstop(f);
 early_errout:
  if (f->name)
    module_set(f->name, module_error, seclev);
  return false;
}

void mstop(struct mfile *f)
{
  assert(current_mfile == f);
  current_mfile = NULL;
}

void mrecall(const struct loc *loc, ulong n, const char *name,
             struct fncode *fn)
/* Effects: Generate code to recall variable n
*/
{
  assert(current_mfile != NULL);

  struct string *mod;
  enum vstatus status = module_vstatus(n, &mod);

  if (in_glist(n, definable, false)
      || in_glist(n, readable, true)
      || in_glist(n, writable, false)
      || status == var_system_write
      || status == var_system_mutable)
    ;
  else if (status == var_module)
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
        compile_error(loc, "read of global %s (module %s)", name, mod->str);
    }
  else if (!all_readable)
    compile_error(loc, "read of global %s", name);

   ins2(op_recall_global, n, fn);
}

void mexecute(const struct loc *loc, ulong n, const char *name, int count,
              struct fncode *fn)
/* Effects: Generates code to call function in variable n, with count
     arguments
*/
{
  assert(current_mfile != NULL);

  if (name == NULL)
    goto skip_checks;

  if (in_glist(n, definable, false) || in_glist(n, readable, true)
      || in_glist(n, writable, false))
    goto skip_checks;

  struct string *mod;
  enum vstatus status = module_vstatus(n, &mod);

  if (status == var_system_write || status == var_system_mutable)
    ;
  else if (status == var_module)
    {
      /* Implicitly import protected modules */
      if (module_status(mod->str) == module_protected)
        {
          value gvar = GVAR(n);

          imported(mod->str, 1);

          if (TYPE(gvar, primitive))
            {
              if (count >= 1 && count <= 2
                  && ((struct primitive *)gvar)->op->nargs == count)
                {
                  if (count == 1)
                    ins2(op_execute_primitive_1arg, n, fn);
                  else
                    ins2(op_execute_primitive_2arg, n, fn);
                }
              else
                {
                  /* Could merge, but can't be bothered... */
                  ins2(op_recall_global, n, fn);
                  if (count <= ARG1_MAX)
                    ins1(op_execute_primitive, count, fn);
                  else
                    ins2(op_execute_primitive2, count, fn);
                }
              return;
            }

          if (TYPE(gvar, secure))
            {
              /* Could merge, but can't be bothered... */
              ins2(op_recall_global, n, fn);
              if (count <= ARG1_MAX)
                ins1(op_execute_secure, count, fn);
              else
                ins2(op_execute_secure2, count, fn);
              return;
            }

          if (TYPE(gvar, varargs))
            {
              /* Could merge, but can't be bothered... */
              ins2(op_recall_global, n, fn);
              if (count <= ARG1_MAX)
                ins1(op_execute_varargs, count, fn);
              else
                ins2(op_execute_varargs2, count, fn);
              return;
            }
        }
      else if (!all_readable && imported(mod->str, 1) == module_unloaded)
        compile_error(loc, "read of global %s (module %s)", name, mod->str);
    }
  else if (!all_readable)
    compile_error(loc,"read of global %s", name);

 skip_checks:
  if (count == 1)
    ins2(op_execute_global_1arg, n, fn);
  else if (count == 2)
    ins2(op_execute_global_2arg, n, fn);
  else
    {
      /* Could have an op_execute_global */
      ins2(op_recall_global, n, fn);
      if (count <= ARG1_MAX)
        ins1(op_execute, count, fn);
      else
        ins2(op_execute2, count, fn);
    }
}

bool mwritable(const struct loc *loc, ulong n, const char *name)
{
  if (all_writable || in_glist(n, writable, true))
    {
      struct string *mod;
      if (module_vstatus(n, &mod) != var_write)
        module_vset(n, var_write, NULL);
      return true;
    }
  if (GMUTABLE(n))
    return true;
  compile_error(loc, "write of global %s", name);
  return false;
}

void massign(const struct loc *loc, ulong n, const char *name,
             struct fncode *fn)
/* Effects: Generate code to assign to variable n
*/
{
  assert(current_mfile != NULL);

  struct string *mod;
  enum vstatus status = module_vstatus(n, &mod);

  if (status == var_module)
    if (mod == this_module && fntoplevel(fn))
      {
	if (!in_glist(n, definable, true))
          abort();

	/* defined here */
	ins2(op_define, n, fn);
      }
    else
      compile_error(loc, "write of global %s (module %s)", name, mod->str);
  else if (mwritable(loc, n, name))
    ins2(op_assign_global, n, fn);
}

void mwarn_module(seclev_t seclev, struct block *b)
{
  assert(current_mfile != NULL);

  for (struct mlist *ml = imported_modules; ml; ml = ml->next)
    if (!ml->used)
      compile_warning(&ml->var->loc,
                  "symbols from required module %s were never used",
                  ml->var->var);

  for (struct glist *gl = readable; gl; gl = gl->next)
    {
      if (!gl->used)
        compile_warning(&gl->loc, "readable global %s was never read",
                    GNAME(gl->n)->str);
      struct string *belongs;
      if (module_vstatus(gl->n, &belongs) == var_module)
        {
          int mlev = module_seclevel(belongs->str);
          struct strbuf sblev = SBNULL;
          if (mlev < seclev)
            {
              sb_addstr(&sblev, " (lvl ");
              sb_add_seclevel(&sblev, mlev);
              sb_addstr(&sblev, ")");
            }
          compile_warning(&gl->loc, "reads global variable %s defined in %s%s",
                      GNAME(gl->n)->str, belongs->str, sb_str(&sblev));
          sb_free(&sblev);
        }
    }

  for (struct glist *gl = writable; gl; gl = gl->next)
    if (!gl->used)
      compile_warning(&gl->loc, "writable global %s was never written",
                  GNAME(gl->n)->str);

  for (struct glist *gl = definable; gl; gl = gl->next)
    if (!gl->used)
      compile_warning(&gl->loc,
                  "definable variable %s was never defined",
                  GNAME(gl->n)->str);
}

void mcompile_init(void)
{
  staticpro(&this_module);
}
