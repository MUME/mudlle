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
#include <stdlib.h>

#include "alloc.h"
#include "global.h"
#include "ins.h"
#include "lexer.h"
#include "mcompile.h"
#include "module.h"
#include "strbuf.h"
#include "tree.h"
#include "utils.h"


/* A list of global variable indexes */
typedef struct _glist {
  struct _glist *next;
  ulong n;
  bool used;
  int lineno;
} *glist;

static glist new_glist(block_t heap, ulong n, int lineno, glist next)
{
  glist newp = allocate(heap, sizeof *newp);
  *newp = (struct _glist){
    .next = next,
    .n = n,
    .used = false,
    .lineno = lineno
  };

  return newp;
}

static bool in_glist(ulong n, glist l, bool do_mark)
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

static glist readable;
static bool all_readable;
static glist writable;
static bool all_writable;
static glist definable;
static struct string *this_module;

/* A list of imported modules */
typedef struct _mlist {
  struct _mlist *next;
  const char *name;
  int status;
  bool used;
} *mlist;

static mlist imported_modules;

static mlist new_mlist(block_t heap, const char *name, int status, mlist next)
{
  mlist newp = allocate(heap, sizeof *newp);
  *newp = (struct _mlist){
    .next   = next,
    .name   = name,
    .status = status,
    .used   = false
  };

  return newp;
}

static int imported(const char *name, int do_mark)
/* Returns: status of name in imported_modules, module_unloaded if absent
*/
{
  for (mlist mods = imported_modules; mods; mods = mods->next)
    if (stricmp(mods->name, name) == 0)
      {
	if (do_mark)
	  mods->used = 1;
	return mods->status;
      }

  return module_unloaded;
}

static mfile current_mfile;

int mstart(block_t heap, mfile f, int seclev)
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

  mlist lmodules = NULL;

  yylineno = 0;
  if (f->name)
    {
      if (module_status(f->name) == module_loaded &&
	  module_seclevel(f->name) > seclev)
	return false;

      if (!module_unload(f->name))
	return false;

      module_set(f->name, module_loading, seclev);
    }

  /* Load all modules */
  for (vlist mods = f->imports; mods; mods = mods->next)
    {
      int mstatus = module_require(mods->var);

      if (mstatus < module_loaded)
	{
	  if (mstatus == module_loading)
	    log_error("loop in requires of %s", mods->var);
	  else
	    log_error("failed to load %s", mods->var);
          goto early_errout;
	}
      lmodules = new_mlist(heap, mods->var, mstatus, lmodules);
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
  for (vlist defines = f->defines; defines; defines = defines->next)
    {
      yylineno = defines->lineno;

      ulong n = global_lookup(defines->var);
      struct string *omod;
      int ostatus = module_vstatus(n, &omod);

      if (ostatus == var_system_write)
        log_error("cannot define %s: cannot be written from mudlle",
                  defines->var);
      else if ((ostatus == var_write && seclev < SECLEVEL_GLOBALS)
               || ostatus == var_system_write)
	log_error("cannot define %s: exists and is writable", defines->var);
      else if (!module_vset(n, var_module, this_module))
        log_error("cannot define %s: belongs to module %s", defines->var,
                  omod->str);
      else if (ostatus == var_write)
 	compile_warning("%s was writable", defines->var);

      definable = new_glist(heap, n, defines->lineno, definable);
    }

  for (vlist writes = f->writes; writes; writes = writes->next)
    {
      yylineno = writes->lineno;

      ulong n = global_lookup(writes->var);

      if (in_glist(n, definable, false))
        log_error("cannot write and define %s", writes->var);

      if (!module_vset(n, var_write, NULL))
	{
	  struct string *belongs;
	  enum vstatus vs = module_vstatus(n, &belongs);
	  switch (vs)
	    {
	    case var_system_write:
	      log_error("cannot write %s from mudlle", writes->var);
	      break;
	    case var_system_mutable:
              compile_warning("%s is always writable", writes->var);
              break;
	    case var_module:
	      assert(TYPE(belongs, type_string));
	      log_error("cannot write %s: belongs to module %s", writes->var,
			belongs->str);
	      break;
            case var_normal:
	    case var_write:
              abort();
	    }
          continue;
	}

      writable = new_glist(heap, n, f->lineno, writable);
    }

  for (vlist reads = f->reads; reads; reads = reads->next)
    {
      yylineno = reads->lineno;

      ulong n = global_lookup(reads->var);

      if (in_glist(n, definable, false))
        log_error("cannot read and define %s", reads->var);

      readable = new_glist(heap, n, reads->lineno, readable);
    }

  for (vlist statics = f->statics; statics; statics = statics->next)
    {
      yylineno = statics->lineno;

      ulong n;
      if (!global_exists(statics->var, &n))
        continue;

      if (in_glist(n, definable, false))
        log_error("cannot define static %s", statics->var);
      if (in_glist(n, writable, false))
        log_error("cannot write static %s", statics->var);
      if (in_glist(n, readable, false))
        log_error("cannot read static %s", statics->var);
    }

  yylineno = 0;

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

void mstop(mfile f)
{
  assert(current_mfile == f);
  current_mfile = NULL;
}

void mrecall(ulong n, const char *name, fncode fn)
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
        log_error("read of global %s (module %s)", name, mod->str);
    }
  else if (!all_readable)
    log_error("read of global %s", name);

   ins2(op_recall + global_var, n, fn);
}

void mexecute(ulong n, const char *name, int count, fncode fn)
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

 skip_checks:
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

bool mwritable(ulong n, const char *name)
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
  log_error("write of global %s", name);
  return false;
}

void massign(ulong n, const char *name, fncode fn)
/* Effects: Generate code to assign to variable n
*/
{
  assert(current_mfile != NULL);

  struct string *mod;
  int status = module_vstatus(n, &mod);

  if (status == var_module)
    if (mod == this_module && fntoplevel(fn))
      {
	if (!in_glist(n, definable, true))
          abort();

	/* defined here */
	ins2(op_define, n, fn);
      }
    else
      log_error("write of global %s (module %s)", name, mod->str);
  else if (mwritable(n, name))
    ins2(op_assign + global_var, n, fn);
}

void mwarn_module(int seclev, block b)
{
  assert(current_mfile != NULL);

  for (mlist ml = imported_modules; ml; ml = ml->next)
    if (!ml->used)
      warning_line(b->filename, b->nicename, 0,
                   "symbols from required module %s were never used",
                   ml->name);

  for (glist gl = readable; gl; gl = gl->next)
    {
      if (!gl->used)
        warning_line(b->filename, b->nicename, gl->lineno,
                     "readable global %s was never read",
                     GNAME(gl->n)->str);
      struct string *belongs;
      if (module_vstatus(gl->n, &belongs) == var_module)
        {
          int mlev = module_seclevel(belongs->str);
          strbuf_t sblev = SBNULL;
          if (mlev < seclev)
            {
                sb_printf(&sblev, " (lvl %d)", mlev);
            }
          warning_line(b->filename, b->nicename, gl->lineno,
                       "reads global variable %s defined in %s%s",
                       GNAME(gl->n)->str,
                       belongs->str, sb_str(&sblev));
          sb_free(&sblev);
        }
    }

  for (glist gl = writable; gl; gl = gl->next)
    if (!gl->used)
      warning_line(b->filename, b->nicename, gl->lineno,
                   "writable global %s was never written",
                   GNAME(gl->n)->str);

  for (glist gl = definable; gl; gl = gl->next)
    if (!gl->used)
      warning_line(b->filename, b->nicename, gl->lineno,
                   "definable variable %s was never defined",
                   GNAME(gl->n)->str);
}

void mcompile_init(void)
{
  staticpro(&this_module);
}
