/*
 * Copyright (c) 1993-2004 David Gay and Gustav Hållberg
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
#include "alloc.h"
#include "types.h"
#include "code.h"
#include "ins.h"
#include "env.h"
#include "global.h"
#include "valuelist.h"
#include "calloc.h"
#include "runtime/runtime.h"
#include "utils.h"
#include "module.h"
#include "mcompile.h"
#include "mparser.h"
#include "call.h"
#include "runtime/bigint.h"
#include "table.h"
#include "compile.h"

#include <string.h>
#include <stdlib.h>

static ulong builtin_functions[last_builtin];
static ubyte builtin_ops[last_builtin];
component component_undefined, component_true, component_false;

static struct string *last_filename;
static const char *last_c_filename;
static uword compile_level;	/* Security level for generated code */

struct string *make_filename(const char *fname)
{
  if (strcmp(fname, last_c_filename))
    {
      free((void *)last_c_filename);
      last_c_filename = xstrdup(fname);
      last_filename = alloc_string(fname);
      last_filename->o.flags |= OBJ_READONLY;
    }
  return last_filename;
}

value make_constant(constant c);

static value make_list(cstlist csts, int has_tail)
{
  struct gcpro gcpro1;
  struct list *l;

  if (has_tail && csts != NULL)
    {
      l = csts->cst ? make_constant(csts->cst) : NULL;
      csts = csts->next;
    }
  else
    l = NULL;

  GCPRO1(l);
  /* Remember that csts is in reverse order ... */
  while (csts)
    {
      value tmp = make_constant(csts->cst);

      l = alloc_list(tmp, l);
      l->o.flags |= OBJ_READONLY | OBJ_IMMUTABLE;
      csts = csts->next;
    }
  UNGCPRO();

  return l;
}

static value make_bigint(const char *s)
{
  struct bigint *bi;
  mpz_t m;

  if (mpz_init_set_str(m, s, 0))
    assert(0);
  bi = alloc_bigint(m);
  free_mpz_temps();
  return (value)bi;
}

static value make_array(cstlist csts)
{
  struct gcpro gcpro1;
  struct list *l;
  struct vector *v;
  ulong size = 0, i;
  cstlist scan;
  
  for (scan = csts; scan; scan = scan->next) size++;

  /* This intermediate step is necessary as v is IMMUTABLE
     (so must be allocated after its contents) */
  l = make_list(csts, 0);
  GCPRO1(l);
  v = alloc_vector(size);
  v->o.flags |= OBJ_IMMUTABLE | OBJ_READONLY;
  UNGCPRO();

  for (i = 0; i < size; i++, l = l->cdr) v->data[i] = l->car;

  return v;
}

static void protect_symbol(struct symbol *s)
{
  s->o.flags |= OBJ_READONLY | OBJ_IMMUTABLE;
}

static value make_table(cstlist csts)
{
  struct gcpro gcpro1;
  struct table *t = alloc_table(DEF_TABLE_SIZE);
  
  GCPRO1(t);
  for (; csts; csts = csts->next)
    table_set(t, csts->cst->u.constpair->cst1->u.string,
	      make_constant(csts->cst->u.constpair->cst2));
  table_foreach(t, protect_symbol);
  immutable_table(t);
  UNGCPRO();

  return t;
}

static value make_symbol(cstpair p)
{
  struct symbol *sym;
  struct gcpro gcpro1;
  struct string *s = alloc_string(p->cst1->u.string);

  GCPRO1(s);
  s->o.flags |= OBJ_READONLY | OBJ_IMMUTABLE;
  sym = alloc_symbol(s, make_constant(p->cst2));
  sym->o.flags |= OBJ_READONLY | OBJ_IMMUTABLE;
  UNGCPRO();
  return sym;
}

value make_constant(constant c)
{
  struct obj *cst;

  switch (c->vclass)
    {
    case cst_string:
      cst = (value)alloc_string(c->u.string);
      cst->flags |= OBJ_READONLY | OBJ_IMMUTABLE;
      return cst;
    case cst_list: return make_list(c->u.constants, 1);
    case cst_array: return make_array(c->u.constants);
    case cst_int: return makeint(c->u.integer);
    case cst_float: return (value)alloc_mudlle_float(c->u.mudlle_float);
    case cst_bigint: return make_bigint(c->u.bigint_str);
    case cst_table: return make_table(c->u.constants);
    case cst_symbol: return make_symbol(c->u.constpair);
    default:
      abort();
    }
}

typedef void (*gencode)(void *data, fncode fn);

static struct code *generate_function(function f, int toplevel, fncode fn);
static void generate_component(component comp, fncode fn);
static void generate_condition(component condition,
			       label slab, gencode scode, void *sdata,
			       label flab, gencode fcode, void *fdata,
			       fncode fn);

struct andordata
{
  label lab, slab, flab;
  gencode scode, fcode;
  void *sdata, *fdata;
  component arg2;
};

static void andorcode(void *_data, fncode fn)
{
  struct andordata *data = _data;

  set_label(data->lab, fn);
  generate_condition(data->arg2,
		     data->slab, data->scode, data->sdata,
		     data->flab, data->fcode, data->fdata,
		     fn);
}

static void generate_condition(component condition,
			       label slab, gencode scode, void *sdata,
			       label flab, gencode fcode, void *fdata,
			       fncode fn)
{
  struct andordata data;

  switch (condition->vclass)
    {
    case c_builtin:
      switch (condition->u.builtin.fn)
	{
	case b_sc_and: case b_sc_or:
	  {
	    component arg1 = condition->u.builtin.args->c;

	    data.arg2 = condition->u.builtin.args->next->c;
	    data.lab = new_label(fn);
	    data.slab = slab; data.scode = scode; data.sdata = sdata;
	    data.flab = flab; data.fcode = fcode; data.fdata = fdata;

	    if (condition->u.builtin.fn == b_sc_and)
	      generate_condition(arg1,
				 data.lab, andorcode, &data,
				 flab, NULL, NULL,
				 fn);
	    else
	      generate_condition(arg1,
				 slab, NULL, NULL,
				 data.lab, andorcode, &data,
				 fn);
	    return;
	  }
	case b_not:
	  /* Just swap conclusions */
	  generate_condition(condition->u.builtin.args->c,
			     flab, fcode, fdata,
			     slab, scode, sdata,
			     fn);
	  return;
	}
      /* Fall through */
    default:
      generate_component(condition, fn);
      if (scode)
	{
	  branch(op_branch_z1, flab, fn);
	  scode(sdata, fn);
	  if (fcode) fcode(fdata, fn);
	}
      else
	{
	  branch(op_branch_nz1, slab, fn);
	  if (fcode) fcode(fdata, fn);
	  else branch(op_branch1, flab, fn);
	}
      break;
    }
}

struct ifdata
{
  label slab, flab, endlab;
  component success, failure;
};

static void ifs_code(void *_data, fncode fn)
{
  struct ifdata *data = _data;

  set_label(data->slab, fn);
  generate_component(data->success, fn);
  branch(op_branch1, data->endlab, fn);
  adjust_depth(-1, fn);
}

static void iff_code(void *_data, fncode fn)
{
  struct ifdata *data = _data;

  set_label(data->flab, fn);
  generate_component(data->failure, fn);
  branch(op_branch1, data->endlab, fn);
  adjust_depth(-1, fn);
}

static void generate_if(component condition, component success, 
			component failure, fncode fn)
{
  struct ifdata ifdata;

  ifdata.slab = new_label(fn);
  ifdata.flab = new_label(fn);
  ifdata.endlab = new_label(fn);
  ifdata.success = success;
  ifdata.failure = failure;

  generate_condition(condition, ifdata.slab, ifs_code, &ifdata,
		     ifdata.flab, iff_code, &ifdata, fn);
  set_label(ifdata.endlab, fn);
  adjust_depth(1, fn);
}

struct whiledata {
  label looplab, mainlab, exitlab, endlab;
  component code;
};

static void wmain_code(void *_data, fncode fn)
{
  struct whiledata *wdata = _data;

  set_label(wdata->mainlab, fn);
  generate_component(wdata->code, fn);
  branch(op_loop1, wdata->looplab, fn);
}

static void wexit_code(void *_data, fncode fn)
{
  struct whiledata *wdata = _data;

  set_label(wdata->exitlab, fn);
  generate_component(component_undefined, fn);
  branch(op_branch1, wdata->endlab, fn);
  adjust_depth(-1, fn);
}

static void generate_while(component condition, component iteration, fncode fn)
{
  struct whiledata wdata;

  wdata.looplab = new_label(fn);
  wdata.mainlab = new_label(fn);
  wdata.exitlab = new_label(fn);
  wdata.endlab = new_label(fn);
  wdata.code = iteration;

  set_label(wdata.looplab, fn);
  generate_condition(condition, wdata.mainlab, wmain_code, &wdata,
		     wdata.exitlab, wexit_code, &wdata, fn);
  set_label(wdata.endlab, fn);
  adjust_depth(1, fn);
}

void generate_args(clist args, fncode fn, uword *_count)
{
  uword count = 0;

  while (args)
    {
      count++;
      generate_component(args->c, fn);
      args = args->next;
    }
  *_count = count;
}

static void generate_block(block b, fncode fn)
{
  clist cc = b->sequence;
  vlist vl;

  env_block_push(b->locals);

  /* Generate code for sequence */
  for (; cc; cc = cc->next)
    {
      generate_component(cc->c, fn);
      if (cc->next) ins0(op_discard, fn);
    }

  for (vl = b->locals; vl; vl = vl->next)
    if (!vl->was_written)
      if (!vl->was_read)
	warning_line(b->filename, b->lineno, "local variable %s is unused", vl->var);
      else
	warning_line(b->filename, b->lineno, "local variable %s is never written", vl->var);
    else if (!vl->was_read)
      warning_line(b->filename, b->lineno, "local variable %s is never read", vl->var);
  env_block_pop();
}

static void generate_execute(component acall, int count, fncode fn)
{
  /* Optimise main case: calling a given global function */
  if (acall->vclass == c_recall)
    {
      ulong offset;
      variable_class vclass = env_lookup(acall->u.recall, &offset, 1, 0);

      if (vclass == global_var)
	{
	  mexecute(offset, acall->u.recall, count, fn);
	  return;
	}
    }
  generate_component(acall, fn);
  ins1(op_execute, count, fn);
}

static void generate_component(component comp, fncode fn)
{
  clist args;

  set_lineno(comp->lineno, fn);

  switch (comp->vclass)
    {
    case c_assign:
      {
	ulong offset;
	variable_class vclass = env_lookup(comp->u.assign.symbol, &offset, 0, 1);
	component val = comp->u.assign.value;

	if (val->vclass == c_closure)
	  {
	    /* Defining a function, give it a name */
	    if (vclass == global_var)
	      val->u.closure->varname = comp->u.assign.symbol;
	    else
	      {
		char *varname = allocate(fnmemory(fn), strlen(comp->u.assign.symbol) + 7);

		sprintf(varname, "local-%s", comp->u.assign.symbol);
		val->u.closure->varname = varname;
	      }
	  }
	generate_component(comp->u.assign.value, fn);

	set_lineno(comp->lineno, fn);

	if (vclass == global_var)
	  massign(offset, comp->u.assign.symbol, fn);
	else
	  ins1(op_assign + vclass, offset, fn);
	/* Note: varname becomes a dangling pointer when fnmemory(fn) is
	   deallocated, but it is never used again so this does not cause
	   a problem. */
	break;
      }
    case c_recall:
      {
	ulong offset;
	variable_class vclass = env_lookup(comp->u.recall, &offset, 1, 0);

	if (vclass == global_var) mrecall(offset, comp->u.recall, fn);
	else ins1(op_recall + vclass, offset, fn);
	break;
      }
    case c_constant:
      ins_constant(make_constant(comp->u.cst), fn);
      break;
    case c_closure:
      {
	uword idx;

	idx = add_constant(generate_function(comp->u.closure, FALSE, fn), fn);
	if (idx < ARG1_MAX) ins1(op_closure_code1, idx, fn);
	else ins2(op_closure_code2, idx, fn);
	break;
      }
    case c_block:
      generate_block(comp->u.blk, fn);
      break;
    case c_labeled:
      start_block(comp->u.labeled.name, fn);
      generate_component(comp->u.labeled.expression, fn);
      end_block(fn);
      break;
    case c_exit:
      generate_component(comp->u.labeled.expression, fn);
      if (!exit_block(comp->u.labeled.name, fn)) {
	if (!comp->u.labeled.name)
	  log_error("No loop to exit from");
	else
	  log_error("No block labeled %s", comp->u.labeled.name);
      }
      break;
    case c_execute:
      {
	uword count;

	generate_args(comp->u.execute->next, fn, &count);
	set_lineno(comp->lineno, fn);
	generate_execute(comp->u.execute->c, count, fn);
	break;
      }
    case c_builtin:
      args = comp->u.builtin.args;

      switch (comp->u.builtin.fn)
	{
	case b_if:
	  generate_if(args->c,
		      new_component(fnmemory(fn), c_block,
				    new_codeblock(fnmemory(fn), NULL,
						  new_clist(fnmemory(fn), args->next->c,
							    new_clist(fnmemory(fn), component_undefined, NULL)),
						  NULL,
						  -1)),
		      component_undefined,
		      fn);
	  break;
	case b_ifelse:
	  generate_if(args->c, args->next->c, args->next->next->c, fn);
	  break;
	case b_sc_and: case b_sc_or:
	  generate_if(comp, component_true, component_false, fn);
	  break;

	case b_while:
	  generate_while(args->c, args->next->c, fn);
	  break;

	case b_loop:
	  {
	    label loop = new_label(fn);

	    set_label(loop, fn);
	    start_block(NULL, fn);
	    generate_component(args->c, fn);
	    branch(op_loop1, loop, fn);
	    end_block(fn);
	    adjust_depth(1, fn);
	    break;
	  }

	case b_add: case b_subtract:
	case b_ref: case b_set:
	case b_bitor: case b_bitand:
	case b_not:
	case b_eq: case b_ne:
	case b_lt: case b_le: case b_ge: case b_gt:
	  {
	    uword count;

	    assert(comp->u.builtin.fn < last_builtin);
	    generate_args(args, fn, &count);
	    set_lineno(comp->lineno, fn);
	    ins0(builtin_ops[comp->u.builtin.fn], fn);
	    break;
	  }
	default:
	  {
	    uword count;

	    assert(comp->u.builtin.fn < last_builtin);
	    generate_args(args, fn, &count);
	    set_lineno(comp->lineno, fn);
	    mexecute(builtin_functions[comp->u.builtin.fn], NULL, count, fn);
	    break;
	  }
	}
      break;
    default: assert(0);
    }
}

static struct code *generate_function(function f, int toplevel, fncode fn)
{
  struct code *c;
  struct string *help, *filename, *varname;
  fncode newfn;
  vlist argument;
  uword nargs, clen;
  struct gcpro gcpro1, gcpro2, gcpro3;
  varlist closure, cvar;

  /* Make help string (must be allocated before code (immutability restriction)) */
  if (f->help)
    help = alloc_string(f->help);
  else
    help = NULL;
  GCPRO1(help);

  /* Make variable name (if present) */
  if (f->varname)
    varname = alloc_string(f->varname);
  else
    varname = NULL;
  GCPRO(gcpro2, varname);

  /* Make filename string */
  filename = make_filename(f->filename);
  GCPRO(gcpro3, filename);

  newfn = new_fncode(toplevel);

  set_lineno(f->lineno, newfn);

  if (f->varargs)
    /* varargs makes a vector from the first nargs entries of the stack and
       stores it in local value 0 */
    ins0(op_varargs, newfn);
  else
    {
      /* First, generate code to check the argument types & count */
      /* argcheck copies the arguments into the local variables, assuming that
	 the last argument (on top of the stack) is local value 0, the next to
	 last local value 1, and so on.
	 It then discards all the parameters */
      for (nargs = 0, argument = f->args; argument; argument = argument->next)
	nargs++;
      ins1(op_argcheck, nargs, newfn);

      for (nargs = 0, argument = f->args; argument; argument = argument->next) 
	{
	  if (argument->type != stype_any)
	    ins1(op_typecheck + argument->type, nargs, newfn);

	  nargs++;
	}
      ins1(op_pop_n, nargs, newfn);
    }

  /* Generate code of function */
  env_push(f->args, newfn);
  
  start_block("function", newfn);
  generate_component(f->value, newfn);
  end_block(newfn);
  if (f->type != stype_any) ins1(op_typecheck + f->type, 0, newfn);
  ins0(op_return, newfn);
  peephole(newfn);
  c = generate_fncode(newfn, help, varname, filename, f->lineno,
		      compile_level);
  closure = env_pop(&c->nb_locals);

  UNGCPRO();

  /* Generate code for creating closure */
  
  /* Count length of closure */
  clen = 0;
  for (cvar = closure; cvar; cvar = cvar->next) clen++;

  /* Generate closure */
  ins1(op_closure, clen, fn);

  /* Add variables to it */
  for (cvar = closure; cvar; cvar = cvar->next)
    ins1(op_closure_var + cvar->vclass, cvar->offset, fn);

  delete_fncode(newfn);

  return c;
}

struct closure *compile_code(mfile f, int seclev)
{
  struct code *cc;
  struct gcpro gcpro1;
  uword dummy;
  fncode top;
  block b = f->body;

  compile_level = seclev;
  erred = FALSE;
  env_reset();
  top = new_fncode(TRUE);
  env_push(NULL, top);		/* Environment must not be totally empty */
  cc = generate_function(new_function(fnmemory(top), stype_any, NULL, NULL,
				      new_component(fnmemory(top), c_block, b),
				      0, ""), TRUE, top);
  GCPRO1(cc);
  generate_fncode(top, NULL, NULL, NULL, 0, seclev);
  env_pop(&dummy);
  delete_fncode(top);
  UNGCPRO();

  if (erred)
    return NULL;
  return alloc_closure0(cc);
}

int interpret(value *result, int seclev, int reload)
{
  int ok = FALSE;
  block_t parser_block;
  mfile f;

  parser_block = new_block();
  if ((f = parse(parser_block)))
    {
      enum module_status status;

      if (f->name && !reload
	  && (status = module_status(f->name)) != module_unloaded)
	{
	  free_block(parser_block);
	  return status == module_loaded;
	}

      if (mstart(parser_block, f, seclev))
	{
	  struct closure *closure = compile_code(f, seclev);

	  if (closure)
	    {
	      mwarn_module();

	      *result = mcatch_call0(closure);

	      ok = exception_signal == 0;
	    }
	  if (f->name)
	    module_set(f->name, ok ? module_loaded : module_error, seclev);
	}
    }

  free_block(parser_block);

  return ok;
}

static block_t compile_block;

void compile_init(void)
{
  compile_block = new_block();

  /* Note: These definitions actually depend on those in types.h and runtime.c */
  component_undefined = new_component(compile_block, c_constant,
				      new_constant(compile_block, cst_int, 42));
  component_true = new_component(compile_block, c_constant,
				 new_constant(compile_block, cst_int, TRUE));
  component_false = new_component(compile_block, c_constant,
				  new_constant(compile_block, cst_int, FALSE));
  
  builtin_functions[b_or] = global_lookup("or");
  builtin_functions[b_and] = global_lookup("and");
  builtin_ops[b_eq] = op_builtin_eq;
  builtin_ops[b_ne] = op_builtin_neq;
  builtin_ops[b_lt] = op_builtin_lt;
  builtin_ops[b_le] = op_builtin_le;
  builtin_ops[b_gt] = op_builtin_gt;
  builtin_ops[b_ge] = op_builtin_ge;
  builtin_ops[b_bitor] = op_builtin_bitor;
  builtin_functions[b_bitxor] = global_lookup("^");
  builtin_ops[b_bitand] = op_builtin_bitand;
  builtin_functions[b_shift_left] = global_lookup("<<");
  builtin_functions[b_shift_right] = global_lookup(">>");
  builtin_ops[b_add] = op_builtin_add;
  builtin_ops[b_subtract] = op_builtin_sub;
  builtin_functions[b_multiply] = global_lookup("*");
  builtin_functions[b_divide] = global_lookup("/");
  builtin_functions[b_remainder] = global_lookup("%");
  builtin_functions[b_negate] = global_lookup("negate");
  builtin_ops[b_not] = op_builtin_not;
  builtin_functions[b_bitnot] = global_lookup("~");
  builtin_ops[b_ref] = op_builtin_ref;
  builtin_ops[b_set] = op_builtin_set;
  builtin_functions[b_cons] = global_lookup("cons");

  staticpro((value *)&last_filename);
  last_filename = alloc_string("");
  last_c_filename = xstrdup("");
}
