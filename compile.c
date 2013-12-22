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
#include "call.h"
#include "compile.h"
#include "context.h"
#include "env.h"
#include "global.h"
#include "mcompile.h"
#include "module.h"
#include "mparser.h"
#include "print.h"
#include "table.h"
#include "utils.h"

#include "runtime/runtime.h"
#include "runtime/bigint.h"


static ulong builtin_functions[last_builtin];
static ubyte builtin_ops[last_builtin];
component component_undefined, component_true, component_false;

static struct string *last_filename;
static char *last_c_filename;
static uword compile_level;	/* Security level for generated code */

mfile this_mfile;

struct string *make_filename(const char *fname)
{
  if (strcmp(fname, last_c_filename))
    {
      free(last_c_filename);
      last_c_filename = xstrdup(fname);
      last_filename = make_readonly(alloc_string(fname));
    }
  return last_filename;
}

value make_constant(constant c);

static value make_list(cstlist csts, int has_tail)
{
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
  mpz_t m;
  if (mpz_init_set_str(m, s, 0))
    abort();
  struct bigint *bi = alloc_bigint(m);
  free_mpz_temps();
  return (value)bi;
}

static value make_array(cstlist csts)
{
  ulong size = 0;

  for (cstlist scan = csts; scan; scan = scan->next) size++;

  /* This intermediate step is necessary as v is IMMUTABLE
     (so must be allocated after its contents) */
  struct list *l = make_list(csts, 0);

  GCPRO1(l);
  struct vector *v = alloc_vector(size);
  v->o.flags |= OBJ_IMMUTABLE | OBJ_READONLY;
  UNGCPRO();

  for (int i = 0; i < size; i++, l = l->cdr) v->data[i] = l->car;

  return v;
}

static void protect_symbol(struct symbol *s, void *dummy)
{
  s->o.flags |= OBJ_READONLY | OBJ_IMMUTABLE;
}

static value make_table(cstlist csts)
{
  struct table *t = alloc_table(DEF_TABLE_SIZE);

  GCPRO1(t);
  for (; csts; csts = csts->next)
    table_set_len(t,
                  csts->cst->u.constpair->cst1->u.string.str,
                  csts->cst->u.constpair->cst1->u.string.len,
                  make_constant(csts->cst->u.constpair->cst2));
  table_foreach(t, NULL, protect_symbol);
  immutable_table(t);
  UNGCPRO();

  return t;
}

static value make_symbol(cstpair p)
{
  struct symbol *sym;
  struct string *s = alloc_string_length(p->cst1->u.string.str,
                                         p->cst1->u.string.len);

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
      cst = (value)alloc_string_length(c->u.string.str, c->u.string.len);
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

static struct icode *generate_function(function f, int toplevel, fncode fn);
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
        default:
          break;
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

  env_start_loop();
  set_label(wdata.looplab, fn);
  generate_condition(condition, wdata.mainlab, wmain_code, &wdata,
		     wdata.exitlab, wexit_code, &wdata, fn);
  set_label(wdata.endlab, fn);
  env_end_loop();
  adjust_depth(1, fn);
}

static void generate_args(clist args, fncode fn, uword *_count)
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

  env_block_push(b->locals);

  /* Generate code for sequence */
  for (; cc; cc = cc->next)
    {
      generate_component(cc->c, fn);
      if (cc->next) ins0(op_discard, fn);
    }

  for (vlist vl = b->locals; vl; vl = vl->next)
    if (!vl->was_written)
      if (!vl->was_read)
	warning_line(b->filename, b->nicename, vl->lineno,
                     "local variable %s is unused", vl->var);
      else
	warning_line(b->filename, b->nicename, vl->lineno,
                     "local variable %s is never written", vl->var);
    else if (!vl->was_read)
      warning_line(b->filename, b->nicename, vl->lineno,
                   "local variable %s is never read", vl->var);
  env_block_pop();
}

static void generate_execute(component acall, int count, fncode fn)
{
  /* Optimise main case: calling a given global function */
  if (acall->vclass == c_recall)
    {
      ulong offset;
      variable_class vclass = env_lookup(acall->u.recall, &offset,
                                         true, false);

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
	variable_class vclass = env_lookup(comp->u.assign.symbol, &offset,
                                           false, true);
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
    case c_vref:
    case c_recall:
      {
        bool is_vref = comp->vclass == c_vref;
	ulong offset;
	variable_class vclass = env_lookup(comp->u.recall, &offset,
                                           true, is_vref);

	if (vclass != global_var)
          ins1((is_vref ? op_vref : op_recall) + vclass, offset, fn);
        else if (is_vref)
          {
            if (!mwritable(offset, comp->u.recall))
              return;
            ins_constant(makeint(offset), fn);
          }
        else
          mrecall(offset, comp->u.recall, fn);
        if (is_vref)
          mexecute(global_lookup("make_variable_ref"),
                   "make_variable_ref", 1, fn);
	break;
      }
    case c_constant:
      ins_constant(make_constant(comp->u.cst), fn);
      break;
    case c_closure:
      {
	uword idx;

	idx = add_constant(generate_function(comp->u.closure, false, fn), fn);
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
	  log_error("no loop to exit from");
	else
	  log_error("no block labeled %s", comp->u.labeled.name);
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
	case b_if: {
          block cb = new_codeblock(fnmemory(fn), NULL,
                                   new_clist(fnmemory(fn), args->next->c,
                                             new_clist(fnmemory(fn),
                                                       component_undefined,
                                                       NULL)),
                                   NULL, NULL, -1);
	  generate_if(args->c, new_component(fnmemory(fn),
                                             args->next->c->lineno,
                                             c_block, cb),
		      component_undefined, fn);
	  break;
        }
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

            env_start_loop();
	    set_label(loop, fn);
	    start_block(NULL, fn);
	    generate_component(args->c, fn);
	    branch(op_loop1, loop, fn);
	    end_block(fn);
            env_end_loop();
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

static struct vector *make_arg_types(function f)
{
  if (f->varargs)
    return NULL;

  int i = 0;
  for (vlist a = f->args; a; a = a->next)
    ++i;

  struct vector *result = alloc_vector(i);
  for (vlist a = f->args; a; a = a->next)
    result->data[--i] = makeint(a->typeset);

  result->o.flags |= OBJ_READONLY | OBJ_IMMUTABLE;
  return result;
}

static void generate_typeset_check(unsigned typeset, unsigned arg,
                                   fncode newfn)
{
  if (typeset == TYPESET_ANY)
    return;
  mtype t;
  if (typeset == TYPESET_FUNCTION)
    t = stype_function;
  else if (typeset == TYPESET_LIST)
    t = stype_list;
  else if (typeset == 0)
    t = stype_none;
  else if ((typeset & (typeset - 1)) == 0)
    t = ffs(typeset) - 1;
  else
    {
      ins_constant(makeint(typeset), newfn);
      ins1(op_typeset_check, arg, newfn);
      return;
    }
  ins1(op_typecheck + t, arg, newfn);
}

static struct icode *generate_function(function f, int toplevel, fncode fn)
{
  /* Make help string (must be allocated before code (immutability restriction)) */
  struct string *help = NULL;
  if (f->help.len)
    help = make_readonly(alloc_string_length(f->help.str, f->help.len));
  struct string *varname = NULL, *filename = NULL, *nicename = NULL;
  struct vector *arg_types = NULL;
  GCPRO5(help, varname, filename, nicename, arg_types);

  /* Make variable name (if present) */
  if (f->varname)
    varname = make_readonly(alloc_string(f->varname));
  else
    varname = NULL;

  /* Make filename string */
  filename = make_filename(f->filename);
  nicename = make_filename(f->nicename);

  arg_types = make_arg_types(f);

  fncode newfn = new_fncode(toplevel);

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
      int nargs = 0;
      for (vlist argument = f->args; argument; argument = argument->next)
	nargs++;
      ins1(op_argcheck, nargs, newfn);

      nargs = 0;
      for (vlist argument = f->args; argument; argument = argument->next)
	{
          generate_typeset_check(argument->typeset, nargs, newfn);
	  nargs++;
	}
      ins1(op_pop_n, nargs, newfn);
    }

  /* Generate code of function */
  env_push(f->args, newfn);

  start_block("function", newfn);
  generate_component(f->value, newfn);
  end_block(newfn);

  generate_typeset_check(f->typeset, 0, newfn);

  ins0(op_return, newfn);
  peephole(newfn);

  struct icode *c = generate_fncode(
    newfn, help, varname, filename, nicename, f->lineno, arg_types,
    f->typeset, compile_level);
  varlist closure = env_pop(&c->nb_locals);

  UNGCPRO();

  /* Generate code for creating closure */

  /* Count length of closure */
  int clen = 0;
  for (varlist cvar = closure; cvar; cvar = cvar->next) clen++;

  /* Generate closure */
  ins1(op_closure, clen, fn);

  /* Add variables to it */
  for (varlist cvar = closure; cvar; cvar = cvar->next)
    ins1(op_closure_var + cvar->vclass, cvar->offset, fn);

  delete_fncode(newfn);

  return c;
}

struct closure *compile_code(mfile f, int seclev)
{
  str_and_len_t sl = { .len = 0, .str = NULL };

  const char *filename = (f->body->filename
                          ? f->body->filename
                          : "");
  const char *nicename = (f->body->nicename
                            ? f->body->nicename
                            : "");
  compile_level = seclev;
  erred = false;
  env_reset();
  fncode top = new_fncode(true);
  env_push(NULL, top);		/* Environment must not be totally empty */
  function func = new_function(fnmemory(top), TYPESET_ANY, sl, NULL,
                               new_component(fnmemory(top), 0, c_block,
                                             f->body),
                               f->body->lineno,
                               filename, nicename);
  func->varname = "top-level";
  struct icode *cc = generate_function(func, true, top);

  GCPRO1(cc);
  generate_fncode(top, NULL, NULL, NULL, NULL, 0, NULL, TYPESET_ANY, seclev);
  uword dummy;
  env_pop(&dummy);
  delete_fncode(top);
  UNGCPRO();

  if (erred)
    return NULL;
  return alloc_closure0(&cc->code);
}

struct call_info {
  value f, *result;
};

static void docall0(void *_ci)
{
  struct call_info *ci = _ci;
  *ci->result = call0(ci->f);
}

bool interpret(value *result, int seclev, int reload)
{
  bool ok = false;
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

      mfile prev_mfile = this_mfile;
      this_mfile = f;
      if (mstart(parser_block, f, seclev))
	{
	  struct closure *closure = compile_code(f, seclev);
	  if (closure)
            mwarn_module(seclev, f->body);
          mstop(f);

	  if (closure)
            {
              struct call_info ci = {
                .f      = closure,
                .result = result
              };
	      ok = mcatch(docall0, &ci, call_trace_barrier);
	    }

	  if (f->name)
	    module_set(f->name, ok ? module_loaded : module_error, seclev);
	}
      this_mfile = prev_mfile;
    }

  free_block(parser_block);

  return ok;
}

static block_t compile_block;

void compile_init(void)
{
  compile_block = new_block();

  /* Note: These definitions actually depend on those in types.h and runtime.c */
  component_undefined = new_component(compile_block, 0, c_constant,
				      new_constant(compile_block, cst_int, 42));
  component_true = new_component(compile_block, 0, c_constant,
				 new_constant(compile_block, cst_int, true));
  component_false = new_component(compile_block, 0, c_constant,
				  new_constant(compile_block, cst_int, false));

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

  staticpro(&last_filename);
  last_filename = static_empty_string;
  last_c_filename = xstrdup("");
}
