/* $Log: compile.c,v $
 * Revision 1.32  1995/07/15  15:24:16  arda
 * Context cleanup.
 * Remove GCDEBUG.
 *
 * Revision 1.31  1995/06/04  14:24:26  arda
 * Rename/move some files, misc. junk
 *
 * Revision 1.30  1995/04/29  20:05:19  arda
 * fix
 *
 * Revision 1.29  1994/10/09  06:41:51  arda
 * Libraries
 * Type inference
 * Many minor improvements
 *
 * Revision 1.28  1994/08/29  13:17:15  arda
 * Contagious immutability.
 * Global array of values instead of variables.
 * Direct recursion.
 *
 * Revision 1.27  1994/08/22  11:18:20  arda
 * Moved code allocation to ins.c
 * Changes for mudlle compiler in MUME.
 *
 * Revision 1.26  1994/08/16  19:15:47  arda
 * Mudlle compiler for sparc now fully functional (68k compiler now needs
 * updating for primitives).
 * Changes to allow Sparc trap's for runtime errors.
 * Also added flags to primitives for better calling sequences.
 *
 * Revision 1.23  1994/03/23  14:31:18  arda
 * *** empty log message ***
 *
 * Revision 1.22  1994/02/24  08:32:48  arda
 * Owl: New error messages.
 *
 * Revision 1.21  1994/02/12  17:24:42  arda
 * Owl: Better code generated.
 *
 * Revision 1.20  1994/02/11  09:58:42  dgay
 * Owl: -Wall
 *      new shared string handling
 *      configuration file
 *
 * Revision 1.19  1994/01/29  19:50:23  dgay
 * Owl: add file & line information to functions.
 *
 * Revision 1.18  1994/01/27  21:59:30  dgay
 * Owl: Improve the collector (yet again).
 *      Now has just one zone for generation 0 (one extra copy involved).
 *
 * Revision 1.17  1994/01/10  13:34:55  arda
 * Fix compile (GCPRO missing).
 *
 * Revision 1.16  1994/01/08  12:49:40  dgay
 * Owl: Improved code generation for blocks (they are not implemented
 * as 0 argument functions anymore, they are folded into the current
 * function instead).
 *
 * Revision 1.15  1993/12/31  10:16:03  dgay
 * Owl: help string must be allocated before code.
 *
 * Revision 1.14  1993/12/23  20:48:50  dgay
 * Owl: New alloc.c: semi-generational collector.
 *      Included Amiga makefile for convenience.
 *
 * Revision 1.13  1993/11/27  11:28:59  arda
 * Owl: Major changes to affect.
 *      Save mudlle data with players & objects.
 *      Change skill format on disk.
 *      Other minor changes.
 *      Still needs full debugging.
 *
 * Revision 1.12  1993/10/03  14:07:13  dgay
 * Bumper disun8 update.
 *
 * Revision 1.11  1993/08/15  21:00:22  un_mec
 * Owl: Overload [].
 *      Added xcalloc, xrealloc.
 *
 * Revision 1.10  1993/07/21  20:36:36  un_mec
 * Owl: Added &&, ||, optimised if.
 *      Added branches to the intermediate language.
 *      Separated destiniation language generation into ins module
 *      (with some peephole optimisation)
 *      Standalone version of mudlle (mkf, runtime/mkf, mudlle.c) added to CVS
 *
 * Revision 1.9  1993/04/25  19:50:13  un_mec
 * Owl: Miscellaneous changes.
 *      I HATE fixing bugs twice.
 *
 * Revision 1.8  1993/04/24  15:20:32  un_mec
 * Owl: Code cleanup.
 *
 * Revision 1.7  1993/04/22  18:58:33  un_autre
 * (MD) & Owl. Bug fixes. /player fixes. EVER_WHINER flag. saving_spells adjusted.
 *
 * Revision 1.6  1993/04/17  10:01:14  un_autre
 * Various
 *
 * Revision 1.5  1993/04/10  09:16:56  un_mec
 * Owl: Debug mudlle.
 *
 * Revision 1.4  1993/04/02  18:08:46  un_autre
 * Owl: bug fixes.
 *
 * Revision 1.3  1993/03/29  09:23:42  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.4  1993/03/17  12:49:32  dgay
 * Fixed GC of help strings in code blocks.
 * Added security features.
 *
 * Revision 1.3  1993/03/14  16:14:03  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.1  1992/12/27  21:41:00  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

static char rcsid[] = "$Id: compile.c,v 1.32 1995/07/15 15:24:16 arda Exp $";

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

#include <string.h>
#include <stdlib.h>

static ulong builtin_functions[last_builtin];
static ubyte builtin_ops[last_builtin];
static component component_undefined, component_true, component_false;

static struct string *last_filename;
static const char *last_c_filename;
static uword compile_level;	/* Security level for generated code */
static int toplevel;		/* Hack of the day */

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

value make_list(cstlist csts)
{
  struct gcpro gcpro1;
  struct list *l = NULL;

  GCPRO1(l);
  /* Remember that csts is in reverse order ... */
  while (csts)
    {
      l = alloc_list(make_constant(csts->cst), l);
      l->o.flags |= OBJ_READONLY | OBJ_IMMUTABLE;
      csts = csts->next;
    }
  UNGCPRO();

  return l;
}

value make_array(cstlist csts)
{
  struct gcpro gcpro1;
  struct list *l;
  struct vector *v;
  ulong size = 0, i;
  cstlist scan;
  
  for (scan = csts; scan; scan = scan->next) size++;

  /* This intermediate step is necessary as v is IMMUTABLE
     (so must be allocated after its contents) */
  l = make_list(csts);
  GCPRO1(l);
  v = alloc_vector(size);
  v->o.flags |= OBJ_IMMUTABLE | OBJ_READONLY;
  UNGCPRO();

  for (i = 0; i < size; i++, l = l->cdr) v->data[i] = l->car;

  return v;
}

value make_constant(constant c)
{
  struct obj *cst;

  switch (c->class)
    {
    case cst_string:
      cst = (value)alloc_string(c->u.string);
      cst->flags |= OBJ_READONLY | OBJ_IMMUTABLE;
      return cst;
    case cst_list: return make_list(c->u.constants); break;
    case cst_array: return make_array(c->u.constants); break;
    case cst_int: return makeint(c->u.integer);
    }
  abort();
}

typedef void (*gencode)(void *data, fncode fn);

struct code *generate_function(function f, fncode fn);
void generate_component(component comp, fncode fn);
void generate_condition(component condition,
			label slab, gencode scode, void *sdata,
			label flab, gencode fcode, void *fdata,
			fncode fn);

struct andordata
{
  label label, slab, flab;
  gencode scode, fcode;
  void *sdata, *fdata;
  component arg2;
};

static void andorcode(void *_data, fncode fn)
{
  struct andordata *data = _data;

  set_label(data->label, fn);
  generate_condition(data->arg2,
		     data->slab, data->scode, data->sdata,
		     data->flab, data->fcode, data->fdata,
		     fn);
}

void generate_condition(component condition,
			label slab, gencode scode, void *sdata,
			label flab, gencode fcode, void *fdata,
			fncode fn)
{
  struct andordata data;

  switch (condition->class)
    {
    case c_builtin:
      switch (condition->u.builtin.fn)
	{
	case b_sc_and: case b_sc_or:
	  {
	    component arg1 = condition->u.builtin.args->c;

	    data.arg2 = condition->u.builtin.args->next->c;
	    data.label = new_label();
	    data.slab = slab; data.scode = scode; data.sdata = sdata;
	    data.flab = flab; data.fcode = fcode; data.fdata = fdata;

	    if (condition->u.builtin.fn == b_sc_and)
	      generate_condition(arg1,
				 data.label, andorcode, &data,
				 flab, NULL, NULL,
				 fn);
	    else
	      generate_condition(arg1,
				 slab, NULL, NULL,
				 data.label, andorcode, &data,
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

void generate_if(component condition, component success, component failure,
		 fncode fn)
{
  struct ifdata ifdata;

  ifdata.slab = new_label();
  ifdata.flab = new_label();
  ifdata.endlab = new_label();
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
}

void generate_while(component condition, component iteration, fncode fn)
{
  struct whiledata wdata;

  wdata.looplab = new_label();
  wdata.mainlab = new_label();
  wdata.exitlab = new_label();
  wdata.endlab = new_label();
  wdata.code = iteration;

  set_label(wdata.looplab, fn);
  generate_condition(condition, wdata.mainlab, wmain_code, &wdata,
		     wdata.exitlab, wexit_code, &wdata, fn);
  set_label(wdata.endlab, fn);
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

void generate_block(block b, fncode fn)
{
  clist cc = b->sequence;

  env_block_push(b->locals, fn);

  /* Generate code for sequence */
  for (; cc; cc = cc->next)
    {
      generate_component(cc->c, fn);
      if (cc->next) ins0(op_discard, fn);
    }
  env_block_pop();
}

void generate_execute(component call, int count, fncode fn)
{
  /* Optimise main case: calling a given global function */
  if (call->class == c_recall)
    {
      ulong offset;
      variable_class class = env_lookup(call->u.recall, &offset);

      if (class == global_var)
	{
	  mexecute(offset, call->u.recall, count, fn);
	  return;
	}
    }
  generate_component(call, fn);
  ins1(op_execute, count, fn);
}

void generate_component(component comp, fncode fn)
{
  clist args;

  switch (comp->class)
    {
    case c_assign:
      {
	ulong offset;
	variable_class class = env_lookup(comp->u.assign.symbol, &offset);
	component val = comp->u.assign.value;

	if (val->class == c_closure)
	  {
	    /* Defining a function, give it a name */
	    if (class == global_var)
	      val->u.closure->varname = comp->u.assign.symbol;
	    else
	      {
		char *varname = allocate(memory, strlen(comp->u.assign.symbol) + 7);

		sprintf(varname, "local-%s", comp->u.assign.symbol);
		val->u.closure->varname = varname;
	      }
	  }
	generate_component(comp->u.assign.value, fn);
	if (class == global_var)
	  massign(offset, comp->u.assign.symbol, toplevel, fn);
	else
	  ins1(op_assign + class, offset, fn);
	break;
      }
    case c_recall:
      {
	ulong offset;
	variable_class class = env_lookup(comp->u.recall, &offset);

	if (class == global_var) mrecall(offset, comp->u.recall, fn);
	else ins1(op_recall + class, offset, fn);
	break;
      }
    case c_constant:
      ins_constant(make_constant(comp->u.constant), fn);
      break;
    case c_closure:
      {
	int old_toplevel = toplevel;
	uword index;

	/* hack */
	toplevel = FALSE;
	index = add_constant(generate_function(comp->u.closure, fn), fn);
	toplevel = old_toplevel;

	if (index < ARG1_MAX) ins1(op_closure_code1, index, fn);
	else ins2(op_closure_code2, index, fn);
	break;
      }
    case c_block:
      generate_block(comp->u.block, fn);
      break;
    case c_labeled:
      start_block(comp->u.labeled.name, fn);
      generate_component(comp->u.labeled.expression, fn);
      end_block(fn);
      break;
    case c_exit:
      generate_component(comp->u.labeled.expression, fn);
      if (!exit_block(comp->u.labeled.name, fn))
	if (!comp->u.labeled.name)
	  error("No loop to exit from");
	else
	  error("No block labeled %s", comp->u.labeled.name);
      break;
    case c_execute:
      {
	uword count;

	generate_args(comp->u.execute->next, fn, &count);
	generate_execute(comp->u.execute->c, count, fn);
	break;
      }
    case c_builtin:
      args = comp->u.builtin.args;

      switch (comp->u.builtin.fn)
	{
	case b_if:
	  generate_if(args->c,
		      new_component(c_block,
				    new_codeblock(NULL,
				    new_clist(args->next->c,
				    new_clist(component_undefined, NULL)))),
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
	    label loop = new_label();

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
	    ins0(builtin_ops[comp->u.builtin.fn], fn);
	    break;
	  }
	default:
	  {
	    uword count;

	    assert(comp->u.builtin.fn < last_builtin);
	    generate_args(args, fn, &count);
	    mexecute(builtin_functions[comp->u.builtin.fn], NULL, count, fn);
	    break;
	  }
	}
      break;
    default: assert(0);
    }
}

struct code *generate_function(function f, fncode fn)
{
  struct code *c;
  struct string *help, *filename, *varname;
  fncode preamble;
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

  preamble = new_fncode();
  if (f->varargs)
    /* varargs makes a vector from the first nargs entries of the stack and
       stores it in local value 0 */
    ins0(op_varargs, preamble);
  else
    {
      /* First, generate code to check the argument types & count */
      /* argcheck copies the arguments into the local variables, assuming that
	 the last argument (on top of the stack) is local value 0, the next to
	 last local value 1, and so on.
	 It then discards all the parameters */
      for (nargs = 0, argument = f->args; argument; argument = argument->next)
	nargs++;
      ins1(op_argcheck, nargs, preamble);

      for (nargs = 0, argument = f->args; argument; argument = argument->next) 
	{
	  if (argument->type != stype_any)
	    ins1(op_typecheck + argument->type, nargs, preamble);

	  nargs++;
	}
      ins1(op_pop_n, nargs, preamble);
    }

  /* Generate code of function */
  env_push(f->args);
  
  start_block("function", preamble);
  generate_component(f->value, preamble);
  end_block(preamble);
  if (f->type != stype_any) ins1(op_typecheck + f->type, 0, preamble);
  ins0(op_return, preamble);
  peephole(preamble);
  c = generate_fncode(preamble, help, varname, filename, f->lineno,
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
    ins1(op_closure_var + cvar->class, cvar->offset, fn);

  return c;
}

static struct closure *compile_code(block b, int seclev)
{
  struct code *cc;
  struct gcpro gcpro1;
  uword dummy;
  fncode top;

  compile_level = seclev;
  erred = FALSE;
  env_reset();
  env_push(NULL);		/* Environment must not be totally empty */
  top = new_fncode();
  toplevel = TRUE;
  cc = generate_function(new_function(stype_any, NULL, NULL,
				      new_component(c_block, b),
				      0, ""), top);
  GCPRO1(cc);
  generate_fncode(top, NULL, NULL, NULL, 0, seclev);
  env_pop(&dummy);
  UNGCPRO();

  if (erred) return NULL;
  else return alloc_closure0(cc);
}

int interpret(value *result, int seclev, int reload)
{
  int ok = FALSE;
  block_t old_memory = memory, my_memory;

  my_memory = memory = new_block();
  erred = FALSE;
  if (yyparse() == 0 && !erred)
    {
      file f = parsed_code;

      if (f->name && !reload && module_status(f->name) != module_unloaded)
	return TRUE;

      if (mstart(f))
	{
	  value closure = compile_code(f->body, seclev);

	  if (closure)
	    {
#ifdef MUME
	      if (muduser && debug_level >= 2)
		{
		  struct gcpro gcpro1;
		  struct oport *f;

		  GCPRO1(closure);
		  f = char_output(muduser);
		  output_value(f, prt_examine, closure);
		  UNGCPRO();
		}
#endif
	      *result = catch_call0(closure);

	      ok = exception_signal == 0;
	    }
	  if (f->name) module_set(f->name, ok ? module_loaded : module_error);
	}
    }

  free_block(my_memory);
  memory = old_memory;

  return ok;
}

static block_t compile_block;

void compile_init(void)
{
  memory = compile_block = new_block();

  /* Note: These definitions actually depend on those in types.h and runtime.c */
  component_undefined = new_component(c_constant, new_constant(cst_int, 42));
  component_true = new_component(c_constant, new_constant(cst_int, TRUE));
  component_false = new_component(c_constant, new_constant(cst_int, FALSE));
  
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
