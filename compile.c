/*
 * Copyright (c) 1993-2012 David Gay and Gustav H�llberg
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
#include "calloc.h"
#include "code.h"
#include "compile.h"
#include "context.h"
#include "global.h"
#include "ins.h"
#include "lexer.h"
#include "mcompile.h"
#include "module.h"
#include "mparser.h"
#include "print.h"
#include "table.h"
#include "tree.h"
#include "utils.h"

#include "runtime/bigint.h"
#include "runtime/mudlle-string.h"


static ulong g_get_static, g_symbol_set, g_symbol_get;
static ulong g_make_variable_ref, g_make_symbol_ref;

static ulong builtin_functions[last_builtin];
static const ubyte builtin_ops[last_builtin] = {
  [b_add]      = op_builtin_add,
  [b_bitand]   = op_builtin_bitand,
  [b_bitor]    = op_builtin_bitor,
  [b_eq]       = op_builtin_eq,
  [b_ge]       = op_builtin_ge,
  [b_gt]       = op_builtin_gt,
  [b_le]       = op_builtin_le,
  [b_lt]       = op_builtin_lt,
  [b_ne]       = op_builtin_neq,
  [b_not]      = op_builtin_not,
  [b_ref]      = op_builtin_ref,
  [b_set]      = op_builtin_set,
  [b_subtract] = op_builtin_sub,
};
struct component *component_undefined, *component_true, *component_false;

static struct string *last_filename;
static char *last_c_filename;
static uword compile_level;	/* Security level for generated code */

struct mfile *this_mfile;

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

static value make_list(const struct cstlist *csts)
{
  if (csts == NULL)
    return NULL;

  /* the first entry has the list tail */
  struct list *l = csts->cst ? make_constant(csts->cst) : NULL;
  csts = csts->next;

  GCPRO1(l);
  /* Remember that csts is in reverse order ... */
  while (csts)
    {
      value tmp = make_constant(csts->cst);
      assert(immutablep(tmp));
      l = alloc_list(tmp, l);
      l->o.flags |= OBJ_READONLY | OBJ_IMMUTABLE;
      csts = csts->next;
    }
  UNGCPRO();

  return l;
}

static value make_bigint(const struct bigint_const *bc)
{
  mpz_t m;
  if (mpz_init_set_str(m, bc->str, bc->base))
    abort();
  if (bc->neg)
    mpz_neg(m, m);
  struct bigint *bi = alloc_bigint(m);
  free_mpz_temps();
  return (value)bi;
}

static value make_array(const struct cstlist *csts)
{
  ulong size = 0;
  for (const struct cstlist *scan = csts; scan; scan = scan->next)
    ++size;

  struct vector *v = alloc_vector(size);
  GCPRO1(v);
  for (const struct cstlist *scan = csts; scan; scan = scan->next)
    {
      value val = make_constant(scan->cst);
      assert(immutablep(val));
      v->data[--size] = val;
    }
  UNGCPRO();

  assert(size == 0);

  v->o.flags |= OBJ_IMMUTABLE | OBJ_READONLY;

  return v;
}

static struct table *string_cache = NULL;

static value make_string(const struct str_and_len *str)
{
  if (string_cache != NULL)
    {
      struct symbol *sym = table_lookup_len(string_cache, str->str, str->len);
      if (sym != NULL)
        return sym->name;
    }
  struct string *s = make_readonly(alloc_string_length(str->str, str->len));
  if (string_cache != NULL && (~string_cache->o.flags & OBJ_READONLY))
    table_add(string_cache, s, makebool(true));
  return s;
}

static void protect_symbol(struct symbol *s, void *dummy)
{
  s->o.flags |= OBJ_READONLY | OBJ_IMMUTABLE;
}

static value make_table(const struct cstlist *csts, bool ctable)
{
  ulong l = 0;
  for (const struct cstlist *c = csts; c; c = c->next)
    ++l;
  struct table *t
    = (ctable ? alloc_ctable : alloc_table)(table_good_size(l));

  value name = NULL;
  GCPRO2(t, name);
  for (; csts; csts = csts->next)
    {
      name = make_string(&csts->cst->u.constpair->cst1->u.string);
      value cst = make_constant(csts->cst->u.constpair->cst2);
      table_mset(t, name, cst);
    }
  UNGCPRO();
  table_foreach(t, NULL, protect_symbol);
  immutable_table(t);

  return t;
}

static value make_symbol(const struct cstpair *p)
{
  struct string *s = make_string(&p->cst1->u.string);
  GCPRO1(s);
  value cst = make_constant(p->cst2);
  UNGCPRO();
  struct symbol *sym = alloc_symbol(s, cst);
  sym->o.flags |= OBJ_READONLY | OBJ_IMMUTABLE;
  return sym;
}

value make_constant(const struct constant *c)
{
  switch (c->vclass)
    {
    case cst_string: return make_string(&c->u.string);
    case cst_list:   return make_list(c->u.constants);
    case cst_array:  return make_array(c->u.constants);
    case cst_int:    return makeint(c->u.integer);
    case cst_float:  return alloc_mudlle_float(c->u.mudlle_float);
    case cst_bigint: return make_bigint(c->u.bigint);
    case cst_ctable: return make_table(c->u.constants, true);
    case cst_table:  return make_table(c->u.constants, false);
    case cst_symbol: return make_symbol(c->u.constpair);
    case cst_expression: abort();
    }
  abort();
}

value make_shared_string_constant(const struct constant *c,
                                  struct table *cache)
{
  assert(string_cache == NULL);
  string_cache = cache;
  GCPRO1(string_cache);
  value r = make_constant(c);
  UNGCPRO();
  string_cache = NULL;
  return r;
}

typedef void (*gencode_fn)(void *data, struct fncode *fn);

static struct icode *generate_function(struct function *f, bool toplevel,
                                       struct fncode *fn);
static void generate_component(struct component *comp, struct fncode *fn);
static void generate_condition(struct component *condition,
			       struct label *slab, gencode_fn scode,
			       void *sdata, struct label *flab,
			       gencode_fn fcode, void *fdata,
			       struct fncode *fn);

struct andordata
{
  struct label *lab, *slab, *flab;
  gencode_fn scode, fcode;
  void *sdata, *fdata;
  struct component *arg2;
};

static void andorcode(void *_data, struct fncode *fn)
{
  struct andordata *data = _data;

  set_label(data->lab, fn);
  generate_condition(data->arg2,
		     data->slab, data->scode, data->sdata,
		     data->flab, data->fcode, data->fdata,
		     fn);
}

static void generate_condition(struct component *condition,
			       struct label *slab, gencode_fn scode,
			       void *sdata, struct label *flab,
			       gencode_fn fcode, void *fdata,
			       struct fncode *fn)
{
  struct andordata data;

  switch (condition->vclass)
    {
    case c_builtin:
      switch (condition->u.builtin.fn)
	{
	case b_sc_and: case b_sc_or:
	  {
	    struct component *arg1 = condition->u.builtin.args->c;

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
  struct label *slab, *flab, *endlab;
  struct component *success, *failure;
};

static void ifs_code(void *_data, struct fncode *fn)
{
  struct ifdata *data = _data;

  set_label(data->slab, fn);
  generate_component(data->success, fn);
  branch(op_branch1, data->endlab, fn);
  adjust_depth(-1, fn);
}

static void iff_code(void *_data, struct fncode *fn)
{
  struct ifdata *data = _data;

  set_label(data->flab, fn);
  generate_component(data->failure, fn);
  branch(op_branch1, data->endlab, fn);
  adjust_depth(-1, fn);
}

static void generate_if(struct component *condition, struct component *success,
			struct component *failure, struct fncode *fn)
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
  struct label *looplab, *mainlab, *exitlab, *endlab;
  struct component *code;
};

static void wmain_code(void *_data, struct fncode *fn)
{
  struct whiledata *wdata = _data;

  set_label(wdata->mainlab, fn);
  generate_component(wdata->code, fn);
  branch(op_loop1, wdata->looplab, fn);
}

static void wexit_code(void *_data, struct fncode *fn)
{
  struct whiledata *wdata = _data;

  set_label(wdata->exitlab, fn);
  generate_component(component_undefined, fn);
  branch(op_branch1, wdata->endlab, fn);
  adjust_depth(-1, fn);
}

static void generate_while(struct component *condition,
                           struct component *iteration, struct fncode *fn)
{
  struct whiledata wdata = {
    .looplab = new_label(fn),
    .mainlab = new_label(fn),
    .exitlab = new_label(fn),
    .endlab  = new_label(fn),
    .code    = iteration
  };

  env_start_loop();
  set_label(wdata.looplab, fn);
  start_block(NULL, fn);
  generate_condition(condition, wdata.mainlab, wmain_code, &wdata,
		     wdata.exitlab, wexit_code, &wdata, fn);
  set_label(wdata.endlab, fn);
  end_block(fn);
  env_end_loop();
  adjust_depth(1, fn);
}

static void generate_args(struct clist *args, struct fncode *fn, uword *_count)
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

static void generate_block(struct block *b, struct fncode *fn)
{
  struct clist *cc = b->sequence;

  env_block_push(b->locals, b->statics);

  if (b->statics)
    for (struct vlist *vl = b->locals; vl; vl = vl->next)
      {
        ulong offset;
        bool is_static;
        enum variable_class vclass = env_lookup(vl->var, &offset,
                                                false, true, &is_static);
        assert(is_static && vclass == vclass_local);
        ins_constant(alloc_string(vl->var), fn);
        mexecute(g_get_static, NULL, 1, fn);
        ins1(op_assign + vclass, offset, fn);
      }

  /* Generate code for sequence */
  for (; cc; cc = cc->next)
    {
      generate_component(cc->c, fn);
      if (cc->next) ins0(op_discard, fn);
    }

  for (struct vlist *vl = b->locals; vl; vl = vl->next)
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

static void generate_execute(struct component *acall, int count,
                             struct fncode *fn)
{
  /* Optimise main case: calling a given global struct function **/
  if (acall->vclass == c_recall)
    {
      ulong offset;
      bool is_static;
      enum variable_class vclass = env_lookup(acall->u.recall, &offset,
                                              true, false, &is_static);

      if (vclass == vclass_global)
	{
          assert(!is_static);
	  mexecute(offset, acall->u.recall, count, fn);
	  return;
	}
    }
  generate_component(acall, fn);
  ins1(op_execute, count, fn);
}

static void generate_component(struct component *comp, struct fncode *fn)
{
  struct clist *args;

  set_lineno(comp->lineno, fn);

  switch (comp->vclass)
    {
    case c_assign:
      {
	ulong offset;
        bool is_static;
	enum variable_class vclass = env_lookup(comp->u.assign.symbol, &offset,
                                                false, true, &is_static);
	struct component *val = comp->u.assign.value;

	if (val->vclass == c_closure)
	  {
	    /* Defining a function, give it a name */
	    if (vclass == vclass_global)
	      val->u.closure->varname = comp->u.assign.symbol;
	    else
	      {
		char *varname = allocate(fnmemory(fn),
                                         strlen(comp->u.assign.symbol) + 7);

		sprintf(varname, "local-%s", comp->u.assign.symbol);
		val->u.closure->varname = varname;
	      }
	  }

        if (is_static)
          {
            ins1(op_recall + vclass, offset, fn);
            generate_component(comp->u.assign.value, fn);
            mexecute(g_symbol_set, NULL, 2, fn);
            break;
          }

	generate_component(comp->u.assign.value, fn);

	set_lineno(comp->lineno, fn);

        if (vclass == vclass_global)
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
        bool is_static;
	enum variable_class vclass = env_lookup(comp->u.recall, &offset,
                                                true, is_vref, &is_static);

        if (is_static)
          {
            assert(vclass != vclass_global);
            ins1(op_recall + vclass, offset, fn);
            ulong gidx = is_vref ? g_make_symbol_ref : g_symbol_get;
            mexecute(gidx, NULL, 1, fn);
            break;
          }
	if (vclass != vclass_global)
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
          mexecute(g_make_variable_ref, "make_variable_ref", 1, fn);
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
      if (!exit_block(comp->u.labeled.name, fn))
        {
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
          struct block *cb = new_codeblock(fnmemory(fn), NULL,
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
	    struct label *loop = new_label(fn);

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
    default: abort();
    }
}

static struct vector *make_arg_types(struct function *f)
{
  if (f->varargs)
    return NULL;

  int i = 0;
  for (struct vlist *a = f->args; a; a = a->next)
    ++i;

  struct vector *result = alloc_vector(i);
  for (struct vlist *a = f->args; a; a = a->next)
    result->data[--i] = makeint(a->typeset);

  result->o.flags |= OBJ_READONLY | OBJ_IMMUTABLE;
  return result;
}

static void generate_typeset_check(unsigned typeset, unsigned arg,
                                   struct fncode *newfn)
{
  if (typeset == TYPESET_ANY)
    return;
  enum mudlle_type t;
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

static struct icode *generate_function(struct function *f, bool toplevel,
                                       struct fncode *fn)
{
  /* make help string; must be allocated before code (immutability
     restriction) */
  struct string *help = NULL;
  if (f->help.len)
    help = make_string(&f->help);
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

  struct fncode *newfn = new_fncode(toplevel);

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
      for (struct vlist *argument = f->args;
           argument;
           argument = argument->next)
	nargs++;
      ins1(op_argcheck, nargs, newfn);

      nargs = 0;
      for (struct vlist *argument = f->args;
           argument;
           argument = argument->next)
	{
          generate_typeset_check(argument->typeset, nargs, newfn);
	  nargs++;
	}
      if (nargs > 0)
        ins1(op_pop_n, nargs, newfn);
    }

  /* Generate code of struct function **/
  env_push(f->args, newfn);

  start_block(toplevel ? "" : "function", newfn);
  generate_component(f->value, newfn);
  end_block(newfn);

  generate_typeset_check(f->typeset, 0, newfn);

  ins0(op_return, newfn);
  peephole(newfn);

  struct icode *c = generate_fncode(
    newfn, help, varname, filename, nicename, f->lineno, arg_types,
    f->typeset, compile_level);
  struct variable_list *closure = env_pop(&c->nb_locals);

  UNGCPRO();

  /* Generate code for creating closure */

  /* Count length of closure */
  int clen = 0;
  for (struct variable_list *cvar = closure; cvar; cvar = cvar->next)
    clen++;

  /* Generate closure */
  ins1(op_closure, clen, fn);

  /* Add variables to it */
  for (struct variable_list *cvar = closure; cvar; cvar = cvar->next)
    ins1(op_closure_var + cvar->vclass, cvar->offset, fn);

  delete_fncode(newfn);

  return c;
}

struct closure *compile_code(struct mfile *f, int seclev)
{
  struct str_and_len sl = { .len = 0, .str = NULL };

  const char *filename = (f->body->filename
                          ? f->body->filename
                          : "");
  const char *nicename = (f->body->nicename
                          ? f->body->nicename
                          : "");
  compile_level = seclev;
  erred = false;
  env_reset();
  struct fncode *top = new_fncode(true);
  env_push(NULL, top);		/* Environment must not be totally empty */
  struct block *body = new_toplevel_codeblock(
    fnmemory(top), f->statics, f->body);
  struct function *func = new_function(
    fnmemory(top), TYPESET_ANY, sl, NULL,
    new_component(fnmemory(top), 0, c_block, body),
    body->lineno, filename, nicename);
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
  ASSERT_NOALLOC_START();
  struct alloc_block *parser_block = new_block();
  struct mfile *f;
  bool ok = parse(parser_block, &f);
  if (!ok)
    goto done;
  /* parse error prints to mudout which may cause allocation */
  ASSERT_NOALLOC();

  enum module_status status;
  if (f->name && !reload
      && (status = module_status(f->name)) != module_unloaded)
    {
      ok = status == module_loaded;
      goto done;
    }

  struct mfile *prev_mfile = this_mfile;
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
      else
        ok = false;

      if (f->name)
        module_set(f->name, ok ? module_loaded : module_error, seclev);
    }
  this_mfile = prev_mfile;

 done:
  free_block(parser_block);
  return ok;
}

bool load_file(const char *fullname, const char *filename,
               const char *nicename, int seclev, bool reload)
{
  FILE *f = fopen(fullname, "r");
  if (f == NULL)
    runtime_error(error_bad_value);

  struct reader_state rstate;
  save_reader_state(&rstate);
  read_from_file(f, filename, nicename);
  value result;
  bool ok = interpret(&result, seclev, reload);
  fclose(f);
  restore_reader_state(&rstate);

  return ok;
}

void compile_init(void)
{
  static struct alloc_block *compile_block;
  compile_block = new_block();

  /* Note: These definitions actually depend on those in types.h and
     runtime.c */
  component_undefined = new_component(
    compile_block, 0, c_constant,
    new_int_constant(compile_block, 42));
  component_true = new_component(
    compile_block, 0, c_constant,
    new_int_constant(compile_block, true));
  component_false = new_component(
    compile_block, 0, c_constant,
    new_int_constant(compile_block, false));

  builtin_functions[b_bitnot]      = global_lookup("~");
  builtin_functions[b_bitxor]      = global_lookup("^");
  builtin_functions[b_cons]        = global_lookup("cons");
  builtin_functions[b_divide]      = global_lookup("/");
  builtin_functions[b_multiply]    = global_lookup("*");
  builtin_functions[b_negate]      = global_lookup("negate");
  builtin_functions[b_remainder]   = global_lookup("%");
  builtin_functions[b_shift_left]  = global_lookup("<<");
  builtin_functions[b_shift_right] = global_lookup(">>");

  g_get_static = global_lookup("get_static");
  g_symbol_get = global_lookup("symbol_get");
  g_symbol_set = global_lookup("symbol_set!");
  g_make_variable_ref = global_lookup("make_variable_ref");
  g_make_symbol_ref = global_lookup("make_symbol_ref");

  staticpro(&last_filename);
  last_filename = static_empty_string;
  last_c_filename = xstrdup("");
}
