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
#include "runtime/vector.h"


static ulong g_get_static, g_symbol_set, g_symbol_get;
static ulong g_make_variable_ref, g_make_symbol_ref;

static ulong builtin_functions[last_builtin];
static const uint8_t builtin_ops[last_builtin] = {
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
struct constant *constant_null;

static struct string *last_filename;
static seclev_t compile_level; /* Security level for generated code */

struct mfile *this_mfile;

static struct list *make_list(const struct cstlist *csts)
{
  if (csts == NULL)
    return NULL;

  /* the first entry has the list tail */
  struct list *l = csts->cst ? make_constant(csts->cst) : NULL;
  csts = csts->next;

  GCPRO(l);
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

static struct bigint *make_bigint(const struct bigint_const *bc)
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

static struct vector *make_array(const struct cstlist *csts)
{
  ulong size = 0;
  for (const struct cstlist *scan = csts; scan; scan = scan->next)
    ++size;

  struct vector *v = alloc_vector(size);
  GCPRO(v);
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

static void set_string_cache(struct table *t)
{
  assert(string_cache == NULL);
  string_cache = t;
}

void init_string_cache(void)
{
  set_string_cache(alloc_ctable(DEF_TABLE_SIZE));
}

void free_string_cache(void)
{
  assert(string_cache != NULL);
  string_cache = NULL;
}

struct string *scache_alloc_str_len(const char *str, size_t len)
{
  if (string_cache != NULL)
    {
      struct symbol *sym = table_lookup_len(string_cache, str, len);
      if (sym != NULL)
        return sym->name;
    }
  struct string *s = make_readonly(alloc_string_length(str, len));
  if (string_cache != NULL && !obj_readonlyp(&string_cache->o))
    {
      GCPRO(s);
      table_add(string_cache, s, makebool(true));
      UNGCPRO();
    }
  return s;
}

struct string *scache_alloc_str(const char *str)
{
  return scache_alloc_str_len(str, strlen(str));
}

static struct string *make_string(const struct str_and_len *str)
{
  return scache_alloc_str_len(str->str, str->len);
}

static struct symbol *make_symbol(const struct cstpair *p)
{
  struct string *s = make_string(&p->cst1->u.string);
  GCPRO(s);
  value cst = make_constant(p->cst2);
  UNGCPRO();
  struct symbol *sym = alloc_symbol(s, cst);
  sym->o.flags |= OBJ_READONLY | OBJ_IMMUTABLE;
  return sym;
}

static struct table *make_table(const struct cstlist *csts, bool ctable)
{
  ulong l = 0;
  for (const struct cstlist *c = csts; c; c = c->next)
    ++l;
  struct table *t = (ctable ? alloc_ctable : alloc_table)(table_good_size(l));

  GCPRO(t);
  for (; csts; csts = csts->next)
    {
      assert(csts->cst->vclass == cst_symbol);
      struct symbol *sym = make_symbol(csts->cst->u.constpair);
      if (table_mlookup(t, sym->name) != NULL)
        abort();
      table_add_sym_fast(t, sym);
    }
  UNGCPRO();
  immutable_table(t);

  return t;
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
  set_string_cache(cache);
  value r = make_constant(c);
  if (cache != NULL)
    free_string_cache();
  return r;
}

typedef void (*gencond_fn)(struct label *lab, void *data, struct fncode *fn);

static struct icode *generate_function(struct function *f, bool toplevel,
                                       struct fncode *fn);
static void generate_component(struct component *comp, bool leave_result,
                               struct fncode *fn);
static void generate_condition(struct component *condition,
			       struct label *slab, gencond_fn scode,
			       void *sdata, struct label *flab,
			       gencond_fn fcode, void *fdata,
			       struct fncode *fn);

struct andordata
{
  struct label *lab, *slab, *flab;
  gencond_fn scode, fcode;
  void *sdata, *fdata;
  struct component *arg2;
};

static void andorcode(struct label *lab, void *_data, struct fncode *fn)
{
  struct andordata *data = _data;
  set_label(lab, fn);
  generate_condition(data->arg2,
		     data->slab, data->scode, data->sdata,
		     data->flab, data->fcode, data->fdata,
		     fn);
}

/*
 * Conceptually generates:
 *
 *   if (condition) goto slab; else goto flab;
 *   scode(slab, sdata);
 *   fcode(flab, fdata);
 */
static void generate_condition(struct component *condition,
			       struct label *slab, gencond_fn scode,
			       void *sdata, struct label *flab,
			       gencond_fn fcode, void *fdata,
			       struct fncode *fn)
{
  switch (condition->vclass)
    {
    case c_builtin:
      switch (condition->u.builtin.fn)
	{
	case b_sc_and: case b_sc_or:
	  {
	    struct component *arg1 = condition->u.builtin.args->c;

            struct andordata data = {
              .arg2  = condition->u.builtin.args->next->c,
              .slab  = slab,
              .scode = scode,
              .sdata = sdata,
              .flab  = flab,
              .fcode = fcode,
              .fdata = fdata
            };

            struct label *lab = new_label(fn);
	    if (condition->u.builtin.fn == b_sc_and)
	      generate_condition(arg1,
				 lab, andorcode, &data,
				 flab, NULL, NULL,
				 fn);
	    else
	      generate_condition(arg1,
				 slab, NULL, NULL,
				 lab, andorcode, &data,
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
      generate_component(condition, true, fn);
      if (scode)
	{
	  branch(op_branch_z1, flab, fn);
	  scode(slab, sdata, fn);
	  if (fcode) fcode(flab, fdata, fn);
	}
      else
	{
	  branch(op_branch_nz1, slab, fn);
	  if (fcode) fcode(flab, fdata, fn);
	  else branch(op_branch1, flab, fn);
	}
      break;
    }
}

struct ifdata
{
  struct label *endlab;
  struct component *success, *failure;
  bool leave_result;
};

static void ifs_code(struct label *lab, void *_data, struct fncode *fn)
{
  struct ifdata *data = _data;

  set_label(lab, fn);
  generate_component(data->success, data->leave_result, fn);
  branch(op_branch1, data->endlab, fn);
  if (data->leave_result)
    adjust_depth(-1, fn);
}

static void iff_code(struct label *lab, void *_data, struct fncode *fn)
{
  struct ifdata *data = _data;

  set_label(lab, fn);
  generate_component(data->failure, data->leave_result, fn);
  branch(op_branch1, data->endlab, fn);
  if (data->leave_result)
    adjust_depth(-1, fn);
}

static void generate_if(struct component *condition, struct component *success,
			struct component *failure, bool leave_result,
                        struct fncode *fn)
{
  struct ifdata ifdata = {
    .endlab       = new_label(fn),
    .success      = success,
    .failure      = failure,
    .leave_result = leave_result
  };

  generate_condition(condition, new_label(fn), ifs_code, &ifdata,
		     new_label(fn), iff_code, &ifdata, fn);
  set_label(ifdata.endlab, fn);
  if (leave_result)
    adjust_depth(1, fn);
}

struct whiledata {
  struct label *looplab;
  struct component *code;
};

static void wmain_code(struct label *lab, void *_data, struct fncode *fn)
{
  struct whiledata *wdata = _data;
  set_label(lab, fn);
  generate_component(wdata->code, true, fn);
  /* loop1 will pop the result */
  branch(op_loop1, wdata->looplab, fn);
}

static void generate_while(struct component *condition,
                           struct component *iteration,
                           bool leave_result, struct fncode *fn)
{
  struct whiledata wdata = {
    .looplab = new_label(fn),
    .code    = iteration
  };

  env_start_loop();
  set_label(wdata.looplab, fn);
  start_block(NULL, fn);
  struct label *endlab = new_label(fn);
  generate_condition(condition,
                     new_label(fn), wmain_code, &wdata,
		     endlab, NULL, NULL, fn);
  set_label(endlab, fn);
  if (leave_result)
    generate_component(component_undefined, true, fn);
  end_block(fn);
  env_end_loop();
}

static void generate_args(struct clist *args, struct fncode *fn,
                          uint16_t *countp)
{
  uint16_t count = 0;
  while (args)
    {
      assert(count < MAX_FUNCTION_ARGS);
      ++count;
      generate_component(args->c, true, fn);
      args = args->next;
    }
  *countp = count;
}

static void generate_block(struct block *b, bool leave_result,
                           struct fncode *fn)
{
  struct clist *cc = b->sequence;

  if (!env_block_push(b->locals, b->statics))
    {
      log_error(&b->loc, "too many local variables in function");
      return;
    }

  if (b->statics)
    for (struct vlist *vl = b->locals; vl; vl = vl->next)
      {
        ulong offset;
        bool is_static;
        enum variable_class vclass = env_lookup(vl->var, &offset,
                                                false, true, &is_static);
        assert(is_static && vclass == vclass_local);
        ins_constant(scache_alloc_str(vl->var), fn);
        mexecute(&vl->loc, g_get_static, NULL, 1, fn);
        ins1(op_assign + vclass, offset, fn);
        ins0(op_discard, fn);
      }

  /* Generate code for sequence */
  for (; cc; cc = cc->next)
    generate_component(cc->c, leave_result && cc->next == NULL, fn);

  if (erred)
    return;

  for (struct vlist *vl = b->locals; vl; vl = vl->next)
    if (!vl->was_written)
      if (!vl->was_read)
	warning_loc(b->filename, b->nicename, &vl->loc,
                    "local variable %s is unused", vl->var);
      else
	warning_loc(b->filename, b->nicename, &vl->loc,
                    "local variable %s is never written", vl->var);
    else if (!vl->was_read)
      warning_loc(b->filename, b->nicename, &vl->loc,
                  "local variable %s is never read", vl->var);
  env_block_pop();
}

static void generate_execute(const struct loc *loc,
                             struct component *acall, int count,
                             struct fncode *fn)
{
  set_lineno(loc->line, fn);

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
	  mexecute(loc, offset, acall->u.recall, count, fn);
	  return;
	}
    }
  generate_component(acall, true, fn);
  if (count <= ARG1_MAX)
    ins1(op_execute, count, fn);
  else
    ins2(op_execute2, count, fn);
}

static void generate_builtin(struct component *comp,
                             enum builtin_op op, struct clist *args,
                             bool leave_result, struct fncode *fn)
{
  switch (op)
    {
    case b_if: {
      struct block *cb = new_codeblock(
        fnmemory(fn), NULL,
        new_clist(fnmemory(fn), args->next->c,
                  new_clist(fnmemory(fn), component_undefined, NULL)),
        NULL, NULL, NO_LOC);
      generate_if(
        args->c,
        new_block_component(fnmemory(fn), &args->next->c->loc, cb),
        component_undefined, leave_result, fn);
      return;
    }
    case b_ifelse:
      generate_if(args->c, args->next->c, args->next->next->c,
                  leave_result, fn);
      return;
    case b_sc_and: case b_sc_or:
      generate_if(comp, component_true, component_false,
                  leave_result, fn);
      return;
    case b_while:
      generate_while(args->c, args->next->c, leave_result, fn);
      return;

    case b_loop:
      {
        struct label *loop = new_label(fn);

        env_start_loop();
        set_label(loop, fn);
        start_block(NULL, fn);
        generate_component(args->c, true, fn);
        /* loop1 will pop the result */
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
        uint16_t count;

        assert(comp->u.builtin.fn < last_builtin);
        generate_args(args, fn, &count);
        set_lineno(comp->loc.line, fn);
        ins0(builtin_ops[comp->u.builtin.fn], fn);
        break;
      }
    default:
      {
        uint16_t count;

        assert(comp->u.builtin.fn < last_builtin);
        generate_args(args, fn, &count);
        mexecute(&comp->loc, builtin_functions[comp->u.builtin.fn],
                 NULL, count, fn);
        break;
      }
    }

  if (!leave_result)
    ins0(op_discard, fn);
}

static void generate_component(struct component *comp, bool leave_result,
                               struct fncode *fn)
{
  if (erred)
    return;

  set_lineno(comp->loc.line, fn);

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
            generate_component(comp->u.assign.value, true, fn);
            mexecute(&comp->loc, g_symbol_set, NULL, 2, fn);
            break;
          }

	generate_component(comp->u.assign.value, true, fn);

	set_lineno(comp->loc.line, fn);

        if (vclass == vclass_global)
	  massign(&comp->loc, offset, comp->u.assign.symbol, fn);
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

        if (vclass != vclass_global && !leave_result)
          return;

        if (is_static)
          {
            assert(vclass != vclass_global);
            ins1(op_recall + vclass, offset, fn);
            ulong gidx = is_vref ? g_make_symbol_ref : g_symbol_get;
            mexecute(&comp->loc, gidx, NULL, 1, fn);
            break;
          }
	if (vclass != vclass_global)
          ins1((is_vref ? op_vref : op_recall) + vclass, offset, fn);
        else if (is_vref)
          {
            if (!mwritable(&comp->loc, offset, comp->u.recall))
              return;
            ins_constant(makeint(offset), fn);
          }
        else
          mrecall(&comp->loc, offset, comp->u.recall, fn);
        if (is_vref)
          mexecute(&comp->loc, g_make_variable_ref,
                   "make_variable_ref", 1, fn);
	break;
      }
    case c_constant:
      if (!leave_result)
        return;
      ins_constant(make_constant(comp->u.cst), fn);
      break;
    case c_closure:
      {
	uint16_t idx = add_constant(
          generate_function(comp->u.closure, false, fn), fn);
	if (idx <= ARG1_MAX)
          ins1(op_closure_code1, idx, fn);
	else
          ins2(op_closure_code2, idx, fn);
	break;
      }
    case c_block:
      generate_block(comp->u.blk, leave_result, fn);
      return;
    case c_labeled:
      start_block(comp->u.labeled.name, fn);
      generate_component(comp->u.labeled.expression, true, fn);
      if (erred)
        return;
      end_block(fn);
      break;
    case c_exit:
      generate_component(comp->u.labeled.expression, true, fn);
      if (erred)
        return;
      if (!exit_block(comp->u.labeled.name, fn))
        {
          if (!comp->u.labeled.name)
            log_error(&comp->loc, "no loop to exit from");
          else
            log_error(&comp->loc, "no block labeled %s", comp->u.labeled.name);
        }
      break;
    case c_execute:
      {
	uint16_t count;
	generate_args(comp->u.execute->next, fn, &count);
	generate_execute(&comp->loc, comp->u.execute->c, count, fn);
	break;
      }
    case c_builtin:
      generate_builtin(comp, comp->u.builtin.fn, comp->u.builtin.args,
                       leave_result, fn);
      return;
    default: abort();
    }

  if (!leave_result)
    ins0(op_discard, fn);
}

static struct obj *make_arguments(struct function *f)
{
  if (f->varargs)
    return &alloc_string(f->args->var)->o;

  int i = 0;
  for (struct vlist *a = f->args; a; a = a->next)
    ++i;

  if (i == 0)
    return &empty_vector->o;

  struct vector *result = alloc_vector(i);
  GCPRO(result);
  for (struct vlist *a = f->args; --i, a; a = a->next)
    {
      struct list *e = make_readonly(alloc_list(
        a->var[0] == '$' ? makebool(false) : alloc_string(a->var),
        makeint(a->typeset)));
      if (!check_immutable(make_readonly(e)))
        abort();
      result->data[i] = e;
    }
  UNGCPRO();

  if (!check_immutable(make_readonly(result)))
    abort();
  return &result->o;
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
  struct obj *arguments = NULL;
  GCPRO(help, varname, filename, nicename, arguments);

  /* Make variable name (if present) */
  if (f->varname)
    varname = scache_alloc_str(f->varname);
  else
    varname = NULL;

  /* Make filename string */
  filename = scache_alloc_str(f->filename);
  nicename = scache_alloc_str(f->nicename);

  arguments = make_arguments(f);

  struct fncode *newfn = new_fncode(toplevel);

  set_lineno(f->loc.line, newfn);

  int nargs = 0;
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
  generate_component(f->value, true, newfn);
  if (erred)
    {
      UNGCPRO();
      return NULL;
    }

  end_block(newfn);

  generate_typeset_check(f->typeset, 0, newfn);

  ins0(op_return, newfn);
  /* we must have popped arguments and left one return value on the stack;
     n.b,. for varargs this assumes 0 arguments (worst-case stack usage) */
  assert(adjust_depth(0, newfn) == -nargs + 1);
  peephole(newfn);

  struct icode *c = generate_fncode(
    newfn, help, varname, filename, nicename, &f->loc, arguments,
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

struct closure *compile_code(struct mfile *f, seclev_t seclev)
{
  init_string_cache();

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
  struct function *func = new_fn(
    fnmemory(top), new_block_component(fnmemory(top), NO_LOC, body),
    &body->loc, filename, nicename);
  func->varname = "top-level";
  struct icode *cc = generate_function(func, true, top);

  GCPRO(cc);
  generate_fncode(top, NULL, NULL, NULL, NULL, &f->loc, &empty_vector->o,
                  TYPESET_ANY, seclev);
  uint16_t dummy;
  env_pop(&dummy);
  delete_fncode(top);
  UNGCPRO();

  free_string_cache();

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

bool interpret(value *result, seclev_t seclev, int reload)
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
               const char *nicename, seclev_t seclev, bool reload)
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
  component_undefined = new_const_component(
    compile_block, NO_LOC, new_int_constant(compile_block, 42));
  component_true = new_const_component(
    compile_block, NO_LOC, new_int_constant(compile_block, true));
  component_false = new_const_component(
    compile_block, NO_LOC, new_int_constant(compile_block, false));

  constant_null = allocate(compile_block, sizeof *constant_null);
  *constant_null = (struct constant){
    .vclass = cst_list,
    .u.constants = NULL
  };

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

  staticpro(&string_cache);
}
