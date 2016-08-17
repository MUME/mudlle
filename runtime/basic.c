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

#include <ctype.h>
#include <fcntl.h>
#ifndef WIN32
#  include <netinet/in.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/time.h>

#include "../alloc.h"
#include "../call.h"
#include "../calloc.h"
#include "../compile.h"
#include "../context.h"
#include "../global.h"
#include "../interpret.h"
#include "../lexer.h"
#include "../mcompile.h"
#include "../mparser.h"
#include "../table.h"
#include "../tree.h"

#include "basic.h"
#include "list.h"
#include "runtime.h"
#include "mudlle-string.h"
#include "symbol.h"
#include "vector.h"


TYPEDOP(functionp, "function?", "`x -> `b. True if `x is a function;"
        " cf. callable?().",
        1, (value v),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(is_function(v));
}

TYPEDOP(callablep, "callable?", "`f `n -> `b. True if function `f"
        " can be called with `n arguments; cf. function?()",
        2, (value f, value margs),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "fn.n")
{
  if (!is_function(f))
    runtime_error(error_bad_function);
  int nargs = GETRANGE(margs, 0, INT_MAX);
  return makebool(callablep(f, nargs));
}

TYPEDOP(may_callp, "may_call?", "`f -> `b. True if the caller is allowed to"
	" call `f without a security violation due to minlevel.", 1, (value f),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "f.n")
{
  if (!is_function(f))
    runtime_error(error_bad_function);
  return makebool(!minlevel_violator(f));
}

static struct vector *vector_copy(struct vector *v)
{
  struct vector *result;
  long l = vector_len(v);

  GCPRO1(v);
  result = alloc_vector(l);
  memcpy(result->data, v->data, l * sizeof *v->data);
  UNGCPRO();

  return result;
}

/* called from x86builtins.S */
value apply_vararg(value (*prim)(), struct vector *args)
{
  args = vector_copy(args);
  return prim(args, vector_len(args));
}

static enum runtime_error apply(value f, struct vector *args, value *r)
{
  if (!TYPE(args, type_vector))
    return error_bad_type;
  if (!is_function(f))
    return error_bad_function;

  int nargs = vector_len(args);
  if (!callablep(f, nargs))
    return error_wrong_parameters;

  struct obj *fobj = f;
  if (fobj->type == type_closure)
    {
      *r = call(f, args);
      return error_none;
    }

  if (fobj->type == type_varargs)
    {
      struct {
        struct call_stack_c_header c;
        value args[1];
      } me = {
        .c = {
          .s = {
            .next = call_stack,
            .type = call_c
          },
          .u.prim = f,
          .nargs = 1
        },
        .args = { args }
      };
      call_stack = &me.c.s;
      struct primitive *prim = f;
      prim->call_count++;
      *r = apply_vararg(prim->op->op, args);
      call_stack = me.c.s.next;
    }
  else
    {
      struct {
        struct call_stack_c_header c;
        value args[MAX_PRIMITIVE_ARGS];
      } me;
      me.c = (struct call_stack_c_header){
        .s = {
          .next = call_stack,
          .type = call_c
        },
        .u.prim = f,
        .nargs = nargs
      };
      call_stack = &me.c.s;

      memcpy(me.args, args->data, sizeof args->data[0] * nargs);

      if (fobj->type == type_secure)
        callable(f, nargs);     /* checks seclevel */

      *r = call(f, args);
      call_stack = me.c.s.next;
    }

  return error_none;
}

TYPEDOP(apply, 0,
        "`f `v -> `x. Excutes `f with arguments `v, returns its result",
	2, (value f, struct vector *args),
	OP_APPLY, "fv.x")
{
  value result;
  enum runtime_error error = apply(f, args, &result);
  if (error != error_none)
    primitive_runtime_error(error, &op_apply, 2, f, args);
  return result;
}

static value do_eval(value input, struct table *cache, bool force_constant)
{
  ASSERT_NOALLOC_START();

  const char **lines;
  if (TYPE(input, type_string))
    {
      lines = malloc(3 * sizeof *lines);
      lines[0] = force_constant ? "'" : "";
      lines[1] = ((struct string *)input)->str;
      lines[2] = NULL;
    }
  else
    {
      TYPEIS(input, type_vector);
      struct vector *iv = input;
      lines = malloc((vector_len(iv) + 2) * sizeof *lines);
      lines[0] = force_constant ? "'" : "";
      for (int i = 0; i < vector_len(iv); ++i)
        {
          TYPEIS(iv->data[i], type_string);
          lines[i + 1] = ((struct string *)iv->data[i])->str;
        }
      lines[vector_len(iv) + 1] = NULL;
    }

  struct reader_state rstate;
  save_reader_state(&rstate);
  read_from_strings(lines, "<eval>", "<eval>", force_constant);
  struct alloc_block *parser_block = new_block();
  struct mfile *f = NULL;
  struct constant *c = NULL;

  ASSERT_NOALLOC();
  bool ok = (force_constant
             ? parse_constant(parser_block, &c)
             : parse(parser_block, &f));

  /* parse error prints to mudout which may cause allocation */
  if (ok) ASSERT_NOALLOC();

  free(lines);

  if (force_constant)
    {
      value result = makebool(false);
      if (ok)
        result = alloc_list(make_shared_string_constant(c, cache), NULL);
      restore_reader_state(&rstate);
      free_block(parser_block);
      return result;
    }

  enum runtime_error error;
  if (!ok)
    {
      error = error_compile;
      goto errout;
    }

  if (f->vclass != f_plain)
    {
      error = error_bad_value;
      goto errout;
    }

  if (mstart(parser_block, f, get_seclevel()))
    {
      struct closure *closure = compile_code(f, get_seclevel());
      mstop(f);
      if (closure != NULL)
        {
          restore_reader_state(&rstate);
          free_block(parser_block);
          return call0(closure);
        }
    }

  error = error_compile;
 errout:
  restore_reader_state(&rstate);
  free_block(parser_block);
  runtime_error(error);
}

#  define EVAL_NAME "ieval"
SECTOP(eval, EVAL_NAME,
       "`s|`v -> `x. Excute the expression in `s (or `v, a vector of strings)"
       " and return its result.\n"
       "On compile error, the message is printed using `display() and"
       " `error_compile is thrown.",
       1, (value input), 1, 0, "[sv].x")
{
  return do_eval(input, NULL, false);
}

SECTOP(read_constant, 0,
       "`s|`v `t -> `p|false. Evaluate `s (or `v, a vector of strings) as a"
       " constant and return its value `x as `list(`x).\n"
       "`t is a table (or ctable) of shared strings. If a string for which `t"
       " has has a symbol, a new string is not created. If `t is writable,"
       " any newly created strings are added to `t.\n"
       "`t can be null to disable string sharing.\n"
       "The input will be interpreted as if it started with an apostrophe (')"
       " and cannot contain any comma-prefixed expressions.\n"
       "On syntax error, a message is printed using `display() and"
       " false is returned.",
       2, (value input, struct table *cache), 1, 0, "[sv][tu].[zk]")
{
  if (cache)
    TYPEIS(cache, type_table);
  return do_eval(input, cache, true);
}

TYPEDOP(call_trace, 0, "`b -> `v. Returns current call trace. Each entry is"
        " either a function, a machine code object (`type_mcode),"
        " or a string.\n"
        "If `b is true, each entry becomes cons(`called, `line), where"
        " `line is null if unknown.",
        1, (value lines),
        OP_NOESCAPE, "x.v")
{
  return get_mudlle_call_trace(istrue(lines));
}

TYPEDOP(maxseclevel, 0,
        "-> `n. Returns the current maxseclevel, which is the highest"
        " secure primitive seclevel that currently may be called."
        " Cf. `effective_seclevel().",
        0, (void), OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".n")
{
  return maxseclevel;
}

SECTOP(effective_seclevel, 0,
       "-> `n. Returns the effective seclevel used for security checks"
       " when calling secure primitives. This is the lowest value"
       " of `maxseclevel() and `seclevel().",
       0, (void), 1, OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".n")
{
  return makeint(get_effective_seclevel());
}

TYPEDOP(error, 0, "`n -> . Causes error `n", 1, (value errn),
        OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "n.")
{
  enum runtime_error n = GETRANGE(errn, 0, last_runtime_error - 1);
  runtime_error(n);
}

TYPEDOP(warning, 0, "`s -> . Prints the warning message `s and a stack trace.",
        1, (struct string *s),
        0, "s.")
{
  TYPEIS(s, type_string);
  char *msg;
  ALLOCA_STRING(msg, s, 1024);
  if (msg == NULL)
    runtime_error(error_bad_value);
  runtime_warning(msg);
  undefined();
}

const struct primitive_ext *const warning_prim_ext = &op_warning;

TYPEDOP(compiledp, "compiled?",
        "-> `b. Returns true if called from compiled code, ignoring"
        " levels of primitives.",
        0, (void), OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".n")
{
  struct call_stack *cstack = call_stack;
  while (cstack && cstack->type == call_c)
    cstack = cstack->next;
  return makebool(cstack && cstack->type == call_compiled);
}

TYPEDOP(max_loop_count, 0,
        "-> `n. Returns the maximum loop count before an `error_loop is"
        " generated. See `loop_count().",
        0, (void),
        OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".n")
{
  return makeint(MAX_LOOP_COUNT);
}

TYPEDOP(loop_count, 0,
        "-> `n. Returns the current loop counter. This counter is"
        " decremented at every function call and every iteration of a loop"
        " statement. When `n reaches 0, an `error_loop is generated.\n"
        "A good way to test for whether we are approaching an error is"
        " 0 < `n && `n < 1000, or (`max_loop_count() >> 8) instead"
        " of 1000.",
        0, (void),
        OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".n")
{
  /* in theory xcount may be outside the range of mudlle integers, but
     it seems very unlikely this would happen for real */
  return makeint(xcount);
}

struct call0_data {
  value func;
  value result;
};

static void docall0(void *_data)
{
  struct call0_data *data = _data;
  data->result = call0(data->func);
}

/* calls f() and returns value in "result"

   if f() causes an error:
     for loop and recurse errors, re-raise the error

     if handler is non-null, call handler(errno) and returns its value

     if handler is null, return errno
 */
static value trap_error(value f, value handler,
                        enum call_trace_mode call_trace_mode,
                        bool handle_loops)
{
  callable(f, 0);
  if (handler)
    callable(handler, 1);

  ulong saved_recursion_count = 0, saved_xcount = 0;
  if (handle_loops)
    {
      saved_xcount          = MIN(xcount, xcount / 10 + 100);
      saved_recursion_count = MIN(session_context->recursion_count,
                                  session_context->recursion_count / 10 + 100);
      xcount                           -= saved_xcount;
      session_context->recursion_count -= saved_recursion_count;
    }

  {
    struct call0_data d = { .func = f };
    GCPRO1(handler);
    bool ok = mcatch(docall0, &d, call_trace_mode);
    UNGCPRO();

    /* careful in case f() called unlimited_execution() */
    xcount = MIN(MAX_TAGGED_INT, xcount + saved_xcount);
    if (session_context->recursion_count)
      session_context->recursion_count += saved_recursion_count;

    if (ok)
      return d.result;
  }

  if (mexception.sig == SIGNAL_ERROR)
    {
      enum runtime_error err = mexception.err;
      if (err == error_loop
          && (!handle_loops
              || xcount == 0))
        goto propagate;
      if (err == error_recurse
          && (!handle_loops
              || session_context->recursion_count == 0))
        goto propagate;

      if (handler == NULL)
        return makeint(err);

      return call1(handler, makeint(err));
    }

 propagate:
  mrethrow();
}

TYPEDOP(catch_error, 0, "`f `b -> `x. Executes `f() and returns its result."
        " If an error occurs, returns the error number.\n"
        "If `b is true, call traces are not sent to the `with_output()"
        " target, but only to those added with `add_call_trace_oport!().\n"
        "Does not catch loop/recursion errors. Cf. `trap_error().",
        2, (value f, value suppress),
        0, "fx.x")
{
  return trap_error(f, NULL,
                    istrue(suppress) ? call_trace_no_err : call_trace_on,
                    false);
}

TYPEDOP(handle_error, 0, "`f0 `f1 -> `x. Executes `f0(). If an error occurs,"
        " calls `f1(`errno). Returns result of `f0() or `f1(`errno)."
        " Does not handle loop/recursion errors. Cf. `trap_error().",
        2, (value f, value handler),
        0, "ff.x")
{
  if (handler == NULL)
    runtime_error(error_bad_function);
  return trap_error(f, handler, call_trace_no_err, false);
}

TYPEDOP(trap_error, 0,
        "`f0 `f1 `n `b -> `x. Executes `f0() and returns its return"
        " value.\n"
        "If an error occurs in `f0(), calls `f1(`errno), and returns its"
        " return value instead. `f1 can be `null, in which case an error"
        " in `f0() makes `trap_error() return the error number.\n"
        "If `b is true, reserve some space to handle loop and recursion"
        " errors as well; otherwise they are not handled.\n"
        "By default, call traces are sent to the `with_output() target"
        " as well as observers added using `add_call_trace_oport!().\n"
        "`n controls call traces from `f0 are sent:\n"
        "  `call_trace_off      \tdo not send call traces to anyone\n"
        "  `call_trace_barrier  \tonly print call traces down to the"
        " current stack level\n"
        "  `call_trace_on       \tprint complete call traces",
        4, (value f, value handler, value ct_mode, value handle_loops), 0,
        "f[fu]nx.x")
{
  long call_trace_mode = GETINT(ct_mode);
  if (call_trace_mode != call_trace_off
      && call_trace_mode != call_trace_barrier
      && call_trace_mode != call_trace_on)
    runtime_error(error_bad_value);

  return trap_error(f, handler, call_trace_mode, istrue(handle_loops));
}

SECTOP(with_minlevel, 0,
       "`n `c -> `x. Call `c() with the minimum function security level"
       " set to `n and return its result, where 0 <= `n <= the calling"
       " closure's `function_seclevel().\n"
       "No closure whose `function_seclevel() is less than `n can be called"
       " from `c(), or an `error_security_violation will be caused.",
       2, (value n, value f), 1, OP_APPLY, "nf.x")
{
  long new_minlevel = GETRANGE(n, 0, LVL_IMPLEMENTOR);
  if (new_minlevel > get_seclevel())
    runtime_error(error_security_violation);

  int old_minlevel = minlevel;
  minlevel = new_minlevel;
  value r = call0(f);
  minlevel = old_minlevel;

  return r;
}

SECTOP(with_maxseclevel, 0,
       "`n `c -> `x. Temporarily change `maxseclevel() to `n"
       " (or `LVL_IMPLEMENTOR if `n >= `LVL_VALA), then calls `c"
       " and returns its result.\nUse this function in your code to make it"
       " available to Ms- even if you need to call secure primitives. The"
       " typical invocation is:\n"
       "  \t`with_maxseclevel(\t`seclevel(), fn () secure_call(...))",
       2, (value n, value f), 1, 0, "nf.x")
{
  callable(f, 0);
  long new_maxseclev = GETINT(n);
  if (new_maxseclev < MIN_SECLEVEL
      || new_maxseclev > MAX_SECLEVEL)
    runtime_error(error_bad_value);
  if (new_maxseclev > get_seclevel())
    runtime_error(error_security_violation);

  /* The compiled mudlle code paths assume that maxseclevel is set to
   * MAX_SECLEVEL (ie. noop) if it shouldn't be checked. */
  if (new_maxseclev >= LEGACY_SECLEVEL)
    n = makeint(MAX_SECLEVEL);

  value old_maxseclev = maxseclevel;
  maxseclevel = n;
  value r = call0(f);
  maxseclevel = old_maxseclev;

  return r;
}

TYPEDOP(setjmp, 0,
        "`f -> `x. Executes `f(`buf). `buf can be used with `longjmp(). The"
        " return value is either the result of `f(`buf), or the value `x1"
        " passed to `longjmp(`buf, `x1)",
        1, (value f), 0, "f.x")
{
  callable(f, 1);
  return msetjmp(f);
}

TYPEDOP(longjmp, 0,
        "`buf `x -> . Returns `x from the `setjmp() that created `buf",
        2, (value buf, value x), 0, "ox.")
{
  if (!is_mjmpbuf(buf))
    runtime_error(error_bad_value);

  mlongjmp(buf, x);
}

UNSAFETOP(session, 0, "`f -> . Calls `f() in it's own session and return its"
          " result.\n"
          "A session has its own recursion and loop limits; cf. `loop_count.\n"
          "If `f() causes an error, return null.",
          1, (struct closure *fn), OP_APPLY, "f.x")
{
  callable(fn, 0);

  struct session_context newc;
  session_start(&newc,
                &(const struct session_info){
                  .minlevel    = minlevel,
                  .maxseclevel = intval(maxseclevel),
                  .muser       = muduser,
                  .mout        = mudout,
                  .merr        = muderr });
  value aresult = mcatch_call0(NULL, fn);
  session_end();

  return aresult;
}

static const typing tref = { "vn.x", "sn.n", "[ton]s.x", NULL };

FULLOP(ref, 0, "`x1 `x2 -> `x3. Generic interface to lookup operations:"
       " `x1[`x2] -> `x3. See `set!() for allowed types.",
       2, (value x1, value x2),
       0, OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_OPERATOR, tref, /* extern */)
{
  if (!pointerp(x1)) goto bad_type;
  switch (((struct obj *)x1)->type)
    {
    case type_vector:
      return code_vector_ref(x1, x2);
    case type_string:
      return code_string_ref(x1, x2);
    case type_table:
      return code_table_ref(x1, x2);
    default:
      goto bad_type;
    }
 bad_type:
  ref_runtime_error(error_bad_type, x1, x2);
}

const struct primitive_ext *const ref_prim_ext = &op_ref;

void ref_runtime_error(enum runtime_error error, value x1, value x2)
{
  primitive_runtime_error(error_bad_type, &op_ref, 2, x1, x2);
}

static const typing tset = { "vnx.3", "snn.n", "[ton]sx.3", NULL };

FULLOP(setb, "set!",
       "`x1 `x2 `x3 -> `x3. Generic set operation: `x1[`x2] = `x3.\n"
       "`x1 can be a vector or a string (`x2 is the integer index,"
       " where negative values are counted from the end)"
       " or a table"
       " (`x2 is the string symbol name).\n"
       "For string `x1, `x3 must be an integer and the returned"
       " value is the stored value (`x3 & 255).",
       3, (value x1, value x2, value x3),
       0, OP_LEAF | OP_NOESCAPE | OP_OPERATOR, tset, /* extern */)
{
  if (!pointerp(x1)) goto bad_type;
  switch (((struct obj *)x1)->type)
    {
    case type_vector:
      return code_vector_set(x1, x2, x3);
    case type_string:
      return code_string_set(x1, x2, x3);
    case type_table:
      return code_table_set(x1, x2, x3);
    default:
      goto bad_type;
    }
 bad_type:
  set_runtime_error(error_bad_type, x1, x2, x3);
}

const struct primitive_ext *const setb_prim_ext = &op_setb;

void set_runtime_error(enum runtime_error error, value x1, value x2, value x3)
{
  primitive_runtime_error(error_bad_type, &op_setb, 3, x1, x2, x3);
}

/*
 * References are grecords with 1 or 2 data entries. The following
 * kinds exist:
 *
 * 1 entry:
 *   integer n  -> global number n
 *   variable v -> v->vvalue
 *   symbol s   -> s->data
 * 2 entries:
 *   pair p, 0  -> p->car
 *   pair p, 1  -> p->cdr
 *   x, y       -> x[y]
 *   x, vector  -> custom reference; see make_custom_ref()
 */
TYPEDOP(dereference, 0, "`r -> `x. Return the value of the reference `r.",
        1, (struct grecord *r), OP_OPERATOR, "r.x")
{
  TYPEIS(r, type_reference);
  long items = grecord_len(r);
  value v = r->data[0];
  switch (items)
    {
    case 1:
      if (integerp(v))
        {
          long idx = intval(v);
          if (idx >= 0 && idx < intval(environment->used))
            {
              check_global_read(idx);
              return GVAR(idx);
            }
        }
      else if (pointerp(v))
        switch (r->data[0]->type)
          {
          case type_variable:
            return ((struct variable *)v)->vvalue;
          case type_symbol:
            return ((struct symbol *)v)->data;
          default:
            break;
          }
      break;
    case 2:
      if (TYPE(v, type_pair) && integerp(r->data[1]))
        {
          struct list *p = v;
          return intval(r->data[1]) ? p->cdr : p->car;
        }
      if (TYPE(r->data[1], type_vector))
        {
          struct vector *fns = (struct vector *)r->data[1];
          return call1(fns->data[1], r->data[0]);
        }
      return code_ref(v, r->data[1]);
    }
  abort();
}

const struct primitive_ext *const dereference_prim_ext = &op_dereference;

TYPEDOP(set_refb, "set_ref!", "`r `x -> `x. Set the value of the reference"
        " `r to `x.",
        2, (struct grecord *r, value val), OP_OPERATOR, "rx.2")
{
  TYPEIS(r, type_reference);
  long items = grecord_len(r);
  value v = r->data[0];
  switch (items)
    {
    case 1:
      if (integerp(v))
        {
          long idx = intval(v);
          assert(idx >= 0 && idx < intval(environment->used));
          check_global_write(idx, val);
          return GVAR(idx) = val;
        }
      else if (pointerp(v))
        switch (r->data[0]->type)
          {
          case type_variable: ;
            struct variable *var = (struct variable *)r->data[0];
            assert(TYPE(var, type_variable));
            if (obj_readonlyp(&var->o))
              runtime_error(error_value_read_only);
            return var->vvalue = val;
          case type_symbol:
            return code_symbol_set((struct symbol *)r->data[0], val);
          default:
            break;
          }
    case 2:
      if (TYPE(v, type_pair) && integerp(r->data[1]))
        {
          assert(integerp(r->data[1]));
          return (intval(r->data[1]) ? code_set_cdrb : code_set_carb)(
            (struct list *)r->data[0], val);
        }
      if (TYPE(r->data[1], type_vector))
        {
          struct vector *fns = (struct vector *)r->data[1];
          return call2(fns->data[2], r->data[0], val);
        }
      return code_setb(r->data[0], r->data[1], val);
    }
  abort();
}

const struct primitive_ext *const set_refb_prim_ext = &op_set_refb;

TYPEDOP(make_pair_ref, 0, "`p `n -> `r. Return a reference to the"
        " `car (`n == 0) or `cdr (`n == 1) of `p.",
        2, (struct list *p, value n), OP_LEAF | OP_NOESCAPE, "kn.r")
{
  TYPEIS(p, type_pair);
  (void)GETRANGE(n, 0, 1);
  GCPRO1(p);
  struct grecord *r = allocate_record(type_reference, 2);
  UNGCPRO();
  r->data[0] = &p->o;
  r->data[1] = n;
  return make_readonly(r);
}

TYPEDOP(make_symbol_ref, 0, "`s -> `r. Return a reference to the"
        " symbol value of `s.",
        1, (struct symbol *s), OP_LEAF | OP_NOESCAPE, "y.r")
{
  TYPEIS(s, type_symbol);
  GCPRO1(s);
  struct grecord *r = allocate_record(type_reference, 1);
  UNGCPRO();
  r->data[0] = &s->o;
  return make_readonly(r);
}

TYPEDOP(make_variable_ref, 0, "`n|`v -> `r. Return a reference to global"
        " number `n or variable `v. Cf. `global_lookup().",
        1, (value midx), OP_LEAF | OP_NOESCAPE, "[no].r")
{
  if (integerp(midx))
    {
      long idx = intval(midx);
      if (idx < 0 || idx >= intval(environment->used))
        runtime_error(error_bad_value);
    }
  else
    TYPEIS(midx, type_variable);
  struct grecord *r = allocate_record(type_reference, 1);
  r->data[0] = midx;
  return make_readonly(r);
}

TYPEDOP(make_custom_ref, 0, "`x0 `v -> `r. Returns a custom reference."
        " `v must be a sequence(`desc, `deref, `setref) that"
        " defines the reference:\n"
        "  `desc   \tA readonly string that describes the reference.\n"
        "  `deref  \t`x0 -> `x1. Dereference the reference and return its"
        " value.\n"
        "  `setref \t`x0 `x1 -> `x2. Set the reference's value to `x1 and"
        " return a sensible value, typically `x1 unmodified.",
        2, (value val, struct vector *fns), OP_LEAF | OP_NOESCAPE, "xv.r")
{
  TYPEIS(fns, type_vector);
  if (vector_len(fns) != 3 || !readonlyp(fns))
    runtime_error(error_bad_value);
  TYPEIS(fns->data[0], type_string);
  if (!readonlyp(fns->data[0]))
    runtime_error(error_bad_value);
  callable(fns->data[1], 1);
  callable(fns->data[2], 2);

  GCPRO2(val, fns);
  struct grecord *r = allocate_record(type_reference, 2);
  UNGCPRO();
  r->data[0] = val;
  r->data[1] = &fns->o;
  return make_readonly(r);
}

static const typing tmake_ref = { "vn.r", "sn.r", "[ton]s.r", NULL };

FULLOP(make_ref, 0, "`x0 `x1 -> `r. Return a reference to `x0[`x1].",
       2, (value x1, value x2),
       0, OP_LEAF | OP_NOESCAPE, tmake_ref, static)
{
  switch (TYPEOF(x1))
    {
    case type_vector:
      {
        struct vector *v = x1;
        long idx = intval(x2);
        if (idx < 0)
          idx += vector_len(v);
        if (idx < 0 || idx >= vector_len(v))
          runtime_error(error_bad_index);
        break;
      }
    case type_string:
      {
        struct string *s = x1;
        long idx = intval(x2);
        if (idx < 0)
          idx += string_len(s);
        if (idx < 0 || idx >= string_len(s))
          runtime_error(error_bad_index);
        break;
      }
    case type_table:
      TYPEIS(x2, type_string);
      if (!readonlyp(x2))
        {
         GCPRO1(x1);
          x2 = make_readonly(mudlle_string_copy(x2));
          UNGCPRO();
        }
      break;
    default:
      runtime_error(error_bad_type);
    }
  GCPRO2(x1, x2);
  struct grecord *r = allocate_record(type_reference, 2);
  UNGCPRO();
  r->data[0] = x1;
  r->data[1] = x2;
  return make_readonly(r);
}

/* "Object" manipulation:
   load, save, size
   protect, test status, etc
*/

static mode_t get_umask(void)
{
  mode_t um = umask(0);
  umask(um);
  return um;
}


/* MDATA_MAGIC + MDATA_VER_xxx is the actual magic number */
#define MDATA_MAGIC         0x871f54ab  /* just a random number */

static value do_save_data(struct string *file, value x,
                          int (*rename_fn)(const char *oldpath,
                                           const char *newpath))
{
  TYPEIS(file, type_string);
  char *fname;
  ALLOCA_PATH(fname, file);
  if (*fname == 0)
    runtime_error(error_bad_value);


  ulong size;
  void *data = gc_save(x, &size);

  static const char tpattern[] = "%s.XXXXXX";
  size_t tmplen = strlen(fname) + strlen(tpattern) - 2 /* %s */;
  char tmpname[tmplen + 1];
  if (sprintf(tmpname, tpattern, fname) != tmplen)
    abort();

  int fd = mkstemp(tmpname);
  if (fd < 0)
    goto failed;

  uint32_t magic   = htonl(MDATA_MAGIC + MDATA_VER_CURRENT);
  uint32_t nsize   = htonl(size);
  bool ok = (write(fd, &magic, sizeof magic) == sizeof magic &&
             write(fd, &nsize, sizeof nsize) == sizeof nsize &&
             write(fd, data, size) == size);

  /* set mode to a+rw modified by umask */
  mode_t um = get_umask();
  int mode = S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH;
  fchmod(fd, mode & ~um);

  close(fd);

  ok = ok && rename_fn(tmpname, fname) == 0;

  if (!ok)
    {
      unlink(tmpname);
      goto failed;
    }


  undefined();

 failed:
  runtime_error(error_bad_value);
}

UNSAFETOP(save_data, 0, "`s `x -> . Writes mudlle value `x to file `s",
          2, (struct string *file, value x),
          OP_LEAF | OP_NOESCAPE, "sx.")
{
  return do_save_data(file, x, rename);
}


UNSAFETOP(load_data, 0, "`s -> `x. Loads a value from a mudlle save file",
          1, (struct string *file),
          OP_LEAF | OP_NOESCAPE, "s.x")
{

  int fd = open(file->str, O_RDONLY);
  if (fd < 0)
    goto failed_open;

  uint32_t magic;
  if (read(fd, &magic, sizeof magic) != sizeof magic)
    goto failed;
  magic = ntohl(magic);

  uint32_t v = magic - MDATA_MAGIC;
  if (v > MDATA_VER_CURRENT)
    goto failed;
  enum mudlle_data_version version = v;

  uint32_t size;
  if (read(fd, &size, sizeof size) != sizeof size)
    goto failed;
  size = ntohl(size);

  char *data = malloc(size);
  if (read(fd, data, size) == size)
    {
      close(fd);
      value res = gc_load(data, size, version);
      free(data);
      return res;
    }
  free(data);

failed:
  close(fd);

failed_open:


  runtime_error(error_bad_value);
}

TYPEDOP(size_data, 0,
        "`x -> `v. Return the size of object `x in bytes as"
        " vector(`total, `mutable, `static).", 1, (value x),
        OP_LEAF | OP_NOESCAPE, "x.v")
{
  struct gc_size size;
  gc_size(x, &size);
  struct vector *v = alloc_vector(3);
  v->data[0] = makeint(size.s_total);
  v->data[1] = makeint(size.s_mutable);
  v->data[2] = makeint(size.s_static);
  return v;
}

UNSAFETOP(staticpro_data, 0, "-> `v. Returns a vector of all statically "
          "protected data", 0, (void), OP_LEAF | OP_NOESCAPE, ".v")
{
  return get_staticpro_data();
}

TYPEDOP(immutablep, "immutable?",
        "`x -> `b. Returns true if `x is an immutable value",
	1, (value x),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(immutablep(x));
}

TYPEDOP(readonlyp, "readonly?",
        "`x -> `b. Returns true if `x is a read-only value",
	1, (value x),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(readonlyp(x));
}

TYPEDOP(protect, 0, "`x -> `x. Makes object `x readonly."
        " Outport ports will silently not be made readonly. Cf. `rprotect().",
	1, (struct obj *x),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.1")
{
  if (!pointerp(x))
    return x;

  switch (x->type)
    {
    case type_table:
      protect_table((struct table *)x);
      break;
    case type_oport:
      break;
    default:
      x->flags |= OBJ_READONLY;
    }

  return x;
}

/* return true if v was (made) immutable */
static bool rprotect(value v)
{
  if (!pointerp(v))
    return true;
  struct grecord *rec = v;
  if (!readonlyp(v))
    {
      if (rec->o.type == type_oport)
        return false;
      rec->o.flags |= OBJ_READONLY;
    }
  if (immutablep(v))
    return true;
  switch (rec->o.type)
    {
    case type_pair:
    case type_vector:
    case type_symbol:
    case type_table:
      {
        size_t len = grecord_len(rec);
        bool all = true;
        /* Use the immutable flag to indicate that we've already been
           here and to prevent infinite recursion. */
        rec->o.flags |= OBJ_IMMUTABLE;
        for (size_t i = 0; i < len; ++i)
          {
            if (!rprotect(rec->data[i]))
              all = false;
          }
        if (!all)
          rec->o.flags &= ~OBJ_IMMUTABLE;
        return all;
      }
    default:
      return false;
    }
}

TYPEDOP(rprotect, 0, "`x -> `x. Recursively (for pairs, vectors, symbols, and"
        " tables) makes value `x readonly and, if possible, immutable.\n"
        "Outport ports will silently not be made readonly.\n"
        "The recursion stops at immutable objects, so any strings only"
        " reachable through immutable objects will not be made readonly.\n"
        "Cf. `protect().",
	1, (value x), OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.1")
{
  rprotect(x);
  return x;
}

UNSAFETOP(detect_immutability, 0, "-> . Detects immutable values.",
          0, (void),
          OP_LEAF | OP_NOESCAPE, ".")
{
  detect_immutability();
  undefined();
}

TYPEDOP(check_immutable, 0, "`x -> `x. Makes `x immutable if possible without"
        " recursion", 1, (value x),
        OP_LEAF | OP_NOESCAPE, "x.1")
{
  if (pointerp(x))
    check_immutable(x);
  return x;
}

TYPEDOP(typeof, 0, "`x -> `n. Return type of `x",
	1, (value x),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makeint(TYPEOF(x));
}

SECTOP(seclevel, 0, "-> `n. Returns the current value of the global seclevel"
       " variable.\n"
       "When called directly from a closure, this returns that closure's"
       " `function_seclevel().\n"
       "All function calls from inside closures except those to regular"
       " (non-secure and non-vararg) primitives set seclevel.\n"
       "Use `with_minlevel() and `minlevel() if you need to write secure"
       " functions in mudlle.\n"
       " Cf. `effective_seclevel().",
       0, (void),
       1, OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".n")
{
  return makeint(get_seclevel());
}

TYPEDOP(minlevel, 0, "-> `n. Returns the minimum security level of the"
	" current session. Calling a function with seclevel less than"
	" minlevel will result in a security violation error", 0, (void),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".n")
{
  return makeint(minlevel);
}

UNSAFETOP(unlimited_execution, 0, "-> . Disables recursion and loop limits in"
          " the current session; cf. `session() and `loop_count().\n"
          "There is still a limit on the maximum amount of stack space"
          " that can be used.",
          0, (void), OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".")
{
  unlimited_execution();
  undefined();
}

#ifdef ALLOC_STATS

UNSAFETOP(alloc_stats, 0, "-> `v", 0, (void), OP_LEAF | OP_NOESCAPE,
          ".v")
{
  return get_alloc_stats();
}

#endif

void basic_init(void)
{
  DEFINE(functionp);
  DEFINE(callablep);
  DEFINE(may_callp);

  DEFINE(error);
  DEFINE(warning);
  DEFINE(catch_error);
  DEFINE(handle_error);
  DEFINE(trap_error);
  DEFINE(with_minlevel);
  DEFINE(with_maxseclevel);

  STATIC_STRING(sstr_seclev_globals, "SECLEVEL_GLOBALS");
  system_write(GET_STATIC_STRING(sstr_seclev_globals),
               makeint(SECLEVEL_GLOBALS));

  system_define("call_trace_off",     makeint(call_trace_off));
  system_define("call_trace_barrier", makeint(call_trace_barrier));
  system_define("call_trace_on",      makeint(call_trace_on));

  define_string_vector("error_messages", mudlle_errors, last_runtime_error);

#define SYSDEF_MTYPE(type) system_define(#type, makeint(type));
  FOR_MUDLLE_TYPES(SYSDEF_MTYPE);
#undef SYSDEF_MTYPE

  define_string_vector("type_names", mudlle_type_names, last_synthetic_type);

  DEFINE(setjmp);
  DEFINE(longjmp);

  DEFINE(session);
  DEFINE(apply);
  DEFINE(eval);
  DEFINE(read_constant);

  DEFINE(call_trace);
  DEFINE(max_loop_count);
  DEFINE(loop_count);
  DEFINE(compiledp);

  DEFINE(typeof);
  DEFINE(immutablep);
  DEFINE(readonlyp);
  DEFINE(protect);
  DEFINE(rprotect);
  DEFINE(detect_immutability);
  DEFINE(check_immutable);

  DEFINE(size_data);
  DEFINE(staticpro_data);
  DEFINE(save_data);
  DEFINE(load_data);

  DEFINE(ref);
  DEFINE(setb);

  DEFINE(dereference);
  DEFINE(make_ref);
  DEFINE(make_pair_ref);
  DEFINE(make_symbol_ref);
  DEFINE(make_variable_ref);
  DEFINE(make_custom_ref);
  DEFINE(set_refb);

  DEFINE(unlimited_execution);

  DEFINE(seclevel);
  DEFINE(minlevel);
  DEFINE(maxseclevel);
  DEFINE(effective_seclevel);

#ifdef ALLOC_STATS
  DEFINE(alloc_stats);
#endif
}
