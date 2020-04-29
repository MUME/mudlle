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

#include "../mudlle-config.h"

#include <ctype.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include <netinet/in.h>

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/utsname.h>

#include "basic.h"
#include "check-types.h"
#include "list.h"
#include "mudlle-string.h"
#include "prims.h"
#include "symbol.h"
#include "vector.h"

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


TYPEDOP(functionp, "function?", "`x -> `b. True if `x is a function;"
        " cf. callable?().",
        (value v),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(is_function(v));
}

TYPEDOP(callablep, "callable?", "`f `n -> `b. True if function `f"
        " can be called with `n arguments; cf. function?()",
        (value f, value margs),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "fn.n")
{
  int nargs;
  CHECK_TYPES(f,     CT_FUNCTION,
              margs, CT_RANGE(nargs, 0, INT_MAX));
  return makebool(callablep(f, nargs));
}

TYPEDOP(may_callp, "may_call?", "`f -> `b. True if the caller is allowed to"
	" call `f without a security violation due to minlevel.", (value f),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "f.n")
{
  CHECK_TYPES(f, CT_FUNCTION);
  return makebool(!minlevel_violator(f, minlevel));
}

static struct vector *vector_copy(struct vector *v)
{
  struct vector *result;
  long l = vector_len(v);

  GCPRO(v);
  result = alloc_vector(l);
  memcpy(result->data, v->data, l * sizeof *v->data);
  UNGCPRO();

  return result;
}

#if (defined __i386__ || defined __x86_64__) && !defined NOCOMPILER
/* called from x{64,86}builtins.S */
value builtin_call_vararg(value (*prim)(), int nargs, const value *args);
value builtin_call_vararg(value (*prim)(), int nargs, const value *args)
{
  if (nargs > MAX_FUNCTION_ARGS)
    runtime_error(error_wrong_parameters);

  struct vector *argv = alloc_vector(nargs);
  memcpy(argv->data, args, sizeof *args * nargs);
  return prim(argv, nargs);
}

/* called from x{64,86}builtins.S */
value builtin_apply_vararg(vararg_op_fn prim, struct vector *args);
value builtin_apply_vararg(vararg_op_fn prim, struct vector *args)
#else  /* ! __i386__ || NOCOMPILER */
static value builtin_apply_vararg(vararg_op_fn prim, struct vector *args)
#endif
{
  long nargs = vector_len(args);
  if (nargs > MAX_FUNCTION_ARGS)
    runtime_error(error_wrong_parameters);

  args = vector_copy(args);
  return prim(args, nargs);
}

TYPEDOP(apply, ,
        "`f `v -> `x. Excutes `f with arguments `v, returns its result",
	(struct obj *f, struct vector *args),
	OP_APPLY, "fv.x")
{
  int nargs;
  CHECK_TYPES(/* safe as the type check always happens first */
              f,    CT_CALLABLE(nargs = vector_len(args)),
              args, vector);

  if (f->type == type_closure)
    return callv(f, args);

  struct primitive *prim = (struct primitive *)f;
  if (f->type == type_varargs)
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
          .u.prim = prim,
          .nargs = 1
        },
        .args = { args }
      };
      call_stack = &me.c.s;
      value r = builtin_apply_vararg(prim->op->op, args);
      call_stack = me.c.s.next;
      return r;
    }

  struct {
    struct call_stack_c_header c;
    value args[MAX_PRIMITIVE_ARGS];
  } me;
  me.c = (struct call_stack_c_header){
    .s = {
      .next = call_stack,
      .type = call_c
    },
    .u.prim = prim,
    .nargs = nargs
  };
  call_stack = &me.c.s;

  memcpy(me.args, args->data, sizeof args->data[0] * nargs);

  if (f->type == type_secure)
    callable(f, nargs);     /* checks seclevel */

  value r = callv(f, args);
  call_stack = me.c.s.next;
  return r;
}

static value do_eval(value input, struct table *cache, bool force_constant)
{
  ASSERT_NOALLOC_START();

  const char **lines;
  if (TYPE(input, string))
    {
      lines = malloc(2 * sizeof *lines);
      lines[0] = ((struct string *)input)->str;
      lines[1] = NULL;
    }
  else
    {
      assert(TYPE(input, vector));
      struct vector *iv = input;
      lines = malloc((vector_len(iv) + 1) * sizeof *lines);
      for (int i = 0; i < vector_len(iv); ++i)
        {
          TYPEIS(iv->data[i], string);
          lines[i] = ((struct string *)iv->data[i])->str;
        }
      lines[vector_len(iv)] = NULL;
    }

  struct reader_state rstate;
  save_reader_state(&rstate);
  static const struct filename eval_fname = {
    .path = "<eval>",
    .nice = "<eval>"
  };
  read_from_strings(lines, &eval_fname, force_constant);
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

  value result = makebool(false);
  enum runtime_error error = error_none;

  if (force_constant)
    {
      if (ok)
        result = alloc_list(make_shared_string_constant(c, cache), NULL);
      goto done;
    }

  if (ok)
    {
      if (f->vclass != f_plain)
        {
          error = error_bad_value;
          goto done;
        }

      if (mstart(parser_block, f, get_seclevel()))
        {
          struct closure *closure = compile_code(f, get_seclevel());
          mstop(f);
          if (closure != NULL)
            {
              result = call0(closure);
              goto done;
            }
        }
    }

  error = error_compile;
 done:
  restore_reader_state(&rstate);
  free_block(parser_block);
  if (error != error_none)
    runtime_error(error);
  return result;
}

#  define EVAL_NAME "ieval"
SECOP(eval, EVAL_NAME,
       "`s|`v -> `x. Excute the expression in `s (or `v, a vector of strings)"
       " and return its result.\n"
       "On compile error, the message is printed using `display() and"
       " `error_compile is thrown.",
       (value input), 1, 0, "[sv].x")
{
  CHECK_TYPES(input, CT_TYPES(string, vector));
  return do_eval(input, NULL, false);
}

SECOP(read_constant, ,
      "`s|`v `t -> `p|false. Evaluate `s (or `v, a vector of strings) as a"
      " constant and return its value `x as `list(`x).\n"
      "`t is a table (or ctable) of shared strings. If a string for which `t"
      " has has a symbol, a new string is not created. If `t is writable,"
      " any newly created strings are added to `t.\n"
      "`t can be null to disable string sharing.\n"
      "The input will be interpreted as if it started with an apostrophe (')"
      " and cannot contain any comma-prefixed expressions.\n"
      "On syntax error, a message is printed using `display() and"
      " false is returned.\n"
      "See also `pwrite_constant().",
      (value input, struct table *cache), 1, 0, "[sv][tu].[zk]")
{
  CHECK_TYPES(input, CT_TYPES(string, vector),
              cache, CT_TYPES(null, table));
  return do_eval(input, cache, true);
}

TYPEDOP(call_trace, , "`b -> `v. Returns current call trace. Each entry is"
        " either a function, a machine code object (`type_mcode),"
        " or a string.\n"
        "If `b is true, each entry becomes cons(`called, `line), where"
        " `line is null if unknown.",
        (value lines),
        OP_NOESCAPE, "x.v")
{
  return get_mudlle_call_trace(istrue(lines));
}

TYPEDOP(maxseclevel, ,
        "-> `n. Returns the current maxseclevel, which is the highest"
        " secure primitive seclevel that currently may be called."
        " Cf. `effective_seclevel().",
        (void), OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".n")
{
  return maxseclevel;
}

SECOP(effective_seclevel, ,
      "-> `n. Returns the effective seclevel used for security checks"
      " when calling secure primitives. This is the lowest value"
      " of `maxseclevel() and `seclevel().",
      (void), 1, OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".n")
{
  return makeint(get_effective_seclevel());
}

TYPEDOP(error, , "`n -> . Causes error `n", (value errn),
        OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "n.")
{
  enum runtime_error n;
  CHECK_TYPES(errn, CT_RANGE(n, 0, last_runtime_error - 1));
  runtime_error(n);
}

TYPEDOP(error_message, , "`n `s -> . Displays `s and causes error `n.",
        (value errn, struct string *s),
        OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_STR_READONLY, "ns.")
{
  enum runtime_error n;
  CHECK_TYPES(errn, CT_RANGE(n, 0, last_runtime_error - 1),
              s,    string);
  LOCAL_C_STR(msg, s, 1024);
  runtime_error_message(n, msg);
}

TYPEDOP(warning, , "`s -> . Prints the warning message `s and a stack trace.",
        (struct string *s),
        0, "s.")
{
  CHECK_TYPES(s, string);
  LOCAL_C_STR(msg, s, 1024);
  runtime_warning(msg);
  undefined();
}

const struct prim_op *const warning_prim_ext = &op_warning;

TYPEDOP(compiledp, "compiled?",
        "-> `b. Returns true if called from compiled code, ignoring"
        " levels of primitives.",
        (void), OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".n")
{
  struct call_stack *cstack = call_stack;
  while (cstack && cstack->type == call_c)
    cstack = cstack->next;
  return makebool(cstack && cstack->type == call_compiled);
}

TYPEDOP(max_loop_count, ,
        "-> `n. Returns the maximum loop count before an `error_loop is"
        " generated. See `loop_count().",
        (void),
        OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".n")
{
  return makeint(MAX_LOOP_COUNT);
}

TYPEDOP(loop_count, ,
        "-> `n. Returns the current loop counter. This counter is"
        " decremented at every function call and every iteration of a loop"
        " statement. When `n reaches 0, an `error_loop is generated.\n"
        "A good way to test for whether we are approaching an error is"
        " 0 < `n && `n < 1000, or (`max_loop_count() >> 8) instead"
        " of 1000.",
        (void),
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
     for loop errors, re-raise the error

     if handler is non-null, call handler(errno) and returns its value

     if handler is null, return errno
 */
static value trap_error(value f, value handler,
                        enum call_trace_mode call_trace_mode)
{
  ulong saved_xcount = xcount / 10 + 100;
  if (saved_xcount > xcount)
    saved_xcount = xcount;
  xcount -= saved_xcount;

  {
    struct call0_data d = { .func = f };
    GCPRO(handler);
    bool ok = mcatch(docall0, &d, call_trace_mode);
    UNGCPRO();

    /* careful in case f() called unlimited_execution() */
    xcount += saved_xcount;
    if (xcount > MAX_TAGGED_INT)
      xcount = MAX_TAGGED_INT;

    if (ok)
      return d.result;
  }

  if (mexception.sig == SIGNAL_ERROR)
    {
      enum runtime_error err = mexception.err;
      /* if we are really out of loop space, rethrow the error */
      if (err == error_loop && xcount == 0)
        goto propagate;

      if (handler == NULL)
        return makeint(err);

      return call1(handler, makeint(err));
    }

 propagate:
  mrethrow();
}

TYPEDOP(catch_error, , "`f `b -> `x. Executes `f() and returns its result."
        " If an error occurs, returns the error number.\n"
        "If `b is true, call traces are not sent to the `with_output()"
        " target, but only to those added with `add_call_trace_oport!().",
        (value f, value suppress),
        0, "fx.x")
{
  CHECK_TYPES(f,        CT_CALLABLE(0),
              suppress, any);
  return trap_error(f, NULL,
                    istrue(suppress) ? call_trace_no_err : call_trace_on);
}

TYPEDOP(handle_error, , "`f0 `f1 -> `x. Executes `f0(). If an error occurs,"
        " calls `f1(`errno). Returns result of `f0() or `f1(`errno).",
        (value f, value handler),
        0, "ff.x")
{
  CHECK_TYPES(f,       CT_CALLABLE(0),
              handler, CT_CALLABLE(1));
  return trap_error(f, handler, call_trace_no_err);
}

TYPEDOP(trap_error, ,
        "`f0 `f1 `n -> `x. Executes `f0() and returns its return"
        " value.\n"
        "If an error occurs in `f0(), calls `f1(`errno), and returns its"
        " return value instead. `f1 can be `null, in which case an error"
        " in `f0() makes `trap_error() return the error number.\n"
        "If possible, some space is reserved to handle loop errors.\n"
        "By default, call traces are sent to the `with_output() target"
        " as well as observers added using `add_call_trace_oport!().\n"
        "`n controls call traces from `f0 are sent:\n"
        "  `call_trace_off      \tdo not send call traces to anyone\n"
        "  `call_trace_barrier  \tonly print call traces down to the"
        " current stack level\n"
        "  `call_trace_on       \tprint complete call traces",
        (value f, value handler, value ct_mode), 0,
        "f[fu]n.x")
{
  long call_trace_mode;
  CHECK_TYPES(f,       CT_CALLABLE(0),
              handler, OR(null, CT_CALLABLE(1)),
              ct_mode, CT_INT(call_trace_mode));

  if (call_trace_mode != call_trace_off
      && call_trace_mode != call_trace_barrier
      && call_trace_mode != call_trace_on)
    RUNTIME_ERROR(error_bad_value, "invalid call trace mode");

  return trap_error(f, handler, call_trace_mode);
}

SECOP(with_minlevel, ,
      "`n `c -> `x. Call `c() with the minimum function security level"
      " set to `n and return its result, where 0 <= `n <= the calling"
      " closure's `function_seclevel().\n"
      "No closure whose `function_seclevel() is less than `n can be called"
      " from `c(), or an `error_security_violation will be caused.",
      (value n, value f), 1, OP_APPLY, "nf.x")
{
  seclev_t new_minlevel;
  CHECK_TYPES(n, CT_RANGE(new_minlevel, 0, LVL_IMPLEMENTOR),
              f, CT_CALLABLE(0));
  if (new_minlevel > get_seclevel())
    RUNTIME_ERROR(error_security_violation, NULL);

  seclev_t old_minlevel = minlevel;
  minlevel = new_minlevel;
  value r = call0(f);
  minlevel = old_minlevel;

  return r;
}

SECOP(with_maxseclevel, ,
      "`n `c -> `x. Temporarily change `maxseclevel() to `n"
      " (or `LVL_IMPLEMENTOR if `n >= `LVL_VALA), then calls `c"
      " and returns its result.\nUse this function in your code to make it"
      " available to Ms- even if you need to call secure primitives. The"
      " typical invocation is:\n"
      "   \t`with_maxseclevel(\t`seclevel(), fn () secure_call(...))",
      (value n, value f), 1, 0, "nf.x")
{
  long new_maxseclev;
  CHECK_TYPES(n, CT_RANGE(new_maxseclev, MIN_SECLEVEL, MAX_SECLEVEL),
              f, CT_CALLABLE(0));

  if (new_maxseclev > get_seclevel())
    RUNTIME_ERROR(error_security_violation, NULL);

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

TYPEDOP(setjmp, ,
        "`f -> `x. Executes `f(`buf). `buf can be used with `longjmp(). The"
        " return value is either the result of `f(`buf), or the value `x1"
        " passed to `longjmp(`buf, `x1)",
        (value f), 0, "f.x")
{
  CHECK_TYPES(f, CT_CALLABLE(1));
  return msetjmp(f);
}

TYPEDOP(longjmp, ,
        "`buf `x -> . Returns `x from the `setjmp() that created `buf",
        (value buf, value x), 0, "ox.")
{
  if (!is_mjmpbuf(buf))
    runtime_error(error_bad_value);

  mlongjmp(buf, x);
}

UNSAFEOP(session, , "`f -> . Calls `f() in its own session and return its"
         " result.\n"
         "A session has its own loop and stack depth limits;"
         " cf. `loop_count.\n"
         "If `f() causes an error, return null.",
         (value fn), OP_APPLY, "f.x")
{
  CHECK_TYPES(fn, CT_CALLABLE(0));

  struct session_context newc;
  session_start(&newc,
                &(const struct session_info){
                  .minlevel    = minlevel,
                  .maxseclevel = intval(maxseclevel),
                  .muser       = muduser,
                  .mout        = mudout,
                  .merr        = muderr });
  value aresult = mcatch_call(NULL, fn);
  session_end();

  return aresult;
}

EXT_TYPEDOP(ref, ,
            "`x1 `x2 -> `x3. Generic interface to lookup operations:"
            " `x1[`x2] -> `x3. See `set!() for allowed types.",
            (value x1, value x2), (x1, x2),
            OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_OPERATOR,
            ("vn.x", "sn.n", "[ton]s.x"))
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
  ref_bad_type_error(x1, x2);
}

#define SYMBOL_REF_EXTRA_TYPES ""

EXT_TYPEDOP(symbol_ref, ,
            "`x `s -> `sym. Returns the symbol for `x[`s], creating it if"
            " necessary. `x can be a table" SYMBOL_REF_EXTRA_TYPES ".",
            (value x, struct string *s), (x, s),
            OP_LEAF | OP_NOESCAPE, "[ton]s.y")
{
  CHECK_TYPES(x, any,
              s, string);
  struct table *t;

  if (integerp(x))
    {
    }
  else if (x == NULL)
    ;
  else
    switch (((struct obj *)x)->type)
      {
      case type_table:
        t = x;
        goto ok;
      default:
        break;
      }

  RUNTIME_ERROR(error_bad_type, NULL);

 ok:
  return table_symbol_ref(t, s, NULL);
}

const struct prim_op *const ref_prim_ext = &op_ref;

void ref_bad_type_error(value x1, value x2)
{
  primitive_runtime_error(error_bad_type, &op_ref, 2, x1, x2);
}

#define SET_EXTRA_TYPES " or a table"

EXT_TYPEDOP(setb, "set!",
            "`x1 `x2 `x3 -> `x3. Generic set operation: `x1[`x2] = `x3.\n"
            "`x1 can be a vector or a string (`x2 is the integer index,"
            " where negative values are counted from the end)"
            SET_EXTRA_TYPES
            " (`x2 is the string symbol name).\n"
            "For string `x1, `x3 must be an integer and the returned"
            " value is the stored value (`x3 & 255).",
            (value x1, value x2, value x3), (x1, x2, x3),
            OP_LEAF | OP_NOESCAPE | OP_OPERATOR,
            ("vnx.3", "snn.n", "[ton]sx.3"))
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
  set_bad_type_error(x1, x2, x3);
}

const struct prim_op *const setb_prim_ext = &op_setb;

void set_bad_type_error(value x1, value x2, value x3)
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
 */
TYPEDOP(dereference, , "`r -> `x. Return the value of the reference `r.",
        (struct grecord *r), OP_OPERATOR, "r.x")
{
  CHECK_TYPES(r, reference);
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
    case 2: ;
      value r1 = r->data[1];
      if (TYPE(v, pair) && integerp(r1))
        {
          struct list *p = v;
          return intval(r1) ? p->cdr : p->car;
        }
      return code_ref(v, r1);
    }
  abort();
}

const struct prim_op *const dereference_prim_ext = &op_dereference;

TYPEDOP(set_refb, "set_ref!", "`r `x -> `x. Set the value of the reference"
        " `r to `x.",
        (struct grecord *r, value val), OP_OPERATOR, "rx.2")
{
  CHECK_TYPES(r,   reference,
              val, any);
  long items = grecord_len(r);
  value v = r->data[0];
  switch (items)
    {
    case 1:
      if (integerp(v))
        {
          long idx = intval(v);
          assert(idx >= 0 && idx < intval(environment->used));
          check_global_write(val, idx);
          return GVAR(idx) = val;
        }
      else if (pointerp(v))
        switch (r->data[0]->type)
          {
          case type_variable: ;
            struct variable *var = (struct variable *)r->data[0];
            assert(TYPE(var, variable));
            if (obj_readonlyp(&var->o))
              runtime_error(error_value_read_only);
            return var->vvalue = val;
          case type_symbol:
            return code_symbol_set((struct symbol *)r->data[0], val);
          default:
            break;
          }
    case 2: ;
      value r1 = r->data[1];
      if (TYPE(v, pair) && integerp(r1))
        return (intval(r1) ? code_set_cdrb : code_set_carb)(v, val);
      return code_setb(v, r1, val);
    }
  abort();
}

const struct prim_op *const set_refb_prim_ext = &op_set_refb;

TYPEDOP(make_pair_ref, , "`p `n -> `r. Return a reference to the"
        " `car (`n == 0) or `cdr (`n == 1) of `p.",
        (struct list *p, value n), OP_LEAF | OP_NOESCAPE, "kn.r")
{
  int UNUSED i;
  CHECK_TYPES(p, pair,
              n, CT_RANGE(i, 0, 1));
  GCPRO(p);
  struct grecord *r = allocate_record(type_reference, 2);
  UNGCPRO();
  r->data[0] = &p->o;
  r->data[1] = n;
  return make_readonly(r);
}

TYPEDOP(make_symbol_ref, , "`s -> `r. Return a reference to the"
        " symbol value of `s.",
        (struct symbol *s), OP_LEAF | OP_NOESCAPE, "y.r")
{
  CHECK_TYPES(s, symbol);
  GCPRO(s);
  struct grecord *r = allocate_record(type_reference, 1);
  UNGCPRO();
  r->data[0] = &s->o;
  return make_readonly(r);
}

TYPEDOP(make_variable_ref, , "`n|`v -> `r. Return a reference to global"
        " number `n or variable `v. Cf. `global_lookup().",
        (value midx), OP_LEAF | OP_NOESCAPE, "[no].r")
{
  CHECK_TYPES(midx, CT_TYPES(integer, variable));
  if (integerp(midx))
    {
      long idx = intval(midx);
      if (idx < 0 || idx >= intval(environment->used))
        RUNTIME_ERROR(error_bad_value, "invalid global");
    }
  struct grecord *r = allocate_record(type_reference, 1);
  r->data[0] = midx;
  return make_readonly(r);
}

TYPEDOP(make_ref, , "`x0 `x1 -> `r. Return a reference to `x0[`x1].",
        (value x1, value x2),
        OP_LEAF | OP_NOESCAPE, ("vn.r", "sn.r", "[ton]s.r"))
{
  switch (TYPEOF(x1))
    {
    case type_vector:
      {
        struct vector *v = x1;
        long idx = GETINT(x2);
        if (idx < 0)
          idx += vector_len(v);
        if (idx < 0 || idx >= vector_len(v))
          runtime_error(error_bad_index);
        break;
      }
    case type_string:
      {
        struct string *s = x1;
        long idx = GETINT(x2);
        if (idx < 0)
          idx += string_len(s);
        if (idx < 0 || idx >= string_len(s))
          runtime_error(error_bad_index);
        break;
      }
    case type_table:
      TYPEIS(x2, string);
      if (!readonlyp(x2))
        {
          GCPRO(x1);
          x2 = make_readonly(mudlle_string_copy(x2));
          UNGCPRO();
        }
      break;
    default:
      runtime_error(error_bad_type);
    }
  GCPRO(x1, x2);
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
                                           const char *newpath),
                          const struct prim_op *op)
{
  CHECK_TYPES_OP(op,
                 file, string,
                 x,    any);
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
  bool ok = (write(fd, &magic, sizeof magic) == sizeof magic
             && write(fd, &nsize, sizeof nsize) == sizeof nsize
             && write(fd, data, size) == size);

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

UNSAFEOP(save_data, , "`s `x -> . Writes mudlle value `x to file `s",
         (struct string *file, value x),
         OP_LEAF | OP_NOESCAPE | OP_NUL_STR, "sx.")
{
  return do_save_data(file, x, rename, THIS_OP);
}


UNSAFEOP(load_data, , "`s -> `x. Loads a value from a mudlle save file",
         (struct string *file),
         OP_LEAF | OP_NOESCAPE | OP_STR_READONLY | OP_NUL_STR, "s.x")
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

TYPEDOP(size_data, ,
        "`x -> `v. Return the size of object `x in bytes as"
        " vector(`total, `mutable, `static).\n"
        "Throws `error_bad_value if out of memory.", (value x),
        OP_LEAF | OP_NOESCAPE, "x.v")
{
  struct gc_size size;
  if (!gc_size(x, &size))
    runtime_error(error_bad_value);
  struct vector *v = alloc_vector(3);
  v->data[0] = makeint(size.s_total);
  v->data[1] = makeint(size.s_mutable);
  v->data[2] = makeint(size.s_static);
  return v;
}

UNSAFEOP(staticpro_data, , "-> `v. Returns a vector of all statically"
         " protected data", (void), OP_LEAF | OP_NOESCAPE, ".v")
{
  return get_staticpro_data();
}

UNSAFEOP(dynpro_data, , "-> `l. Returns a list of all dynamically"
         " protected data as vector(`x, `s, `n).",
         (void), OP_LEAF | OP_NOESCAPE, ".l")
{
  return get_dynpro_data();
}

UNSAFEOP(all_code, , "-> `v. Return a vector of all defined"
         " primitives, code and mcode objects.",
         (void), OP_LEAF | OP_NOESCAPE, ".v")
{
  return all_mudlle_code();
}

TYPEDOP(immutablep, "immutable?",
        "`x -> `b. Returns true if `x is an immutable value",
	(value x),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(immutablep(x));
}

TYPEDOP(readonlyp, "readonly?",
        "`x -> `b. Returns true if `x is a read-only value",
	(value x),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(readonlyp(x));
}

TYPEDOP(protect, , "`x -> `x. Makes object `x readonly."
        " Outport ports will silently not be made readonly. Cf. `rprotect().",
	(struct obj *x),
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

TYPEDOP(rprotect, , "`x -> `x. Recursively (for pairs, vectors, symbols, and"
        " tables) makes value `x readonly and, if possible, immutable.\n"
        "Outport ports will silently not be made readonly.\n"
        "The recursion stops at immutable objects, so any strings only"
        " reachable through immutable objects will not be made readonly.\n"
        "Cf. `protect().",
	(value x), OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.1")
{
  rprotect(x);
  return x;
}

UNSAFEOP(detect_immutability, , "-> . Detects immutable values.",
         (void),
         OP_LEAF | OP_NOESCAPE, ".")
{
  detect_immutability();
  undefined();
}

TYPEDOP(check_immutable, , "`x -> `x. Makes `x immutable if possible without"
        " recursion", (value x),
        OP_LEAF | OP_NOESCAPE, "x.1")
{
  if (pointerp(x))
    try_make_immutable(x);
  return x;
}

TYPEDOP(typeof, , "`x -> `n. Return type of `x",
	(value x),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makeint(TYPEOF(x));
}

SECOP(seclevel, , "-> `n. Returns the current value of the global seclevel"
      " variable.\n"
      "When called directly from a closure, this returns that closure's"
      " `function_seclevel().\n"
      "All function calls from inside closures except those to regular"
      " (non-secure and non-vararg) primitives set seclevel.\n"
      "Use `with_minlevel() and `minlevel() if you need to write secure"
      " functions in mudlle.\n"
      " Cf. `effective_seclevel().",
      (void),
      1, OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".n")
{
  return makeint(get_seclevel());
}

TYPEDOP(minlevel, , "-> `n. Returns the minimum security level of the"
	" current session. Calling a function with seclevel less than"
	" minlevel will result in a security violation error", (void),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".n")
{
  return makeint(minlevel);
}

UNSAFEOP(unlimited_execution, , "-> . Disables loop limits and increases"
         " the stack depth limit to the maximum allowed in the current"
         " session; cf. `session() and `loop_count().",
         (void), OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".")
{
  unlimited_execution();
  undefined();
}

#ifdef ALLOC_STATS

UNSAFEOP(alloc_stats, , "-> `v", (void), OP_LEAF | OP_NOESCAPE,
         ".v")
{
  return get_alloc_stats();
}

#endif

TYPEDOP(uname, , "-> `v. Returns a vector of strings of system information,"
        " indexed by `un_xxx:\n"
        "  `un_sysname  \toperating system name\n"
        "  `un_release  \toperating system release\n"
        "  `un_version  \toperating system version\n"
        "  `un_machine  \thardware identifier",
        (void), OP_LEAF | OP_NOESCAPE, ".v")
{
  CHECK_TYPES();

  struct utsname u;
  if (uname(&u) != 0)
    RUNTIME_ERROR(error_abort, "uname() failed");

  struct vector *v = alloc_vector(un_fields);
  GCPRO(v);
  SET_VECTOR(v, un_sysname, alloc_string(u.sysname));
  SET_VECTOR(v, un_release, alloc_string(u.release));
  SET_VECTOR(v, un_version, alloc_string(u.version));
  SET_VECTOR(v, un_machine, alloc_string(u.machine));
  CASSERT_EXPR(un_fields == 4);
  UNGCPRO();
  return v;
}

void basic_init(void)
{
  DEFINE(functionp);
  DEFINE(callablep);
  DEFINE(may_callp);

  DEFINE(error);
  DEFINE(error_message);
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
  DEFINE(dynpro_data);
  DEFINE(save_data);

  DEFINE(all_code);

  DEFINE(load_data);

  DEFINE(symbol_ref);
  DEFINE(ref);
  DEFINE(setb);

  DEFINE(dereference);
  DEFINE(make_ref);
  DEFINE(make_pair_ref);
  DEFINE(make_symbol_ref);
  DEFINE(make_variable_ref);
  DEFINE(set_refb);

  DEFINE(unlimited_execution);

  DEFINE(seclevel);
  DEFINE(minlevel);
  DEFINE(maxseclevel);
  DEFINE(effective_seclevel);

#ifdef ALLOC_STATS
  DEFINE(alloc_stats);
#endif

  DEFINE(uname);
}
