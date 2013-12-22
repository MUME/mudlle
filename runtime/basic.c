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
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>

#include "../alloc.h"
#include "../call.h"
#include "../compile.h"
#include "../context.h"
#include "../global.h"
#include "../interpret.h"
#include "../lexer.h"
#include "../mcompile.h"
#include "../mparser.h"
#include "../table.h"

#include "basic.h"
#include "list.h"
#include "runtime.h"
#include "stringops.h"
#include "symbol.h"
#include "vector.h"


static bool is_function(value v)
{
  switch (TYPEOF(v))
    {
    case type_closure:
    case type_primitive:
    case type_secure:
    case type_varargs:
      return true;
    default:
      return false;
    }
}

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
    runtime_error(error_bad_type);
  long nargs = GETINT(margs);
  if (nargs < 0)
    runtime_error(error_bad_value);
  return makebool(callablep(f, nargs));
}

TYPEDOP(may_callp, "may_call?", "`f -> `b. True if the caller is allowed to"
	" call `f without a security violation due to minlevel.", 1, (value f),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "f.n")
{
  callable(f, -1);
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

value apply_vararg(value (*prim)(), struct vector *args)
{
  args = vector_copy(args);
  return prim(args, vector_len(args));
}

static runtime_errors apply(value f, struct vector *args, value *r)
{
  if (!TYPE(args, type_vector))
    return error_bad_type;
  if (!pointerp(f))
    return error_bad_function;

  int nargs = vector_len(args);
  mtype type = ((struct obj *)f)->type;
  if (!((1 << type) & TYPESET_FUNCTION))
    return error_bad_function;

  if (!callablep(f, nargs))
    return error_wrong_parameters;

  if (type == type_closure)
    {
      *r = call(f, args);
      return error_none;
    }

  /* set up a call stack frame when calling primitives */
  struct call_stack me = {
    .next = call_stack,
    .type = call_c,
    .u.c = {
      .u.prim = f,
    }
  };
  call_stack = &me;

  if (type == type_varargs)
    {
      struct primitive *prim = f;
      prim->call_count++;
      me.u.c.args[0] = args;
      me.u.c.nargs = 1;
      *r = apply_vararg(prim->op->op, args);
    }
  else
    {
      assert(nargs < MAX_PRIMITIVE_ARGS);
      memcpy(me.u.c.args, args->data, sizeof args->data[0] * nargs);
      me.u.c.nargs = nargs;
      *r = call(f, args);
    }

  call_stack = me.next;
  return error_none;
}

TYPEDOP(apply, 0,
        "`f `v -> `x. Excutes `f with arguments `v, returns its result",
	2, (value f, struct vector *args),
	0, "fv.x")
{
  value result;
  runtime_errors error = apply(f, args, &result);
  if (error != error_none)
    primitive_runtime_error(error, &op_apply, 2, f, args);
  return result;
}

TYPEDOP(apply_stack_entry, 0,
        "`x0 `f `v -> `x1. Applies `v to `f as `apply does and returns its"
        " result.\n"
        "This primitive's call stack entry will display its arguments even"
        " when called from a compiled closure.",
        3, (value x, value f, struct vector *args), 0, "xfv.x")
{
  struct call_stack *old_call_stack = call_stack;
  struct call_stack me;

  /* prevent duplicate entries */
  if (call_stack == NULL || call_stack->type != call_c
      || call_stack->u.c.u.prim->op != &op_apply_stack_entry)
    {
      me = (struct call_stack){
        .next = call_stack,
        .type = call_primop,
        .u.c = {
          .u.op = &op_apply_stack_entry,
          .nargs = 3,
          .args = { x, f, args }
        }
      };
      call_stack = &me;
    }

  value result;
  runtime_errors error = apply(f, args, &result);

  if (error != error_none)
    runtime_error(error);

  call_stack = old_call_stack;
  return result;
}

#  define EVAL_NAME "ieval"
SECTOP(eval, EVAL_NAME,
       "`s -> `x. Excute the expression in `s and return its result."
       " On compile error, the message is display()'ed and `error_compile"
       " is caused.",
       1, (struct string *str), 1, 0, "s.x")
{
  TYPEIS(str, type_string);

  char *scopy;
  LOCALSTR(scopy, str);
  read_from_string(scopy, "<eval>", "<eval>");

  runtime_errors error;
  block_t parser_block = new_block();
  mfile f = parse(parser_block);
  if (f == NULL)
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
          free_block(parser_block);
          return call0(closure);
        }
    }

  error = error_compile;
 errout:
  free_block(parser_block);
  runtime_error(error);
}

TYPEDOP(call_trace, 0, " -> `v. Returns current call trace. Each entry is"
        " either a function, a machine code object (`type_mcode),"
        " or a string.", 0, (void),
        OP_NOESCAPE, ".v")
{
  return get_mudlle_call_trace();
}

TYPEDOP(error, 0, "`n -> . Causes error `n", 1, (value errn),
        OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "n.")
{
  long n = GETINT(errn);
  if (n < 0 || n >= last_runtime_error)
    runtime_error(error_bad_value);
  runtime_error(n);
}

TYPEDOP(compiledp, "compiled?",
        " -> `b. Returns true if called from compiled code, ignoring"
        " levels of primitives.",
        0, (void), OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".n")
{
  struct call_stack *cstack = call_stack;
  while (cstack && cstack->type == call_c)
    cstack = cstack->next;
  return makebool(cstack && cstack->type == call_compiled);
}

TYPEDOP(max_loop_count, 0,
        " -> `n. Returns the maximum loop count before an `error_loop is"
        " generated. See `loop_count().",
        0, (void),
        OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".n")
{
  struct call_stack *cstack = call_stack;
  while (cstack && cstack->type == call_c)
    cstack = cstack->next;

  if (cstack == NULL)
    return makeint(0);

  if (cstack->type == call_compiled)
    return makeint(MAX_FAST_CALLS);

  return makeint(MAX_CALLS);
}

TYPEDOP(loop_count, 0,
        " -> `n. Returns the current loop counter. This counter is"
        " decremented at every function call and every iteration of a loop"
        " statement. When `n reaches 0, an `error_loop is generated.\n"
        "A good way to test for whether we are approaching an error is"
        " 0 < `n && `n < 1000, or (`max_loop_count() >> 8) instead"
        " of 1000.",
        0, (void),
        OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".n")
{
  struct call_stack *cstack = call_stack;
  while (cstack && cstack->type == call_c)
    cstack = cstack->next;

  if (cstack == NULL)
    return makeint(0);

  if (cstack->type == call_compiled)
    return makeint(xcount);

  return makeint(session_context->call_count);
}

static value result;

static void docall0(void *x)
{
  result = call0(x);
}

/* calls f() and returns value in "result"

   if f() causes an error:
     for loop and recurse errors, re-raise the error

     if handler is non-null, call handler(errno) and returns its value
     in "result"

     if handler is null, return errno in "result"
 */
static int trap_error(value f, value handler,
                      enum call_trace_mode call_trace_mode,
                      bool handle_loops,
                      uword new_minlevel)
{
  callable(f, 0);
  if (handler)
    callable(handler, 1);

  ulong saved_call_count = 0, saved_recursion_count = 0, saved_xcount = 0;
  if (handle_loops)
    {
      saved_xcount          = MIN(xcount, xcount / 10 + 100);
      saved_call_count      = MIN(session_context->call_count,
                                  session_context->call_count / 10 + 100);
      saved_recursion_count = MIN(session_context->recursion_count,
                                  session_context->recursion_count / 10 + 100);
      xcount                           -= saved_xcount;
      session_context->call_count      -= saved_call_count;
      session_context->recursion_count -= saved_recursion_count;
    }

  uword old_minlevel = minlevel;
  minlevel = new_minlevel;

  int ok;
  {
    GCPRO1(handler);
    ok = mcatch(docall0, f, call_trace_mode);
    UNGCPRO();
  }

  minlevel = old_minlevel;

  xcount                           += saved_xcount;
  session_context->call_count      += saved_call_count;
  session_context->recursion_count += saved_recursion_count;

  if (ok)
    return 0;

  if (exception_signal == SIGNAL_ERROR)
    {
      if (exception_value == makeint(error_loop)
          && (!handle_loops
              || session_context->call_count == 0
              || xcount == 0))
        goto propagate;
      if (exception_value == makeint(error_recurse)
          && (!handle_loops
              || session_context->recursion_count == 0))
        goto propagate;

      if (handler)
        {
          value exc = exception_value;
          int sig = exception_signal;
          GCPRO1(exc);
          result = call1(handler, exception_value);
          UNGCPRO();
          exception_signal = sig;
          exception_value = exc;
        }
      else
        result = exception_value;
      return 1;
    }

propagate:
  mthrow(exception_signal, exception_value);
}

TYPEDOP(catch_error, 0, "`f `b -> `x. Executes `f() and returns its result."
        " If an error occurs, returns the error number. If `b is true, error"
        " messages are suppressed. Does not catch loop/recursion errors."
        " Cf. `trap_error().",
        2, (value f, value suppress),
        0, "fx.x")
{
  trap_error(f, NULL, istrue(suppress) ? call_trace_off : call_trace_on,
             false, minlevel);
  return result;
}

TYPEDOP(handle_error, 0, "`f0 `f1 -> `x. Executes `f0(). If an error occurs,"
        " calls `f1(`errno). Returns result of `f0() or `f1(`errno)."
        " Does not handle loop/recursion errors. Cf. `trap_error().",
        2, (value f, value handler),
        0, "ff.x")
{
  if (handler == NULL)
    runtime_error(error_bad_function);
  trap_error(f, handler, call_trace_off, false, minlevel);
  return result;
}

TYPEDOP(trap_error, 0,
        "`f0 `f1 `n `b -> `x. Executes `f0() and returns its return"
        " value.\n"
        "If an error occurs in `f0(), calls `f1(`errno), and returns its"
        " return value instead. `f1 can be `null, in which case an error"
        " in `f0() makes `trap_error() return the error number.\n"
        "If `b is true, reserve some space to handle loop and recursion"
        " errors as well; otherwise they are not handled.\n"
        "`n specifies how to print any call traces from `f0:\n"
        "  `call_trace_off      \tdo not print any call trace\n"
        "  `call_trace_barrier  \tprint call traces, but only print until"
        " the current stack level\n"
        "  `call_trace_on       \tprint complete call traces",
        4, (value f, value handler, value ct_mode, value handle_loops), 0,
        "f[fu]nx.x")
{
  enum call_trace_mode call_trace_mode = GETINT(ct_mode);

  if (call_trace_mode < call_trace_off
      || call_trace_mode > call_trace_on)
    runtime_error(error_bad_value);

  trap_error(f, handler, call_trace_mode, istrue(handle_loops), minlevel);
  return result;
}

SECTOP(with_minlevel, 0,
       "`n `c -> `x. Call `c() with the minimum function security level"
       " set to `n and return its result, where 0 <= `n <= the calling"
       " closure's `function_seclevel().\n"
       "No closure whose `function_seclevel() is less than `n can be called"
       " from `c(), or an `error_security_violation will be caused.",
       2, (value n, value f), 1, 0, "nf.x")
{
  long new_minlevel = GETINT(n);
  if (new_minlevel < 0 || new_minlevel > LVL_IMPLEMENTOR)
    runtime_error(error_bad_value);
  if (new_minlevel > get_seclevel())
    runtime_error(error_security_violation);

  if (trap_error(f, NULL, call_trace_on, false, new_minlevel))
    mthrow(exception_signal, exception_value);

  return result;
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
          1, (struct closure *fn), 0, "f.x")
{
  struct session_context newc;
  value aresult;

  callable(fn, 0);
  session_start(&newc,
                &(const struct session_info){
                  .minlevel = minlevel,
                  .muser    = muduser,
                  .mout     = mudout,
                  .merr     = muderr });
  aresult = mcatch_call0(NULL, fn);
  session_end();

  return aresult;
}

static const typing tref = { "vn.x", "sn.n", "[ton]s.x", NULL };

FULLOP(ref, 0, "`x1 `x2 -> `x3. Generic interface to lookup operations:"
       " `x1[`x2] -> `x3",
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

void ref_runtime_error(runtime_errors error, value x1, value x2)
{
  primitive_runtime_error(error_bad_type, &op_ref, 2, x1, x2);
}

static const typing tset = { "vnx.3", "snn.n", "[ton]sx.3", NULL };

FULLOP(set, "set!",
       "`x1 `x2 `x3 -> . Generic interface to set operations: `x1[`x2] = `x3",
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

void set_runtime_error(runtime_errors error, value x1, value x2, value x3)
{
  primitive_runtime_error(error_bad_type, &op_set, 3, x1, x2, x3);
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
        1, (struct grecord *r), 0, "r.x")
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
            return GVAR(idx);
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

TYPEDOP(set_ref, "set_ref!", "`r `x -> `x. Set the value of the reference"
        " `r to `x.",
        2, (struct grecord *r, value val), 0, "rx.2")
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
          if (GCONSTANT(idx))
            runtime_error(error_variable_read_only);
          return GVAR(idx) = val;
        }
      else if (pointerp(v))
        switch (r->data[0]->type)
          {
          case type_variable: ;
            struct variable *var = (struct variable *)r->data[0];
            assert(TYPE(var, type_variable));
            if (var->o.flags & OBJ_READONLY)
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
          return (intval(r->data[1]) ? code_setcdr : code_setcar)(
            (struct list *)r->data[0], val);
        }
      if (TYPE(r->data[1], type_vector))
        {
          struct vector *fns = (struct vector *)r->data[1];
          return call2(fns->data[2], r->data[0], val);
        }
      return code_set(r->data[0], r->data[1], val);
    }
  abort();
}

TYPEDOP(make_pair_ref, 0, "`p `n -> `r. Return a reference to the"
        " `car (`n == 0) or `cdr (`n == 1) of `p.",
        2, (struct list *p, value n), OP_LEAF | OP_NOESCAPE, "kn.r")
{
  TYPEIS(p, type_pair);
  long i = GETINT(n);
  if (i < 0 || i > 1)
    runtime_error(error_bad_value);
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
        " `v must be a sequence(`desc, `deref, `setref) of functions that"
        " defines what the reference does:\n"
        "  `desc   \t`x0 -> `s. Return a description of the reference."
        " An ampersand (&) is automatically prepended.\n"
        "  `deref  \t`x0 -> `x1. Dereference the reference and return its"
        " value.\n"
        "  `setref \t`x0 `x1 -> `x2. Set the reference's value to `x1 and"
        " return a sensible value, typically `x1 unmodified.",
        2, (value val, struct vector *fns), OP_LEAF | OP_NOESCAPE, "xv.r")
{
  TYPEIS(fns, type_vector);
  if (vector_len(fns) != 3 || !readonlyp(fns))
    runtime_error(error_bad_value);
  callable(fns->data[0], 1);
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
          struct string *s = x2;
          char *scopy;
          LOCALSTR(scopy, s);
          GCPRO1(x1);
          s = alloc_string_length(scopy, string_len(s));
          UNGCPRO();
          x2 = make_readonly(s);
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

#define OBJ_MAGIC 0x871f54ab

static value do_save_data(struct string *file, value x,
                          int (*rename_fn)(const char *oldpath,
                                           const char *newpath))
{
  TYPEIS(file, type_string);


  char *fname;
  LOCALSTR(fname, file);

  ulong size;
  void *data = gc_save(x, &size);

  static const char tpattern[] = "%s.XXXXXX";
  size_t tmplen = strlen(fname) + strlen(tpattern) - 2 /* %s */;
  char *tmpname = alloca(tmplen + 1);
  if (sprintf(tmpname, tpattern, fname) != tmplen)
    abort();

  int fd = mkstemp(tmpname);
  if (fd < 0)
    goto failed;

  ulong magic = htonl(OBJ_MAGIC);
  ulong nsize = htonl(size);
  bool ok = (write(fd, &magic, sizeof magic) == sizeof magic &&
             write(fd, &nsize, sizeof nsize) == sizeof nsize &&
             write(fd, data, size) == size);

  fchmod(fd, S_IRUSR | S_IWUSR | S_IRGRP);
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


static value _load_data(value (*fgc_load)(void *_load, unsigned long size),
		       struct string *file)
{
  int fd;
  unsigned long magic, size;


  TYPEIS(file, type_string);


  fd = open(file->str, O_RDONLY);
  if (fd < 0)
    goto failed;

  if (read(fd, &magic, sizeof magic) == sizeof magic &&
      ntohl(magic) == OBJ_MAGIC &&
      read(fd, &size, sizeof size) == sizeof size)
    {
      size = ntohl(size);
      char data[size];
      if (read(fd, data, size) == size)
	{
	  value res;

	  close(fd);

	  res = fgc_load(data, size);
	  return res;
	}
    }
  close(fd);

failed:


  runtime_error(error_bad_value);
}

UNSAFETOP(load_data, 0, "`s -> `x. Loads a value from a mudlle save file",
          1, (struct string *file),
          OP_LEAF | OP_NOESCAPE, "s.x")
{
  return _load_data(gc_load, file);
}

#ifndef GCDEBUG
UNSAFEOP(load_data_debug, 0,
         "`s -> `x. Loads a value from a GCDEBUG mudlle save file",
	 1, (struct string *file),
	 OP_LEAF | OP_NOESCAPE)
{
  return _load_data(gc_load_debug, file);
}
#endif

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

UNSAFETOP(staticpro_data, 0, " -> `v. Returns a vector of all statically "
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

TYPEDOP(protect, 0, "`x -> `x. Makes value `x readonly",
	1, (struct obj *x),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.1")
{
  if (!pointerp(x))
    return x;

  if (x->type == type_table)
    protect_table((struct table *)x);
  else
    x->flags |= OBJ_READONLY;

  return x;
}

/* return true if v was (made) immutable */
static bool rprotect(value v)
{
  if (!pointerp(v))
    return true;
  struct grecord *rec = v;
  if (!readonlyp(v))
    rec->o.flags |= OBJ_READONLY;
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
        "The recursion stops at immutable objects, so any strings only"
        " reachable through immutable objects will not be made readonly.",
	1, (value x), OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.1")
{
  rprotect(x);
  return x;
}

UNSAFEOP(detect_immutability, 0, " -> . Detects immutable values.",
	 0, (void),
	 OP_LEAF | OP_NOESCAPE)
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

TYPEDOP(seclevel, 0, " -> `n. Returns the current value of the global seclevel"
        " variable.\n"
        "All function calls from inside closures except those to regular"
        " (non-secure and non-vararg) primitives set seclevel.\n"
        "Use `with_minlevel() and `minlevel() if you need to write secure"
        " functions in mudlle.",
        0, (void),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".n")
{
  return makeint(internal_seclevel);
}

TYPEDOP(minlevel, 0, " -> `n. Returns the minimum security level of the"
	" current session. Calling a function with seclevel less than"
	" minlevel will result in a security violation error", 0, (void),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".n")
{
  return makeint(minlevel);
}

UNSAFEOP(unlimited_execution, 0, " -> . Disables recursion and loop limits in"
         " the current session; cf. `session() and `loop_count().\n"
         "There is still a limit on the maximum amount of stack space"
         " that can be used.",
         0, (void), OP_LEAF | OP_NOALLOC | OP_NOESCAPE)
{
  unlimited_execution();
  undefined();
}

#ifdef ALLOC_STATS

UNSAFEOP(closure_alloc_stats, 0, "  -> `v", 0, (void), OP_LEAF | OP_NOESCAPE)
{
  return get_closure_alloc_stats();
}

struct vector *get_pair_alloc_stats(void);

UNSAFEOP(pair_alloc_stats, 0, "  -> `v", 0, (void), OP_LEAF | OP_NOESCAPE)
{
  return get_pair_alloc_stats();
}

struct vector *get_variable_alloc_stats(void);

UNSAFEOP(variable_alloc_stats, 0, "  -> `v", 0, (void), OP_LEAF | OP_NOESCAPE)
{
  return get_variable_alloc_stats();
}

#endif

void basic_init(void)
{
  DEFINE(functionp);
  DEFINE(callablep);
  DEFINE(may_callp);

  DEFINE(error);
  DEFINE(catch_error);
  DEFINE(handle_error);
  DEFINE(trap_error);
  DEFINE(with_minlevel);

  system_define("call_trace_off",     makeint(call_trace_off));
  system_define("call_trace_barrier", makeint(call_trace_barrier));
  system_define("call_trace_on",      makeint(call_trace_on));

  define_string_vector("error_messages", mudlle_errors, last_runtime_error);

  define_string_vector("type_names", mtypenames, last_synthetic_type);

  DEFINE(setjmp);
  DEFINE(longjmp);

  DEFINE(session);
  DEFINE(apply);
  DEFINE(apply_stack_entry);
  DEFINE(eval);

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

#ifndef GCDEBUG
  DEFINE(load_data_debug);
#endif

  DEFINE(ref);
  DEFINE(set);

  DEFINE(dereference);
  DEFINE(make_ref);
  DEFINE(make_pair_ref);
  DEFINE(make_symbol_ref);
  DEFINE(make_variable_ref);
  DEFINE(make_custom_ref);
  DEFINE(set_ref);

  DEFINE(unlimited_execution);
  DEFINE(seclevel);
  DEFINE(minlevel);

#ifdef ALLOC_STATS
  DEFINE(closure_alloc_stats);
  DEFINE(pair_alloc_stats);
  DEFINE(variable_alloc_stats);
#endif
}
