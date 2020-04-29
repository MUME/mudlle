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

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "alloc.h"
#include "builtins.h"
#include "call.h"
#include "context.h"
#include "error.h"
#include "interpret.h"
#include "stack.h"
#include "strbuf.h"

/* Interface to machine code. */

#ifdef NOCOMPILER
value invoke0(struct closure *c) { return NULL; }
#define DEF_INVOKE(N) \
value invoke ## N(struct closure *c, PRIMARGS ## N) { return NULL; }
DOPRIMARGS(DEF_INVOKE, SEP_EMPTY)

value invoke1plus(struct closure *c, value arg, struct vector *args)
{ return NULL; }

value invokev(struct closure *c, struct vector *args) { return NULL; }
#endif  /* NOCOMPILER */

bool minlevel_violator(value c, seclev_t minlev)
{
  struct obj *o = c;
  enum mudlle_type t = o->type;
  struct code *code;
  if (t == type_closure)
    code = ((struct closure *)o)->code;
  else if (t == type_code || t == type_mcode)
    code = (struct code *)o;
  else
    return false;

  return code->seclevel < minlev;
}

bool callablep(value c, int nargs)
/* Returns: false if c is not something that can be called with
     nargs (>= 0) arguments.
*/
{
  if (!pointerp(c))
    return false;

  struct obj *o = c;
  switch (o->type)
    {
    case type_closure:
      {
        struct closure *cl = c;
        if (code_is_vararg(cl->code))
          return nargs <= MAX_FUNCTION_ARGS;
        return vector_len(cl->code->arguments.argv) == nargs;
      }
    case type_secure:
    case type_primitive:
      return ((struct primitive *)o)->op->nargs == nargs;
    case type_varargs:
      return nargs <= MAX_FUNCTION_ARGS;
    default:
      return false;
    }
}

enum runtime_error function_callable(struct obj *f, const char **errmsg,
                                     int nargs)
{
  switch (f->type)
    {
    case type_closure: {
      struct closure *cl = (struct closure *)f;
      if (code_is_vararg(cl->code))
        {
          if (nargs > MAX_FUNCTION_ARGS)
            goto wrong_parameters;
          return error_none;
        }
      if (vector_len(cl->code->arguments.argv) == nargs)
        return error_none;

      goto wrong_parameters;
    }
    case type_secure: {
      const seclev_t op_seclevel = ((struct primitive *)f)->op->seclevel;

      /* Security for Valar: disallow calling A+ secures without going through
       * mudlle code (which has its own security checks). */
      if (DEFAULT_SECLEVEL < op_seclevel)
	return error_security_violation;

      /* Security for Maiar: enforce maxseclevel if it has a meaningful
       * value. */
      if (session_context != NULL && intval(maxseclevel) < op_seclevel)
	return error_security_violation;

      /* fall through */
    }
    case type_primitive:
      if (((struct primitive *)f)->op->nargs == nargs)
	return error_none;
      goto wrong_parameters;
    case type_varargs:
      if (nargs > MAX_FUNCTION_ARGS)
        goto wrong_parameters;
      return error_none;
    default:
      abort();
    }

 wrong_parameters:
  if (errmsg)
    *errmsg = not_callable_message(nargs);
  return error_wrong_parameters;
}

void callable(value c, int nargs)
/* Effects: Causes an error if c is not something that can be called with
     nargs arguments.
*/
{
  if (!is_function(c))
    bad_typeset_error(c, TYPESET_FUNCTION);

  enum runtime_error error = function_callable(c, NULL, nargs);
  if (error != error_none)
    runtime_error(error);
}

/* if not NULL, the primitive forbidding calls */
const struct prim_op *forbid_mudlle_calls;

void fail_allow_mudlle_call(void)
{
  abort();
}

value call0(value c)
/* Effects: Calls c with no arguments
   Returns: c's result
   Requires: callable(c, 0) does not fail.
*/
{
  check_allow_mudlle_call();

  switch (((struct obj *)c)->type)
    {
    case type_closure:
      {
	struct closure *cl = c;
	if (cl->code->o.type == type_mcode)
	  return invoke0(cl);

        do_interpret(cl, 0);
        return stack_pop();
      }

    case type_secure: case type_primitive:
      {
        struct primitive *prim = c;
        return prim->op->op();
      }

    case type_varargs:
      {
        struct primitive *prim = c;
        vararg_op_fn op = prim->op->op;
	struct vector *args = UNSAFE_ALLOCATE_RECORD(vector, 0);
	return op(args, 0);
      }

    default: break;
    }
  abort();
}

#define __STACK_PUSH(N) stack_push(__PRIMNAME(N))
#define __VSET(N) args->data[DEC(N)] = __PRIMNAME(N)

#define CALL_N(N)                                                       \
value call ## N(value c, PRIMARGS ## N)                                 \
/* Effects: Calls c with arguments arg1...argN                          \
   Returns: c's result                                                  \
   Requires: callable(c, N) does not fail.                              \
*/                                                                      \
{                                                                       \
  check_allow_mudlle_call();                                            \
                                                                        \
  switch (((struct obj *)c)->type)                                      \
    {                                                                   \
    case type_closure:                                                  \
      {                                                                 \
        struct closure *cl = c;                                         \
        if (cl->code->o.type == type_mcode)                             \
          return invoke ## N(cl, PRIMARGNAMES ## N);                    \
                                                                        \
        GCPRO(PRIMARGNAMES ## N);                                       \
        stack_reserve(N);                                               \
        UNGCPRO();                                                      \
        CONCATSEMI(N, __STACK_PUSH);                                    \
        do_interpret(cl, N);                                            \
        return stack_pop();                                             \
      }                                                                 \
                                                                        \
    case type_secure: case type_primitive:                              \
      {                                                                 \
        struct primitive *prim = c;                                     \
        return prim->op->op(PRIMARGNAMES ## N);                         \
      }                                                                 \
                                                                        \
    case type_varargs:                                                  \
      {                                                                 \
        struct primitive *prim = c;                                     \
        vararg_op_fn op = prim->op->op;                                 \
        GCPRO(PRIMARGNAMES ## N);                                       \
        struct vector *args = UNSAFE_ALLOCATE_RECORD(vector, N);        \
        CONCATSEMI(N, __VSET);                                          \
        UNGCPRO();                                                      \
        return op(args, N);                                             \
      }                                                                 \
                                                                        \
    default: break;                                                     \
    }                                                                   \
  abort();                                                              \
}

DOPRIMARGS(CALL_N, SEP_EMPTY)

#define __VECT1ARG(N) (N == 1 ? arg : args->data[N - 2])
#define __V1CALLOP(N) case N: return op(CONCATCOMMA(N, __VECT1ARG))

value call1plus(value c, value arg, struct vector *args)
/* Effects: Calls c with argument arg
   Returns: c's result
   Requires: callable(c, 1 + vector_len(args)) does not fail.
   Cheat: If c is a closure, it will do the argument count check, so
     the requirement is waved (otherwise cause_event/react_event
     become painful).
*/
{
  check_allow_mudlle_call();

  int nargs = 1 + vector_len(args);
  switch (((struct obj *)c)->type)
    {
    case type_closure:
      {
	struct closure *cl = c;

	if (cl->code->o.type == type_mcode)
	  return invoke1plus(cl, arg, args);

        GCPRO(cl, args);
        stack_reserve(nargs);
        UNGCPRO();
        stack_push(arg);
        for (int i = 0; i < nargs - 1; ++i)
          stack_push(args->data[i]);

        do_interpret(cl, nargs);
        return stack_pop();
      }

    case type_secure: case type_primitive:
      {
        struct primitive *prim = c;
        value (*op)() = prim->op->op;
        switch (nargs)
          {
            DOPRIMARGS(__V1CALLOP, SEP_SEMI);
          }
        abort();
      }

    case type_varargs:
      {
	struct primitive *prim = c;
        vararg_op_fn op = prim->op->op;

        GCPRO(arg, args);
	struct vector *real_args = UNSAFE_ALLOCATE_RECORD(vector, nargs);
	real_args->data[0] = arg;
	memcpy(real_args->data + 1, args->data,
               (nargs - 1) * sizeof (value));
	UNGCPRO();

	return op(args, nargs);
      }

    default: break;
    }
  abort();
}

#define __VECTARG(N) (args->data[DEC(N)])
#define __VCALLOP(N) case N: return op(CONCATCOMMA(N, __VECTARG))

value callv(value c, struct vector *args)
/* Effects: Calls c with arguments args
   Returns: c's result
   Requires: callable(c, vector_len(args)) does not fail.
*/
{
  int nargs = vector_len(args);
  if (nargs == 0)
    return call0(c);

  check_allow_mudlle_call();

  switch (((struct obj *)c)->type)
    {
    case type_closure:
      {
	struct closure *cl = c;

	if (cl->code->o.type == type_mcode)
	  return invokev(cl, args);

        GCPRO(cl, args);
        stack_reserve(nargs);
        UNGCPRO();
        for (int i = 0; i < nargs; ++i)
          stack_push(args->data[i]);

        do_interpret(cl, nargs);
        return stack_pop();
      }

    case type_secure: case type_primitive:
      {
        struct primitive *prim = c;
        value (*op)() = prim->op->op;
        switch (nargs)
          {
            DOPRIMARGS(__VCALLOP, SEP_SEMI);
          }
        abort();
      }
    case type_varargs:
      {
        struct primitive *prim = c;
        vararg_op_fn op = prim->op->op;
        return op(args, nargs);
      }

    default: break;
    }
  abort();
}

struct call_va_info {
  value c, result;
  int nargs;
  va_list va;
  const char *name;
};

static void docall_va(void *_info)
{
  struct call_va_info *info = _info;
  assert(info->nargs > 0);

  struct {
    struct call_stack_c_header c;
    value args[MAX_PRIMITIVE_ARGS];
  } me;

  if (info->name)
    {
      me.c = (struct call_stack_c_header){
	.s = {
	  .next = call_stack,
	  .type = call_string_args,
	},
	.u.name = info->name,
	.nargs = info->nargs
      };
      call_stack = &me.c.s;
    }

  if (info->nargs > MAX_PRIMITIVE_ARGS)
    {
      GCPRO(info->c);
      me.c.nargs = 0;		/* in case there's GC */
      struct vector *argv = make_vector(info->nargs, info->va);
      me.c.s.type = call_string_argv;
      me.c.nargs = 1;
      UNGCPRO();
      me.args[0] = argv;

      callable(info->c, info->nargs);
      info->result = callv(info->c, argv);
      goto done;
    }

#define __ARG(N) me.args[DEC(N)]
#define __SETARG(N) do {                        \
    __ARG(N) = va_arg(info->va, value);         \
    if (info->nargs == N)                       \
      goto done_args;                           \
  } while (0)

  DOPRIMARGS(__SETARG, SEP_SEMI);

#undef __SETARG

  abort();

 done_args:
  callable(info->c, info->nargs);

  switch (info->nargs)
    {
#define __CALLN(N)                                                      \
      case N:                                                           \
        info->result = call ## N(info->c, CONCATCOMMA(N, __ARG));       \
        goto done
      DOPRIMARGS(__CALLN, SEP_SEMI);
#undef __CALLN
#undef __ARG
    }

 done:
  if (info->name)
    call_stack = me.c.s.next;
}

/* Calls with error trapping */

static inline enum call_trace_mode call_trace_mode(void)
{
  if (catch_context && catch_context->call_trace_mode != call_trace_barrier)
    return catch_context->call_trace_mode;
  return call_trace_on;
}

struct setjmp_data {
  value func;
  value result;
};

static void docall0_setjmp(void *_data)
{
  struct setjmp_data *data = _data;

  value f = data->func;
  GCPRO(f);
  value buf = mjmpbuf(&data->result);
  UNGCPRO();
  data->result = call1(f, buf);
}

value msetjmp(value f)
{
  struct setjmp_data data = { .func = f };
  mcatch(docall0_setjmp, &data, call_trace_mode());
  return data.result;
}

void mlongjmp(struct mjmpbuf *buf, value x)
{
  assert(is_mjmpbuf(buf));
  *buf->result = x;
  buf->result = NULL;           /* mark as target of longjmp() */
  mthrow(SIGNAL_LONGJMP, error_none);
}

void mrethrow(void)
{
  siglongjmp(catch_context->exception, 1);
}

void mthrow(enum mudlle_signal sig, enum runtime_error err)
{
  mexception = (struct mexception){ .sig = sig, .err = err };
  mrethrow();
}

void maybe_mrethrow(void)
{
  if (has_pending_exception())
    mrethrow();
}

struct call_info {
  value c, result;
  const char *name;
  struct vector *args;
};

static void docallv(void *x)
{
  struct call_info *info = x;
  callable(info->c, vector_len(info->args));
  info->result = callv(info->c, info->args);
}

static void docallv_named(void *x)
{
  struct call_info *info = x;
  struct {
    struct call_stack_c_header c;
    value args[1];
  } me = {
    .c = {
      .s = {
	.next = call_stack,
	.type = call_string_argv
      },
      .u.name = info->name,
      .nargs = 1
    },
    .args = { info->args }
  };
  call_stack = &me.c.s;
  docallv(x);
  call_stack = me.c.s.next;
}

value mcatch_callv(const char *name, value c, struct vector *arguments)
{
  struct call_info info = {
    .c = c, .args = arguments, .name = name
  };
  if (mcatch(name == NULL ? docallv : docallv_named, &info, call_trace_mode()))
    return info.result;
  return NULL;
}

static void docall_va0(void *_info)
{
  struct call_va_info *info = _info;
  callable(info->c, 0);
  info->result = call0(info->c);
}

static void docall_va0_named(void *_info)
{
  struct call_va_info *info = _info;
  struct call_stack_c_header me = {
    .s = {
      .next = call_stack,
      .type = call_string_args,
    },
    .u.name = info->name,
    .nargs  = 0
  };
  call_stack = &me.s;
  docall_va0(_info);
  call_stack = me.s.next;
}

value internal_mcatch_call0(const char *name, value c)
{
  struct call_va_info info = {
    .c     = c,
    .nargs = 0,
    .name  = name
  };
  if (mcatch(name == NULL ? docall_va0 : docall_va0_named, &info,
             call_trace_mode()))
      return info.result;
  return NULL;
}

value internal_mcatch_call(int argc, const char *name, value c, ...)
{
  struct call_va_info info = {
    .c     = c,
    .nargs = argc,
    .name  = name
  };
  va_start(info.va, c);
  bool ok = mcatch(docall_va, &info, call_trace_mode());
  va_end(info.va);
  return ok ? info.result : NULL;
}

struct call1plus_info {
  value c, result;
  value arg;
  const char *name;
  struct vector *args;
};

static void docall1plus(void *x)
{
  struct call1plus_info *info = x;
  callable(info->c, 1 + vector_len(info->args));
  info->result = call1plus(info->c, info->arg, info->args);
}

static void docall1plus_named(void *x)
{
  struct call1plus_info *info = x;
  struct {
    struct call_stack_c_header c;
    value args[2];
  } me = {
    .c = {
      .s = {
        .next = call_stack,
        .type = call_string_argv
      },
      .u.name = info->name,
      .nargs = 2,
    },
    .args = { info->arg, info->args }
  };
  call_stack = &me.c.s;
  docall1plus(x);
  call_stack = me.c.s.next;
}

value mcatch_call1plus(const char *name, value c, value arg,
                       struct vector *arguments)
{
  struct call1plus_info info = {
    .c = c, .arg = arg, .args = arguments, .name = name
  };
  if (mcatch(name == NULL ? docall1plus : docall1plus_named, &info,
             call_trace_mode()))
    return info.result;
  return NULL;
}
