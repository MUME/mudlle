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

#ifdef i386
/* The invokexxx fns are defined in x86builtins.s */
#endif

#ifdef NOCOMPILER
value invoke0(struct closure *c) { return NULL; }
#define DEF_INVOKE(N) \
value invoke ## N(struct closure *c, PRIMARGS ## N) { return NULL; }
DOPRIMARGS(DEF_INVOKE)

value invoke1plus(struct closure *c, value arg, struct vector *args)
{ return NULL; }

value invoke(struct closure *c, struct vector *args) { return NULL; }
#endif  /* NOCOMPILER */

bool minlevel_violator(value c)
{
  struct obj *o = c;

  switch (o->type)
    {
    case type_closure:
      return minlevel_violator(((struct closure *)o)->code);
    case type_code:
    case type_mcode:
      return ((struct code *)o)->seclevel < minlevel;
    default:
      return false;
    }
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
    case type_closure: {
      struct closure *cl = c;
      struct vector *args = cl->code->arg_types;
      if (args == NULL)
        return true;            /* varargs */

      assert(TYPE(args, type_vector));
      return vector_len(args) == nargs;
    }
    case type_secure:
    case type_primitive:
      return ((struct primitive *)o)->op->nargs == nargs;
    case type_varargs:
      return true;
    default:
      return false;
    }
}

void callable(value c, int nargs)
/* Effects: Causes an error if c is not something that can be called with
     nargs arguments.
*/
{
  if (!pointerp(c))
    runtime_error(error_bad_function);

  struct obj *o = c;
  switch (o->type)
    {
    case type_closure: {
      struct closure *cl = c;
      struct vector *args = cl->code->arg_types;
      if (args == NULL)         /* varargs */
        return;
      assert(TYPE(args, type_vector));
      if (vector_len(args) == nargs)
        return;

      runtime_error(error_wrong_parameters);
    }
    case type_secure: {
      const uword op_seclevel = ((struct primitive *)o)->op->seclevel;

      /* Security for Valar: disallow calling A+ secures without going through
       * mudlle code (which has its own security checks). */
      if (DEFAULT_SECLEVEL < op_seclevel)
	runtime_error(error_security_violation);

      /* Security for Maiar: enforce maxseclevel if it has a meaningful
       * value. */
      if (session_context != NULL && intval(maxseclevel) < op_seclevel)
	runtime_error(error_security_violation);

      /* fall through */
    }
    case type_primitive:
      if (((struct primitive *)o)->op->nargs == nargs)
	return;
      runtime_error(error_wrong_parameters);
    case type_varargs:
      return;
    default:
      break;
    }
  runtime_error(error_bad_function);
}

/* if not NULL, is name of primitive forbidding calls */
const char *forbid_mudlle_calls;

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
        ++prim->call_count;
        return prim->op->op();
      }

    case type_varargs:
      {
        struct primitive *prim = c;
        ++prim->call_count;
        value (*op)() = prim->op->op;
	struct vector *args = UNSAFE_ALLOCATE_RECORD(vector, 0);
	return op(args, 0);
      }

    default: break;
    }
  abort();
}

#define __STACK_PUSH(N) stack_push(__PRIMNAME(N))
#define __VSET(N) args->data[N - 1] = __PRIMNAME(N)

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
        GCPRO_N(N, PRIMARGNAMES ## N);                                  \
        CONCATSEMI(N, __STACK_PUSH);                                    \
        UNGCPRO();                                                      \
        do_interpret(cl, N);                                            \
        return stack_pop();                                             \
      }                                                                 \
                                                                        \
    case type_secure: case type_primitive:                              \
      {                                                                 \
        struct primitive *prim = c;                                     \
        ++prim->call_count;                                             \
        return prim->op->op(PRIMARGNAMES ## N);                         \
      }                                                                 \
                                                                        \
    case type_varargs:                                                  \
      {                                                                 \
        struct primitive *prim = c;                                     \
        ++prim->call_count;                                             \
        value (*op)() = prim->op->op;                                   \
        GCPRO_N(N, PRIMARGNAMES ## N);                                  \
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

DOPRIMARGS(CALL_N)

#define __VECT1ARG(N) (N == 1 ? arg : args->data[N - 2])
#define __V1CALLOP(N) case N: return op(CONCATCOMMA(N, __VECT1ARG));

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

        GCPRO2(cl, args);
        stack_push(arg);
        for (int i = 0; i < nargs - 1; ++i)
          stack_push(args->data[i]);
        UNGCPRO();

        do_interpret(cl, nargs);
        return stack_pop();
      }

    case type_secure: case type_primitive:
      {
        struct primitive *prim = c;
        ++prim->call_count;
        value (*op)() = prim->op->op;
        switch (nargs)
          {
            DOPRIMARGS(__V1CALLOP)
          }
        abort();
      }

    case type_varargs:
      {
	struct primitive *prim = c;
        ++prim->call_count;
        value (*op)() = prim->op->op;

        GCPRO2(arg, args);
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

#define __VECTARG(N) (args->data[N - 1])
#define __VCALLOP(N) case N: return op(CONCATCOMMA(N, __VECTARG));

value call(value c, struct vector *args)
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
	  return invoke(cl, args);

        GCPRO2(cl, args);
        for (int i = 0; i < nargs; ++i)
          stack_push(args->data[i]);
        UNGCPRO();

        do_interpret(cl, nargs);
        return stack_pop();
      }

    case type_secure: case type_primitive:
      {
        struct primitive *prim = c;
        ++prim->call_count;
        value (*op)() = prim->op->op;
        switch (nargs)
          {
            DOPRIMARGS(__VCALLOP)
          }
        abort();
      }
    case type_varargs:
      {
        struct primitive *prim = c;
        ++prim->call_count;
        return prim->op->op(args, nargs);
      }

    default: break;
    }
  abort();
}

static struct vector *make_vargs(int argc, va_list vargs)
{
  if (argc == 0)
    return UNSAFE_ALLOCATE_RECORD(vector, 0);

  struct gcpro gcpros[argc];
  value args[argc];
  for (int i = 0; i < argc; ++i)
    {
      args[i] = va_arg(vargs, value);
      GCPRO(gcpros[i], args[i]);
    }
  struct vector *v = UNSAFE_ALLOCATE_RECORD(vector, argc);
  UNGCPRO1(gcpros[0]);
  memcpy(v->data, args, sizeof args);
  return v;
}

#define __VARG(N) __PRIMARG(N) = me.args[N - 1] = va_arg(va, value)
#define __INVOKE(N)                             \
 case N:                                        \
 {                                              \
   CONCATSEMI(N, __VARG);                       \
   result = invoke ## N(cl, PRIMARGNAMES ## N); \
   goto done;                                   \
 }
#define __CALLOP(N)                             \
 case N:                                        \
 {                                              \
   CONCATSEMI(N, __VARG);                       \
   result = op(PRIMARGNAMES ## N);              \
   goto done;                                   \
 }

static value callv(value c, int nargs, va_list va, const char *name)

/* Effects: Calls c with argc arguments in va
   Returns: c's result
   Requires: callable(c, argc) does not fail.
*/
{
  if (nargs == 0)
    {
      if (name == NULL)
        return call0(c);

      struct call_stack_c_header me = {
        .s = {
          .next = call_stack,
          .type = call_string,
        },
        .u.name = name,
        .nargs  = 0
      };
      call_stack = &me.s;
      value result = call0(c);
      call_stack = me.s.next;
      return result;
    }

  bool use_vector = nargs > MAX_PRIMITIVE_ARGS;
  struct {
    struct call_stack_c_header c;
    value args[MAX_PRIMITIVE_ARGS];
  } me;

  if (name)
    {
      me.c = (struct call_stack_c_header){
	.s = {
	  .next = call_stack,
	  .type = call_string,
	},
	.u.name = name,
	.nargs = nargs
      };
      call_stack = &me.c.s;
    }

  value result;
  if (use_vector)
    goto call_vector;

  check_allow_mudlle_call();

  switch (((struct obj *)c)->type)
    {
    case type_closure:
      {
	struct closure *cl = c;

	if (cl->code->o.type != type_mcode)
          goto call_vector;
        switch (nargs)
          {
            DOPRIMARGS(__INVOKE)
          }
        abort();
      }

    case type_secure: case type_primitive:
      {
        struct primitive *prim = c;
        ++prim->call_count;
        value (*op)() = prim->op->op;
        switch (nargs)
          {
            DOPRIMARGS(__CALLOP)
          }
        abort();
      }
    case type_varargs:
      goto call_vector;

    default:
      abort();
    }

 call_vector: ;
  GCPRO1(c);
  me.c.nargs = 0;		/* in case there's GC */
  struct vector *argv = make_vargs(nargs, va);
  UNGCPRO();
  me.c.nargs = 1;
  me.args[0] = argv;
  result = call(c, argv);

 done:
  if (name)
    call_stack = me.c.s.next;
  return result;
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
  GCPRO1(f);
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
  nosiglongjmp(catch_context->exception, 1);
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

static void docall(void *x)
{
  struct call_info *info = x;
  if (info->name == NULL)
    {
      info->result = call(info->c, info->args);
      return;
    }
  struct {
    struct call_stack_c_header c;
    value args[1];
  } me = {
    .c = {
      .s = {
	.next = call_stack,
	.type = call_string
      },
      .u.name = info->name,
      .nargs = 1
    },
    .args = { info->args }
  };
  call_stack = &me.c.s;
  info->result = call(info->c, info->args);
  call_stack = me.c.s.next;
}

value mcatch_call(const char *name, value c, struct vector *arguments)
{
  struct call_info info = {
    .c = c, .args = arguments, .name = name
  };
  if (mcatch(docall, &info, call_trace_mode()))
    return info.result;
  return NULL;
}

struct callv_info {
  value c, result;
  int argc;
  va_list va;
  const char *name;
};

static void docallv(void *_info)
{
  struct callv_info *info = _info;
  info->result = callv(info->c, info->argc, info->va, info->name);
}

value mcatchv(const char *name, value c, int argc, ...)
{
  struct callv_info info = {
    .c = c,
    .argc = argc,
    .name = name
  };
  va_start(info.va, argc);
  bool ok = mcatch(docallv, &info, call_trace_mode());
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
  if (info->name == NULL)
    {
      info->result = call1plus(info->c, info->arg, info->args);
      return;
    }
  struct {
    struct call_stack_c_header c;
    value args[2];
  } me = {
    .c = {
      .s = {
        .next = call_stack,
        .type = call_string
      },
      .u.name = info->name,
      .nargs = 2,
    },
    .args = { info->arg, info->args }
  };
  call_stack = &me.c.s;
  info->result = call1plus(info->c, info->arg, info->args);
  call_stack = me.c.s.next;
}

value mcatch_call1plus(const char *name, value c, value arg,
                       struct vector *arguments)
{
  struct call1plus_info info = {
    .c = c, .arg = arg, .args = arguments, .name = name
  };
  if (mcatch(docall1plus, &info, call_trace_mode())) return info.result;
  else return NULL;
}
