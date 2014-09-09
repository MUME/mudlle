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

#ifdef AMIGA

inline value invoke0(struct closure *c)
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c()
   Returns: c()'s result
*/
{
  value result;
  GCPRO1(c);
  push_registers();
  UNGCPRO();
  result = mc_invoke(c, 0, NULL, NULL, NULL, NULL);
  pop_registers();
  return result;
}

inline value invoke1(struct closure *c, value arg)
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c(arg)
   Returns: c(arg)'s result
*/
{
  GCPRO2(arg, c);
  push_registers();
  UNGCPRO();

  value result = mc_invoke(c, 1, arg, NULL, NULL, NULL);
  pop_registers();
  return result;
}

inline value invoke1plus(struct closure *c, value arg, struct vector *args)
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c(args)
   Returns: c(args)'s result
*/
{
  struct vector *extra;
  value result;
  int nargs;

  {
    GCPRO3(arg, args, c);
    push_registers();
    UNGCPRO();
  }

  nargs = 1 + vector_len(args);
  switch (nargs)
    {
    case 1:
      result = mc_invoke(c, 1, arg, NULL, NULL, NULL);
      break;
    case 2:
      result = mc_invoke(c, 2, arg, args->data[0], NULL, NULL);
      break;
    case 3:
      result = mc_invoke(c, 3, arg, args->data[0], args->data[1], NULL);
      break;
    case 4:
      result = mc_invoke(c, 4, arg, args->data[0], args->data[1], args->data[2]);
      break;
    default:
      {
        GCPRO3(arg, args, c);
        extra = (struct vector *)unsafe_allocate_record(type_internal, nargs - 3);
        UNGCPRO();
      }
      memcpy(extra->data, args->data + 2, (nargs - 3) * sizeof(value));
      result = mc_invoke(c, 4, arg, args->data[0], args->data[1], extra);
      break;
    }
  pop_registers();
  return result;
}

inline value invoke(struct closure *c, struct vector *args)
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c(args)
   Returns: c(args)'s result
*/
{
  struct vector *extra;
  value result;
  int nargs;

  {
    GCPRO2(c, args);
    push_registers();
    UNGCPRO();
  }

  nargs = vector_len(args);
  switch (nargs)
    {
    case 0:
      result = mc_invoke(c, 0, NULL, NULL, NULL, NULL);
      break;
    case 1:
      result = mc_invoke(c, 1, args->data[0], NULL, NULL, NULL);
      break;
    case 2:
      result = mc_invoke(c, 2, args->data[0], args->data[1], NULL, NULL);
      break;
    case 3:
      result = mc_invoke(c, 3, args->data[0], args->data[1], args->data[2], NULL);
      break;
    case 4:
      result = mc_invoke(c, 4, args->data[0], args->data[1], args->data[2], args->data[3]);
      break;
    default:
      {
        GCPRO2(c, args);
        extra = (struct vector *)unsafe_allocate_record(type_internal, nargs - 3);
        UNGCPRO();
        memcpy(extra->data, args->data + 3, (nargs - 3) * sizeof(value));
        result = mc_invoke(c, 4, args->data[0], args->data[1], args->data[2], extra);
        break;
      }
    }
  pop_registers();
  return result;
}

#endif

#ifdef sparc

inline value invoke0(struct closure *c)
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c()
   Returns: c()'s result
*/
{
  return mc_invoke(NULL, NULL, NULL, NULL, NULL, c, 0);
}

inline value invoke1(struct closure *c, value arg)
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c(arg)
   Returns: c(arg)'s result
*/
{
  return mc_invoke(arg, NULL, NULL, NULL, NULL, c, 1);
}

inline value invoke2(struct closure *c, value arg1, value arg2)
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c(arg1, arg2)
   Returns: c()'s result
*/
{
  return mc_invoke(arg1, arg2, NULL, NULL, NULL, c, 2);
}

inline value invoke3(struct closure *c, value arg1, value arg2, value arg3)
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c(arg1, arg2, arg3)
   Returns: c()'s result
*/
{
  return mc_invoke(arg1, arg2, arg3, NULL, NULL, c, 3);
}

inline value invoke4(struct closure *c, value arg1, value arg2, value arg3,
		     value arg4)
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c(arg1, arg2, arg3, arg4)
   Returns: c()'s result
*/
{
  return mc_invoke(arg1, arg2, arg3, arg4, NULL, c, 4);
}

inline value invoke1plus(struct closure *c, value arg, struct vector *args)
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c(args)
   Returns: c(args)'s result
*/
{
  int nargs = 1 + vector_len(args);

  CASSERT_EXPR(MAX_PRIMITIVE_ARGS == 5);
  switch (nargs)
    {
    case 1:
      return mc_invoke(arg, NULL, NULL, NULL, NULL, c, 1);
    case 2:
      return mc_invoke(arg, args->data[0], NULL, NULL, NULL, c, 2);
    case 3:
      return mc_invoke(arg, args->data[0], args->data[1], NULL, NULL, c, 3);
    case 4:
      return mc_invoke(arg, args->data[0], args->data[1], args->data[2], NULL, c, 4);
    case 5:
      return mc_invoke(arg, args->data[0], args->data[1], args->data[2],
		       args->data[3], c, 5);
    default:
      return mc_invoke_vector(arg, args->data[0], args->data[1], args->data[2],
			      args->data[3], c, nargs, args, 4);
    }
}

inline value invoke(struct closure *c, struct vector *args)
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c(args)
   Returns: c(args)'s result
*/
{
  int nargs = vector_len(args);

  CASSERT_EXPR(MAX_PRIMITIVE_ARGS == 5);
  switch (nargs)
    {
    case 0:
      return mc_invoke(NULL, NULL, NULL, NULL, NULL, c, 0);
    case 1:
      return mc_invoke(args->data[0], NULL, NULL, NULL, NULL, c, 1);
    case 2:
      return mc_invoke(args->data[0], args->data[1], NULL, NULL, NULL, c, 2);
    case 3:
      return mc_invoke(args->data[0], args->data[1], args->data[2], NULL, NULL, c, 3);
    case 4:
      return mc_invoke(args->data[0], args->data[1], args->data[2],
		       args->data[3], NULL, c, 4);
    case 5:
      return mc_invoke(args->data[0], args->data[1], args->data[2],
		       args->data[3], args->data[4], c, 5);
    default:
      return mc_invoke_vector(args->data[0], args->data[1], args->data[2],
			      args->data[3], args->data[4], c, nargs, args, 5);
    }
}

#endif

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
	struct vector *args
          = (struct vector *)unsafe_allocate_record(type_vector, 0);
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
        struct vector *args                                             \
          = (struct vector *)unsafe_allocate_record(type_vector, N);    \
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
	struct vector *real_args
          = (struct vector *)unsafe_allocate_record(type_vector, nargs);
	real_args->data[0] = arg;
	memcpy(real_args->data + 1, args->data, (nargs - 1) * sizeof (value));
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
    return (struct vector *)unsafe_allocate_record(type_vector, 0);

  struct gcpro gcpros[argc];
  value args[argc];
  for (int i = 0; i < argc; ++i)
    {
      args[i] = va_arg(vargs, value);
      GCPRO(gcpros[i], args[i]);
    }
  struct vector *v
    = (struct vector *)unsafe_allocate_record(type_vector, argc);
  UNGCPRO1(gcpros[0]);
  memcpy(v->data, args, sizeof args);
  return v;
}

#define __VARG(N) __PRIMARG(N) = me.u.c.args[N - 1] = va_arg(va, value)
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
  struct call_stack me;
  me.next = call_stack;

  if (name != NULL)
    {
      me.type = call_string;
      me.u.c.u.name = name;
      me.u.c.nargs = nargs;
      call_stack = &me;
    }

  value result;

  if (nargs == 0)
    {
      result = call0(c);
      goto done;
    }

  if (nargs > MAX_PRIMITIVE_ARGS)
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
  /* initiate data in case there is a GC */
  me.u.c.nargs = 1;
  me.u.c.args[0] = NULL;
  GCPRO1(c);
  struct vector *argv = make_vargs(nargs, va);
  UNGCPRO();
  me.u.c.args[0] = argv;
  result = call(c, argv);

 done:
  call_stack = me.next;
  return result;
}

/* Calls with error trapping */

static inline enum call_trace_mode call_trace_mode(void)
{
  if (catch_context && catch_context->call_trace_mode != call_trace_barrier)
    return catch_context->call_trace_mode;
  return call_trace_on;
}

static value longjmp_result;

static void docall0_setjmp(void *f)
{
  value buf;

  GCPRO1(f);
  buf = mjmpbuf();
  longjmp_result = call1(f, buf);
  UNGCPRO();
}

value msetjmp(value f)
{
  if (mcatch(docall0_setjmp, f, call_trace_mode()))
    return longjmp_result;
  return NULL;
}

void mlongjmp(value buf, value x)
{
  assert(is_mjmpbuf(buf));

  exception_context = ((struct mjmpbuf *)buf)->context;
  longjmp_result = x;
  mthrow(SIGNAL_LONGJMP, x);
}

void mthrow(long sig, value val)
{
  exception_signal = sig;
  exception_value = val;
  nosiglongjmp(catch_context->exception, sig);
}

struct call_info {
  value c, result;
  const char *name;
  struct vector *args;
};

static void docall(void *x)
{
  struct call_info *info = x;
  struct call_stack me;
  me.next = call_stack;
  if (info->name)
    {
      me.type = call_string;
      me.u.c.u.name = info->name;
      me.u.c.nargs = 1;
      me.u.c.args[0] = info->args;
      call_stack = &me;
    }
  info->result = call(info->c, info->args);
  call_stack = me.next;
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
  struct call_stack me;
  me.next = call_stack;
  if (info->name)
    {
      me.type = call_string;
      me.u.c.u.name = info->name;
      me.u.c.nargs = 2;
      me.u.c.args[0] = info->arg;
      me.u.c.args[1] = info->args;
      call_stack = &me;
    }
  info->result = call1plus(info->c, info->arg, info->args);
  call_stack = me.next;
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
