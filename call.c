/*
 * Copyright (c) 1993-2006 David Gay and Gustav Hållberg
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
#include <stdio.h>

#include "alloc.h"
#include "builtins.h"
#include "interpret.h"
#include "error.h"
#include "stack.h"
#include "call.h"
#include "context.h"

/* Interface to machine code. */

#ifdef AMIGA

INLINE value invoke0(struct closure *c)
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c()
   Returns: c()'s result
*/
{
  value result;
  struct gcpro gcpro1;

  GCPRO1(c);
  push_registers();
  UNGCPRO();
  result = mc_invoke(c, 0, NULL, NULL, NULL, NULL);
  pop_registers();
  return result;
}

INLINE value invoke1(struct closure *c, value arg)
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c(arg)
   Returns: c(arg)'s result
*/
{
  struct gcpro gcpro1, gcpro2;
  value result;

  GCPRO2(arg, c);
  push_registers();
  UNGCPRO();

  result = mc_invoke(c, 1, arg, NULL, NULL, NULL);
  pop_registers();
  return result;
}

INLINE value invoke1plus(struct closure *c, value arg, struct vector *args)
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c(args)
   Returns: c(args)'s result
*/
{
  struct gcpro gcpro1, gcpro2, gcpro3;
  struct vector *extra;
  value result;
  int nargs;

  GCPRO2(arg, args); GCPRO(gcpro3, c);
  push_registers();
  UNGCPRO();

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
      GCPRO2(arg, args); GCPRO(gcpro3, c);
      extra = (struct vector *)unsafe_allocate_record(type_internal, nargs - 3);
      UNGCPRO();
      memcpy(extra->data, args->data + 2, (nargs - 3) * sizeof(value));
      result = mc_invoke(c, 4, arg, args->data[0], args->data[1], extra);
      break;
    }
  pop_registers();
  return result;
}

INLINE value invoke(struct closure *c, struct vector *args)
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c(args)
   Returns: c(args)'s result
*/
{
  struct gcpro gcpro1, gcpro2;
  struct vector *extra;
  value result;
  int nargs;

  GCPRO2(c, args);
  push_registers();
  UNGCPRO();

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
      GCPRO2(c, args);
      extra = (struct vector *)unsafe_allocate_record(type_internal, nargs - 3);
      UNGCPRO();
      memcpy(extra->data, args->data + 3, (nargs - 3) * sizeof(value));
      result = mc_invoke(c, 4, args->data[0], args->data[1], args->data[2], extra);
      break;
    }
  pop_registers();
  return result;
}

#endif

#ifdef sparc

INLINE value invoke0(struct closure *c)
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c()
   Returns: c()'s result
*/
{
  return mc_invoke(NULL, NULL, NULL, NULL, NULL, c, 0);
}

INLINE value invoke1(struct closure *c, value arg)
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c(arg)
   Returns: c(arg)'s result
*/
{
  return mc_invoke(arg, NULL, NULL, NULL, NULL, c, 1);
}

INLINE value invoke2(struct closure *c, value arg1, value arg2)
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c(arg1, arg2)
   Returns: c()'s result
*/
{
  return mc_invoke(arg1, arg2, NULL, NULL, NULL, c, 2);
}

INLINE value invoke3(struct closure *c, value arg1, value arg2, value arg3)
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c(arg1, arg2, arg3)
   Returns: c()'s result
*/
{
  return mc_invoke(arg1, arg2, arg3, NULL, NULL, c, 3);
}

INLINE value invoke4(struct closure *c, value arg1, value arg2, value arg3,
		     value arg4)
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c(arg1, arg2, arg3, arg4)
   Returns: c()'s result
*/
{
  return mc_invoke(arg1, arg2, arg3, arg4, NULL, c, 4);
}

INLINE value invoke1plus(struct closure *c, value arg, struct vector *args)
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c(args)
   Returns: c(args)'s result
*/
{
  int nargs = 1 + vector_len(args);

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

INLINE value invoke(struct closure *c, struct vector *args)
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c(args)
   Returns: c(args)'s result
*/
{
  int nargs = vector_len(args);

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
INLINE value invoke0(struct closure *c)
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c()
   Returns: c()'s result
*/
{
  return NULL;
}

INLINE value invoke1(struct closure *c, value arg)
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c(arg)
   Returns: c(arg)'s result
*/
{
  return NULL;
}

INLINE value invoke2(struct closure *c, value arg1, value arg2)
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c(arg1, arg2)
   Returns: c()'s result
*/
{
  return NULL;
}

INLINE value invoke3(struct closure *c, value arg1, value arg2, value arg3)
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c(arg1, arg2, arg3)
   Returns: c()'s result
*/
{
  return NULL;
}

INLINE value invoke4(struct closure *c, value arg1, value arg2, value arg3, value arg4)
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c(arg1, arg2, arg3, arg4)
   Returns: c()'s result
*/
{
  return NULL;
}

INLINE value invoke1plus(struct closure *c, value arg, struct vector *args)
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c(args)
   Returns: c(args)'s result
*/
{
  return NULL;
}

INLINE value invoke(struct closure *c, struct vector *args)
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c(args)
   Returns: c(args)'s result
*/
{
  return NULL;
}
#endif

int seclevel_violator(value c)
{
  struct obj *o = c;

  switch (o->type)
    {
    case type_closure:
      return seclevel_violator(((struct closure *)o)->code);
    case type_code:
      return ((struct code *)o)->seclevel < minlevel;
    case type_mcode:
      return ((struct mcode *)o)->seclevel < minlevel;
    default:
      return false;
    }
}

int callablep(value c, int nargs)
/* Returns: false if c is not something that can be called with
     nargs arguments.
*/
{
  struct obj *o = c;

  if (!pointerp(c))
    return false;

  switch (o->type)
    {
    case type_closure: return true;
    case type_secure:
    case type_primitive: 
      if (nargs < 0 || ((struct primitive *)o)->op->nargs == nargs)
	return true;
      return false;
    case type_varargs: return true;
    default:
      break;
    }
  return false;
}

void callable(value c, int nargs)
/* Effects: Causes an error if c is not something that can be called with
     nargs arguments (-1 only checks type of c).
*/
{
  struct obj *o = c;

  if (!pointerp(c))
    runtime_error(error_bad_function);

  switch (o->type)
    {
    case type_closure: return;
    case type_secure:
      if (DEFAULT_SECLEVEL < ((struct primitive *)o)->op->seclevel)
	runtime_error(error_security_violation);
      /* fall through */
    case type_primitive: 
      if (nargs < 0 || ((struct primitive *)o)->op->nargs == nargs)
	return;
      runtime_error(error_wrong_parameters);
    case type_varargs: return;
    default:
      break;
    }
  runtime_error(error_bad_function);
}

value call0(value c)
/* Effects: Calls c with no arguments
   Returns: c's result
   Requires: callable(c, 0) does not fail.
*/
{
  struct obj *o = c;
  value result;

  /* Start with important case, explicitly */
  if (o->type == type_closure && ((struct closure *)o)->code->o.type == type_mcode)
    return invoke0(c);
  else
    {
      switch (o->type)
	{
	case type_closure:
	  do_interpret(c, 0);
	  return stack_pop();

	case type_secure: case type_primitive:
	  ((struct primitive *)o)->call_count++;
	  result = ((struct primitive *)o)->op->op();
	  return result;

	case type_varargs:
	  {
	    struct vector *args;

	    ((struct primitive *)o)->call_count++;
	    args = (struct vector *)unsafe_allocate_record(type_vector, 0);
	    result = ((struct primitive *)o)->op->op(args, 0);
	    return result;
	  }
	default: break;
	}
    }
  abort();
}

value call1(value c, value arg)
/* Effects: Calls c with argument arg
   Returns: c's result
   Requires: callable(c, 1) does not fail.
*/
{
  struct obj *o = c;
  struct gcpro gcpro1;
  value result;

  switch (o->type)
    {
    case type_closure:
      {
	struct closure *cl = (struct closure *)o;

	if (cl->code->o.type == type_mcode)
	  return invoke1(cl, arg);
	else
	  {
	    GCPRO1(cl);
	    stack_push(arg);
	    UNGCPRO();
	    do_interpret(cl, 1);
	    return stack_pop();
	  }
      }

    case type_secure: case type_primitive:
      ((struct primitive *)o)->call_count++;
      result = ((struct primitive *)o)->op->op(arg);
      return result;

    case type_varargs:
      {
	struct vector *args;

	((struct primitive *)o)->call_count++;
	GCPRO1(arg);
	args = (struct vector *)unsafe_allocate_record(type_vector, 1);
	args->data[0] = arg;
	UNGCPRO();
	result = ((struct primitive *)o)->op->op(args, 1);
      }
    default: break;
    }
  abort();
}

value call2(value c, value arg1, value arg2)
/* Effects: Calls c with arguments arg1, arg2
   Returns: c's result
   Requires: callable(c, 2) does not fail.
*/
{
  struct obj *o = c;
  struct gcpro gcpro1, gcpro2;
  value result;

  switch (o->type)
    {
    case type_closure:
      {
	struct closure *cl = (struct closure *)o;

	if (cl->code->o.type == type_mcode)
	  return invoke2(cl, arg1, arg2);
	else
	  {
	    GCPRO2(cl, arg2);
	    stack_push(arg1);
	    stack_push(arg2);
	    UNGCPRO();
	    do_interpret(cl, 2);
	    return stack_pop();
	  }
      }

    case type_secure: case type_primitive:
      ((struct primitive *)o)->call_count++;
      result = ((struct primitive *)o)->op->op(arg1, arg2);
      return result;

    case type_varargs:
      {
	struct vector *args;

	((struct primitive *)o)->call_count++;
	GCPRO2(arg1, arg2);
	args = (struct vector *)unsafe_allocate_record(type_vector, 2);
	args->data[0] = arg1;
	args->data[1] = arg2;
	UNGCPRO();
	result = ((struct primitive *)o)->op->op(args, 2);
	return result;
      }
    default: break;
    }
  abort();
}

value call3(value c, value arg1, value arg2, value arg3)
/* Effects: Calls c with arguments arg1, arg2, arg3
   Returns: c's result
   Requires: callable(c, 3) does not fail.
*/
{
  struct obj *o = c;
  struct gcpro gcpro1, gcpro2, gcpro3;
  value result;

  switch (o->type)
    {
    case type_closure:
      {
	struct closure *cl = (struct closure *)o;

	if (cl->code->o.type == type_mcode)
	  return invoke3(cl, arg1, arg2, arg3);
	else
	  {
	    GCPRO2(cl, arg2); GCPRO(gcpro3, arg3);
	    stack_push(arg1);
	    stack_push(arg2);
	    stack_push(arg3);
	    UNGCPRO();
	    do_interpret(cl, 3);
	    return stack_pop();
	  }
      }

    case type_secure: case type_primitive:
      ((struct primitive *)o)->call_count++;
      result = ((struct primitive *)o)->op->op(arg1, arg2, arg3);
      return result;

    case type_varargs:
      {
	struct vector *args;

	((struct primitive *)o)->call_count++;
	GCPRO2(arg1, arg2); GCPRO(gcpro3, arg3);
	args = (struct vector *)unsafe_allocate_record(type_vector, 3);
	args->data[0] = arg1;
	args->data[1] = arg2;
	args->data[2] = arg3;
	UNGCPRO();
	result = ((struct primitive *)o)->op->op(args, 3);
      }
    default: break;
    }
  abort();
}

value call4(value c, value arg1, value arg2, value arg3, value arg4)
/* Effects: Calls c with arguments arg1, arg2, arg3, arg4
   Returns: c's result
   Requires: callable(c, 4) does not fail.
*/
{
  struct obj *o = c;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
  value result;

  switch (o->type)
    {
    case type_closure:
      {
	struct closure *cl = (struct closure *)o;

	if (cl->code->o.type == type_mcode)
	  return invoke4(cl, arg1, arg2, arg3, arg4);
	else
	  {
	    GCPRO2(cl, arg2); GCPRO(gcpro3, arg3); GCPRO(gcpro4, arg4);
	    stack_push(arg1);
	    stack_push(arg2);
	    stack_push(arg3);
	    stack_push(arg4);
	    UNGCPRO();
	    do_interpret(cl, 4);
	    return stack_pop();
	  }
      }

    case type_secure: case type_primitive:
      ((struct primitive *)o)->call_count++;
      result = ((struct primitive *)o)->op->op(arg1, arg2, arg3, arg4);
      return result;

    case type_varargs:
      {
	struct vector *args;

	((struct primitive *)o)->call_count++;
	GCPRO2(arg1, arg2); GCPRO(gcpro3, arg3);
	args = (struct vector *)unsafe_allocate_record(type_vector, 4);
	args->data[0] = arg1;
	args->data[1] = arg2;
	args->data[2] = arg3;
	args->data[3] = arg4;
	UNGCPRO();
	result = ((struct primitive *)o)->op->op(args, 4);
      }
    default: break;
    }
  abort();
}

value call1plus(value c, value arg, struct vector *args)
/* Effects: Calls c with argument arg
   Returns: c's result
   Requires: callable(c, 1 + vector_len(args)) does not fail.
   Cheat: If c is a closure, it will do the argument count check, so
     the requirement is waved (otherwise cause_event/react_event
     become painful).
*/
{
  struct obj *o = c;
  value result = NULL;

  int nargs = 1 + vector_len(args);
  switch (o->type)
    {
    case type_closure:
      {
	struct closure *cl = (struct closure *)o;

	if (cl->code->o.type == type_mcode)
	  return invoke1plus(cl, arg, args);
	else
	  {
            struct gcpro gcpro1, gcpro2;
	    GCPRO2(cl, args);
	    stack_push(arg);
	    for (int i = 0; i < nargs - 1; i++) stack_push(args->data[i]);
	    UNGCPRO();

	    do_interpret(cl, nargs);
	    return stack_pop();
	  }
      }

    case type_secure: case type_primitive:
      {
        struct primitive *prim = (struct primitive *)o;
        prim->call_count++;
        const struct primitive_ext *const op = prim->op;
      switch (nargs)
	{
	case 1:
	  result = op->op(arg);
	  break;
	case 2:
	  result = op->op(arg, args->data[0]);
	  break;
	case 3:
	  result = op->op(arg, args->data[0], args->data[1]);
	  break;
	case 4:
	  result = op->op(arg, args->data[0], args->data[1], args->data[2]);
	  break;
	case 5:
	  result = op->op(arg, args->data[0], args->data[1], args->data[2],
			  args->data[3]);
	  break;
	default:
	  assert(0);
	}
      return result;
      }

    case type_varargs:
      {
	struct vector *real_args;
	struct primitive *prim = (struct primitive *)o;

        prim->call_count++;

        struct gcpro gcpro1;
	GCPRO1(arg);
	real_args = (struct vector *)unsafe_allocate_record(type_vector, nargs);
	real_args->data[0] = arg;
	memcpy(real_args->data + 1, args->data, (nargs - 1) * sizeof(value));
	UNGCPRO();

	return prim->op->op(args, nargs);
      }
    default: break;
    }
  abort();
}

value call(value c, struct vector *args)
/* Effects: Calls c with arguments args
   Returns: c's result
   Requires: callable(c, vector_len(args)) does not fail.
*/
{
  struct obj *o = c;
  int nargs = vector_len(args);

  switch (o->type)
    {
    case type_closure:
      {
	struct closure *cl = (struct closure *)o;

	if (cl->code->o.type == type_mcode)
	  return invoke(cl, args);
	else
	  {
            struct gcpro gcpro1, gcpro2;
	    GCPRO2(cl, args);
	    for (int i = 0; i < nargs; i++) stack_push(args->data[i]);
	    UNGCPRO();

	    do_interpret(cl, nargs);
	    return stack_pop();
	  }
      }

    case type_secure: case type_primitive:
      {
        struct primitive *prim = (struct primitive *)o;
        prim->call_count++;
        const struct primitive_ext *op = prim->op;
        value result;

      switch (nargs)
	{
	case 0:
	  result = op->op();
	  break;
	case 1:
	  result = op->op(args->data[0]);
	  break;
	case 2:
	  result = op->op(args->data[0], args->data[1]);
	  break;
	case 3:
	  result = op->op(args->data[0], args->data[1], args->data[2]);
	  break;
	case 4:
	  result = op->op(args->data[0], args->data[1], args->data[2],
                          args->data[3]);
	  break;
	case 5:
	  result = op->op(args->data[0], args->data[1], args->data[2],
                          args->data[3], args->data[4]);
	  break;
	default:
	  assert(0);
	}
      return result;
      }
    case type_varargs:
      {
        struct primitive *prim = (struct primitive *)o;
        prim->call_count++;
        return prim->op->op(args, nargs);
      }

    default: break;
    }
  abort();
}

/* Calls with error trapping */

static INLINE enum call_trace_mode call_trace_mode(void)
{
  if (catch_context)
    return catch_context->call_trace_mode;
  return call_trace_on;
}

struct val5 { value v1, v2, v3, v4, v5; };
static value result;

static void docall0_setjmp(void *f)
{
  struct gcpro gcpro1;
  value buf;

  GCPRO1(f);
  buf = mjmpbuf();
  result = call1(f, buf);
  UNGCPRO();
}

value msetjmp(value f)
{
  if (mcatch(docall0_setjmp, f, catch_context->call_trace_mode)) 
    return result;
  return NULL;
}

void mlongjmp(value buf, value x)
{
  assert(is_mjmpbuf(buf));
  
  exception_context = ((struct mjmpbuf *)buf)->context;
  result = x;
  mthrow(SIGNAL_LONGJMP, x);
}

void mthrow(long sig, value val)
{
  exception_signal = sig;
  exception_value = val;
  nosiglongjmp(catch_context->exception, sig);
}

static void docall(void *x)
{
  struct val5 *args = x;

  result = call(args->v1, args->v2);
}

value mcatch_call(value c, struct vector *arguments)
{
  struct val5 args;

  args.v1 = c; args.v2 = arguments;
  if (mcatch(docall, &args, call_trace_mode())) return result;
  else return NULL;
}

static void docall0(void *x)
{
  result = call0(x);
}

value mcatch_call0(value c)
{
  if (mcatch(docall0, c, call_trace_mode())) return result;
  else return NULL;
}

static void docall1(void *x)
{
  struct val5 *args = x;

  result = call1(args->v1, args->v2);
}

value mcatch_call1(value c, value arg)
{
  struct val5 args;

  args.v1 = c; args.v2 = arg;
  if (mcatch(docall1, &args, call_trace_mode())) return result;
  else return NULL;
}

static void docall2(void *x)
{
  struct val5 *args = x;

  result = call2(args->v1, args->v2, args->v3);
}

value mcatch_call2(value c, value arg1, value arg2)
{
  struct val5 args;

  args.v1 = c; args.v2 = arg1; args.v3 = arg2;
  if (mcatch(docall2, &args, call_trace_mode())) return result;
  else return NULL;
}

static void docall3(void *x)
{
  struct val5 *args = x;

  result = call3(args->v1, args->v2, args->v3, args->v4);
}

value mcatch_call3(value c, value arg1, value arg2, value arg3)
{
  struct val5 args;

  args.v1 = c; args.v2 = arg1; args.v3 = arg2; args.v4 = arg3;
  if (mcatch(docall3, &args, call_trace_mode())) return result;
  else return NULL;
}

static void docall4(void *x)
{
  struct val5 *args = x;

  result = call4(args->v1, args->v2, args->v3, args->v4, args->v5);
}

value mcatch_call4(value c, value arg1, value arg2, value arg3, value arg4)
{
  struct val5 args;

  args.v1 = c; args.v2 = arg1; args.v3 = arg2; args.v4 = arg3; args.v5 = arg4;
  if (mcatch(docall4, &args, call_trace_mode())) return result;
  else return NULL;
}

static void docall1plus(void *x)
{
  struct val5 *args = x;

  result = call1plus(args->v1, args->v2, args->v3);
}

value mcatch_call1plus(value c, value arg, struct vector *arguments)
{
  struct val5 args;

  args.v1 = c; args.v2 = arg; args.v3 = arguments;
  if (mcatch(docall1plus, &args, call_trace_mode())) return result;
  else return NULL;
}
