/* $Log: call.c,v $
 * Revision 1.1  1995/07/15  15:49:25  arda
 * New files, missing from previous commit.
 *
 *
 * Purpose: Call mudlle code from C
 */

static char rcsid[] = "$Id: call.c,v 1.1 1995/07/15 15:49:25 arda Exp $";

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "mudlle.h"
#include "alloc.h"
#include "builtins.h"
#include "interpret.h"
#include "error.h"
#include "stack.h"

/* Interface to machine code. */

#ifdef AMIGA

static INLINE value invoke0(struct closure *c)
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

static INLINE value invoke1(struct closure *c, value arg)
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

static INLINE value invoke1plus(struct closure *c, value arg, struct vector *args)
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

static INLINE value invoke(struct closure *c, struct vector *args)
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

static INLINE value invoke0(struct closure *c)
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c()
   Returns: c()'s result
*/
{
  return mc_invoke(NULL, NULL, NULL, NULL, NULL, c, 0);
}

static INLINE value invoke1(struct closure *c, value arg)
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c(arg)
   Returns: c(arg)'s result
*/
{
  return mc_invoke(arg, NULL, NULL, NULL, NULL, c, 1);
}

static INLINE value invoke1plus(struct closure *c, value arg, struct vector *args)
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

static INLINE value invoke(struct closure *c, struct vector *args)
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

#ifdef linux

static INLINE value invoke0(struct closure *c)
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c()
   Returns: c()'s result
*/
{
}

static INLINE value invoke1(struct closure *c, value arg)
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c(arg)
   Returns: c(arg)'s result
*/
{
}

static INLINE value invoke1plus(struct closure *c, value arg, struct vector *args)
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c(args)
   Returns: c(args)'s result
*/
{
}

static INLINE value invoke(struct closure *c, struct vector *args)
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, type_mcode);
   Effects: Executes c(args)
   Returns: c(args)'s result
*/
{
}

#endif

int callablep(value c, int nargs)
/* Returns: FALSE if c is not something that can be called with
     nargs arguments.
*/
{
  if (pointerp(c))
    {
      struct obj *o = c;

      switch (o->type)
	{
	case type_closure: return TRUE;
	case type_secure:
	  if (seclevel < ((struct primitive *)o)->op->seclevel)
	    return FALSE;
	  /* fall through */
	case type_primitive: 
	  if (((struct primitive *)o)->op->nargs == nargs) return TRUE;
	  break;
	case type_varargs: return TRUE;
	}
    }
  return FALSE;
}

void callable(value c, int nargs)
/* Effects: Causes an error if c is not something that can be called with
     nargs arguments.
*/
{
  if (pointerp(c))
    {
      struct obj *o = c;

      switch (o->type)
	{
	case type_closure: return;
	case type_secure:
	  if (seclevel < ((struct primitive *)o)->op->seclevel)
	    runtime_error(error_security_violation);
	  /* fall through */
	case type_primitive: 
	  if (((struct primitive *)o)->op->nargs == nargs) return;
	  break;
	case type_varargs: return;
	}
    }
  runtime_error(error_wrong_parameters);
}

INLINE value call0(value c)
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
  struct gcpro gcpro1, gcpro2;
  int i, nargs;
  struct primitive_ext *op;
  value result = NULL;

  nargs = 1 + vector_len(args);
  switch (o->type)
    {
    case type_closure:
      {
	struct closure *cl = (struct closure *)o;

	if (cl->code->o.type == type_mcode)
	  return invoke1plus(cl, arg, args);
	else
	  {
	    GCPRO2(cl, args);
	    stack_push(arg);
	    for (i = 0; i < nargs - 1; i++) stack_push(args->data[i]);
	    UNGCPRO();

	    do_interpret(cl, nargs);
	    return stack_pop();
	  }
      }

    case type_secure: case type_primitive:
      ((struct primitive *)o)->call_count++;
      op = ((struct primitive *)o)->op;
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
	}
      return result;

    case type_varargs:
      {
	struct vector *real_args;

	((struct primitive *)o)->call_count++;
	GCPRO1(arg);
	real_args = (struct vector *)unsafe_allocate_record(type_vector, nargs);
	real_args->data[0] = arg;
	memcpy(real_args->data + 1, args->data, (nargs - 1) * sizeof(value));
	UNGCPRO();
	result = ((struct primitive *)o)->op->op(args, nargs);
	return result;
      }
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
  struct gcpro gcpro1, gcpro2;
  int i, nargs;
  struct primitive_ext *op;
  value result = NULL;

  nargs = vector_len(args);
  switch (o->type)
    {
    case type_closure:
      {
	struct closure *cl = (struct closure *)o;

	if (cl->code->o.type == type_mcode)
	  return invoke(cl, args);
	else
	  {
	    GCPRO2(cl, args);
	    for (i = 0; i < nargs; i++) stack_push(args->data[i]);
	    UNGCPRO();

	    do_interpret(cl, nargs);
	    return stack_pop();
	  }
      }

    case type_secure: case type_primitive:
      ((struct primitive *)o)->call_count++;
      op = ((struct primitive *)o)->op;
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
	  result = op->op(args->data[0], args->data[1], args->data[2], args->data[3]);
	  break;
	case 5:
	  result = op->op(args->data[0], args->data[1], args->data[2], args->data[3],
			  args->data[4]);
	  break;
	}
      return result;

    case type_varargs:
      ((struct primitive *)o)->call_count++;
      result = ((struct primitive *)o)->op->op(args, nargs);
      return result;
    }
  abort();
}

/* Calls with error trapping */

static INLINE int display_error(void)
{
  if (catch_context) return catch_context->display_error;
  else return TRUE;		/* Default is display errors */
}

struct val3 { value v1, v2, v3; };
static value result;

static void docall(void *x)
{
  struct val3 *args = x;

  result = call(args->v1, args->v2);
}

value catch_call(value c, struct vector *arguments)
{
  struct val3 args;

  args.v1 = c; args.v2 = arguments;
  if (catch(docall, &args, display_error())) return result;
  else return NULL;
}

static void docall0(void *x)
{
  result = call0(x);
}

value catch_call0(value c)
{
  if (catch(docall0, c, display_error())) return result;
  else return NULL;
}

static void docall1(void *x)
{
  struct val3 *args = x;

  result = call1(args->v1, args->v2);
}

value catch_call1(value c, value arg)
{
  struct val3 args;

  args.v1 = c; args.v2 = arg;
  if (catch(docall1, &args, display_error())) return result;
  else return NULL;
}

static void docall1plus(void *x)
{
  struct val3 *args = x;

  result = call1plus(args->v1, args->v2, args->v3);
}

value catch_call1plus(value c, value arg, struct vector *arguments)
{
  struct val3 args;

  args.v1 = c; args.v2 = arg; args.v3 = arguments;
  if (catch(docall1plus, &args, display_error())) return result;
  else return NULL;
}
