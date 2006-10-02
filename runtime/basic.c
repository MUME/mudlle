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

#include <ctype.h>
#include <fcntl.h>
#ifndef WIN32
#  include <netinet/in.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/param.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>

#include "runtime/runtime.h"
#include "runtime/basic.h"
#include "interpret.h"
#include "context.h"
#include "global.h"
#include "alloc.h"
#include "vector.h"
#include "stringops.h"
#include "symbol.h"
#include "call.h"
#include "table.h"
#include "mparser.h"
#include "lexer.h"
#include "compile.h"


TYPEDOP(functionp, "function?", "`x -> `b. TRUE if `x is a function",
        1, (value v),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(TYPE(v, type_closure) || TYPE(v, type_primitive) ||
		  TYPE(v, type_secure) || TYPE(v, type_varargs));
}

TYPEDOP(may_callp, "may_call?", "`f -> `b. TRUE if the caller is allowed to"
	" call `f without a security violation.", 1, (value f),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "f.n")
{
  callable(f, -1);
  return makebool(!seclevel_violator(f));
}

static struct vector *vector_copy(struct vector *v)
{
  struct gcpro gcpro1;
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

TYPEDOP(apply, 0,
        "`f `v -> `x. Excutes `f with arguments `v, returns its result",
	2, (value f, struct vector *args),
	0, "fv.x")
{
  TYPEIS(args, type_vector);
  
  if (TYPE(f, type_varargs))
    {
      struct primitive *prim = f;
      prim->call_count++;
      return apply_vararg(prim->op->op, args);
    }

  callable(f, vector_len(args));
  return call(f, args);
}

#  define EVAL_NAME "ieval"
SECTOP(eval, EVAL_NAME,
       "`s -> `x. Excute the expression in `s and return its result."
       " On compile error, the message is display()'ed and `error_compile"
       " is caused.",
       1, (struct string *str), 1, 0, "s.x")
{
  block_t parser_block;
  value closure;
  char *scopy;
  mfile f;

  TYPEIS(str, type_string);
  LOCALSTR(scopy, str);
  read_from_string(scopy, "<eval>");

  parser_block = new_block();
  if (!(f = parse(parser_block)))
    runtime_error(error_compile);

  if (f->vclass != f_plain)
    runtime_error(error_bad_value);

  if (!(closure = compile_code(f, seclevel)))
    runtime_error(error_compile);

  return call0(closure);
}

TYPEDOP(call_trace, 0, " -> `v. Returns current call trace", 0, (void),
        OP_NOESCAPE, ".v")
{
  return get_mudlle_call_trace();
}

TYPEDOP(error, 0, "`n -> . Causes error `n", 1, (value errn),
        OP_NOESCAPE | OP_LEAF | OP_NOALLOC, "n.")
{
  long n = GETINT(errn);
  if (n < 0 || n >= last_runtime_error)
    runtime_error(error_bad_value);
  runtime_error(n);
  NOTREACHED;
}

TYPEDOP(compiledp, "compiled?",
        " -> `b. Returns true if called from compiled code (ignoring"
        " levels of primitives.",
        0, (void), OP_LEAF | OP_NOESCAPE | OP_NOALLOC, ".n")
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
        OP_NOALLOC | OP_NOESCAPE| OP_LEAF, ".n")
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
        " statement. When `n reaches 0, an `error_loop is generated.\r\n"
        "A good way to test for whether we are approaching an error is"
        " 0 < `n && `n < 1000, or (`max_loop_count() >> 8) instead"
        " of 1000.",
        0, (void),
        OP_NOALLOC | OP_NOESCAPE| OP_LEAF, ".n")
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
                      enum call_trace_mode call_trace_mode)
{
  struct gcpro gcpro1;

  callable(f, 0);
  if (handler)
    callable(handler, 1);

  GCPRO1(handler);
  int ok = mcatch(docall0, f, call_trace_mode);
  UNGCPRO();

  if (ok)
    return 0;

  if (exception_signal == SIGNAL_ERROR &&
      exception_value != makeint(error_loop) &&
      exception_value != makeint(error_recurse))
    {
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

  mthrow(exception_signal, exception_value);
}

TYPEDOP(catch_error, 0, "`f `b -> `x. Executes `f() and returns its result."
        " If an error occurs, returns the error number. If `b is true, error"
        " messages are suppressed. Cf. `trap_error().",
        2, (value f, value suppress),
        0, "fx.x")
{
  trap_error(f, NULL, istrue(suppress) ? call_trace_off : call_trace_on);
  return result;
}

TYPEDOP(handle_error, 0, "`f0 `f1 -> `x. Executes `f0(). If an error occurs,"
        " calls `f1(`errno). Returns result of `f0() or `f1(`errno)."
        " Cf. `trap_error().",
        2, (value f, value handler),
        0, "fx.x")
{
  trap_error(f, handler, call_trace_off);
  return result;
}

TYPEDOP(trap_error, 0,
        "`f0 `f1 `n -> `x. Executes `f0() and returns its return"
        " value.\r\n"
        "If an error occurs in `f0(), calls `f1(`errno), and returns its"
        " return value instead. `f1 can be `null, in which case an error"
        " in `f0() makes `trap_error() return the error number.\r\n"
        "`n specifies how to print any call traces from `f0:\r\n"
        "  `call_trace_off      \tdo not print any call trace\r\n"
        "  `call_trace_barrier  \tprint call traces, but only print until"
        " the current stack level\r\n"
        "  `call_trace_on       \tprint complete call traces",
        3, (value f, value handler, value ct_mode), 0,
        "fxn.x")
{
  enum call_trace_mode call_trace_mode = GETINT(ct_mode);

  if (call_trace_mode < call_trace_off
      || call_trace_mode > call_trace_on)
    runtime_error(error_bad_value);

  trap_error(f, handler, call_trace_mode);
  return result;
}

TYPEDOP(setjmp, 0,
        "`f -> `x. Executes `f(`buf). `buf can be used with `longjmp(). The"
        " return value is either the result of `f(`buf), or the value `x1"
        " passed to `longjmp(`buf, `x1)",
        1, (value f), 0, "f.o")
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
  NOTREACHED;
}

UNSAFEOP(session, 0, "`f -> . Calls `f() in it's own session",
	 1, (struct closure *fn),
	 0)
{
  struct session_context newc;
  value aresult;

  callable(fn, 0);
  session_start(&newc, minlevel, muduser, mudout, muderr);
  aresult = mcatch_call0(fn);
  session_end();

  return aresult;
}

static const typing tref = { "vn.x", "sn.n", "ts.x", "os.x", "ns.x", NULL };

FULLOP(ref, 0, "`x1 `x2 -> `x3. Generic interface to lookup operations:"
       " `x1[`x2] -> `x3",
       2, (value x1, value x2),
       0, OP_LEAF | OP_NOALLOC | OP_NOESCAPE, tref, /* extern */)
{
  if (!pointerp(x1)) runtime_error(error_bad_type);
  switch (((struct obj *)x1)->type)
    {
    case type_vector:
      return code_vector_ref(x1, x2);
    case type_string:
      return code_string_ref(x1, x2);
    case type_table:
      return code_table_ref(x1, x2);
    default: runtime_error(error_bad_type);
    }
  NOTREACHED;
}

static const typing tset = {
  "vnx.3", "snn.n", "tsx.3", "osx.3", "nsx.3", NULL
};

FULLOP(set, "set!",
       "`x1 `x2 `x3 -> . Generic interface to set operations: `x1[`x2] = `x3",
       3, (value x1, value x2, value x3),
       0, OP_LEAF | OP_NOESCAPE, tset, /* extern */)
{
  if (!pointerp(x1)) runtime_error(error_bad_type);
  switch (((struct obj *)x1)->type)
    {
    case type_vector:
      return code_vector_set(x1, x2, x3);
    case type_string:
      return code_string_set(x1, x2, x3);
    case type_table:
      return code_table_set(x1, x2, x3);
    default: runtime_error(error_bad_type);
    }
  NOTREACHED;
}

/* "Object" manipulation:
   load, save, size
   protect, test status, etc
*/

#define OBJ_MAGIC 0x871f54ab

UNSAFEOP(obj_save, "save_data", "`s `x -> . Writes mudlle value `x to file `s",
	 2, (struct string *file, value x), 
	 OP_LEAF | OP_NOESCAPE)
{
  int fd, ok;
  struct gcpro gcpro1;
  void *data;
  unsigned long size, magic, nsize;
  char tmp_file[PATH_MAX];


  TYPEIS(file, type_string);


  GCPRO1(file);
  data = gc_save(x, &size);
  UNGCPRO();

  snprintf(tmp_file, sizeof tmp_file, "%s.%d.%ld", 
	   file->str, getpid(), time(NULL));

  fd = creat(tmp_file, 0666);

  if (fd < 0)
    goto failed;

  magic = htonl(OBJ_MAGIC);
  nsize = htonl(size);
  ok = (write(fd, &magic, sizeof magic) == sizeof magic &&
	write(fd, &nsize, sizeof nsize) == sizeof nsize &&
	write(fd, data, size) == size);

  close(fd);
  
  if (!ok || rename(tmp_file, file->str)) 
    {
      unlink(tmp_file);
      goto failed;
    }


  undefined();

 failed:
  runtime_error(error_bad_value);
}

static value _load_data(value (*fgc_load)(void *_load, unsigned long size),
		       struct string *file)
{
  int fd;
  unsigned long magic, size;
  void *data;


  TYPEIS(file, type_string);


  fd = open(file->str, O_RDONLY);
  if (fd < 0)
    goto failed;
  
  if (read(fd, &magic, sizeof magic) == sizeof magic &&
      ntohl(magic) == OBJ_MAGIC &&
      read(fd, &size, sizeof size) == sizeof size)
    {
      size = ntohl(size);
      data = alloca(size);
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

UNSAFEOP(load_data, 0, "`s -> `x. Loads a value from a mudlle save file",
	 1, (struct string *file),
	 OP_LEAF | OP_NOESCAPE)
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

OPERATION(size_data, 0,
          "`x -> (`n1 . `n2) Returns object's size `n1 (in bytes),"
	  " of which `n2 mutable bytes",
	  1, (value x),
	  OP_LEAF | OP_NOESCAPE)
{
  unsigned long size, mutble;

  size = gc_size(x, &mutble);
  return alloc_list(makeint(size), makeint(mutble));
}

UNSAFEOP(staticpro_data, 0, " -> `v. Returns a vector of all statically "
	 "protected data", 0, (void), OP_LEAF | OP_NOESCAPE)
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

UNSAFEOP(detect_immutability, 0, " -> . Detects the immutable values",
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

TYPEDOP(seclevel, 0, " -> `n. Returns security level of your caller",
        0, (void),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".n")
{
  return makeint(seclevel);
}

TYPEDOP(minlevel, 0, " -> `n. Returns the minimum security level of the"
	" current session. Calling a function with seclevel less than"
	" minlevel will result in a security violation error", 0, (void),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".n")
{
  return makeint(minlevel);
}

UNSAFEOP(unlimited_execution, 0, " -> . Disables execution-time limits",
         0, (void),
	 OP_NOESCAPE | OP_LEAF | OP_NOALLOC)
{
  unlimited_execution();
  undefined();
}

#ifdef ALLOC_STATS

UNSAFEOP(closure_alloc_stats, 0, "  -> `v", 0, (void), OP_LEAF)
{
  return get_closure_alloc_stats();
}

struct vector *get_pair_alloc_stats(void);

UNSAFEOP(pair_alloc_stats, 0, "  -> `v", 0, (void), OP_LEAF)
{
  return get_pair_alloc_stats();
}

struct vector *get_variable_alloc_stats(void);

UNSAFEOP(variable_alloc_stats, 0, "  -> `v", 0, (void), OP_LEAF)
{
  return get_variable_alloc_stats();
}

#endif

void basic_init(void)
{
  DEFINE(functionp);
  DEFINE(may_callp);

  DEFINE(error);
  DEFINE(catch_error);
  DEFINE(handle_error);
  DEFINE(trap_error);

  system_define("call_trace_off",     makeint(call_trace_off));
  system_define("call_trace_barrier", makeint(call_trace_barrier));
  system_define("call_trace_on",      makeint(call_trace_on));

  define_string_vector("error_messages", mudlle_errors, last_runtime_error);

  define_string_vector("type_names", mtypenames, last_synthetic_type);

  DEFINE(setjmp);
  DEFINE(longjmp);

  DEFINE(session);
  DEFINE(apply);
  DEFINE(eval);

  DEFINE(call_trace);
  DEFINE(max_loop_count);
  DEFINE(loop_count);
  DEFINE(compiledp);

  DEFINE(typeof);
  DEFINE(immutablep);
  DEFINE(readonlyp);
  DEFINE(protect);
  DEFINE(detect_immutability);
  DEFINE(check_immutable);

  DEFINE(size_data);
  DEFINE(staticpro_data);
  DEFINE(obj_save);
  DEFINE(load_data);

#ifndef GCDEBUG
  DEFINE(load_data_debug);
#endif

  DEFINE(ref);
  DEFINE(set);

  DEFINE(unlimited_execution);
  DEFINE(seclevel);
  DEFINE(minlevel);

#ifdef ALLOC_STATS
  DEFINE(closure_alloc_stats);
  DEFINE(pair_alloc_stats);
  DEFINE(variable_alloc_stats);
#endif
}
