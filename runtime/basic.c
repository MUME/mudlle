/* $Log: basic.c,v $
 * Revision 1.47  1995/07/15  15:24:52  arda
 * Context cleanup.
 * Remove GCDEBUG.
 *
 * Revision 1.46  1994/10/09  06:44:02  arda
 * Libraries
 * Type inference
 * Many minor improvements
 *
 * Revision 1.45  1994/09/16  13:07:23  arda
 * Rename protect to catch.
 * New protect/unprotect functions (like dynpro/undynpro).
 *
 * Revision 1.44  1994/09/15  19:48:06  arda
 * Performance improvements:
 *   improve reaction list rep
 *   make timed action lists C arrays
 * Check for readonly symbols
 *
 * Revision 1.43  1994/09/09  19:36:34  arda
 * Table prefixes.
 *
 * Revision 1.42  1994/09/03  13:37:45  arda
 * Shops mudlled.
 * Some macros changed to functions.
 *
 * Revision 1.41  1994/08/31  13:03:33  arda
 * Bug fixes (argh, no, new version of characters structures! (MD))
 *
 * Revision 1.40  1994/08/29  13:19:45  arda
 * Contagious immutability.
 * Global array of values instead of variables.
 * Direct recursion.
 *
 * Revision 1.39  1994/08/26  04:35:59  arda
 * hidden objects
 *
 * Revision 1.38  1994/08/22  18:03:36  arda
 * Primitives for compiler.
 *
 * Revision 1.37  1994/08/22  11:18:50  arda
 * Changes for mudlle compiler in MUME.
 *
 * Revision 1.36  1994/08/17  16:30:05  arda
 * Seclevel fixes.
 *
 * Revision 1.35  1994/08/17  15:08:11  arda
 * Removed $HOME/mume/lib from obj_load/save path.
 * Changed def of UNSAFEOP to make standalone compiled files compatible
 * with MUME ones.
 *
 * Revision 1.34  1994/08/16  19:16:55  arda
 * Added flags to primitives for better calling sequences.
 *
 * Revision 1.30  1994/05/08  14:13:52  arda
 * Event review
 *
 * Revision 1.29  1994/04/12  20:12:01  arda
 * (MD) Alignments and fixes + unknown from others...
 *
 * Revision 1.28  1994/03/08  01:50:44  arda
 * (MD) New Istari.
 *
 * Revision 1.27  1994/02/24  08:33:33  arda
 * Owl: New error messages.
 *
 * Revision 1.26  1994/02/12  17:25:35  arda
 * Owl: MUME IV (special procedures eliminated).
 *
 * Revision 1.25  1994/02/11  10:00:04  dgay
 * Owl: -Wall
 *      new shared string handling
 *      configuration file
 *
 * Revision 1.24  1994/02/03  20:02:55  arda
 * Owl: Much faster regular actions (still need testing).
 *
 * Revision 1.22  1994/01/29  19:50:55  dgay
 * Owl: add file & line information to functions.
 *
 * Revision 1.21  1994/01/07  13:09:41  arda
 * Owl: Spec countdown continues.
 *
 * Revision 1.20  1993/12/23  20:50:50  dgay
 * Owl: Added Amiga makefile for convenience.
 *
 * Revision 1.19  1993/12/06  19:21:27  arda
 * divers CLI
 *
 * Revision 1.18  1993/11/27  11:29:18  arda
 * Owl: Major changes to affect.
 *      Save mudlle data with players & objects.
 *      Change skill format on disk.
 *      Other minor changes.
 *      Still needs full debugging.
 *
 * Revision 1.17  1993/10/03  14:07:23  dgay
 * Bumper disun8 update.
 *
 * Revision 1.16  1993/08/28  17:01:37  un_autre
 * SIZE OF ARMORS (CLI)
 *
 * Revision 1.15  1993/08/15  21:01:55  un_mec
 * Owl: Several extras functions.
 *      rent.
 *
 * Revision 1.14  1993/08/14  16:43:44  un_mec
 * Owl: New input system (with an input stack) => small interaction changes
 *
 * Revision 1.13  1993/07/21  20:37:59  un_mec
 * Owl: Standalone version of mudlle (mkf, runtime/mkf, mudlle.c) added to CVS
 *      New builtin functions, new abbreviations (. = cons, ! = not).
 *
 * Revision 1.12  1993/05/02  13:03:01  un_mec
 * Owl: ARGH! Bugs.
 *
 * Revision 1.11  1993/04/24  19:38:51  un_mec
 * Owl: Fix apropos bug.
 *
 * Revision 1.10  1993/04/24  15:20:50  un_mec
 * Owl: Code cleanup.
 *
 * Revision 1.9  1993/04/22  18:59:12  un_autre
 * (MD) & Owl. Bug fixes. /player fixes. EVER_WHINER flag. saving_spells adjusted.
 *
 * Revision 1.8  1993/04/17  10:03:43  un_autre
 * Various
 *
 * Revision 1.7  1993/04/12  16:16:02  un_autre
 * *** empty log message ***
 *
 * Revision 1.6  1993/03/29  09:25:24  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.4  1993/03/17  12:50:53  dgay
 * Added security features.
 *
 * Revision 1.3  1993/03/14  16:16:31  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.4  1993/02/11  15:49:17  un_mec
 * Change to if syntax (Owl)
 *
 * Revision 1.3  1993/01/08  23:57:43  un_mec
 * Owl: Add character and object types.
 *
 * Revision 1.2  1992/12/30  14:11:56  un_mec
 * Owl:
 * Several changes:
 * - Variables don't have separate value & function cells, instead their are
 *   now 2 types: type_function & type_variable.
 * 	-> new functions store, recall. Removed store-xx, recall-xx.
 * - New types: list (Lisp style pair), vector (array)
 *
 * Revision 1.1  1992/12/27  21:42:11  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

static char rcsid[] = "$Id: basic.c,v 1.47 1995/07/15 15:24:52 arda Exp $";

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <alloca.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/param.h>
#include "runtime/runtime.h"
#include "runtime/basic.h"
#include "interpret.h"
#include "mudio.h"
#include "tree.h"
#include "global.h"
#include "alloc.h"
#include "vector.h"
#include "stringops.h"
#include "symbol.h"
#include "call.h"

#ifdef MUME
#include "char.h"
#include "object.h"
#include "room.h"
#include "interact.h"
#include <sys/time.h>
#include "def.char.h"
#include "struct.time.h"
#include "struct.char.h"
#include "macro.h"
#include "frontend.h"
#include "def.files.h"
#endif

TYPEDOP(codep, "x -> b. TRUE if x is a function", 1, (value v),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(TYPE(v, type_closure) || TYPE(v, type_primitive) ||
		  TYPE(v, type_secure) || TYPE(v, type_varargs));
}

TYPEDOP(apply, "fn v -> x. Excutes fn with arguments v, returns its result",
	2, (value f, struct vector *args),
	0, "fv.x")
{
  TYPEIS(args, type_vector);
  callable(f, vector_len(args));
  return call(f, args);
}

TYPEDOP(apply1plus,
	"fn x1 v -> x2. Excutes fn with arguments x1, v, returns its result",
	3, (value f, value x, struct vector *args),
	0, "fxv.x")
{
  TYPEIS(args, type_vector);
  callable(f, 1 + vector_len(args));
  return call1plus(f, x, args);
}

OPERATION(error, "n -> . Causes error n", 1, (value errno),
	  OP_NOESCAPE)
{
  ISINT(errno);
  runtime_error(intval(errno));
}

static value result;

static value result;

static void docall0(void *x)
{
  result = call0(x);
}

OPERATION(catch_error, "fn b -> x. Executes fn() and returns its result. If an error occurs,\n\
returns the error number. If b is true, error messages are suppressed",
	  2, (value f, value suppress),
	  0)
{
  callable(f, 0);

  if (catch(docall0, f, !istrue(suppress))) return result; /* No error */

  if (exception_signal == SIGNAL_ERROR &&
      exception_value != makeint(error_loop) &&
      exception_value != makeint(error_recurse)) return exception_value;
  throw(exception_signal, exception_value);
}

OPERATION(handle_error, "fn1 fn2 -> x. Executes fn1(). If an error occurs, calls fn2(errno). Returns result of fn1 or fn2",
	  2, (value f, value handler),
	  0)
{
  struct gcpro gcpro1;
  int ok;

  callable(f, 0);
  callable(handler, 1);

  GCPRO1(handler);
  ok = catch(docall0, f, FALSE);
  UNGCPRO();

  if (ok) return result; /* No error */
  if (exception_signal == SIGNAL_ERROR &&
      exception_value != makeint(error_loop) &&
      exception_value != makeint(error_recurse))
    return call1(handler, exception_value);

  throw(exception_signal, exception_value);
}

static typing tref = { "vn.x", "sn.n", "ts.x", "os.x", "ns.x", NULL };

FULLOP(ref, "x1 x2 -> x3. Generic interface to lookup operations: x1[x2] -> x3",
       2, (value x1, value x2),
       0, OP_LEAF | OP_NOALLOC | OP_NOESCAPE, tref)
{
#ifdef MUME
  if (integerp(x1)) return code_room_ref(x1, x2);
  if (!x1) runtime_error(error_bad_type);
#else
  if (!pointerp(x1)) runtime_error(error_bad_type);
#endif
  switch (((struct obj *)x1)->type)
    {
    case type_vector:
      return code_vector_ref(x1, x2);
    case type_string:
      return code_string_ref(x1, x2);
    case type_table:
      return code_table_ref(x1, x2);
#ifdef MUME
    case type_character:
      return code_char_ref(x1, x2);
    case type_object:
      return code_obj_ref(x1, x2);
#endif
    default: runtime_error(error_bad_type);
    }
}

static typing tset = { "vnx.3", "snn.n", "tsx.3", "osx.3", "nsx.3", NULL };

FULLOP(set, "x1 x2 x3 -> . Generic interface to set operations: x1[x2] = x3",
       3, (value x1, value x2, value x3),
       0, OP_LEAF | OP_NOESCAPE, tset)
{
#ifdef MUME
  if (integerp(x1)) return code_room_set(x1, x2, x3);
  if (!x1) runtime_error(error_bad_type);
#else
  if (!pointerp(x1)) runtime_error(error_bad_type);
#endif
  switch (((struct obj *)x1)->type)
    {
    case type_vector:
      return code_vector_set(x1, x2, x3);
    case type_string:
      return code_string_set(x1, x2, x3);
    case type_table:
      return code_table_set(x1, x2, x3);
#ifdef MUME
    case type_character:
      return code_char_set(x1, x2, x3);
    case type_object:
      return code_obj_set(x1, x2, x3);
#endif
    default: runtime_error(error_bad_type);
    }
}

/* "Object" manipulation:
   load, save, size
   protect, test status, etc
*/

#define OBJ_MAGIC 0x871f54ab

UNSAFEOP(obj_save, "s x -> . Writes mudlle value x to file s",
	 2, (struct string *file, value x), 
	 OP_LEAF | OP_NOESCAPE)
{
  int fd, ok;
  struct gcpro gcpro1;
  void *data;
  unsigned long size, magic;
  
  TYPEIS(file, type_string);
  GCPRO1(file);
  data = gc_save(x, &size);
  UNGCPRO();

  fd = creat(file->str, 0666);
  if (fd < 0) runtime_error(error_bad_value);

  magic = OBJ_MAGIC;
  ok = write(fd, &magic, sizeof magic) == sizeof magic &&
    write(fd, &size, sizeof size) == sizeof size &&
      write(fd, data, size) == size;
  close(fd);
  
  if (!ok) 
    {
      unlink(file->str);
      runtime_error(error_bad_value);
    }
  undefined();
}

static value _obj_load(value (*gc_load)(void *_load, unsigned long size),
		       struct string *file)
{
  int fd;
  unsigned long magic, size;
  void *data;

  TYPEIS(file, type_string);
  fd = open(file->str, O_RDONLY);
  if (fd < 0) runtime_error(error_bad_value);
  
  if (read(fd, &magic, sizeof magic) == sizeof magic &&
      magic == OBJ_MAGIC &&
      read(fd, &size, sizeof size) == sizeof size)
    {
      data = alloca(size);
      if (read(fd, data, size) == size)
	{
	  close(fd);
	  return gc_load(data, size);
	}
    }
  close(fd);
  runtime_error(error_bad_value);
}

UNSAFEOP(obj_load, "s -> x. Loads a value from a mudlle save file",
	 1, (struct string *file),
	 OP_LEAF | OP_NOESCAPE)
{
  return _obj_load(gc_load, file);
}

#ifndef GCDEBUG
UNSAFEOP(obj_load_debug, "s -> x. Loads a value from a GCDEBUG mudlle save file",
	 1, (struct string *file),
	 OP_LEAF | OP_NOESCAPE)
{
  return _obj_load(gc_load_debug, file);
}
#endif

OPERATION(obj_size, "x -> (n1 . n2) Returns object's size n1 (in bytes) (of which n2 mutable bytes)",
	  1, (value x),
	  OP_LEAF | OP_NOESCAPE)
{
  unsigned long size, mutable;

  size = gc_size(x, &mutable);
  return alloc_list(makeint(size), makeint(mutable));
}

TYPEDOP(immutablep, "x -> b. Returns true if x is an immutable value",
	1, (value x),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(immutablep(x));
}

TYPEDOP(readonlyp, "x -> b. Returns true if x is a read-only value",
	1, (value x),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(readonlyp(x));
}

TYPEDOP(protect, "x -> x. Makes value x readonly",
	1, (struct obj *x),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.1")
{
  if (pointerp(x)) x->flags |= OBJ_READONLY;
  return x;
}

UNSAFEOP(detect_immutability, " -> . Detects the immutable values",
	 0, (void),
	 OP_LEAF | OP_NOESCAPE)
{
  detect_immutability();
  undefined();
}

TYPEDOP(typeof, "x -> n. Return type of x",
	1, (value x),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makeint(TYPEOF(x));
}

#if 0
TYPEDOP(seclevel, " -> n. Returns security level of your caller", 0, (void),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".n")
{
  return makeint(caller_level);
}
#endif

UNSAFEOP(unlimited_execution, " -> . Disables execution-time limits", 0, (void),
	 OP_NOESCAPE)
{
  unlimited_execution();
  undefined();
}

void basic_init(void)
{
  DEFINE("function?", codep);
  DEFINE("error", error);
  DEFINE("catch_error", catch_error);
  DEFINE("handle_error", handle_error);
  DEFINE("apply", apply);
  /*DEFINE("apply1plus", apply1plus);*/ /* Really useful ? */

  DEFINE("typeof", typeof);
  DEFINE("immutable?", immutablep);
  DEFINE("readonly?", readonlyp);
  DEFINE("protect", protect);
  DEFINE("detect_immutability", detect_immutability);

  DEFINE("size_data", obj_size);
  DEFINE("save_data", obj_save);
  DEFINE("load_data", obj_load);
#ifndef GCDEBUG
  DEFINE("load_data_debug", obj_load_debug);
#endif

  DEFINE("ref", ref);
  DEFINE("set!", set);

  DEFINE("unlimited_execution", unlimited_execution);
#if 0
  DEFINE("seclevel", seclevel);
#endif
}
