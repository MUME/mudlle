/* $Log: runtime.h,v $
 * Revision 1.20  1995/07/15  15:25:06  arda
 * Context cleanup.
 * Remove GCDEBUG.
 *
 * Revision 1.19  1994/10/09  06:44:19  arda
 * Libraries
 * Type inference
 * Many minor improvements
 *
 * Revision 1.18  1994/08/17  16:30:10  arda
 * Seclevel fixes.
 *
 * Revision 1.17  1994/08/17  15:08:12  arda
 * Removed $HOME/mume/lib from obj_load/save path.
 * Changed def of UNSAFEOP to make standalone compiled files compatible
 * with MUME ones.
 *
 * Revision 1.16  1994/08/16  19:17:16  arda
 * Added flags to primitives for better calling sequences.
 *
 * Revision 1.13  1994/02/24  08:33:44  arda
 * Owl: New error messages.
 *
 * Revision 1.12  1994/02/03  19:22:41  arda
 * nothing special(3)
 *
 * Revision 1.11  1993/08/15  21:02:09  un_mec
 * Owl: Several extras functions.
 *      rent.
 *
 * Revision 1.10  1993/07/21  20:38:15  un_mec
 * Owl: Standalone version of mudlle (mkf, runtime/mkf, mudlle.c) added to CVS
 *      New builtin functions, new abbreviations (. = cons, ! = not).
 *
 * Revision 1.9  1993/05/02  07:38:19  un_mec
 * Owl: New output (mudlle ports).
 *
 * Revision 1.8  1993/04/24  16:50:11  un_autre
 * Owl's
 *
 * Revision 1.6  1993/04/17  10:03:54  un_autre
 * Various
 *
 * Revision 1.5  1993/03/29  09:25:53  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.4  1993/03/17  12:51:13  dgay
 * Added security features.
 *
 * Revision 1.3  1993/03/14  16:16:48  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.3  1993/01/08  23:57:49  un_mec
 * Owl: Add character and object types.
 *
 * Revision 1.2  1992/12/30  14:12:02  un_mec
 * Owl:
 * Several changes:
 * - Variables don't have separate value & function cells, instead their are
 *   now 2 types: type_function & type_variable.
 * 	-> new functions store, recall. Removed store-xx, recall-xx.
 * - New types: list (Lisp style pair), vector (array)
 *
 * Revision 1.1  1992/12/27  21:42:21  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

#ifndef RUNTIME_H
#define RUNTIME_H

#include "mudlle.h"
#include "types.h"
#include "alloc.h"
#include "stack.h"
#include "global.h"
#include "error.h"

void runtime_init(void);

#define FULLOP(x, helpmsg, nargs, args, seclevel, flags, type) \
value code_ ## x args; \
static struct primitive_ext op_ ## x = { NULL, helpmsg, code_ ## x, nargs, flags, type, seclevel }; \
\
value code_ ## x args

#define TYPEDOP(x, helpmsg, nargs, args, flags, type) \
  MTYPE(type_ ## x, type); \
  FULLOP(x, helpmsg, nargs, args, 0, flags, type_ ## x)

#define OPERATION(x, helpmsg, nargs, args, flags) \
  FULLOP(x, helpmsg, nargs, args, 0, flags, NULL)

#define VAROP(x, helpmsg, flags) \
  FULLOP(x, helpmsg, -1, (struct vector *args, ulong nargs), 0, flags, NULL)

#define SECOP(x, helpmsg, nargs, args, seclevel, flags) \
  FULLOP(x, helpmsg, nargs, args, seclevel, flags, NULL)

/* Unsafe operations are restricted in MUME */
#ifndef MUME
#define LVL_IMPLEMENTOR 1
#endif

#define UNSAFEOP(x, helpmsg, nargs, args, flags) \
  SECOP(x, "UNSAFE:" helpmsg, nargs, args, LVL_IMPLEMENTOR, flags)

#define DEFINE(name, x) runtime_define(name, &op_ ## x)

void system_define(const char *name, value val);
/* Modifies: environment
   Requires: name not already exist in environment.
   Effects: Adds name to environment, with value val for the variable,
     as a 'define' of the system module.
*/

void runtime_define(const char *name, struct primitive_ext *op);

#define TYPEIS(v, want_type) \
  if (!TYPE((v), (want_type))) runtime_error(error_bad_type)

#define ISINT(v) if (!integerp((v))) runtime_error(error_bad_type)

#define CHECK_FAST_LOOP() \
  if (!--xcount) runtime_error(error_loop);

void check_interrupt(void);
/* Effects: Causes a user_interrupt runtime error if user caused SIGINT or SIGQUIT
*/

/* Return the undefined result */
#define undefined()  return undefined_value

extern value undefined_value;

/* Typing information for primitives */
/* A type signature is a string xxx.y, where the
   x's stand for the type of arguments, y for the type of the result.
   y can be ommitted for functions with undefined results.
   The following characters are used:

   f: function
   n: integer
   s: string
   v: vector
   l: list (pair or null)
   k: pair
   t: table
   y: symbol
   x: any
   o: other
   1-9: same type as corresponding argument (must be a previous arg)
   A-Z: special typesets, as follows:
    S: string or integer

  A typing is just an array of strings (terminated by NULL).
  Rep chosen for ease of type specification
*/

#define MTYPE(name, sig) static typing name = { sig, NULL }

#endif

