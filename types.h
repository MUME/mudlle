/* $Log: types.h,v $
 * Revision 1.18  1995/01/22  15:11:53  arda
 * Linux patches.
 *
 * Revision 1.17  1994/10/09  06:43:10  arda
 * Libraries
 * Type inference
 * Many minor improvements
 *
 * Revision 1.16  1994/08/29  13:17:36  arda
 * Contagious immutability.
 * Global array of values instead of variables.
 * Direct recursion.
 *
 * Revision 1.15  1994/08/16  19:16:26  arda
 * Mudlle compiler for sparc now fully functional (68k compiler now needs
 * updating for primitives).
 * Changes to allow Sparc trap's for runtime errors.
 * Also added flags to primitives for better calling sequences.
 *
 * Revision 1.13  1994/02/24  08:33:10  arda
 * Owl: New error messages.
 *
 * Revision 1.12  1994/02/12  17:25:00  arda
 * Owl: Better code generated.
 *
 * Revision 1.11  1994/02/11  09:59:25  dgay
 * Owl: -Wall
 *      new shared string handling
 *      configuration file
 *
 * Revision 1.10  1994/02/03  19:21:50  arda
 * nothing special(2)
 *
 * Revision 1.9  1993/11/27  11:29:10  arda
 * Owl: Major changes to affect.
 *      Save mudlle data with players & objects.
 *      Change skill format on disk.
 *      Other minor changes.
 *      Still needs full debugging.
 *
 * Revision 1.8  1993/07/25  10:58:39  un_mec
 * Owl: General schema for private types defined.
 *
 * Revision 1.7  1993/07/21  20:37:03  un_mec
 * Owl: Added &&, ||, optimised if.
 *      Added branches to the intermediate language.
 *      Separated destiniation language generation into ins module
 *      (with some peephole optimisation)
 *      Standalone version of mudlle (mkf, runtime/mkf, mudlle.c) added to CVS
 *
 * Revision 1.6  1993/05/02  07:38:08  un_mec
 * Owl: New output (mudlle ports).
 *
 * Revision 1.5  1993/03/29  09:24:49  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.4  1993/03/17  12:50:13  dgay
 * Fixed GC of help strings in code blocks.
 * Added security features.
 *
 * Revision 1.3  1993/03/14  16:15:14  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.3  1993/01/08  23:57:12  un_mec
 * Owl: Allow characters and objects to appear in mudlle.
 *
 * Revision 1.2  1992/12/30  14:10:59  un_mec
 * Owl:
 * Several changes:
 * - Variables don't have separate value & function cells, instead their are
 *   now 2 types: type_function & type_variable.
 * - print_value: New types (list, vector), printing rationalised.
 * - New type: list (Lisp style pair)
 * - lexer.l: Debug read_from_string
 * - debug_level & DEBUG macro provided to help debugging.
 *
 * Revision 1.1  1992/12/27  21:41:43  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

#ifndef TYPES_H
#define TYPES_H

#include "mvalues.h"

/* The different types */

typedef enum 
{
  /* The values below MUST NEVER CHANGE. Stored data depends on them.
     Add new types just before 'last_type'.
     Also some generated code depends on the values. */
  type_code, type_closure, type_variable, type_internal,
  type_primitive, type_varargs, type_secure,

  type_integer, type_string, type_vector, type_pair, type_symbol, type_table,
  type_private,

  type_object, type_character, type_gone,

  type_outputport, type_mcode, type_null,

  last_type,

  /* Synthetic types, not used in object representations but represent
     a set of the previous types.
     They can thus change */
  stype_none = last_type,	/* no type, the empty set */
  stype_any,			/* All types */
  stype_function,		/* { closure, primitive, varargs, secure } */
  stype_list,			/* { pair, null } */
  last_synthetic_type
} mtype;

#define TYPE(v, want_type) \
  (integerp((v)) ? (want_type) == type_integer : \
   		   ((v) && ((struct obj *)(v))->type == (want_type)))

#define TYPEOF(v) \
  (integerp((v)) ? type_integer : \
   !v ? type_null : ((struct obj *)(v))->type)

/* Code is defined in values (it is known to the gc) */

struct closure			/* Is a record */
{
  struct obj o;
  struct code *code;		/* May be type_code, type_mcode, type_primitive
				   as well */
  struct variable *variables[1]; /* May be other types */
};

struct string			/* Is a string */
{
  struct obj o;
  char str[1];			/* Must be null terminated */
};

struct variable			/* Is a record */
{
  /* This is used for type_variable and type_function */
  struct obj o;
  value value;
};

struct symbol			/* Is a record */
{
  struct obj o;
  struct string *name;
  value data;
};

/* A primitive operation (eg +, -) */
struct primitive		/* Is a permanent external */
{
  struct obj o;
  ulong nb;
  struct primitive_ext *op;
  ulong call_count;
};

typedef const char *typing[];

struct primitive_ext		/* The external structure */
{
  const char *name;
  const char *help;
  value (*op)();
  word nargs;
  uword flags;			/* Helps compiler select calling sequence */
  const char **type;		/* Pointer to a typing array */
  uword seclevel;		/* Only for type_secure */
};

#define OP_LEAF 1		/* Operation is leaf (calls no other mudlle code) */
#define OP_NOALLOC 2		/* Operation does not allocate anything */
#define OP_CLEAN 4		/* Operation can be called directly
				   (guarantees GC integrity w/ respect to registers) */
#define OP_NOESCAPE 8		/* Operation does not lead to any variables being
				   changed (~= calls no other mudlle functions) */

struct vector			/* Is a record */
{
  struct obj o;
  value data[1];
};

struct list			/* Is a record */
{
  struct obj o;
  value car, cdr;
};

struct character		/* Is a temporary external */
{
  struct obj o;
  struct char_data *ch;
};

struct object			/* Is a temporary external */
{
  struct obj o;
  struct obj_data *obj;
};

struct closure *unsafe_alloc_closure(ulong nb_variables);
struct closure *alloc_closure0(struct code *code);
struct string *alloc_string(const char *s);
struct variable *alloc_variable(value val);
struct symbol *alloc_symbol(struct string *name, value data);
struct vector *alloc_vector(ulong size);
struct list *alloc_list(value car, value cdr);
struct character *alloc_character(struct char_data *ch);
struct object *alloc_object(struct obj_data *obj);
struct primitive *alloc_primitive(ulong nb, struct primitive_ext *op);
struct primitive *alloc_secure(ulong nb, struct primitive_ext *op);

/* Private types which are visible to the mudlle programmer must be
   records identified by their first element with one of the following
   constants: */
#define PRIVATE_REGISTER_AT 0

struct grecord *alloc_private(int id, ulong size);

#define string_len(str) ((str)->o.size - (sizeof(struct obj) + 1))
#define vector_len(vec) (((vec)->o.size - sizeof(struct obj)) / sizeof(value))

/* For the time being, 0 is false, everything else is true */
#define istrue(v) ((value)(v) != makebool(FALSE))
/* Make a mudlle boolean from a C boolean (1 or 0) */
#define makebool(i) makeint(i)

#endif
