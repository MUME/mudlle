/*
 * Copyright (c) 1993-1999 David Gay and Gustav Hållberg
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

#ifndef TYPES_H
#define TYPES_H

#include <alloca.h>

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

  type_outputport, type_mcode, type_float, type_bigint, type_null,

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

struct mudlle_float
{
  struct obj o;
  double     d;
};

struct bigint
{
  struct obj	o;
  mpz_t		mpz;
  mp_limb_t	limbs[1];
};

struct variable			/* Is a record */
{
  /* This is used for type_variable and type_function */
  struct obj o;
  value vvalue;
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
#ifdef __cplusplus
  value (*op)(...);
#else
  value (*op)();
#endif
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
struct mudlle_float *alloc_mudlle_float(double d);
struct bigint *alloc_bigint(mpz_t mpz);
struct string *safe_alloc_string(const char *s);
struct variable *alloc_variable(value val);
struct symbol *alloc_symbol(struct string *name, value data);
struct vector *alloc_vector(ulong size);
struct list *alloc_list(value car, value cdr);
struct character *alloc_character(struct char_data *ch);
struct object *alloc_object(struct obj_data *obj);
struct primitive *alloc_primitive(ulong nb, struct primitive_ext *op);
struct primitive *alloc_secure(ulong nb, struct primitive_ext *op);
void check_bigint(struct bigint *bi);

/* Private types which are visible to the mudlle programmer must be
   records identified by their first element with one of the following
   constants: */
enum {
  PRIVATE_CALL_IN = 1
};

struct grecord *alloc_private(int id, ulong size);

#define string_len(str) ((str)->o.size - (sizeof(struct obj) + 1))
#define vector_len(vec) (((vec)->o.size - sizeof(struct obj)) / sizeof(value))

/* For the time being, 0 is false, everything else is true */
#define istrue(v) ((value)(v) != makebool(FALSE))
/* Make a mudlle boolean from a C boolean (1 or 0) */
#define makebool(i) makeint(!!(i))

#define LOCALSTR(local, from) do {		\
  int __l = string_len(from) + 1;		\
						\
  local = alloca(__l);				\
  memcpy(local, from->str, __l);		\
} while (0)

/*
 * Converts the string sp into an int i and returns 1.
 * On over/underflow or illegal characters, it returns 0.
 */
int mudlle_strtoint(const char *sp, int *i);
int mudlle_strtofloat(const char *sp, double *d);

#endif
