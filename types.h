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

#ifndef TYPES_H
#define TYPES_H

/* The different types */

#include "mudlle.h"

#ifdef USE_GMP
#include <gmp.h>
#endif

#ifdef __sparc__
#include <sys/types.h>
#endif

#ifndef HAVE_ULONG
#ifndef __sparc__
typedef unsigned long ulong;
#endif
#endif
typedef signed short word;
typedef unsigned short uword;
typedef signed char sbyte;
typedef unsigned char ubyte;

#define sizeoffield(type, field) (sizeof ((type *)0)->field)
#define offsetinobj(type, field) (offsetof(type, field) - sizeof (struct obj))

/* The basic classes of all objects, as seen by the garbage collector */
enum garbage_type {
  garbage_string,		/* contains binary, non-GCed data */
  garbage_record,		/* container for other GCed data  */
  garbage_code,			/* special for code 		  */
  garbage_forwarded,		/* temporarily used during GCs 	  */
  garbage_permanent,		/* primitives 			  */
  garbage_temp,			/* container for C pointer 	  */
  garbage_mcode			/* special for mcode 		  */
};

typedef enum 
{
  /* The values below MUST NEVER CHANGE. Stored data depends on them.
     Add new types just before 'type_null'.
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

extern const char *const mtypenames[];

/* The basic structure of all values */
typedef void *value;

struct obj 
{
  ulong size;			/* Total size in bytes, including header */
  enum garbage_type garbage_type : 8;
  mtype type : 8;
  short flags;			/* Eg read-only */
#ifdef GCDEBUG
  ulong generation;
#endif
};

#define TYPEOF(v)				\
  (integerp(v) ? type_integer			\
   : (v) == NULL ? type_null			\
   : ((struct obj *)(v))->type)

#define TYPE(v, want_type) ((want_type) == TYPEOF(v))

/* Code is defined in values (it is known to the gc) */

struct closure			/* Is a record */
{
  struct obj o;
  struct code *code;		/* May be type_code, type_mcode, type_primitive
				   as well */
  struct variable *variables[]; /* May be other types */
};

struct string			/* Is a string */
{
  struct obj o;
  char str[];			/* Must be null terminated */
};

struct mudlle_float
{
  struct obj o;
  double     d;
};

struct bigint
{
  struct obj	o;
#ifdef USE_GMP
  mpz_t		mpz;
  mp_limb_t	limbs[];
#endif
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
  const struct primitive_ext *op;
  ulong call_count;
};

#define MAX_PRIMITIVE_ARGS 5

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
  const char *const *type;	/* Pointer to a typing array */
  uword seclevel;		/* Only for type_secure */
  const char *filename;
  int lineno;
};

#define OP_LEAF     1           /* Operation is leaf (calls no other mudlle
				   code) */
#define OP_NOALLOC  2           /* Operation does not allocate anything */
#define OP_CLEAN    4           /* Operation can be called directly (guarantees
				   GC integrity w/ respect to registers) */
#define OP_NOESCAPE 8           /* Operation does not lead to any variables
				   being changed (~= calls no other mudlle
				   functions) */

struct vector			/* Is a record */
{
  struct obj o;
  value data[];
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

struct mjmpbuf {
  struct obj o;
  value ptype;
  struct catch_context *context;
};

struct closure *unsafe_alloc_closure(ulong nb_variables);
struct closure *alloc_closure0(struct code *code);
struct string *alloc_string(const char *s);
struct string *mudlle_string_copy(struct string *s);
struct string *alloc_empty_string(size_t length);
struct string *alloc_string_length(const char *s, size_t length);
struct mudlle_float *alloc_mudlle_float(double d);
struct bigint *alloc_bigint(mpz_t mpz);
struct string *safe_alloc_string(const char *s);
struct variable *alloc_variable(value val);
struct symbol *alloc_symbol(struct string *name, value data);
struct vector *alloc_vector(ulong size);
struct list *alloc_list(value car, value cdr);
struct character *alloc_character(struct char_data *ch);
struct object *alloc_object(struct obj_data *obj);
struct primitive *alloc_primitive(ulong nb, const struct primitive_ext *op);
struct primitive *alloc_secure(ulong nb, const struct primitive_ext *op);
void check_bigint(struct bigint *bi);

/* Private types which are visible to the mudlle programmer must be
   records identified by their first element with one of the following
   constants: */
enum {
  PRIVATE_CALL_IN = 1,
  PRIVATE_MJMPBUF = 2,
  PRIVATE_REGEXP  = 3
};

struct grecord *alloc_private(int id, ulong size);

#define string_len(str) ((str)->o.size - (sizeof(struct obj) + 1))
#define vector_len(vec) (((vec)->o.size - sizeof(struct obj)) / sizeof(value))

/* 0 is false, everything else is true */
#define isfalse(v) ((value)(v) == makebool(false))
#define istrue(v)  (!isfalse(v))
/* Make a mudlle boolean from a C boolean (1 or 0) */
#define makebool(i) makeint(!!(i))

#define LOCALSTR(local, from) do {		\
  int __l = string_len(from) + 1;		\
						\
  local = alloca(__l);				\
  memcpy(local, from->str, __l);		\
} while (0)

#define SET_VECTOR(v, idx, val)			\
do {						\
  value __tmp = (val);				\
  (v)->data[idx] = __tmp;			\
} while(0)

/*
 * Converts the string sp into an int i and returns 1.
 * On over/underflow or illegal characters, it returns 0.
 */
int mudlle_strtoint(const char *sp, int *i);
int mudlle_strtofloat(const char *sp, double *d);

#endif
