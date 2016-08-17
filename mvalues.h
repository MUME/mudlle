/*
 * Copyright (c) 1993-2012 David Gay and Gustav H�llberg
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

#ifndef MVALUES_H
#define MVALUES_H

#include <limits.h>
#include <stddef.h>
#include "types.h"

/* Objects are either null, integers or pointers to more complex things (like
   variables). Null is represented by NULL. Integers have the lowest bit set.
   Anything else is a pointer to an object (struct obj *). */

static inline bool is_function(value v)
{
  if (!pointerp(v))
    return false;
  return (1U << ((struct obj *)v)->type) & TYPESET_FUNCTION;
}

static inline bool is_any_primitive(value p)
{
  if (!pointerp(p))
    return false;
  return ((struct obj *)p)->garbage_type == garbage_primitive;
}

/* Make & unmake integers */
#define intval(obj)  ((long)(obj) >> 1)
#define uintval(obj) ((unsigned long)(obj) >> 1)
/* add 0UL to integer-promote to unsigned long */
#define makeint(i)   ((value)((((i) + 0UL) << 1) | 1))

enum {
  MAX_MUDLLE_OBJECT_SIZE = 16 * 1024 * 1024,
  MAX_VECTOR_SIZE = ((MAX_MUDLLE_OBJECT_SIZE - sizeof (struct vector))
                     / sizeof (value)),
  MAX_STRING_SIZE = (MAX_MUDLLE_OBJECT_SIZE - sizeof (struct string) - 1),
  MAX_TABLE_ENTRIES  = ((MAX_MUDLLE_OBJECT_SIZE / sizeof (value) / 2)
                        * 3 / 4 - 1)
};

#define TAGGED_INT_BITS (CHAR_BIT * sizeof (long) - 1)
#define MAX_TAGGED_INT  (LONG_MAX >> 1)
#define MIN_TAGGED_INT  (-MAX_TAGGED_INT - 1)

enum {
  OBJ_READONLY = 1,       /* Used for some values */
  OBJ_IMMUTABLE = 2,      /* Contains only pointers to other immutable objects.
                             Its pointers are never modified after allocation/
                             initialisation. All initialisation must be
                             done before any other allocation. */
  OBJ_FLAG_0 = 4,         /* Temporarily used to flag recursions  */
  OBJ_FLAG_1 = 8          /* Temporarily used to flag recursions  */
};

static inline bool obj_readonlyp(struct obj *obj)
{
  return obj->flags & OBJ_READONLY;
}

/* True if x is immutable */
#define immutablep(x) \
  (!pointerp((x)) || (((struct obj *)(x))->flags & OBJ_IMMUTABLE) != 0)

/* True if x is readonly */
#define readonlyp(x) (!pointerp((x)) || obj_readonlyp((struct obj *)(x)))

static inline value make_readonly(value v)
{
  /* must not try to modify static strings, so check readonlyp() */
  if (!readonlyp(v))
    {
      struct obj *o = v;
      assert(o->type != type_oport);
      o->flags |= OBJ_READONLY;
    }
  return v;
}

#define STATIC_STRING(name, value)                      \
static ulong static_data_ ## name;                      \
static const struct static_string name = {              \
  .static_data = &static_data_ ## name,                 \
  .mobj = {                                             \
    .size = sizeof (struct string) + sizeof value,      \
    .garbage_type = garbage_static_string,              \
    .type = type_string,                                \
    .flags = OBJ_IMMUTABLE | OBJ_READONLY               \
  },                                                    \
  .str = value                                          \
}

#define GET_STATIC_STRING(name) ((struct string *)&(name).mobj)

/* How each class of object is structured */

struct gstring
{
  struct obj o;
  char data[];
};

struct grecord
{
  struct obj o;
  struct obj *data[];		/* Pointers to other objects */
};

/* A pointer to a temporary external data structure.
   These disappear when data is reloaded (as the pointers become invalid)
*/
struct gtemp
{
  struct obj o;
  void *external;
};

/* The code structures are somewhat machine-dependent */

struct code
{
  struct obj o;
  struct string *varname;
  struct string *filename;      /* Name on disk */
  struct string *nicename;      /* Pretty-printed file name */
  struct string *help;
  struct vector *arg_types;     /* null for varargs */
  uword lineno;
  uword seclevel;
  ulong return_typeset;
};

#if defined(i386) && !defined(NOCOMPILER)
struct icode
{
  struct code code;
  uword nb_constants;
  uword nb_locals;
  uword stkdepth;
  uword dummy0;
  ulong call_count;		/* Profiling */
  struct string *lineno_data;
  ulong instruction_count;
  ulong dummy1[2];
  ubyte magic_dispatch[8];	/* Machine code jump to interpreter.
				   This is at the same offset as mcode
				   in struct mcode */
  struct obj *constants[/*nb_constants*/];
  /* instructions follow the constants array */
};

struct mcode /* machine-language code object */
{
  struct code code;
  ulong code_length;		/* Length of machine code in bytes */
  struct string *linenos;
  uword nb_constants;
  uword nb_rel;
  uword return_itype;
  ubyte closure_flags;          /* CLF_xxx flags */
  ubyte dummy;
  struct mcode *myself;		/* Self address, for relocation */
  ubyte magic[8];               /* Magic pattern that doesn't occur in code */

  ubyte mcode[/*code_length*/];  /* Aligned on CODE_ALIGNMENT */
  /* Following the machine code:
       - nb_constants offsets of contants in mcode
       - nb_rel relative addresses of C functions in mcode
       Each offset is a uword if code_length <= (uword)~0; otherwise a long.
  */
};

CASSERT(mdispatch, (offsetof(struct mcode, mcode)
                    == offsetof(struct icode, magic_dispatch)));
#endif  /* i386 && !NOCOMPILER */

#ifdef NOCOMPILER
struct icode
{
  struct code code;
  uword nb_constants;
  uword nb_locals;
  uword stkdepth;
  uword dummy;
  ulong call_count;		/* Profiling */
  struct string *lineno_data;
  ulong instruction_count;
  struct obj *constants[/*nb_constants*/];
  /* instructions follow the constants array */
};

struct mcode /* machine-language code object */
{
  struct code code;
  /* Not used when no compiler around ... */
  ubyte closure_flags;          /* CLF_xxx flags */
  ubyte mcode[];
};
#endif  /* NOCOMPILER */

#endif  /* MVALUES_H */
