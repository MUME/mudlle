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

#ifndef MVALUES_H
#define MVALUES_H

#include <limits.h>
#include <stddef.h>

#ifdef __x86_64__
  #include "x64.h"
#elif defined __i386__
  #include "x86.h"
#endif

#include "types.h"

/* increase this as compiled mudlle suffers a backwards-incompatible change */
#define MCODE_VERSION 9

/* Objects are either null, integers or pointers to more complex things (like
   variables). Null is represented by NULL. Integers have the lowest bit set.
   Anything else is a pointer to an object (struct obj *). */

static inline bool is_function(value v)
{
  if (!pointerp(v))
    return false;
  return P(((struct obj *)v)->type) & TYPESET_FUNCTION;
}

static inline bool is_any_primitive(value p)
{
  if (!pointerp(p))
    return false;
  return ((struct obj *)p)->garbage_type == garbage_primitive;
}

#define is_typeset(v, typeset)                  \
  (__builtin_constant_p(typeset)                \
   ? is_const_typeset(v, typeset)               \
   : is_generic_typeset(v, typeset))

__attribute__((always_inline))
static inline bool is_const_typeset(value v, unsigned typeset)
{
  if (typeset == TYPESET_ANY)
    return true;
  if ((typeset & TSET(null)) && v == NULL)
    return true;
  if (integerp(v))
    return typeset & TSET(integer);
  if (!(typeset & TSET(null)) && v == NULL)
    return false;
  struct obj *obj = v;
  typeset &= ~(TSET(null) | TSET(integer));
  switch (typeset & TYPESET_ANY)
    {
    case 0:
      return false;
    case TYPESET_ANY ^ (TSET(null) | TSET(integer)):
      return true;
    case TYPESET_PRIMITIVE:
      return is_any_primitive(obj);
#define __ISTYPE(t, arg)                        \
      case P(type_ ## t):                       \
        return obj->type == type_ ## t;
      FOR_PLAIN_TYPES(__ISTYPE,)
#undef __ISTYPE
    }
  return P(obj->type) & typeset;
}

static inline bool is_generic_typeset(value v, unsigned typeset)
{
  if (v == NULL)
    return typeset & TSET(null);
  if (integerp(v))
    return typeset & TSET(integer);
  struct obj *obj = v;
  return P(obj->type) & typeset;
}

/* Make & unmake integers */
#define intval(obj)  (CHECK_VALUE(obj), (long)(obj) >> 1)
#define uintval(obj) (CHECK_VALUE(obj), (unsigned long)(obj) >> 1)
/* add 0UL to integer-promote to unsigned long */
#define makeint(i)   ((value)((((i) + 0UL) << 1) | 1))

/* return the sum of mudlle integer 'v' and 'l' */
static inline value mudlle_iadd(value v, long l)
{
  return (value)((long)v + 2 * l);
}

enum {
  MAX_MUDLLE_OBJECT_SIZE = 16 * 1024 * 1024,
  MAX_VECTOR_SIZE   = ((MAX_MUDLLE_OBJECT_SIZE - sizeof (struct vector))
                       / sizeof (value)),
  MAX_STRING_SIZE   = (MAX_MUDLLE_OBJECT_SIZE - sizeof (struct string) - 1),
  MAX_TABLE_ENTRIES = ((MAX_MUDLLE_OBJECT_SIZE / sizeof (value) / 2)
                       * 3 / 4 - 1),
  MAX_FUNCTION_ARGS = 2048,
  MAX_LOCAL_VARS    = 4096
};

#define TAGGED_INT_BITS (CHAR_BIT * sizeof (long) - 1)
#define MAX_TAGGED_UINT (ULONG_MAX >> 1)
#define MAX_TAGGED_INT  (LONG_MAX >> 1)
#define MIN_TAGGED_INT  (-MAX_TAGGED_INT - 1)

static inline long mudlle_sign_extend(long l)
{
  struct { long i : TAGGED_INT_BITS; } m = { .i = l };
  return m.i;
}

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

#define STATIC_STRING(name, value)                              \
  static const struct {                                         \
    struct obj o;                                               \
    char str[sizeof value];                                     \
  } name = {                                                    \
    .o = {                                                      \
      .size         = sizeof (struct obj) + sizeof value,       \
      .garbage_type = garbage_static_string,                    \
      .type         = type_string,                              \
      .flags        = OBJ_IMMUTABLE | OBJ_READONLY              \
    },                                                          \
    .str = value                                                \
  }

#define GET_STATIC_STRING(name) ((struct string *)&(name).o)

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

/* The code structures are somewhat machine-dependent */
struct code
{
  struct obj o;
  struct string *varname;
  struct string *filename;      /* Name on disk */
  struct string *nicename;      /* Pretty-printed file name */
  struct string *help;
  union {
    struct obj *obj;
    struct string *vararg;      /* name for varargs */
    struct vector *argv;        /* ... or vector(name|false . typeset) */
  } arguments;
  struct string *linenos;       /* DWARF line number information */
  uint16_t lineno;
  seclev_t seclevel;
  unsigned return_typeset : 24;
  unsigned column : 8;
#ifdef PROFILE_CALL_COUNT
  uint32_t call_count;
#endif
};
CASSERT(P(24) > TYPESET_ANY);

static inline bool code_is_vararg(const struct code *code)
{
  return !TYPE(code->arguments.obj, vector);
}

#if (defined __i386__ || defined __x86_64__) && !defined NOCOMPILER
struct icode
{
  struct code code;
  uint32_t instruction_count;
  uint16_t nb_constants;
  uint16_t nb_locals;
  uint16_t stkdepth;
  uint16_t dummy0;
  ulong dummy1;

  /* Machine code jump to interpreter. This is at the same offset as
     mcode in struct mcode */
#ifdef __i386__
  uint32_t dummy2[2];
  struct magic_dispatch {
    uint8_t movl_ecx;
    void (*invoke)(void);
    uint8_t jmp_ecx[2];
    uint8_t nop1[1];
  } __attribute__((__packed__)) magic_dispatch;
#elif defined __x86_64__
  struct magic_dispatch {
    uint8_t movq_r11[2];
    void (*invoke)(void);
    uint8_t jmpq_r11[3];
    uint8_t nop3[3];
  } __attribute__((__packed__)) magic_dispatch;
#else
  #error Unsupported architecture
#endif
  value constants[/*nb_constants*/];
  /* instructions follow the constants array */
};

struct mcode /* machine-language code object */
{
  struct code code;
#ifdef __i386__
  struct mcode *myself;		/* Self address, for relocation */
#endif
  uint32_t code_length;		/* Length of machine code in bytes */
  uint16_t nb_constants;
#ifdef __i386__
  uint16_t nb_rel;
#elif defined __x86_64__
  uint16_t nb_pc_rel;           /* %rip-relative constants */
#endif
  uint16_t return_itype;
  uint8_t closure_flags;        /* CLF_xxx flags */

  bool dwarf_seen : 1;
  unsigned : 7;

#ifdef __x86_64__
  uint32_t dummy;
#endif

  uint8_t magic[8];             /* Magic pattern that doesn't occur in code */

  uint8_t mcode[/*code_length*/]; /* Aligned on CODE_ALIGNMENT */
  /* Following the machine code:
       - nb_pc_rel addresses of C functions (not for i386)
       - nb_constants offsets of contants in mcode
       - nb_rel relative addresses of C functions in mcode (not for x86-64)
       Each offset is a uint16_t if code_length <= UINT16_MAX;
       otherwise a uint32_t.
  */
};

CASSERT(offsetof(struct mcode, mcode)
        == offsetof(struct icode, magic_dispatch));

struct mcode_fields {
  void *cst_offsets;
  uint32_t code_size;
  unsigned code_pad;
  uint32_t (*get_cst_ofs)(void **srcp);
  void *(*add_cst_ofs)(void *dst, uint32_t ofs);
};

void mcode_fields(struct mcode_fields *f, const struct mcode *mcode);
void mcode_fields_spec(struct mcode_fields *f, char *mcode,
                       ulong code_len, uint32_t nb_pc_rel,
                       uint32_t nb_rel, uint32_t nb_constants);

#endif  /* (__i386__ || __x86_64__) && !NOCOMPILER */

#ifdef NOCOMPILER
struct icode
{
  struct code code;
  uint16_t nb_constants;
  uint16_t nb_locals;
  uint16_t stkdepth;
  uint16_t dummy;
  ulong call_count;		/* Profiling */
  struct string *lineno_data;
  ulong instruction_count;
  value constants[/*nb_constants*/];
  /* instructions follow the constants array */
};

struct mcode /* machine-language code object */
{
  struct code code;
  /* Not used when no compiler around ... */
  uint8_t closure_flags;          /* CLF_xxx flags */
  uint8_t mcode[];
};
#endif  /* NOCOMPILER */

#endif  /* MVALUES_H */
