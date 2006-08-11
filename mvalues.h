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

#ifndef VALUES_H
#define VALUES_H

#include <stddef.h>
#include "types.h"

/* Objects are either integers or pointers to more complex things (like
   variables) The low order bit differentiates between the 2, 0 for pointers, 1
   for integers If the object is a pointer, it is directly valid, if an integer
   the low order bit must be ignored */

#define pointerp(obj) ((obj) && ((long)(obj) & 1) == 0)
#define integerp(obj) (((long)(obj) & 1) == 1)

/* Make & unmake integers */
#define intval(obj)  ((long)(obj) >> 1)
#define uintval(obj) ((unsigned long)(obj) >> 1)
#define makeint(i)   ((value)(((i) << 1) + 1))

#define MAX_TAGGED_INT ((1 << 30) - 1)
#define MIN_TAGGED_INT (-(1 << 30))

#define OBJ_READONLY 1		/* Used for some values */
#define OBJ_IMMUTABLE 2		/* Contains only pointers to other immutable
				   objects.
				   Its pointers are never modified after 
				   allocation + initialisation (and all
				   initialisation must be done before any other
				   allocation) */

/* True if x is immutable */
#define immutablep(x) \
  (!pointerp((x)) || (((struct obj *)(x))->flags & OBJ_IMMUTABLE) != 0)

/* True if x is readonly */
#define readonlyp(x) \
  (!pointerp((x)) || (((struct obj *)(x))->flags & OBJ_READONLY) != 0)

/* How each class of object is structured */

struct gstring
{
  struct obj o;
  char data[1];
};

struct grecord
{
  struct obj o;
  struct obj *data[1];		/* Pointers to other objects */
};

struct gforwarded
{
  /* the struct obj is replaced by: */
  struct obj *newp;
};

/* A pointer to a temporary external data structure.
   These disappear when data is reloaded (as the pointers become invalid)
*/
struct gtemp
{
  struct obj o;
  void *external;
};

/* A pointer to a permanent external data structure.
   These are identified by a unique # so that they can be found again
   after reboot.
*/
struct gpermanent
{
  struct obj o;
  ulong nb;
  void *external;
  ulong call_count;
};

/* The code structures are somewhat machine-dependent */

#if defined(i386) && !defined(NOCOMPILER)
struct code
{
  struct obj o;
  uword nb_constants;
  uword nb_locals;
  uword stkdepth;
  uword seclevel;
  uword lineno;
  mtype return_type : 16;
  ulong call_count;		/* Profiling */
  ulong instruction_count;
  struct string *varname;
  struct string *filename;
  struct string *help;
  struct string *lineno_data;
  ulong dummy;
  ubyte magic_dispatch[7];	/* Machine code jump to interpreter.
				   This is at the same offset as mcode
				   in struct mcode */
  struct obj *constants[1/*nb_constants*/];
  /* instructions follow the constants array */
};

struct mcode /* machine-language code object */
{
  struct obj o;
  uword seclevel;
  mtype return_type : 16;
  struct string *filename;
  struct string *varname;
  struct string *help;
  struct string *linenos;
  uword lineno;
  uword code_length;		/* Length of machine code in words */
  uword nb_constants;
  uword nb_rel;
  ubyte *myself;		/* Self address, for relocation */
  ubyte magic[8];		/* A magic pattern that doesn't occur in code. 
				   Offset must be multiple of 4 */
  ubyte mcode[1/*code_length*/];
  /* following the machine code:
       - nb_constants offsets of contants in mcode
       - nb_rel relative addresses of C functions in mcode
  */
};

CASSERT(mdispatch, (offsetof(struct mcode, mcode)
                    == offsetof(struct code, magic_dispatch)));
#endif

#ifdef sparc
struct code
{
  struct obj o;
  uword nb_constants;
  uword nb_locals;
  uword stkdepth;
  uword seclevel;
  uword lineno;
  ubyte filler[2];
  ulong call_count;		/* Profiling */
  ulong instruction_count;
  struct string *varname;
  struct string *filename;
  struct string *help;
  struct string *lineno_data;
  ubyte magic_dispatch[16];	/* Machine code jump to interpreter.
				   This is at the same offset as mcode
				   in struct mcode */
  struct obj *constants[1/*nb_constants*/];
  /* instructions follow the constants array */
};

struct mcode /* machine-language code object */
{
  struct obj o;
  uword seclevel;
  uword nb_constants;
  uword code_length;		/* Length of machine code in words */
  uword lineno;
  struct string *filename;
  struct string *varname;
  struct string *help;
  void *filler;
  ubyte *myself;		/* Self address, for relocation */
  ubyte magic[8];		/* magic pattern that doesn't occur in code */
  ulong mcode[1];               /* really of size code_length */
  /* the constant's offsets follow the machine code (they are word
     offsets, not byte offsets) */
};
#endif

#ifdef AMIGA
struct code
{
  struct obj o;
  uword nb_constants;
  uword nb_locals;
  uword stkdepth;
  uword seclevel;
  ulong call_count;		/* Profiling */
  ulong instruction_count;
  struct string *varname;
  struct string *filename;
  struct string *help;
  ubyte magic_dispatch[6];	/* Machine code jump to interpreter.
				   This is at the same offset as mcode
				   in struct mcode */
  uword lineno;
  struct obj *constants[1/*nb_constants*/];
  /* instructions follow the constants array */
};

struct mcode /* machine-language code object */
{
  struct obj o;
  uword seclevel;
  uword nb_constants;
  uword code_length;		/* Length of machine code in bytes */
  uword lineno;
  struct string *filename;
  struct string *varname;
  struct string *help;
  ubyte magic[8];		/* magic pattern that doesn't occur in code */
  ulong mcode[1];               /* really of size code_length */
  /* the constant's offsets follow the machine code */
};
#endif

#ifdef NOCOMPILER
struct code
{
  struct obj o;
  uword nb_constants;
  uword nb_locals;
  uword stkdepth;
  uword seclevel;
  uword lineno;
  mtype return_type : 16;
  ulong call_count;		/* Profiling */
  ulong instruction_count;
  struct string *varname;
  struct string *filename;
  struct string *help;
  struct string *lineno_data;
  struct obj *constants[1/*nb_constants*/];
  /* instructions follow the constants array */
};

struct mcode /* machine-language code object */
{
  struct obj o;
  /* Not used when no compiler around ... */
  struct string *filename;
  struct string *help;
  struct string *varname;
  uword lineno;
  mtype return_type : 16;
  uword seclevel;
  ubyte mcode[1];
};
#endif


#endif
