/* $Log: mvalues.h,v $
 * Revision 1.1  1995/01/22  15:11:48  arda
 * Linux patches.
 *
 * Revision 1.12  1994/10/09  06:43:14  arda
 * Libraries
 * Type inference
 * Many minor improvements
 *
 * Revision 1.11  1994/08/16  19:16:27  arda
 * Mudlle compiler for sparc now fully functional (68k compiler now needs
 * updating for primitives).
 * Changes to allow Sparc trap's for runtime errors.
 * Also added flags to primitives for better calling sequences.
 *
 * Revision 1.9  1994/02/24  08:33:12  arda
 * Owl: New error messages.
 *
 * Revision 1.8  1994/02/03  19:21:52  arda
 * nothing special(2)
 *
 * Revision 1.7  1994/01/29  19:50:40  dgay
 * Owl: add file & line information to functions.
 *
 * Revision 1.6  1993/12/23  20:48:57  dgay
 * Owl: New alloc.c: semi-generational collector.
 *      Included Amiga makefile for convenience.
 *
 * Revision 1.5  1993/04/22  18:59:02  un_autre
 * (MD) & Owl. Bug fixes. /player fixes. EVER_WHINER flag. saving_spells adjusted.
 *
 * Revision 1.4  1993/03/29  09:24:58  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.4  1993/03/17  12:50:19  dgay
 * Fixed GC of help strings in code blocks.
 * Added security features.
 *
 * Revision 1.3  1993/03/14  16:15:31  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.3  1993/01/08  23:57:13  un_mec
 * Owl: Allow characters and objects to appear in mudlle.
 *
 * Revision 1.2  1992/12/30  14:11:01  un_mec
 * Owl:
 * Several changes:
 * - Variables don't have separate value & function cells, instead their are
 *   now 2 types: type_function & type_variable.
 * - print_value: New types (list, vector), printing rationalised.
 * - New type: list (Lisp style pair)
 * - lexer.l: Debug read_from_string
 * - debug_level & DEBUG macro provided to help debugging.
 *
 * Revision 1.1  1992/12/27  21:41:51  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

#ifndef VALUES_H
#define VALUES_H

/* The basic structure of all values */
typedef void *value;

/* Objects are either integers or pointers to more complex things (like variables)
   The low order bit differentiates between the 2, 0 for pointers, 1 for integers
   If the object is a pointer, it is directly valid, if an integer the low order
   bit must be ignored */

#define pointerp(obj) ((obj) && ((long)obj & 1) == 0)
#define integerp(obj) (((long)obj & 1) == 1)

/* Make & unmake integers */
#define intval(obj) ((long)(obj) >> 1)
#define makeint(i) ((value)(((i) << 1) + 1))

struct obj 
{
  ulong size;			/* Total size in bytes, including header */
  ubyte garbage_type;
  ubyte type;
  short flags;			/* Eg read-only */
#ifdef GCDEBUG
  ulong generation;
#endif
};

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

/* The basic classes of all objects, as seen by the garbage collector */
enum { garbage_string, garbage_record, garbage_code, garbage_forwarded,
       garbage_permanent, garbage_temp, garbage_mcode };

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
  struct obj *new;
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
  ubyte *myself;		/* Self address, for relocation */
  ubyte magic[8];		/* A magic pattern that doesn't occur in code */
  ulong mcode[1/*code_length*/];
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
  ubyte magic[8];		/* A magic pattern that doesn't occur in code */
  ubyte mcode[1/*code_length*/];
  /* the constant's offsets follow the machine code */
};
#endif

#ifdef linux
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
  ubyte magic[8];		/* A magic pattern that doesn't occur in code */
  ubyte mcode[1/*code_length*/];
  /* the constant's offsets follow the machine code */
};
#endif

#endif
