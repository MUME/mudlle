/* $Log: alloc.c,v $
 * Revision 1.39  1995/07/16  09:16:46  arda
 * Add GCSTATS option.
 * Misc bug fixes.
 *
 * Revision 1.38  1995/07/15  15:24:09  arda
 * Context cleanup.
 * Remove GCDEBUG.
 *
 * Revision 1.37  1995/01/22  15:11:35  arda
 * Linux patches.
 *
 * Revision 1.36  1994/10/09  06:41:41  arda
 * Libraries
 * Type inference
 * Many minor improvements
 *
 * Revision 1.35  1994/09/16  13:07:07  arda
 * Rename protect to catch.
 * New protect/unprotect functions (like dynpro/undynpro).
 *
 * Revision 1.34  1994/09/15  19:46:33  arda
 * Performance improvements:
 *   setjmp -> _setjmp (setjmp is horrendously slow)
 *   cold_protect
 * reset_limits split from reset_interpreter
 * fix division of negative numbers
 * Add ?\{n,r,t}
 * gc_size returns "mutable" size
 *
 * Revision 1.33  1994/09/11  11:24:05  arda
 * Bug fixes.
 *
 * Revision 1.32  1994/09/06  07:50:25  arda
 * Constant support: detect_immutability, global_set!, string_{i}search.
 *
 * Revision 1.31  1994/08/31  16:57:17  arda
 * *** empty log message ***
 *
 * Revision 1.30  1994/08/29  15:44:50  arda
 * Mudlle stuff
 *
 * Revision 1.29  1994/08/29  13:17:09  arda
 * Contagious immutability.
 * Global array of values instead of variables.
 * Direct recursion.
 *
 * Revision 1.28  1994/08/26  04:35:34  arda
 * Fix bug in builtins.
 * Check gen0 size by type.
 *
 * Revision 1.27  1994/08/22  11:18:18  arda
 * Moved code allocation to ins.c
 * Changes for mudlle compiler in MUME.
 *
 * Revision 1.26  1994/08/16  19:15:42  arda
 * Mudlle compiler for sparc now fully functional (68k compiler now needs
 * updating for primitives).
 * Changes to allow Sparc trap's for runtime errors.
 * Also added flags to primitives for better calling sequences.
 *
 * Revision 1.23  1994/02/24  08:32:44  arda
 * Owl: New error messages.
 *
 * Revision 1.22  1994/02/12  17:24:36  arda
 * Owl: Better code generated.
 *
 * Revision 1.21  1994/02/11  09:58:31  dgay
 * Owl: -Wall
 *      new shared string handling
 *      configuration file
 *
 * Revision 1.20  1994/02/03  19:21:25  arda
 * nothing special(2)
 *
 * Revision 1.19  1994/01/29  19:50:19  dgay
 * Owl: add file & line information to functions.
 *
 * Revision 1.18  1994/01/27  21:59:27  dgay
 * Owl: Improve the collector (yet again).
 *      Now has just one zone for generation 0 (one extra copy involved).
 *
 * Revision 1.17  1994/01/27  17:07:55  arda
 * Hmm.
 *
 * Revision 1.16  1994/01/07  13:09:25  arda
 * Owl: Spec countdown continues.
 *
 * Revision 1.15  1993/12/29  10:50:18  arda
 * divers
 *
 * Revision 1.14  1993/12/27  13:32:01  arda
 * version buggee a david
 *
 * Revision 1.13  1993/12/26  12:12:17  arda
 * Fixed 1 year old bug.
 *
 * Revision 1.12  1993/12/25  21:43:41  arda
 * Owl: Fixed bug in major_collection (wrong value for endgen1).
 *      Messages for major collections.
 *
 * Revision 1.11  1993/12/24  11:44:24  arda
 * *** empty log message ***
 *
 * Revision 1.10  1993/12/23  20:48:47  dgay
 * Owl: New alloc.c: semi-generational collector.
 *      Included Amiga makefile for convenience.
 *
 * Revision 1.9  1993/12/06  19:20:47  arda
 * divers CLI
 *
 * Revision 1.8  1993/11/27  11:28:53  arda
 * Owl: Major changes to affect.
 *      Save mudlle data with players & objects.
 *      Change skill format on disk.
 *      Other minor changes.
 *      Still needs full debugging.
 *
 * Revision 1.7  1993/04/22  18:58:29  un_autre
 * (MD) & Owl. Bug fixes. /player fixes. EVER_WHINER flag. saving_spells adjusted.
 *
 * Revision 1.6  1993/03/29  09:23:34  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.5  1993/03/17  12:49:15  dgay
 * Fixed GC of help strings in code blocks.
 * Added security features.
 *
 * Revision 1.4  1993/03/16  10:44:10  dgay
 * Optimisations continue.
 *
 * Revision 1.3  1993/03/14  16:13:45  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.5  1993/01/30  12:13:30  un_mec
 * Owl: Mudlle reactions installed, with loading and editing commands.
 *
 * Revision 1.4  1993/01/18  22:22:54  un_mec
 * Owl: Yucky GC bugs. Why didn't they happen earlier ??
 *
 * Revision 1.3  1993/01/08  23:57:03  un_mec
 * Owl: Allow characters and objects to appear in mudlle.
 *
 * Revision 1.2  1992/12/30  14:10:42  un_mec
 * Owl:
 * Several changes:
 * - Variables don't have separate value & function cells, instead their are
 *   now 2 types: type_function & type_variable.
 * - print_value: New types (list, vector), printing rationalised.
 * - New type: list (Lisp style pair)
 * - lexer.l: Debug read_from_string
 * - debug_level & DEBUG macro provided to help debugging.
 *
 * Revision 1.1  1992/12/27  21:40:52  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

static char rcsid[] = "$Id: alloc.c,v 1.39 1995/07/16 09:16:46 arda Exp $";

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <setjmp.h>
#include <stddef.h>
#include "mudlle.h"
#include "alloc.h"
#include "utils.h"
#include "types.h"
#include "runtime/basic.h"
#include "builtins.h"
#ifdef MUME
#include "runtime/invoke.h"
#endif

/* A semi-generational GC:
   - is copying for mutable data
   - a generational for immutable data.

   A few principles:
   - immutable data may not point at mutable data
   - on each minor GC, the mutable data stays in gen 0, the immutable data
     moves to gen 1

   Advantages:
   - 2/3 of the data is immutable and doesn't change much
     => a lot of effort is wasted in copying it.
   - the allocation rate of very short lived information is rather
     high (see above)
   - Avoids having to keep track of assignments (esp. painful for the C code)
   - the interpreter can easily enforce the 'immutable' clause.

   Disadvantages:
   - More implementation effort => more bugs
   - What will happen to the memory requirements ?
     (copying is 2x, generational is 3x, this may well be 4x)
   - On each minor GC, all the mutable data is copied
     => minimise amount of mutable data ?
   - GCDEBUG becomes more complicated
*/

/* New: generation of machine code complicates garbage collection:
    - registers are now roots
    - pc & (processor) stack are roots, but do not point to beginning
      of objects
    - code under execution may move
    - flow of control may thread from C to machine code to C to ...

   ==> each call to machine code has an area reserved for it to save
    its registers and stack when it calls C code. This area also contains the
    stack pointer value when the machine code was called.

    The set of all such active areas serves as GC roots. The stack areas delimited
    in these areas are also examined for pointers into code objects.

    The beginning of code objects is recognised by a special magic sequence
    which is never generated as machine code (currently 8 bytes of 0xff is
    used).

    The instruction cache must be flushed after garbage collection.

    Important optimisation: if no machine code objects are seen during
    GC, there is no need to flush the cache (i.e. most minor collections).
*/

int junk;

/* Roots */
struct gcpro *gcpro;		/* Local (C) variables which need protection */
struct gcpro_list *gcpro_list;	/* List of values which need protection */

static struct dynpro first, last;
#define MAXROOTS 100
static value *roots[MAXROOTS];
static ulong last_root;

/* machine code roots */
static void forward_registers(void);
static INLINE void save_restore_mcode(struct mcode *code);
static INLINE void scan_mcode(struct mcode *code);
void flush_icache(ubyte *from, ubyte *to);

#if 0
/* External permanent pointers */
#define MAX_PERMANENT 1024
static void *permanent_obj[MAX_PERMANENT];
#endif

#define ALIGN(x, n) ((x) + ((n) - 1) & ~((n) - 1))

void staticpro(value *pro)
{
  assert (last_root < MAXROOTS);
  roots[last_root++] = pro;
}

void dynpro(struct dynpro *what, value obj)
{
  assert(!what->next); assert(!what->prev);
  GCCHECK(obj);
  what->next = first.next;
  what->prev = &first;
  what->obj = obj;
  what->next->prev = what;
  first.next = what;
}

void undynpro(struct dynpro *what)
{
  what->prev->next = what->next;
  what->next->prev = what->prev;
  what->obj = NULL;
  what->prev = what->next = NULL;
}

struct dynpro *protect(value v)
{
  struct dynpro *new = xmalloc(sizeof *new);

  new->next = new->prev = NULL;
  dynpro(new, v);

  return new;
}

value unprotect(struct dynpro *pro)
{
  value v = pro->obj;

  undynpro(pro);
  free(pro);

  return v;
}

/* A semi-generational garbage collector */

/* The GC block */
static ubyte *block;
static ulong blocksize;

/* Current allocation state */

/* Memory for generation 0 extends from startgen0 to endgen0. Allocation
   is downward, with posgen0 pointing at the start of the last allocation.
*/
ubyte *startgen0, *endgen0, *posgen0;
static ulong minor_offset;

/* During GC, the newstart0, newend0, newpos0 variables fulfill the same roles
   for the next copy of generation 0
*/
static ubyte *newstart0, *newend0, *newpos0;

/* Memory for generation 1 extends from startgen1 to endgen1. No memory is
   allocated directly. */
ubyte *startgen1, *endgen1;
static ulong oldsize1;

/* During GC, newstart1, newend1 and newpos1 indicate the memory for the next
   copy of generation 1. Allocation is upwards, with newpos1 pointing at the
   first byte after the last allocation. */
static ubyte *newstart1, *newend1, *newpos1;

/* During a major GC we may run out of memory. In that case we spill to a
   temporary block, then increase the GC area. */
static ubyte *tempblock1, *oldpos1, *oldstart1;
static ulong tempsize1;

static int newarea;		/* TRUE during new_major_collection */
static ulong major_offset;
static int saw_mcode;		/* TRUE if forwarded an mcode object */

static int (*special_forward)(struct obj **ptr);

ulong minorgen, majorgen;
static ulong newminorgen, newmajorgen;
#ifdef GCSTATS
struct gcstats gcstats;
#endif

/* Range affected by current GC */
ubyte *gcrange_start, *gcrange_end;

/* Garbage collector initialisation & cleanup */
void garbage_init(void)
{
  ulong half;

  blocksize = INITIAL_BLOCKSIZE;
  block = xmalloc(blocksize);

  /* No first generation yet */
  startgen1 = endgen1 = block;

  /* Divide remainder into 2 */
  half = (blocksize / 2) & ~(sizeof(value) - 1);
  posgen0 = endgen0 = block + blocksize;
  startgen0 = endgen0 - half;
  
  first.prev = NULL; first.next = &last;
  last.prev = &first; last.next = NULL;

#ifdef GCDEBUG
  minorgen = 98146523;		/* Must be odd */
  majorgen = 19643684;		/* Must be even */
#endif
}

void garbage_cleanup(void)
{
  if (block) free(block);
  block = NULL;
}

INLINE static value move_object(struct obj *obj, struct obj *newobj, long newgen)
{
  long size = obj->size;

  assert(obj->garbage_type <= garbage_mcode);

  GCCHECK(obj);
  memcpy(newobj, obj, size);
#ifdef GCDEBUG
  newobj->generation = newgen;
#endif
  obj->garbage_type = garbage_forwarded;
  newobj = (struct obj *)((ubyte *)newobj + major_offset);
  obj->size = (long)newobj;

  return newobj;
}

INLINE static ubyte *scan(ubyte *ptr)
{
  struct obj *obj = (struct obj *)ptr;

  ptr += ALIGN(obj->size, sizeof(value));

  switch (obj->garbage_type)
    {
    case garbage_record: {
      struct grecord *rec = (struct grecord *)obj;
      struct obj **o, **recend;

      recend = (struct obj **)((ubyte *)rec + rec->o.size);
      o = rec->data;
      while (o < recend)
	{
	  if (pointerp(*o)) special_forward(o);
	  o++;
	}
      break;
    }
    case garbage_code: {
      struct code *code = (struct code *)obj;
      struct obj **c, **cend;

      saw_mcode = TRUE;		/* Code objects contain a jump to the interpreter */
      if (pointerp(code->help)) special_forward((struct obj **)&code->help);
      if (pointerp(code->filename)) special_forward((struct obj **)&code->filename);
      if (pointerp(code->varname)) special_forward((struct obj **)&code->varname);
      c = code->constants;
      cend = code->constants + code->nb_constants;
      while (c < cend)
	{
	  if (pointerp(*c)) special_forward(c);
	  c++;
	}
      break;
    }
    case garbage_mcode: 
      saw_mcode = TRUE;
      scan_mcode((struct mcode *)obj);
      break;
#if 0
    /* Sanity checks */
    case garbage_permanent: case garbage_temp: case garbage_string: 
      break;
    default:
      fprintf(stderr, "gc: unknown garbage type %d\n",
	      obj->garbage_type);
      garbage_cleanup();
      exit(2);
#endif
    }
  return ptr;
}

static void safe_forward(value *ptr)
{
  /* A C variable may be protected several times. It should be forwarded
     only once */
  if (pointerp(*ptr) && !((ulong)*ptr & 0x2)) /* Hack */
    {
      special_forward((struct obj **)ptr);
      *ptr = (value)((ulong)*ptr | 0x2);
    }
}

static void unmark_safe(value *ptr)
{
  if (pointerp(*ptr)) *ptr = (value)((ulong)*ptr & ~0x2);
}

static void forward_roots(void)
/* Forward all roots */
{
  struct gcpro *protect;
  struct gcpro_list *protect_list;
  struct dynpro *dyn;
  ulong i;
  value **root;
  struct call_stack *stk;

  /* Forward activation stack */
  forward_registers();

  /* Static roots */
  for (root = roots, i = 0; i < last_root; root++, i++) safe_forward(*root);

  /* Dynamic roots */
  for (dyn = first.next; dyn->next; dyn = dyn->next)
    if (pointerp(dyn->obj)) special_forward((struct obj **)&dyn->obj);

  /* Forward protected C vars */
  for (protect = gcpro; protect; protect = protect->next)
    safe_forward(protect->obj);
  
  /* Forward protected C lists */
  for (protect_list = gcpro_list; protect_list; protect_list = protect_list->next)
    {
      struct local_value *val;

      for (val = protect_list->cl->first; val; val = val->next)
	safe_forward(&val->value);
    }

  /* Forward the call stack */
  for (stk = call_stack; stk; stk = stk->next)
    {
      switch (stk->type)
	{
	case call_c:
	  if (pointerp(stk->u.c.arg1)) special_forward((struct obj **)&stk->u.c.arg1);
	  if (pointerp(stk->u.c.arg2)) special_forward((struct obj **)&stk->u.c.arg2);
	  if (pointerp(stk->u.c.arg3)) special_forward((struct obj **)&stk->u.c.arg3);
	  if (pointerp(stk->u.c.arg4)) special_forward((struct obj **)&stk->u.c.arg4);
	  if (pointerp(stk->u.c.arg5)) special_forward((struct obj **)&stk->u.c.arg5);
	  break;

	case call_bytecode:
	  if (pointerp(stk->u.mudlle.fn))
	    special_forward((struct obj **)&stk->u.mudlle.fn);
	  if (pointerp(stk->u.mudlle.code))
	    special_forward((struct obj **)&stk->u.mudlle.code);
	  if (pointerp(stk->u.mudlle.locals))
	    special_forward((struct obj **)&stk->u.mudlle.locals);
	  break;
	}
    }

#ifdef MUME
  forward_mume_roots(special_forward);
#endif

  /* Static roots */
  for (root = roots, i = 0; i < last_root; root++, i++) unmark_safe(*root);

  /* Forward protected C vars */
  for (protect = gcpro; protect; protect = protect->next)
    unmark_safe(protect->obj);
  
  /* Forward protected C lists */
  for (protect_list = gcpro_list; protect_list; protect_list = protect_list->next)
    {
      struct local_value *val;

      for (val = protect_list->cl->first; val; val = val->next)
	unmark_safe(&val->value);
    }

}

INLINE static int minor_forward(struct obj **ptr)
/* The forward function for minor collections */
{
  struct obj *obj = *ptr, *newobj;
  long size;

  size = obj->size;

  if (obj->garbage_type == garbage_forwarded)
    {
      *ptr = (value)size;
      return (ubyte *)size < newpos1; /* true iff forwarded to gen1 */
    }
  else
    {
      if (obj->flags & OBJ_IMMUTABLE)
	{
	  /* Immutable, forward to generation 1 */

	  /* In minor collections only bother with generation 0 */
	  if ((ubyte *)obj < newpos1) return TRUE;

	  /* Gen 1 grows upward */
	  newobj = (struct obj *)newpos1;
	  newpos1 += ALIGN(size, sizeof(value));
	  memcpy(newobj, obj, size);

#ifdef GCSTATS
	  gcstats.g1nb[obj->type]++;
	  gcstats.g1sizes[obj->type] += size;
#endif
#ifdef GCDEBUG
	  newobj->generation = newmajorgen;
#endif

	  obj->garbage_type = garbage_forwarded;
	  obj->size = (long)newobj;
	  *ptr = newobj;

	  return TRUE;
	}
      else
	{
	  /* Mutable, forward to generation 0 */

	  /* Must have been in gen 0 before */
	  /*assert((ubyte *)obj >= startgen0 && (ubyte *)obj < endgen0);*/

	  newobj = (struct obj *)(newpos0 -= ALIGN(size, sizeof(value)));
	  /*assert(newpos0 >= newstart0);*/
	  memcpy(newobj, obj, size);

#ifdef GCSTATS
	  gcstats.g0nb[obj->type]++;
	  gcstats.g0sizes[obj->type] += size;
#endif
#ifdef GCDEBUG
	  newobj->generation = newminorgen;
#endif

	  obj->garbage_type = garbage_forwarded;
	  newobj = (struct obj *)((ubyte *)newobj + minor_offset);
	  obj->size = (long)newobj;
	  *ptr = newobj;

	  return FALSE;
	}
    }
}

INLINE static ubyte *minor_scan(ubyte *ptr)
{
  struct obj *obj = (struct obj *)ptr;

  ptr += ALIGN(obj->size, sizeof(value));

  /* Generation 0 never contains code, so only records contain pointers */
  if (obj->garbage_type == garbage_record)
    {
      struct grecord *rec = (struct grecord *)obj;
      struct obj **o, **recend;

      recend = (struct obj **)((ubyte *)rec + rec->o.size);
      o = rec->data;

      if (rec->o.flags & OBJ_READONLY)
	{
	  int imm = TRUE;

	  /* Scan & check if it can be made immutable */
	  while (o < recend)
	    {
	      if (pointerp(*o)) imm = imm & minor_forward(o);
	      o++;
	    }
	  /* Readonly objects can be made immutable if all their contents
	     are themselves immutable.
	     Note that this will not detect recursive immutable objects */
	  if (imm) rec->o.flags |= OBJ_IMMUTABLE;
	}
      else
	{
	  while (o < recend)
	    {
	      if (pointerp(*o)) minor_forward(o);
	      o++;
	    }
	}
    }
  return ptr;
}

static void minor_collection(void)
{
  ubyte *unscanned0, *oldstart0, *data;
  int i;
  ulong nsize0;

  saw_mcode = FALSE;
  special_forward = minor_forward;
  major_offset = 0;

  /* Allocate below current block, but move it up afterwards */
  newpos0 = newend0 = startgen0;
  newstart0 = endgen1;
  minor_offset = endgen0 - newend0;

  newstart1 = startgen1;
  newpos1 = endgen1;		/* Add to end of gen1 */
  newend1 = newend0;

  gcrange_start = startgen0; gcrange_end = endgen0;

#ifdef GCDEBUG
  newminorgen = minorgen + 2;
  newmajorgen = majorgen;
#endif
#ifdef GCSTATS
  /* Minor stats */
  gcstats.minor_count++;
  for (i = 0; i < last_type; i++) gcstats.g0nb[i] = gcstats.g0sizes[i] = 0;
#endif
  forward_roots();

  /* Scan mutable copied data */
  /* Must scan forwards, but data is allocated downwards => double loop */
  unscanned0 = newend0;		/* Upper bound of unscanned data */
  do {
    oldstart0 = data = newpos0;
    while (data < unscanned0) data = minor_scan(data);
    assert(data == unscanned0);
    unscanned0 = oldstart0;
  } while (oldstart0 != newpos0); /* Till nothing forwarded */

  /* Scan immutable data */
  data = endgen1;
  while (data < newpos1) data = scan(data);
  assert(data == newpos1);

  /* Move new generation 0 into place */
  nsize0 = newend0 - newpos0;
  posgen0 = endgen0 - nsize0;
  memmove(posgen0, newpos0, nsize0);

  /* Flush cache if necessary */
  if (saw_mcode) flush_icache(endgen1, newpos1);

  /* Reset blocks */
  endgen1 = newpos1;
  /* endgen0 is unchanged */
  /* Divide available mem into 2 */
  nsize0 = (endgen0 - endgen1) / 2 & ~(sizeof(value) - 1);
  startgen0 = endgen0 - nsize0;

  /* NOTE: Here startgen0 may be > posgen0 !!! */

#ifdef GCDEBUG
  minorgen = newminorgen;
#endif
#ifdef GCSTATS
  gcstats.usage_minor = endgen0 - posgen0;
  gcstats.usage_major = endgen1 - startgen1;
#endif
}

static void scan_temp(ubyte *data)
{
  /* Scan main & temp blocks */
  while (data < oldpos1) data = scan(data);
  assert(data == oldpos1);

  data = newstart1;
  while (data < newpos1) data = scan(data);
  assert(data == newpos1);
}


static int major_forward(struct obj **ptr)
{
  struct obj *obj = *ptr, *newobj;
  long size = obj->size, newgen;

  if (obj->garbage_type == garbage_forwarded)
    *ptr = (value)size;
  else
    {
      /* Objects in generation 0 stay in generation 0 during major 
	 collections (even if they are immutable)
	 (Otherwise you can't scan them for roots to gen 1) */

      if ((ubyte *)obj >= posgen0 && (ubyte *)obj < endgen0)
	{
	  if (!newarea) return FALSE;	/* No gen 0 data needs copying */

	  /* forward to new generation 0 */
	  newobj = (struct obj *)(newpos0 -= ALIGN(size, sizeof(value)));
	  assert(newpos0 >= newstart0);
	  newgen = newminorgen;
#ifdef GCSTATS
	  gcstats.g0nb[obj->type]++;
	  gcstats.g0sizes[obj->type] += size;
#endif
	}
      else
	{
	  /* forward to new generation 1 */

	  /* Grows upward */
	  newobj = (struct obj *)newpos1;
	  newpos1 += ALIGN(size, sizeof(value));

	  if (newpos1 > newend1) /* We are in trouble, spill to temp block */
	    {
	      /* Allocate a block big enough to handle the spill */
	      assert(!tempblock1 && !newarea); /* Only once ! */
	      oldpos1 = (ubyte *)newobj; oldstart1 = newstart1;

	      tempsize1 = (endgen1 - startgen1) - (oldpos1 - oldstart1);
	      assert(size <= tempsize1);
	      tempblock1 = xmalloc(tempsize1);
	      newstart1 = tempblock1;
	      newend1 = tempblock1 + tempsize1;
	      newpos1 = tempblock1 + ALIGN(size, sizeof(value));
	      major_offset = 0;

	      newobj = (struct obj *)tempblock1;
	    }
#ifdef GCSTATS
	  gcstats.g1nb[obj->type]++;
	  gcstats.g1sizes[obj->type] += size;
#endif
	  newgen = newmajorgen;
	}
      *ptr = move_object(obj, newobj, newgen);
    }
  return FALSE;
}

static void major_collection(void)
{
  ulong nsize0;
  ubyte *data;
  int i;

  saw_mcode = FALSE;
  newarea = FALSE;
  special_forward = major_forward;
  major_offset = startgen1 - endgen1;

  newstart0 = newpos0 = newend0 = NULL;

  newstart1 = newpos1 = endgen1; newend1 = posgen0;
  
#ifdef GCDEBUG
  newminorgen = -1;
  newmajorgen = majorgen + 2;
#endif
#ifdef GCSTATS
  /* Make stats */
  gcstats.major_count++;
  for (i = 0; i < last_type; i++) gcstats.g1nb[i] = gcstats.g1sizes[i] = 0;
#endif

  gcrange_start = block; gcrange_end = block + blocksize;
  forward_roots();

  /* Generation 0 is also a set of roots */
  data = posgen0;
  while (data < endgen0) data = scan(data);
  assert(data == endgen0);

  /* Scan generation 1, handling spill block overflow */
  if (tempblock1)
    scan_temp(oldstart1);
  else
    {
      data = newstart1;
      while (data < newpos1)
	{
	  data = scan(data);
	  if (tempblock1)	/* We spilt to a temp block */
	    {
	      scan_temp(data);
	      break;
	    }
	}
    }

  if (tempblock1)		/* We ran out of memory ! */
    {
      /* Just move block, and return. A new major collection will be started
	 to handle the problem */
      memmove(startgen1, oldstart1, oldpos1 - oldstart1);
    }
  else
    {
      /* Reset & move generation 1 */
      memmove(startgen1, newstart1, newpos1 - newstart1);
      oldsize1 = newpos1 - newstart1;
      endgen1 = startgen1 + oldsize1;
      /* Flush cache */
      if (saw_mcode) flush_icache(startgen1, endgen1);

      /* Reset generation 0 */
      /* Divide available mem into 2 */
      nsize0 = (endgen0 - endgen1) / 2 & ~(sizeof(value) - 1);
      startgen0 = endgen0 - nsize0;

      /* NOTE: Here startgen0 may be > posgen0 !!! */
    }
#ifdef GCDEBUG
  majorgen = newmajorgen;
#endif
#ifdef GCSTATS
  gcstats.usage_major = endgen1 - startgen1;
#endif
}

static void new_major_collection(ulong newsize)
{
  ubyte *newblock = xmalloc(newsize), *data, *oldstart0, *unscanned0;
  ulong nsize0;
  int i;

  saw_mcode = FALSE;
  newarea = TRUE;
  major_offset = 0;
  special_forward = major_forward;

  /* Setup areas */
  newpos0 = newend0 = newblock + newsize;
  newstart0 = newend0 - (endgen0 - posgen0);

  newpos1 = newstart1 = newblock;
  newend1 = newstart1 + newsize;

#ifdef GCDEBUG
  newminorgen = minorgen + 2;
  newmajorgen = majorgen + 2;
#endif
#ifdef GCSTATS
  /* Make stats */
  gcstats.size = newsize;
  gcstats.minor_count++;
  gcstats.major_count++;
  for (i = 0; i < last_type; i++)
    gcstats.g1nb[i] = gcstats.g1sizes[i] = gcstats.g0nb[i] = gcstats.g0sizes[i] = 0;
#endif
  gcrange_start = block; gcrange_end = block + blocksize;
  forward_roots();

  /* Scan mutable copied data */
  /* Must scan forwards, but data is allocated downwards => double loop */
  unscanned0 = newend0;		/* Upper bound of unscanned data */
  do {
    oldstart0 = data = newpos0;
    while (data < unscanned0) data = scan(data);
    assert(data == unscanned0);
    unscanned0 = oldstart0;
  } while (oldstart0 != newpos0); /* Till nothing forwarded */

  /* Scan generation 1 */
  data = newstart1;
  while (data < newpos1) data = scan(data);
  assert(data == newpos1);
  assert(newpos1 <= newpos0);

  /* Remove old block */
  free(block);
  if (tempblock1) free(tempblock1);
  tempblock1 = NULL;
  block = newblock;
  blocksize = newsize;

  /* Reset generation 1 */
  startgen1 = newstart1;
  endgen1 = newpos1;
  oldsize1 = endgen1 - startgen1;
  if (saw_mcode) flush_icache(startgen1, endgen1);

  /* Reset generation 0 */
  endgen0 = newend0;
  posgen0 = newpos0;
  /* Divide available mem into 2 */
  nsize0 = (endgen0 - endgen1) / 2 & ~(sizeof(value) - 1);
  startgen0 = endgen0 - nsize0;

  assert(startgen0 <= posgen0);

#ifdef GCDEBUG
  minorgen = newminorgen;
  majorgen = newmajorgen;
#endif
#ifdef GCSTATS
  gcstats.usage_major = endgen1 - startgen1;
  assert(gcstats.usage_minor == endgen0 - posgen0);
#endif
}

void garbage_collect(long n)
{
  ulong newsize, need, size1, used0;
  int i;

#if 0
  fprintf(stderr, "GC %ld ...", n); fflush(stderr);
#endif

#ifdef GCSTATS
  /* Copy allocation stats */
  gcstats.size = blocksize;
  for (i = 0; i < last_type; i++)
    {
      gcstats.lnb[i] = gcstats.anb[i]; gcstats.anb[i] = 0;
      gcstats.lsizes[i] = gcstats.asizes[i]; gcstats.asizes[i] = 0;
    }
#endif

  /* No space, try a minor collection */
  minor_collection();
#if 0
  fprintf(stderr, "minor\n"); fflush(stderr);
#endif

  /* If block is too full, do a major collection.
     not(Too full) is:
       - startgen0 < posgen0
       - At least 2x generation 1 size left
       - generation 1 has not doubled in size
       - minor area not THRESHOLD_MAJOR % full. */
  if (startgen0 < posgen0 &&
      2 * (endgen1 - startgen1) < blocksize - (endgen0 - (posgen0 - n)) &&
      (endgen1 - startgen1) < 2 * oldsize1 &&
      (endgen0 - (posgen0 - n)) * THRESHOLD_MAJOR_B <
      (endgen0 - startgen0) * THRESHOLD_MAJOR_A) 
    return;

#ifdef GCSTATS
  fflush(stdout);
  fprintf(stderr, "MUDLLE: Major collection: gen0: %d of %d, gen1: %d, blocksize: %ld\n",
	  endgen0 - (posgen0 - n), endgen0 - startgen0, endgen1 - startgen1, blocksize);
  if (startgen0 >= posgen0)
    fprintf(stderr, "MUDLLE: generation 0 lacks space\n");
  else if (2 * (endgen1 - startgen1) >= blocksize - (endgen0 - (posgen0 - n)))
    fprintf(stderr, "MUDLLE: Cause: not 2x generation 1\n");
  else if ((endgen1 - startgen1) >= 2 * oldsize1)
    fprintf(stderr, "MUDLLE: Generation 1 has doubled in size, was %ld\n", oldsize1);
  else
    fprintf(stderr, "MUDLLE: Cause generation 0 too full\n");
#endif

  major_collection();

#ifdef GCSTATS
  if (!tempblock1)
    fprintf(stderr, "MUDLLE: Now: gen0: %d of %d, gen1: %d\n",
	    endgen0 - (posgen0 - n), endgen0 - startgen0, endgen1 - startgen1);
#endif

  /* If block is too full, increase its size
     Too full is:
       - had to allocate a spill block for the major collection
       - no space for the minor area
       - not much space for the major area (at least 2x)
       - not much space for the minor area (not more than THRESHOLD_MINOR % full)
         (to avoid constant garbage collection as memory usage approaches block size)
  */
  if (tempblock1)
    size1 = (newpos1 - newstart1) + (oldpos1 - oldstart1);
  else
    size1 = endgen1 - startgen1;
  used0 = endgen0 - (posgen0 - n);
  if (!tempblock1 &&
      size1 * 2 + used0 < blocksize &&
      startgen0 < posgen0 &&
      used0 * THRESHOLD_INCREASE_B <
      (endgen0 - startgen0) * THRESHOLD_INCREASE_A) 
    return;

  /* Running out of memory. Increase block size */
  newsize = (blocksize * INCREASE_A) / INCREASE_B;
  newsize = ALIGN(newsize, sizeof(long));

#ifdef GCSTATS
  fprintf(stderr, "MUDLLE: Increasing block size to %ld\n", newsize);
  fprintf(stderr, "MUDLLE: Stats: gen 0: %ld of %d, gen1: %ld\n",
	  used0, endgen0 - startgen0, size1);
  if (tempblock1)
    fprintf(stderr, "MUDLLE: Cause: spill block of %ld bytes\n", tempsize1);
  else if (size1 * 2 + used0 >= blocksize)
    fprintf(stderr, "MUDLLE: Cause: not 2x generation 1 size (%ld) available\n", size1);
  else if (startgen0 >= posgen0)
    fprintf(stderr, "MUDLLE: Cause: not enough space for both generations\n");
  else
    fprintf(stderr, "MUDLLE: Cause: generation 0 is too full (used %ld, size %d)\n",
	    used0, endgen0 - startgen0);
#endif

  /* And make sure there will be n bytes available and that generation 0 is
     only THRESHOLD_INCREASE full. */
  need = size1 + 2 * ((THRESHOLD_INCREASE_B * used0) / THRESHOLD_INCREASE_A);
  if (need > newsize)
    {
#ifdef GCSTATS
      fprintf(stderr, "MUDLLE: Reincreasing block size to %ld to allow 2x generation0\n",
	      need);
#endif
      newsize = need;
    }

  /* And that there will be enough major space */
  need = 3 * size1 + used0;
  if (need > newsize)
    {
#ifdef GCSTATS
      fprintf(stderr, "MUDLLE: Reincreasing block size to %ld to allow 3x generation1\n",
	      need);
#endif
      newsize = need;
    }

  new_major_collection(newsize);
#ifdef GCSTATS
  fprintf(stderr, "MUDLLE: New stats: gen 0: %d of %d, gen1: %d\n",
	  endgen0 - (posgen0 - n), endgen0 - startgen0, endgen1 - startgen1);
#endif

  assert(posgen0 - n >= startgen0);
}

static INLINE void reserve(long n)
/* The basic allocation routine. Makes sure that n bytes can be allocated */
/* Requires: n == ALIGN(n, sizeof(value))
*/
{
  if (posgen0 - n >= startgen0) return;

  garbage_collect(n);
}

INLINE static value fast_gc_allocate(long n)
/* Requires: n == ALIGN(n, sizeof(value))
*/
{
  ubyte *new = posgen0 -= n;

  assert(posgen0 >= startgen0);
#ifdef GCDEBUG
  ((struct obj *)new)->generation = minorgen;
#endif
  return new;
}

value gc_allocate(long n)
/* Effects: Allocates n bytes and returns a pointer to the start of
     the allocated area.
     DOES ABSOLUTELY NO INITIALISATION. BEWARE!
   Returns: Pointer to allocated area
*/
{
  ulong aligned = ALIGN(n, sizeof(long));

  reserve(aligned);
  return fast_gc_allocate(aligned);
}

/* Basic allocation */
/* ---------------- */

struct grecord *unsafe_allocate_record(ubyte type, ulong entries)
{
  ulong size = sizeof(struct obj) + entries * sizeof(value);
  struct grecord *new = gc_allocate(size);

  new->o.size = size;
  new->o.garbage_type = garbage_record;
  new->o.type = type;
  new->o.flags = 0;

  /* WARNING: data is not initialised!!! */
#ifdef GCSTATS
  gcstats.anb[type]++;
  gcstats.asizes[type] += size;
#endif

  return new;
}

struct grecord *allocate_record(ubyte type, ulong entries)
{
  struct grecord *new = unsafe_allocate_record(type, entries);
  value *o;

  /* Initialise data to NULL */
  o = (value *)&new->data;
  while (entries) { entries--; *o++ = NULL; }

  return new;
}

struct gstring *allocate_string(ubyte type, ulong bytes)
{
  ulong size = sizeof(struct obj) + bytes;
  struct gstring *new = gc_allocate(size);

  new->o.size = size;
  new->o.garbage_type = garbage_string;
  new->o.type = type;
  new->o.flags = OBJ_IMMUTABLE;
#ifdef GCSTATS
  gcstats.anb[type]++;
  gcstats.asizes[type] += size;
#endif

  return new;
}

struct gpermanent *allocate_permanent(ubyte type, ulong nb, void *ext)
{
  struct gpermanent *new = gc_allocate(sizeof(struct gpermanent));

  new->o.size = sizeof(struct gpermanent);
  new->o.garbage_type = garbage_permanent;
  new->o.type = type;
  new->o.flags = OBJ_IMMUTABLE;
#ifdef GCSTATS
  gcstats.anb[type]++;
  gcstats.asizes[type] += sizeof(struct gpermanent);
#endif

  new->external = ext;
  new->nb = nb;
  new->call_count = 0;

#if 0
  assert(nb < MAX_PERMANENT && !permanent_obj[nb]);
  permanent_obj[nb] = ext;
#endif

  return new;
}

struct gtemp *allocate_temp(ubyte type, void *ext)
{
  struct gtemp *new = gc_allocate(sizeof(struct gtemp));

  new->o.size = sizeof(struct gtemp);
  new->o.garbage_type = garbage_temp;
  new->o.type = type;
  new->o.flags = OBJ_IMMUTABLE;
  new->external = ext;
#ifdef GCSTATS
  gcstats.anb[type]++;
  gcstats.asizes[type] += sizeof(struct gtemp);
#endif

  return new;
}

struct vector *allocate_locals(ulong n)
/* Effect: Allocate a vector of local variables in an optimised fashion.
*/
{
  ulong vecsize = sizeof(struct obj) + n * sizeof(value);
#define varsize (sizeof(struct obj) + sizeof(value))
  struct vector *locals;
  value *var;

  reserve(vecsize + n * varsize);

  locals = fast_gc_allocate(vecsize);
  locals->o.size = vecsize;
  locals->o.garbage_type = garbage_record;
  locals->o.type = type_internal;
  locals->o.flags = 0;
#ifdef GCSTATS
  gcstats.anb[type_internal]++;
  gcstats.asizes[type_internal] += vecsize;
#endif
  
  var = locals->data;
  while (n--)
    {
      struct variable *v = fast_gc_allocate(varsize);

      *var++ = v;
      v->o.size = varsize;
      v->o.garbage_type = garbage_record;
      v->o.type = type_variable;
      v->o.flags = 0;
      v->value = NULL;
#ifdef GCSTATS
      gcstats.anb[type_variable]++;
      gcstats.asizes[type_variable] += varsize;
#endif
    }
#undef varsize

  return locals;
}

/* Detect immutability */
/* ------------------- */

void detect_immutability(void)
{
  ubyte *ptr;
  int change;

  garbage_collect(0); /* Get rid of junk in generation 0 */
  do
    {
      change = FALSE;
      ptr = posgen0;
      while (ptr < endgen0)
	{
	  struct obj *obj = (struct obj *)ptr;

	  ptr += ALIGN(obj->size, sizeof(value));

	  if (obj->garbage_type == garbage_record &&
	      (obj->flags & (OBJ_READONLY | OBJ_IMMUTABLE)) == OBJ_READONLY)
	    {
	      struct grecord *rec = (struct grecord *)obj;
	      struct obj **o, **recend;

	      recend = (struct obj **)((ubyte *)rec + rec->o.size);
	      o = rec->data;

	      /* Scan & check if it can be made immutable */
	      for (;;)
		{
		  if (o == recend)
		    {
		      /* Readonly objects can be made immutable if all their contents
			 are themselves immutable.
			 Note that this will not detect recursive immutable objects */

		      change = TRUE;
		      rec->o.flags |= OBJ_IMMUTABLE;
		      break;
		    }
		  /* If contains non-immutable pointer, give up */
		  if (pointerp(*o) && !((*o)->flags & OBJ_IMMUTABLE)) break;
		  o++;
		}
	    }
	}
    }
  while (change);
}



/* Persistent data */
/* --------------- */

static ulong from_offset;	/* Value to add to pointers in forward to get
				   real pointer address.
				   (used to unpatch a dump file) */

void save_restore(struct obj *obj)
/* Effects: Restores the information used during GC for obj (forwarding)
*/
{
  if (pointerp(obj) && obj->garbage_type == garbage_forwarded)
    {
      struct obj *copy = (struct obj *)obj->size;

      /* Restore this value */
      obj->garbage_type = copy->garbage_type;
      obj->size = copy->size;

      /* And its contents, recursively */
      switch (obj->garbage_type)
	{
	case garbage_record: {
	  struct grecord *rec = (struct grecord *)obj;
	  ulong i;
	  value *o;

	  i = (rec->o.size - sizeof(struct obj)) / sizeof(struct obj *);
	  o = (value *)&rec->data;
	  while (i) { --i; save_restore(*o++); }
	  break;
	}
	case garbage_code: {
	  struct code *code = (struct code *)obj;
	  unsigned i;
	  value *c;

	  save_restore((struct obj *)code->help);
	  save_restore((struct obj *)code->filename);
	  save_restore((struct obj *)code->varname);
	  i = code->nb_constants;
	  c = (value *)&code->constants;
	  while (i) { --i; save_restore(*c++); }
	  break;
	}
	case garbage_mcode:
	  save_restore_mcode((struct mcode *)obj);
	  break;
	}
    }
}

jmp_buf nomem;
static unsigned long mutable_size;

static int size_forward(struct obj **ptr)
{
  struct obj *obj = *ptr;
  struct obj *newobj;
  long size;

  size = obj->size;
  /* GC to generation 0, allocating forwards */
  if (obj->garbage_type == garbage_forwarded)
    *ptr = (value)size;
  else
    {
      /* Forward to generation 0 */
      newobj = (struct obj *)newpos0;
      newpos0 += ALIGN(size, sizeof(value));
      if (!(obj->flags & OBJ_IMMUTABLE)) mutable_size += ALIGN(size, sizeof(value));
      if (newpos0 > newend0) longjmp(nomem, 1);

      *ptr = move_object(obj, newobj, minorgen);
    }
  return FALSE;
}

unsigned long gc_size(value x, unsigned long *mutable)
/* Effects: Returns number of bytes accessible from x
     Sets mutable (if not NULL) to the # of mutable bytes in x
   Modifies: mutable
*/
{
  ubyte *data;
  value *save;
  struct gcpro gcpro1;
  volatile ulong gcsize = DEF_SAVE_SIZE;

  for (;;)
    {
      if (!setjmp(nomem))
	{
	  /* Be really sure that there is enough space for the header */
	  GCPRO1(x);
	  reserve(gcsize);
	  UNGCPRO();

	  major_offset = 0;
	  special_forward = size_forward; 
	  newstart0 = newpos0 = posgen0 - gcsize; newend0 = posgen0;

	  /* Forward the root value */
	  mutable_size = 0;
	  save = (value *)newpos0;
	  newpos0 += sizeof(value);
	  *save = x;
	  if (pointerp(x)) special_forward((struct obj **)save);

	  /* Scan copied data */
	  data = (ubyte *)(save + 1);
	  while (data < newpos0) data = scan(data);
	  assert(data == newpos0);

	  /* Restore old block (contains forwarded data) */
	  save_restore(x);

	  if (mutable) *mutable = mutable_size;
	  return (unsigned long)(newpos0 - newstart0);
	}
      else
	{
	  /* No memory, try again */
	  save_restore(x);
	  gcsize *= 2;
	}
    }
}

static int save_forward(struct obj **ptr)
{
  struct obj *obj = *ptr;

  GCCHECK(obj);

  if (obj->garbage_type == garbage_permanent ||
      obj->garbage_type == garbage_temp ||
      obj->garbage_type == garbage_code ||
      obj->garbage_type == garbage_mcode ||
      obj->type == type_closure ||
      obj->type == type_outputport ||
      obj->type == type_internal)
    {
      /* Set ptr to the gone value */
      *ptr = (struct obj *)(newstart0 + sizeof(ubyte *));
    }
  else
    {
      struct obj *newobj;
      long size;

      size = obj->size;
      /* GC to generation 0, allocating forwards */
      if (obj->garbage_type == garbage_forwarded)
	*ptr = (value)size;
      else
	{
	  /* Forward to generation 0 */
	  newobj = (struct obj *)newpos0;
	  newpos0 += ALIGN(size, sizeof(value));
	  if (newpos0 > newend0) longjmp(nomem, 1);

	  *ptr = move_object(obj, newobj, minorgen);
	}
    }
  return FALSE;
}

void *gc_save(value x, unsigned long *size)
/* Effects: Saves a value x into a contiguous block of memory so
     that it can be reloaded by gc_load.

     Not all types of data may be saved, some will be silently 
     replaced by a `gone' value:
       - all external data (permanent & temporary)
       - code, closure
       - outputports
       - internal

     Sharing of values reachable from x is preserved by gc_save/gc_load.
     However, any sharing between values saved with separate calls to 
     gc_save/gc_load is lost.

   Returns: A pointer to the block of memory containing x. This
     pointer only remains valid till the next allocation or call
     to gc_load.
     *size contains the # of bytes required for x.
*/
{
  ubyte *data;
  value *save;
  struct gcpro gcpro1;
  struct obj gone;
  volatile ulong gcsize = DEF_SAVE_SIZE;

  for (;;)
    {
      if (!setjmp(nomem))
	{
	  /* Be really sure that there is enough space for the header */
	  GCPRO1(x);
	  reserve(gcsize);
	  UNGCPRO();

	  major_offset = 0;
	  special_forward = save_forward; 
	  newstart0 = newpos0 = posgen0 - gcsize; newend0 = posgen0;

	  /* Save data needed for reload: address of start of block */
	  /* This address is also used as a special `gone' marker */
	  *(ubyte **)newpos0 = newstart0;
	  newpos0 += sizeof(ubyte *);

	  /* Add a nice gone value */
	  gone.size = sizeof gone;
	  gone.garbage_type = garbage_string;
	  gone.type = type_gone;
	  gone.flags = 0;
#ifdef GCDEBUG
	  gone.generation = minorgen;
#endif
	  memcpy(newpos0, &gone, sizeof gone);
	  newpos0 += sizeof gone;

	  /* Forward the root value */
	  save = (value *)newpos0;
	  newpos0 += sizeof(value);
	  *save = x;
	  if (pointerp(x)) special_forward((struct obj **)save);

	  /* Scan copied data */
	  data = (ubyte *)(save + 1);
	  while (data < newpos0) data = scan(data);
	  assert(data == newpos0);

	  /* Restore old block (contains forwarded data) */
	  save_restore(x);

	  *size = newpos0 - newstart0;
	  return newstart0;
	}
      else
	{
	  /* No memory, try again */
	  save_restore(x);
	  gcsize *= 2;
	}
    }
}

static value _gc_load(int (*forwarder)(struct obj **ptr),
		      ubyte *load, value *old, unsigned long size)
/* Effects: Reloads a value saved with gc_save. <load,size> delimits
     the zone of memory containing gc_save's results.
     See gc_save for details.
   Returns: The loaded value
*/
{
  ubyte *data, *unscanned0, *oldstart0;

  /* Data is loaded into generation 0 */

  /* Make sure there is enough place for the value */
  reserve(size - sizeof(ubyte *) - sizeof(value));

  from_offset = load - *(ubyte **)load;
  major_offset = 0;
  special_forward = forwarder; 
  newstart0 = startgen0; newend0 = endgen0; newpos0 = posgen0;
  
  /* Old value of pointer to first value, forward it */
  if (pointerp(*old)) special_forward((struct obj **)old);

  /* Must scan forwards, but data is allocated downwards => double loop */
  unscanned0 = posgen0;		/* Upper bound of unscanned data */
  do {
    oldstart0 = data = newpos0;
    while (data < unscanned0) data = scan(data);
    assert(data == unscanned0);
    unscanned0 = oldstart0;
  } while (oldstart0 != newpos0); /* Till nothing forwarded */

  posgen0 = newpos0;

  return *old;
}

static int load_forward(struct obj **ptr)
{
  struct obj *obj, *newobj;
  long size;

  /* Correct pointer addresses on load */
  obj = (struct obj *)((ubyte *)*ptr + from_offset);
  size = obj->size;

  /* And GC them to generation 0 */
  if (obj->garbage_type == garbage_forwarded)
    *ptr = (value)size;
  else
    {
      /* Forward to generation 0 */
      newobj = (struct obj *)(newpos0 -= ALIGN(size, sizeof(value)));
      assert(newpos0 >= newstart0);
#ifdef GCDEBUG
      obj->generation = minorgen;
#endif
      *ptr = move_object(obj, newobj, minorgen);
    }
  return FALSE;
}

value gc_load(void *_load, unsigned long size)
/* Effects: Reloads a value saved with gc_save. <load,size> delimits
     the zone of memory containing gc_save's results.
     See gc_save for details.
   Returns: The loaded value
*/
{
  ubyte *load = _load;
  value *old = (value *)(load + sizeof(value) + sizeof(struct obj));

  return _gc_load(load_forward, load, old, size);
}

#ifndef GCDEBUG
static int load_forward_debug(struct obj **ptr)
{
  struct obj *obj, *newobj;
  long size;

  /* Correct pointer addresses on load */
  obj = (struct obj *)((ubyte *)*ptr + from_offset);
  size = obj->size;

  /* And GC them to generation 0 */
  if (obj->garbage_type == garbage_forwarded)
    *ptr = (value)size;
  else
    {
      /* Forward to generation 0 while removing generation field*/
      size -= sizeof(ulong);

      newobj = (struct obj *)(newpos0 -= ALIGN(size, sizeof(value)));
      assert(newpos0 >= newstart0);

      newobj->size = size;
      newobj->garbage_type = obj->garbage_type;
      newobj->type = obj->type;
      newobj->flags = obj->flags;
      memcpy(newobj + 1, (char *)(obj + 1) + sizeof(ulong),
	     size - sizeof(struct obj));

      assert(obj->garbage_type <= garbage_mcode);
      obj->garbage_type = garbage_forwarded;
      newobj = (struct obj *)((ubyte *)newobj + major_offset);
      obj->size = (long)newobj;

      *ptr = newobj;
    }
  return FALSE;
}

value gc_load_debug(void *_load, unsigned long size)
/* Effects: Reloads a value saved with gc_save in a mudlle where GCDEBUG was defined.
     <load,size> delimits the zone of memory containing gc_save's results.
     See gc_save for details.
   Returns: The loaded value
*/
{
  ubyte *load = _load;
  value *old = (value *)(load + sizeof(value) + sizeof(struct obj) + sizeof(ulong));

  return _gc_load(load_forward_debug, load, old, size);
}
#endif

void dump_memory(void)
/* Effects: Dumps GC's memory to a file for use by the profiler.
*/
{
  int fd = creat("mudlle-memory.dump", 0666);

  if (fd < 0)
    {
      perror("MUDLLE: Failed to dump");
      return;
    }
  if (write(fd, &startgen1, sizeof startgen1) != sizeof startgen1 ||
      write(fd, &endgen1, sizeof endgen1) != sizeof endgen1 ||
      write(fd, startgen1, endgen1 - startgen1) != endgen1 - startgen1 ||
      write(fd, &posgen0, sizeof posgen0) != sizeof posgen0 ||
      write(fd, &endgen0, sizeof endgen0) != sizeof endgen0 ||
      write(fd, posgen0, endgen0 - posgen0) != endgen0 - posgen0)
    perror("MUDLLE: Failed to write dump");
  close(fd);
}


/* Machine specific portion of allocator */
/* ------------------------------------- */

#ifdef sparc

/* The standard Sparc SAVE/RESTORE mechanism is used for stack frames.
   These frames must therefore be parsed at GC time.
   The portion of the stack that belongs to the last invocation of
   some mudlle machine code is found in the region
   frame_start .. frame_end (frame_start > frame_end as stack
   allocation is downwards). */

ulong *frame_start, *frame_end;

/* The next frames are found by following a special pointer which
   is kept near frame_end (see mc_invoke & forward_registers) for
   details. */

static void forward_pc(ulong *pcreg)
{
  ulong *pc = (ulong *)*pcreg;

#if 0
  fprintf(stderr, "pc %x ", pc); fflush(stderr);
#endif
  /* Check for moving code */
  if ((ubyte *)pc >= gcrange_start &&
      (ubyte *)pc < gcrange_end)
    {
      struct mcode *base, *oldbase;

      /* We have found a collectable code object.
	 First find where it begins by locating its
	 magic sequence */
      while (pc[0] != 0xffffffff ||
	     pc[-1] != 0xffffffff) pc--;

      /* Forward it */
      base = (struct mcode *)((char *)pc - 4 -
			      offsetof(struct mcode, magic));
      oldbase = base;
      GCCHECK(base);
      special_forward((struct obj **)&base);

      /* And adjust the pc value for the new base location */
      *pcreg = (ulong)base + (*pcreg - (ulong)oldbase);
#if 0
      fprintf(stderr, "-> %x ", *pcreg); fflush(stderr);
#endif
    }
}

static void forward_registers(void)
{
  ulong *frame, *next, *fstart;
  int first, i;

  flush_windows(); /* Insure that register windows are all on the stack */

  frame = frame_end;
  fstart = frame_start;
  first = TRUE;
  while (frame)
    {
      assert(fstart && frame < fstart);
#if 0
      fprintf(stderr, "frame "); fflush(stderr);
#endif
      /* Forward register save area.
	 This code depends on the order in which registers are saved
	 so as to recognise the special registers
	   i6: pointer to next frame
	   i7: return address (points within a machine code object)
      */

      /* In first frame, there is also a return address in l1 ... */
      if (first) forward_pc(frame + 1);
      first = FALSE;

      frame += 2;		/* Skip scratch regs */

      for (i = 0; i < 12; i++)
	{
	  if (pointerp(*frame))
	    {
	      GCCHECK((value)*frame);
	      special_forward((struct obj **)frame);
	    }
	  frame++;
	}
      next = *(ulong **)frame++;/* fp is next frame */
      forward_pc(frame++);	/* Forward return address (i7) */

      /* Now forward scratch area (after the 5+1 words reserved for calling
	 conventions) */
      frame += 6;

      while (frame < next)
	{
	  if (pointerp(*frame))
	    {
	      GCCHECK((value)*frame);
	      special_forward((struct obj **)frame);
	    }
	  frame++;
	}

      if (frame == fstart) /* Reached initial frame */
	{
	  ulong *args;

#if 0
	  fprintf(stderr, "new "); fflush(stderr);
#endif

	  /* This frame contains: some arguments, a link to the next set of frames */
	  args = frame + 22;
	  next = (ulong *)frame[14];
	  while (args < next)
	    {
	      if (pointerp(*args))
		{
		  GCCHECK((value)*args);
		  special_forward((struct obj **)args);
		}
	      args++;
	    }

	  /* Check for link to next set of frames */
	  /* This will have been left in l3/l2 by mc_invoke, so: */
	  fstart = (ulong *)frame[3];
	  frame = (ulong *)frame[2];
	  first = TRUE;
	}
    }
}

static INLINE void scan_mcode(struct mcode *code)
{
  unsigned i;
  uword *offsets;
  ubyte *old_base;
  ulong *mcode, delta;

  if (pointerp(code->help)) special_forward((struct obj **)&code->help);
  if (pointerp(code->varname)) special_forward((struct obj **)&code->varname);
  if (pointerp(code->filename)) special_forward((struct obj **)&code->filename);
  i = code->nb_constants;
  mcode = code->mcode;

  /* Forward constants */
  offsets = (uword *)(mcode + code->code_length);
  while (i--)
    {
      ulong *cstins = mcode + *offsets++, *orins, ortemplate;
      value cst;

      /* Extract split constant, forward it, and modify instructions */
      /* (Constants are represented in code by a sethi/or pair) */
      /* Find the or that goes with the cstins ... */
      ortemplate = (2 << 30 | 2 << 19) | (*cstins & ~((1 << 25) - 1));
      orins = cstins;
      while ((*++orins & ~((1 << 19) - 1)) != ortemplate) ;

      cst = (value)(*cstins << 10 | (*orins & ((1 << 10) - 1)));
      if (pointerp(cst))
	{
	  special_forward((struct obj **)&cst);
	  *cstins = (*cstins & ~((1 << 22) - 1)) | ((ulong)cst >> 10);
	  *orins = (*orins & ~((1 << 13) - 1)) | ((ulong)cst & ((1 << 10) - 1));
	}
    }

  /* Relocate calls to builtins */
  old_base = code->myself;
  code->myself = (ubyte *)code + major_offset;

  /* Scan code and relocate all calls.
     This assumes that all calls are calls to unmoving code (builtin ops) */
  i = code->code_length;
  delta = (old_base - code->myself) >> 2;
  while (i--)
    {
      if (*mcode >> 30 == 1)	/* call instruction */
	/* adjust offset by delta */
	*mcode = 1 << 30 | (*mcode + delta) & (1 << 30) - 1;
      mcode++;
    }
}

static INLINE void save_restore_mcode(struct mcode *code)
{
  unsigned i;
  uword *offsets;
  ulong *mcode;

  save_restore((struct obj *)code->help);
  save_restore((struct obj *)code->varname);
  save_restore((struct obj *)code->filename);
  i = code->nb_constants;
  mcode = code->mcode;
  offsets = (uword *)(mcode + code->code_length);
  while (i--)
    {
      ulong *cstins = mcode + *offsets++;
      value cst;

      /* Extract split constant and restore it */
      /* (Constants are represented in code by a sethi/or pair) */
      cst = (value)(cstins[0] << 10 | cstins[1] & (1 << 10) - 1);
      save_restore(cst);
    }
}

#endif

#ifdef AMIGA

#include <exec/types.h>
#include <exec/execbase.h>
#include <proto/exec.h>

/* the static area where the current machine code activation saves its
   information. The old values are copied to the activation stack on
   every call to machine code */
int registers_valid;		/* TRUE if static area is being used */
ulong save_registers[5];
ulong extra_registers[5];
ulong initial_stack, final_stack;

struct vector *activation_stack;

void check_registers(void)
{
  GCCHECK(activation_stack);
  if (registers_valid)
    {
      int i;

      for (i = 0; i < 5; i++) GCCHECK(save_registers[i]);
      for (i = 0; i < 5; i++) GCCHECK(extra_registers[i]);
    }
}

void push_registers(void)
{
  if (registers_valid)
    {
      struct vector *saveregs = (struct vector *)unsafe_allocate_record(type_internal, 13);

      memcpy(saveregs->data, save_registers, 5 * sizeof(ulong));
      memcpy(saveregs->data + 5, extra_registers, 5 * sizeof(ulong));
      saveregs->data[10] = (value)(initial_stack ^ 1);
      saveregs->data[11] = (value)(final_stack ^ 1);
      saveregs->data[12] = activation_stack;
      activation_stack = saveregs;

      /*check_registers();*/
    }
  registers_valid = TRUE;
}

void pop_registers(void)
{
  if (activation_stack)
    {
      memcpy(save_registers, activation_stack->data, 5 * sizeof(ulong));
      memcpy(extra_registers, activation_stack->data + 5, 5 * sizeof(ulong));
      initial_stack = (ulong)activation_stack->data[10] ^ 1;
      final_stack = (ulong)activation_stack->data[11] ^ 1;
      activation_stack = activation_stack->data[12];

      /*check_registers();*/
    }
  else
    registers_valid = FALSE;
}

static void forward_stack(ulong *from, ulong *to)
{
  while (from < to)
    {
      ulong *location = (ulong *)*from;
      struct mcode *base, *oldbase;

      if ((ubyte *)location >= gcrange_start &&
	  (ubyte *)location < gcrange_end)
	{
	  /* We have found a collectable code object.
	     First find where it begins by locating its
	     magic sequence */
	  location = (ulong *)ALIGN((ulong)location, sizeof(value));

	  while (location[0] != 0xffffffff ||
		 location[-1] != 0xffffffff) location--;

	  base = (struct mcode *)((char *)location - 4 -
				  offsetof(struct mcode, magic));
	  oldbase = base;
	  GCCHECK(base);
	  special_forward((struct obj **)&base);

	  *from = (ulong)base + (*from - (ulong)oldbase);
	}
      from++;
    }
}

static void forward_registers(void)
{
  struct vector *astack = activation_stack;

  if (registers_valid)
    {
      int i;

      forward_stack((ulong *)final_stack, (ulong *)initial_stack);
      for (i = 0; i < 5; i++)
	if (pointerp(save_registers[i]))
	  special_forward((struct obj **)&save_registers[i]);
      for (i = 0; i < 5; i++)
	if (pointerp(extra_registers[i]))
	  special_forward((struct obj **)&extra_registers[i]);
    }
  while (astack)
    {
      forward_stack((ulong *)((ulong)astack->data[11] ^ 1),
		    (ulong *)((ulong)astack->data[10] ^ 1));
		    

      astack = astack->data[12];
    }
  if (activation_stack) special_forward((struct obj **)&activation_stack);
}

void flush_icache(ubyte *from, ubyte *to)
{
  CacheClearU();
}

static INLINE void scan_mcode(struct mcode *code)
{
  unsigned i;
  uword *offsets;
  ubyte *mcode;

  if (pointerp(code->help)) special_forward((struct obj **)&code->help);
  if (pointerp(code->varname)) special_forward((struct obj **)&code->varname);
  if (pointerp(code->filename)) special_forward((struct obj **)&code->filename);
  i = code->nb_constants;
  mcode = code->mcode;
  offsets = (uword *)(mcode + ALIGN(code->code_length, sizeof(uword)));
  while (i)
    {
      --i;
      if (pointerp(*(value *)(mcode + *offsets)))
	special_forward((struct obj **)(mcode + *offsets));
      offsets++;
    }
}

static INLINE void save_restore_mcode(struct mcode *code)
{
  unsigned i;
  uword *offsets;
  ubyte *mcode;

  save_restore((struct obj *)code->help);
  save_restore((struct obj *)code->varname);
  save_restore((struct obj *)code->filename);
  i = code->nb_constants;
  mcode = code->mcode;
  offsets = (uword *)(mcode + ALIGN(code->code_length, sizeof(uword)));
  while (i) { --i; save_restore(*(value *)(mcode + *offsets++)); }
}

#endif

#ifdef linux

void flush_icache(ubyte *from, ubyte *to)
{
}

static void forward_registers(void)
{
}

static INLINE void scan_mcode(struct mcode *code)
{
}

static INLINE void save_restore_mcode(struct mcode *code)
{
}

#endif

