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

#include <fcntl.h>
#include <setjmp.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>
#ifndef WIN32
#  include <netinet/in.h>
#endif

#include "alloc.h"
#include "builtins.h"
#include "context.h"
#include "mvalgrind.h"
#include "table.h"
#include "utils.h"

#include "runtime/basic.h"


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

    The set of all such active areas serves as GC roots. The stack areas
    delimited in these areas are also examined for pointers into code objects.

    The beginning of code objects is recognised by a special magic sequence
    which is never generated as machine code (currently 8 bytes of 0xff is
    used).

    The instruction cache must be flushed after garbage collection.

    Important optimisation: if no machine code objects are seen during
    GC, there is no need to flush the cache (i.e. most minor collections).
*/

/* Offset from a multiple of CODE_ALIGNMENT at which to place struct mcode's so
   that the mcode field is aligned on a multiple of CODE_ALIGNMENT */
#define MCODE_OFFSET (-offsetof(struct mcode, mcode) & (CODE_ALIGNMENT - 1))

/* Roots */
struct gcpro *gcpro;		/* Local (C) variables which need protection */

static struct dynpro dynpro_head = {
  .next = &dynpro_head,
  .prev = &dynpro_head
};
#define MAXROOTS 100
static struct {
  const char *desc, *file;
  int line;
  value *data;
} roots[MAXROOTS];

static ulong last_root;

union free_obj {
  struct obj o;
  union free_obj *next;         /* reuses the size field */
};
CASSERT(free_obj, (offsetof(union free_obj, next)
		   == offsetof(union free_obj, o.size)));

#define FREE_SIZES 12
static union free_obj *free_lists[FREE_SIZES];

/* machine code roots */
static void forward_registers(void);
static void unmark_safe_registers(void);
static void save_restore_mcode(struct mcode *code);
static void save_restore(struct obj *obj);
#ifndef NOCOMPILER
static void scan_mcode(struct mcode *code);
#endif
static void flush_icache(ubyte *from, ubyte *to);

static inline ulong htonlong(ulong l)
{
#ifdef WORDS_BIGENDIAN
  return l;
#else
  CASSERT_EXPR(sizeof l == 4 || sizeof l == 8);
  if (sizeof l == 4)
    return htonl(l);
  return htonl(l >> 16 >> 16) | ((ulong)htonl(l) << 16 << 16);
#endif
}

#define ntohlong(l) htonlong(l)

void _staticpro(void *pro, const char *desc, const char *file, int line)
{
  assert (last_root < MAXROOTS);

  if (*desc == '&')
    ++desc;
  roots[last_root].data = pro;
  roots[last_root].desc = desc;
  roots[last_root].file = file;
  roots[last_root].line = line;
  ++last_root;
}

struct vector *get_staticpro_data(void)
{
  struct vector *res = alloc_vector(last_root);
  GCPRO1(res);
  for (int i = 0; i < last_root; ++i)
    {
      struct vector *v = alloc_vector(4);
      v->data[2] = makeint(roots[i].line);
      v->data[3] = *roots[i].data;

      res->data[i] = v;
      SET_VECTOR((struct vector *)res->data[i], 0,
                 alloc_string(roots[i].desc));
      SET_VECTOR((struct vector *)res->data[i], 1,
                 alloc_string(roots[i].file));

    }
  UNGCPRO();

  return res;
}

void dynpro(struct dynpro *what, value obj)
{
  assert(what->next == NULL && what->prev == NULL);
  GCCHECK(obj);
  *what = (struct dynpro){
    .next = dynpro_head.next,
    .prev = &dynpro_head,
    .obj  = obj
  };
  dynpro_head.next->prev = what;
  dynpro_head.next = what;
}

void undynpro(struct dynpro *what)
{
  what->prev->next = what->next;
  what->next->prev = what->prev;
  *what = (struct dynpro){ 0 };
}

void maybe_dynpro(struct dynpro *what, value obj)
{
  if (staticp(obj))
    {
      assert(what->next == NULL && what->prev == NULL);
      GCCHECK(obj);
      what->obj = obj;
    }
  else
    dynpro(what, obj);
}

void maybe_undynpro(struct dynpro *what)
{
  if (!staticp(what->obj))
    {
      what->prev->next = what->next;
      what->next->prev = what->prev;
    }
  *what = (struct dynpro){ 0 };
}

struct dynpro *protect(value v)
{
  struct dynpro *newp = xmalloc(sizeof *newp);
  *newp = (struct dynpro){ 0 };
  dynpro(newp, v);
  return newp;
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
ubyte *gcblock;
ulong gcblocksize;

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

static int newarea;		/* true during new_major_collection */
static ulong major_offset;
static bool saw_mcode;		/* true if forwarded an mcode object */

static bool (*special_forward)(void *_ptr);

ulong minorgen, majorgen;
static ulong newminorgen, newmajorgen;
#ifdef GCSTATS
struct gcstats gcstats;
#endif

#ifdef GCQDEBUG
static ulong maxobjsize = 1024; /* biggest object created (ignores stuff done
				   by compiler, but those aren't big) */

void gccheck_qdebug(value x)
{
  if (!pointerp(x))
    return;
  assert(~(long)x & 2);
  struct obj *o = x;
  assert(!((o->size > maxobjsize &&
            o->garbage_type != garbage_forwarded) ||
           o->size < 8 ||
           o->garbage_type >= garbage_free ||
           o->type >= last_type ||
           o->flags & ~(OBJ_READONLY | OBJ_IMMUTABLE)));
}
#endif

/* Range affected by current GC */
ubyte *gcrange_start, *gcrange_end;

static void free_gc_block(void *b, size_t size)
{
  if (b == NULL)
    {
      assert(size == 0);
      return;
    }
  mprotect(b, size, PROT_READ | PROT_WRITE);
  free(b);
}

static void *alloc_gc_block(size_t size)
{
  void *b = valloc(size);
  assert(b != NULL);
  mprotect(b, size, PROT_READ | PROT_WRITE | PROT_EXEC);
  return b;
}

/* Garbage collector initialisation & cleanup */
void garbage_init(void)
{
  /* All hell will break loose if this isn't true */
  CASSERT_EXPR(sizeof (value) == 4 || sizeof (value) == 8);
  CASSERT_EXPR(sizeof (ulong) == sizeof (value));

  gcblocksize = INITIAL_BLOCKSIZE;
  gcblock = alloc_gc_block(gcblocksize);

  /* No first generation yet */
  startgen1 = endgen1 = gcblock;

  /* Divide remainder into 2 */
  ulong half = (gcblocksize / 2) & ~(sizeof (value) - 1);
  posgen0 = endgen0 = gcblock + gcblocksize;
  startgen0 = endgen0 - half;

#ifdef GCDEBUG
  minorgen = 98146523;		/* Must be odd */
  majorgen = 19643684;		/* Must be even */
#endif
}

static value move_object(struct obj *obj, struct obj *newobj, long newgen)
{
  long size = obj->size;

  assert(obj->garbage_type < garbage_free);

  GCCHECK(obj);
  memcpy(newobj, obj, size);
#ifdef GCDEBUG
  newobj->generation = newgen;
#endif
  newobj = (struct obj *)((ubyte *)newobj + major_offset);
  if (obj->garbage_type == garbage_static_string)
    {
      newobj->garbage_type = garbage_string;
      *static_data(obj) = (long)newobj;
    }
  else
    {
      obj->garbage_type = garbage_forwarded;
      obj->size = (long)newobj;
    }

  return newobj;
}

static void special_forward_code(struct code *code)
{
  if (pointerp(code->help)) special_forward(&code->help);
  if (pointerp(code->filename)) special_forward(&code->filename);
  if (pointerp(code->nicename)) special_forward(&code->nicename);
  if (pointerp(code->varname)) special_forward(&code->varname);
  if (pointerp(code->arg_types)) special_forward(&code->arg_types);
}

static void save_restore_code(struct code *code)
{
  save_restore(&code->help->o);
  save_restore(&code->filename->o);
  save_restore(&code->nicename->o);
  save_restore(&code->varname->o);
  save_restore(&code->arg_types->o);
}

static ubyte *scan(ubyte *ptr)
{
  struct obj *obj = (struct obj *)ptr;

  ptr += MUDLLE_ALIGN(obj->size, sizeof (value));

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
      struct icode *icode = (struct icode *)obj;
      struct obj **c, **cend;

#ifndef NOCOMPILER
      saw_mcode = true; /* code objects contain jump to interpreter */
#endif
      special_forward_code(&icode->code);
      if (pointerp(icode->lineno_data)) special_forward(&icode->lineno_data);

      c = icode->constants;
      cend = icode->constants + icode->nb_constants;
      while (c < cend)
	{
	  if (pointerp(*c)) special_forward(c);
	  c++;
	}
      break;
    }
    case garbage_mcode:
#ifdef NOCOMPILER
      abort();
#else
      saw_mcode = true;
      scan_mcode((struct mcode *)obj);
#endif
      break;
    case garbage_primitive: case garbage_temp:
    case garbage_string: case garbage_forwarded:
      break;
    case garbage_static_string: case garbage_free:
      abort();
#if 0
    /* Sanity checks */
    default:
      abort();
#endif
    }
  return ptr;
}

#define MOVE_PAST_ZERO(data, endpos)					\
  while (data < endpos && *(ulong *)data == 0) data += sizeof (ulong)

static ubyte *major_scan(ubyte *data)
{
  data = scan(data);
  /* There may be holes filled with 0 bytes before code objects. Skip
     over them */
  MOVE_PAST_ZERO(data, newpos1);

  return data;
}

static ubyte *major_scan2(ubyte *data)
{
  data = scan(data);
  /* There may be holes filled with 0 bytes before code objects. Skip
     over them */
  MOVE_PAST_ZERO(data, oldpos1);

  return data;
}

static void safe_forward(value *ptr)
{
  /* A C variable may be protected several times. It should be forwarded
     only once */
  if (pointerp(*ptr) && !((ulong)*ptr & 0x2)) /* Hack */
    {
      special_forward(ptr);
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
  /* Forward activation stack */
  forward_registers();

  /* Static roots */
  for (int i = 0; i < last_root; i++)
    safe_forward(roots[i].data);

  /* Dynamic roots */
  for (struct dynpro *dyn = dynpro_head.next;
       dyn != &dynpro_head;
       dyn = dyn->next)
    if (pointerp(dyn->obj)) special_forward(&dyn->obj);

  /* Forward protected C vars */
  for (struct gcpro *aprotect = gcpro; aprotect; aprotect = aprotect->next)
    safe_forward(aprotect->obj);

  /* Forward jmpbufs in ccontexts */
  for (struct catch_context *mcc = catch_context; mcc; mcc = mcc->parent)
    if (pointerp(mcc->mjmpbuf))
      special_forward(&mcc->mjmpbuf);

  /* Forward the call stack */
  for (struct call_stack *stk = call_stack; stk; stk = stk->next)
    {
      struct call_stack_c *cstk = (struct call_stack_c *)stk;
      switch (stk->type)
	{
	case call_c:
	  if (pointerp(cstk->c.u.prim)) special_forward(&cstk->c.u.prim);
          /* fallthrough */
        case call_string:
        case call_primop:
          for (int i = 0; i < cstk->c.nargs; ++i)
            if (pointerp(cstk->args[i])) special_forward(&cstk->args[i]);
	  continue;

	case call_bytecode:
          {
            struct call_stack_mudlle *mstk = (struct call_stack_mudlle *)stk;
            if (pointerp(mstk->fn))
              special_forward(&mstk->fn);
            if (pointerp(mstk->code))
              special_forward(&mstk->code);
            if (pointerp(mstk->locals))
              special_forward(&mstk->locals);
            continue;
          }

        case call_session:
        case call_compiled:
          continue;

        case call_invalid:
          if (pointerp(cstk->c.u.value)) special_forward(&cstk->c.u.value);
          continue;
	}
      abort();
    }

  /* Forward the oports in the session_context */
  for (struct session_context *sc = session_context; sc; sc = sc->parent)
    {
      if (sc->_mudout) special_forward(&sc->_mudout);
      if (sc->_muderr) special_forward(&sc->_muderr);
    }


  /* Static roots */
  for (int i = 0; i < last_root; i++)
    unmark_safe(roots[i].data);

  /* Forward protected C vars */
  for (struct gcpro *aprotect = gcpro; aprotect; aprotect = aprotect->next)
    unmark_safe(aprotect->obj);

  unmark_safe_registers();
}

#ifdef GCSTATS
static inline void gcstats_add_gen(struct obj *obj, ulong size, int gen)
{
  gcstats.gen[gen].types[obj->type].nb++;
  if (obj->flags & OBJ_READONLY)
    gcstats.gen[gen].types[obj->type].rosize += size;
  else
    gcstats.gen[gen].types[obj->type].rwsize += size;
}
#endif

static bool minor_forward(void *_ptr)
/* The forward function for minor collections; return true if object
   is immutable */
{
  struct obj **ptr = _ptr;
  struct obj *obj = *ptr, *newobj;
  long size;

  size = obj->size;

  if (obj->garbage_type == garbage_forwarded)
    {
      *ptr = (value)size;
      return (ubyte *)size < newpos1; /* true iff forwarded to gen1 */
    }
  else if (obj->garbage_type == garbage_static_string)
    return true;
  else
    {
      GCCHECK(obj);

      if (obj->flags & OBJ_IMMUTABLE)
	{
	  /* Immutable, forward to generation 1 */

	  /* In minor collections only bother with generation 0 */
	  if ((ubyte *)obj < newpos1) return true;

	  /* Gen 1 grows upward */
	  if (obj->garbage_type == garbage_mcode)
	    /* We align the actual code on a 16 byte boundary. This is
	       a good idea on x86, and probably elsewhere too */
	    {
	      ubyte *alignedpos =
		(ubyte *)(MUDLLE_ALIGN((ulong)newpos1, CODE_ALIGNMENT)
                          + MCODE_OFFSET);

	      if (alignedpos - CODE_ALIGNMENT >= newpos1)
		alignedpos -= CODE_ALIGNMENT;
	      /* We clear memory between newpos1 and alignedpos so that scan
		 can find the start of the code object */
	      if (alignedpos > newpos1)
		memset(newpos1, 0, alignedpos - newpos1);
	      newpos1 = alignedpos;
	    }

	  newobj = (struct obj *)newpos1;
	  newpos1 += MUDLLE_ALIGN(size, sizeof (value));
	  memcpy(newobj, obj, size);

#ifdef GCSTATS
	  gcstats_add_gen(obj, size, 1);
#endif
#ifdef GCDEBUG
	  newobj->generation = newmajorgen;
#endif

	  obj->garbage_type = garbage_forwarded;
	  obj->size = (long)newobj;
	  *ptr = newobj;

	  return true;
	}
      else
	{
	  /* Mutable, forward to generation 0 */

	  /* Must have been in gen 0 before */
	  /*assert((ubyte *)obj >= startgen0 && (ubyte *)obj < endgen0);*/

	  newobj = (struct obj *)(newpos0 -= MUDLLE_ALIGN(size,
                                                          sizeof (value)));
	  /*assert(newpos0 >= newstart0);*/
	  memcpy(newobj, obj, size);

#ifdef GCSTATS
	  gcstats_add_gen(obj, size, 0);
#endif
#ifdef GCDEBUG
	  newobj->generation = newminorgen;
#endif

	  obj->garbage_type = garbage_forwarded;
	  newobj = (struct obj *)((ubyte *)newobj + minor_offset);
	  obj->size = (long)newobj;
	  *ptr = newobj;

	  return false;
	}
    }
}

static ubyte *minor_scan(ubyte *ptr)
{
  struct obj *obj = (struct obj *)ptr;

  ptr += MUDLLE_ALIGN(obj->size, sizeof (value));

  /* Generation 0 never contains code, so only records contain pointers */
  if (obj->garbage_type == garbage_record)
    {
      struct grecord *rec = (struct grecord *)obj;
      struct obj **o, **recend;

      recend = (struct obj **)((ubyte *)rec + rec->o.size);
      o = rec->data;

      if (obj_readonlyp(&rec->o))
	{
	  bool imm = true;

	  /* Scan & check if it can be made immutable */
	  while (o < recend)
	    {
	      if (pointerp(*o) && !minor_forward(o))
                imm = false;
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
  ulong nsize0;


  saw_mcode = false;
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
  gcstats.gen[0] = GCSTATS_GEN_NULL;
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
  MOVE_PAST_ZERO(data, newpos1);
  while (data < newpos1) data = major_scan(data);
  assert(data == newpos1);

  /* Move new generation 0 into place */
  nsize0 = newend0 - newpos0;
  posgen0 = endgen0 - nsize0;
  memmove(posgen0, newpos0, nsize0);

  /* Flush cache if necessary */
  if (saw_mcode)
    flush_icache(gcblock, gcblock + gcblocksize);

  /* Reset blocks */
  endgen1 = newpos1;
  /* endgen0 is unchanged */
  /* Divide available mem into 2 */
  nsize0 = (endgen0 - endgen1) / 2 & ~(sizeof (value) - 1);
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
  MOVE_PAST_ZERO(data, oldpos1);
  while (data < oldpos1) data = major_scan2(data);
  assert(data == oldpos1);

  data = newstart1;
  MOVE_PAST_ZERO(data, newpos1);
  while (data < newpos1) data = major_scan(data);
  assert(data == newpos1);
}


static bool major_forward(void *_ptr)
{
  struct obj **ptr = _ptr;
  struct obj *obj = *ptr, *newobj;
  long size = obj->size, newgen;

  if (obj->garbage_type == garbage_forwarded)
    *ptr = (value)size;
  else if (obj->garbage_type != garbage_static_string)
    {
      GCCHECK(obj);
      /* Objects in generation 0 stay in generation 0 during major
	 collections (even if they are immutable)
	 (Otherwise you can't scan them for roots to gen 1) */

      if ((ubyte *)obj >= posgen0 && (ubyte *)obj < endgen0)
	{
	  if (!newarea) return false;	/* No gen 0 data needs copying */

	  /* forward to new generation 0 */
	  newobj = (struct obj *)(newpos0 -= MUDLLE_ALIGN(size,
                                                          sizeof (value)));
	  assert(newpos0 >= newstart0);
	  newgen = newminorgen;
#ifdef GCSTATS
          gcstats_add_gen(obj, size, 0);
#endif
	}
      else
	{
	  /* forward to new generation 1 */

	  /* Grows upward */
	  if (obj->garbage_type == garbage_mcode)
	    /* We align the actual code on a 16 byte boundary. This is
	       a good idea on x86, and probably elsewhere too */
	    {
	      ubyte *alignedpos =
		(ubyte *)(MUDLLE_ALIGN((ulong)newpos1 + major_offset,
                                       CODE_ALIGNMENT)
                          + MCODE_OFFSET);

	      alignedpos -= major_offset;
	      if (alignedpos - CODE_ALIGNMENT >= newpos1)
                alignedpos -= CODE_ALIGNMENT;
	      /* We clear memory between newpos1 and alignedpos so
		 that scan can find the start of the code object */
	      if (alignedpos > newend1) alignedpos = newend1;
	      if (alignedpos > newpos1)
                memset(newpos1, 0, alignedpos - newpos1);
	      newpos1 = alignedpos;
	    }

	  newobj = (struct obj *)newpos1;
	  newpos1 += MUDLLE_ALIGN(size, sizeof (value));

	  if (newpos1 > newend1) /* We are in trouble, spill to temp block */
	    {
	      /* Allocate a block big enough to handle the spill */
	      assert(!tempblock1 && !newarea); /* Only once ! */
	      oldpos1 = (ubyte *)newobj; oldstart1 = newstart1;

              ulong tsz1 = (endgen1 - startgen1) - (oldpos1 - oldstart1);
              /* allocate extra space to handle code alignment overhead */
              tempsize1 = (tsz1
                           * ((sizeof (struct mcode) + CODE_ALIGNMENT - 1.0)
                              / sizeof (struct mcode)));
              /* make sure there's no overflow */
              assert(tempsize1 > tsz1);

#ifdef GCSTATS
              fprintf(stderr, "MUDLLE: Temp block size %ld (%ld prealign)"
                      " remain %td already-moved %td\n",
                      tempsize1, tsz1,
                      endgen1 - startgen1,
                      oldpos1 - oldstart1);
#endif

	      assert(size <= tempsize1);
	      tempblock1 = xmalloc(tempsize1);
	      newstart1 = tempblock1;
	      newend1 = tempblock1 + tempsize1;
	      newpos1 = tempblock1 + MUDLLE_ALIGN(size, sizeof (value));
	      major_offset = 0;

	      newobj = (struct obj *)tempblock1;
	    }
#ifdef GCSTATS
          gcstats_add_gen(obj, size, 1);
#endif
	  newgen = newmajorgen;
	}
      *ptr = move_object(obj, newobj, newgen);
    }
  return false;
}

static void major_collection(void)
{
  ulong nsize0;
  ubyte *data;

  saw_mcode = false;
  newarea = false;
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
  gcstats.gen[1] = GCSTATS_GEN_NULL;
#endif

  gcrange_start = gcblock; gcrange_end = gcblock + gcblocksize;
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
      MOVE_PAST_ZERO(data, newpos1);
      while (data < newpos1)
	{
	  data = major_scan(data);
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
      if (saw_mcode)
	flush_icache(gcblock, gcblock + gcblocksize);

      /* Reset generation 0 */
      /* Divide available mem into 2 */
      nsize0 = (endgen0 - endgen1) / 2 & ~(sizeof (value) - 1);
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
  ubyte *data, *oldstart0, *unscanned0;
  ulong nsize0;

  ubyte *newblock = alloc_gc_block(newsize);

  saw_mcode = false;
  newarea = true;
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
  gcstats.gen[0] = gcstats.gen[1] = GCSTATS_GEN_NULL;
#endif
  gcrange_start = gcblock; gcrange_end = gcblock + gcblocksize;
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
  MOVE_PAST_ZERO(data, newpos1);
  while (data < newpos1) data = major_scan(data);
  assert(data == newpos1);
  assert(newpos1 <= newpos0);

  /* Remove old block */
  free_gc_block(gcblock, gcblocksize);
  free(tempblock1);
  tempblock1 = NULL;
  gcblock = newblock;
  gcblocksize = newsize;

  /* Reset generation 1 */
  startgen1 = newstart1;
  endgen1 = newpos1;
  oldsize1 = endgen1 - startgen1;
  if (saw_mcode)
    flush_icache(gcblock, gcblock + gcblocksize);

  /* Reset generation 0 */
  endgen0 = newend0;
  posgen0 = newpos0;
  /* Divide available mem into 2 */
  nsize0 = (endgen0 - endgen1) / 2 & ~(sizeof (value) - 1);
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
  /* clear all free lists */
  memset(free_lists, 0, sizeof free_lists);

  ulong newsize, need, size1, used0;

#if 0
  fprintf(stderr, "GC %ld ...", n); fflush(stderr);
#endif

#ifdef GCSTATS
  /* Copy allocation stats */
  gcstats.size = gcblocksize;
  gcstats.l = gcstats.a;
  gcstats.a = GCSTATS_ALLOC_NULL;
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
      2 * (endgen1 - startgen1) < gcblocksize - (endgen0 - (posgen0 - n)) &&
      (endgen1 - startgen1) < 2 * oldsize1 &&
      (endgen0 - (posgen0 - n)) * THRESHOLD_MAJOR_B <
      (endgen0 - startgen0) * THRESHOLD_MAJOR_A)
    return;

#ifdef GCSTATS
  fflush(stdout);
  fprintf(stderr, "MUDLLE: Major collection: gen0: %td of %td, gen1: %td,"
          " blocksize: %ld\n",
	  endgen0 - (posgen0 - n), endgen0 - startgen0, endgen1 - startgen1,
          gcblocksize);
  if (startgen0 >= posgen0)
    fprintf(stderr, "MUDLLE: generation 0 lacks space\n");
  else if (2 * (endgen1 - startgen1)
           >= gcblocksize - (endgen0 - (posgen0 - n)))
    fprintf(stderr, "MUDLLE: Cause: not 2x generation 1\n");
  else if ((endgen1 - startgen1) >= 2 * oldsize1)
    fprintf(stderr, "MUDLLE: Generation 1 has doubled in size, was %ld\n",
            oldsize1);
  else
    fprintf(stderr, "MUDLLE: Cause generation 0 too full\n");
#endif  /* GCSTATS */

  major_collection();

#ifdef GCSTATS
  if (!tempblock1)
    fprintf(stderr, "MUDLLE: Now: gen0: %td of %td, gen1: %td\n",
	    endgen0 - (posgen0 - n), endgen0 - startgen0, endgen1 - startgen1);
#endif

  /* If block is too full, increase its size
     Too full is:
       - had to allocate a spill block for the major collection
       - no space for the minor area
       - not much space for the major area (at least 2x)
       - not much space for the minor area (not more than THRESHOLD_MINOR %
         full) (to avoid constant garbage collection as memory usage approaches
         block size)
  */
  if (tempblock1)
    size1 = (newpos1 - newstart1) + (oldpos1 - oldstart1);
  else
    size1 = endgen1 - startgen1;
  used0 = endgen0 - (posgen0 - n);
  if (!tempblock1 &&
      size1 * 2 + used0 < gcblocksize &&
      startgen0 < posgen0 &&
      used0 * THRESHOLD_INCREASE_B <
      (endgen0 - startgen0) * THRESHOLD_INCREASE_A)
    return;

  /* Running out of memory. Increase block size */
  newsize = (gcblocksize * INCREASE_A) / INCREASE_B;
  newsize = MUDLLE_ALIGN(newsize, sizeof (value));

#ifdef GCSTATS
  fprintf(stderr, "MUDLLE: Increasing block size to %ld\n", newsize);
  fprintf(stderr, "MUDLLE: Stats: gen 0: %ld of %td, gen1: %ld\n",
	  used0, endgen0 - startgen0, size1);
  if (tempblock1)
    fprintf(stderr, "MUDLLE: Cause: spill block of %ld bytes\n", tempsize1);
  else if (size1 * 2 + used0 >= gcblocksize)
    fprintf(stderr,
            "MUDLLE: Cause: not 2x generation 1 size (%ld) available\n",
            size1);
  else if (startgen0 >= posgen0)
    fprintf(stderr, "MUDLLE: Cause: not enough space for both generations\n");
  else
    fprintf(stderr, "MUDLLE: Cause: generation 0 is too full"
            " (used %ld, size %td)\n",
	    used0, endgen0 - startgen0);
#endif  /* GCSTATS */

  /* And make sure there will be n bytes available and that generation 0 is
     only THRESHOLD_INCREASE full. */
  need = size1 + 2 * ((THRESHOLD_INCREASE_B * used0) / THRESHOLD_INCREASE_A);
  if (need > newsize)
    {
#ifdef GCSTATS
      fprintf(stderr, "MUDLLE: Reincreasing block size to %ld to allow"
              " 2x generation0\n",
	      need);
#endif
      newsize = need;
    }

  /* And that there will be enough major space */
  need = 3 * size1 + used0;
  if (need > newsize)
    {
#ifdef GCSTATS
      fprintf(stderr, "MUDLLE: Reincreasing block size to %ld to allow"
              " 3x generation1\n",
	      need);
#endif
      newsize = need;
    }

  new_major_collection(newsize);
#ifdef GCSTATS
  fprintf(stderr, "MUDLLE: New stats: gen 0: %td of %td, gen1: %td\n",
	  endgen0 - (posgen0 - n), endgen0 - startgen0, endgen1 - startgen1);
#endif

  assert(posgen0 - n >= startgen0);
}

long gc_reserve(long x)
/* The basic allocation routine. Makes sure that n bytes can be allocated. */
{
  const long len = MUDLLE_ALIGN(x, sizeof (value));

  if (posgen0 >= startgen0 + len)
    return posgen0 - startgen0;

  garbage_collect(len);
  return -(posgen0 - startgen0);
}

static value fast_gc_allocate(long n)
/* Requires: n == MUDLLE_ALIGN(n, sizeof (value))
*/
{
  ubyte *newp = posgen0 -= n;

#ifdef GCQDEBUG
  if (n > maxobjsize) maxobjsize = n;
#endif

  assert(posgen0 >= startgen0);
#ifdef GCDEBUG
  ((struct obj *)newp)->generation = minorgen;
#endif
  return newp;
}

/* free gc'ed object known to be mutable and not referenced */
void gc_free(struct obj *o)
{
  GCCHECK(o);
  assert((o->flags & (OBJ_READONLY | OBJ_IMMUTABLE)) == 0);
  union free_obj *f = (union free_obj *)o;
  ulong size = grecord_len(f);
  if (size >= FREE_SIZES)
    {
      return;
    }
  f->next = free_lists[size];
  f->o.garbage_type = garbage_free;
  free_lists[size] = f;
}

value gc_allocate(long n)
/* Effects: Allocates n bytes and returns a pointer to the start of
     the allocated area.
     DOES ABSOLUTELY NO INITIALISATION. BEWARE!
   Returns: Pointer to allocated area
*/
{
  assert(n >= 0 && n <= MAX_MUDLLE_OBJECT_SIZE);

  ulong aligned = MUDLLE_ALIGN(n, sizeof (value));

  /* check for available free_lists entry */
  if (aligned < sizeof (struct obj) + sizeof (value) * FREE_SIZES)
    {
      ulong fields = (aligned - sizeof (struct obj)) / sizeof (value);
      union free_obj *f = free_lists[fields];
      if (f != NULL)
	{
	  free_lists[fields] = f->next;
#ifdef GCDEBUG
	  f->o.generation = minorgen;
#endif
	  return f;
	}
    }

#ifdef GCQDEBUG
  if (n > maxobjsize) maxobjsize = n;
#endif

  gc_reserve(aligned);
  return fast_gc_allocate(aligned);
}

/* Basic allocation */
/* ---------------- */

struct grecord *unsafe_allocate_record(enum mudlle_type type, ulong entries)
{
  ulong size = sizeof (struct obj) + entries * sizeof (value);
  struct grecord *newp = gc_allocate(size);

  newp->o.size = size;
  newp->o.garbage_type = garbage_record;
  newp->o.type = type;

  newp->o.flags = entries == 0 ? OBJ_READONLY | OBJ_IMMUTABLE : 0;

  /* WARNING: data is not initialised!!! */
#ifdef GCSTATS
  gcstats_add_alloc(type, size);
#endif

#ifdef ALLOC_STATS
  record_allocation(type, size);
#endif

  return newp;
}

struct grecord *allocate_record(enum mudlle_type type, ulong entries)
{
  struct grecord *newp = unsafe_allocate_record(type, entries);

  /* Initialise data to NULL */
  memset(newp->data, 0, entries * sizeof *newp->data);

  return newp;
}

struct gstring *allocate_string(enum mudlle_type type, ulong bytes)
{
  ulong size = sizeof (struct obj) + bytes;
  struct gstring *newp = gc_allocate(size);

  newp->o.size = size;
  newp->o.garbage_type = garbage_string;
  newp->o.type = type;
  newp->o.flags = OBJ_IMMUTABLE;
#ifdef GCSTATS
  gcstats_add_alloc(type, MUDLLE_ALIGN(size, sizeof (value)));
#endif
#ifdef ALLOC_STATS
  record_allocation(type, size);
#endif

  return newp;
}

struct primitive *allocate_primitive(const struct primitive_ext *op)
{
  enum mudlle_type type = (op->nargs < 0
                           ? type_varargs
                           : (op->seclevel == 0 || (op->flags & OP_FASTSEC)
                              ? type_primitive
                              : type_secure));

  struct primitive *newp = gc_allocate(sizeof *newp);
  *newp = (struct primitive){
    .o = {
      .size = sizeof *newp,
      .garbage_type = garbage_primitive,
      .type = type,
      .flags = OBJ_IMMUTABLE | OBJ_READONLY,
#ifdef GCDEBUG
      .generation = newp->o.generation,
#endif
    },
    .op = op,
    .call_count = 0
  };

#ifdef GCSTATS
  gcstats_add_alloc(type, newp->o.size);
#endif

  return newp;
}

struct gtemp *allocate_temp(enum mudlle_type type, void *ext)
{
  struct gtemp *newp = gc_allocate(sizeof *newp);

  newp->o.size = sizeof *newp;
  newp->o.garbage_type = garbage_temp;
  newp->o.type = type;
  newp->o.flags = OBJ_IMMUTABLE;
  newp->external = ext;
#ifdef GCSTATS
  gcstats_add_alloc(type, sizeof *newp);
#endif

  return newp;
}

struct vector *allocate_locals(ulong n)
/* Effect: Allocate a vector of local variables in an optimised fashion.
*/
{
  ulong vecsize = sizeof (struct obj) + n * sizeof (value);
  const ulong varsize = sizeof (struct obj) + sizeof (value);
  struct vector *locals;
  value *var;

  gc_reserve(vecsize + n * varsize);

  locals = fast_gc_allocate(vecsize);
  locals->o.size = vecsize;
  locals->o.garbage_type = garbage_record;
  locals->o.type = type_internal;
  locals->o.flags = 0;
#ifdef GCSTATS
  gcstats_add_alloc(type_internal, vecsize);
#endif

  var = locals->data;
  while (n--)
    {
#ifdef ALLOC_STATS
      struct variable *v = alloc_variable(NULL);
#else
      struct variable *v = fast_gc_allocate(varsize);
      v->o.size = varsize;
      v->o.garbage_type = garbage_record;
      v->o.type = type_variable;
      v->o.flags = 0;
      v->vvalue = NULL;
#endif
      *var++ = v;
#ifdef GCSTATS
      gcstats_add_alloc(type_variable, varsize);
#endif
    }

  return locals;
}

/* Detect immutability */
/* ------------------- */

/* checks if one object can be made immutable; returns true if possible */
bool check_immutable(struct obj *obj)
{
  if (obj->garbage_type != garbage_record ||
      (obj->flags & (OBJ_READONLY | OBJ_IMMUTABLE)) != OBJ_READONLY)
    return false;

  struct grecord *rec = (struct grecord *)obj;
  struct obj **recend = (struct obj **)((ubyte *)rec + rec->o.size);
  for (struct obj **o = rec->data; ; ++o)
    {
      if (o == recend)
        {
          /* Readonly objects can be made immutable if all their
             contents are themselves immutable.
             Note that this will not detect non-trivial recursive
             immutable objects */

          rec->o.flags |= OBJ_IMMUTABLE;
          return true;
        }
      /* if contains non-immutable pointer (not to itself), give up */
      if (*o != obj && !immutablep(*o))
        return false;
    }
}

void detect_immutability(void)
{
  garbage_collect(0); /* Get rid of junk in generation 0 */
  for (;;)
    {
      bool change = false;
      ubyte *ptr = posgen0;
      while (ptr < endgen0)
	{
	  struct obj *obj = (struct obj *)ptr;

	  ptr += MUDLLE_ALIGN(obj->size, sizeof (value));

          change |= check_immutable(obj);
	}
      if (!change)
        break;
    }
}



/* Persistent data */
/* --------------- */

static ulong from_offset;	/* Value to add to pointers in forward to get
				   real pointer address.
				   (used to unpatch a dump file) */
static ubyte *save_hwm;

static void save_restore(struct obj *obj)
/* Effects: Restores the information used during GC for obj (forwarding)
*/
{
 again:
  if (!pointerp(obj))
    return;

  if (obj->garbage_type == garbage_static_string)
    {
      *static_data(obj) = 0;
      return;
    }

  if (obj->garbage_type != garbage_forwarded)
    return;

  struct obj *copy = (struct obj *)obj->size;

  /* Restore this value */
  obj->garbage_type = copy->garbage_type;
  if ((ubyte *)copy < save_hwm)
    obj->size = ntohlong(copy->size);
  else /* if save aborted, data at end has not been networkized */
    obj->size = copy->size;

  unsigned n;
  struct obj **no;

  /* And its contents, recursively */
  switch (obj->garbage_type)
    {
    case garbage_record: {
      struct grecord *rec = (struct grecord *)obj;

      n = (rec->o.size - sizeof (struct obj)) / sizeof (struct obj *);
      no = rec->data;
      goto tail;
    }
    case garbage_code: {
      struct icode *icode = (struct icode *)obj;

      save_restore_code(&icode->code);
      save_restore(&icode->lineno_data->o);
      n = icode->nb_constants;
      no = icode->constants;
      goto tail;
    }
    case garbage_mcode:
      save_restore_mcode((struct mcode *)obj);
      return;
    case garbage_primitive: case garbage_temp:
    case garbage_string: case garbage_forwarded:
      return;
    case garbage_static_string: case garbage_free:
      abort();
    }
  abort();

 tail:
  /* manual tail recursion */
  if (n == 0)
    return;
  while (n-- > 1)
    save_restore(*no++);
  obj = *no;
  goto again;
}

static jmp_buf nomem;
static ulong mutable_size, static_size;

static bool size_forward(void *_ptr)
{
  struct obj **ptr = _ptr;
  struct obj *obj = *ptr;
  struct obj *newobj;
  long size;

  size = obj->size;
  /* GC to generation 0, allocating forwards */
  if (obj->garbage_type == garbage_forwarded)
    *ptr = (value)size;
  else
    {
      long asize = MUDLLE_ALIGN(size, sizeof (value));
      if (obj->garbage_type == garbage_static_string)
        {
          ulong *sdata = static_data(obj);
          if (*sdata)
            {
              *ptr = (value)*sdata;
              return false;
            }
          static_size += asize;
        }

      /* Forward to generation 0 */
      newobj = (struct obj *)newpos0;
      newpos0 += asize;
      if (!(obj->flags & OBJ_IMMUTABLE))
        mutable_size += asize;
      if (newpos0 > newend0) longjmp(nomem, 1);

      *ptr = move_object(obj, newobj, minorgen);
    }
  return false;
}

void gc_size(value x, struct gc_size *size)
/* Effects: Returns number of bytes accessible from x
     Sets mutable (if not NULL) to the # of mutable bytes in x
   Modifies: mutable
*/
{
  value *save;
  volatile ulong gcsize = DEF_SAVE_SIZE;

  for (;;)
    {
      if (!setjmp(nomem))
	{
	  /* Be really sure that there is enough space for the header */
	  GCPRO1(x);
	  gc_reserve(gcsize);
	  UNGCPRO();

	  major_offset = 0;
	  special_forward = size_forward;
	  newstart0 = newpos0 = posgen0 - gcsize; newend0 = posgen0;

	  /* Forward the root value */
	  static_size = mutable_size = 0;
	  save = (value *)newpos0;
	  newpos0 += sizeof (value);
	  *save = x;
	  if (pointerp(x)) special_forward(save);

	  /* Scan copied data */
	  save_hwm = (ubyte *)(save + 1);
	  while (save_hwm < newpos0)
	    {
	      struct obj *o = (struct obj *)save_hwm;
	      save_hwm = scan(save_hwm);
	      o->size = htonlong(o->size); /* save_restore undoes this */
	    }
	  assert(save_hwm == newpos0);

	  /* Restore old block (contains forwarded data) */
	  save_restore(x);

          *size = (struct gc_size){
            .s_total = (ulong)(newpos0 - newstart0),
            .s_mutable = mutable_size,
            .s_static = static_size
          };
          return;
	}
      else
	{
	  /* No memory, try again */
	  save_restore(x);
	  gcsize *= 2;
	}
    }
}

/*
  Saved data format:
    ulong        self address; used for relocation
    struct obj   gone object
    value        saved value
 */
static void save_forward(struct obj **ptr)
{
  struct obj *obj = *ptr;

  GCCHECK(obj);

  if (obj->garbage_type == garbage_primitive ||
      obj->garbage_type == garbage_temp ||
      obj->garbage_type == garbage_code ||
      obj->garbage_type == garbage_mcode ||
      obj->type == type_closure ||
      obj->type == type_oport ||
      obj->type == type_internal ||
      obj->type == type_private)
    {
      /* Set ptr to the gone value */
      *ptr = (struct obj *)(newstart0 + sizeof (ubyte *));
    }
  else
    {
      if (obj->garbage_type == garbage_static_string)
        {
          ulong *sdata = static_data(obj);
          if (*sdata)
            {
              *ptr = (value)*sdata;
              return;
            }
        }

      if (obj->garbage_type == garbage_record
          && obj->type == type_symbol)
        {
          /* assert that symbol names are readonly */
          struct string *name = ((struct symbol *)obj)->name;
          uint16_t flags;
          if (name->o.garbage_type == garbage_forwarded)
            {
              name = (value)name->o.size;
              flags = ntohs(name->o.flags);
            }
          else
            flags = name->o.flags;
          assert(name->o.garbage_type == garbage_string
                 || name->o.garbage_type == garbage_static_string);
          assert(name->o.type == type_string);
          assert(flags & OBJ_READONLY);
        }

      /* GC to generation 0, allocating forwards */
      long size = obj->size;
      if (obj->garbage_type == garbage_forwarded)
	*ptr = (value)size;
      else
	{
	  /* Forward to generation 0 */
          struct obj *newobj = (struct obj *)newpos0;
          long align_pad = -size & (sizeof (value) - 1);
          ubyte *padpos = newpos0 + size;
	  newpos0 = padpos + align_pad;
	  if (newpos0 > newend0) longjmp(nomem, 1);
          memset(padpos, 0, align_pad);
	  *ptr = move_object(obj, newobj, minorgen);
          newobj->flags = htons(newobj->flags);
	}
    }
}

static ubyte *save_scan(ubyte *ptr)
{
  struct obj *obj = (struct obj *)ptr;

  ptr += MUDLLE_ALIGN(obj->size, sizeof (value));

  if (obj->garbage_type == garbage_record)
    {
      struct grecord *rec = (struct grecord *)obj;
      struct obj **o, **recend;

      recend = (struct obj **)((ubyte *)rec + rec->o.size);
      o = rec->data;
      while (o < recend)
	{
	  if (pointerp(*o)) save_forward(o);
	  *o = (struct obj *)htonlong((ulong)*o);
	  o++;
	}
    }

  obj->size = htonlong(obj->size);

  return ptr;
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
  value *save;
  volatile ulong gcsize = DEF_SAVE_SIZE;

  for (;;)
    {
      if (!setjmp(nomem))
	{
	  /* Be really sure that there is enough space for the header */
	  GCPRO1(x);
	  gc_reserve(gcsize);
	  UNGCPRO();

	  major_offset = 0;
	  newstart0 = newpos0 = posgen0 - gcsize; newend0 = posgen0;

	  /* Save data needed for reload: address of start of block */
	  /* This address is also used as a special `gone' marker */
	  *(ubyte **)newpos0 = (ubyte *)htonlong((ulong)newstart0);
	  newpos0 += sizeof (ubyte *);

	  /* Add a nice gone value */
          *(struct obj *)newpos0 = (struct obj){
            .size         = htonlong(sizeof (struct obj)),
            .garbage_type = garbage_string,
            .type         = type_gone,
            .flags        = htons(OBJ_READONLY | OBJ_IMMUTABLE),
#ifdef GCDEBUG
            .generation   = minorgen,
#endif
          };
	  newpos0 += sizeof (struct obj);

	  /* Forward the root value */
	  save = (value *)newpos0;
	  newpos0 += sizeof (value);
	  *save = x;
	  if (pointerp(x)) save_forward((struct obj **)save);
	  *save = (value)htonlong((ulong)*save);

	  /* Scan copied data */
	  save_hwm = (ubyte *)(save + 1);
	  while (save_hwm < newpos0) save_hwm = save_scan(save_hwm);
	  assert(save_hwm == newpos0);

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

struct obj32
{
  uint32_t size;
  enum garbage_type garbage_type : 8;
  enum mudlle_type type : 8;
  uint16_t flags;
#ifdef GCDEBUG
  uint32_t generation;
#endif
};

struct grecord32
{
  struct obj32 o;
  uint32_t data[];
};

/* legacy tables only exist in 32-bit; union to avoid aliasing problems */
union legacy_table
{
  struct obj o;
  struct {
    struct obj32 o;
    union {
      struct {
        uint32_t size;
        uint32_t used;
        uint32_t buckets;
      } old;
      struct {
        uint32_t used;
        uint32_t buckets;
      } new;
    } u;
  } o32;
};

/* requires ltab->o.size to be in host endianness */
static void update_legacy_table(union legacy_table *ltab)
{
  if (ltab->o32.o.type != type_table
      || ltab->o32.o.size != sizeof ltab->o32.o + sizeof ltab->o32.u.old)
    return;

  uint32_t used = ltab->o32.u.old.used;
  uint32_t buckets = ltab->o32.u.old.buckets;
  ltab->o32.u.new.used = used;
  ltab->o32.u.new.buckets = buckets;
  ltab->o32.o.size = sizeof ltab->o32.o + sizeof ltab->o32.u.new;
}

static bool load_forward(void *_ptr)
{
  struct obj **ptr = _ptr;
  struct obj *obj, *newobj;

  /* Correct pointer addresses on load */
  obj = (struct obj *)((ubyte *)*ptr + from_offset);

  /* And GC them to generation 0 */
  /* This only works because garbage_type is a single byte... */
  if (obj->garbage_type == garbage_forwarded)
    *ptr = (value)obj->size;
  else
    {
      /* Forward to generation 0 */
      obj->size = ntohlong(obj->size);
      update_legacy_table((union legacy_table *)obj);
      obj->flags = ntohs(obj->flags);

      newobj = (struct obj *)(newpos0 -= MUDLLE_ALIGN(obj->size,
                                                      sizeof (value)));
      assert(newpos0 >= newstart0);
#ifdef GCDEBUG
      obj->generation = minorgen;
#endif
#ifdef GCQDEBUG
      if (obj->size > maxobjsize) maxobjsize = obj->size;
#endif
      *ptr = move_object(obj, newobj, minorgen);
    }
  return false;
}

struct table_node {
  struct table_node *next;
  struct table *t;
};

static ubyte *load_scan(ubyte *ptr, struct table_node **old_tables,
                        bool make_sym_names_ro)
{
  struct obj *obj = (struct obj *)ptr;

  assert(obj->size > 0);
  ptr += MUDLLE_ALIGN(obj->size, sizeof (value));

#ifdef GCSTATS
  gcstats_add_alloc(obj->type, MUDLLE_ALIGN(obj->size, sizeof (value)));
#endif

  if (obj->garbage_type == garbage_record)
    {
      if (obj->flags & OBJ_IMMUTABLE)
	assert(obj_readonlyp(obj));

      struct grecord *rec = (struct grecord *)obj;
      struct obj **recend = (struct obj **)((ubyte *)rec + rec->o.size);
      struct obj **o = rec->data;
      while (o < recend)
	{
	  *o = (struct obj *)ntohlong((ulong)*o);
	  if (pointerp(*o))
	    {
	      special_forward(o);
	      if (obj->flags & OBJ_IMMUTABLE)
		assert((*o)->flags & OBJ_IMMUTABLE);
	    }
	  o++;
	}

      if (old_tables && obj->type == type_table)
        {
          struct table_node *t = malloc(sizeof *t);
          *t = (struct table_node){
            .next = *old_tables,
            .t    = (struct table *)obj
          };
          *old_tables = t;
        }

      if (make_sym_names_ro && obj->type == type_symbol)
        {
          /* force symbol names to be readonly */
          struct string *name = ((struct symbol *)obj)->name;
          assert(name->o.garbage_type == garbage_string);
          assert(name->o.type == type_string);
          make_readonly(name);
        }
    }
  return ptr;
}

static value _gc_load(bool (*forwarder)(void *_ptr),
		      ubyte *load, value *old, unsigned long size,
		      bool rehash_tables, bool make_sym_names_ro)
/* Effects: Reloads a value saved with gc_save. <load,size> delimits
     the zone of memory containing gc_save's results.
     See gc_save for details.
   Returns: The loaded value
*/
{
  ubyte *data, *unscanned0, *oldstart0;

  /* Data is loaded into generation 0 */

  major_offset = 0;
  special_forward = forwarder;
  newstart0 = startgen0; newend0 = endgen0; newpos0 = posgen0;

  /* Old value of pointer to first value, forward it */
  *old = (value)ntohlong((ulong)*old);
  if (pointerp(*old)) special_forward(old);

  /* track tables that have to be rehashed */
  struct table_node *old_tables = NULL;
  struct table_node **old_tables_ptr = rehash_tables ? &old_tables : NULL;

  /* Must scan forwards, but data is allocated downwards => double loop */
  unscanned0 = posgen0;		/* Upper bound of unscanned data */
  do {
    oldstart0 = data = newpos0;
    while (data < unscanned0)
      data = load_scan(data, old_tables_ptr, make_sym_names_ro);
    assert(data == unscanned0);
    unscanned0 = oldstart0;
  } while (oldstart0 != newpos0); /* Till nothing forwarded */

  posgen0 = newpos0;

  for (struct table_node *t = old_tables, *next; t; t = next)
    {
      next = t->next;
      rehash_table(t->t);
      free(t);
    }

  return *old;
}

static bool internal_load_forward32(void *_ptr, bool strip_generation)
{
  struct obj **ptr = _ptr;

  /* Correct pointer addresses on load */
  struct obj32 *obj = (struct obj32 *)((ubyte *)*ptr + from_offset);

  /* And GC them to generation 0 */
  if (obj->garbage_type == garbage_forwarded)
    *ptr = (value)(posgen0 - obj->size);
  else
    {
      struct obj32 *orig_obj = obj;

      obj->size = ntohl(obj->size);

      if (strip_generation)
	{
#ifdef GCDEBUG
	  abort();
#endif
	  /* strip stored generation field by moving struct obj32 forward;
	     cheaper than moving following data backwards */
	  union obj32_debug {
	    struct {
	      struct obj32 o;
	      uint32_t generation;
	    } old;
	    struct {
	      uint32_t unused;
	      struct obj32 o;
	    } new;
	  } *dobj = (union obj32_debug *)obj;
	  CASSERT_EXPR(offsetof(union obj32_debug, new.o) == 4);
	  CASSERT_EXPR(sizeof (dobj->old) == sizeof (dobj->new));
	  struct obj32 o = dobj->old.o;
	  o.size -= sizeof dobj->old - sizeof o;
	  dobj->new.o = o;
	  obj = &dobj->new.o;
	}

      update_legacy_table((union legacy_table *)obj);

      /* Forward to generation 0 and change from 32-bit format */
      uint32_t size = obj->size; /* 32-bit size */
      uint32_t nsize;		 /* native size */
      switch (obj->garbage_type)
	{
	case garbage_string:
	  nsize = size - sizeof (struct obj32) + sizeof (struct obj);
	  break;
	case garbage_record:
	  nsize = (((size - sizeof (struct obj32))
		    / sizeof (uint32_t)
		    * sizeof (value))
		   + sizeof (struct obj));
	  break;
	default:
	  abort();
	}
      uint32_t asize = MUDLLE_ALIGN(nsize, sizeof (value));
      struct obj *newobj = (struct obj *)(newpos0 -= asize);
      assert(newpos0 >= newstart0);

      newobj->size = nsize;
      newobj->garbage_type = obj->garbage_type;
      newobj->type = obj->type;
      newobj->flags = ntohs(obj->flags);
#ifdef GCDEBUG
      newobj->generation = minorgen;
#endif

      switch (newobj->garbage_type)
	{
	case garbage_string:
	  memcpy(newobj + 1, obj + 1, size - sizeof *obj);
	  break;
	case garbage_record:
	  {
	    struct grecord32 *gobj = (struct grecord32 *)obj;
	    ulong orecs = (size - sizeof *gobj) / sizeof gobj->data[0];
	    for (int i = 0; i < orecs; ++i)
	      {
		uint32_t d32 = ntohl(gobj->data[i]);
		/* only integers must be sign-extended */
		ulong d64 = pointerp(d32) ? d32 : (long)(int32_t)d32;
		((struct grecord *)newobj)->data[i] = (value)htonlong(d64);
	      }
	    break;
	  }
	default:
	  abort();
	}

      orig_obj->garbage_type = garbage_forwarded;
      newobj = (struct obj *)((ubyte *)newobj + major_offset);
      orig_obj->size = posgen0 - (ubyte *)newobj;

      *ptr = newobj;
    }
  return false;
}

static bool load_forward32(void *_ptr)
{
  return internal_load_forward32(_ptr, false);
}

#ifndef GCDEBUG
static bool load_forward_debug32(void *_ptr)
{
  return internal_load_forward32(_ptr, true);
}

static bool load_forward_debug(void *_ptr)
{
  struct obj **ptr = _ptr;

  /* Correct pointer addresses on load */
  struct obj *obj = (struct obj *)((ubyte *)*ptr + from_offset);

  /* And GC them to generation 0 */
  if (obj->garbage_type == garbage_forwarded)
    {
      *ptr = (value)obj->size;
      return false;
    }

  /* Remove redundant field; inefficient but should only be used as a
     fall-back. */
  long size = ntohlong(obj->size) - sizeof (long);
  obj->size = htonlong(size);
  memmove(obj + 1, (ubyte *)(obj + 1) + sizeof (long), size - sizeof *obj);
  *(long *)((char *)obj + size) = 0;
  return load_forward(_ptr);
}
#endif  /* ! GCDEBUG */

/* load 32-bit saved mudlle value */
static value gc_load32(void *_load, unsigned long size,
		       enum mudlle_data_version version)
{
  ubyte *load = _load;

  bool (*forwarder)(void *_ptr) = load_forward32;

  struct obj32 *gone = (struct obj32 *)(load + sizeof (uint32_t));
  uint32_t gsize = ntohl(gone->size);
  uint32_t old32 = ntohl(*(uint32_t *)((char *)gone + gsize));
  ulong old64 = pointerp(old32) ? old32 : (long)(int32_t)old32;
  value old = (value)htonlong(old64);
  if (gsize == sizeof *gone)
    ;
#ifndef GCDEBUG
  else if (gsize == sizeof *gone + sizeof (uint32_t))
    forwarder = load_forward_debug32;
#endif
  else
    abort();

  from_offset = load - (ubyte *)(ulong)ntohl(*(uint32_t *)load);

  gc_reserve((size - sizeof (uint32_t) - sizeof (uint32_t)) * 2);

  return _gc_load(forwarder, load, &old, size, true,
                  version < MDATA_VER_RO_SYM_NAMES);
}

value gc_load(void *_load, unsigned long size,
              enum mudlle_data_version version)
/* Effects: Reloads a value saved with gc_save. <load,size> delimits
     the zone of memory containing gc_save's results.
     See gc_save for details.
   Returns: The loaded value
*/
{
  ubyte *load = _load;

  if (sizeof (ulong) > sizeof (uint32_t))
    {
      /* check for 32-bit format */
      struct obj32 *gone32 = (struct obj32 *)(load + sizeof (uint32_t));
      uint32_t gsize32 = ntohl(gone32->size);
      if (gsize32 == sizeof *gone32
	  || gsize32 == sizeof *gone32 + sizeof (uint32_t))
	return gc_load32(_load, size, version);
    }

  bool (*forwarder)(void *_ptr) = load_forward;

  struct obj *gone = (struct obj *)(load + sizeof (value));
  long gsize = ntohlong(gone->size);
  value *old = (value *)((char *)gone + gsize);
  if (gsize == sizeof *gone)
    ;
#ifndef GCDEBUG
  else if (gsize == sizeof *gone + sizeof (ulong))
    forwarder = load_forward_debug;
#endif
  else
    abort();

  from_offset = load - (ubyte *)ntohlong((ulong)*(ubyte **)load);

  gc_reserve(size - sizeof (ubyte *) - sizeof (value));

  return _gc_load(forwarder, load, old, size, version < MDATA_VER_NEW_HASH,
                  version < MDATA_VER_RO_SYM_NAMES);
}

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

#if defined(i386) && !defined(NOCOMPILER)
static void flush_icache(ubyte *from, ubyte *to)
{
  VALGRIND_DISCARD_TRANSLATIONS((ulong)from, (ulong)(to - from));
}

struct mcode *find_pc_mcode(ulong pc, ulong range_start, ulong range_end)
{
  ubyte *magic = (ubyte *)((pc & ~(CODE_ALIGNMENT - 1))
                           - sizeoffield(struct mcode, magic));

  if ((ulong)magic < range_start || pc > range_end)
    return NULL;

  /* We have found a collectable code object.
     First find where it begins by locating its
     magic sequence */
  while (((ulong *)magic)[0] != 0xffffffff
         || ((ulong *)magic)[1] != 0xffffffff)
    magic -= CODE_ALIGNMENT;

  struct mcode *mcode
    = (struct mcode *)(magic - offsetof(struct mcode, magic));;
  assert((ulong)mcode >= range_start);

  /* pc may be after the last instruction byte as that may be a
     call to berror_xxx */
  assert(pc >= (ulong)&mcode->mcode[0]
         && pc <= (ulong)&mcode->mcode[mcode->code_length]);
  return mcode;
}

static void forward_pc(ulong *pcreg)
{
  struct mcode *base = find_pc_mcode(*pcreg, (ulong)gcrange_start,
                                     (ulong)gcrange_end);
  if (base == NULL)
    return;

  GCCHECK(base);
  struct mcode *oldbase = base;
  special_forward(((char *)&base)); /* type punning */

  /* And adjust the pc value for the new base location */
  *pcreg = (ulong)base + (*pcreg - (ulong)oldbase);
}

static void forward_ccontext(struct ccontext *cc)
{
  if (pointerp(cc->callee[0]))
    special_forward(&cc->callee[0]);
  if (pointerp(cc->callee[1]))
    special_forward(&cc->callee[1]);
  if (pointerp(cc->caller[0]))
    special_forward(&cc->caller[0]);
  if (pointerp(cc->caller[1]))
    special_forward(&cc->caller[1]);
}

static void forward_registers(void)
{
  for (struct ccontext *cc = &ccontext;
       cc->frame_start;
       cc = next_ccontext(cc))
    {
      forward_ccontext(cc);

      ulong *sp, *bp;
      ccontext_frame(cc, &bp, &sp);

      // The return address is at sp[-1]
      forward_pc(&sp[-1]);

      /* First frame is special: it may contain C arguments that might
	 be GCPROtected. So we must use safe_forward */
      forward_pc(bp + 1);
      /* forward mudlle values & caller's closure) */
      for (ulong *x = sp; x < bp; x++)
	if (pointerp(*x)) safe_forward((value *)x);

      sp = bp + 2;
      assert(bp[0] > (ulong)bp);
      bp = (ulong *)bp[0];

      while (bp < cc->frame_start)
	{
	  /* bp[-1] is caller's closure
	   * bp[0] is previous bp
	   * bp[1] is return address
	   * sp[0] -> bp[-2] is mudlle values
	   */
	  forward_pc(bp + 1);
          /* forward mudlle values & caller's closure) */
	  for (ulong *x = sp; x < bp; x++)
	    if (pointerp(*x)) special_forward(x);

	  sp = bp + 2;
	  assert(bp[0] > (ulong)bp);
	  bp = (ulong *)bp[0];
	}
      assert(bp == cc->frame_start);

      ulong *argsend = ccontext_argsend(cc);
      for (ulong *x = sp; x < argsend; x++)
	if (pointerp(*x)) special_forward(x);
    }

  /* Also forward saved ccontexts */
  for (struct catch_context *mcc = catch_context; mcc; mcc = mcc->parent)
    forward_ccontext(&mcc->old_ccontext);
}

static void unmark_safe_registers(void)
{
  for (struct ccontext *cc = &ccontext;
       cc->frame_start;
       cc = next_ccontext(cc))
    {
      ulong *sp, *bp;
      ccontext_frame(cc, &bp, &sp);

      /* Unmark safe forwarded variables from first frame */
      for (ulong *x = sp; x < bp; x++)
	unmark_safe((value *)x);
    }
}

static long get_long(const char **src)
{
  long l;
  memcpy(&l, *src, sizeof l);
  *src += sizeof l;
  return l;
}

static long get_uword(const char **src)
{
  uword u;
  memcpy(&u, *src, sizeof u);
  *src += sizeof u;
  return u;
}

static const char *get_offsets(struct mcode *code,
                               long (**getter)(const char **))
{
  const char *mcode = (const char *)code->mcode;
  if (code->code_length > (uword)~0)
    {
      *getter = get_long;
      return mcode + MUDLLE_ALIGN(code->code_length, sizeof (long));
    }
  *getter = get_uword;
  return mcode + MUDLLE_ALIGN(code->code_length, sizeof (uword));
}

static void scan_mcode(struct mcode *code)
{
  special_forward_code(&code->code);
  if (pointerp(code->linenos)) special_forward(&code->linenos);

  ubyte *mcode = code->mcode;
  long (*get_const)(const char **);
  const char *offsets = get_offsets(code, &get_const);

  for (int i = code->nb_constants; i > 0; --i)
    {
      long ofs = get_const(&offsets);
      if (pointerp(*(value *)(mcode + ofs)))
	special_forward(mcode + ofs);
    }

  /* Relocate calls to builtins */
  ubyte *old_base = (ubyte *)code->myself;
  code->myself = (struct mcode *)((ubyte *)code + major_offset);
  ulong delta = old_base - (ubyte *)code->myself;
  for (int i = code->nb_rel; i > 0; --i)
    {
      long ofs = get_const(&offsets);
      *(ulong *)(mcode + ofs) += delta;
    }
}

static void save_restore_mcode(struct mcode *code)
{
  save_restore_code(&code->code);
  save_restore(&code->linenos->o);

  ubyte *mcode = code->mcode;
  long (*get_const)(const char **);
  const char *offsets = get_offsets(code, &get_const);
  for (int i = code->nb_constants; i > 0; --i)
    {
      long ofs = get_const(&offsets);
      save_restore(*(value *)(mcode + ofs));
    }
}

static void patch_value(value *x, value oldglobals, value newglobals)
{
  if (*x == oldglobals)
    *x = newglobals;
}

void patch_globals_stack(value oldglobals, value newglobals)
{
  for (struct ccontext *cc = &ccontext;
       cc->frame_start;
       cc = next_ccontext(cc))
    {
      /* callee[1] is used to store reg_globals (esi) */
      patch_value(&cc->callee[1], oldglobals, newglobals);

      ulong *sp, *bp;
      ccontext_frame(cc, &bp, &sp);

      while (bp < cc->frame_start)
	{
	  /* bp[-1] is caller's closure
	   * bp[0] is previous bp
	   * bp[1] is return address
	   * sp[0] -> bp[-2] is mudlle values
	   */
	  for (ulong *x = sp; x < bp; x++)
	    patch_value((value *)x, oldglobals, newglobals);

	  sp = bp + 2;
	  assert(bp[0] > (ulong)bp);
	  bp = (ulong *)bp[0];
	}
      assert(bp == cc->frame_start);
    }

  /* Also forward saved ccontexts */
  for (struct catch_context *mcc = catch_context; mcc; mcc = mcc->parent)
    patch_value(&mcc->old_ccontext.callee[1], oldglobals, newglobals);
}

#endif  /* i386 && ! NOCOMPILER */

#ifdef NOCOMPILER
struct mcode *find_pc_mcode(ulong pc, ulong range_start, ulong range_end)
{
  return NULL;
}

static void flush_icache(ubyte *from, ubyte *to)
{
}

static void forward_registers(void)
{
}

static void unmark_safe_registers(void)
{
}

static void save_restore_mcode(struct mcode *code)
{
}
#endif  /* NOCOMPILER */

#if 0

static int what_generation(value o)
{
  if ((ubyte *)o >= startgen0 && (ubyte *)o < endgen0)
    return 0;
  if ((ubyte *)o >= startgen1 && (ubyte *)o < endgen1)
    return 1;
  abort();
}

void gc_verify(value _o, bool start, int gen, bool immut)
{
  if (_o == NULL || (unsigned long)_o & 1)
    return;

  struct obj *o = _o;
  if (start ? o->garbage_type & 0x80 : ~o->garbage_type & 0x80)
    return;

  if (gen < 0)
    gen = what_generation(o);
  else if (gen == 1)
    assert(what_generation(o) == 1);
  else
    gen = what_generation(o);

  if (immut)
    assert(immutablep(o));

  immut = immutablep(o);
  if (gen == 1)
    assert(immut);

  if (!start)
    o->garbage_type &= ~0x80;

  switch (o->garbage_type)
    {
    case garbage_temp:
      if (o->type != type_internal)
        abort();
      break;
    case garbage_primitive:
      assert(o->type == type_primitive
             || o->type == type_secure
             || o->type == type_varargs);
      break;
    case garbage_string:
      if (!(o->type == type_string
            || o->type == type_gone
            || o->type == type_float))
        abort();
      break;
    case garbage_record:
      {
	value *recend = (void *)o + o->size;
	value *rec = (void *)(o + 1);

	if (immut)
	  assert(obj_readonlyp(o));

	if (!(o->type == type_vector
              || o->type == type_pair
              || o->type == type_table
              || o->type == type_internal
              || o->type == type_closure
              || o->type == type_oport
              || o->type == type_symbol))
          abort();

	while (rec < recend)
	  gc_verify(*rec++, start, gen, immut);

	break;
      }
    case garbage_code:
      {
	if (immut)
          assert(obj_readonlyp(o));

        switch (o->type)
          {
          case type_code:
            {
              struct icode *icode = (struct icode *)o;

              gc_verify(icode->code.help, start, gen, immut);
              gc_verify(icode->code.filename, start, gen, immut);
              gc_verify(icode->code.varname, start, gen, immut);
              gc_verify(code->lineno_data, start, gen, immut);
              gc_verify(icode->code.arg_types, start, gen, immut);
              break;
            }
          case type_mcode:
            /* unimplemented */
          default:
            abort();
          }
        break;
      }
    default:
      abort();
    }

  if (start)
    o->garbage_type |= 0x80;
}

#endif /* 0 */
