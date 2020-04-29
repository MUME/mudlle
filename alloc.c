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

#include "mudlle-config.h"

#include <errno.h>
#include <fcntl.h>
#include <setjmp.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <netinet/in.h>

#include <sys/mman.h>

#include "alloc.h"
#include "assoc.h"
#include "builtins.h"
#include "context.h"
#include "dwarf.h"
#include "mvalgrind.h"
#include "table.h"
#include "utils.h"

#include "runtime/basic.h"
#include "runtime/runtime.h"


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
#define MCODE_OFFSET PADDING(offsetof(struct mcode, mcode), CODE_ALIGNMENT)

/* Roots */
struct gcpro *gcpro;		/* Local (C) variables which need protection */

static struct dynpro dynpro_head = {
  .next = &dynpro_head,
  .prev = &dynpro_head
};
#define MAXROOTS 100
static struct gc_root {
  const char *desc, *file;
  int line;
  value *data;
} roots[MAXROOTS];

static ulong last_root;

union free_obj {
  struct grecord g;
  union free_obj *next;         /* reuses the size field */
};
CASSERT(offsetof(union free_obj, next)
        == offsetof(union free_obj, g.o.size));

#define FREE_SIZES 12
static union free_obj *free_lists[FREE_SIZES];

enum {
  nomem_grow_memory = 1,
  nomem_out_of_memory
};
static bool alloc_can_fail;     /* if true, gc_reserve() may longjmp() */
static sigjmp_buf nomem;

/* machine code roots */
static void forward_registers(void);
static void unmark_safe_registers(void);
static void save_restore_mcode(struct mcode *code);
static void save_restore(struct obj *obj);
#ifndef NOCOMPILER
static void scan_mcode(struct mcode *code);
#endif
static void flush_icache(uint8_t *from, uint8_t *to);

static struct assoc_array static_data_table = {
  .type = &long_to_voidp_assoc_array_type
};

static void free_static_data_table(void)
{
  assoc_array_free(&static_data_table);
}

static ulong *static_data(struct obj *obj)
{
  ulong *r = (ulong *)assoc_array_lookup_ref(&static_data_table, obj);
  if (r != NULL)
    return r;

  assoc_array_set(&static_data_table, obj, NULL);
  return (ulong *)assoc_array_lookup_ref(&static_data_table, obj);
}

static inline ulong htonlong(ulong l)
{
#ifdef WORDS_BIGENDIAN
  return l;
#else
  CASSERT_EXPR(sizeof l == 4 || sizeof l == 8);
  if (sizeof l == 4)
    return htonl(l);
  /* __builtin_bswap64() is available in GCC versions 4.3 and later */
  return __builtin_bswap64(l);
#endif
}

static inline ulong ntohlong(ulong l)
{
  return htonlong(l);
}

void internal_staticpro(void *pro, const char *desc, const char *file,
                        int line)
{
  assert(last_root < MAXROOTS);

  if (*desc == '&')
    ++desc;
  roots[last_root++] = (struct gc_root){
    .data = pro,
    .desc = desc,
    .file = file,
    .line = line
  };
}

struct list *get_dynpro_data(void)
{
  struct list *result = NULL;
  GCPRO(result);

  for (struct dynpro *dyn = dynpro_head.next;
       dyn != &dynpro_head;
       dyn = dyn->next)
    {
      struct vector *v = alloc_vector(3);
      SET_VECTOR(v, 0, dyn->obj);
      SET_VECTOR(v, 1, alloc_string(dyn->file));
      SET_VECTOR(v, 2, makeint(dyn->lineno));
      result = alloc_list(v, result);
    }

  UNGCPRO();
  return result;
}

struct vector *get_staticpro_data(void)
{
  struct vector *res = alloc_vector(last_root);
  GCPRO(res);
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

void internal_dynpro(struct dynpro *what, value obj, const char *file,
                     int lineno)
{
  assert(what->next == NULL && what->prev == NULL);
  GCCHECK(obj);
  *what = (struct dynpro){
    .next   = dynpro_head.next,
    .prev   = &dynpro_head,
    .obj    = obj,
    .file   = file,
    .lineno = lineno
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

struct dynpro *internal_protect(value v, const char *file, int lineno)
{
  struct dynpro *newp = xmalloc(sizeof *newp);
  *newp = (struct dynpro){ 0 };
  internal_dynpro(newp, v, file, lineno);
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
uint8_t *gcblock;
ulong gcblocksize;

/* Current allocation state */

/* Memory for generation 0 extends from startgen0 to endgen0. Allocation
   is downward, with posgen0 pointing at the start of the last allocation.
*/
extern uint8_t *startgen0, *endgen0; /* used by x86builtins.S */
uint8_t *startgen0, *endgen0, *posgen0;
static ulong minor_offset, save_offset;

static struct ary mcode_ary;    /* currently seen mcode objects */

/* During GC, the newstart0, newend0, newpos0 variables fulfill the same roles
   for the next copy of generation 0
*/
static uint8_t *newstart0, *newend0, *newpos0;

/* Memory for generation 1 extends from startgen1 to endgen1. No memory is
   allocated directly. */
static uint8_t *startgen1;
#ifdef GCDEBUG
uint8_t *endgen1;
#else
static uint8_t *endgen1;
#endif
static ulong oldsize1;

/* During GC, newstart1, newend1 and newpos1 indicate the memory for the next
   copy of generation 1. Allocation is upwards, with newpos1 pointing at the
   first byte after the last allocation. */
static uint8_t *newstart1, *newend1, *newpos1;

/* During a major GC we may run out of memory. In that case we spill to a
   temporary block, then increase the GC area. */
static uint8_t *tempblock1, *oldpos1, *oldstart1;
static ulong tempsize1;

static bool newarea;		/* true during new_major_collection */
static ulong major_offset;
static bool saw_mcode;		/* true if forwarded an mcode object */

static gc_forward_fn special_forward;

#ifdef GCDEBUG
ulong minorgen, majorgen;
#else
static ulong minorgen;
#endif

static ulong newminorgen, newmajorgen;
struct gcstats gcstats;

#ifdef GCQDEBUG
static ulong maxobjsize = 1024; /* biggest object created (ignores stuff done
				   by compiler, but those aren't big) */

static unsigned allowed_obj_flags = OBJ_READONLY | OBJ_IMMUTABLE;

void gccheck_qdebug(value x)
{
  if (!pointerp(x))
    return;
  assert(~(long)x & 2);
  struct obj *o = x;
  assert(!((o->size > maxobjsize
            && o->garbage_type != garbage_forwarded)
           || o->size < sizeof *o
           || o->garbage_type >= garbage_free
           || o->type >= last_type
           || o->flags & ~allowed_obj_flags));
}
#endif

/* Range affected by current GC */
static uint8_t *gcrange_start, *gcrange_end;

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
  static long pagesize;
  if (pagesize == 0)
    {
      pagesize = sysconf(_SC_PAGESIZE);
      assert(pagesize > 0);
    }

  void *b;
  int r = posix_memalign(&b, pagesize, size);
  if (r != 0)
    {
      assert(r == ENOMEM);
      if (alloc_can_fail)
        siglongjmp(nomem, nomem_out_of_memory);
      abort();
    }
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
  newobj = (struct obj *)((uint8_t *)newobj + major_offset);
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
  if (pointerp(code->arguments.obj)) special_forward(&code->arguments.obj);
  if (pointerp(code->linenos)) special_forward(&code->linenos);
}

static void save_restore_code(struct code *code)
{
  save_restore(&code->help->o);
  save_restore(&code->filename->o);
  save_restore(&code->nicename->o);
  save_restore(&code->varname->o);
  save_restore(&code->arguments.argv->o);
  save_restore(&code->linenos->o);
}

#define FOR_GRECORDS(o, i)                                              \
  for (struct obj **i = ((struct grecord *)(o))->data,                  \
         **const _recend = (struct obj **)((char *)(o) + (o)->size);    \
       (i) < _recend;                                                   \
       ++i)

static uint8_t *scan(uint8_t *ptr)
{
  struct obj *obj = (struct obj *)ptr;

  ptr += MUDLLE_ALIGN(obj->size, sizeof (value));

  switch (obj->garbage_type)
    {
    case garbage_record:
      FOR_GRECORDS(obj, o)
        if (pointerp(*o))
          special_forward(o);
      break;
    case garbage_code:
      {
        struct icode *icode = (struct icode *)obj;

#ifndef NOCOMPILER
        saw_mcode = true; /* code objects contain jump to interpreter */
#endif
        special_forward_code(&icode->code);

        value *const cend = icode->constants + icode->nb_constants;
        for (value *c = icode->constants; c < cend; ++c)
          if (pointerp(*c))
            special_forward(c);
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

static uint8_t *major_scan(uint8_t *data)
{
  data = scan(data);
  /* There may be holes filled with 0 bytes before code objects. Skip
     over them */
  MOVE_PAST_ZERO(data, newpos1);

  return data;
}

static uint8_t *major_scan2(uint8_t *data)
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
        case call_string_args:
        case call_string_argv:
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
        case call_invalid_argp:
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

static bool minor_forward_immutablep(void *_ptr)
/* The forward function for minor collections; return true if object
   is immutable */
{
  struct obj **ptr = _ptr;
  struct obj *obj = *ptr, *newobj;
  long size = obj->size;

  if (obj->garbage_type == garbage_forwarded)
    {
      *ptr = (value)size;
      return (uint8_t *)size < newpos1; /* true iff forwarded to gen1 */
    }

  if (obj->garbage_type == garbage_static_string)
    return true;

  GCCHECK(obj);

  if (obj->flags & OBJ_IMMUTABLE)
    {
      /* Immutable, forward to generation 1 */

      /* In minor collections only bother with generation 0 */
      if ((uint8_t *)obj < newpos1)
        return true;

      bool is_mcode = obj->garbage_type == garbage_mcode;
      /* Gen 1 grows upward */
      if (is_mcode)
        /* We align the actual code on a 16 byte boundary. This is
           a good idea on x86, and probably elsewhere too */
        {
          uint8_t *alignedpos =
            (uint8_t *)(MUDLLE_ALIGN((ulong)newpos1, CODE_ALIGNMENT)
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

      if (is_mcode)
        {
          struct mcode *mcode = (struct mcode *)newobj;
          assert(TYPE(mcode, mcode));
          ary_add(&mcode_ary, mcode);
        }

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

  /* Mutable, forward to generation 0 */

  /* Must have been in gen 0 before */
  /*assert((uint8_t *)obj >= startgen0 && (uint8_t *)obj < endgen0);*/

  newobj = (struct obj *)(newpos0 -= MUDLLE_ALIGN(size, sizeof (value)));
  /*assert(newpos0 >= newstart0);*/
  memcpy(newobj, obj, size);

#ifdef GCSTATS
  gcstats_add_gen(obj, size, 0);
#endif
#ifdef GCDEBUG
  newobj->generation = newminorgen;
#endif

  obj->garbage_type = garbage_forwarded;
  newobj = (struct obj *)((uint8_t *)newobj + minor_offset);
  obj->size = (long)newobj;
  *ptr = newobj;

  return false;
}

static void minor_forward(void *ptr)
{
  minor_forward_immutablep(ptr);
}

static uint8_t *minor_scan(uint8_t *ptr)
{
  struct obj *obj = (struct obj *)ptr;

  ptr += MUDLLE_ALIGN(obj->size, sizeof (value));

  /* Generation 0 never contains code, so only records contain pointers */
  if (obj->garbage_type == garbage_record)
    {
      bool imm = (obj->flags & (OBJ_READONLY | OBJ_IMMUTABLE)) == OBJ_READONLY;

      /* Scan & check if it can be made immutable */
      FOR_GRECORDS(obj, o)
        if (pointerp(*o) && !minor_forward_immutablep(o))
          imm = false;

      /* Readonly objects can be made immutable if all their contents
         are themselves immutable.
         Note that this will not detect recursive immutable objects */
      if (imm)
        obj->flags |= OBJ_IMMUTABLE;
    }
  return ptr;
}

static void minor_collection(void)
{
  uint8_t *unscanned0, *oldstart0, *data;
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
  ++gcstats.minor_count;
#ifdef GCSTATS
  /* Minor stats */
  gcstats.gen[0] = GCSTATS_GEN_NULL;
#endif
  forward_roots();

  /* Scan mutable copied data */
  /* Must scan forwards, but data is allocated downwards => double loop */
  unscanned0 = newend0;		/* Upper bound of unscanned data */
  do
    {
      oldstart0 = data = newpos0;
      while (data < unscanned0) data = minor_scan(data);
      assert(data == unscanned0);
      unscanned0 = oldstart0;
    }
  while (oldstart0 != newpos0); /* Till nothing forwarded */

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

static void scan_temp(uint8_t *data)
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

static void major_forward(void *_ptr)
{
  struct obj **ptr = _ptr;
  struct obj *obj = *ptr, *newobj;
  long size = obj->size, newgen;

  if (obj->garbage_type == garbage_forwarded)
    {
      *ptr = (value)size;
      return;
    }

  if (obj->garbage_type == garbage_static_string)
    return;

  GCCHECK(obj);
  /* Objects in generation 0 stay in generation 0 during major
     collections (even if they are immutable)
     (Otherwise you can't scan them for roots to gen 1) */

  bool is_mcode = false;

  if ((uint8_t *)obj >= posgen0 && (uint8_t *)obj < endgen0)
    {
      if (!newarea) return;	/* No gen 0 data needs copying */

      /* forward to new generation 0 */
      newobj = (struct obj *)(newpos0 -= MUDLLE_ALIGN(size, sizeof (value)));
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
      is_mcode = obj->garbage_type == garbage_mcode;
      if (is_mcode)
        /* We align the actual code on a 16 byte boundary. This is
           a good idea on x86, and probably elsewhere too */
        {
          uint8_t *alignedpos =
            (uint8_t *)(MUDLLE_ALIGN((ulong)newpos1 + major_offset,
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
          oldpos1 = (uint8_t *)newobj; oldstart1 = newstart1;

          ulong tsz1 = (endgen1 - startgen1) - (oldpos1 - oldstart1);
          /* allocate extra space to handle code alignment overhead */
          tempsize1 = (tsz1 * ((sizeof (struct mcode) + CODE_ALIGNMENT - 1.0)
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

  if (is_mcode)
    {
      assert(*ptr == (struct obj *)((uint8_t *)newobj + major_offset));
      ary_add(&mcode_ary, (struct mcode *)*ptr);
    }
}

static void major_collection(void)
{
  ulong nsize0;
  uint8_t *data;

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
  ++gcstats.major_count;
#ifdef GCSTATS
  /* Make stats */
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
  uint8_t *data, *oldstart0, *unscanned0;
  ulong nsize0;

  uint8_t *newblock = alloc_gc_block(newsize);

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
  ++gcstats.minor_count;
  ++gcstats.major_count;
#ifdef GCSTATS
  /* Make stats */
  gcstats.size = newsize;
  gcstats.gen[0] = gcstats.gen[1] = GCSTATS_GEN_NULL;
#endif
  gcrange_start = gcblock; gcrange_end = gcblock + gcblocksize;
  forward_roots();

  /* Scan mutable copied data */
  /* Must scan forwards, but data is allocated downwards => double loop */
  unscanned0 = newend0;		/* Upper bound of unscanned data */
  do
    {
      oldstart0 = data = newpos0;
      while (data < unscanned0) data = scan(data);
      assert(data == unscanned0);
      unscanned0 = oldstart0;
    }
  while (oldstart0 != newpos0); /* Till nothing forwarded */

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

#if 0
  fprintf(stderr, "GC %ld ...", n); fflush(stderr);
#endif

#ifdef GCSTATS
  /* Copy allocation stats */
  gcstats.size = gcblocksize;
  gcstats.l = gcstats.a;
  gcstats.a = GCSTATS_ALLOC_NULL;
#endif

  ary_empty(&mcode_ary);

  /* No space, try a minor collection */
  minor_collection();
  reset_dwarf_mcodes(0);

#if 0
  fprintf(stderr, "minor\n"); fflush(stderr);
#endif

  /* If block is too full, do a major collection.
     not(Too full) is:
       - startgen0 < posgen0
       - At least 2x generation 1 size left
       - generation 1 has not doubled in size
       - minor area not THRESHOLD_MAJOR % full. */
  if (startgen0 < posgen0
      && 2 * (endgen1 - startgen1) < gcblocksize - (endgen0 - (posgen0 - n))
      && (endgen1 - startgen1) < 2 * oldsize1
      && (endgen0 - (posgen0 - n)) * THRESHOLD_MAJOR_B <
      (endgen0 - startgen0) * THRESHOLD_MAJOR_A)
    goto done_minor;

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

  ary_empty(&mcode_ary);
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
       - not much space for the minor area (not more than THRESHOLD_MINOR
       % full) (to avoid constant garbage collection as memory usage approaches
         block size)
  */
  ulong size1;
  if (tempblock1)
    size1 = (newpos1 - newstart1) + (oldpos1 - oldstart1);
  else
    size1 = endgen1 - startgen1;

  ulong used0 = endgen0 - (posgen0 - n);
  if (!tempblock1
      && size1 * 2 + used0 < gcblocksize
      && startgen0 < posgen0
      && used0 * THRESHOLD_INCREASE_B <
      (endgen0 - startgen0) * THRESHOLD_INCREASE_A)
    goto done_major;

  /* Running out of memory. Increase block size */
  ulong newsize = (gcblocksize * INCREASE_A) / INCREASE_B;
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
  ulong need = size1 + 2 * ((THRESHOLD_INCREASE_B * used0)
                            / THRESHOLD_INCREASE_A);
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

  ary_empty(&mcode_ary);
  new_major_collection(newsize);
#ifdef GCSTATS
  fprintf(stderr, "MUDLLE: New stats: gen 0: %td of %td, gen1: %td\n",
	  endgen0 - (posgen0 - n), endgen0 - startgen0, endgen1 - startgen1);
#endif

  assert(posgen0 - n >= startgen0);

 done_major:
  reset_dwarf_mcodes(1);
 done_minor:
  register_dwarf_mcodes(1, &mcode_ary);
  ary_free(&mcode_ary);

  free_static_data_table();
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
  posgen0 -= n;
  assert(posgen0 >= startgen0);

#ifdef GCQDEBUG
  if (n > maxobjsize)
    {
      assert(n <= MAX_MUDLLE_OBJECT_SIZE);
      maxobjsize = n;
    }
#endif

#ifdef GCDEBUG
  ((struct obj *)posgen0)->generation = minorgen;
#endif
  return posgen0;
}

/* free gc'ed object known to be mutable and not referenced */
void gc_free(struct obj *o)
{
  GCCHECK(o);
  assert((o->flags & (OBJ_READONLY | OBJ_IMMUTABLE)) == 0);
  union free_obj *f = (union free_obj *)o;
  ulong size = grecord_len(&f->g);
  if (size >= FREE_SIZES)
    {
      return;
    }
  f->next = free_lists[size];
  f->g.o.garbage_type = garbage_free;
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
	  f->g.o.generation = minorgen;
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
  *newp = (struct grecord){
    .o = {
      .size	    = size,
      .garbage_type = garbage_record,
      .type	    = type,
      .flags	    = entries == 0 ? OBJ_READONLY | OBJ_IMMUTABLE : 0,
#ifdef GCDEBUG
      .generation   = newp->o.generation
#endif
    }
  };

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

  *newp = (struct gstring){
    .o = {
      .size	    = size,
      .garbage_type = garbage_string,
      .type	    = type,
      .flags	    = OBJ_IMMUTABLE,
#ifdef GCDEBUG
      .generation   = newp->o.generation
#endif
    }
  };

#ifdef GCSTATS
  gcstats_add_alloc(type, MUDLLE_ALIGN(size, sizeof (value)));
#endif
#ifdef ALLOC_STATS
  record_allocation(type, size);
#endif

  return newp;
}

struct primitive *allocate_primitive(const struct prim_op *op)
{
  enum mudlle_type type = (op->nargs < 0
                           ? type_varargs
                           : (op->seclevel == 0 || (op->flags & OP_FASTSEC)
                              ? type_primitive
                              : type_secure));

  struct primitive *newp = gc_allocate(sizeof *newp);
  *newp = (struct primitive){
    .o = {
      .size         = sizeof *newp,
      .garbage_type = garbage_primitive,
      .type         = type,
      .flags        = OBJ_IMMUTABLE | OBJ_READONLY,
#ifdef GCDEBUG
      .generation   = newp->o.generation,
#endif
    },
    .op = op
  };

#ifdef GCSTATS
  gcstats_add_alloc(type, newp->o.size);
#endif

  return newp;
}

struct obj *allocate_temp(enum mudlle_type type, size_t size)
{
  struct obj *obj = gc_allocate(size);
  *obj = (struct obj){
    .size	  = size,
    .garbage_type = garbage_temp,
    .type	  = type,
    .flags	  = OBJ_IMMUTABLE,
#ifdef GCDEBUG
    .generation   = obj->generation
#endif
  };
#ifdef GCSTATS
  gcstats_add_alloc(type, size);
#endif
  return obj;
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
  *locals = (struct vector){
    .o = {
      .size         = vecsize,
      .garbage_type = garbage_record,
      .type         = type_internal,
      .flags        = 0,
#ifdef GCDEBUG
      .generation   = locals->o.generation
#endif
    }
  };
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
      *v = (struct variable){
        .o = {
          .size         = varsize,
          .garbage_type = garbage_record,
          .type         = type_variable,
          .flags        = 0,
#ifdef GCDEBUG
          .generation   = v->o.generation
#endif
        },
        .vvalue = NULL
      };
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

/* makes obj immutable if possible without recursion; return true on success */
bool try_make_immutable(struct obj *obj)
{
  if (obj->garbage_type != garbage_record
      || (obj->flags & (OBJ_READONLY | OBJ_IMMUTABLE)) != OBJ_READONLY)
    return false;

  struct grecord *rec = (struct grecord *)obj;
  struct obj **recend = (struct obj **)((uint8_t *)rec + rec->o.size);
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
      uint8_t *ptr = posgen0;
      while (ptr < endgen0)
	{
	  struct obj *obj = (struct obj *)ptr;

	  ptr += MUDLLE_ALIGN(obj->size, sizeof (value));

          change |= try_make_immutable(obj);
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
static uint8_t *save_hwm;

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
  if ((uint8_t *)copy < save_hwm)
    obj->size = ntohlong(copy->size);
  else /* if save aborted, data at end has not been networkized */
    obj->size = copy->size;

  unsigned n;
  value *no;

  /* And its contents, recursively */
  switch (obj->garbage_type)
    {
    case garbage_record:
      {
        struct grecord *rec = (struct grecord *)obj;
        n = (rec->o.size - sizeof (struct obj)) / sizeof (struct obj *);
        no = (value *)rec->data;
        goto tail;
      }
    case garbage_code:
      {
        struct icode *icode = (struct icode *)obj;

        save_restore_code(&icode->code);
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

static ulong mutable_size, static_size;

static void size_forward(void *_ptr)
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
              return;
            }
          static_size += asize;
        }

      /* Forward to generation 0 */
      newobj = (struct obj *)newpos0;
      newpos0 += asize;
      if (!(obj->flags & OBJ_IMMUTABLE))
        mutable_size += asize;
      if (newpos0 > newend0) siglongjmp(nomem, nomem_grow_memory);

      *ptr = move_object(obj, newobj, minorgen);
    }
}

bool gc_size(value xarg, struct gc_size *sizearg)
/* Effects: Sets *sizearg to number of bytes accessible from x
   Returns: true if successful; false otherwise (out of memory)
*/
{
  volatile value x = xarg;
  struct gcpro *const volatile old_gcpro = gcpro;
  struct gc_size *const volatile size = sizearg;
  volatile ulong gcsize = DEF_SAVE_SIZE;

  for (;;)
    switch (sigsetjmp(nomem, 0))
      {
      case 0:
        /* Be really sure that there is enough space for the header */
        {
          value xlocal = x; /* don't try to protect volatile object */
          GCPRO(xlocal);
          alloc_can_fail = true;
          gc_reserve(gcsize);
          alloc_can_fail = false;
          UNGCPRO();
          x = xlocal;
        }

        major_offset = 0;
        special_forward = size_forward;
        newstart0 = newpos0 = posgen0 - gcsize; newend0 = posgen0;

        /* Forward the root value */
        static_size = mutable_size = 0;
        value *save = (value *)newpos0;
        newpos0 += sizeof (value);
        *save = x;
        if (pointerp(x)) special_forward(save);

        /* Scan copied data */
        save_hwm = (uint8_t *)(save + 1);
        while (save_hwm < newpos0)
          {
            struct obj *o = (struct obj *)save_hwm;
            save_hwm = scan(save_hwm);
            o->size = htonlong(o->size); /* save_restore undoes this */
          }
        assert(save_hwm == newpos0);

        /* Restore old block (contains forwarded data) */
        save_restore(x);
        free_static_data_table();

        *size = (struct gc_size){
          .s_total = (ulong)(newpos0 - newstart0),
          .s_mutable = mutable_size,
          .s_static = static_size
        };
        return true;

      case nomem_grow_memory:
        /* No memory, try again */
        save_restore(x);
        gcsize *= 2;
        break;

      case nomem_out_of_memory:
        /* No memory, don't try again */
        gcpro = old_gcpro;
        alloc_can_fail = false;
        free_static_data_table();
        return false;
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

  if (obj->garbage_type == garbage_primitive
      || obj->garbage_type == garbage_temp
      || obj->garbage_type == garbage_code
      || obj->garbage_type == garbage_mcode
      || obj->type == type_closure
      || obj->type == type_oport
      || obj->type == type_internal
      || obj->type == type_private)
    {
      /* Set ptr to the gone value */
      *ptr = (struct obj *)(newstart0 + sizeof (uint8_t *));
      return;
    }

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

  /* GC to save area, allocating forwards */
  long size = obj->size;
  if (obj->garbage_type == garbage_forwarded)
    {
      *ptr = (value)size;
      return;
    }

  /* forward to save area */
  struct obj *newobj = (struct obj *)newpos0;
  long align_pad = PADDING(size, sizeof (value));
  uint8_t *padpos = newpos0 + size;
  newpos0 = padpos + align_pad;
  if (newpos0 > newend0) siglongjmp(nomem, nomem_grow_memory);
  memset(padpos, 0, align_pad);
  *ptr = move_object(obj, newobj, minorgen);
  newobj->flags = htons(newobj->flags);
}

union obj_adr {
  struct obj *o;
  ulong a;
};

static uint8_t *save_scan(uint8_t *ptr)
{
  struct obj *obj = (struct obj *)ptr;

  ptr += MUDLLE_ALIGN(obj->size, sizeof (value));

  if (obj->garbage_type == garbage_record)
    {
      struct grecord *rec = (struct grecord *)obj;

      for (union obj_adr *o = (union obj_adr *)rec->data;
           o < (union obj_adr *)ptr;
           ++o)
	{
	  if (pointerp(o->o))
            {
              save_forward(&o->o);
	      o->a += save_offset;
            }
	  o->a = htonlong(o->a);
	}
    }

  obj->size = htonlong(obj->size);

  return ptr;
}

void *gc_save(value xarg, unsigned long *sizearg)
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
  volatile value x = xarg;
  unsigned long *const volatile size = sizearg;
  volatile ulong gcsize = DEF_SAVE_SIZE;

  for (;;)
    switch (sigsetjmp(nomem, 0))
      {
      case 0:
        /* Be really sure that there is enough space for the header */
        {
          value xlocal = x; /* don't try to protect volatile object */
          GCPRO(xlocal);
          gc_reserve(gcsize);
          UNGCPRO();
          x = xlocal;
        }

        major_offset = 0;
        newstart0 = newpos0 = posgen0 - gcsize; newend0 = posgen0;

        /* Save data needed for reload: address of start of block */
        save_offset = 0 - (ulong)newstart0;

        *(uint8_t **)newpos0
          = (uint8_t *)htonlong((ulong)newstart0 + save_offset);
        newpos0 += sizeof (uint8_t *);

        /* And then, a nice gone value */
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
	union obj_adr *save = (union obj_adr *)newpos0;
        newpos0 += sizeof *save;
	save->a = (ulong)x;
        if (pointerp(x))
          {
            save_forward(&save->o);
            save->a += save_offset;
          }
        save->a = htonlong(save->a);

        /* Scan copied data */
        save_hwm = (uint8_t *)(save + 1);
        while (save_hwm < newpos0) save_hwm = save_scan(save_hwm);
        assert(save_hwm == newpos0);

        /* Restore old block (contains forwarded data) */
        save_restore(x);

        *size = newpos0 - newstart0;
        free_static_data_table();
        return newstart0;

      case nomem_grow_memory:
        /* No memory, try again */
        save_restore(x);
        gcsize *= 2;
        break;

      default:
        abort();
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

  /* ensure we alias the correct type */
  if (sizeof (ulong) == sizeof (uint32_t))
    ltab->o.size = sizeof ltab->o32.o + sizeof ltab->o32.u.new;
  else
    ltab->o32.o.size = sizeof ltab->o32.o + sizeof ltab->o32.u.new;
}

static void load_forward(void *_ptr)
{
  union obj_adr *ptr = _ptr;
  struct obj *newobj;

  /* Correct pointer addresses on load */
  struct obj *obj = (struct obj *)(ptr->a + from_offset);

  /* And GC them to generation 0 */
  /* This only works because garbage_type is a single byte... */
  if (obj->garbage_type == garbage_forwarded)
    ptr->a = obj->size;
  else
    {
      /* Forward to generation 0 */
      obj->size = ntohlong(obj->size);
      if (sizeof (ulong) == sizeof (uint32_t))
	update_legacy_table((union legacy_table *)obj);
      obj->flags = ntohs(obj->flags);

      ulong asize = MUDLLE_ALIGN(obj->size, sizeof (value));
      newobj = (struct obj *)(newpos0 -= asize);
      assert(newpos0 >= newstart0);
#ifdef GCDEBUG
      obj->generation = minorgen;
#endif
#ifdef GCQDEBUG
      if (obj->size > maxobjsize) maxobjsize = obj->size;
#endif
      ptr->o = move_object(obj, newobj, minorgen);
    }
}

struct table_node {
  struct table_node *next;
  struct table *t;
};

static uint8_t *load_scan(uint8_t *ptr, struct table_node **old_tables,
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
      bool imm = obj->flags & OBJ_IMMUTABLE;
      if (imm)
	assert(obj_readonlyp(obj));

      struct grecord *rec = (struct grecord *)obj;

      for (union obj_adr *o = (union obj_adr *)rec->data;
           o < (union obj_adr *)ptr;
	   ++o)
	{
	  o->a = ntohlong(o->a);
	  if (pointerp(o->o))
	    {
	      special_forward(o);
	      if (imm)
		assert(o->o->flags & OBJ_IMMUTABLE);
	    }
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

static value _gc_load(gc_forward_fn forwarder,
		      uint8_t *load, value *_old, unsigned long size,
		      bool rehash_tables, bool make_sym_names_ro)
/* Effects: Reloads a value saved with gc_save. <load,size> delimits
     the zone of memory containing gc_save's results.
     See gc_save for details.
   Returns: The loaded value
*/
{
  uint8_t *data, *unscanned0, *oldstart0;

  /* Data is loaded into generation 0 */

  major_offset = 0;
  special_forward = forwarder;
  newstart0 = startgen0; newend0 = endgen0; newpos0 = posgen0;

  /* Old value of pointer to first value, forward it */
  union obj_adr *old = (union obj_adr *)_old;
  old->a = ntohlong(old->a);
  if (pointerp(old->o)) special_forward(&old->o);

  /* track tables that have to be rehashed */
  struct table_node *old_tables = NULL;
  struct table_node **old_tables_ptr = rehash_tables ? &old_tables : NULL;

  /* Must scan forwards, but data is allocated downwards => double loop */
  unscanned0 = posgen0;		/* Upper bound of unscanned data */
  do
    {
      oldstart0 = data = newpos0;
      while (data < unscanned0)
        data = load_scan(data, old_tables_ptr, make_sym_names_ro);
      assert(data == unscanned0);
      unscanned0 = oldstart0;
    }
  while (oldstart0 != newpos0); /* Till nothing forwarded */

  posgen0 = newpos0;

  for (struct table_node *t = old_tables, *next; t; t = next)
    {
      next = t->next;
      rehash_table(t->t);
      free(t);
    }

  return old->o;
}

static void internal_load_forward32(void *_ptr, bool strip_generation)
{
  struct obj **ptr = _ptr;

  /* Correct pointer addresses on load */
  struct obj32 *obj = (struct obj32 *)((uint8_t *)*ptr + from_offset);

  /* And GC them to generation 0 */
  if (obj->garbage_type == garbage_forwarded)
    {
      *ptr = (value)(posgen0 - obj->size);
      return;
    }

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

  *newobj = (struct obj){
    .size         = nsize,
    .garbage_type = obj->garbage_type,
    .type         = obj->type,
    .flags        = ntohs(obj->flags),
#ifdef GCDEBUG
    .generation   = minorgen
#endif
  };

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
  newobj = (struct obj *)((uint8_t *)newobj + major_offset);
  orig_obj->size = posgen0 - (uint8_t *)newobj;

  *ptr = newobj;
}

static void load_forward32(void *_ptr)
{
  internal_load_forward32(_ptr, false);
}

#ifndef GCDEBUG
static void load_forward_debug32(void *_ptr)
{
  internal_load_forward32(_ptr, true);
}

static void load_forward_debug(void *_ptr)
{
  struct obj **ptr = _ptr;

  /* Correct pointer addresses on load */
  struct obj *obj = (struct obj *)((uint8_t *)*ptr + from_offset);

  /* And GC them to generation 0 */
  if (obj->garbage_type == garbage_forwarded)
    {
      *ptr = (value)obj->size;
      return;
    }

  /* Remove redundant field; inefficient but should only be used as a
     fall-back. */
  long size = ntohlong(obj->size) - sizeof (long);
  obj->size = htonlong(size);
  memmove(obj + 1, (uint8_t *)(obj + 1) + sizeof (long), size - sizeof *obj);
  *(long *)((char *)obj + size) = 0;
  load_forward(_ptr);
}
#endif  /* ! GCDEBUG */

/* load 32-bit saved mudlle value */
static value gc_load32(void *_load, unsigned long size,
		       enum mudlle_data_version version)
{
  uint8_t *load = _load;

  gc_forward_fn forwarder = load_forward32;

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

  from_offset = (ulong)load - ntohl(*(uint32_t *)load);

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
  /* make sure argument is aligned */
  assert((uintptr_t)_load % sizeof (ulong) == 0);

  uint8_t *load = _load;

  if (sizeof (ulong) > sizeof (uint32_t))
    {
      /* check for 32-bit format */
      struct obj32 *gone32 = (struct obj32 *)(load + sizeof (uint32_t));
      uint32_t gsize32 = ntohl(gone32->size);
      if (gsize32 == sizeof *gone32
	  || gsize32 == sizeof *gone32 + sizeof (uint32_t))
	return gc_load32(_load, size, version);
    }

  gc_forward_fn forwarder = load_forward;

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

  from_offset = (ulong)load - ntohlong((ulong)*(uint8_t **)load);

  gc_reserve(size - sizeof (uint8_t *) - sizeof (value));

  return _gc_load(forwarder, load, old, size, version < MDATA_VER_NEW_HASH,
                  version < MDATA_VER_RO_SYM_NAMES);
}

/* Machine specific portion of allocator */
/* ------------------------------------- */

#if (defined __i386__ || defined __x86_64__) && !defined NOCOMPILER
static void flush_icache(uint8_t *from, uint8_t *to)
{
  VALGRIND_DISCARD_TRANSLATIONS((ulong)from, (ulong)(to - from));
}

struct mcode *find_pc_mcode(ulong pc, ulong range_start, ulong range_end)
{
  uint8_t *magic = (uint8_t *)((pc & ~(CODE_ALIGNMENT - 1))
                           - sizeoffield(struct mcode, magic));

  if ((ulong)magic < range_start || pc > range_end)
    return NULL;

  /* We have found a collectable code object.
     First find where it begins by locating its magic sequence */
  while (((uint32_t *)magic)[0] != 0xffffffff
         || ((uint32_t *)magic)[1] != 0xffffffff)
    magic -= CODE_ALIGNMENT;

  struct mcode *mcode = (struct mcode *)(
    magic - offsetof(struct mcode, magic));
  assert((ulong)mcode >= range_start);

  /* pc may be after the last instruction byte as that may be a
     (noreturn) call to berror_xxx */
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
#define FWD(field) if (pointerp(cc->field)) special_forward(&cc->field)
 #define __FWD_CALLEE(n, reg) FWD(callee.reg)
 #define __FWD_CALLER(n, reg) FWD(caller.reg)
  FOR_CALLEE_SAVE(__FWD_CALLEE, SEP_SEMI);
  FOR_CALLER_SAVE(__FWD_CALLER, SEP_SEMI);
 #undef __FWD_CALLER
 #undef __FWD_CALLEE
#undef FWD
}

#ifdef __i386__
static ulong *skip_prim_args(ulong *sp)
{
  /* move sp past arguments to a call to any primitive */
  const uint8_t *return_pc = (const uint8_t *)sp[-1];
  static const uint8_t closure_dispatch[] = {
    /* add $offsetof(icode, magic_dispatch),%ecx */
    0x83, 0xc1, offsetof(struct icode, magic_dispatch),
    /* call %*ecx */
    0xff, 0xd1
  };
  if (memcmp(closure_dispatch, return_pc - sizeof closure_dispatch,
             sizeof closure_dispatch) == 0)
    {
      /* Calling known closures. This can be the first frame when
         calling interpreted closures; i.e., interpreter_invoke() */
      return sp;
    }

  assert(return_pc[-5] == 0xe8);
  /* we might be garbage-collecting, so we have to find which address
     the call preceding return_pc should be computed relatively to */
  struct mcode *base = find_pc_mcode((ulong)return_pc, (ulong)gcrange_start,
                                     (ulong)gcrange_end);
  ulong gcofs = base == NULL ? 0 : (ulong)base->myself - (ulong)base;

  ulong primadr = (ulong)return_pc + *(ulong *)(return_pc - 4) + gcofs;
  if (primadr >= (ulong)&builtin_start && primadr < (ulong)&builtin_end)
    return sp;

  assert(lookup_primitive(primadr) != NULL);

  /* check for (optional) add $N,%esp */
  if (return_pc[0] == 0x83 && return_pc[1] == 0xc4)
    {
      ulong primstacksize = return_pc[2];
      assert(primstacksize % sizeof (ulong) == 0);
      return (ulong *)((ulong)sp + primstacksize);
    }

  return sp;
}
#endif

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
      ulong *return_pc = &sp[-1];

#ifdef __i386__
      /* First frame is special: it may contain C arguments that might
	 be overwritten; make sure to skip them */
      sp = skip_prim_args(sp);
#endif

      forward_pc(return_pc);

      forward_pc(bp + 1);
      /* forward mudlle values & caller's closure) */
      for (ulong *x = sp; x < bp; x++)
	if (pointerp(*x)) special_forward((value *)x);

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

static uint32_t get_cst_ofs32(void **srcp)
{
  return *((*(uint32_t **)srcp)++);
}

static uint32_t get_cst_ofs16(void **srcp)
{
  return *((*(uint16_t **)srcp)++);
}

static void *add_cst_ofs32(void *dst, uint32_t ofs)
{
  *(uint32_t *)dst = ofs;
  return (uint32_t *)dst + 1;
}

static void *add_cst_ofs16(void *dst, uint32_t ofs)
{
  *(uint16_t *)dst = ofs;
  return (uint16_t *)dst + 1;
}

void mcode_fields(struct mcode_fields *f, const struct mcode *mcode)
{
#ifdef __i386__
  const uint32_t nb_pc_rel = 0;
  const uint32_t nb_rel = mcode->nb_rel;
#else
  const uint32_t nb_pc_rel = mcode->nb_pc_rel;
  const uint32_t nb_rel = 0;
#endif
  mcode_fields_spec(f, (char *)mcode->mcode, mcode->code_length,
                    nb_pc_rel, nb_rel, mcode->nb_constants);
}

void mcode_fields_spec(struct mcode_fields *f, char *mcode,
                       ulong code_len, uint32_t nb_pc_rel,
                       uint32_t nb_rel, uint32_t nb_constants)
{
  *f = (struct mcode_fields){ 0 };

#ifdef __i386__
  const unsigned npcrel = 0;
#elif defined __x86_64__
  const unsigned npcrel = nb_pc_rel;
#endif
  ulong tot_clen = code_len;
  if (npcrel)
    {
      tot_clen = MUDLLE_ALIGN(tot_clen, sizeof (value));
      f->code_pad = tot_clen - code_len;
      tot_clen += npcrel * sizeof (value);
    }

  uint32_t nofs = nb_rel + nb_constants;
  if (nofs)
    {
      unsigned cstsize;
      if (tot_clen > UINT16_MAX)
        {
          cstsize = sizeof (uint32_t);
          f->get_cst_ofs = get_cst_ofs32;
          f->add_cst_ofs = add_cst_ofs32;
        }
      else
        {
          cstsize = sizeof (uint16_t);
          f->get_cst_ofs = get_cst_ofs16;
          f->add_cst_ofs = add_cst_ofs16;
        }
      tot_clen = MUDLLE_ALIGN(tot_clen, cstsize);
      if (npcrel == 0)
        f->code_pad = tot_clen - code_len;
      f->cst_offsets = mcode + tot_clen;
      tot_clen += nofs * cstsize;
    }
  f->code_size = tot_clen;
}

static void scan_mcode(struct mcode *code)
{
  special_forward_code(&code->code);

  struct mcode_fields f;
  mcode_fields(&f, code);

  for (int i = code->nb_constants; i > 0; --i)
    {
      uint32_t ofs = f.get_cst_ofs(&f.cst_offsets);
      value *c = (value *)(code->mcode + ofs);
      if (pointerp(*c))
	special_forward(c);
    }

#ifdef __i386__
  /* Relocate calls to builtins */
  uint8_t *old_base = (uint8_t *)code->myself;
  code->myself = (struct mcode *)((uint8_t *)code + major_offset);
  ulong delta = old_base - (uint8_t *)code->myself;
  for (int i = code->nb_rel; i > 0; --i)
    {
      uint32_t ofs = f.get_cst_ofs(&f.cst_offsets);
      *(ulong *)(code->mcode + ofs) += delta;
    }
#endif
}

static void save_restore_mcode(struct mcode *code)
{
  save_restore_code(&code->code);

  struct mcode_fields f;
  mcode_fields(&f, code);
  for (int i = code->nb_constants; i > 0; --i)
    {
      uint32_t ofs = f.get_cst_ofs(&f.cst_offsets);
      save_restore(*(value *)(code->mcode + ofs));
    }
}

static void patch_value(value *x, value oldglobals, value newglobals)
{
  if (*x == oldglobals)
    *x = newglobals;
}

void patch_globals_stack(value oldglobals, value newglobals)
{
#ifdef __i386__
 #define REG_GLOBALS esi
#elif defined __x86_64__
 #define REG_GLOBALS rbx
#endif

  for (struct ccontext *cc = &ccontext;
       cc->frame_start;
       cc = next_ccontext(cc))
    {
      patch_value(&cc->callee.REG_GLOBALS, oldglobals, newglobals);

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
    patch_value(&mcc->old_ccontext.callee.REG_GLOBALS, oldglobals, newglobals);
}

#endif  /* (__i386__ || __x86_64__) && ! NOCOMPILER */

#ifdef NOCOMPILER
struct mcode *find_pc_mcode(ulong pc, ulong range_start, ulong range_end)
{
  return NULL;
}

static void flush_icache(uint8_t *from, uint8_t *to)
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

static struct ary func_ary;

static void func_forward(void *_ptr)
{
  struct obj **ptr = _ptr;
  struct obj *obj = *ptr;

  if ((obj->flags & OBJ_FLAG_0)
      || obj->garbage_type == garbage_static_string)
    return;

  if (is_typeset(obj, TYPESET_PRIMITIVE | TSET(code) | TSET(mcode)))
    ary_add(&func_ary, obj);

  obj->flags |= OBJ_FLAG_0;
  scan((void *)obj);
}

static void func_unforward(void *_ptr)
{
  struct obj **ptr = _ptr;
  struct obj *obj = *ptr;

  if (!(obj->flags & OBJ_FLAG_0))
    return;

  obj->flags &= ~OBJ_FLAG_0;
  scan((void *)obj);
}

/* return a vector with all reachable mudlle code */
struct vector *all_mudlle_code(void)
{
  struct vector *res = NULL;

  for (;;)
    {
#ifdef GCQDEBUG
      allowed_obj_flags |= OBJ_FLAG_0;
#endif
      special_forward = func_forward;
      forward_roots();
      special_forward = func_unforward;
      forward_roots();
#ifdef GCQDEBUG
      allowed_obj_flags &= ~OBJ_FLAG_0;
#endif

      ulong gcgen = gcstats.minor_count + gcstats.major_count;
      if (res == NULL)
        res = alloc_vector(ary_entries(&func_ary));
      else
        assert(vector_len(res) == ary_entries(&func_ary));
      if (gcgen == gcstats.minor_count + gcstats.major_count)
        break;
      /* if there was a GC while allocating res, func_ary() contains
         old pointers and we need to recreate it */

      ary_empty(&func_ary);
    }

  memcpy(res->data, func_ary.data, sizeof (value) * ary_entries(&func_ary));

  ary_free(&func_ary);

  return res;
}

/* make 'v' immutable (and readonly if necessary); abort() on failure */
value make_immutable(value v)
{
  if (immutablep(v))
    return v;
  v = make_readonly(v);
  if (!try_make_immutable(v))
    abort();
  return v;
}
