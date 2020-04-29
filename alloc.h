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

#ifndef ALLOC_H
#define ALLOC_H

#include "mudlle-config.h"

#include <stdlib.h>

#include "types.h"

#undef ALLOC_STATS

#ifdef ALLOC_STATS
struct vector *get_alloc_stats(void);
void record_allocation(enum mudlle_type type, long size);
#endif

void garbage_init(void);

/* The GC block */
extern uint8_t *gcblock;
extern ulong gcblocksize;

#define CODE_ALIGNMENT 16

extern uint8_t *posgen0;

#ifdef GCDEBUG
extern ulong majorgen, minorgen;
extern uint8_t *endgen1;

#ifdef GCDEBUG_CHECK
static inline void gccheck_debug(value x)
{
  if (!pointerp(x))
    return;
  struct obj *o = x;
  assert(o->generation == (o->garbage_type == garbage_static_string
                           ? 0
                           : (o->generation & 1) ? minorgen : majorgen));
}
#define GCCHECK2(x) gccheck_debug(x)
#endif /* GCDEBUG_CHECK */
#elif defined(GCQDEBUG)
void gccheck_qdebug(value x);
#define GCCHECK2(x) gccheck_qdebug(x)
#endif /* ! GCDEBUG && ! GCQDEBUG */

#ifndef GCCHECK2
#define GCCHECK2(x) ((void)0)
#endif
#define GCCHECK(x) (CASSERT_EXPR(IS_MUDLLE_TYPE(x)), GCCHECK2(x))

/* Provide temporary protection for some values */
extern struct gcpro *gcpro;	/* Local (C) variables which need protection */

struct gcpro {
  struct gcpro *next;
  value *obj;
};

/* GCPROV() / UNGCPROV(): protect one value in a specified struct gcpro */
#define GCPROV(gc, var) do {				\
  GCCHECK(var);						\
  (gc).next = gcpro;					\
  gcpro = &(gc);					\
  (gc).obj = (void *)&(var);				\
} while (0)
#define UNGCPROV(gc) ((void)(gcpro = (gc).next))

#define __GCPROSTART struct gcpro gcpros[] = {
#define __GCPRO_N(N, var) {				\
  .next = IF_ONE(N)(					\
    gcpro,						\
    gcpros IF_TWO(N)(, + DEC(DEC(N)))),			\
  .obj	= (GCCHECK(var), (void *)&(var))		\
}
#define __GCPROEND(n) };				\
  ((void)(gcpro = &gcpros[DEC(n)]))

#define __GCPRO1(v, ...) __GCPRO_N(1, v)
#define __GCPRO2(v, ...) __GCPRO1(__VA_ARGS__), __GCPRO_N(2, v)
#define __GCPRO3(v, ...) __GCPRO2(__VA_ARGS__), __GCPRO_N(3, v)
#define __GCPRO4(v, ...) __GCPRO3(__VA_ARGS__), __GCPRO_N(4, v)
#define __GCPRO5(v, ...) __GCPRO4(__VA_ARGS__), __GCPRO_N(5, v)
#define __GCPRO6(v, ...) __GCPRO5(__VA_ARGS__), __GCPRO_N(6, v)

#define ___GCPRO(N, ...) __GCPRO ## N(__VA_ARGS__)
#define __GCPRO(N, ...) ___GCPRO(N, __VA_ARGS__)

/* GCPRO(...), UNGCPRO(): protect 1-6 values in a default gcpros[] array */
#define _GCPRO(n, ...)				\
  __GCPROSTART					\
  __GCPRO(n, __VA_ARGS__)			\
  __GCPROEND(n)
#define GCPRO(...) _GCPRO(VA_NARGS(__VA_ARGS__), __VA_ARGS__)
#define UNGCPRO() ((void)(gcpro = gcpros[0].next))

/* Protection for dynamically allocated variables, that may be freed */
struct dynpro
{
  struct dynpro *prev, *next;
  value obj;
  const char *file;
  int lineno;
};

void internal_dynpro(struct dynpro *what, value obj, const char *file,
                     int lineno);
#define dynpro(what, obj)                                       \
  (CHECK_MUDLLE_TYPE(obj), internal_dynpro((what), (obj), __FILE__, __LINE__))
void undynpro(struct dynpro *what);
struct dynpro *internal_protect(value v, const char *file, int lineno);
#define protect(v) (CHECK_MUDLLE_TYPE(v),                       \
                    internal_protect(v, __FILE__,__LINE__))
value unprotect(struct dynpro *pro);

/* dynpro() unless staticp() */
void maybe_dynpro(struct dynpro *what, value obj);
void maybe_undynpro(struct dynpro *what);

/* Protection of global variables */
void internal_staticpro(void *pro, const char *desc, const char *file,
                        int line);
#define staticpro(ptr)                                  \
  (CHECK_MUDLLE_TYPE(*(ptr)),                           \
   internal_staticpro(ptr, #ptr, __FILE__, __LINE__))

struct vector *get_staticpro_data(void);
struct list *get_dynpro_data(void);

/* Values below are integer fractions (A/B) */

/* Threshold for major collection */
/* A major collection occurs if:

      used mem 0
   --------------- > THRESHOLD_MAJOR
   available mem 0

   after a minor collection
*/


#define THRESHOLD_MAJOR_A 4
#define THRESHOLD_MAJOR_B 5

/* Threshold for increasing block size */
/* The block size is increased after a major collection if:

      used mem 0
   --------------- > THRESHOLD_INCREASE
   available mem 0

*/

#define THRESHOLD_INCREASE_A 2
#define THRESHOLD_INCREASE_B 3

/* Block increase factor */
#define INCREASE_A 14
#define INCREASE_B 10

#define ASSERT_NOALLOC_START() uint8_t *__old_posgen0 = posgen0
#define ASSERT_NOALLOC()       assert(__old_posgen0 == posgen0)

struct grecord *allocate_record(enum mudlle_type type, ulong entries);

/* Do not call this function if you don't understand how the gc works !! */
struct grecord *unsafe_allocate_record(enum mudlle_type type, ulong entries);
#define UNSAFE_ALLOCATE_RECORD(type, entries) \
  ((struct type *)unsafe_allocate_record(type_ ## type, entries))

struct primitive *allocate_primitive(const struct prim_op *op);

struct gstring *allocate_string(enum mudlle_type type, ulong bytes);
struct obj *allocate_temp(enum mudlle_type type, size_t size);
struct vector *allocate_locals(ulong n);
/* Effect: Allocate a vector of local variables in an optimised fashion.
*/

value gc_allocate(long n);
/* Effects: Allocates n bytes and returns a pointer to the start of
     the allocated area.
     DOES ABSOLUTELY NO INITIALISATION. BEWARE!
     Do not use if you don't understand the gc ...
   Returns: Pointer to allocated area
*/

void gc_free(struct obj *o);

value make_immutable(value v);

bool try_make_immutable(struct obj *obj);
void detect_immutability(void);
/* Effects: Detects all values that can be made immutable.
     Has the same restrictions as the normal GC, ie won't handle
     recursive values.
   Note: not extremely efficient, to be called only occasionnally
*/

struct gc_size {
  ulong s_total, s_mutable, s_static;
};
bool gc_size(value x, struct gc_size *size);
/* Effects: Returns number of bytes accessible from x
     Sets mutable (if not NULL) to the # of mutable bytes in x
   Modifies: mutable
*/

void *gc_save(value x, unsigned long *size);
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
     Returns NULL if x is itself not saveable.
*/

value gc_load(void *_load, unsigned long size,
              enum mudlle_data_version version);

#ifdef GCSTATS
struct gcstats_gen {
  struct {
    ulong nb, rwsize, rosize;
  } types[last_type];
};
#define GCSTATS_GEN_NULL (struct gcstats_gen){ .types[0].nb = 0 }

struct gcstats_alloc {
  struct {
    ulong nb, size;
  } types[last_type];
};
#define GCSTATS_ALLOC_NULL (struct gcstats_alloc){ .types[0].nb = 0 }
#endif  /* GCSTATS */

struct gcstats
{
  ulong minor_count, major_count;
#ifdef GCSTATS
  ulong size, usage_minor, usage_major;
  struct gcstats_gen gen[2];
  struct gcstats_alloc l, a;    /* before GC; since GC */
#endif
};

extern struct gcstats gcstats;

#ifdef GCSTATS
static inline void gcstats_add_alloc(enum mudlle_type type, ulong size)
{
  gcstats.a.types[type].nb++;
  gcstats.a.types[type].size += size;
}
#endif

struct vector *all_mudlle_code(void);

void garbage_collect(long n);
/* Effects: Does a garbage collection, ensuring that n bytes will be
     available at its completion.
   Modifies: the world
*/

#if defined __i386__ || defined __x86_64__
void patch_globals_stack(value oldglobals, value newglobals);
#endif

struct mcode *find_pc_mcode(ulong pc, ulong range_start, ulong range_end);

long gc_reserve(long n); /* Make sure n bytes are available,
			    return x >= 0 if x bytes are available,
			    and x >= n, otherwise return
			    -N number of bytes available after gc. */

typedef void (*gc_forward_fn)(void *ptr);

#endif
