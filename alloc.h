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

#ifndef ALLOC_H
#define ALLOC_H
#include "mvalues.h"
#include "types.h"

void garbage_cleanup(void);
void garbage_init(void);

/* The GC block */
extern ubyte *gcblock;
extern ulong gcblocksize;

#define ALIGN(x, n) (((x) + (n) - 1) & ~((n) - 1))
#define CODE_ALIGNMENT 16

#ifdef GCDEBUG
extern ulong majorgen, minorgen;
extern ubyte *endgen1;
#ifdef GCDEBUG_CHECK
#define GCCHECK(x) \
  if (pointerp(x) && \
      ((struct obj *)(x))->generation != \
      (((struct obj *)(x))->generation & 1 ? minorgen : majorgen)) assert(0); else
#else
#define GCCHECK(x) ;
#endif
#elif defined(GCQDEBUG)
extern ulong maxobjsize;
#define GCCHECK(x)							\
  do if (pointerp(x) &&							\
	 (((long)(x) & 2) ||						\
	  (((struct obj *)(x))->size > maxobjsize && 			\
	   ((struct obj *)(x))->garbage_type != garbage_forwarded) ||	\
	  ((struct obj *)(x))->size < 8 ||				\
	  ((struct obj *)(x))->garbage_type > garbage_mcode ||		\
	  ((struct obj *)(x))->type >= last_type ||			\
	  ((struct obj *)(x))->flags & ~3))				\
     assert(0); while (0)
#else
#define GCCHECK(x) ;
#endif

/* Provide temporary protection for some values */
extern struct gcpro *gcpro;	/* Local (C) variables which need protection */

struct gcpro 
{
    struct gcpro *next;
    value *obj;
};

#define GCPRO1(var) do { GCCHECK(var); gcpro1.next = gcpro; gcpro = &gcpro1; \
		         gcpro1.obj = (value *)&var; } while(0)

#define GCPRO2(var1, var2) do { GCCHECK(var1); GCCHECK(var2); gcpro1.next = gcpro; gcpro1.obj = (value *)&var1; \
			        gcpro2.next = &gcpro1; gcpro2.obj = (value *)&var2; \
				gcpro = &gcpro2; } while(0)

#define GCPRO(gc, var) do { GCCHECK(var); gc.next = gcpro; gcpro = &gc; \
			    gc.obj = (value *)&var; } while(0)

#define UNGCPRO() (gcpro = gcpro1.next)
#define UNGCPRO1(gc) (gcpro = gc.next)

#include "valuelist.h"

extern struct gcpro_list *gcpro_list; /* List of values which need protection */

struct gcpro_list 
{
    struct gcpro_list *next;
    valuelist *cl;
};

#define PUSH_LIST(var) do { var.next = gcpro_list; \
			    gcpro_list = &var; } while(0)

#define POP_LIST(var) (gcpro_list = var.next)

#define GCPRO_LIST1(var) do { PUSH_LIST(gcpro_list1); \
			      gcpro_list1.cl = &var; } while(0)

#define UNGCPRO_LIST() POP_LIST(gcpro_list1)

/* Protection for dynamically allocated variables, that may be freed */
struct dynpro
{
  struct dynpro *prev, *next;
  value obj;
};

void dynpro(struct dynpro *what, value obj);
void undynpro(struct dynpro *what);
struct dynpro *protect(value v);
value unprotect(struct dynpro *pro);

/* Protection of global variables */
void staticpro(value *pro);


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

struct grecord *allocate_record(ubyte type, ulong entries);

/* Do not call this function if you don't understand how the gc works !! */
struct grecord *unsafe_allocate_record(ubyte type, ulong entries);

struct gstring *allocate_string(ubyte type, ulong bytes);
struct gpermanent *allocate_permanent(ubyte type, ulong nb, void *ext);
struct gtemp *allocate_temp(ubyte type, void *ext);
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

void detect_immutability(void);
/* Effects: Detects all values that can be made immutable.
     Has the same restrictions as the normal GC, ie won't handle
     recursive values.
   Note: not extremely efficient, to be called only occasionnally
*/

unsigned long gc_size(value x, unsigned long *mutble);
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

value gc_load(void *_load, unsigned long size);
#ifdef GCDEBUG
#define gc_load_debug gc_load
#else
value gc_load_debug(void *_load, unsigned long size);
/* Effects: Reloads a value saved with gc_save. <load,size> delimits
     the zone of memory containing gc_save's results.
     See gc_save for details.
   Returns: The loaded value
*/
#endif

#ifdef GCSTATS
struct gcstats
{
  ulong size, usage_minor, usage_major;
  ulong minor_count, major_count;
  ulong g0nb[last_type], g0sizes[last_type]; /* Data in generation 0 at last GC */
  ulong g1nb[last_type], g1sizes[last_type]; /* Data in generation 1 at last GC */
  ulong lnb[last_type], lsizes[last_type]; /* Amount allocated till GC */
  ulong anb[last_type], asizes[last_type]; /* Amount allocated since GC */
};

extern struct gcstats gcstats;
#endif

void dump_memory(void);
/* Effects: Dumps GC's memory to a file for use by the profiler.
*/

void garbage_collect(long n);
/* Effects: Does a garbage collection, ensuring that n bytes will be
     available at its completion.
   Modifies: the world
*/

#ifdef AMIGA
void push_registers(void);
void pop_registers(void);
extern struct vector *activation_stack;
extern int registers_valid;		/* TRUE if static area is being used */
#endif

#ifdef i386
void patch_globals_stack(value oldglobals, value newglobals);
#endif

#endif
