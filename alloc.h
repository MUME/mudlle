/* $Log: alloc.h,v $
 * Revision 1.31  1995/07/16  09:16:48  arda
 * Add GCSTATS option.
 * Misc bug fixes.
 *
 * Revision 1.30  1995/07/15  15:24:10  arda
 * Context cleanup.
 * Remove GCDEBUG.
 *
 * Revision 1.29  1995/01/22  15:11:36  arda
 * Linux patches.
 *
 * Revision 1.28  1994/10/09  06:41:43  arda
 * Libraries
 * Type inference
 * Many minor improvements
 *
 * Revision 1.27  1994/09/16  13:07:09  arda
 * Rename protect to catch.
 * New protect/unprotect functions (like dynpro/undynpro).
 *
 * Revision 1.26  1994/09/15  19:46:35  arda
 * Performance improvements:
 *   setjmp -> _setjmp (setjmp is horrendously slow)
 *   cold_protect
 * reset_limits split from reset_interpreter
 * fix division of negative numbers
 * Add ?\{n,r,t}
 * gc_size returns "mutable" size
 *
 * Revision 1.25  1994/09/06  07:50:28  arda
 * Constant support: detect_immutability, global_set!, string_{i}search.
 *
 * Revision 1.24  1994/09/04  09:54:55  arda
 * Proper delayed actions, including mudlle interface. Affect mobs too.
 *
 * Revision 1.23  1994/08/29  13:17:11  arda
 * Contagious immutability.
 * Global array of values instead of variables.
 * Direct recursion.
 *
 * Revision 1.22  1994/08/22  11:18:19  arda
 * Moved code allocation to ins.c
 * Changes for mudlle compiler in MUME.
 *
 * Revision 1.21  1994/08/16  19:15:43  arda
 * Mudlle compiler for sparc now fully functional (68k compiler now needs
 * updating for primitives).
 * Changes to allow Sparc trap's for runtime errors.
 * Also added flags to primitives for better calling sequences.
 *
 * Revision 1.18  1994/05/08  14:13:28  arda
 * Event review
 *
 * Revision 1.17  1994/04/12  20:11:45  arda
 * (MD) Alignments and fixes + unknown from others...
 *
 * Revision 1.16  1994/02/24  08:32:46  arda
 * Owl: New error messages.
 *
 * Revision 1.15  1994/02/12  17:24:38  arda
 * Owl: Better code generated.
 *
 * Revision 1.14  1994/02/03  19:21:27  arda
 * nothing special(2)
 *
 * Revision 1.13  1994/01/29  19:50:21  dgay
 * Owl: add file & line information to functions.
 *
 * Revision 1.12  1994/01/27  21:59:29  dgay
 * Owl: Improve the collector (yet again).
 *      Now has just one zone for generation 0 (one extra copy involved).
 *
 * Revision 1.11  1994/01/27  17:07:59  arda
 * Hmm.
 *
 * Revision 1.10  1994/01/07  13:09:27  arda
 * Owl: Spec countdown continues.
 *
 * Revision 1.9  1994/01/02  15:50:12  arda
 * bug fix
 *
 * Revision 1.8  1993/12/23  20:48:48  dgay
 * Owl: New alloc.c: semi-generational collector.
 *      Included Amiga makefile for convenience.
 *
 * Revision 1.7  1993/12/06  19:20:49  arda
 * divers CLI
 *
 * Revision 1.6  1993/11/27  11:28:57  arda
 * Owl: Major changes to affect.
 *      Save mudlle data with players & objects.
 *      Change skill format on disk.
 *      Other minor changes.
 *      Still needs full debugging.
 *
 * Revision 1.5  1993/07/21  20:36:32  un_mec
 * Owl: Added &&, ||, optimised if.
 *      Added branches to the intermediate language.
 *      Separated destiniation language generation into ins module
 *      (with some peephole optimisation)
 *      Standalone version of mudlle (mkf, runtime/mkf, mudlle.c) added to CVS
 *
 * Revision 1.4  1993/04/22  18:58:31  un_autre
 * (MD) & Owl. Bug fixes. /player fixes. EVER_WHINER flag. saving_spells adjusted.
 *
 * Revision 1.3  1993/03/29  09:23:36  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.4  1993/03/17  12:49:24  dgay
 * Fixed GC of help strings in code blocks.
 * Added security features.
 *
 * Revision 1.3  1993/03/14  16:13:49  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.2  1993/01/08  23:57:05  un_mec
 * Owl: Allow characters and objects to appear in mudlle.
 *
 * Revision 1.1  1992/12/27  21:40:54  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

#ifndef ALLOC_H
#define ALLOC_H
#include "mvalues.h"
#include "types.h"

void garbage_cleanup(void);
void garbage_init(void);

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

unsigned long gc_size(value x, unsigned long *mutable);
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
value gc_load_debug(void *_load, unsigned long size);
/* Effects: Reloads a value saved with gc_save. <load,size> delimits
     the zone of memory containing gc_save's results.
     See gc_save for details.
   Returns: The loaded value
*/

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

#ifdef sparc
extern ulong *frame_start, *frame_end;
#endif

#ifdef AMIGA
void push_registers(void);
void pop_registers(void);
extern struct vector *activation_stack;
extern int registers_valid;		/* TRUE if static area is being used */
#endif

#endif
