/* $Log: global.h,v $
 * Revision 1.12  1995/01/22  15:11:37  arda
 * Linux patches.
 *
 * Revision 1.11  1994/10/09  06:42:07  arda
 * Libraries
 * Type inference
 * Many minor improvements
 *
 * Revision 1.10  1994/09/06  07:50:35  arda
 * Constant support: detect_immutability, global_set!, string_{i}search.
 *
 * Revision 1.9  1994/08/29  13:17:19  arda
 * Contagious immutability.
 * Global array of values instead of variables.
 * Direct recursion.
 *
 * Revision 1.8  1994/08/16  19:15:55  arda
 * Mudlle compiler for sparc now fully functional (68k compiler now needs
 * updating for primitives).
 * Changes to allow Sparc trap's for runtime errors.
 * Also added flags to primitives for better calling sequences.
 *
 * Revision 1.5  1994/02/12  17:24:47  arda
 * Owl: Better code generated.
 *
 * Revision 1.4  1993/04/22  18:58:38  un_autre
 * (MD) & Owl. Bug fixes. /player fixes. EVER_WHINER flag. saving_spells adjusted.
 *
 * Revision 1.3  1993/03/29  09:23:52  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.3  1993/03/14  16:14:16  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.1  1992/12/27  21:41:08  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

#ifndef GLOBAL_H
#define GLOBAL_H

#include "mvalues.h"
#include "objenv.h"

extern struct env *environment;
extern struct vector *mvars;
extern struct table *global;

ulong global_lookup(const char *name);
/* Returns: the index for global variable name in environment.
     If name doesn't exist yet, it is created with a variable
     whose value is NULL.
   Modifies: environment
*/

ulong mglobal_lookup(struct string *name);
/* Returns: the index for global variable name in environment.
     If name doesn't exist yet, it is created with a variable
     whose value is NULL.
   Modifies: environment
*/

void global_init(void);
/* Effects: Initialises the global environment before use.
*/

struct list *global_list(void);
/* Returns: List of symbols representing all the global variables.
     The value cell of each symbol contains the variables number
*/

#define GVAR(offset) (environment->values->data[(offset)])
/* Returns: The global value at 'offset'
*/

#define GCONSTANT(offset) (!integerp(mvars->data[(offset)]))
/* Returns: a true value if global variable offset is not modifiable
     (ie is a 'define' of some module)
*/

#endif
