/* $Log: env.h,v $
 * Revision 1.4  1994/10/09  06:42:00  arda
 * Libraries
 * Type inference
 * Many minor improvements
 *
 * Revision 1.3  1994/01/08  12:49:45  dgay
 * Owl: Improved code generation for blocks (they are not implemented
 * as 0 argument functions anymore, they are folded into the current
 * function instead).
 *
 * Revision 1.2  1993/03/29  09:23:48  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.3  1993/03/14  16:14:10  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.1  1992/12/27  21:41:05  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

#ifndef ENV_H
#define ENV_H

#include "tree.h"
#include "code.h"
#include "ins.h"

typedef struct varlist
{
  struct varlist *next;
  variable_class class;
  ulong offset;
} *varlist;

void env_reset(void);
/* Effects: Clears the environment stack
*/

void env_push(vlist locals);
/* Effects: Starts a new environment (for a new function), with local
     variables 'locals'.
*/

void env_block_push(vlist locals, fncode fn);
/* Effects: We have entered a local scope of the environment at the top
     of the stack. Add locals to the list of variables for this scope,
     and initialise them to null if necessary.
   Modifies: fn
*/

void env_block_pop(void);
/* Effects: Pop a local scope
*/

varlist env_pop(uword *nb_locals);
/* Effects: Pop an environement, returning the variables that it needs
     it it's closure as well as the number of local variables it uses.
*/

variable_class env_lookup(const char *name, ulong *offset);
/* Effects: Returns the class & offset of variable name for the current
     environment. Modifies the closures appropriately.
*/

#endif
