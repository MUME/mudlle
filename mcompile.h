/* $Log: mcompile.h,v $
 * Revision 1.1  1994/10/09  06:42:30  arda
 * Libraries
 * Type inference
 * Many minor improvements
 * */
#ifndef MCOMPILE_H
#define MCOMPILE_H

#include "tree.h"
#include "ins.h"

/* Compile module references */

int mstart(file f);
/* Effects: Start processing module f:
     - unload f
     - load required modules
     - change status of variables of f (defines, writes)
     - setup information for mrecall/massign/mexecute

     Sends error/warning messages.
   Returns: TRUE if compilation can proceed
*/

void mrecall(ulong n, const char *name, fncode fn);
/* Effects: Generate code to recall variable n
*/

void mexecute(ulong offset, const char *name, int count, fncode fn);
/* Effects: Generates code to call function in variable n, with count
     arguments. If name is NULL, assume that it is part of a protected
     imported module (used for builtins)
*/

void massign(ulong n, const char *name, int toplevel, fncode fn);
/* Effects: Generate code to assign to variable n
*/

void mcompile_init(void);

#endif
