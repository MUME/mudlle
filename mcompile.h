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

#ifndef MCOMPILE_H
#define MCOMPILE_H

#include "tree.h"
#include "ins.h"

/* Compile module references */

int mstart(block_t heap, mfile f, int seclev);
/* Effects: Start processing module f:
     - unload f
     - load required modules
     - change status of variables of f (defines, writes)
     - setup information for mrecall/massign/mexecute

     Sends error/warning messages.
   Returns: true if compilation can proceed
*/

void mstop(mfile f);
/* Effects: Stop processing module f */

void mrecall(ulong n, const char *name, fncode fn);
/* Effects: Generate code to recall variable n
*/

void mexecute(ulong offset, const char *name, int count, fncode fn);
/* Effects: Generates code to call function in variable n, with count
     arguments. If name is NULL, assume that it is part of a protected
     imported module (used for builtins)
*/

bool mwritable(ulong n, const char *name);
/* Effects: Return true if variable n/name may be written to. Otherwise,
   log error message.*/

void massign(ulong n, const char *name, fncode fn);
/* Effects: Generate code to assign to variable n
*/

void mwarn_module(int seclev, block b);
/* Effects: Warns about unused variables in module name
 */

void mcompile_init(void);

#endif
