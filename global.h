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

#ifndef GLOBAL_H
#define GLOBAL_H

#include "module.h"
#include "objenv.h"

extern struct env *environment;
extern struct vector *env_values;
extern struct vector *mvars;
extern struct table *global;
extern struct vector *global_names;

ulong global_lookup(const char *name);
/* Returns: the index for global variable name in environment.
     If name doesn't exist yet, it is created with a variable
     whose value is NULL.
   Modifies: environment
*/

/* true if 'name' is a known global */
bool global_exists(const char *name, ulong *ofs);

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

#define GVAR(offset) (env_values->data[(offset)])
/* Returns: The global value at 'offset'
*/

#define GCONSTANT(offset) (!integerp(mvars->data[offset]))
/* Returns: a true value if global variable offset is not modifiable
     (i.e., is a 'define' of some module or system-write)
*/

#define GMUTABLE(offset) (mvars->data[offset] == makeint(var_system_mutable))

#define GNAME(offset) ((struct string *)global_names->data[offset])
/* Returns: the name of the global at 'offset'
*/

void check_global_read(ulong goffset);
void check_global_write(ulong goffset);
/* Errors out if the calling code is not allowed to access that global.
   NB: adjust x86builtins.S if you change this declaration.
*/

#endif
