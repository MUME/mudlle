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

#ifndef MODULE_H
#define MODULE_H

#include "types.h"

#define MAX_MODULE_NAME_LENGHT 256

enum module_status {
  module_unloaded, module_error, module_loading, module_loaded,
  module_protected
};

extern struct table *module_data;

enum module_status module_status(const char *name);
/* Returns: Status of module name:
     module_unloaded: module has never been loaded, or has been unloaded
     module_error: attempt to load module led to error
     module_loaded: module loaded successfully
     module_protected: module loaded & protected
*/

void module_set(const char *name, enum module_status status, int seclev);
/* Requires: status != module_unloaded
   Effects: Sets module status after load attempt
*/

bool module_unload(const char *name);
/* Effects: Removes all knowledge about module 'name' (eg prior to
     reloading it)
     module_status(name) will return module_unloaded if this operation is
     successful
     Sets to null all variables that belonged to name, and resets their status
     to var_normal
   Returns: false if name was protected
*/

enum module_status module_load(const char *name);
/* Effects: Attempts to load module name by calling mudlle hook
     Error/warning messages are sent to muderr
     Sets erred to true in case of error
     Updates module status
   Modifies: erred
   Requires: module_status(name) == module_unloaded
   Returns: New module status
*/

enum module_status module_require(const char *name);
/* Effects: Does module_load(name) if module_status(name) == module_unloaded
     Other effects as in module_load
*/

enum vstatus {
  var_normal,                   /* default */
  var_module,                   /* defined by a module */
  var_write,                    /* written by mudlle */
  var_system_write,             /* C may write, mudlle read */
  var_system_mutable            /* anyone may read/write */
 };
enum vstatus module_vstatus(long n, struct string **name);
/* Returns: status of global variable n:
     var_normal: normal global variable, no writes
     var_write: global variable which is written
     var_module: defined symbol of a module
       module name is stored in *name
     var_system_write: may be written but not directly by mudlle
   Modifies: name
   Requires: n be a valid global variable offset
*/

bool module_vset(long n, enum vstatus status, struct string *name);
/* Effects: Sets status of global variable n to status.
     name is the module name for status var_module
   Returns: true if successful, false if the change is impossible
     (i.e., status was already var_module or var_system_write)
*/

int module_seclevel(const char *name);

void module_init(void);
/* Initialise this module */

#endif
