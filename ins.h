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

#ifndef INS_H
#define INS_H

typedef struct _label *label;
typedef struct _fncode *fncode;

#include "code.h"

fncode new_fncode(int toplevel);
/* Returns: A new function code structure (in which code for functions
     may be generated and in which constants may be stored).
   Warning: calls to new_fncode/delete_fncode must be matched in a stack-like
     discipline (new_fncode calls PUSH_LIST, delete_fncode POP_LIST)
*/

void delete_fncode(fncode fn);
/* Effects: deletes fncode 'fn'
 */

block_t fnmemory(fncode fn);
/* Returns: memory block for fn
 */

int fntoplevel(fncode fn);
/* Returns: true if 'fn' is the toplevel function
 */

void ins0(instruction ins, fncode fn);
/* Effects: Adds instruction ins to code of 'fn'.
   Modifies: fn
*/

void ins1(instruction ins, int arg1, fncode fn);
/* Effects: Adds instruction ins to code of 'fn'.
     The instruction has one argument, arg1.
   Modifies: fn
*/

void ins2(instruction ins, int arg2, fncode fn);
/* Effects: Adds instruction ins to code of 'fn'.
     The instruction has a two byte argument (arg2), stored in big-endian
     format.
   Modifies: fn
*/

void branch(instruction branch, label to, fncode fn);
/* Effects: Adds a branch instruction to lavel 'to' to instruction 
     list 'next'.
     A 1 byte offset is added at this stage.
   Requires: 'branch' be a 1 byte branch instruction.
   Modifies: fn
*/

void adjust_depth(int by, fncode fn);
/* Effects: Adjusts the current static stack depth of fn by the given
     amount. This is necessary for structures such as 'if' (which have
     code to compute 2 values, but which leave one on the stack).
   Modifies: fn
*/

uword add_constant(value cst, fncode fn);
/* Effects: Adds a constant to code of 'fn'.
   Returns: The index where this constant is stored.
   Modifies: fn
*/

void ins_constant(value cst, fncode fn);
/* Effects: Adds code to push cst onto the stack in 'fn'
   Modifies: fn
*/

void peephole(fncode fn);
/* Effects: Does some peephole optimisation on instructions of 'fn'
     Currently this only includes branch size optimisation (1 vs 2 bytes)
     and removal of unconditional branches to the next instruction.
   Modifies: fn
   Returns: Optimised instruction list
*/

struct code *generate_fncode(fncode fn,
			     struct string *help,
			     struct string *varname,
			     struct string *afilename,
			     int alineno,
			     int seclev);
/* Returns: A code structure with the instructions and constants in 'fn'.
   Requires: generate_fncode may only be called on the result of the most
     recent call to new_fncode. That call is then deemed to never have
     occured :-) (this means that new_fncode/generate_fncode must be paired
     in reverse temporal order)
*/

label new_label(fncode fn);
/* Returns: A new label which points to nothing. Use label() to make it
     point at a particular instruction.
*/

void set_label(label lab, fncode fn);
/* Effects: lab will point at the next instruction generated with ins0, 
     ins1, ins2 or branch.
   Modifies: lab, fn
*/

void start_block(const char *name, fncode fn);
/* Effects: Starts a block called name (may be NULL), which can be
     exited with exit_block()
   Modifies: fn
*/

void end_block(fncode fn);
/* Effects: End of named block. Generate exit label
   Modifies: fn
*/

int exit_block(const char *name, fncode fn);
/* Effects: Generates code to exit from specified named block
     (pop stack, jump to block exit label)
   Returns: FALSE if the named block doesn't exist
   Modifies: fn
*/

#endif
