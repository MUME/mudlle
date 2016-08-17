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

#ifndef INS_H
#define INS_H

#include "code.h"
#include "mudlle.h"
#include "types.h"

struct label;

union instruction {
#ifdef __GNUC__
  enum operator op : 8 __attribute__((packed));
#else
  ubyte op;
#endif
  ubyte u;
  sbyte s;
};
CASSERT(instruction, sizeof (union instruction) == 1);

struct fncode *new_fncode(bool toplevel);
/* Returns: A new function code structure (in which code for functions
     may be generated and in which constants may be stored).
   Warning: calls to new_fncode/delete_fncode must be matched in a stack-like
     discipline (new_fncode calls PUSH_LIST, delete_fncode POP_LIST)
*/

void delete_fncode(struct fncode *fn);
/* Effects: deletes fn
 */

struct alloc_block *fnmemory(struct fncode *fn);
/* Returns: memory block for fn
 */

bool fntoplevel(struct fncode *fn);
/* Returns: true if 'fn' is the toplevel function
 */

void ins0(enum operator op, struct fncode *fn);
/* Effects: Adds instruction ins to code of 'fn'.
   Modifies: fn
*/

void ins1(enum operator op, ubyte arg1, struct fncode *fn);
/* Effects: Adds instruction ins to code of 'fn'.
     The instruction has one argument, arg1.
   Modifies: fn
*/

void ins2(enum operator op, uword arg2, struct fncode *fn);
/* Effects: Adds instruction ins to code of 'fn'.
     The instruction has a two byte argument (arg2), stored in big-endian
     format.
   Modifies: fn
*/

void branch(enum operator branch, struct label *to, struct fncode *fn);
/* Effects: Adds a branch instruction to lavel 'to' to instruction
     list 'next'.
     A 1 byte offset is added at this stage.
   Requires: 'branch' be a 1 byte branch instruction.
   Modifies: fn
*/

void adjust_depth(int by, struct fncode *fn);
/* Effects: Adjusts the current static stack depth of fn by the given
     amount. This is necessary for structures such as 'if' (which have
     code to compute 2 values, but which leave one on the stack).
   Modifies: fn
*/

uword add_constant(value cst, struct fncode *fn);
/* Effects: Adds a constant to code of 'fn'.
   Returns: The index where this constant is stored.
   Modifies: fn
*/

void ins_constant(value cst, struct fncode *fn);
/* Effects: Adds code to push cst onto the stack in 'fn'
   Modifies: fn
*/

void peephole(struct fncode *fn);
/* Effects: Does some peephole optimisation on instructions of 'fn'
     Currently this only includes branch size optimisation (1 vs 2 bytes)
     and removal of unconditional branches to the next instruction.
   Modifies: fn
   Returns: Optimised instruction list
*/

struct icode *generate_fncode(struct fncode *fn,
                              struct string *help,
                              struct string *varname,
                              struct string *afilename,
                              struct string *anicename,
                              int alineno,
                              struct vector *arg_types,
                              unsigned return_typeset,
                              int seclev);
/* Returns: A code structure with the instructions and constants in 'fn'.
   Requires: generate_fncode may only be called on the result of the most
     recent call to new_fncode. That call is then deemed to never have
     occured :-) (this means that new_fncode/generate_fncode must be paired
     in reverse temporal order)
*/

struct label *new_label(struct fncode *fn);
/* Returns: A new label which points to nothing. Use label() to make it
     point at a particular instruction.
*/

void set_label(struct label *lab, struct fncode *fn);
/* Effects: lab will point at the next instruction generated with ins0,
     ins1, ins2 or branch.
   Modifies: lab, fn
*/

void start_block(const char *name, struct fncode *fn);
/* Effects: Starts a block called name (may be NULL), which can be
     exited with exit_block()
   Modifies: fn
*/

void end_block(struct fncode *fn);
/* Effects: End of named block. Generate exit label
   Modifies: fn
*/

int exit_block(const char *name, struct fncode *fn);
/* Effects: Generates code to exit from specified named block
     (pop stack, jump to block exit label)
   Returns: false if the named block doesn't exist
   Modifies: fn
*/

void set_lineno(int line, struct fncode *fn);
/* Effects: Sets line number for upcoming instructions
   Modifies: fn
*/

int get_code_line_number(struct icode *code, int offset);

#endif
