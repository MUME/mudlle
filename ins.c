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

#include "mudlle.h"
#include "ins.h"
#include "code.h"
#include "alloc.h"
#include "runtime/runtime.h"
#include "builtins.h"
#include <string.h>
#include <stddef.h>

/* Instruction lists are stored in reverse order, to simplify creation.
   They are reversed before use ...
*/

typedef struct _ilist		/* Instruction list */
{
  struct _ilist *next;
  instruction ins;
  label lab;			/* The main label for this instruction.
				   All other labels are aliases of this one. */
  label to;			/* Destination of branches */
  ulong offset;			/* Offset from end of code ... */
} *ilist;

typedef struct _blocks
{
  struct _blocks *next;
  const char *name;
  label exitlab;		/* Label for block exit */
  word stack_depth;		/* Stack depth at block entry */
} *blocks;

struct _fncode
{
  ilist instructions;
  word current_depth, max_depth; /* This tracks the stack depth as
				    determined by the instructions */
  label next_label;		/* For the 'label' function */
  struct gcpro_list cstpro;	/* Protect csts list */
  valuelist csts;		/* Constants of this function */
  uword cstindex;		/* Index of next constant */
  blocks blks;			/* Stack of named blocks */
  int toplevel;
  block_t memory;
};

struct _label			/* A pointer to an instruction */
{
  ilist ins;			/* The instruction this label points to */
  label alias;			/* This label is actually an alias for
				   another label ... */
};

int bc_length; /* For statistical purposes */

static void add_ins(instruction ins, fncode fn)
{
  ilist newp = allocate(fnmemory(fn), sizeof *newp);

  newp->next = fn->instructions;
  fn->instructions = newp;

  newp->ins = ins;
  newp->to = NULL;
  newp->lab = fn->next_label;
  if (fn->next_label) fn->next_label->ins = newp;
  fn->next_label = NULL;
}

void adjust_depth(int by, fncode fn)
/* Effects: Adjusts the current static stack depth of fn by the given
     amount. This is necessary for structures such as 'if' (which have
     code to compute 2 values, but which leave one on the stack).
   Modifies: fn
*/
{
  fn->current_depth += by;
  if (fn->current_depth > fn->max_depth) fn->max_depth = fn->current_depth;
}

fncode new_fncode(int toplevel)
/* Returns: A new function code structure (in which code for functions
     may be generated).
*/
{
  block_t afnmemory = new_block();
  fncode newp = allocate(afnmemory, sizeof *newp);

  newp->toplevel = toplevel;
  newp->memory = afnmemory;
  newp->instructions = NULL;
  newp->current_depth = newp->max_depth = 0;
  newp->next_label = NULL;
  newp->blks = NULL;
  PUSH_LIST(newp->cstpro);
  newp->cstpro.cl = &newp->csts;
  init_list(&newp->csts);
  newp->cstindex = 0;

  return newp;
}

void delete_fncode(fncode fn)
/* Effects: deletes fncode 'fn'
 */
{
  POP_LIST(fn->cstpro);
  free_block(fn->memory);
}

block_t fnmemory(fncode fn)
/* Returns: memory block for fn
 */
{
  return fn->memory;
}

int fntoplevel(fncode fn)
/* Returns: true if 'fn' is the toplevel function
 */
{
  return fn->toplevel;
}

uword add_constant(value cst, fncode fn)
/* Effects: Adds a constant to code of 'fn'.
   Returns: The index where this constant is stored.
*/
{
  addtail(fn->memory, &fn->csts, cst);

  return fn->cstindex++;
}

void ins_constant(value cst, fncode fn)
/* Effects: Adds code to push cst onto the stack in 'fn'
   Modifies: fn
*/
{
  uword aindex;

  if (integerp(cst))
    {
      long i = intval(cst);

      if (i >= INTEGER1_MIN && i <= INTEGER1_MAX)
	{
	  ins1(op_integer1, i, fn);
	  return;
	}
      if (i >= INTEGER2_MIN && i <= INTEGER2_MAX)
	{
	  ins2(op_integer2, i, fn);
	  return;
	}
    }

  aindex = add_constant(cst, fn);
  if (aindex < ARG1_MAX) ins1(op_constant1, aindex, fn);
  else ins2(op_constant2, aindex, fn);
}

void ins0(instruction ins, fncode fn)
/* Effects: Adds instruction ins to code of 'fn'.
   Modifies: fn
*/
{
  switch (ins)
    {
    case op_discard: case op_builtin_eq: case op_builtin_neq:
    case op_builtin_le: case op_builtin_lt: case op_builtin_ge:
    case op_builtin_gt: case op_builtin_ref: case op_builtin_add:
    case op_builtin_sub: case op_builtin_bitand: case op_builtin_bitor:
      fn->current_depth--;
      break;
    case op_builtin_set:
      fn->current_depth -= 2;
      break;
    case op_dup:
      fn->current_depth++;
      break;
    }
  if (fn->current_depth > fn->max_depth) fn->max_depth = fn->current_depth;
  add_ins(ins, fn);
}

void ins1(instruction ins, int arg1, fncode fn)
/* Effects: Adds instruction ins to code of 'fn'.
     The instruction has one argument, arg1.
   Modifies: fn
*/
{
  switch (ins)
    {
      /* Note: op_exit_n *MUST NOT* modify stack depth */
    case op_recall + local_var: case op_recall + closure_var: case op_integer1:
    case op_constant1: case op_closure:
      fn->current_depth++;
      if (fn->current_depth > fn->max_depth) fn->max_depth = fn->current_depth;
      break;
    case op_execute: case op_pop_n: case op_execute_primitive:
    case op_execute_secure: case op_execute_varargs:
      fn->current_depth -= arg1;
      break;
    }
  add_ins(ins, fn);
  add_ins(arg1, fn);
}

void ins2(instruction ins, int arg2, fncode fn)
/* Effects: Adds instruction ins to code of 'fn'.
     The instruction has a two byte argument (arg2), stored in big-endian
     format.
   Modifies: fn
*/
{
  if (ins == op_recall + global_var || ins == op_integer2 || ins == op_constant2)
    {
      fn->current_depth++;
      if (fn->current_depth > fn->max_depth) fn->max_depth = fn->current_depth;
    }
  else if (ins == op_execute_global2 || ins == op_execute_primitive2)
    fn->current_depth--;
  add_ins(ins, fn);
  add_ins(arg2 >> 8, fn);
  add_ins(arg2 & 0xff, fn);
}

void branch(instruction abranch, label to, fncode fn)
/* Effects: Adds a branch instruction to lavel 'to' to instruction 
     list 'next'.
     A 1 byte offset is added at this stage.
   Requires: 'branch' be a 1 byte branch instruction.
   Modifies: fn
*/
{
  switch (abranch)
    {
    case op_branch1: break;
    case op_branch_nz1: case op_branch_z1: case op_loop1:
      fn->current_depth--;
      break;
    default: assert(0);
    }
  add_ins(abranch, fn);
  fn->instructions->to = to;
  add_ins(0, fn);		/* Reserve a 1 byte offset */
}

static void resolve_labels(fncode fn)
/* Effects: Removes all references in branches to labels that are aliases
     (replaces them with the 'real' label.
     Also removes unconditional branches to the next instruction.
   Modifies: fn
   Requires: The code only contain 1 byte branches.
*/
{
  ilist scan, prev1, prev2;

  prev1 = prev2 = NULL;
  for (scan = fn->instructions; scan; scan = scan->next)
    {
      if (scan->to)
	{
	  if (scan->to->alias) scan->to = scan->to->alias;
	  assert(scan->to->ins);

	  /* prev1 is the (reserved) offset, prev2 is the next instruction */
	  if (scan->ins == op_branch1 &&
	      scan->to->ins == prev2)
	    {
	      /* Remove branch to next instruction */
	      prev2->next = scan->next;
	      if (scan->lab) 
		/* If removed instruction had a label, make it point to prev2 */
		/* NOTE: This can lead to there being more than one unaliased
		   label pointing to a particular instruction !!! */
		scan->lab->ins = prev2;

	      /* Needed to handle consecutive branches to the next ins */
	      scan = prev2;
	      /* prev1 is junk here (deleted ins) */
	    }
	}

      prev2 = prev1;
      prev1 = scan;
    } 
}

static void number_instructions(fncode fn)
/* Effects: Numbers the instructions in fn (starting from the end)
   Modifies: fn
*/
{
  ulong offset;
  ilist scan;

  for (scan = fn->instructions, offset = 0; scan; scan = scan->next, offset++)
    scan->offset = offset;
}

static int resolve_offsets(fncode fn)
/* Effects: Resolves all branch offsets in fn. Increases the size of
     the branches if necessary.
   Returns: TRUE if all branches could be resolved without increasing
     the size of any branches
*/
{
  ilist scan, prev1, prev2;
  int ok = TRUE;

  prev1 = prev2 = NULL;

  for (scan = fn->instructions; scan; scan = scan->next)
    {
      if (scan->to)		/* This is a branch */
	{
	  long offset = scan->offset - scan->to->ins->offset;

	  if ((scan->ins - op_branch1) & 1)
	    {
	      /* Two byte branch */
	      assert(prev1); assert(prev2);
	      offset -= 3;

	      if (offset >= INTEGER2_MIN && offset <= INTEGER2_MAX)
		{
		  prev1->ins = offset >> 8;
		  prev2->ins = offset & 0xff;
		}
	      else
		{
		  /* Branch doesn't fit. TBD. */
		  assert(0);
		}
	    }
	  else
	    {
	      /* One byte */
	      assert(prev1);
	      offset -= 2;

	      if (offset >= INTEGER1_MIN && offset <= INTEGER1_MAX)
		prev1->ins = offset;
	      else
		{
		  /* Make a 2 byte branch */
		  ilist newp = allocate(fn->memory, sizeof *newp);

		  scan->ins++;	/* he he */
		  newp->next = scan;
		  newp->lab = newp->to = NULL;
		  prev1->next = newp;

		  ok = FALSE;
		}
	    }
	}

      prev2 = prev1;
      prev1 = scan;
    }
  return ok;
}

void peephole(fncode fn)
/* Effects: Does some peephole optimisation on instructions of 'fn'
     Currently this only includes branch size optimisation (1 vs 2 bytes)
     and removal of unconditional branches to the next instruction.
     Also resolves branches...
   Modifies: fn
   Requires: All labels be defined
*/
{
  resolve_labels(fn);

  do number_instructions(fn);
  while (!resolve_offsets(fn));
}

struct code *generate_fncode(fncode fn,
			     struct string *help,
			     struct string *varname,
			     struct string *afilename,
			     int alineno,
			     int seclev)
/* Returns: A code structure with the instructions and constants in 'fn'.
   Requires: generate_fncode may only be called on the result of the most
     recent call to new_fncode. That call is then deemed to never have
     occured :-) (this means that new_fncode/generate_fncode must be paired
     in reverse temporal order)
*/
{
  ulong sequence_length;
  ilist scanins;
  instruction *codeins;
  uword i;
  struct local_value *scancst;
  struct code *gencode;
  ulong size;
  struct gcpro gcpro1, gcpro2, gcpro3;

  /* Count # of instructions */
  sequence_length = 0;
  for (scanins = fn->instructions; scanins; scanins = scanins->next) sequence_length++;

  GCPRO2(help, varname); GCPRO(gcpro3, afilename);
  /* Warning: Portability */
  size = offsetof(struct code, constants) + fn->cstindex * sizeof(value) + 
    sequence_length * sizeof(instruction);
  bc_length += size;
  gencode = gc_allocate(size);
  UNGCPRO();

  gencode->o.size = size;
  gencode->o.garbage_type = garbage_code;
  gencode->o.type = type_code;
  gencode->o.flags = OBJ_IMMUTABLE; /* Code is immutable */
  gencode->nb_constants = fn->cstindex;
  gencode->nb_locals = 0; /* Initialised later */
  gencode->stkdepth = fn->max_depth;
  gencode->seclevel = seclev;
  gencode->help = help;
  gencode->lineno = alineno;
  gencode->filename = afilename;
  gencode->varname = varname;

  gencode->call_count = gencode->instruction_count = 0;

  /* Copy the sequence (which is reversed) */
  codeins = (instruction *)(gencode->constants + fn->cstindex) + sequence_length;
  for (scanins = fn->instructions; scanins; scanins = scanins->next)
    *--codeins = scanins->ins;

  /* Copy the constants */
  for (i = 0, scancst = fn->csts.first; i < fn->cstindex; i++, scancst = scancst->next)
    {
      gencode->constants[i] = scancst->lvalue;
      GCCHECK(scancst->lvalue);
    }

  /* Jump to interpreter to execute interpreted code - machine specific */

#ifdef AMIGA
  gencode->magic_dispatch[0] = 0x4e;
  gencode->magic_dispatch[1] = 0xf9;
  *(ulong *)(gencode->magic_dispatch + 2) = (ulong)interpreter_invoke;
#endif

#ifdef sparc

  /* Sequence is:
     sethi %hi(interpreter_invoke),%g2
     or %g2,%lo(interpreter_invoke),%g2
     jmpl %g2+0,%g0
     nop
  */

  {
    ulong *dispatch = (ulong *)gencode->magic_dispatch;
    
    dispatch[0] = 4 << 22 | 2 << 25 | (ulong)interpreter_invoke >> 10;
    dispatch[1] = 2 << 30 | 2 << 25 | 2 << 19 | 2 << 14 | 1 << 13 |
      (ulong)interpreter_invoke & (1 << 10) - 1;
    dispatch[2] = 2 << 30 | 56 << 19 | 2 << 14;
    dispatch[3] = 4 << 22;
  }
#endif

#ifdef i386
  /* jmp interpreter_invoke */
  gencode->magic_dispatch[0] = 0xea;
  *(ulong *)(gencode->magic_dispatch + 1) = (ulong)interpreter_invoke;
  /* segment */
  asm("mov %%cs,%0" : "=r" (*(short *)(gencode->magic_dispatch + 5)));
  
#endif

#ifdef GCSTATS
  gcstats.anb[type_code]++;
  gcstats.asizes[type_code] += size;
#endif

  return gencode;
}

label new_label(fncode fn)
/* Returns: A new label which points to nothing. Use label() to make it
     point at a particular instruction.
*/
{
  label newp = allocate(fn->memory, sizeof *newp);

  newp->ins = NULL;
  newp->alias = NULL;

  return newp;
}

void set_label(label lab, fncode fn)
/* Effects: lab will point at the next instruction generated with ins0, 
     ins1, ins2 or branch.
   Modifies: lab
*/
{
  if (fn->next_label) lab->alias = fn->next_label;
  else fn->next_label = lab;
}

void start_block(const char *name, fncode fn)
/* Effects: Starts a block called name (may be NULL), which can be
     exited with exit_block()
*/
{
  blocks newp = allocate(fn->memory, sizeof *newp);

  newp->next = fn->blks;
  newp->name = name;
  newp->exitlab = new_label(fn);
  newp->stack_depth = fn->current_depth;

  fn->blks = newp;
}

void end_block(fncode fn)
/* Effects: End of named block. Generate exit label
*/
{
  set_label(fn->blks->exitlab, fn);
  fn->blks = fn->blks->next;
}

int exit_block(const char *name, fncode fn)
/* Effects: Generates code to exit from specified named block
     (pop stack, jump to block exit label)
   Returns: FALSE if the named block doesn't exist
*/
{
  blocks find = fn->blks;
  int npop;

  for (;;)
    {
      if (!find) return FALSE;
      if (name == NULL)
	{
	  if (find->name == NULL) break;
	}
      else if (find->name != NULL && stricmp(name, find->name) == 0) break;
      find = find->next;
    }

  npop = fn->current_depth - find->stack_depth - 1;
  assert(npop >= 0);
  if (npop > 0) ins1(op_exit_n, npop, fn);
  branch(op_branch1, find->exitlab, fn);

  return TRUE;
}
