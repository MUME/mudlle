/* $Log: ins.c,v $
 * Revision 1.14  1994/10/09  06:42:11  arda
 * Libraries
 * Type inference
 * Many minor improvements
 *
 * Revision 1.13  1994/08/29  13:17:20  arda
 * Contagious immutability.
 * Global array of values instead of variables.
 * Direct recursion.
 *
 * Revision 1.12  1994/08/22  11:18:27  arda
 * Moved code allocation to ins.c
 * Changes for mudlle compiler in MUME.
 *
 * Revision 1.11  1994/08/16  19:15:57  arda
 * Mudlle compiler for sparc now fully functional (68k compiler now needs
 * updating for primitives).
 * Changes to allow Sparc trap's for runtime errors.
 * Also added flags to primitives for better calling sequences.
 *
 * Revision 1.8  1994/03/23  14:31:21  arda
 * *** empty log message ***
 *
 * Revision 1.7  1994/02/24  08:32:51  arda
 * Owl: New error messages.
 *
 * Revision 1.6  1994/02/12  17:24:50  arda
 * Owl: Better code generated.
 *
 * Revision 1.5  1994/01/29  19:50:25  dgay
 * Owl: add file & line information to functions.
 *
 * Revision 1.4  1993/12/23  20:48:51  dgay
 * Owl: New alloc.c: semi-generational collector.
 *      Included Amiga makefile for convenience.
 *
 * Revision 1.3  1993/11/27  11:29:00  arda
 * Owl: Major changes to affect.
 *      Save mudlle data with players & objects.
 *      Change skill format on disk.
 *      Other minor changes.
 *      Still needs full debugging.
 *
 * Revision 1.2  1993/10/03  14:07:13  dgay
 * Bumper disun8 update.
 *
 * Revision 1.1  1993/07/21  20:36:38  un_mec
 * Owl: Added &&, ||, optimised if.
 *      Added branches to the intermediate language.
 *      Separated destiniation language generation into ins module
 *      (with some peephole optimisation)
 *      Standalone version of mudlle (mkf, runtime/mkf, mudlle.c) added to CVS
 *
 */

static char rcsid[] = "$Id: ins.c,v 1.14 1994/10/09 06:42:11 arda Exp $";

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

typedef struct ilist		/* Instruction list */
{
  struct ilist *next;
  instruction ins;
  label lab;			/* The main label for this instruction.
				   All other labels are aliases of this one. */
  label to;			/* Destination of branches */
  ulong offset;			/* Offset from end of code ... */
} *ilist;

typedef struct blocks
{
  struct blocks *next;
  const char *name;
  label exitlab;		/* Label for block exit */
  word stack_depth;		/* Stack depth at block entry */
} *blocks;

struct fncode
{
  ilist instructions;
  word current_depth, max_depth; /* This tracks the stack depth as
				    determined by the instructions */
  label next_label;		/* For the 'label' function */
  struct gcpro_list cstpro;	/* Protect csts list */
  valuelist csts;		/* Constants of this function */
  uword cstindex;		/* Index of next constant */
  blocks blocks;		/* Stack of named blocks */
};

struct label			/* A pointer to an instruction */
{
  struct ilist *ins;		/* The instruction this label points to */
  label alias;			/* This label is actually an alias for
				   another label ... */
};

static void add_ins(instruction ins, fncode fn)
{
  ilist new = allocate(memory, sizeof *new);

  new->next = fn->instructions;
  fn->instructions = new;

  new->ins = ins;
  new->to = NULL;
  new->lab = fn->next_label;
  if (fn->next_label) fn->next_label->ins = new;
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

fncode new_fncode(void)
/* Returns: A new function code structure (in which code for functions
     may be generated).
*/
{
  fncode new = allocate(memory, sizeof *new);

  new->instructions = NULL;
  new->current_depth = new->max_depth = 0;
  new->next_label = NULL;
  new->blocks = NULL;
  PUSH_LIST(new->cstpro);
  new->cstpro.cl = &new->csts;
  init_list(&new->csts);
  new->cstindex = 0;

  return new;
}


uword add_constant(value cst, fncode fn)
/* Effects: Adds a constant to code of 'fn'.
   Returns: The index where this constant is stored.
*/
{
  addtail(&fn->csts, cst);

  return fn->cstindex++;
}

void ins_constant(value cst, fncode fn)
/* Effects: Adds code to push cst onto the stack in 'fn'
   Modifies: fn
*/
{
  uword index;

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

  index = add_constant(cst, fn);
  if (index < ARG1_MAX) ins1(op_constant1, index, fn);
  else ins2(op_constant2, index, fn);
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
    case op_execute: case op_pop_n:
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

void branch(instruction branch, label to, fncode fn)
/* Effects: Adds a branch instruction to lavel 'to' to instruction 
     list 'next'.
     A 1 byte offset is added at this stage.
   Requires: 'branch' be a 1 byte branch instruction.
   Modifies: fn
*/
{
  switch (branch)
    {
    case op_branch1: break;
    case op_branch_nz1: case op_branch_z1: case op_loop1:
      fn->current_depth--;
      break;
    default: assert(0);
    }
  add_ins(branch, fn);
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
		  ilist new = allocate(memory, sizeof *new);

		  scan->ins++;	/* he he */
		  new->next = scan;
		  new->lab = new->to = NULL;
		  prev1->next = new;

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

struct code *generate_fncode(fncode fn, struct string *help, struct string *varname,
			     struct string *filename, int lineno, int seclev)
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

  GCPRO2(help, varname); GCPRO(gcpro3, filename);
  /* Warning: Portability */
  size = offsetof(struct code, constants) + fn->cstindex * sizeof(value) + 
    sequence_length * sizeof(instruction);
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
  gencode->lineno = lineno;
  gencode->filename = filename;
  gencode->varname = varname;

  gencode->call_count = gencode->instruction_count = 0;

  /* Copy the sequence (which is reversed) */
  codeins = (instruction *)(gencode->constants + fn->cstindex) + sequence_length;
  for (scanins = fn->instructions; scanins; scanins = scanins->next)
    *--codeins = scanins->ins;

  /* Copy the constants */
  for (i = 0, scancst = fn->csts.first; i < fn->cstindex; i++, scancst = scancst->next)
    {
      gencode->constants[i] = scancst->value;
      GCCHECK(scancst->value);
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

#ifdef GCDEBUG
  gcstats.anb[type_code]++;
  gcstats.asizes[type_code] += size;
#endif

  POP_LIST(fn->cstpro);

  return gencode;
}

label new_label(void)
/* Returns: A new label which points to nothing. Use label() to make it
     point at a particular instruction.
*/
{
  label new = allocate(memory, sizeof *new);

  new->ins = NULL;
  new->alias = NULL;

  return new;
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
  blocks new = allocate(memory, sizeof *new);

  new->next = fn->blocks;
  new->name = name;
  new->exitlab = new_label();
  new->stack_depth = fn->current_depth;

  fn->blocks = new;
}

void end_block(fncode fn)
/* Effects: End of named block. Generate exit label
*/
{
  set_label(fn->blocks->exitlab, fn);
  fn->blocks = fn->blocks->next;
}

int exit_block(const char *name, fncode fn)
/* Effects: Generates code to exit from specified named block
     (pop stack, jump to block exit label)
   Returns: FALSE if the named block doesn't exist
*/
{
  blocks find = fn->blocks;
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
