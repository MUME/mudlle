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

#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "alloc.h"
#include "builtins.h"
#include "calloc.h"
#include "code.h"
#include "ins.h"
#include "lexer.h"
#include "mvalues.h"
#include "utils.h"

/* Instruction lists are stored in reverse order, to simplify creation.
   They are reversed before use ...
*/

struct ilist			/* Instruction list */
{
  struct ilist *next;
  union instruction ins;
  struct label *lab;	       /* The main label for this instruction.
				  All other labels are aliases of this one. */
  struct label *to;		/* Destination of branches */
  ulong offset;			/* Offset from end of code ... */
  int lineno;
};

struct blocks
{
  struct blocks *next;
  const char *name;
  struct label *exitlab;		/* Label for block exit */
  word stack_depth;		/* Stack depth at block entry */
};

struct fncode {
  struct ilist *instructions;
  word current_depth, max_depth; /* This tracks the stack depth as
				    determined by the instructions */
  struct label *next_label;	/* For the 'label' function */
  struct dynpro csts;           /* Mudlle list of constants */
  uword cstindex;		/* Index of next constant */
  struct blocks *blks;		/* Stack of named blocks */
  bool toplevel;
  struct alloc_block *memory;
  int lineno;
};

struct label			/* A pointer to an instruction */
{
  struct ilist *ins;		/* The instruction this label points to */
  struct label *alias;		/* This label is actually an alias for
				   another label ... */
};

static int bc_length; /* For statistical purposes */

static void add_ins(ubyte ins, struct fncode *fn)
{
  struct ilist *newp = allocate(fnmemory(fn), sizeof *newp);
  *newp = (struct ilist){
    .next   = fn->instructions,
    .ins    = (union instruction)ins,
    .lab    = fn->next_label,
    .lineno = fn->lineno
  };
  fn->instructions = newp;
  if (fn->next_label)
    {
      fn->next_label->ins = newp;
      fn->next_label = NULL;
    }
}

void set_lineno(int line, struct fncode *fn)
{
  if (line)
    yylineno = fn->lineno = line;
}

void adjust_depth(int by, struct fncode *fn)
/* Effects: Adjusts the current static stack depth of fn by the given
     amount. This is necessary for structures such as 'if' (which have
     code to compute 2 values, but which leave one on the stack).
   Modifies: fn
*/
{
  fn->current_depth += by;
  if (fn->current_depth > fn->max_depth) fn->max_depth = fn->current_depth;
}

struct fncode *new_fncode(bool toplevel)
/* Returns: A new function code structure (in which code for functions
     may be generated).
*/
{
  struct alloc_block *afnmemory = new_block();
  struct fncode *newp = allocate(afnmemory, sizeof *newp);
  *newp = (struct fncode){
    .toplevel = toplevel,
    .memory   = afnmemory
  };
  dynpro(&newp->csts, NULL);

  return newp;
}

void delete_fncode(struct fncode *fn)
/* Effects: deletes struct fncode *'fn'
 */
{
  undynpro(&fn->csts);
  free_block(fn->memory);
}

struct alloc_block *fnmemory(struct fncode *fn)
/* Returns: memory block for fn
 */
{
  return fn->memory;
}

bool fntoplevel(struct fncode *fn)
/* Returns: true if 'fn' is the toplevel function
 */
{
  return fn->toplevel;
}

uword add_constant(value cst, struct fncode *fn)
/* Effects: Adds a constant to code of 'fn'.
   Returns: The index where this constant is stored.
*/
{
  fn->csts.obj = alloc_list(cst, fn->csts.obj);
  return fn->cstindex++;
}

void ins_constant(value cst, struct fncode *fn)
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

void ins0(enum operator op, struct fncode *fn)
/* Effects: Adds instruction ins to code of 'fn'.
   Modifies: fn
*/
{
  switch (op)
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
    default:
      break;
    }
  if (fn->current_depth > fn->max_depth)
    fn->max_depth = fn->current_depth;
  add_ins(op, fn);
}

void ins1(enum operator op, ubyte arg1, struct fncode *fn)
/* Effects: Adds instruction ins to code of 'fn'.
     The instruction has one argument, arg1.
   Modifies: fn
*/
{
  switch (op)
    {
      /* Note: op_exit_n *MUST NOT* modify stack depth */
    case op_recall + vclass_local: case op_recall + vclass_closure:
    case op_vref + vclass_local: case op_integer1: case op_constant1:
    case op_closure:
      fn->current_depth++;
      if (fn->current_depth > fn->max_depth) fn->max_depth = fn->current_depth;
      break;
    case op_execute: case op_pop_n: case op_execute_primitive:
    case op_execute_secure: case op_execute_varargs:
      fn->current_depth -= arg1;
      break;
    default:
      break;
    }
  add_ins(op, fn);
  add_ins(arg1, fn);
}

void ins2(enum operator op, uword arg2, struct fncode *fn)
/* Effects: Adds instruction ins to code of 'fn'.
     The instruction has a two byte argument (arg2), stored in big-endian
     format.
   Modifies: fn
*/
{
  switch (op)
    {
    case op_recall + vclass_global: case op_vref + vclass_global:
    case op_integer2: case op_constant2:
      fn->current_depth++;
      if (fn->current_depth > fn->max_depth) fn->max_depth = fn->current_depth;
      break;
    case op_execute_global2: case op_execute_primitive2:
      fn->current_depth--;
      break;
    default:
      break;
    }
  add_ins(op, fn);
  add_ins(arg2 >> 8, fn);
  add_ins(arg2 & 0xff, fn);
}

void branch(enum operator abranch, struct label *to, struct fncode *fn)
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
    default: abort();
    }
  add_ins(abranch, fn);
  fn->instructions->to = to;
  add_ins(0, fn); /* Reserve a 1 byte offset */
}

static void resolve_labels(struct fncode *fn)
/* Effects: Removes all references in branches to labels that are aliases
     (replaces them with the 'real' label.
     Also removes unconditional branches to the next instruction.
   Modifies: fn
   Requires: The code only contain 1 byte branches.
*/
{
  struct ilist *prev1 = NULL, *prev2 = NULL;
  for (struct ilist *scan = fn->instructions; scan; scan = scan->next)
    {
      if (scan->to)
	{
	  if (scan->to->alias) scan->to = scan->to->alias;
	  assert(scan->to->ins);

	  /* prev1 is the (reserved) offset, prev2 is the next instruction */
	  if (scan->ins.op == op_branch1 && scan->to->ins == prev2)
	    {
	      /* Remove branch to next instruction */
	      prev2->next = scan->next;
	      if (scan->lab)
		/* If removed instruction had a label, make it point
                   to prev2 */
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

static void number_instructions(struct fncode *fn)
/* Effects: Numbers the instructions in fn (starting from the end)
   Modifies: fn
*/
{
  ulong offset = 0;
  for (struct ilist *scan = fn->instructions;
       scan;
       scan = scan->next, offset++)
    scan->offset = offset;
}

static bool resolve_offsets(struct fncode *fn)
/* Effects: Resolves all branch offsets in fn. Increases the size of
     the branches if necessary.
   Returns: true if all branches could be resolved without increasing
     the size of any branches
*/
{
  bool ok = true;

  struct ilist *prev1 = NULL, *prev2 = NULL;

  for (struct ilist *scan = fn->instructions; scan; scan = scan->next)
    {
      if (scan->to)		/* This is a branch */
	{
	  long offset = scan->offset - scan->to->ins->offset;

	  if ((scan->ins.op - op_branch1) & 1)
	    {
	      /* Two byte branch */
	      assert(prev1); assert(prev2);
	      offset -= 3;

	      if (offset >= INTEGER2_MIN && offset <= INTEGER2_MAX)
		{
		  prev1->ins.u = offset >> 8;
		  prev2->ins.u = offset & 0xff;
		}
	      else
		{
		  /* Branch doesn't fit. TBD. */
                  abort();
		}
	    }
	  else
	    {
	      /* One byte */
	      assert(prev1);
	      offset -= 2;

	      if (offset >= INTEGER1_MIN && offset <= INTEGER1_MAX)
		prev1->ins.u = offset;
	      else
		{
		  /* Make a 2 byte branch */
		  struct ilist *newp = allocate(fn->memory, sizeof *newp);
                  *newp = (struct ilist){
                    .next = scan
                  };
		  prev1->next = newp;
		  scan->ins.u++;

		  ok = false;
		}
	    }
	}

      prev2 = prev1;
      prev1 = scan;
    }
  return ok;
}

void peephole(struct fncode *fn)
/* Effects: Does some peephole optimisation on instructions of 'fn'
     Currently this only includes branch size optimisation (1 vs 2 bytes)
     and removal of unconditional branches to the next instruction.
     Also resolves branches...
   Modifies: fn
   Requires: All labels be defined
*/
{
  resolve_labels(fn);

  do
    number_instructions(fn);
  while (!resolve_offsets(fn));
}

static inline struct ilist *reverse_ilist(struct ilist *l)
{
  return reverse_list(l, struct ilist);
}

struct linenos {
  int size, used;
  ubyte *data;
};

static void grow_linenos(struct linenos *l)
{
  l->size = l->size ? l->size * 2 : 64;
  l->data = realloc(l->data, l->size);
}

/*
  line information encoding:

    bytes on current line
    delta lines

  if the value to be encoded is < 0 or >= 255, a 255 byte is used,
  followed by the new number (not a delta)
*/
static struct string *build_lineno_data(struct fncode *fn)
{
  struct ilist *ins = fn->instructions;
  if (ins == NULL)
    return NULL;

  struct linenos data = { 0 };

#define ADD(n) do {				\
  if (data.used == data.size)			\
    grow_linenos(&data);			\
  data.data[data.used++] = (n);		\
} while (0)

  int last_line = fn->lineno, start_offset = ins ? ins->offset : 0;
  int last_offset = start_offset;

  for (; ins; ins = ins->next)
    {
      if (ins->lineno == 0 || ins->lineno == last_line)
	continue;

      if (ins->offset + 255 < last_offset)
	{
	  ADD(255);
	  for (int i = 0; i < 4; ++i)
	    ADD(((start_offset - ins->offset) >> (8 * i)) & 0xff);
	}
      else
	ADD(last_offset - ins->offset);
      last_offset = ins->offset;

      if (last_line > ins->lineno || (ins->lineno >= last_line + 255))
	{
	  int i;
	  ADD(255);
	  for (i = 0; i < 4; ++i)
	    ADD((ins->lineno >> (8 * i)) & 0xff);
	}
      else
	ADD(ins->lineno - last_line);
      last_line = ins->lineno;
    }

#undef ADD

  struct string *res = (struct string *)allocate_string(type_string,
							data.used + 1);
  memcpy(res->str, data.data, data.used);
  res->str[data.used] = 0;
  res->o.flags |= OBJ_IMMUTABLE | OBJ_READONLY;

  free(data.data);

  return res;
}

int get_code_line_number(struct icode *code, int offset)
{
  int line = code->code.lineno;
  int dlen;
  int pos = 0;
  ubyte *data;
  int cofs = 0;

  if (code->lineno_data == NULL)
    return -1;

  dlen = string_len(code->lineno_data);
  data = (ubyte *)code->lineno_data->str;

  for (;;)
    {
      if (pos >= dlen)
	return line;

      if (data[pos] == 255)
	{
	  cofs = 0;
	  ++pos;
	  for (int i = 0; i < 4; ++i)
	    cofs |= data[pos++] << (8 * i);
	}
      else
	cofs += data[pos++];

      if (cofs > offset)
	return line;

      if (data[pos] == 255)
	{
	  line = 0;
	  ++pos;
	  for (int i = 0; i < 4; ++i)
	    line |= data[pos++] << (8 * i);
	}
      else
	line += data[pos++];
    }
}

struct icode *generate_fncode(struct fncode *fn,
                              struct string *help,
                              struct string *varname,
                              struct string *afilename,
                              struct string *anicename,
                              int alineno,
                              struct vector *arg_types,
                              unsigned return_typeset,
                              int seclev)
/* Returns: A code structure with the instructions and constants in 'fn'.
   Requires: generate_fncode may only be called on the result of the most
     recent call to new_fncode. That call is then deemed to never have
     occured :-) (this means that new_fncode/generate_fncode must be paired
     in reverse temporal order)
*/
{
  struct string *lineno_data = NULL;

  fn->instructions = reverse_ilist(fn->instructions);

  /* Count # of instructions */
  ulong sequence_length = 0;
  for (struct ilist *scanins = fn->instructions;
       scanins;
       scanins = scanins->next)
    ++sequence_length;

  assert(immutablep(arg_types));

  GCPRO6(help, varname, afilename, anicename, arg_types, lineno_data);
  lineno_data = build_lineno_data(fn);

  /* Warning: Portability */
  ulong size = (offsetof(struct icode, constants)
                + fn->cstindex * sizeof(value)
                + sequence_length * sizeof (union instruction));
  bc_length += size;
  struct icode *gencode = gc_allocate(size);
  UNGCPRO();

  *gencode = (struct icode){
    .code = {
      .o = {
        .size         = size,
        .garbage_type = garbage_code,
        .type         = type_code,
        .flags        = OBJ_IMMUTABLE, /* code is immutable */
#ifdef GCDEBUG
        .generation = gencode->code.o.generation,
#endif
      },
      .varname        = varname,
      .filename       = afilename,
      .nicename       = anicename,
      .help           = help,
      .arg_types      = arg_types,
      .lineno         = alineno,
      .seclevel       = seclev,
      .return_typeset = return_typeset,
    },
    .nb_constants      = fn->cstindex,
    .nb_locals         = 0,     /* initialized later */
    .stkdepth          = fn->max_depth,
    .call_count        = 0,
    .instruction_count = 0,
    .lineno_data       = lineno_data,
  };

  /* Copy the sequence (which is reversed) */
  union instruction *codeins
    = (union instruction *)(gencode->constants + fn->cstindex);
  for (struct ilist *scanins = fn->instructions;
       scanins;
       scanins = scanins->next)
    *codeins++ = scanins->ins;

  /* Copy the constants */
  {
    struct list *csts = fn->csts.obj;
    for (int i = fn->cstindex; i-- > 0; )
      {
        GCCHECK(csts);
        assert(TYPE(csts, type_pair));
        GCCHECK(csts->car);
        gencode->constants[i] = csts->car;
        csts = csts->cdr;
      }
    assert(csts == NULL);
  }

  /* Jump to interpreter to execute interpreted code - machine specific */

#if defined(i386) && !defined(NOCOMPILER)
  /* movl interpreter_invoke,%ecx */
  gencode->magic_dispatch[0] = 0xb9;
  ulong invoke_addr = (ulong)interpreter_invoke;
  memcpy(gencode->magic_dispatch + 1, &invoke_addr, sizeof invoke_addr);
  /* jmp *%ecx */
  gencode->magic_dispatch[5] = 0xff;
  gencode->magic_dispatch[6] = 0xe1;
  /* nop */
  gencode->magic_dispatch[7] = 0x90;
#endif

#ifdef GCSTATS
  gcstats_add_alloc(type_code, MUDLLE_ALIGN(size, sizeof (long)));
#endif

  return gencode;
}

struct label *new_label(struct fncode *fn)
/* Returns: A new label which points to nothing. Use label() to make it
     point at a particular instruction.
*/
{
  struct label *newp = allocate(fn->memory, sizeof *newp);

  newp->ins = NULL;
  newp->alias = NULL;

  return newp;
}

void set_label(struct label *lab, struct fncode *fn)
/* Effects: lab will point at the next instruction generated with ins0,
     ins1, ins2 or branch.
   Modifies: lab
*/
{
  if (fn->next_label) lab->alias = fn->next_label;
  else fn->next_label = lab;
}

void start_block(const char *name, struct fncode *fn)
/* Effects: Starts a block called name (may be NULL), which can be
     exited with exit_block()
*/
{
  struct blocks *newp = allocate(fn->memory, sizeof *newp);

  newp->next = fn->blks;
  newp->name = name;
  newp->exitlab = new_label(fn);
  newp->stack_depth = fn->current_depth;

  fn->blks = newp;
}

void end_block(struct fncode *fn)
/* Effects: End of named block. Generate exit label
*/
{
  set_label(fn->blks->exitlab, fn);
  fn->blks = fn->blks->next;
}

int exit_block(const char *name, struct fncode *fn)
/* Effects: Generates code to exit from specified named block
     (pop stack, jump to block exit label)
   Returns: false if the named block doesn't exist
*/
{
  struct blocks *find = fn->blks;
  int npop;

  for (;;)
    {
      if (!find) return false;
      if (name == NULL)
	{
	  if (find->name == NULL) break;
	}
      else if (find->name != NULL && strcasecmp(name, find->name) == 0) break;
      find = find->next;
    }

  npop = fn->current_depth - find->stack_depth - 1;
  assert(npop >= 0);
  if (npop > 0) ins1(op_exit_n, npop, fn);
  branch(op_branch1, find->exitlab, fn);

  return true;
}
