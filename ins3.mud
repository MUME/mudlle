/*
 * Copyright (c) 1993-2012 David Gay
 * All rights reserved.
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose, without fee, and without written agreement is hereby granted,
 * provided that the above copyright notice and the following two paragraphs
 * appear in all copies of this software.
 *
 * IN NO EVENT SHALL DAVID GAY BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
 * SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OF
 * THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF DAVID GAY HAVE BEEN ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
 *
 * DAVID GAY SPECIFICALLY DISCLAIM ANY WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS ON AN "AS IS" BASIS, AND DAVID
 * GAY HAVE NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES,
 * ENHANCEMENTS, OR MODIFICATIONS.
 */

// 3-address (not really) instructions for intermediate representation
library ins3
requires system, compiler, vars, misc, dlist, sequences
defines

  mc:i_class, mc:i_compute, mc:i_aop, mc:i_adest, mc:i_aargs, mc:i_atypes,
  mc:i_asizeinfo,
  mc:i_branch, mc:i_bop, mc:i_bdest, mc:i_bargs, mc:i_btypes, mc:i_trap,
  mc:i_top, mc:i_tdest, mc:i_targs, mc:i_ttypes, mc:i_memory, mc:i_mop,
  mc:i_marray, mc:i_mindex, mc:i_mscalar, mc:i_closure, mc:i_fdest,
  mc:i_ffunction,
  mc:i_call, mc:i_cdest, mc:i_cargs, mc:i_cfunction, mc:i_ctypes,
  mc:i_csizeinfo,
  mc:i_return, mc:i_rvalue, mc:i_rtype, mc:i_vref, mc:i_vdest, mc:i_varg,

  mc:branch_never, mc:branch_always, mc:branch_true, mc:branch_false,
  mc:branch_or, mc:branch_nor, mc:branch_and, mc:branch_nand, mc:branch_eq,
  mc:branch_ne, mc:branch_lt, mc:branch_ge, mc:branch_le, mc:branch_gt,
  mc:branch_slength, mc:branch_vlength, mc:branch_equal, mc:branch_nequal,
  mc:branch_immutable, mc:branch_mutable, mc:branch_readonly,
  mc:branch_writable, mc:branch_any_prim, mc:branch_not_prim,
  mc:branch_type?, mc:branch_ntype?,

  mc:trap_never, mc:trap_always, mc:trap_argcheck, mc:trap_loop, mc:trap_type,
  mc:trap_global_write, mc:trap_global_read,

  mc:memory_read, mc:memory_write, mc:memory_write_safe,
  mc:il_label, mc:il_ins, mc:il_node, mc:il_number, mc:il_lineno,
  mc:il_defined_var, mc:il_arguments, mc:il_live_in, mc:il_live_out, mc:l_ilist,
  mc:l_alias, mc:l_number, mc:l_mclabel,

  mc:itype_typeset, mc:itypemap, mc:itypemap_inverse,
  itype_none, itype_function, itype_integer, itype_string, itype_vector,
  itype_null, itype_symbol, itype_table, itype_pair, itype_other, itype_any,
  itype_float, itype_bigint, itype_reference, itype_non_zero, itype_zero,
  itype_list, itype_names,
  mc:typeset_any, mc:stype_typesets,

  mc:types_from_typeset, mc:itypeset_from_typeset,

  mc:new_fncode, mc:set_instruction, mc:get_instructions, mc:remove_branches,
  mc:remove_aliases, mc:remove_labels, mc:remove_var_aliases, mc:new_local,
  mc:ins_return, mc:ins_trap, mc:ins_branch, mc:make_compute_ins,
  mc:ins_compute, mc:ins_closure, mc:ins_vref, mc:ins_call, mc:ins_memory,
  mc:make_memory_ins, mc:ins_assign,
  mc:new_label, mc:ins_label, mc:set_label,
  mc:start_block, mc:end_block, mc:exit_block, mc:defined_var,
  mc:closure_vars, mc:arguments, mc:set_closure_vars!, mc:barguments,
  mc:replace_dest, mc:replace_args, mc:ins_list, mc:ins_list1, mc:print_ins,
  mc:slabel, mc:fncode_fn, mc:new_varset, mc:set_vars!, mc:call_escapes?,
  mc:my_protected_global?, mc:make_trap_ins

reads mc:lineno, mc:this_module

[
//  x := y op z, x := op y, x := y
//  if x op y goto l, if op x goto l, goto l
//  if x op y trap t, if op x trap t, trap t
//  x[i] := y, y := x[i] (these are not the mudlle [] operator)
//  x := closure y, z1, ..., zn
//  x := call y, z1, z2, ..., zn
//  return x

// All values in variables are mudlle values -> no data size considerations

// Intermediate code rep. Each instruction is a vector:

mc:i_class = 0; // Class of instruction:
		// mc:i_compute, mc:i_branch, mc:i_memory,
		// mc:i_closure, mc:i_call, mc:i_trap, mc:i_return

mc:i_compute = 0; // no side effects in the actual op
 mc:i_aop = 1; // operator, determines # of args
 mc:i_adest = 2; // destination variable
 mc:i_aargs = 3; // arguments (list of var)
 mc:i_atypes = 4; // types of arguments (list of itype) or false
 mc:i_asizeinfo = 5; // known min. size of ref argument, or null

mc:i_branch = 1;
 mc:i_bop = 1; // branch operator
 mc:i_bdest = 2; // destination label
 mc:i_bargs = 3; // arguments (list of var)
 mc:i_btypes = 4; // types of arguments (list of itype)

mc:i_trap = 5; // runtime error checks
 mc:i_top = 1; // trap operator
 mc:i_tdest = 2; // destination trap
 mc:i_targs = 3; // arguments (list of var)
 mc:i_ttypes = 4; // types of arguments (list of itype)

mc:i_memory = 2;
 mc:i_mop = 1; // read or write
 mc:i_marray = 2; // the array variable
 mc:i_mindex = 3; // the index (int)
 mc:i_mscalar = 4; // source or destination variable

mc:i_closure = 3;
 mc:i_fdest = 1; // where to place the closure
 mc:i_ffunction = 2; // the actual function (component)

mc:i_call = 4;
 mc:i_cdest = 1; // function result (var)
 mc:i_cargs = 2; // arguments, first is function (list of var)
 mc:i_cfunction = 3; // an mc:c_closure (for module locals), or null
                     // x0 . x1 for apply-like function x0 calling x1
 mc:i_ctypes = 4;    // itype_xxx bitfield list, or false
 mc:i_csizeinfo = 5; // known min. size of set! argument, or null

mc:i_return = 6;
 mc:i_rvalue = 1;
 mc:i_rtype = 2;

mc:i_vref = 7;
 mc:i_vdest = 1; // result variable
 mc:i_varg = 2; // source variable

// branch ops
mc:branch_never      = 0; // simplifies constant folding
mc:branch_always     = 1;
mc:branch_true       = 2;
mc:branch_false      = 3;
mc:branch_or         = 4;
mc:branch_nor        = 5;
mc:branch_and        = 6;
mc:branch_nand       = 7;
mc:branch_eq         = 8;
mc:branch_ne         = 9;
mc:branch_lt         = 10;      // lt, ge, le, gt toggle meaning by
mc:branch_ge         = 11;      // flipping the lowest bit
mc:branch_le         = 12;
mc:branch_gt         = 13;
mc:branch_slength    = 14;      // eq, ne, ...
mc:branch_vlength    = 20;      // eq, ne, ...
mc:branch_equal      = 26;
mc:branch_nequal     = 27;
mc:branch_immutable  = 28;
mc:branch_mutable    = 29;
mc:branch_readonly   = 30;
mc:branch_writable   = 31;
mc:branch_any_prim   = 32;
mc:branch_not_prim   = 33;
mc:branch_type?      = 34;
mc:branch_ntype?     = mc:branch_type? + last_synthetic_type;

| branch_names |
branch_names = '[
  "never" "always" "true" "false" "or" "nor" "and" "nand"
  "==" "!=" "<" ">=" "<=" ">"
  "slength ==" "slength !=" "slength <" "slength >=" "slength <=" "slength >"
  "vlength ==" "vlength !=" "vlength <" "vlength >=" "vlength <=" "vlength >"
  "equal?" "!equal?" "immutable?"
  "!immutable?" "readonly?" "!readonly?" "any_primitive?"
  "!any_primitive?"];
assert(vlength(branch_names) == mc:branch_type?);

// traps
mc:trap_never         = 0; // simplifies constant folding
mc:trap_always        = 1;
mc:trap_argcheck      = 2;
mc:trap_loop          = 3; // check for infinite loops (implicit at
                           // function entry)
mc:trap_type          = 4; // arg1 is value, arg2 is type (constant)
mc:trap_global_write  = 5; // check that arg1 is not readonly (arg1 is global)
                           // and that the calling code is allowed to write it
mc:trap_global_read   = 6; // check that reading global arg1 is allowed

| trap_names |
trap_names = '["never" 0 "argcheck" "loop" "!type" "!writable" "!readable"];

// memory ops
mc:memory_read = 1;
mc:memory_write = 2;
mc:memory_write_safe = 3; // write with check for read-only

// An instruction list is a list of the following vectors:

mc:il_label        = 0; // label of this instruction, or false
mc:il_ins          = 1; // the actual instruction
mc:il_node         = 2; // the basic block to which this instruction belongs
mc:il_number       = 3; // a unique number (for display)
mc:il_lineno       = 4; // line number of this instruction
mc:il_defined_var  = 5; // number of variable defined here (or false)
mc:il_arguments    = 6; // varset of arguments used

mc:il_live_in      = 5; // data-flow information
mc:il_live_out     = 6;


// labels:

mc:l_ilist    = 0;      // instruction list pointed to
mc:l_alias    = 1;      // we are an alias to this label
mc:l_number   = 2;      // unique number (for display)
mc:l_mclabel  = 3;      // corresponding machine code label

itype_none      = 0;	// no type

itype_function  = 1;
itype_non_zero  = 2;
itype_string    = 4;
itype_vector    = 8;
itype_null      = 16;
itype_symbol    = 32;
itype_table     = 64;
itype_pair      = 128;
itype_bigint    = 256;
itype_float     = 512;
itype_reference = 1024;
itype_other     = 2048;
itype_zero      = 4096;
itype_integer   = itype_non_zero | itype_zero;
itype_list      = itype_null | itype_pair;

itype_any       = 8191;         // "any" type

itype_names = '[
  "function" "non-zero integer" "string" "vector" "null"
  "symbol" "table" "pair" "bigint" "float" "reference" "\"other\"" "false"
];
assert((1 << vlength(itype_names)) - 1 == itype_any);

// map from itype_xxx bit -> type_xxx bitset
mc:itype_typeset =
  '[// itype_function
    ,((1 << type_closure) | (1 << type_primitive) | (1 << type_varargs)
      | (1 << type_secure))
    ,(1 << type_integer)        // non-zero
    ,(1 << type_string)
    ,(1 << type_vector)
    ,(1 << type_null)
    ,(1 << type_symbol)
    ,(1 << type_table)
    ,(1 << type_pair)
    ,(1 << type_bigint)
    ,(1 << type_float)
    ,(1 << type_reference)
    // itype_other
    ,((1 << type_code) | (1 << type_variable) | (1 << type_internal)
      | (1 << type_private) | (1 << type_object) | (1 << type_character)
      | (1 << type_gone) | (1 << type_oport) | (1 << type_mcode))
    ,(1 << type_integer)        // zero
  ];
assert(vlength(mc:itype_typeset) == vlength(itype_names));

mc:itypemap = sequence // map from type_xxx/stype_xxx -> itype typesets
  (itype_other,                 // type_code
   itype_function,              // type_closure
   itype_other,                 // type_variable
   itype_other,                 // type_internal
   itype_function,              // type_primitive
   itype_function,              // type_varargs
   itype_function,              // type_secure
   itype_integer,               // type_integer
   itype_string,                // type_string
   itype_vector,                // type_vector
   itype_pair,                  // type_pair
   itype_symbol,                // type_symbol
   itype_table,                 // type_table
   itype_other,                 // type_private
   itype_other,                 // type_object
   itype_other,                 // type_character
   itype_other,                 // type_gone
   itype_other,                 // type_oport
   itype_other,                 // type_mcode
   itype_float,                 // type_float
   itype_bigint,                // type_bigint
   itype_reference,             // type_reference
   itype_null,                  // type_null
   itype_none,                  // stype_none
   itype_any,                   // stype_any
   itype_function,              // stype_function
   itype_list);                 // stype_list
assert(vlength(mc:itypemap) == last_synthetic_type);

vfori(fn (i) [
  | it |
  it = 1 << i;
  for (| ts, t | [ t = 0; ts = mc:itype_typeset[i] ]; ts; [ ts >>= 1; ++t ])
    if (ts & 1)
      assert(mc:itypemap[t] & it);
], mc:itype_typeset);

mc:itypemap_inverse = sequence // map from type_xxx/stype_xxx -> itype typesets
  (itype_any,		        // type_code
   itype_any,		        // type_closure
   itype_any,		        // type_variable
   itype_any,		        // type_internal
   itype_any,		        // type_primitive
   itype_any,		        // type_varargs
   itype_any,		        // type_secure
   itype_any & ~itype_integer,  // type_integer
   itype_any & ~itype_string,	// type_string
   itype_any & ~itype_vector,	// type_vector
   itype_any & ~itype_pair,	// type_pair
   itype_any & ~itype_symbol,	// type_symbol
   itype_any & ~itype_table,	// type_table
   itype_any,	                // type_private
   itype_any,	                // type_object
   itype_any,	                // type_character
   itype_any,	                // type_gone
   itype_any,	                // type_oport
   itype_any,	                // type_mcode
   itype_any & ~itype_float,    // type_float
   itype_any & ~itype_bigint,   // type_bigint
   itype_any & ~itype_reference,// type_reference
   itype_any & ~itype_null,	// type_null
   itype_any,	                // stype_none
   itype_none,                  // stype_any
   itype_any & ~itype_function, // stype_function
   itype_any & ~itype_list);    // stype_list
assert(vlength(mc:itypemap) == last_synthetic_type);

mc:typeset_any = (1 << last_type) - 1;
mc:stype_typesets =
  '[ 0                                             // stype_none
     ,mc:typeset_any                               // stype_any
     ,((1 << type_closure) | (1 << type_primitive) // stype_function
       | (1 << type_varargs) | (1 << type_secure))
     ,((1 << type_pair) | (1 << type_null)) ];     // stype_list
assert(vlength(mc:stype_typesets) == last_synthetic_type - last_type);

// save on frequent allocations
| static_type_pairs |
static_type_pairs = make_vector(last_synthetic_type);
for (|t| t = 0; t < last_synthetic_type; ++t)
  static_type_pairs[t] = '(,t);
protect(static_type_pairs);

mc:types_from_typeset = list fn (int typeset)
  [
    if (typeset == mc:typeset_any)
      exit<function> static_type_pairs[stype_any];

    if (typeset == 0)
      exit<function> static_type_pairs[stype_none];

    | r, t |
    t = last_synthetic_type;
    loop
      [
        --t;
        | ts |
        if (t >= last_type)
          ts = mc:stype_typesets[t - last_type]
        else
          ts = 1 << t;
        if (ts != 0 && ((typeset & ts) == ts))
          [
            typeset &= ~ts;
            if (r == null)
              r = static_type_pairs[t]
            else
              r = t . r;
            if (typeset == 0)
              exit r;
          ]
      ]
  ];

mc:itypeset_from_typeset = fn (ts)
  [
    | is, types |
    types = mc:types_from_typeset(ts);
    is = 0;
    loop
      [
        if (types == null) exit is;
        is |= mc:itypemap[car(types)];
        types = cdr(types)
      ]
  ];

[
  | label_index, ins_index, add_ins, print_if, print_op, is_temp? |

  mc:new_fncode = fn "component -> fncode. Returns a structure to use for\n\
generating instructions for function component" (top)
    [
      // ilpos, nextlabel, top, blockstack
      vector(null, false, top, null)
    ];

  mc:fncode_fn = fn "fncode -> component. Return compoenent we are generating for" (fcode) fcode[2];

  mc:set_instruction = fn "fncode ilist -> . Sets the current instruction insert position to ilist" (fcode, pos)
    [
      if (fcode[1] || fcode[3] != null) fail();
      fcode[0] = pos;
    ];

  mc:get_instructions = fn "fncode -> ilist. Returns instruction list of ilist" (fcode)
    [
      if (fcode[1] || fcode[3] != null) fail();
      fcode[0]
    ];

  mc:remove_branches = fn (ilist)
    // Types: ilist : instruction list
    // Returns: the list of instructions of fcode, with
    //   branches to the next instruction destroyed.
    [
      | old |

      old = ilist;
      loop
	[
	  | ins, iold, next, label, nlabel, nextins |

	  next = dnext(old);
	  if (next == ilist) exit ilist;

	  iold = dget(old);
	  ins = iold[mc:il_ins];

	  // remove branch to next instruction
	  if (ins[mc:i_class] == mc:i_branch)
	    [
	      label = ins[mc:i_bdest];
	      while (nlabel = label[mc:l_alias]) label = nlabel;

	      nextins = dget(next);

	      if (label[mc:l_ilist] == nextins)
		[
		  // did removed instruction have a label ?
		  if (label = iold[mc:il_label])
		    mc:set_label(label, nextins);
		  dremove!(old, old);
		];
	    ];
	  old = next
	]
    ];

  mc:remove_aliases = fn (ilist)
    // Types: ilist: list of instructions
    // Requires: mc:remove_labels be called on ilist just after
    //   The split is present to allow use even when the code of the
    //   function is split between the flow graph nodes.
    // Returns: the list of instructions of fcode, with aliases removed.
    [
      | scan |

      // first remove all aliases
      scan = ilist;
      loop
	[
	  | ins, label, nlabel |
	  ins = dget(scan)[mc:il_ins];

	  if (ins[mc:i_class] == mc:i_branch)
	    [
	      // branch removal can create more than one depth of aliasing
	      label = ins[mc:i_bdest];
	      while (vector?(nlabel = label[mc:l_alias])) label = nlabel;
	      ins[mc:i_bdest] = label;

	      // label is used
	      label[mc:l_alias] = true;
	    ];
	  scan = dnext(scan);
	  if (scan == ilist) exit ilist
	]
    ];

  mc:remove_labels = fn (ilist)
    // Types: ilist: list of instructions
    // Requires: mc:remove_aliases be called on ilist just before
    // Effects: removes useless labels (is this really necessary here?)
    [
      | scan |

      scan = ilist;
      loop
	[
	  | label |
	  if (label = dget(scan)[mc:il_label])
	    if (!label[mc:l_alias]) // unused
	      dget(scan)[mc:il_label] = false
	    else
	      label[mc:l_alias] = false; // reset to usual state
	  scan = dnext(scan);
	  if (scan == ilist) exit ilist
	]
    ];

  mc:remove_var_aliases = fn (ilist)
    // Types: ilist: list of instructions
    // Returns: the list of instructions of fcode, with variable aliases removed.
    [
      | scan, rep, replist |

      rep = mc:alias_base;
      replist = fn (s) lmap!(rep, s);

      scan = ilist;
      loop
	[
	  | ins, class |

	  ins = dget(scan)[mc:il_ins];
	  class = ins[mc:i_class];

	  if (class == mc:i_compute)
	    [
	      ins[mc:i_adest] = rep(ins[mc:i_adest]);
	      replist(ins[mc:i_aargs])
	    ]
	  else if (class == mc:i_branch)
	    replist(ins[mc:i_bargs])
	  else if (class == mc:i_trap)
	    replist(ins[mc:i_targs])
	  else if (class == mc:i_memory)
	    [
	      ins[mc:i_marray] = rep(ins[mc:i_marray]);
	      ins[mc:i_mscalar] = rep(ins[mc:i_mscalar])
	    ]
	  else if (class == mc:i_call)
	    [
	      ins[mc:i_fdest] = rep(ins[mc:i_fdest]);
	      replist(ins[mc:i_cargs])
	    ]
	  else if (class == mc:i_closure)
	    ins[mc:i_cdest] = rep(ins[mc:i_cdest])
	  else if (class == mc:i_vref)
	    ins[mc:i_vdest] = rep(ins[mc:i_vdest])
	  else if (class == mc:i_return)
	    ins[mc:i_rvalue] = rep(ins[mc:i_rvalue])
          else
            [
              dformat("unhandled class %d\n", class);
              fail()
            ];

	  scan = dnext(scan);
	  if (scan == ilist) exit ilist
	]
    ];

  mc:new_local = fn "fncode -> var. Creates a new local variable for fncode" (fcode)
    [
      | local |
      local = mc:var_make_local("");
      //fcode[2][mc:c_flocals] = local . fcode[2][mc:c_flocals];
      local
    ];

  is_temp? = fn "var -> b. True if var was created by mc:new_local" (v)
    v[mc:v_class] == mc:v_local && string_length(v[mc:v_name]) == 0;

  ins_index = 0;
  add_ins = fn (fcode, ins)
    // Types: fcode : fncode
    //        ins : instruction
    // Effects: Adds ins to the instructions in fcode, setting the label
    //   if necessary.
    //   Clears the current label
    [
      | newins |

      // Add instruction
      newins = vector(fcode[1], ins, null, ins_index = ins_index + 1,
		      mc:lineno + 0, false, false);

      // This is a strange hack:
      //   When code is initially generated, fcode[0] starts at null,
      //   then  gets set to the first instruction with subsequent
      //   instructions inserted before it (and because of the circular
      //   nature of the list, actually at the end).
      //   Later, when code needs patching, mc:set_instruction is called
      //   to set the insertion point, before which new instructions are
      //   added.
      if (fcode[0] == null) fcode[0] = dcons!(newins, null)
      else dcons!(newins, fcode[0]); // insert before fcode[0]

      // Set label if any
      if (fcode[1]) fcode[1][mc:l_ilist] = newins;
      fcode[1] = false;
    ];

  mc:ins_return = fn "fncode var -> . Adds 'return var' to fncode" (fcode, v)
    [
      add_ins(fcode, vector(mc:i_return, v, null));
    ];

  mc:make_trap_ins = fn (op, trap, args)
    vector(mc:i_trap, op, trap, args, false);

  mc:ins_trap = fn "fncode op n l -> . Adds 'if op(l) trap n' to fncode"
    (fcode, op, trap, args)
      add_ins(fcode, mc:make_trap_ins(op, trap, args));

  mc:ins_branch = fn "fncode op label l -> . Adds 'if op(l) goto label' to fncode"
    (fcode, op, label, args)
      add_ins(fcode, vector(mc:i_branch, op, label, args, false));

  mc:make_compute_ins = fn (op, dest, args)
    vector(mc:i_compute, op, dest, args, false, null);

  mc:ins_compute = fn "fncode op var l -> . Adds 'var := op(l)' to fncode"
    (fcode, op, dest, args)
      add_ins(fcode, mc:make_compute_ins(op, dest, args));

  mc:ins_closure = fn "fncode var component -> . Adds 'var := closure(component)' to fncode"
    (fcode, dest, f)
      add_ins(fcode, vector(mc:i_closure, dest, f));

  mc:ins_vref = fn "fncode var0 var1 -> Adds 'var0 := vref(var1)'"
    (fcode, dest, var)
      add_ins(fcode, vector(mc:i_vref, dest, var));

  mc:ins_call = fn "fncode var l -> . Adds 'var := call l' to fncode"
    (fcode, dest, args)
      add_ins(fcode, vector(mc:i_call, dest, args, null, false, null));

  mc:make_memory_ins = fn (op, array, index, scalar)
    vector(mc:i_memory, op, array, index, scalar);

  mc:ins_memory = fn "fncode op var1 n var2-> Adds 'var2 := var1[n] / var1[n] := var2' to fncode"
    (fcode, op, array, index, scalar)
      add_ins(fcode, mc:make_memory_ins(op, array, index, scalar));

  mc:ins_assign = fn "fncode var1 var2 -> . 'var1 := var2', using aliases if possible" (fcode, v1, v2)
    [
      | v2b |

      v2b = mc:alias_base(v2);
      if (is_temp?(v2b)) mc:alias(v2b, v1)
      else mc:ins_compute(fcode, mc:b_assign, v1, list(v2))
    ];

  // labels

  label_index = 0;
  mc:new_label = fn "fncode -> label. Returns a new unassigned label in fncode"
    (fcode)
      vector(false, false, label_index = label_index + 1, null);

  mc:ins_label = fn "fncode label -> . Makes label point at the next instruction to\n\
be generated in fncode" (fcode, label)
      [
	if (fcode[1]) label[mc:l_alias] = fcode[1]
	else fcode[1] = label;
      ];

  mc:set_label = fn "label ilist -> . Sets label to point to ilist. Might make it an alias of existing label" (l, il)
    [
      | lab |

      if (lab = il[mc:il_label]) // make it an alias
	[
	  l[mc:l_alias] = lab;
	  l[mc:l_ilist] = false;
	]
      else
	[
	  l[mc:l_ilist] = il;
	  il[mc:il_label] = l;
	]
    ];

  // labeled blocks
  mc:start_block = fn "fncode s -> . Starts a new block called s in fncode" (fcode, name)
    fcode[3] = vector(name, mc:new_label(fcode), mc:new_local(fcode)) . fcode[3];

  mc:end_block = fn "fncode var1 -> var2. End of block, with value var1. Returns block value var2" (fcode, v)
    [
      | block |

      block = car(fcode[3]);
      fcode[3] = cdr(fcode[3]);
      if (v) mc:ins_assign(fcode, block[2], v);
      mc:ins_label(fcode, block[1]);
      block[2]
    ];

  mc:exit_block = fn "fncode s var -> b. Exit block s with result var. Returns false if block unknown" (fcode, name, v)
    [
      | block |

      if (block = lexists?(if (name == null) fn (b) b[0] == null
			   else fn (b) b[0] != null && string_icmp(b[0], name) == 0,
			   fcode[3]))
	[
	  mc:ins_assign(fcode, block[2], v);
	  mc:ins_branch(fcode, mc:branch_always, block[1], null);
	  true
	]
      else
	false
    ];

  // varsets
  mc:new_varset = fn (ifn)
    // Returns: A new empty bitset of variables of ifn
    new_bitset(ifn[mc:c_fnvars]);

  mc:set_vars! = fn (varset, vars)
    [
      while (vars != null)
	[
	  set_bit!(varset, car(vars)[mc:v_number]);
	  vars = cdr(vars);
	];
      varset
    ];

  // instruction examination
  mc:defined_var = fn (ins)
    [
      | class |
      class = ins[mc:i_class];
      if (class == mc:i_compute) ins[mc:i_adest]
      else if (class == mc:i_memory &&
	       ins[mc:i_mop] == mc:memory_read) ins[mc:i_mscalar]
      else if (class == mc:i_closure) ins[mc:i_fdest]
      else if (class == mc:i_call) ins[mc:i_cdest]
      else if (class == mc:i_vref) ins[mc:i_vdest]
      else false
    ];

  mc:closure_vars = fn (ins)
    lmap(fn (v) v[mc:v_cparent], ins[mc:i_ffunction][mc:c_fclosure]);

  mc:arguments = fn (ins, ambiguous)
    [
      | class |

      class = ins[mc:i_class];
      if (class == mc:i_compute) ins[mc:i_aargs]
      else if (class == mc:i_branch) ins[mc:i_bargs]
      else if (class == mc:i_trap) ins[mc:i_targs]
      else if (class == mc:i_memory)
	if (ins[mc:i_mop] == mc:memory_write ||
            ins[mc:i_mop] == mc:memory_write_safe)
	  list(ins[mc:i_marray],
	       ins[mc:i_mscalar])
	else
	  list(ins[mc:i_marray])
      else if (class == mc:i_closure)
	mc:closure_vars(ins)
      else if (class == mc:i_call)
	if (ambiguous == null || !mc:call_escapes?(ins)) ins[mc:i_cargs]
	else lappend(ins[mc:i_cargs], ambiguous)
      else if (class == mc:i_vref)
        list(ins[mc:i_varg])
      else if (class == mc:i_return)
	ins[mc:i_rvalue] . ambiguous
      else fail()
    ];

  mc:set_closure_vars! = fn (ins, rwmask, b)
    [
      | vars |

      vars = ins[mc:i_ffunction][mc:c_fclosure];
      while (vars != null)
	[
	  | v |

	  v = car(vars);
	  if (rwmask == (mc:closure_read | mc:closure_write) ||
	      (mc:var_base(v)[mc:v_lclosure_uses] & rwmask))
	    set_bit!(b, v[mc:v_cparent][mc:v_number]);
	  vars = cdr(vars);
	]
    ];

  mc:my_protected_global? = fn (var)
    [
      | mod, mname |
      (var[mc:v_class] == mc:v_global_define
       && string?(mod = module_vstatus(var[mc:v_goffset]))
       && string?(mname = mc:this_module[mc:m_name])
       && string_cmp(mod, mname) == 0
       && module_status(mod) == module_protected)
    ];

  mc:call_escapes? = fn (ins)
    // Returns: True if call may escape
    [
      | fun |
      fun = ins[mc:i_cfunction];
      if (pair?(fun))           // apply-like?
        fun = cdr(fun);

      if (vector?(fun))
        exit<function> !fun[mc:c_fnoescape];

      | escapes? |
      escapes? = fn (f, args)
        [
          | fclass |
          fclass = if (mc:my_protected_global?(f))
            mc:v_global_constant
          else
            f[mc:v_class];

          if (fclass == mc:v_global_define)
            [
              | mod |
              mod = module_vstatus(f[mc:v_goffset]);
              if (string?(mod)
                  && equal?(mod, mc:this_module[mc:m_name])
                  && module_status(mod) == module_protected)
                // pretend this is a global constant
                fclass = mc:v_global_constant;
            ];

          if (fclass == mc:v_global_constant)
            if (closure?(fun = global_value(f[mc:v_goffset])))
              ~closure_flags(fun) & clf_noescape
            else if (function?(fun))
              [
                | flags |
                flags = primitive_flags(fun);
                if (flags & OP_APPLY)
                  [
                    | fidx |
                    @[ _ fidx _] = vexists?(fn (x) x[0] == fun,
                                            mc:apply_functions);
                    if (llength(args) > fidx)
                      escapes?(nth(fidx + 1, args), null)
                    else
                      true
                  ]
                else
                  ~flags & OP_NOESCAPE
              ]
            else
              true
          else
            true
        ];

      escapes?(car(ins[mc:i_cargs]), cdr(ins[mc:i_cargs]))
    ];

  mc:barguments = fn (il, ambiguous)
    [
      | ins, class, args |

      ins = il[mc:il_ins];
      class = ins[mc:i_class];
      args = il[mc:il_arguments];
      if (class == mc:i_call && mc:call_escapes?(ins) ||
	  class == mc:i_return) bunion(args, ambiguous)
      else args
    ];

  // instruction update
  mc:replace_dest = fn (ins, newdest)
    [
      | class |

      class = ins[mc:i_class];
      if (class == mc:i_compute) ins[mc:i_adest] = newdest
      else if (class == mc:i_memory &&
	       ins[mc:i_mop] == mc:memory_read) ins[mc:i_mscalar] = newdest
      else if (class == mc:i_closure) ins[mc:i_fdest] = newdest
      else if (class == mc:i_vref) ins[mc:i_vdest] = newdest
      else if (class == mc:i_call) ins[mc:i_cdest] = newdest
      else fail()
    ];

  mc:replace_args = fn (ins, replacements)
    [
      | rep, replist, class |

      rep = fn (v)
	[
	  | r |

	  if (r = assq(v, replacements)) cdr(r)
	  else v
	];

      replist = fn (s)
	while (s != null)
	  [
	    | r |

	    if (r = assq(car(s), replacements)) set_car!(s, cdr(r));
	    s = cdr(s);
	  ];

      class = ins[mc:i_class];

      if (class == mc:i_compute)
	replist(ins[mc:i_aargs])
      else if (class == mc:i_branch)
	replist(ins[mc:i_bargs])
      else if (class == mc:i_trap)
	replist(ins[mc:i_targs])
      else if (class == mc:i_memory)
	[
	  ins[mc:i_marray] = rep(ins[mc:i_marray]);
	  if (ins[mc:i_mop] == mc:memory_write ||
              ins[mc:i_mop] == mc:memory_write_safe)
	    ins[mc:i_mscalar] = rep(ins[mc:i_mscalar])
	]
      else if (class == mc:i_call)
	replist(ins[mc:i_cargs])
      else if (class == mc:i_vref)
	ins[mc:i_varg] = rep(ins[mc:i_varg])
      else if (class == mc:i_return)
	ins[mc:i_rvalue] = rep(ins[mc:i_rvalue])
      else
        fail()
   ];


  // code display

  mc:ins_list = fn "component -> . Prints instruction list of function component" (ifn)
    [
      | closures |

      closures = ifn . null;
      while (closures != null)
	[
	  | cl |

	  cl = car(closures);
	  closures = cdr(closures);

	  dformat("closure %s:\n", cl[mc:c_fnumber]);
	  closures = lappend(closures, mc:ins_list1(cl[mc:c_fvalue]));
	]
    ];

  mc:ins_list1 = fn "ilist -> l. Prints instruction list and returns list of its closures" (ilist)
    [
      | scan, closures |

      closures = null;
      scan = ilist;
      loop
	[
	  | il |
	  il = dget(scan);
	  if (il[mc:il_label])
	    dformat("%s:", mc:slabel(il[mc:il_label]));
	  dformat("\t%s\t(%s) ", il[mc:il_lineno], il[mc:il_number]);
          | ins |
          ins = il[mc:il_ins];
          if (ins == null)
            display("<removed>")
          else
            closures = mc:print_ins(il[mc:il_ins], closures);
	  newline();
	  scan = dnext(scan);
	  if (scan == ilist) exit closures
	]
    ];

  mc:print_ins = fn (ins, closures)
    [
      | print_sz |
      print_sz = fn (sz)
        if (integer?(sz) && sz > 0)
          dformat(" (size >= %d)", sz);

      | class |
      class = ins[mc:i_class];
      if (class == mc:i_compute)
	[
	  dformat("%s := ", mc:svar(ins[mc:i_adest]));
          | op |
          op = ins[mc:i_aop];
	  print_op(mc:builtin_names[op],
		   ins[mc:i_aargs],
                   (op == mc:b_vector || op == mc:b_sequence
                    || op == mc:b_pcons));
          print_sz(ins[mc:i_asizeinfo]);
	]
      else if (class == mc:i_branch)
	[
	  | op, opname |

	  op = ins[mc:i_bop];
	  opname =
	    if (op < mc:branch_type?)
	      branch_names[ins[mc:i_bop]]
	    else if (op < mc:branch_ntype?)
	      format("type[%s]", type_names[op - mc:branch_type?])
	    else
	      format("!type[%s]", type_names[op - mc:branch_ntype?]);

	  print_if(opname, ins[mc:i_bargs]);
	  dformat("goto %s", mc:slabel(ins[mc:i_bdest]));
	]
      else if (class == mc:i_memory)
	if (ins[mc:i_mop] == mc:memory_read)
	  dformat("%s := %s[%s]", mc:svar(ins[mc:i_mscalar]),
                  mc:svar(ins[mc:i_marray]), ins[mc:i_mindex])
	else
	  dformat("%s[%s] := %s",
                  mc:svar(ins[mc:i_marray]), ins[mc:i_mindex],
                  mc:svar(ins[mc:i_mscalar]))
      else if (class == mc:i_closure)
	[
	  | ifn |

	  ifn = ins[mc:i_ffunction];
	  closures = ifn . closures;
	  dformat("%s := newclosure %s(%s)", mc:svar(ins[mc:i_fdest]),
                  ifn[mc:c_fnumber],
                  concat_words(lmap(mc:svar, ifn[mc:c_fclosure]), ", "))
	]
      else if (class == mc:i_call)
        [
          dformat("%s := call %s%s", mc:svar(ins[mc:i_cdest]),
                  concat_words(lmap(mc:svar, ins[mc:i_cargs]), ", "),
                  if (mc:call_escapes?(ins)) " [escape]" else "");
          print_sz(ins[mc:i_csizeinfo]);
        ]
      else if (class == mc:i_trap)
	[
	  print_if(trap_names[ins[mc:i_top]], ins[mc:i_targs]);
	  dformat("trap %s", ins[mc:i_tdest]);
	]
      else if (class == mc:i_return)
	dformat("return %s", mc:svar(ins[mc:i_rvalue]))
      else if (class == mc:i_vref)
        dformat("%s := vref %s", mc:svar(ins[mc:i_vdest]),
                mc:svar(ins[mc:i_varg]))
      else
        fail();
      closures
    ];

  print_if = fn (op, args)
    if (op)
      [
	display("if ");
	print_op(op, args, false);
	display(" ");
      ];

  print_op = fn (op, args, call?)
    [
      | nargs |
      nargs = llength(args);
      if (call? || nargs != 2)
        [
          | prefix |
          if (slength(op))
            [
              display(op);
              prefix = " ";
            ]
          else
            prefix = "";
          lforeach(fn (arg) [
            dformat("%s%s", prefix, mc:svar(arg));
            prefix = " ";
          ], args);
        ]
      else
	dformat("%s %s %s", mc:svar(car(args)), op, mc:svar(cadr(args)))
    ];

  mc:slabel = fn (label)
    [
      | nlabel |
      while (nlabel = label[mc:l_alias]) label = nlabel;
      itoa(label[mc:l_number])
    ];

];

];
