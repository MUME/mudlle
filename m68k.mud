/* 
 * Copyright (c) 1993-2006 David Gay
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

library m68k // mudlle assembler for 68k
requires system, compiler, dlist, vars, misc
defines // uses m68k: prefix
  m68k:l_ins, m68k:l_alias, m68k:l_number, m68k:il_label, m68k:il_ins,
  m68k:il_node, m68k:il_number, m68k:il_offset, m68k:i_type, m68k:i_size,
  m68k:i_arg1, m68k:i_arg2, m68k:i_cond, m68k:i_label, m68k:ea_class,
  m68k:er_reg, m68k:ei_reg, m68k:ei_offset, m68k:ek_value, m68k:ec_value,
  m68k:eb_builtin, m68k:ef_function, m68k:eg_name, m68k:ins_compare,
  m68k:ins_jump, m68k:ins_jsr, m68k:ins_branch, m68k:ins_exchange, m68k:ins_rts,
  m68k:ins_move, m68k:ins_and, m68k:ins_clr, m68k:ins_add, m68k:ins_subtract,
  m68k:lregister, m68k:limmediate, m68k:lindexed, m68k:lvar, m68k:lbuiltin,
  m68k:lfunction, m68k:lconstant, m68k:lglobal, m68k:lglobal_offset,
  m68k:lglobal_constant, m68k:sbyte, m68k:sword, m68k:slongword, m68k:nregs,
  m68k:reg_d0, m68k:reg_d1, m68k:reg_d2, m68k:reg_d3, m68k:reg_d4, m68k:reg_d5,
  m68k:reg_d6, m68k:reg_d7, m68k:reg_a0, m68k:reg_a1, m68k:reg_a2, m68k:reg_a3,
  m68k:reg_a4, m68k:reg_a5, m68k:reg_a6, m68k:reg_a7, m68k:reg_closure,
  m68k:reg_spill, m68k:reg_globals, m68k:reg_args, m68k:balways, m68k:bnever,
  m68k:bhi, m68k:bls, m68k:bcc, m68k:bcs, m68k:bne, m68k:beq, m68k:bnv,
  m68k:bov, m68k:bpl, m68k:bmi, m68k:bge, m68k:blt, m68k:bgt, m68k:ble,

  m68k:new_code, m68k:set_instruction, m68k:rts, m68k:andimm, m68k:and,
  m68k:compareimm, m68k:compare, m68k:add, m68k:subtract, m68k:jump, m68k:jsr,
  m68k:exchange, m68k:branch, m68k:move, m68k:new_label, m68k:label,
  m68k:set_label, m68k:ins_list, m68k:print_ins, m68k:slabel

reads m68k:object_offset

[
// labels:

m68k:l_ins = 0; // instruction pointed to
m68k:l_alias = 1; // we are an alias to this label
m68k:l_number = 2; // unique number (for display)

// An instruction list is a list of the following vectors:

m68k:il_label = 0; // label of this instruction, or false
m68k:il_ins = 1; // the actual instruction
m68k:il_node = 2; // the basic block to which this instruction belongs
m68k:il_number = 3; // a unique number (for display)
m68k:il_offset = 4; // instruction offset (int)

// An instruction is a vector:
// (all instructins must represent legal 68k instructions)

m68k:i_type = 0; // type of instruction (add, subtract, etc)
m68k:i_size = 1; // size of instruction (byte, word, longword)
m68k:i_arg1 = 2; // most instructions: 1 or 2 arguments 
m68k:i_arg2 = 3; // (effective address-style)
m68k:i_cond = 2; // branch condition
m68k:i_label = 3; // branch label

// an effective address
m68k:ea_class = 0; // type of ea 

// lregister
 m68k:er_reg = 1; // register number

// lindexed
 m68k:ei_reg = 1;
 m68k:ei_offset = 2;

// limmediate (integer constant)
 m68k:ek_value = 1;

// the following are pseudo-ea's understood by the C "assembler"
// lconstant (mudlle constants, no integers)
 m68k:ec_value = 1;

// lbuiltin (builtin op)
 m68k:eb_builtin = 1;

// lfunction (another function)
 m68k:ef_function = 1;

// lglobal_{,check,constant} (global variable)
 m68k:eg_name = 1;

// the instruction types:
m68k:ins_compare = 0;
m68k:ins_jump = 1;
m68k:ins_jsr = 2;
m68k:ins_branch = 3;
m68k:ins_exchange = 4;
m68k:ins_rts = 5;
m68k:ins_move = 6;
m68k:ins_and = 7;
m68k:ins_clr = 8;
m68k:ins_add = 9;
m68k:ins_subtract = 10;

m68k:lregister = 0;
m68k:limmediate = 1;
m68k:lindexed = 2;
m68k:lvar = 3;
m68k:lbuiltin = 4;
m68k:lfunction = 5;
m68k:lconstant = 6;
m68k:lglobal = 7;
m68k:lglobal_offset = 8;
m68k:lglobal_constant = 9;

m68k:sbyte = 0;
m68k:sword = 1;
m68k:slongword = 2;

m68k:nregs = 16;
m68k:reg_d0 = 0;
m68k:reg_d1 = 1;
m68k:reg_d2 = 2;
m68k:reg_d3 = 3;
m68k:reg_d4 = 4;
m68k:reg_d5 = 5;
m68k:reg_d6 = 6;
m68k:reg_d7 = 7;
m68k:reg_a0 = 8;
m68k:reg_a1 = 9;
m68k:reg_a2 = 10;
m68k:reg_a3 = 11;
m68k:reg_a4 = 12;
m68k:reg_a5 = 13;
m68k:reg_a6 = 14;
m68k:reg_a7 = 15;

m68k:reg_closure = m68k:reg_a4;
m68k:reg_spill = m68k:reg_a5;
m68k:reg_globals = m68k:reg_a6;
m68k:reg_args = m68k:reg_a3;

// this order matches the 68k encoding !
m68k:balways = 0;
m68k:bnever = 1;
m68k:bhi = 2;
m68k:bls = 3;
m68k:bcc = 4;
m68k:bcs = 5;
m68k:bne = 6;
m68k:beq = 7;
m68k:bnv = 8;
m68k:bov = 9;
m68k:bpl = 10;
m68k:bmi = 11;
m68k:bge = 12;
m68k:blt = 13;
m68k:bgt = 14;
m68k:ble = 15;

[
  | ins_index, add_ins, label_index, spillreg, make_ea, rnames, print_ea |


  m68k:new_code = fn "component -> 68kcode. Returns a structure to use for\n\
generating 68k instructions" ()
    [
      vector(null, false)
    ];

  m68k:set_instruction = fn "fncode ilist -> . Sets the current instruction insert position to ilist" (fcode, pos)
    [
      if (fcode[1]) fail();
      fcode[0] = pos;
    ];

  
  ins_index = 0;
  add_ins = fn (fcode, ins)
    // Types: fcode : 68kcode
    //        ins : instruction
    // Effects: Adds ins to the instructions in fcode, setting the label
    //   if necessary.
    //   Clears the current label
    [
      | newins |
      
      // Add instruction
      newins = vector(fcode[1], ins, null, ins_index = ins_index + 1, 0);

      // This is a strange hack:
      //   When code is initially generated, fcode[0] starts at null,
      //   then  gets set to the first instruction with subsequent
      //   instructions inserted before it (and because of the circular
      //   nature of the list, actually at the end).
      //   Later, when code needs patching, m68k:set_instruction is called
      //   to set the insertion point, before which new instructions are
      //   added.
      if (fcode[0] == null) fcode[0] = dcons!(newins, null)
      else dcons!(newins, fcode[0]); // insert before fcode[0]
      
      // Set label if any
      if (fcode[1]) fcode[1][m68k:l_ins] = newins;
      fcode[1] = false;
    ];

  spillreg = vector(m68k:reg_closure, m68k:reg_args, m68k:reg_spill);

  make_ea = fn (type, arg)
    if (type == m68k:lindexed) vector(type, car(arg), cdr(arg))
    else if (type == m68k:lvar)
      [
	| loc |

	loc = arg[mc:v_location];
	if (loc)
	  if (loc[mc:v_lclass] == mc:v_lregister)
	    vector(m68k:lregister, loc[mc:v_lrnumber])
	  else
	    vector(m68k:lindexed, spillreg[loc[mc:v_lstype]],
		   m68k:object_offset + 4 * loc[mc:v_lsoffset])
	else // no location, global or constant
	  if (arg[mc:v_class] == mc:v_constant)
	    [
	      | val |

	      val = arg[mc:v_kvalue];

	      if (integer?(val) && val >= -(1 << 29) && val < (1 << 29))
		vector(m68k:limmediate, 1 | val << 1)
	      else
		vector(m68k:lconstant, val)
	    ]
	  else if (arg[mc:v_class] == mc:v_global ||
		   arg[mc:v_class] == mc:v_global_define)
	    vector(m68k:lglobal, arg[mc:v_name])
	  else if (arg[mc:v_class] == mc:v_global_constant)
	    // immutable constants can be inlined
	    if (immutable?(global_value(arg[mc:v_goffset])))
	      vector(m68k:lglobal_constant, arg[mc:v_name])
	    else
	      vector(m68k:lglobal, arg[mc:v_name])
	  else fail()
      ]
    else vector(type, arg);
  
  // actual instructions:
  // and, andimm, compareimm, jump, move, branch, compare, jsr, exchange, rts

  m68k:rts = fn (fcode)
    add_ins(fcode, vector(m68k:ins_rts, m68k:slongword, null, null));
			  
  m68k:andimm = fn (fcode, size, val, eatype, earg)
    add_ins(fcode, vector(m68k:ins_and, size,
			  make_ea(m68k:limmediate, val),
			  make_ea(eatype, earg)));

  m68k:and = fn (fcode, size, reg, eatype, earg)
    add_ins(fcode, vector(m68k:ins_and, size,
			  make_ea(eatype, earg),
			  make_ea(m68k:lregister, reg)));

  m68k:compareimm = fn (fcode, size, val, eatype, earg)
    add_ins(fcode, vector(m68k:ins_compare, size,
			  make_ea(m68k:limmediate, val),
			  make_ea(eatype, earg)));

  m68k:compare = fn (fcode, size, reg, eatype, earg)
    add_ins(fcode, vector(m68k:ins_compare, size,
			  make_ea(eatype, earg),
			  make_ea(m68k:lregister, reg)));

  m68k:add = fn (fcode, size, reg, eatype, earg)
    add_ins(fcode, vector(m68k:ins_add, size,
			  make_ea(eatype, earg),
			  make_ea(m68k:lregister, reg)));

  m68k:subtract = fn (fcode, size, reg, eatype, earg)
    add_ins(fcode, vector(m68k:ins_subtract, size,
			  make_ea(eatype, earg),
			  make_ea(m68k:lregister, reg)));

  m68k:jump = fn (fcode, eatype, earg)
    add_ins(fcode, vector(m68k:ins_jump, m68k:slongword,
			  make_ea(eatype, earg), null));

  m68k:jsr = fn (fcode, eatype, earg)
    add_ins(fcode, vector(m68k:ins_jsr, m68k:slongword,
			  make_ea(eatype, earg), null));

  m68k:exchange = fn (fcode, r1, r2)
    add_ins(fcode, vector(m68k:ins_exchange, m68k:slongword,
			  make_ea(m68k:lregister, r1),
			  make_ea(m68k:lregister, r2)));

  m68k:branch = fn (fcode, op, label)
    add_ins(fcode, vector(m68k:ins_branch, m68k:sbyte, op, label));

  m68k:move = fn (fcode, size, eatype1, earg1, eatype2, earg2)
    [
      | ea1, ea2 |

      ea2 = make_ea(eatype2, earg2);

      if (eatype1 == m68k:limmediate && earg1 == 0 ||
	  eatype1 == m68k:lconstant && earg1 == null ||
	  eatype1 == m68k:lvar && earg1[mc:v_class] == mc:v_constant &&
	  earg1[mc:v_kvalue] == null)
	add_ins(fcode, vector(m68k:ins_clr, size, ea2, null))
      else
	[
	  ea1 = make_ea(eatype1, earg1);
	  if (!vequal?(ea1, ea2))
	    add_ins(fcode, vector(m68k:ins_move, size, ea1, ea2));
	]
    ];

  // labels

  label_index = 0;
  m68k:new_label = fn "68kcode -> label. Returns a new unassigned label in 68kcode"
    (fcode)
      vector(false, false, label_index = label_index + 1);
  
  m68k:label = fn "68kcode label -> . Makes label point at the next instruction to\n\
be generated in 68kcode" (fcode, label)
      [
	if (fcode[1]) label[m68k:l_alias] = fcode[1]
	else fcode[1] = label;
      ];

  m68k:set_label = fn "label ilist -> . Sets label to point to ilist. Might make it an alias of existing label" (l, il)
    [
      | lab |

      if (lab = il[m68k:il_label]) // make it an alias
	[
	  l[m68k:l_alias] = lab;
	  l[m68k:l_ins] = false;
	]
      else
	[
	  l[m68k:l_ins] = il;
	  il[m68k:il_label] = l;
	]
    ];


  // code display

  m68k:ins_list = fn "68kcode -> . Prints instruction list" (fcode)
    [
      | scan, ilist |
      
      ilist = fcode[0];
      scan = ilist;
      loop
	[
	  | il |
	  il = dget(scan);
	  if (il[m68k:il_label])
	    display(format("%s:", m68k:slabel(il[m68k:il_label])));
	  display(format("\t(%s) ", il[m68k:il_number]));
	  m68k:print_ins(il[m68k:il_ins]);
	  newline();
	  scan = dnext(scan);
	  if (scan == ilist) exit 0
	]
    ];

  m68k:print_ins = fn (ins)
    [
      | class |
      class = ins[m68k:i_type];
      display(format("%s.%s ", '["cmp" "jmp" "jsr" "bra" "exg" "rts" "move" "and" "clr" "add" "sub"][class],
		     '["b" "w" "l"][ins[m68k:i_size]]));
      if (class == m68k:ins_compare || class == m68k:ins_and ||
	  class == m68k:ins_exchange || class == m68k:ins_move ||
	  class == m68k:ins_add || class == m68k:ins_subtract)
	[
	  print_ea(ins[m68k:i_arg1]);
	  display(", ");
	  print_ea(ins[m68k:i_arg2]);
	]
      else if (class == m68k:ins_jump || class == m68k:ins_jsr || class == m68k:ins_clr)
	print_ea(ins[m68k:i_arg1])
      else if (class == m68k:ins_branch)
	display(format("%s,%s",
		       '["always" "never" "hi" "ls" "cc" "cs" "ne" "eq" "nv" "ov"
			 "pl" "mi" "ge" "lt" "gt" "le"][ins[m68k:i_cond]],
		       m68k:slabel(ins[m68k:i_label])));
    ];

  rnames = '["d0" "d1" "d2" "d3" "d4" "d5" "d6" "d7"
	     "a0" "a1" "a2" "a3" "a4" "a5" "a6" "a7"];
  print_ea = fn (ea)
    [
      | class |

      class = ea[m68k:ea_class];

      if (class == m68k:lregister)
        display(rnames[ea[m68k:er_reg]])
      else if (class == m68k:lindexed)
	display(format("%s(%s)", ea[m68k:ei_offset], rnames[ea[m68k:ei_reg]]))
      else if (class == m68k:limmediate)
	display(format("#%s", ea[m68k:ek_value]))
      else if (class == m68k:lconstant)
	display(format("'%s", ea[m68k:ec_value]))
      else if (class == m68k:lbuiltin)
	display(format("builtin[%s]", ea[m68k:eb_builtin]))
      else if (class == m68k:lfunction)
        display(format("function[%s]", ea[m68k:ef_function][mc:c_fnumber]))
      else if (class == m68k:lglobal)
	display(format("global[%s]", ea[m68k:eg_name]))
      else if (class == m68k:lglobal_offset)
	display(format("wglobal[%s]", ea[m68k:eg_name]))
      else if (class == m68k:lglobal_constant)
	display(format("kglobal[%s]", ea[m68k:eg_name]))
      else [ display(class); newline() ]
    ];

  m68k:slabel = fn (label)
    [
      | nlabel |
      while (nlabel = label[m68k:l_alias]) label = nlabel;
      itoa(label[m68k:l_number])
    ];
];

];
