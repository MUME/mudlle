/* 
 * Copyright (c) 1993-1999 David Gay
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

library msparc // mudlle assembler for sparc
// uses sparc: prefix
requires system, dlist, misc, sequences, compiler
defines
  sparc:l_ins, sparc:l_alias, sparc:l_number, sparc:il_label, sparc:il_ins,
  sparc:il_node, sparc:il_number, sparc:il_offset, sparc:i_type, sparc:i_op,
  sparc:i_arg1, sparc:i_arg2, sparc:i_arg3, sparc:ins_alu, sparc:ins_load,
  sparc:ins_store, sparc:ins_sethi, sparc:ins_save, sparc:ins_restore,
  sparc:ins_trap, sparc:ins_branch, sparc:ins_call, sparc:ins_jmpl,
  sparc:op_load_sbyte, sparc:op_load_shword, sparc:op_load_ubyte,
  sparc:op_load_uhword, sparc:op_load_word, sparc:op_store_byte,
  sparc:op_store_hword, sparc:op_store_word, sparc:op_cc, sparc:op_add,
  sparc:op_addc, sparc:op_udiv, sparc:op_sdiv, sparc:op_and, sparc:op_andn,
  sparc:op_or, sparc:op_orn, sparc:op_xor, sparc:op_xorn, sparc:op_umul,
  sparc:op_smul, sparc:op_sub, sparc:op_sll, sparc:op_srl, sparc:op_sra,
  sparc:nregs, sparc:reg_g0, sparc:reg_g1, sparc:reg_g2, sparc:reg_g3,
  sparc:reg_g4, sparc:reg_g5, sparc:reg_g6, sparc:reg_g7, sparc:reg_o0,
  sparc:reg_o1, sparc:reg_o2, sparc:reg_o3, sparc:reg_o4, sparc:reg_o5,
  sparc:reg_o6, sparc:reg_o7, sparc:reg_l0, sparc:reg_l1, sparc:reg_l2,
  sparc:reg_l3, sparc:reg_l4, sparc:reg_l5, sparc:reg_l6, sparc:reg_l7,
  sparc:reg_i0, sparc:reg_i1, sparc:reg_i2, sparc:reg_i3, sparc:reg_i4,
  sparc:reg_i5, sparc:reg_i6, sparc:reg_i7, sparc:balways, sparc:bnever,
  sparc:bne, sparc:beq, sparc:bgt, sparc:ble, sparc:bge, sparc:blt, sparc:bgtu,
  sparc:bleu, sparc:bgeu, sparc:bltu, sparc:bpos, sparc:bneg, sparc:bvc,
  sparc:bvs, sparc:new_code, sparc:set_instruction, sparc:add, sparc:addcc,
  sparc:addc, sparc:addccc, sparc:udiv, sparc:udivcc, sparc:sdiv, sparc:sdivcc,
  sparc:and, sparc:andcc, sparc:andn, sparc:andncc, sparc:or, sparc:orcc,
  sparc:orn, sparc:orncc, sparc:xor, sparc:xorcc, sparc:xorn, sparc:xorncc,
  sparc:umul, sparc:umulcc, sparc:smul, sparc:smulcc, sparc:sub, sparc:subcc,
  sparc:sll, sparc:srl, sparc:sra, sparc:load_sbyte, sparc:load_shword,
  sparc:load_ubyte, sparc:load_uhword, sparc:load_word, sparc:store_byte,
  sparc:store_hword, sparc:store_word, sparc:save, sparc:restore, 
  sparc:sethi, sparc:trap, sparc:branch, sparc:call_builtin, sparc:jmpl,
  sparc:register_constant, sparc:register_function, sparc:register_global,
  sparc:register_global_constant, sparc:register_primitive, sparc:new_label,
  sparc:label, sparc:set_label, sparc:ins_list, sparc:slabel, sparc:print_ins,
  sparc:set_leaf!, sparc:leaf?, sparc:make_leaf, sparc:get_instructions,
  sparc:rem_instruction, sparc:copy_instruction

[
// labels:

sparc:l_ins = 0; // instruction pointed to
sparc:l_alias = 1; // we are an alias to this label
sparc:l_number = 2; // unique number (for display)

// An instruction list is a list of the following vectors:

sparc:il_label = 0; // label of this instruction, or false
sparc:il_ins = 1; // the actual instruction
sparc:il_node = 2; // the basic block to which this instruction belongs
sparc:il_number = 3; // a unique number (for display)
sparc:il_offset = 4; // instruction offset (int)

// An instruction is a vector:
// (all instructins must represent legal sparc instructions)

sparc:i_type = 0; // instruction category
sparc:i_op = 1;   // actual op
sparc:i_arg1 = 2; // upto 3 arguments (unused ones set to null)
sparc:i_arg2 = 3;
sparc:i_arg3 = 4;


// the instruction types:
sparc:ins_alu = 0;
sparc:ins_load = 1;
sparc:ins_store = 2;
sparc:ins_sethi = 3;
sparc:ins_save = 4;
sparc:ins_restore = 5;
sparc:ins_trap = 6;
sparc:ins_branch = 7;
sparc:ins_call = 8;
sparc:ins_jmpl = 9;

sparc:op_load_sbyte = 9;
sparc:op_load_shword = 10;
sparc:op_load_ubyte = 1;
sparc:op_load_uhword = 2;
sparc:op_load_word = 0;

sparc:op_store_byte = 5;
sparc:op_store_hword = 6;
sparc:op_store_word = 4;

// alu ops
sparc:op_cc = 16;		// or with alu op to make cc version
sparc:op_add = 0;
sparc:op_addc = 8;
sparc:op_udiv = 14;
sparc:op_sdiv = 15;
sparc:op_and = 1;
sparc:op_andn = 5;
sparc:op_or = 2;
sparc:op_orn = 6;
sparc:op_xor = 3;
sparc:op_xorn = 7;
sparc:op_umul = 10;
sparc:op_smul = 11;
sparc:op_sub = 4;

// shifts (no cc versions)
sparc:op_sll = 37;
sparc:op_srl = 38;
sparc:op_sra = 39;

sparc:nregs = 32;
// pairs so that they may be distinguished from integer constants
sparc:reg_g0 = protect(0 . 0);
sparc:reg_g1 = protect(1 . 0);
sparc:reg_g2 = protect(2 . 0);
sparc:reg_g3 = protect(3 . 0);
sparc:reg_g4 = protect(4 . 0);
sparc:reg_g5 = protect(5 . 0);
sparc:reg_g6 = protect(6 . 0);
sparc:reg_g7 = protect(7 . 0);
sparc:reg_o0 = protect(8 . 0);
sparc:reg_o1 = protect(9 . 0);
sparc:reg_o2 = protect(10 . 0);
sparc:reg_o3 = protect(11 . 0);
sparc:reg_o4 = protect(12 . 0);
sparc:reg_o5 = protect(13 . 0);
sparc:reg_o6 = protect(14 . 0);
sparc:reg_o7 = protect(15 . 0);
sparc:reg_l0 = protect(16 . 0);
sparc:reg_l1 = protect(17 . 0);
sparc:reg_l2 = protect(18 . 0);
sparc:reg_l3 = protect(19 . 0);
sparc:reg_l4 = protect(20 . 0);
sparc:reg_l5 = protect(21 . 0);
sparc:reg_l6 = protect(22 . 0);
sparc:reg_l7 = protect(23 . 0);
sparc:reg_i0 = protect(24 . 0);
sparc:reg_i1 = protect(25 . 0);
sparc:reg_i2 = protect(26 . 0);
sparc:reg_i3 = protect(27 . 0);
sparc:reg_i4 = protect(28 . 0);
sparc:reg_i5 = protect(29 . 0);
sparc:reg_i6 = protect(30 . 0);
sparc:reg_i7 = protect(31 . 0);

// the value the sparc encoding !
sparc:balways = 8;
sparc:bnever = 0;
sparc:bne = 9;
sparc:beq = 1;
sparc:bgt = 10;
sparc:ble = 2;
sparc:bge = 11;
sparc:blt = 3;
sparc:bgtu = 12;
sparc:bleu = 4;
sparc:bgeu = 13;
sparc:bltu = 5;
sparc:bpos = 14;
sparc:bneg = 6;
sparc:bvc = 15;
sparc:bvs = 7;

[
  | ins_index, label_index, rnames, print_ins, rname, saddress, cnames, 
    smixed, check, register, load_ins, store_ins, alu_ins, okleaf, leafarg,
    remoffset, add_ins, duplicate |


  sparc:new_code = fn ()
    // Returns: Structure in which instructions can be generated
    [
      vector(null, false, null, null, null, null, null, null, false)
    ];

  sparc:set_leaf! = fn (fcode, leaf)
    // Effects: Set leaf status of fcode
    // Modifies: leaf
    fcode[8] = leaf;

  sparc:leaf? = fn (fcode)
    // Returns: true if fcode can be leaf
    fcode[8];

  sparc:set_instruction = fn "fncode ilist -> . Sets the current instruction insert position to ilist" (fcode, pos)
    [
      if (fcode[1]) fail();
      fcode[0] = pos;
    ];

  sparc:get_instructions = fn "fncode -> ilist. Returns instruction list of ilist" (fcode)
    [
      if (fcode[1]) fail();
      fcode[0]
    ];

  remoffset = fn (il, l) lfilter(fn (x) cdr(x) != il, l);

  sparc:rem_instruction = fn (fcode, il)
    [
      fcode[0] = dremove!(il, fcode[0]);
      il = dget(il);
      fcode[mc:a_builtins + 2] = remoffset(il, fcode[mc:a_builtins + 2]);
      fcode[mc:a_constants + 2] = remoffset(il, fcode[mc:a_constants + 2]);
      fcode[mc:a_subfns + 2] = remoffset(il, fcode[mc:a_subfns + 2]);
      fcode[mc:a_globals + 2] = remoffset(il, fcode[mc:a_globals + 2]);
      fcode[mc:a_kglobals + 2] = remoffset(il, fcode[mc:a_kglobals + 2]);
      fcode[mc:a_primitives + 2] = remoffset(il, fcode[mc:a_primitives + 2]);
    ];

  duplicate = fn (il, fcode, n)
    [
      | found |

      found = lexists?(fn (x) cdr(x) == il, fcode[n]);
      if (found)
	register(fcode, n, car(found));
    ];
  
  sparc:copy_instruction = fn (fcode, il)
    [
      add_ins(fcode, il[sparc:il_ins]);
      duplicate(il, fcode, mc:a_builtins + 2);
      duplicate(il, fcode, mc:a_constants + 2);
      duplicate(il, fcode, mc:a_subfns + 2);
      duplicate(il, fcode, mc:a_globals + 2);
      duplicate(il, fcode, mc:a_kglobals + 2);
      duplicate(il, fcode, mc:a_primitives + 2);
    ];
  
  // refs to g0-g7, l0-l1, i0-i7 and non-registers are ok in leaf routines.
  okleaf = fn (arg) !pair?(arg) || (arg = car(arg)) >= 24 || arg < 8 ||
    arg == 16 || arg == 17;

  ins_index = 0;
  add_ins = fn (fcode, ins)
    // Types: fcode : sparcode
    //        ins : instruction
    // Effects: Adds ins to the instructions in fcode, setting the label
    //   if necessary.
    //   Clears the current label
    [
      | newins, type |
      
      // Add instruction
      newins = vector(fcode[1], ins, null, ins_index = ins_index + 1, 0);

      // This is a strange hack:
      //   When code is initially generated, fcode[0] starts at null,
      //   then  gets set to the first instruction with subsequent
      //   instructions inserted before it (and because of the circular
      //   nature of the list, actually at the end).
      //   Later, when code needs patching, sparc:set_instruction is called
      //   to set the insertion point, before which new instructions are
      //   added.
      if (fcode[0] == null) fcode[0] = dcons!(newins, null)
      else dcons!(newins, fcode[0]); // insert before fcode[0]
      
      // Set label if any
      if (fcode[1]) fcode[1][sparc:l_ins] = newins;
      fcode[1] = false;

      // Clear leaf status ?
      type = ins[sparc:i_type];
      if (fcode[8] && type != sparc:ins_save)
	if (type == sparc:ins_restore) // arg3 can touch anything
	  fcode[8] = okleaf(ins[sparc:i_arg1]) && okleaf(ins[sparc:i_arg2])
	else
	  // clear leaf if touches o0-o7, l2-l7
	  fcode[8] = okleaf(ins[sparc:i_arg1]) &&
	    okleaf(ins[sparc:i_arg2]) && okleaf(ins[sparc:i_arg3]);
    ];

  register = fn (fcode, n, info)
    // Registers info associated with last instructin added in list fcode[n]
    fcode[n] = (info . dget(dprev(fcode[0]))) . fcode[n];
  
  // actual instructions:
  // subcc, or, load_word, save, restore, nop, sethi, store_word, store_halfword
  // branch, trap, call_builtin, ret

  check = fn (s)
    // s may be a register or a small constant
    [
      if (integer?(s)) assert(s >= -(1 << 12) && s < (1 << 12));
      s
    ];

  alu_ins = fn (op)
    fn (fcode, s1, s2, d)
      add_ins(fcode, vector(sparc:ins_alu, op, s1, check(s2), d));

  sparc:add = alu_ins(sparc:op_add);
  sparc:addcc = alu_ins(sparc:op_add | sparc:op_cc);
  sparc:addc = alu_ins(sparc:op_addc);
  sparc:addccc = alu_ins(sparc:op_addc | sparc:op_cc);
  sparc:udiv = alu_ins(sparc:op_udiv);
  sparc:udivcc = alu_ins(sparc:op_udiv | sparc:op_cc);
  sparc:sdiv = alu_ins(sparc:op_sdiv);
  sparc:sdivcc = alu_ins(sparc:op_sdiv | sparc:op_cc);
  sparc:and = alu_ins(sparc:op_and);
  sparc:andcc = alu_ins(sparc:op_and | sparc:op_cc);
  sparc:andn = alu_ins(sparc:op_andn);
  sparc:andncc = alu_ins(sparc:op_andn | sparc:op_cc);
  sparc:or = alu_ins(sparc:op_or);
  sparc:orcc = alu_ins(sparc:op_or | sparc:op_cc);
  sparc:orn = alu_ins(sparc:op_orn);
  sparc:orncc = alu_ins(sparc:op_orn | sparc:op_cc);
  sparc:xor = alu_ins(sparc:op_xor);
  sparc:xorcc = alu_ins(sparc:op_xor | sparc:op_cc);
  sparc:xorn = alu_ins(sparc:op_xorn);
  sparc:xorncc = alu_ins(sparc:op_xorn | sparc:op_cc);
  sparc:umul = alu_ins(sparc:op_umul);
  sparc:umulcc = alu_ins(sparc:op_umul | sparc:op_cc);
  sparc:smul = alu_ins(sparc:op_smul);
  sparc:smulcc = alu_ins(sparc:op_smul | sparc:op_cc);
  sparc:sub = alu_ins(sparc:op_sub);
  sparc:subcc = alu_ins(sparc:op_sub | sparc:op_cc);
  sparc:sll = alu_ins(sparc:op_sll);
  sparc:srl = alu_ins(sparc:op_srl);
  sparc:sra = alu_ins(sparc:op_sra);


  load_ins = fn (op)
    fn (fcode, s1, s2, d)
      add_ins(fcode, vector(sparc:ins_load, op, s1, check(s2), d));

  store_ins = fn (op)
    fn (fcode, d, s1, s2)
      add_ins(fcode, vector(sparc:ins_store, op, s1, check(s2), d));

  sparc:load_sbyte = load_ins(sparc:op_load_sbyte);
  sparc:load_shword = load_ins(sparc:op_load_shword);
  sparc:load_ubyte = load_ins(sparc:op_load_ubyte);
  sparc:load_uhword = load_ins(sparc:op_load_uhword);
  sparc:load_word = load_ins(sparc:op_load_word);

  sparc:store_byte = store_ins(sparc:op_store_byte);
  sparc:store_hword = store_ins(sparc:op_store_hword);
  sparc:store_word = store_ins(sparc:op_store_word);

  sparc:save = fn (fcode, s1, s2, d)
    add_ins(fcode, vector(sparc:ins_save, 0, s1, check(s2), d));

  sparc:restore = fn (fcode, s1, s2, d)
    add_ins(fcode, vector(sparc:ins_restore, 0, s1, check(s2), d));

  sparc:sethi = fn (fcode, n, d)
    [
      assert(0 <= n && n < (1 << 22));
      add_ins(fcode, vector(sparc:ins_sethi, 0, n, d, null));
    ];

  sparc:jmpl = fn (fcode, s1, s2, d)
    add_ins(fcode, vector(sparc:ins_jmpl, 0, s1, check(s2), d));

  sparc:trap = fn (fcode, cond, s1, s2)
    add_ins(fcode, vector(sparc:ins_trap, cond, s1, s2, null));

  sparc:branch = fn (fcode, cond, dest, annul)
    add_ins(fcode, vector(sparc:ins_branch, cond, dest, annul, null));

  sparc:call_builtin = fn (fcode, builtin)
    [
      sparc:set_leaf!(fcode, false);
      add_ins(fcode, vector(sparc:ins_call, 0, 0, null, null));
      register(fcode, mc:a_builtins + 2, builtin);
    ];


  sparc:register_constant = fn (fcode, cst)
    register(fcode, mc:a_constants + 2, cst);

  sparc:register_function = fn (fcode, f)
    register(fcode, mc:a_subfns + 2, f);

  sparc:register_global = fn (fcode, name)
    register(fcode, mc:a_globals + 2, name);

  sparc:register_global_constant = fn (fcode, name)
    register(fcode, mc:a_kglobals + 2, name);

  sparc:register_primitive = fn (fcode, info)
    register(fcode, mc:a_primitives + 2, info);

  leafarg = fn (arg)
    if (pair?(arg) && car(arg) >= 24) car(arg) - 16 . null // rename
    else arg;

  sparc:make_leaf = fn (ins)
    // Effects: Makes ins an instruction of a leaf routine, ie renames
    //   i<n> to o<n>, and transforms restore.
    [
      if (ins[sparc:i_type] == sparc:ins_restore)
	[
	  ins[sparc:i_type] = sparc:ins_alu;
	  ins[sparc:i_op] == sparc:op_add;
	  ins[sparc:i_arg1] = leafarg(ins[sparc:i_arg1]);
	  ins[sparc:i_arg2] = leafarg(ins[sparc:i_arg2]);
	]
      else
	[
	  ins[sparc:i_arg1] = leafarg(ins[sparc:i_arg1]);
	  ins[sparc:i_arg2] = leafarg(ins[sparc:i_arg2]);
	  ins[sparc:i_arg3] = leafarg(ins[sparc:i_arg3]);
	]
    ];

  // labels

  label_index = 0;
  sparc:new_label = fn "sparcode -> label. Returns a new unassigned label in sparcode"
    (fcode)
      vector(false, false, label_index = label_index + 1);
  
  sparc:label = fn "sparcode label -> . Makes label point at the next instruction to\n\
be generated in sparcode" (fcode, label)
      [
	if (fcode[1]) label[sparc:l_alias] = fcode[1]
	else fcode[1] = label;
      ];

  sparc:set_label = fn "label ilist -> . Sets label to point to ilist. Might make it an alias of existing label" (l, il)
    [
      | lab |

      if (lab = il[sparc:il_label]) // make it an alias
	[
	  l[sparc:l_alias] = lab;
	  l[sparc:l_ins] = false;
	]
      else
	[
	  l[sparc:l_ins] = il;
	  l[sparc:l_alias] = false;
	  il[sparc:il_label] = l;
	]
    ];


  // code display

  sparc:ins_list = fn "sparcode -> . Prints instruction list" (fcode)
    [
      | scan, ilist, ifind |
      
      ifind = fn (rlist, il, name) 
	lexists?(fn (rinfo)
	         if (cdr(rinfo) == il)
	           [
		     sparc:print_ins(il[sparc:il_ins], name, car(rinfo));
		     true
		   ]
		 else false,
	       rlist);

      ilist = fcode[0];
      scan = ilist;
      loop
	[
	  | il, ins |

	  il = dget(scan);
	  if (il[sparc:il_label])
	    display(format("%s:", sparc:slabel(il[sparc:il_label])));
	  display(format("\t(%s) ", il[sparc:il_number]));

	  ins = il[sparc:il_ins];
	  ins[sparc:i_type] == sparc:ins_sethi &&
	    // look for any associated info
	    (ifind(fcode[3], il, "constant") ||
	     ifind(fcode[4], il, "function") ||
	     ifind(fcode[5], il, "global") ||
	     ifind(fcode[6], il, "kglobal") ||
	     ifind(fcode[7], il, "primitive")) ||
	    ins[sparc:i_type] == sparc:ins_call && ifind(fcode[2], il, "builtin") ||
	    sparc:print_ins(ins, false, false);

	  newline();
	  scan = dnext(scan);
	  if (scan == ilist) exit 0
	]
    ];

  sparc:print_ins = fn (ins, info, val)
    [
      | class, op, opname, a1, a2, a3 |

      class = ins[sparc:i_type];
      op = ins[sparc:i_op];
      a1 = ins[sparc:i_arg1];
      a2 = ins[sparc:i_arg2];
      a3 = ins[sparc:i_arg3];

      opname = 
	if (class == sparc:ins_alu)
	  if (op >= sparc:op_cc + 16)
	    '[0 0 0 0 0 "sll" "srl" "sra"][op - 32]
	  else
	    format("%s%s", 
		   '["add" "and" "or" "xor"
		     "sub" "andn" "orn" "xorn"
		     "addc" 0 "umul" "smul"
		     0 0 "udiv" "sdiv"][op & 15],
		   if (op & sparc:op_cc) "cc" else "")
	else if (class == sparc:ins_load)
	  format("ld%s", '["w" "ub" "uh" 0 0 0 0 0 0 "sb" "sh"][op])
	else if (class == sparc:ins_store)
	  format("st%s", '[0 0 0 0 "w" "h" "b"][op])
	else if (class == sparc:ins_trap)
	  format("t%s", cnames[op])
	else if (class == sparc:ins_branch)
	  format("b%s%s", cnames[op], if (a2) ",a" else "")
	else
	  '[0 0 0 "sethi" "save" "restore" 0 0 "call" "jmpl"][class];

      display(format("%s%s", opname,
		     [ | f | f = make_string(10 - string_length(opname));
		       string_fill!(f, ? ); f ] ));

      if (class == sparc:ins_alu ||
	  class == sparc:ins_save || class == sparc:ins_restore)
        display(format("%s,%s,%s", rname(a1), smixed(a2), rname(a3)))
      else if (class == sparc:ins_load || class == sparc:ins_jmpl)
        display(format("%s,%s", saddress(a1, a2), rname(a3)))
      else if (class == sparc:ins_store)
        display(format("%s,%s", rname(a3), saddress(a1, a2)))
      else if (class == sparc:ins_call)
        if (info) display(format("%s[%s]", info, val))
	else display(a1)
      else if (class == sparc:ins_sethi)
        display(format("%s,%s",
		       if (info) format("%%hi(%s[%s])", info, val) else a1,
		       rname(a2)))
      else if (class == sparc:ins_trap)
        display(saddress(a1, a2))
      else if (class == sparc:ins_branch)
        display(sparc:slabel(a1))
    ];

  cnames = '["n" "e" "le" "l" "leu" "cs" "neg" "vs"
	     "a" "ne" "g" "ge" "gu" "cc" "pos" "vc"];

  rnames = '["%g0" "%g1" "%g2" "%g3" "%g4" "%g5" "%g6" "%g7"
	     "%o0" "%o1" "%o2" "%o3" "%o4" "%o5" "%o6" "%o7"
	     "%l0" "%l1" "%l2" "%l3" "%l4" "%l5" "%l6" "%l7"
	     "%i0" "%i1" "%i2" "%i3" "%i4" "%i5" "%i6" "%i7"];

  rname = fn (r) rnames[car(r)];

  smixed = fn (s) if (integer?(s)) itoa(s) else rname(s);

  saddress = fn (s1, s2)
    rname(s1) + "+" + smixed(s2);

  sparc:slabel = fn (label)
    [
      | nlabel |
      while (nlabel = label[sparc:l_alias]) label = nlabel;
      itoa(label[sparc:l_number])
    ];
];

];
