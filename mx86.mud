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

library mx86 // mudlle assembler for x86
// uses x86: prefix
requires system, dlist, misc, sequences, vars

defines x86:l_ins, x86:l_alias, x86:l_number, x86:il_label, x86:il_ins,
  x86:il_node, x86:il_number, x86:il_offset, x86:i_op, x86:i_arg1, x86:i_arg2,
  x86:lvar, x86:lreg, x86:lidx, x86:lridx, x86:limm, x86:lcst, x86:lfunction,
  x86:lglobal, x86:lglobal_constant, x86:lprimitive, x86:lspecial, x86:nregs,
  x86:reg_eax, x86:reg_ebx, x86:reg_ecx, x86:reg_edx, x86:reg_esp, x86:reg_ebp,
  x86:reg_esi, x86:reg_edi, x86:reg_al, x86:reg_bl, x86:reg_cl, x86:reg_dl,
  x86:reg_ah, x86:reg_bh, x86:reg_ch, x86:reg_dh, x86:reg8, x86:bne, x86:be,
  x86:bg, x86:ble, x86:bge, x86:bl, x86:ba, x86:bbe, x86:bae, x86:bb, x86:bno,
  x86:bo, x86:bns, x86:bs, x86:bnp, x86:bp, x86:balways, x86:op_push,
  x86:op_pop, x86:op_call, x86:op_ret, x86:op_jmp, x86:op_jcc, x86:op_lea,
  x86:op_mov, x86:op_add, x86:op_sub, x86:op_cmp, x86:op_cmpbyte, x86:op_or,
  x86:op_xor, x86:op_and, x86:op_andbyte, x86:op_test, x86:op_inc, x86:op_dec,
  x86:op_neg, x86:op_not, x86:op_bt, x86:op_bts, x86:op_btr, x86:op_btc,
  x86:op_shl, x86:op_shr, x86:op_setcc, x86:op_movzxbyte, x86:op_xchg,
  x86:new_code, x86:set_instruction, x86:get_instructions, x86:rem_instruction,
  x86:copy_instruction, x86:mudlleint, x86:doubleint, x86:push, x86:pop,
  x86:call, x86:ret, x86:jmp, x86:jcc, x86:lea, x86:mov, x86:add, x86:sub,
  x86:cmp, x86:cmpbyte, x86:or, x86:xor, x86:and, x86:andbyte, x86:test,
  x86:inc, x86:dec, x86:neg, x86:not, x86:bt, x86:btr, x86:bts, x86:btc,
  x86:shl, x86:shr, x86:setcc, x86:movzxbyte, x86:xchg, x86:new_label,
  x86:label, x86:set_label, x86:ins_list, x86:print_ins, x86:resolve, x86:trap,
  x86:op_jmp32, x86:op_jcc32, x86:callrel, x86:op_callrel, x86:sar, x86:op_sar,
  x86:op_op16, x86:op16, x86:leave, x86:op_leave, x86:lqidx, x86:adc, 
  x86:op_adc

reads x86:spillreg
[
// labels:

x86:l_ins = 0; // instruction pointed to
x86:l_alias = 1; // we are an alias to this label
x86:l_number = 2; // unique number (for display)

// An instruction list is a list of the following vectors:

x86:il_label = 0; // label of this instruction, or false
x86:il_ins = 1; // the actual instruction
x86:il_node = 2; // the basic block to which this instruction belongs
x86:il_number = 3; // a unique number (for display)
x86:il_offset = 4; // instruction offset (int)

// An instruction is a vector:
// (all instructins must represent legal x86 instructions)

x86:i_op = 0;   // op
x86:i_arg1 = 1; // upto 2 arguments (unused ones set to null)
x86:i_arg2 = 2;

// arguments are pairs: mode . arguments, except for labels and ccs
// modes/arguments are:
//   x86:lreg: register
//   x86:lidx: register . offset
//   x86:lridx: register1 . scale . register2 . disp 
//     (register1 * scale + register2 + disp)
//   x86:lqidx: register1 . scale . disp 
//     (register1 * scale + disp)
//   x86:limm: immediate
//   x86:lcst: mudlle constant
//   x86:lfunction: magic
//   x86:lglobal: global name
//   x86:lglobal_constant: global name
//   x86:lprimitive: primitive name
//   x86:lspecial: name (ref to C vars)
// accepted on input, but converted: lvar: variable
// arguments to x86:limm are integers or pairs: x . 0 for 2*x and x . 1 for 2*x+1
// (to work around 31-bit integer limitations)
// each instruction can contains at most one non-x86:lreg argument
x86:lvar = 0;
x86:lreg = 1;
x86:lidx = 2;
x86:lridx = 3;
x86:lqidx = 11;
x86:limm = 4;
x86:lcst = 5;
x86:lfunction = 6;
x86:lglobal = 7;
x86:lglobal_constant = 8;
x86:lprimitive = 9;
x86:lspecial = 10;

x86:nregs = 8;

x86:reg_eax = 0;
x86:reg_ebx = 3;
x86:reg_ecx = 1;
x86:reg_edx = 2;
x86:reg_esp = 4;
x86:reg_ebp = 5;
x86:reg_esi = 6;
x86:reg_edi = 7;

x86:reg_al = 0;
x86:reg_bl = 3;
x86:reg_cl = 1;
x86:reg_dl = 2;
x86:reg_ah = 4;
x86:reg_bh = 7;
x86:reg_ch = 5;
x86:reg_dh = 6;

x86:reg8 = sequence(x86:reg_al, x86:reg_cl, x86:reg_dl, x86:reg_bl);

// branches, using x86 encoding
x86:bne = 5;
x86:be = 4;

x86:bg = 15;
x86:ble = 14;
x86:bge = 13;
x86:bl = 12;

x86:ba = 7;
x86:bbe = 6;
x86:bae = 3;
x86:bb = 2;

x86:bno = 1;
x86:bo = 0;
x86:bns = 9;
x86:bs = 8;
x86:bnp = 11;
x86:bp = 10;

x86:balways = -1; // special value, used for x86:trap

// operations
x86:op_push = 0;
x86:op_pop = 1;
x86:op_leave = 35;

x86:op_call = 2;
x86:op_callrel = 32;
x86:op_ret = 3;
x86:op_jmp = 4;
x86:op_jcc = 5;
x86:op_jmp32 = 30;
x86:op_jcc32 = 31;

x86:op_lea = 6;
x86:op_mov = 7;

x86:op_add = 8;
x86:op_adc = 36;
x86:op_sub = 9;
x86:op_cmp = 10;
x86:op_cmpbyte = 11;
x86:op_or = 12;
x86:op_xor = 13;
x86:op_and = 14;
x86:op_andbyte = 15;
x86:op_test = 16;

x86:op_inc = 17;
x86:op_dec = 18;
x86:op_neg = 19;
x86:op_not = 20;

x86:op_bt = 21;
x86:op_bts = 22;
x86:op_btr = 23;
x86:op_btc = 24;

x86:op_shl = 25;
x86:op_shr = 26;
x86:op_sar = 33;
x86:op_setcc = 27;
x86:op_movzxbyte = 28;
x86:op_xchg = 29;
x86:op_op16 = 34; // generate the operand size prefix

[
  | ins_index, label_index, rnames32, rnames8, cnames, mode, eastr, slabel,
    opname, add_ins, generic_op0, generic_op1, generic_op2 |


  x86:new_code = fn ()
    // Returns: Structure in which instructions can be generated
    [
      vector(null, false, null)
      // 0: insertion position
      // 1: label for next instruction (false for none)
      // 2: list of error handlers (error number . label)
    ];

  x86:set_instruction = fn "fncode ilist -> . Sets the current instruction insert position to ilist" (fcode, pos)
    [
      if (fcode[1]) fail();
      fcode[0] = pos;
    ];

  x86:get_instructions = fn "fncode -> ilist. Returns instruction list of ilist" (fcode)
    [
      if (fcode[1]) fail();
      fcode[0]
    ];

  x86:rem_instruction = fn (fcode, il)
    [
      fcode[0] = dremove!(il, fcode[0]);
    ];

  x86:copy_instruction = fn (fcode, il)
    [
      add_ins(fcode, il[x86:il_ins]);
    ];
  
  ins_index = 0;
  add_ins = fn (fcode, ins)
    // Types: fcode : x86code
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
      //   Later, when code needs patching, x86:set_instruction is called
      //   to set the insertion point, before which new instructions are
      //   added.
      if (fcode[0] == null) fcode[0] = dcons!(newins, null)
      else dcons!(newins, fcode[0]); // insert before fcode[0]
      
      // Set label if any
      if (fcode[1]) fcode[1][x86:l_ins] = newins;
      fcode[1] = false;
    ];

  x86:resolve = fn (type, arg)
    [
      if (type == x86:lvar)
	[
	  | loc |
	  
	  if (mc:in_reg(arg))
	    x86:lreg . mc:get_reg(arg)
	  else if (loc = arg[mc:v_location])
	    x86:lidx . (x86:spillreg[loc[mc:v_lstype]] . loc[mc:v_lsoffset])
	  else			// no location: global or constant
	    if (arg[mc:v_class] == mc:v_constant)
	      x86:resolve(x86:lcst, arg[mc:v_kvalue])
	    else if (arg[mc:v_class] == mc:v_global ||
		     arg[mc:v_class] == mc:v_global_define)
	      x86:lglobal . arg[mc:v_name]
	    else if (arg[mc:v_class] == mc:v_global_constant)
	      if (immutable?(global_value(arg[mc:v_goffset])))
		x86:lglobal_constant . arg[mc:v_name]
	      else
		x86:lglobal . arg[mc:v_name]
	    else fail()
	]
      else if (type == x86:lcst && integer?(arg))
	x86:limm . x86:mudlleint(arg)
      else
	type . arg
    ];
  
  x86:mudlleint = fn (x) x . 1;
  x86:doubleint = fn (x) x . 0;

  // actual instructions

  generic_op0 = fn (op)
    fn (fcode)
      add_ins(fcode, vector(op, null, null));

  generic_op1 = fn (op)
    fn (fcode, m1, a1)
      add_ins(fcode, vector(op, x86:resolve(m1, a1), null));

  generic_op2 = fn (op)
    fn (fcode, m1, a1, m2, a2)
      add_ins(fcode, vector(op, x86:resolve(m1, a1), x86:resolve(m2, a2)));

  x86:push = generic_op1(x86:op_push);
  x86:pop = generic_op1(x86:op_pop);
  x86:leave = generic_op0(x86:op_leave);

  x86:call = generic_op1(x86:op_call);
  x86:callrel = fn (fcode, builtin)
    add_ins(fcode, vector(x86:op_callrel, builtin, null));
  x86:ret = generic_op0(x86:op_ret);
  x86:jmp = fn (fcode, l)
    add_ins(fcode, vector(x86:op_jmp, l, null));
  x86:jcc = fn (fcode, cc, l)
    add_ins(fcode, vector(x86:op_jcc, l, cc));

  x86:lea = generic_op2(x86:op_lea); // dest must be reg
  x86:mov = generic_op2(x86:op_mov);

  x86:add = generic_op2(x86:op_add);
  x86:adc = generic_op2(x86:op_adc);
  x86:sub = generic_op2(x86:op_sub);
  x86:cmp = generic_op2(x86:op_cmp);
  x86:cmpbyte = generic_op2(x86:op_cmpbyte);
  x86:or = generic_op2(x86:op_or);
  x86:xor = generic_op2(x86:op_xor);
  x86:and = generic_op2(x86:op_and);
  x86:andbyte = generic_op2(x86:op_andbyte);
  x86:test = generic_op2(x86:op_test);

  x86:inc = generic_op1(x86:op_inc);
  x86:dec = generic_op1(x86:op_dec);
  x86:neg = generic_op1(x86:op_neg);
  x86:not = generic_op1(x86:op_not);

  x86:bt = generic_op2(x86:op_bt);
  x86:btr = generic_op2(x86:op_btr);
  x86:bts = generic_op2(x86:op_bts);
  x86:btc = generic_op2(x86:op_btc);

  x86:shl = generic_op2(x86:op_shl); // many restrictions on arg1
  x86:shr = generic_op2(x86:op_shr); // many restrictions on arg1
  x86:sar = generic_op2(x86:op_sar); // many restrictions on arg1
  x86:setcc = fn (fcode, cc, m1, a1)
    add_ins(fcode, vector(x86:op_setcc, cc, x86:resolve(m1, a1)));

  x86:movzxbyte = generic_op2(x86:op_movzxbyte); // dest must be register
  x86:xchg = generic_op2(x86:op_xchg); // dest must be register
  x86:op16 = generic_op0(x86:op_op16);

  // labels

  label_index = 0;
  x86:new_label = fn "x86code -> label. Returns a new unassigned label in x86code"
    (fcode)
      vector(false, false, label_index = label_index + 1);
  
  x86:label = fn "x86code label -> . Makes label point at the next instruction to\n\
be generated in x86code" (fcode, label)
      [
	if (fcode[1]) label[x86:l_alias] = fcode[1]
	else fcode[1] = label;
      ];

  x86:set_label = fn "label ilist -> . Sets label to point to ilist. Might make it an alias of existing label" (l, il)
    [
      | lab |

      if (lab = il[x86:il_label]) // make it an alias
	[
	  l[x86:l_alias] = lab;
	  l[x86:l_ins] = false;
	]
      else
	[
	  l[x86:l_ins] = il;
	  l[x86:l_alias] = false;
	  il[x86:il_label] = l;
	]
    ];

  // traps

  x86:trap = fn "x86code cc n -> Cause error n if cc is true " (fcode, cc, n)
    [
      | l |

      if ((l = lexists?(fn (trap) car(trap) == n, fcode[2])))
	l = cdr(l)
      else
	[
	  // new trap
	  l = x86:new_label(fcode);
	  fcode[2] = (n . l) . fcode[2];
	];

      if (cc == x86:balways)
	x86:jmp(fcode, l)
      else
	x86:jcc(fcode, cc, l);
    ];

  // code display

  x86:ins_list = fn "x86code -> . Prints instruction list" (fcode)
    [
      | scan, ilist, ifind |
      
      ilist = fcode[0];
      scan = ilist;
      loop
	[
	  | il |

	  il = dget(scan);
	  if (il[x86:il_label])
	    display(format("%s:", slabel(il[x86:il_label])));
	  display(format("\t(%s) ", il[x86:il_number]));

	  x86:print_ins(il[x86:il_ins]);

	  newline();
	  scan = dnext(scan);
	  if (scan == ilist) exit 0
	];
      lforeach(fn (n) display(format("error %s: label %s%n", car(n), slabel(cdr(n)))),
	       fcode[2]);
    ];

  opname = '["push" "pop"
	     "call" "ret" "jmp" "jcc"
	     "lea" "mov"
	     "add" "sub" "cmp" "cmp8" "or" "xor" "and" "and8" "test"
	     "inc" "dec" "neg" "not"
	     "bt" "bts" "btr" "btc"
	     "shl" "shr" "setcc" "movzx8" "xchg"
	     "jmp32" "jcc32" "callrel" "sar" "op16" "leave" "adc"];

  cnames = '["o" "no" "b" "ae" "e" "ne" "be" "a"
	     "s" "ns" "p" "np" "l" "ge" "le" "g"];

  rnames32 = '["eax" "ecx" "edx" "ebx" "esp" "ebp" "esi" "edi"];
  //rnames16 = '["ax" "cx" "dx" "bx" "sp" "bp" "si" "di"];
  rnames8 = '["al" "cl" "dl" "bl" "ah" "ch" "dh" "bh"];

  mode = '[0 0 0 0 0 "cst" "fn" "gbl" "gcst" "prim" "sym"];

  eastr = fn (ea, rnames)
    [
      | m, a |

      m = car(ea); a = cdr(ea);
      if (m == x86:lreg)
	rnames[a]
      else if (m == x86:lidx)
	format("%s[%s]", cdr(a), rnames32[car(a)])
      else if (m == x86:lridx)
	format("%s[%s*%s+%s]", cdddr(a), rnames32[car(a)], cadr(a),
	       rnames32[caddr(a)])
      else if (m == x86:lqidx)
	format("%s[%s*%s]", cddr(a), rnames32[car(a)], cadr(a))
      else if (m == x86:limm)
	if (integer?(a)) format("%s", a)
	else if (cdr(a)) format("2*%s+1", car(a))
	else format("2*%s", car(a))
      else
	format("%s[%s]", mode[m], a);
    ];

  slabel = fn (label)
    [
      | nlabel |
      while (nlabel = label[x86:l_alias]) label = nlabel;
      itoa(label[x86:l_number])
    ];

  x86:print_ins = fn (ins)
    [
      | op, a1, a2 |

      op = ins[x86:i_op];
      a1 = ins[x86:i_arg1];
      a2 = ins[x86:i_arg2];

      if (op == x86:op_jmp)
	display(format("jmp %s", slabel(a1)))
      else if (op == x86:op_jmp32)
	display(format("jmp32 %s", slabel(a1)))
      else if (op == x86:op_jcc)
	display(format("j%s %s", cnames[a2], slabel(a1)))
      else if (op == x86:op_jcc32)
	display(format("j%s32 %s", cnames[a2], slabel(a1)))
      else if (op == x86:op_callrel)
	display(format("callrel %s", a1))
      else if (op == x86:op_setcc)
	display(format("set%s %s", cnames[a1], eastr(a2, rnames8)))
      else if (a1 == null)
	display(format("%s", opname[op]))
      else if (a2 == null)
	display(format("%s %s", opname[op], eastr(a1, rnames32)))
      else
	display(format("%s %s,%s", opname[op],
		       eastr(a1, rnames32), eastr(a2, rnames32)));
    ];

];

];
