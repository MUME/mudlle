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

library mx64 // mudlle assembler for x86-64
// uses x64: prefix
requires compiler, dlist, misc, sequences, vars

defines x64:l_ins, x64:l_alias, x64:l_number, x64:il_label, x64:il_ins,
  x64:il_node, x64:il_number, x64:il_offset, x64:il_loc, x64:i_op,
  x64:i_arg1, x64:i_arg2, x64:lvar, x64:lreg, x64:lidx, x64:lridx, x64:limm,
  x64:limm64, x64:lcst, x64:lfunction, x64:lclosure, x64:lglobal,
  x64:lglobal_constant, x64:lglobal_index, x64:gl_c, x64:gl_mudlle,
  x64:lprimitive, x64:lspecial, x64:lindirect,
  x64:lseclev, x64:sl_c, x64:sl_mudlle, x64:sl_maxlev,
  x64:nregs, x64:reg_rax, x64:reg_rbx,
  x64:reg_rcx, x64:reg_rdx, x64:reg_rsp, x64:reg_rbp, x64:reg_rsi, x64:reg_rdi,
  x64:reg_r8, x64:reg_r9, x64:reg_r10, x64:reg_r11, x64:reg_r12, x64:reg_r13,
  x64:reg_r14, x64:reg_r15,
  x64:bne, x64:be, x64:bg, x64:ble, x64:bge,
  x64:bl, x64:ba, x64:bbe, x64:bae, x64:bb, x64:bno, x64:bo, x64:bns, x64:bs,
  x64:bnp, x64:bp, x64:balways, x64:op_push, x64:op_pop, x64:op_call,
  x64:op_ret, x64:op_jmp, x64:op_jcc, x64:op_lea, x64:op_lea32, x64:op_mov,
  x64:op_add, x64:op_sub, x64:op_cmp, x64:op_cmpbyte, x64:op_or, x64:op_xor,
  x64:op_and, x64:op_test, x64:op_dec, x64:op_neg, x64:op_not,
  x64:op_shl, x64:op_shr,
  x64:op_setcc, x64:op_movzxbyte, x64:op_imul, x64:op_movbyte,
  x64:op_orbyte, x64:op_xchg,
  x64:ops, x64:new_code,
  x64:set_instruction, x64:get_instructions, x64:rem_instruction,
  x64:copy_instruction, x64:mudlleint, x64:doubleint, x64:push, x64:pop,
  x64:call, x64:ret, x64:jmp, x64:jcc, x64:lea, x64:lea32, x64:mov, x64:add, x64:sub,
  x64:cmp, x64:cmpbyte, x64:or, x64:xor, x64:and, x64:test,
  x64:dec, x64:neg, x64:not,
  x64:shl, x64:shr, x64:setcc, x64:movzxbyte, x64:imul, x64:movbyte,
  x64:orbyte, x64:xchg,
  x64:new_label,
  x64:label, x64:set_label, x64:skip_label_alias, x64:ins_list, x64:print_ins,
  x64:resolve, x64:resolve64, x64:trap,
  x64:op_jmp32, x64:op_jcc32, x64:sar, x64:op_sar,
  x64:leave, x64:op_leave, x64:lqidx,
  x64:reset_ins_count

reads x64:spillreg

[
// labels:

x64:l_ins = 0; // instruction pointed to
x64:l_alias = 1; // we are an alias to this label
x64:l_number = 2; // unique number (for display)

// An instruction list is a list of the following vectors:

x64:il_label = 0; // label of this instruction, or false
x64:il_ins = 1; // the actual instruction
x64:il_node = 2; // the basic block to which this instruction belongs
x64:il_number = 3; // a unique number (for display)
x64:il_offset = 4; // instruction offset (int)
x64:il_loc = 5;

// An instruction is a vector:
// (all instructins must represent legal x64 instructions)

x64:i_op = 0;   // op
x64:i_arg1 = 1; // upto 2 arguments (unused ones set to null)
x64:i_arg2 = 2;

// arguments are pairs: mode . arguments, except for labels and ccs
// modes/arguments are:
//   x64:lreg: register
//   x64:lidx: register . offset
//   x64:lridx: register1 . scale . register2 . disp
//     (register1 * scale + register2 + disp)
//   x64:lqidx: register1 . scale . disp
//     (register1 * scale + disp)
//   x64:limm: immediate
//   x64:lcst: mudlle constant
//   x64:lfunction: magic
//   x64:lclosure: for functions without closure variables
//   x64:lglobal: global name
//   x64:lglobal_constant: global name
//   x64:lprimitive: primitive name
//   x64:lspecial: name (ref to C vars)
//   x64:lseclev: seclevel-related; arg is one of x64:xl_xxx
// accepted on input, but converted: lvar: variable
// arguments to x64:limm are integers or pairs:
//   x . 0 for 2*x and x . 1 for 2*x+1
// (to work around 31-bit integer limitations)
// each instruction can contains at most one non-x64:lreg argument
x64:lvar = 0;
x64:lreg = 1;
x64:lidx = 2;
x64:lridx = 3;
x64:lqidx = 4;
x64:limm = 5;                   // 32-bit immediate
x64:lcst = 6;
x64:lfunction = 7;
x64:lprimitive = 8;
x64:lclosure = 9;
x64:lglobal = 10;
x64:lglobal_constant = 11;
x64:lglobal_index = 12;
 x64:gl_c = 0;
 x64:gl_mudlle = 1;
x64:lspecial = 13;              // address of symbol
x64:lseclev = 14;
 x64:sl_c = 0;                  // function_seclevel() as C int
 x64:sl_mudlle = 1;             // function_seclevel() as mudlle int
 x64:sl_maxlev = 2;             // function's maxseclevel cap
x64:lindirect = 15;             // 32-bit rip-relative offset to 64-bit
                                // value
x64:limm64 = 16;                // 64-bit immediate; only with x64:mov

x64:nregs = 16;

x64:reg_rax = 0;
x64:reg_rbx = 3;
x64:reg_rcx = 1;
x64:reg_rdx = 2;
x64:reg_rsp = 4;
x64:reg_rbp = 5;
x64:reg_rsi = 6;
x64:reg_rdi = 7;
x64:reg_r8  = 8;
x64:reg_r9  = 9;
x64:reg_r10 = 10;
x64:reg_r11 = 11;
x64:reg_r12 = 12;
x64:reg_r13 = 13;
x64:reg_r14 = 14;
x64:reg_r15 = 15;

// branches, using x64 encoding
x64:bne = 5;                    // ZF = 1
x64:be = 4;                     // ZF = 0

x64:bg = 15;                    // ZF = 0 and SF = OF
x64:ble = 14;                   // ZF = 1 or SF != OF
x64:bge = 13;                   // SF = OF
x64:bl = 12;                    // SF != OF

x64:ba = 7;                     // CF = 0 and ZF = 0
x64:bbe = 6;                    // CF = 1 or ZF 1
x64:bae = 3;                    // CF = 0
x64:bb = 2;                     // CF = 1

x64:bno = 1;                    // OF = 0
x64:bo = 0;                     // OF = 1
x64:bns = 9;                    // SF = 0
x64:bs = 8;                     // SF = 1
x64:bnp = 11;                   // PF = 0
x64:bp = 10;                    // PF = 1

x64:balways = -1; // special value, used for x64:trap

// operations
x64:op_push = 0;
x64:op_pop = 1;
x64:op_leave = 17;

x64:op_call = 2;
x64:op_ret = 3;
x64:op_jmp = 4;
x64:op_jcc = 5;
x64:op_jmp32 = 29;
x64:op_jcc32 = 21;

x64:op_lea = 6;
x64:op_lea32 = 31;
x64:op_mov = 7;

x64:op_add = 8;
x64:op_sub = 9;
x64:op_cmp = 10;
x64:op_cmpbyte = 11;
x64:op_or = 12;
x64:op_xor = 13;
x64:op_and = 14;
x64:op_test = 16;
x64:op_orbyte = 24;
x64:op_xchg = 30;

x64:op_dec = 18;
x64:op_neg = 19;
x64:op_not = 20;

x64:op_shl = 25;
x64:op_shr = 26;
x64:op_sar = 23;
x64:op_setcc = 27;
x64:op_movzxbyte = 28;
x64:op_movbyte = 22;

x64:op_imul = 15;

x64:ops = 32;

| int31? |
int31? = fn (int n) n >= -0x40000000 && n <= 0x3fffffff;

[
  | ins_index, label_index, rnames64, rnames32, rnames16, rnames8, cnames,
    mode, eastr, slabel, opname, add_ins,
    generic_op0, generic_op1, generic_op2 |

  x64:new_code = fn ()
    // Returns: Structure in which instructions can be generated
    [
      vector(null, false, null)
      // 0: insertion position
      // 1: label for next instruction (false for none)
      // 2: list of error handlers [error number, loc, label]
    ];

  x64:set_instruction = fn "fncode ilist -> . Sets the current instruction insert position to ilist" (fcode, pos)
    [
      if (fcode[1]) fail();
      fcode[0] = pos;
    ];

  x64:get_instructions = fn "fncode -> ilist. Returns instruction list of ilist" (fcode)
    [
      if (fcode[1]) fail();
      fcode[0]
    ];

  x64:rem_instruction = fn (fcode, il)
    [
      fcode[0] = dremove!(il, fcode[0]);
    ];

  x64:copy_instruction = fn (fcode, il)
    [
      add_ins(fcode, il[x64:il_ins]);
    ];

  ins_index = 0;

  x64:reset_ins_count = fn () label_index = ins_index = 0;

  add_ins = fn (fcode, ins)
    // Types: fcode : x64code
    //        ins : instruction
    // Effects: Adds ins to the instructions in fcode, setting the label
    //   if necessary.
    //   Clears the current label
    [
      | newins |

      // Add instruction
      newins = vector(fcode[1], ins, null, ++ins_index, 0, mc:get_loc());

      // This is a strange hack:
      //   When code is initially generated, fcode[0] starts at null,
      //   then  gets set to the first instruction with subsequent
      //   instructions inserted before it (and because of the circular
      //   nature of the list, actually at the end).
      //   Later, when code needs patching, x64:set_instruction is called
      //   to set the insertion point, before which new instructions are
      //   added.
      if (fcode[0] == null) fcode[0] = dcons!(newins, null)
      else dcons!(newins, fcode[0]); // insert before fcode[0]

      // Set label if any
      if (fcode[1]) fcode[1][x64:l_ins] = newins;
      fcode[1] = false;
    ];

  x64:resolve64 = fn (type, arg, imm64?)
    [
      <cst> if (type == x64:lvar)
	exit<function> [
	  | loc |

	  if (mc:in_reg(arg))
	    x64:lreg . mc:get_reg(arg)
	  else if (loc = arg[mc:v_location])
	    x64:lidx . (x64:spillreg[loc[mc:v_lstype]] . loc[mc:v_lsoffset])
	  else			// no location: global or constant
            [
              | cls |
              cls = arg[mc:v_class];
              if (cls == mc:v_constant)
                [
                  type = x64:lcst;
                  arg = arg[mc:v_kvalue];
                  exit<cst> null;
                ]
              else if (cls == mc:v_global || cls == mc:v_global_define)
                x64:lglobal . arg[mc:v_name]
              else if (cls == mc:v_global_constant)
                [
                  | val |
                  val = global_value(arg[mc:v_goffset]);
                  if (val == null)
                    x64:limm . 0
                  else if (immutable?(val))
                    x64:lglobal_constant . arg[mc:v_name]
                  else
                    x64:lglobal . arg[mc:v_name]
                ]
              else if (cls == mc:v_function)
                x64:lfunction . arg[mc:v_fvalue]
              else
                fail()
            ]
	];

      if (type == x64:lcst)
        [
          if (arg == null)
            exit<function> x64:limm . 0;
          if (integer?(arg))
            if (int31?(arg))
              exit<function> x64:limm . x64:mudlleint(arg)
            else if (imm64?)
              exit<function> x64:limm64 . x64:mudlleint(arg);
        ];
      type . arg
    ];

  x64:resolve = fn (type, arg) x64:resolve64(type, arg, false);

  x64:mudlleint = fn (x) x . 1;
  x64:doubleint = fn (x) x . 0;

  // actual instructions

  generic_op0 = fn (op)
    fn (fcode)
      add_ins(fcode, vector(op, null, null));

  generic_op1 = fn (op)
    fn (fcode, m1, a1)
      add_ins(fcode, vector(op, x64:resolve(m1, a1), null));

  generic_op2 = fn (op)
    fn (fcode, m1, a1, m2, a2)
      add_ins(fcode, vector(op, x64:resolve(m1, a1), x64:resolve(m2, a2)));

  x64:push = generic_op1(x64:op_push);
  x64:pop = generic_op1(x64:op_pop);
  x64:leave = generic_op0(x64:op_leave);

  x64:call = generic_op1(x64:op_call);
  x64:ret = generic_op0(x64:op_ret);
  x64:jmp = fn (fcode, l)
    add_ins(fcode, vector(x64:op_jmp, l, null));
  x64:jcc = fn (fcode, cc, l)
    add_ins(fcode, vector(x64:op_jcc, l, cc));

  x64:lea = generic_op2(x64:op_lea);   // dest must be reg
  x64:lea32 = generic_op2(x64:op_lea32); // dest must be reg
  x64:mov = generic_op2(x64:op_mov);
  x64:movbyte = generic_op2(x64:op_movbyte);

  x64:imul = fn (fcode, m1, a1, m2, a2, m3, imm)
    [
      assert(m2 == x64:lreg);
      assert(m3 == x64:limm);
      // special case for ternary instruction
      add_ins(fcode, vector(x64:op_imul, x64:resolve(m1, a1),
                            x64:lidx . (a2 . imm)))
    ];

  x64:add = generic_op2(x64:op_add);
  x64:sub = generic_op2(x64:op_sub);
  x64:cmp = generic_op2(x64:op_cmp);
  x64:cmpbyte = generic_op2(x64:op_cmpbyte);
  x64:or = generic_op2(x64:op_or);
  x64:orbyte = generic_op2(x64:op_orbyte);
  x64:xor = generic_op2(x64:op_xor);
  x64:and = generic_op2(x64:op_and);
  x64:test = generic_op2(x64:op_test);
  x64:xchg = generic_op2(x64:op_xchg);

  x64:dec = generic_op1(x64:op_dec);
  x64:neg = generic_op1(x64:op_neg);
  x64:not = generic_op1(x64:op_not);

  x64:shl = generic_op2(x64:op_shl); // many restrictions on arg1
  x64:shr = generic_op2(x64:op_shr); // many restrictions on arg1
  x64:sar = generic_op2(x64:op_sar); // many restrictions on arg1
  x64:setcc = fn (fcode, cc, m1, a1)
    add_ins(fcode, vector(x64:op_setcc, cc, x64:resolve(m1, a1)));

  x64:movzxbyte = generic_op2(x64:op_movzxbyte); // dest must be register

  // labels

  label_index = 0;
  x64:new_label = fn "x64code -> label. Returns a new unassigned label in x64code"
    (fcode)
      vector(false, false, ++label_index);

  x64:label = fn "x64code label -> . Makes label point at the next instruction to\n\
be generated in x64code" (fcode, label)
      [
	if (fcode[1]) label[x64:l_alias] = fcode[1]
	else fcode[1] = label;
      ];

  x64:skip_label_alias = fn (vector label)
    [
      | nlabel |
      while (vector?(nlabel = label[x64:l_alias]))
        label = nlabel;
      label
    ];

  x64:set_label = fn "label ilist -> . Sets label to point to ilist. Might make it an alias of existing label" (vector l, vector il)
    [
      | lab |

      if (lab = il[x64:il_label]) // make it an alias
	[
	  l[x64:l_alias] = lab;
	  l[x64:l_ins] = false;
	]
      else
	[
	  l[x64:l_ins] = il;
	  l[x64:l_alias] = false;
	  il[x64:il_label] = l;
	]
    ];

  // traps

  x64:trap = fn "x64code cc n -> Cause error n with arguments args if cc is true"
    (fcode, cc, n, args)
    [
      | l |

      <found> [
        for ( | tl | tl = fcode[2]; tl != null; tl = cdr(tl))
          match (car(tl))
            [
              [ ,n ,mc:get_loc() ,args label ] => [
                l = label;
                exit<found> null;
              ]
            ];

        // new trap
        l = x64:new_label(fcode);
        fcode[2] = vector(n, mc:get_loc(), args, l) . fcode[2];
      ];

      if (cc == x64:balways)
	x64:jmp(fcode, l)
      else
	x64:jcc(fcode, cc, l);
    ];

  // code display

  x64:ins_list = fn "x64code -> . Prints instruction list" (fcode)
    [
      | scan, ilist |
      ilist = fcode[0];
      scan = ilist;
      loop
	[
	  | il |

	  il = dget(scan);
	  if (il[x64:il_label])
	    dformat("%s:", slabel(il[x64:il_label]));
          | loc |
          loc = il[x64:il_loc];
	  dformat("\t%d:%d\t(%s) ", mc:loc_line(loc), mc:loc_column(loc),
                  il[x64:il_number]);

	  x64:print_ins(il[x64:il_ins]);

	  newline();
	  scan = dnext(scan);
	  if (scan == ilist) exit 0
	];
    ];

  opname = '[
    "push" "pop"
    "call" "ret" "jmp" "jcc"
    "lea" "mov"
    "add" "sub" "cmp" "cmp8" "or" "xor" "and" "imul" "test"
    "leave" "dec" "neg" "not"
    "jcc32" "mov8" "sar" "or8"
    "shl" "shr" "setcc" "movzx8" "jmp32"
    "xchg" "lea"
  ];
  assert(vlength(opname) == x64:ops);

  cnames = '["o" "no" "b" "ae" "e" "ne" "be" "a"
	     "s" "ns" "p" "np" "l" "ge" "le" "g"];

  rnames64 = '["rax" "rcx" "rdx" "rbx" "rsp" "rbp" "rsi" "rdi"
               "r8" "r9" "r10" "r11" "r12" "r13" "r14" "r15"];
  rnames32 = '["eax" "ecx" "edx" "ebx" "esp" "ebp" "esi" "edi"
               "r8d" "r9d" "r10d" "r11d" "r12d" "r13d" "r14d" "r15d"];
  rnames16 = '["ax" "cx" "dx" "bx" "sp" "bp" "si" "di"
               "r8w" "r9w" "r10w" "r11w" "r12w" "r13w" "r14w" "r15w"];
  rnames8 = '["al" "cl" "dl" "bl" "spl" "bpl" "sil" "dil"
              "r8b" "r9b" "r10b" "r11b" "r12b" "r13b" "r14b" "r15b"];

  mode = '[
    0 0 0 0 0 0 "cst" "fn" "prim" "closure" "gbl" "gcst" "gidx" "sym"
    "seclvl" "indirect" 0
  ];
  assert(vlength(mode) == x64:limm64 + 1);

  eastr = fn (@(m . a), rnames)
    [
      | itoea |

      itoea = fn (n)
        if (n >= -1024 && n <= 1024)
          itoa(n)
        else
          format("%#x", n);

      if (m == x64:lreg)
	rnames[a]
      else if (m == x64:lidx)
        [
          | r, disp |
          @(r . disp) = a;
          format("%s[%s]", itoea(disp), rnames64[r])
        ]
      else if (m == x64:lridx)
        [
          | ridx, scale, rbase, disp |
          @(ridx scale rbase . disp) = a;
          format("%s[%s*%d+%s]", itoea(disp), rnames64[ridx], scale,
                 rnames64[rbase])
        ]
      else if (m == x64:lqidx)
        [
          | ridx, scale, disp |
          @(ridx scale . disp) = a;
          format("%s[%s*%d]", itoea(disp), rnames64[ridx], scale)
        ]
      else if (m == x64:limm || m == x64:limm64)
	if (integer?(a))
          itoea(a)
	else if (cdr(a))
          format("2*%s+1", itoea(car(a)))
	else
          format("2*%s", itoea(car(a)))
      else if (m == x64:lfunction)
	format("fn[%s]", if (string?(a)) a else mc:fname(a))
      else if (m == x64:lclosure)
	format("closure[%s]", mc:fname(a))
      else if (m == x64:lseclev)
        match (a) [
          ,x64:sl_c => "seclev";
          ,x64:sl_mudlle => "seclev*2+1";
          ,x64:sl_maxlev => "maxseclev*2+1";
          _ => fail()
        ]
      else if (m == x64:lcst)
        format(if (integer?(a)) "%s[%#x]" else "%s[%0w]", mode[m], a)
      else if (m == x64:lglobal)
        format("%s[%s]", mode[m], a)
      else if (m == x64:lglobal_index)
        match (a) [
          (name . ,x64:gl_c) => format("%s[%s]", mode[m], name);
          (name . ,x64:gl_mudlle) => format("%s[%s]*2+1", mode[m], name);
          _ => fail();
        ]
      else if (m == x64:lindirect)
        if (string?(a))
          format("%s[%s]", mode[m], a)
        else
          format("%s[%s]", mode[m], eastr(a, rnames))
      else
        format("%s[%s]", mode[m], a);
    ];

  slabel = fn (label)
    itoa(x64:skip_label_alias(label)[x64:l_number]);

  x64:print_ins = fn (ins)
    [
      | op, a1, a2 |

      op = ins[x64:i_op];
      a1 = ins[x64:i_arg1];
      a2 = ins[x64:i_arg2];

      if (op == x64:op_jmp)
	dformat("jmp %s", slabel(a1))
      else if (op == x64:op_jmp32)
	dformat("jmp32 %s", slabel(a1))
      else if (op == x64:op_jcc)
	dformat("j%s %s", cnames[a2], slabel(a1))
      else if (op == x64:op_jcc32)
	dformat("j%s32 %s", cnames[a2], slabel(a1))
      else if (op == x64:op_setcc)
	dformat("set%s %s", cnames[a1], eastr(a2, rnames8))
      else if (op == x64:op_mov)
        [
          | rn2 |
          rn2 = match (car(a1))
            [
              ,x64:lseclev && cdr(a1) == x64:sl_c => rnames16;
              ,x64:lseclev || ,x64:lglobal_index => rnames32;
              ,x64:limm => [
                if (match (cdr(a1))
                  [
                    (n . _) => n & (1 << 30);
                    n => n & (1 << 31);
                    _ => fail();
                  ])
                  rnames64
                else
                  rnames32;
              ];
              ,x64:limm64 => [
                if ((match (cdr(a1))
                  [
                    (n . _) => n >> 31;
                    n => n >> 32;
                    _ => fail();
                  ]) == 0)
                  rnames32
                else
                  rnames64;
              ];
              _ => rnames64;
            ];
          dformat("%s %s,%s", opname[op],
                  eastr(a1, rnames64), eastr(a2, rn2));
        ]
      else if (op == x64:op_movbyte || op == x64:op_orbyte)
	dformat("%s %s,%s", opname[op], eastr(a1, rnames8),
                eastr(a2, rnames8))
      else if (op == x64:op_movzxbyte)
	dformat("%s %s,%s", opname[op], eastr(a1, rnames8),
                eastr(a2, rnames32))
      else if (op == x64:op_imul)
        [
          | imm2, r2 |
          @(,x64:lidx . (r2 . imm2)) = a2;
          dformat("%s %d,%s,%s", opname[op], imm2, eastr(a1, rnames64),
                  rnames64[r2]);
        ]
      else if (op == x64:op_lea32)
	dformat("%s %s,%s", opname[op],
                eastr(a1, rnames64), eastr(a2, rnames32))
      else if (a1 == null)
	dformat("%s", opname[op])
      else if (a2 == null)
	dformat("%s %s", opname[op], eastr(a1, rnames64))
      else
	dformat("%s %s,%s", opname[op],
                eastr(a1, rnames64), eastr(a2, rnames64));
    ];

];

];
