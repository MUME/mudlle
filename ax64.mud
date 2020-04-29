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

library ax64 // The x64 assembler
requires mx64, dlist, sequences, misc, compiler, vars
defines x64:assemble, x64:reset_counters
reads mc:verbose, mc:disassemble, x64:reg_globals
writes mc:nins, mc:nbytes, mc:jccjmp_count, mc:labeled_jmp
[
  | remove_aliases, resize_branches, increase_branches,
    assemble, ins_size, iops, ins_gen,
    int8?, uint8?, int31?, int32?, uint32?, immediate8?, immval8, immval32,
    immediate_low32, immediate_high32,
    register?, regval,
    peephole,
    rex_op, rex_w, rex_b, rex_x, rex_r, reg8_rex, reg_hi, rax?,
    imm_jmp8, imm_jmp32,
    word_size |

  word_size = 8;

  rex_op = 0x40;
  rex_w  = 8;
  rex_r  = 4;
  rex_x  = 2;
  rex_b  = 1;

  reg_hi = 8;                   // high bit of register number

  mc:jccjmp_count = mc:labeled_jmp = 0;

  // rex prefix needed to access byte registers except for %[abcd]l
  reg8_rex = fn (arg)
    match (arg)
      [
        (,x64:lreg . r) && r >= 4 => rex_op;
        _ => 0;
      ];

  // special cases
  imm_jmp8   = "jmp8";
  imm_jmp32  = "jmp32";

  rax? = fn (arg)
    match (arg)
      [
        (,x64:lreg . ,x64:reg_rax) => true
      ];

  int8? = fn (int n) n >= -128 && n <= 127;
  uint8? = fn (int n) (n & ~0xff) == 0;

  int31? = fn (int n) n >= -0x40000000 && n <= 0x3fffffff;
  int32? = fn (int n) n >= -0x80000000 && n <= 0x7fffffff;
  uint32? = fn (int n) (n & ~0xffffffff) == 0;

  x64:reset_counters = fn ()
    [
      mc:nins = mc:nbytes = 0;
    ];

  x64:reset_counters();

  x64:assemble = fn "x64code -> x64asm" (fcode)
    [
      | ilist |

      ilist = x64:get_instructions(fcode);
      remove_aliases(ilist);
      if (mc:verbose >= 5)
	[
	  x64:ins_list(fcode);
	  newline();
	];
      ilist = peephole(ilist);
      if (mc:verbose >= 4 || mc:disassemble)
	[
	  x64:ins_list(fcode);
	  newline();
	];
      mc:nins += dlength(ilist);

      dreduce(fn (il, ofs) [
        | ins, op, arg1, arg2, ops, size |
        ins = il[x64:il_ins];
        op = ins[x64:i_op];
        arg1 = ins[x64:i_arg1];
        arg2 = ins[x64:i_arg2];
        ops = iops[op](arg1, arg2);
        size = ins_size(ops);
        il[x64:il_offset] = vector(ofs, size, ops);
        ofs + size
      ], 0, ilist);

      resize_branches(ilist);

      assemble(ilist)
    ];

  remove_aliases = fn (ilist)
    // Types: ilist: list of x64 instructions
    // Effects: Removes aliased labels from ilist
    dforeach(fn (il) [
      | ins |
      ins = il[x64:il_ins];

      if (ins[x64:i_op] == x64:op_jmp || ins[x64:i_op] == x64:op_jcc)
        ins[x64:i_arg1] = x64:skip_label_alias(ins[x64:i_arg1]);
    ], ilist);

  | cmovcc_src? |
  // safe as cmovcc source: reg or memory that must not trigger segv
  cmovcc_src? = fn (arg)
    match (arg)
      [
        ((,x64:lreg || ,x64:lindirect) . _) => true;
        (,x64:lidx . ((,x64:reg_rsp || ,x64:reg_rbp) . _)) => true;
      ];

  peephole = fn (ilist)
    // Types: ilist: list of x64 instructions
    // Requires: ilist != null, no aliased labels
    // Returns: list of x64 instructions
    // Effects: Performs peephole optimisation - replaces
    //   jcc x/jmp y/x: with jncc y/x
    //   jcc 0f; mov x,%reg; 0: with cmovcc x,%reg
    [

      | iscan, jcc, jmp, noreturn, aliased_label |

      aliased_label = false;
      iscan = ilist;
      loop
	[
	  | il, ins, op, ilabel |
	  il = dget(iscan);
	  ins = il[x64:il_ins];
          op = ins[x64:i_op];
          ilabel = il[x64:il_label];

	  if (op == x64:op_jmp && ilabel)
	    [
	      if (mc:verbose >= 3)
		[
		  display("PEEPHOLE labeled jmp: ");
		  x64:print_ins(ins);
		  newline();
		];
	      // make label alias jump destination
              | label |
	      label = x64:skip_label_alias(ins[x64:i_arg1]);
	      x64:set_label(ilabel, label[x64:l_ins]);
	      il[x64:il_label] = ilabel = false;
	      aliased_label = true; // will need to remove aliases
	      ++mc:labeled_jmp;
	    ];

          if (!ilabel && (jmp != null || noreturn != null))
            [
              dremove!(iscan, ilist);
              iscan = if (noreturn != null) noreturn else jmp;
              if (mc:verbose >= 3)
                [
                  display("UNREACHABLE after ");
                  x64:print_ins(dget(iscan)[x64:il_ins]);
                  display(" : ");
                  x64:print_ins(ins);
                  newline();
                ];
            ]
          else if (jmp != null && ilabel == dget(jmp)[x64:il_ins][x64:i_arg1])
            [
              if (mc:verbose >= 3)
                [
                  display("USELESS jmp: ");
                  x64:print_ins(dget(jmp)[x64:il_ins]);
                  newline();
                ];
              dremove!(jmp, ilist);
              jmp = null
            ]
          else if (jcc == null) // nojcc state
	    [
              if (op == x64:op_jmp)
                jmp = iscan
              else
                [
                  jmp = null;
                  if (op == x64:op_jcc)
                    jcc = ins; // to jcc state
		]
	    ]
	  else if (jmp == null) // jcc state
	    [
	      // jmp must be unlabeled, but mc:labeled_jmp optimisation
	      // deals with that.
	      if (op == x64:op_jmp)
		jmp = iscan     // to jcc/jmp state
	      else if (op == x64:op_mov
                       && !ilabel
                       && car(ins[x64:i_arg2]) == x64:lreg
                       && dget(dnext(iscan))[x64:il_label] == jcc[x64:i_arg1]
                       && cmovcc_src?(ins[x64:i_arg1]))
                [
                  // jcc 0f; mov reg/mem,%reg; 0:
                  if (mc:verbose >= 3)
                    [
                      display("REPLACE jcc over mov with cmovcc: ");
                      x64:print_ins(jcc);
                      display("; ");
                      x64:print_ins(ins);
                    ];

                  [
                    | previ |
                    previ = dprev(iscan);
                    dremove!(iscan, ilist);
                    iscan = previ;
                  ];

                  jcc[x64:i_op] = x64:op_cmovcc;
                  jcc[x64:i_arg1] = ins[x64:i_arg1];
                  jcc[x64:i_arg2] = cons(
                    x64:lidx,
                    cons(
                      cdr(ins[x64:i_arg2]),
                      jcc[x64:i_arg2] ^ 1)); // inverse cc

                  if (mc:verbose >= 3)
                    [
                      display(" -> ");
                      x64:print_ins(jcc);
                      newline();
                    ];

                  jcc = null;
                ]
              else
		jcc = null; // back to nojcc
	    ]
	  else // jcc/jmp state
	    [
	      if (ilabel == jcc[x64:i_arg1]) // peephole time!
		[
		  ++mc:jccjmp_count;
		  if (mc:verbose >= 3)
		    [
		      display("PEEPHOLE jcc over jmp: ");
		      x64:print_ins(jcc);
		      newline();
		    ];
		  jcc[x64:i_arg1] = dget(jmp)[x64:il_ins][x64:i_arg1];
		  jcc[x64:i_arg2] ^= 1; // reverse sense.
		  // remove jmp
		  dremove!(jmp, ilist);
		];

              jmp = jcc = null; // back to nojcc
	    ];

          noreturn = null;
          if (op == x64:op_ret
              || (op == x64:op_call && ins[x64:i_arg2]))
            noreturn = iscan;

	  iscan = dnext(iscan);
	  if (iscan == ilist) exit 0;
	];

      if (aliased_label)
	remove_aliases(ilist);
      ilist
    ];

  increase_branches = fn (ilist)
    dreduce(fn (il, delta) [
      | ins, op |

      ins = il[x64:il_ins];
      op = ins[x64:i_op];
      if (delta)
        il[x64:il_offset][0] += delta;
      if (op == x64:op_jmp || op == x64:op_jcc)
        [
          | ofsv, ofs, size, dofs, dest |
          @[ofs size ...] = ofsv = il[x64:il_offset];
          dest = ins[x64:i_arg1];
          dofs = (dest[x64:l_ins][x64:il_offset][0]
                  - (ofs + size));
          if (!int8?(dofs))
            [
              ins[x64:i_op] = op = if (op == x64:op_jmp)
                x64:op_jmp32
              else
                x64:op_jcc32;
              | nops, nsize |
              ofsv[2] = nops = iops[op](dest, ins[x64:i_arg2]);
              ofsv[1] = nsize = ins_size(nops);
              delta += nsize - size
            ]
        ];
      delta
    ], 0, ilist);

  resize_branches = fn (ilist)
    while (increase_branches(ilist))
      null;

  | pri_used, pri_ofs |
  // data for PC-relatative (%rip-relative) addressing
  pri_used = 0;                 // number of slots used
  pri_ofs  = 1;                 // code offset of the first slot

  assemble = fn (ilist)
    [
      | last, code, info |
      last = dget(dprev(ilist));

      | code_end, reloc_start, lofs, lsize |
      @[lofs lsize ...] = last[x64:il_offset];
      code_end = lofs + lsize;

      reloc_start = (code_end + word_size - 1) & -word_size;

      mc:nbytes += code_end;
      code = make_string(code_end);

      info = make_vector(mc:a_info_fields);

      | pcrel_info |
      pcrel_info = vector(0, reloc_start);

      | last_line, linenos |

      dforeach(fn (il) [
        | ofs, size, ops, line |
        @[ofs size ops] = il[x64:il_offset];
        line = mc:loc_line(il[x64:il_loc]);
        if (line > 0 && line != last_line)
          [
            linenos = (ofs . line) . linenos;
            last_line = line;
          ];
        ins_gen(code, il, ops, ofs, size, info, pcrel_info);
      ], ilist);
      linenos = (code_end . last_line) . linenos;
      dforeach(fn (il) il[x64:il_offset] = il[x64:il_offset][0], ilist);

      info[mc:a_linenos] = vreverse!(list_to_vector(linenos));
      info[mc:a_rel_primitives] = pcrel_info[pri_used];

      code . info
    ];

  immval8 = fn (arg)
    [
      | i |

      i = cdr(arg);
      if (integer?(i)) i
      else if (int8?(car(i))) // arithmetic fails on large numbers
	2 * car(i) + cdr(i)
      else
        0xbad; // something that is false for int8? !
    ];
  immval32 = fn (arg)
    match! (cdr(arg))
      [
        i && integer?(i) => [
          assert(int32?(i));
          i
        ];
        (i . e) && integer?(i) => [
          assert(int31?(i));
          i * 2 + e
        ];
      ];
  immediate_low32 = fn (arg)
    (match (cdr(arg))
      [
        i && integer?(i) => i;
        (i . e) && integer?(i) => (i * 2) + e
      ]) & 0xffffffff;

  immediate_high32 = fn (arg)
    (match (cdr(arg))
      [
        i && integer?(i) => i >> 32;
        (i . _) && integer?(i) => i >> 31;
      ]) & 0xffffffff;

  immediate8? = fn (arg) car(arg) == x64:limm && int8?(immval8(arg));

  register? = fn (arg) car(arg) == x64:lreg;
  regval = cdr;

  iops = make_vector(x64:ops);
  // iops return values of (nbytes << 32) | value or (nested) vectors thereof

  | op_int8, op_int16, op_int32 |
  op_int8  = fn (n) (1 << 32) | (n & 0xff);
  op_int16 = fn (n) (2 << 32) | (n & 0xffff);
  op_int32 = fn (n) (4 << 32) | (n & 0xffffffff);

  | iop_op, iop_rex, iop_rex_reg |
  iop_rex = fn (int op, int rex)
    [
      | cnt, r |
      cnt = 1;
      r = op;
      if ((op & 0xff) == 0x0f)
        cnt = 2;
      if (rex)
        [
          r = rex | (r << 8);
          ++cnt
        ];
      (cnt << 32) | r
    ];

  iop_op = fn (op) iop_rex(op, 0);

  iop_rex_reg = fn (int op, int rex, int reg)
    [
      if (reg & reg_hi)
        [
          rex |= rex_op | rex_b;
          reg ^= reg_hi;
        ];
      iop_rex(op | reg, rex)
    ];

  | iop_rex_reg_modrm |
  iop_rex_reg_modrm = fn (op, rex, reg1, a2)
    [
      | iop_rex_modrm, iop_rex_modrm_sib |
      iop_rex_modrm = fn (op, rex, mod, reg0, reg1)
        [
          if (reg0 & reg_hi)
            [
              rex |= rex_op | rex_r;
              reg0 ^= reg_hi;
            ];
          if (reg1 & reg_hi)
            [
              rex |= rex_op | rex_b;
              reg1 ^= reg_hi;
            ];
          vector(iop_rex(op, rex), op_int8(mod | (reg0 << 3) | reg1))
        ];

      iop_rex_modrm_sib = fn (op, rex, mod, reg, scale, idx, base)
        [
          if (reg & reg_hi)
            [
              rex |= rex_op | rex_r;
              reg ^= reg_hi;
            ];
          if (idx & reg_hi)
            [
              rex |= rex_op | rex_x;
              idx ^= reg_hi;
            ];
          if (base & reg_hi)
            [
              rex |= rex_op | rex_b;
              base ^= reg_hi;
            ];

          vector(iop_rex(op, rex),
                 op_int16(mod | (reg << 3)
                          | ((scale | (idx << 3) | base) << 8)))
        ];

      | scale_ss, sib_scale_8, sib_scale_4, sib_scale_2, sib_scale_1 |
      sib_scale_8 = 0xc0;
      sib_scale_4 = 0x80;
      sib_scale_2 = 0x40;
      sib_scale_1 = 0x00;

      scale_ss = fn (scale)
        match! (scale)
          [
            1 => sib_scale_1;
            2 => sib_scale_2;
            4 => sib_scale_4;
            8 => sib_scale_8;
          ];

      match! (car(a2))
        [
          ,x64:lreg => iop_rex_modrm(op, rex, 0xc0, reg1, regval(a2));
          ,x64:lidx => [
            | r, disp, opx, val, mod |
            @(r . disp) = cdr(a2);

            if (r == x64:reg_rip)
              [
                val = if (function?(disp))
                  disp
                else
                  [
                    assert(int32?(disp));
                    op_int32(disp);
                  ];
                exit<function> vector(iop_rex_modrm(op, rex, 0, reg1, 5), val);
              ];

	    if (disp == 0 && (r & 7) != 5)
	      [
		// must use index with %rbp and %r13
		mod = 0;
	      ]
	    else if (int8?(disp))
              [
                val = op_int8(disp);
                mod = 0x40;
              ]
            else
              [
                assert(int32?(disp));
                val = op_int32(disp);
                mod = 0x80;
              ];

            opx = if ((r & 7) == 4)
              [
                // must use sib for %rsp and %r12
                iop_rex_modrm_sib(op, rex, mod | 4, reg1, sib_scale_1, 4, r);
              ]
	    else
              iop_rex_modrm(op, rex, mod, reg1, r);

            if (val != null)
	      vector(opx, val)
	    else
	      opx
          ];
          ,x64:lqidx => [
            | ridx, scale, disp |
            @(ridx scale . disp) = cdr(a2);
            assert(ridx != x64:reg_rsp); // cannot be encoded
            scale = scale_ss(scale);
            vector(iop_rex_modrm_sib(op, rex, 0x04, reg1, scale, ridx, 5),
                   op_int32(disp))
          ];
          ,x64:lridx => [
            | ridx, scale, rbase, disp |
            @(ridx scale rbase . disp) = cdr(a2);
            scale = scale_ss(scale);
            assert(ridx != x64:reg_rsp); // cannot be encoded
	    | mod, val |
	    if (disp == 0 && (ridx & 7) != 5)
	      [
		// must use displacement with %rbp and %r13
		mod = 0x04;
	      ]
	    else if (int8?(disp))
	      [
		mod = 0x44;
		val = op_int8(disp);
	      ]
	    else
	      [
		mod = 0x84;
		val = op_int32(disp);
	      ];
	    | opx |
	    opx = iop_rex_modrm_sib(0x8d, rex, mod, reg1, scale, ridx, rbase);
	    if (val != null)
	      vector(opx, val)
	    else
	      opx
          ];
          ,x64:lglobal => [
            vector(iop_rex_modrm(op, rex, 0x80, reg1, x64:reg_globals), a2)
          ];
          ,x64:lcst || ,x64:lglobal_constant || ,x64:lindirect => [
            if (car(a2) != x64:lindirect)
              a2 = x64:lindirect . a2;
            vector(iop_rex_modrm(op, rex, 0, reg1, 5), a2)
          ];
        ]
    ];
  iops[x64:op_push] = fn (a1, a2)
    match (car(a1))
      [
        ,x64:lreg => iop_rex_reg(0x50, 0, regval(a1));
        ,x64:limm => [
          | imm |
          imm = immval32(a1);
          if (int8?(imm))
            vector(iop_op(0x6a), op_int8(imm))
          else
            vector(iop_op(0x68), op_int32(imm))
        ];
        _ => iop_rex_reg_modrm(0xff, 0, 6, a1);
      ];

  iops[x64:op_pop] = fn (a1, a2)
    if (register?(a1))
      iop_rex_reg(0x58, 0, regval(a1))
    else
      iop_rex_reg_modrm(0x8f, 0, 0, a1);

  iops[x64:op_leave] = fn (a1, a2) iop_op(0xc9);
  iops[x64:op_ret] = fn (a1, a2) iop_op(0xc3);

  iops[x64:op_movzxbyte] = fn (a1, a2)
    [
      assert(register?(a2));
      iop_rex_reg_modrm(0xb60f, reg8_rex(a1), regval(a2), a1);
    ];

  iops[x64:op_movzxword] = fn (a1, a2)
    [
      assert(register?(a2));
      iop_rex_reg_modrm(0xb70f, 0, regval(a2), a1);
    ];

  iops[x64:op_movzx32] = fn (a1, a2)
    [
      assert(register?(a2));
      iop_rex_reg_modrm(0x89, 0, regval(a1), a2);
    ];

  iops[x64:op_mov] = fn (a1, a2)
    match (car(a1))
      [
        ,x64:lreg => iop_rex_reg_modrm(0x89, rex_op | rex_w, regval(a1), a2);
        ,x64:limm64 && immediate_high32(a1) == 0 => [
          // 32-bit zero-extended immediate mov
          assert(register?(a2));
          vector(iop_rex_reg(0xb8, 0, regval(a2)),
                 op_int32(immediate_low32(a1)))
        ];
        ,x64:lcst || ,x64:lglobal_constant || ,x64:lspecial || ,x64:lclosure
        || ,x64:lfunction || ,x64:lprimitive || ,x64:limm64 => [
            // 64-bit immediate mov
            assert(register?(a2));
            vector(iop_rex_reg(0xb8, rex_op | rex_w, regval(a2)), a1)
          ];
        ,x64:limm || ,x64:lseclev || ,x64:lglobal_index => [
          // 32-bit immediate mov
          if (register?(a2)
              && !(car(a1) == x64:limm && (immval32(a1) & (1 << 31))))
            [
              // zero-extends
              vector(iop_rex_reg(0xb8, 0, regval(a2)), a1)
            ]
          else
            [
              // sign-extends
              vector(iop_rex_reg_modrm(0xc7, rex_op | rex_w, 0, a2), a1)
            ]
        ];
        ,x64:lidx || ,x64:lindirect || ,x64:lglobal => [
          assert(register?(a2));
          iop_rex_reg_modrm(0x8b, rex_op | rex_w, regval(a2), a1);
        ];
        _ => fail_message(format("invalid mov %w, %w", a1, a2))
      ];

  | iop_math, iop_unary_math, iop_math_byte |
  iop_math = fn (modrm_reg, dflt_rex) fn (a1, a2)
    match (car(a1))
      [
        ,x64:limm => [
          | imm, rex |
          imm = immval32(a1);

          rex = dflt_rex;

          // special-case and $imm,%rXd
          if (modrm_reg == 4 && register?(a2) && uint32?(imm))
            rex = 0;

          if (int8?(imm))
            vector(iop_rex_reg_modrm(0x83, rex, modrm_reg, a2), op_int8(imm))
          else if (imm == 128 && (modrm_reg == 0 || modrm_reg == 5))
            // convert add/sub $128,r/m to sub/add $-128,r/m
            vector(iop_rex_reg_modrm(0x83, rex, modrm_reg ^ 5, a2),
                   op_int8(-imm))
          else
            [
              imm = op_int32(imm);
              if (rax?(a2))
                vector(iop_rex(0x05 + (modrm_reg << 3), rex), imm)
              else
                vector(iop_rex_reg_modrm(0x81, rex, modrm_reg, a2), imm)
            ]
        ];
        ,x64:lreg => [
          | r1, rex |
          r1 = regval(a1);
          rex = dflt_rex;
          // special-case xor %rX,%rX to xor %rXd,%rXd
          if (modrm_reg == 6)
            match (a2)
              [
                (,x64:lreg . ,r1) => rex = 0;
              ];
          iop_rex_reg_modrm(0x01 + (modrm_reg << 3), rex, r1, a2)
        ];
        _ => [
          assert(register?(a2));
          iop_rex_reg_modrm(0x03 + (modrm_reg << 3), dflt_rex, regval(a2), a1)
        ];
      ];

  iops[x64:op_add]   = iop_math(0, rex_op | rex_w);
  iops[x64:op_add32] = iop_math(0, 0);
  iops[x64:op_and]   = iop_math(4, rex_op | rex_w);
  iops[x64:op_cmp]   = iop_math(7, rex_op | rex_w);
  iops[x64:op_or]    = iop_math(1, rex_op | rex_w);
  iops[x64:op_sub]   = iop_math(5, rex_op | rex_w);
  iops[x64:op_xor]   = iop_math(6, rex_op | rex_w);

  iop_math_byte = fn (modrm_reg) fn (a1, a2)
    [
      if (immediate8?(a1))
        [
          | imm |
          imm = op_int8(immval8(a1));
          if (rax?(a2))
            vector(iop_op(0x04 + (modrm_reg << 3)), op_int8(imm))
          else
            vector(iop_rex_reg_modrm(0x80, 0, modrm_reg, a2), imm)
        ]
      else if (register?(a1))
        iop_rex_reg_modrm(modrm_reg << 3, 0, regval(a1), a2)
      else
        [
          assert(register?(a2));
          iop_rex_reg_modrm(2 + (modrm_reg << 3), 0, regval(a2), a1)
        ];
    ];

  iops[x64:op_orbyte] = iop_math_byte(1);
  iops[x64:op_cmpbyte] = iop_math_byte(7);

  iop_unary_math = fn (op, modrm_reg) fn (a1, a2)
    iop_rex_reg_modrm(op, rex_op | rex_w, modrm_reg, a1);

  iops[x64:op_dec] = iop_unary_math(0xff, 1);
  iops[x64:op_not] = iop_unary_math(0xf7, 2);
  iops[x64:op_neg] = iop_unary_math(0xf7, 3);

  iops[x64:op_imul] = fn (a1, a2)
    [
      | imm, r2 |
      @(,x64:lidx . (r2 . imm)) = a2;
      if (int8?(imm))
        vector(iop_rex_reg_modrm(0x6b, rex_op | rex_w, r2, a1),
               op_int8(imm))
      else
        [
          assert(int32?(imm));
          vector(iop_rex_reg_modrm(0x69, rex_op | rex_w, r2, a1),
                 op_int32(imm))
        ]
    ];

  iops[x64:op_xchg] = fn (a1, a2)
    [
      if (car(a2) != x64:lreg || register?(a1) && rax?(a2))
        [
          | tmp |
          tmp = a1;
          a1 = a2;
          a2 = tmp;
        ];
      | r2 |
      assert(register?(a2));
      r2 = regval(a2);
      if (rax?(a1))
        iop_rex_reg(0x90, rex_op | rex_w, r2)
      else
        iop_rex_reg_modrm(0x87, rex_op | rex_w, r2, a1)
    ];

  iops[x64:op_movbyte] = fn (a1, a2)
    [
      assert(register?(a1));
      iop_rex_reg_modrm(0x88, reg8_rex(a1), regval(a1), a2);
    ];

  iops[x64:op_test] = fn (a1, a2)
    [
      if (car(a1) == x64:limm)
        [
          | iop, val, rax |
          rax = rax?(a2);
          val = immval32(a1);
          if (uint8?(val))
            [
              iop = if (rax)
                iop_op(0xa8)
              else
                iop_rex_reg_modrm(0xf6, reg8_rex(a2), 0, a2);
              val = op_int8(val)
            ]
          else
            [
              | rex |
              rex = if (uint32?(val)) 0 else rex_op | rex_w;
              iop = if (rax)
                iop_rex(0xa9, rex)
              else
                iop_rex_reg_modrm(0xf7, rex, 0, a2);
              val = op_int32(val);
            ];
          vector(iop, val)
        ]
      else
        [
          assert(register?(a1));
          iop_rex_reg_modrm(0x85, rex_op | rex_w, regval(a1), a2);
        ]
    ];

  iops[x64:op_lea] = fn (a1, a2)
    [
      assert(register?(a2));
      iop_rex_reg_modrm(0x8d, rex_op | rex_w, regval(a2), a1);
    ];

  iops[x64:op_lea32] = fn (a1, a2)
    [
      assert(register?(a2));
      iop_rex_reg_modrm(0x8d, 0, regval(a2), a1);
    ];

  iops[x64:op_setcc] = fn (a1, a2)
    iop_rex_reg_modrm(0x900f | (a1 << 8), reg8_rex(a2), 0, a2);
  iops[x64:op_cmovcc] = fn (a1, a2)
    [
      | r2, cc |
      @(,x64:lidx . (r2 . cc)) = a2;
      iop_rex_reg_modrm(0x400f | (cc << 8), rex_op | rex_w, r2, a1);
    ];

  iops[x64:op_jmp] = fn (a1, a2)
    vector(iop_op(0xeb), imm_jmp8);
  iops[x64:op_jmp32] = fn (a1, a2)
    vector(iop_op(0xe9), imm_jmp32);
  iops[x64:op_jcc] = fn (a1, a2)
    vector(iop_op(0x70 | a2), imm_jmp8);
  iops[x64:op_jcc32] = fn (a1, a2)
    vector(iop_op(0x800f | (a2 << 8)), imm_jmp32);

  iops[x64:op_call] = fn (a1, a2)
    iop_rex_reg_modrm(0xff, 0, 2, a1);

  | iop_shift |
  iop_shift = fn (op) fn (a1, a2)
    [
      assert(immediate8?(a1));
      match (immval8(a1))
        [
          1 => iop_rex_reg_modrm(0xd1, rex_op | rex_w, op, a2);
          n => vector(iop_rex_reg_modrm(0xc1, rex_op | rex_w, op, a2),
                      op_int8(n))
        ]
    ];

  iops[x64:op_shl] = iop_shift(4);
  iops[x64:op_shr] = iop_shift(5);
  iops[x64:op_sar] = iop_shift(7);

  ins_size = fn (val)
    [
      if (vector?(val))
        exit<function> vreduce(fn (e, n) n + ins_size(e), 0, val);

      if (integer?(val))
        val >> 32
      else if (val == imm_jmp8)
        1
      else if (val == imm_jmp32)
        4
      else if (function?(val))
        val(0)
      else
        match (car(val))
          [
            ,x64:lindirect || ,x64:lglobal_index || ,x64:lglobal
              || ,x64:lseclev || ,x64:limm => 4;
            ,x64:lspecial || ,x64:lclosure
              || ,x64:limm64 || ,x64:lcst || ,x64:lglobal_constant
              || ,x64:lfunction || ,x64:lprimitive => 8;
            _ => fail_message(format("unknown op %w", val))
          ]
    ];

  | insert_int |
  insert_int = fn (code, offset, nbytes, val)
    while (nbytes > 0)
      [
        code[offset++] = val;
        val >>= 8;
        --nbytes;
      ];

  ins_gen = fn (code, il, ops, offset, size, info, pcrel_info)
    [
      | iter, oofs |
      oofs = offset;

      iter = fn (val, offset)
        [
          if (vector?(val))
            [
              for (|l, i| [ l = vlength(val); i = 0 ]; i < l; ++i)
                offset = iter(val[i], offset);
              exit<function> offset;
            ];

          | v, nbytes |
          v = 0;
          nbytes = match (val)
            [
              n && integer?(n) => [
                v = n;
                nbytes = n >> 32;
                assert(nbytes >= 0 && nbytes <= 4);
                nbytes
              ];
              f && function?(f) => [
                v = f(1);
                nbytes = f(0);
                assert(nbytes >= 0 && nbytes <= 4);
                nbytes
              ];
              x && x == imm_jmp8 => [
                | dest |
                dest = il[x64:il_ins][x64:i_arg1];
                v = dest[x64:l_ins][x64:il_offset][0] - (offset + 1);
                1
              ];
              x && x == imm_jmp32 => [
                | dest |
                dest = il[x64:il_ins][x64:i_arg1];
                v = dest[x64:l_ins][x64:il_offset][0] - (offset + 4);
                4
              ];
              (vop . varg) => [
                match! (vop) [
                  ,x64:lindirect => <done> [
                    | info_list, x |
                    if (string?(varg))
                      [
                        info_list = mc:a_builtins;
                        x = varg;
                      ]
                    else
                      [
                        info_list = match! (car(varg))
                          [
                            ,x64:lcst => mc:a_constants;
                            ,x64:lglobal_constant => mc:a_kglobals;
                            ,x64:lprimitive => mc:a_primitives;
                          ];
                        x = cdr(varg);
                      ];
                    v = match (assq(x, info[info_list]))
                      [
                        (_ . o) => o;
                        _ => [
                          | indofs |
                          indofs = (pcrel_info[pri_used] * word_size
                                    + pcrel_info[pri_ofs]);
                          info[info_list] = (x . indofs) . info[info_list];
                          ++pcrel_info[pri_used];
                          indofs
                        ];
                      ];
                    v -= offset + 4;
                    4
                  ];
                  ,x64:limm => [
                    v = immediate_low32(val);
                    4
                  ];
                  ,x64:limm64 => [
                    insert_int(code, offset, 4, immediate_low32(val));
                    offset += 4;
                    v = immediate_high32(val);
                    4
                  ];
                  ,x64:lglobal || ,x64:lglobal_index => [
                    // varg is name for the byte offset into the global array
                    // varg is name . x64:gl_c for the global index as a
                    //   native integer
                    // varg is name . x64:gl_mudlle for the global index as
                    //   a mudlle integer
                    info[mc:a_globals] = (varg . offset) . info[mc:a_globals];
                    4
                  ];
                  ,x64:lseclev => [
                    info[mc:a_seclevs] = (varg . offset) . info[mc:a_seclevs];
                    4
                  ];
                  ,x64:lspecial => [
                    info[mc:a_builtins]
                      = (varg . offset) . info[mc:a_builtins];
                    8
                  ];
                  ,x64:lclosure => [
                    // negative offset = closure
                    info[mc:a_subfns] = (varg . -offset) . info[mc:a_subfns];
                    8
                  ];
                  ,x64:lfunction => [
                    if (string?(varg))
                      info[mc:a_kglobal_code]
                        = (varg . offset) . info[mc:a_kglobal_code]
                    else
                      // positive offset = code
                      info[mc:a_subfns] = (varg . offset) . info[mc:a_subfns];
                    8
                  ];
                  ,x64:lprimitive => [
                    info[mc:a_primitives]
                      = (varg . offset) . info[mc:a_primitives];
                    8
                  ];
                  ,x64:lcst => [
                    info[mc:a_constants]
                      = (varg . offset) . info[mc:a_constants];
                    8
                  ];
                  ,x64:lglobal_constant => [
                    info[mc:a_kglobals]
                      = (varg . offset) . info[mc:a_kglobals];
                    8
                  ];
                ]
              ];
              x => fail_message(format("unknown op %w", x))
            ];

          insert_int(code, offset, nbytes, v);
          offset + nbytes;
        ];

      offset = iter(ops, oofs);

      if (oofs + size != offset)
        [
          x64:print_ins(il[x64:il_ins]); newline();
          fail_message(format("bad size; expected %d got %d: ",
                              size, offset - oofs))
        ]
    ];
];
