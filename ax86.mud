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

library ax86 // The x86 assembler
requires mx86, dlist, sequences, misc, compiler, vars
defines x86:assemble, x86:reset_counters
reads mc:verbose, mc:disassemble, x86:reg_globals
writes mc:nins, mc:nbytes, mc:jccjmp_count, mc:labeled_jmp
[
  | remove_aliases, setword, resize_branches, set_offsets, increase_branches,
    assemble, ins_size, igen, generic2, generic2byte, generic2math,
    generic2mathbyte, generic1, genericbit, genericshift, ins_gen, setea1,
    setimm, byte?, immediate8?, immval8, immediate?, register?, regval, easize,
    isize, gsize2, gsize2u, gsize2b, gsize2byte, gsize1, gsize1b, gsizebit,
    gsizeshift, peephole, ubyte?, uimmval8, uimmediate8?, byte_reg?, eax?,
    imm_zero? |

  mc:jccjmp_count = mc:labeled_jmp = 0;

  byte? = fn (int n) n >= -128 && n <= 127;

  x86:reset_counters = fn ()
    [
      mc:nins = mc:nbytes = 0;
    ];

  eax? = fn (arg) register?(arg) && regval(arg) == x86:reg_eax;

  x86:assemble = fn "x86code -> x86asm" (fcode)
    [
      | ilist |

      ilist = x86:get_instructions(fcode);
      remove_aliases(ilist);
      if (mc:verbose >= 5)
	[
	  x86:ins_list(fcode);
	  newline();
	];
      ilist = peephole(ilist);
      if (mc:verbose >= 4 || mc:disassemble)
	[
	  x86:ins_list(fcode);
	  newline();
	];
      mc:nins += dlength(ilist);
      resize_branches(ilist);

      assemble(ilist)
    ];

  remove_aliases = fn (ilist)
    // Types: ilist: list of x86 instructions
    // Effects: Removes aliased labels from ilist
    dforeach(fn (il)
	     [
	       | ins |
	       ins = il[x86:il_ins];

	       if (ins[x86:i_op] == x86:op_jmp || ins[x86:i_op] == x86:op_jcc)
                 ins[x86:i_arg1] = x86:skip_label_alias(ins[x86:i_arg1]);
	     ], ilist);

  | cmovcc_src? |
  // safe as cmovcc source: reg or memory that must not trigger segv
  cmovcc_src? = fn (arg)
    match (arg)
      [
        (,x86:lreg . _) => true;
        (,x86:lidx . ((,x86:reg_esp || ,x86:reg_ebp) . _)) => true;
      ];

  peephole = fn (ilist)
    // Types: ilist: list of x86 instructions
    // Requires: ilist != null, no aliased labels
    // Returns: list of x86 instructions
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
	  ins = il[x86:il_ins];
          op = ins[x86:i_op];
          ilabel = il[x86:il_label];

	  if (op == x86:op_jmp && ilabel)
	    [
	      if (mc:verbose >= 3)
		[
		  display("PEEPHOLE labeled jmp: ");
		  x86:print_ins(ins);
		  newline();
		];
	      // make label alias jump destination
              | label |
	      label = x86:skip_label_alias(ins[x86:i_arg1]);
	      x86:set_label(ilabel, label[x86:l_ins]);
	      il[x86:il_label] = ilabel = false;
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
                  x86:print_ins(dget(iscan)[x86:il_ins]);
                  display(" : ");
                  x86:print_ins(ins);
                  newline();
                ]
            ]
          else if (jmp != null && ilabel == dget(jmp)[x86:il_ins][x86:i_arg1])
            [
              if (mc:verbose >= 3)
                [
                  display("USELESS jmp: ");
                  x86:print_ins(dget(jmp)[x86:il_ins]);
                  newline();
                ];
              dremove!(jmp, ilist);
              jmp = null
            ]
          else if (jcc == null) // nojcc state
	    [
              if (op == x86:op_jmp)
                jmp = iscan
              else
                [
                  jmp = null;
                  if (op == x86:op_jcc)
                    jcc = ins; // to jcc state
		]
	    ]
	  else if (jmp == null) // jcc state
	    [
	      // jmp must be unlabeled, but mc:labeled_jmp optimisation
	      // deals with that.
	      if (op == x86:op_jmp)
		jmp = iscan     // to jcc/jmp state
	      else if (op == x86:op_mov
                       && !ilabel
                       && car(ins[x86:i_arg2]) == x86:lreg
                       && dget(dnext(iscan))[x86:il_label] == jcc[x86:i_arg1]
                       && cmovcc_src?(ins[x86:i_arg1]))
                [
                  // jcc 0f; mov reg/mem,%reg; 0:
                  if (mc:verbose >= 3)
                    [
                      display("REPLACE jcc over mov with cmovcc: ");
                      x86:print_ins(jcc);
                      display("; ");
                      x86:print_ins(ins);
                    ];

                  [
                    | previ |
                    previ = dprev(iscan);
                    dremove!(iscan, ilist);
                    iscan = previ;
                  ];

                  jcc[x86:i_op] = x86:op_cmovcc;
                  jcc[x86:i_arg1] = ins[x86:i_arg1];
                  jcc[x86:i_arg2] = cons(
                    x86:lidx,
                    cons(
                      cdr(ins[x86:i_arg2]),
                      jcc[x86:i_arg2] ^ 1)); // inverse cc

                  if (mc:verbose >= 3)
                    [
                      display(" -> ");
                      x86:print_ins(jcc);
                      newline();
                    ];

                  jcc = null;
                ]
              else
		jcc = null; // back to nojcc
	    ]
	  else // jcc/jmp state
	    [
	      if (ilabel == jcc[x86:i_arg1]) // peephole time!
		[
		  ++mc:jccjmp_count;
		  if (mc:verbose >= 3)
		    [
		      display("PEEPHOLE jcc over jmp: ");
		      x86:print_ins(jcc);
		      newline();
		    ];
		  jcc[x86:i_arg1] = dget(jmp)[x86:il_ins][x86:i_arg1];
		  jcc[x86:i_arg2] = jcc[x86:i_arg2] ^ 1; // reverse sense.
		  // remove jmp
		  dremove!(jmp, ilist);
		];

              jmp = jcc = null; // back to nojcc
	    ];

          noreturn = null;
          if (op == x86:op_ret
              || (op == x86:op_callrel && ins[x86:i_arg2]))
            noreturn = iscan;

	  iscan = dnext(iscan);
	  if (iscan == ilist) exit 0;
	];

      if (aliased_label)
	remove_aliases(ilist);
      ilist
    ];

  resize_branches = fn (ilist)
    loop
      [
	set_offsets(ilist);
	if (!increase_branches(ilist)) exit 0
      ];

  set_offsets = fn (ilist)
    dreduce(fn (il, offset)
	    [
	      il[x86:il_offset] = offset;
	      offset + ins_size(il[x86:il_ins])
	    ], 0, ilist);

  increase_branches = fn (ilist)
    dreduce(fn (il, change)
            [
              | ins, delta, op |

              ins = il[x86:il_ins];
              op = ins[x86:i_op];

              if (op == x86:op_jmp || op == x86:op_jcc)
                [
                  delta = ins[x86:i_arg1][x86:l_ins][x86:il_offset] -
                    (il[x86:il_offset] + 2);

                  if (!byte?(delta))
                    [
                      change = true;
                      ins[x86:i_op] =
                        if (op == x86:op_jmp) x86:op_jmp32
                        else x86:op_jcc32;
                    ]
                ];
              change
            ], false, ilist);

  assemble = fn (ilist)
    [
      | size, last, code, info |

      last = dget(dprev(ilist));
      size = last[x86:il_offset] + ins_size(last[x86:il_ins]);
      mc:nbytes += size;
      code = make_string(size);

      info = make_vector(mc:a_info_fields);

      | last_line, linenos |

      dforeach(fn (il) [
        | line |
        line = mc:loc_line(il[x86:il_loc]);
        if (line > 0 && line != last_line)
          [
            linenos = (il[x86:il_offset] . line) . linenos;
            last_line = line;
          ];
        ins_gen(code, il[x86:il_ins], il[x86:il_offset], info);
      ], ilist);
      linenos = (size . last_line) . linenos;
      info[mc:a_linenos] = vreverse!(list_to_vector(linenos));

      code . info
    ];

  immval8 = fn (arg)
    [
      | i |

      i = cdr(arg);
      if (integer?(i)) i
      else if (byte?(car(i))) // arithmetic fails on large numbers
	2 * car(i) + cdr(i)
      else
        0xbad; // something that is false for byte? !
    ];
  ubyte? = fn (x) !(x & ~0xff);
  uimmval8 = fn (arg)
    [
      | i |

      i = cdr(arg);
      if (integer?(i)) i
      else if (byte?(car(i))) // arithmetic fails on large numbers
	2 * car(i) + cdr(i)
      else
        0xbad; // something that is false for ubyte? !
    ];
  immediate? = fn (arg)
    [
      | m |

      m = car(arg);
      m == x86:limm || m == x86:lglobal_constant || m == x86:lfunction ||
      m == x86:lcst || m == x86:lclosure ||
      (m == x86:lglobal && pair?(cdr(arg)))
    ];
  immediate8? = fn (arg) car(arg) == x86:limm && byte?(immval8(arg));

  uimmediate8? = fn (arg) car(arg) == x86:limm && ubyte?(uimmval8(arg));

  imm_zero? = fn (arg) car(arg) == x86:limm && immval8(arg) == 0;

  register? = fn (arg) car(arg) == x86:lreg;
  regval = cdr;

  byte_reg? = fn (arg) !register?(arg) || regval(arg) <= x86:reg_ebx;

  // Returns: The size of the encoding of effective address 'arg'
  easize = fn (arg)
    [
      | m, a |

      m = car(arg); a = cdr(arg);
      if (m == x86:lreg) 1
      else if (m == x86:lidx)
	[
	  | r, offset |

	  r = car(a); offset = cdr(a);
	  if (r == x86:reg_esp)
	    [
	      if (offset == 0) 2
	      else if (byte?(offset)) 3
	      else 6
	    ]
	  else if (offset == 0 && r != x86:reg_ebp) 1
	  else if (integer?(offset) && byte?(offset)) 2
	  else 5
	]
      else if (m == x86:lridx)
	[
	  | d |

	  d = cdddr(a);

	  if (d == 0 && caddr(a) != x86:reg_ebp) 2
	  else if (byte?(d)) 3
	  else 6
	]
      else if (m == x86:lqidx)
	6
      else if (m == x86:lprimitive || m == x86:lspecial || m == x86:lglobal
               || m == x86:lclosure)
        5
      else fail();
    ];

  isize = make_vector(x86:ops);

  isize[x86:op_push] = fn (a1, a2)
    if (immediate8?(a1)) 2
    else if (immediate?(a1)) 5
    else if (register?(a1)) 1
    else 1 + easize(a1);
  isize[x86:op_pop] = isize[x86:op_push];
  isize[x86:op_leave] = fn (a1, a2) 1;

  isize[x86:op_call] = fn (a1, a2)
    if (immediate?(a1)) 5
    else 1 + easize(a1);
  isize[x86:op_callrel_prim] = isize[x86:op_callrel] = fn (a1, a2) 5;
  isize[x86:op_ret] = fn (a1, a2) 1;
  isize[x86:op_jmp] = fn (a1, a2) 2;
  isize[x86:op_jmp32] = fn (a1, a2) 5;
  isize[x86:op_jcc] = fn (a1, a2) 2;
  isize[x86:op_jcc32] = fn (a1, a2) 6;
  isize[x86:op_lea] = fn (a1, a2)
    if (car(a1) == x86:lspecial || car(a1) == x86:lprimitive)
      5
    else
      1 + easize(a1);

  isize[x86:op_movbyte] = isize[x86:op_mov] = fn (a1, a2)
    if (imm_zero?(a1) && register?(a2))
      isize[x86:op_xor](a2, a2)
    else if (immediate?(a1) || car(a1) == x86:lseclev)
      if (register?(a2)) 5
      else 5 + easize(a2)
    else if (register?(a1)) 1 + easize(a2)
    else if (register?(a2)) 1 + easize(a1)
    else [ dformat("%s %s\n", a1, a2); fail(); ];

  gsize2b = fn (a1, a2)
    if (immediate?(a1))
      if (eax?(a2))
        5
      else
        5 + easize(a2)
    else if (register?(a1)) 1 + easize(a2)
    else if (register?(a2)) 1 + easize(a1)
    else fail();

  gsize2 = fn (a1, a2)
    if (immediate8?(a1)) 2 + easize(a2)
    else gsize2b(a1, a2);

  gsize2u = fn (a1, a2)
    if (uimmediate8?(a1) && byte_reg?(a2))
      if (eax?(a2))
        2
      else
        2 + easize(a2)
    else gsize2b(a1, a2);

  gsize2byte = fn (a1, a2)
    if (immediate8?(a1)) 2 + easize(a2)
    else if (register?(a1)) 1 + easize(a2)
    else if (register?(a2)) 1 + easize(a1)
    else fail();

  isize[x86:op_add]     = gsize2;
  isize[x86:op_adc]     = gsize2;
  isize[x86:op_sub]     = gsize2;
  isize[x86:op_cmp]     = gsize2;
  isize[x86:op_or]      = gsize2;
  isize[x86:op_xor]     = gsize2;
  isize[x86:op_and]     = gsize2;
  isize[x86:op_cmpbyte] = gsize2byte;
  isize[x86:op_andbyte] = gsize2byte;
  isize[x86:op_orbyte]  = gsize2byte;

  isize[x86:op_test]    = gsize2u;

  isize[x86:op_imul] = fn (a1, a2)
    [
      | imm2 |
      @(,x86:lidx . (_ . imm2)) = a2;
      (if (byte?(imm2)) 2 else 5) + easize(a1)
    ];

  gsize1b = fn (a1, a2) 1 + easize(a1);
  gsize1 = fn (a1, a2)
      if (register?(a1)) 1
      else 1 + easize(a1);

  isize[x86:op_dec] = gsize1;
  isize[x86:op_inc] = gsize1;
  isize[x86:op_neg] = gsize1b;
  isize[x86:op_not] = gsize1b;

  gsizebit = fn (a1, a2)
    if (immediate8?(a1)) 3 + easize(a2)
    else 2 + easize(a2);

  isize[x86:op_bt] = gsizebit;
  isize[x86:op_bts] = gsizebit;
  isize[x86:op_btr] = gsizebit;
  isize[x86:op_btc] = gsizebit;

  gsizeshift = fn (a1, a2)
    if (immval8(a1) == 1) 1 + easize(a2)
    else 2 + easize(a2);

  isize[x86:op_shl] = gsizeshift;
  isize[x86:op_shr] = gsizeshift;
  isize[x86:op_sar] = gsizeshift;

  isize[x86:op_setcc] = fn (a1, a2) 2 + easize(a2);
  isize[x86:op_cmovcc] = fn (a1, a2) 2 + easize(a1);
  isize[x86:op_movzxbyte] = fn (a1, a2) 2 + easize(a1);
  isize[x86:op_movzxword] = fn (a1, a2) 2 + easize(a1);
  isize[x86:op_xchg] = fn (a1, a2)
    if (register?(a1))
      if (eax?(a2) || eax?(a1) && register?(a2)) 1
      else 1 + easize(a2)
    else 1 + easize(a1);
  isize[x86:op_op16] = fn (a1, a2) 1;

  ins_size = fn (ins)
    isize[ins[x86:i_op]](ins[x86:i_arg1], ins[x86:i_arg2]);

  igen = make_vector(x86:ops);

  igen[x86:op_push] = fn (code, a1, a2, o, info)
    if (immediate8?(a1))
      [
	code[o] = 0x6a;
	code[o + 1] = immval8(a1);
      ]
    else if (immediate?(a1))
      [
	code[o] = 0x68;
	setimm(code, o + 1, a1, info);
      ]
    else if (register?(a1))
      [
	code[o] = 0x50 | regval(a1);
      ]
    else
      [
	code[o] = 0xff;
	setea1(code, o + 1, 6, a1, info);
      ];

  igen[x86:op_pop] = fn (code, a1, a2, o, info)
    if (register?(a1))
      [
	code[o] = 0x58 | regval(a1);
      ]
    else
      [
	code[o] = 0x8f;
	setea1(code, o + 1, 0, a1, info);
      ];

  igen[x86:op_leave] = fn (code, a1, a2, o, info)
    code[o] = 0xc9;

  igen[x86:op_call] = fn (code, a1, a2, o, info)
    if (immediate?(a1))
      [
        code[o] = 0xe8;
        setimm(code, o + 1, a1, info);
      ]
    else
      [
        code[o] = 0xff;
        setea1(code, o + 1, 2, a1, info);
      ];

  igen[x86:op_callrel] = fn (code, a1, a2, o, info)
    [
      code[o] = 0xe8;
      info[mc:a_builtins] = (a1 . o + 1) . info[mc:a_builtins];
    ];

  igen[x86:op_callrel_prim] = fn (code, a1, a2, o, info)
    [
      code[o] = 0xe8;
      info[mc:a_rel_primitives] = (a1 . o + 1) . info[mc:a_rel_primitives];
    ];

  igen[x86:op_ret] = fn (code, a1, a2, o, info)
    code[o] = 0xc3;

  igen[x86:op_jmp] = fn (code, a1, a2, o, info)
    [
      code[o] = 0xeb;
      code[o + 1] = a1[x86:l_ins][x86:il_offset] - (o + 2);
    ];

  igen[x86:op_jmp32] = fn (code, a1, a2, o, info)
    [
      code[o] = 0xe9;
      setword(code, o + 1, a1[x86:l_ins][x86:il_offset] - (o + 5));
    ];

  igen[x86:op_jcc] = fn (code, a1, a2, o, info)
    [
      code[o] = 0x70 | a2;
      code[o + 1] = a1[x86:l_ins][x86:il_offset] - (o + 2);
    ];

  igen[x86:op_jcc32] = fn (code, a1, a2, o, info)
    [
      code[o] = 0x0f;
      code[o + 1] = 0x80 | a2;
      setword(code, o + 2, a1[x86:l_ins][x86:il_offset] - (o + 6));
    ];

  igen[x86:op_lea] = fn (code, a1, a2, o, info)
    if (car(a1) == x86:lspecial || car(a1) == x86:lprimitive)
      [
        assert(register?(a2));
        // this is mov $value,reg
        code[o] = 0xb8 | regval(a2);
        setimm(code, o + 1, a1, info);
      ]
    else
      [
        code[o] = 0x8d;
        setea1(code, o + 1, regval(a2), a1, info);
      ];

  igen[x86:op_mov] = fn (code, a1, a2, o, info)
    if (imm_zero?(a1) && register?(a2))
      igen[x86:op_xor](code, a2, a2, o, info)
    else if (immediate?(a1))
      if (register?(a2))
	[
	  code[o] = 0xb8 | regval(a2);
	  setimm(code, o + 1, a1, info);
	]
      else
	[
	  code[o] = 0xc7;
	  o = setea1(code, o + 1, 0, a2, info);
	  setimm(code, o, a1, info);
	]
    else if (register?(a1))
      [
	// missing move from eax -> abs32 case
	code[o] = 0x89;
	setea1(code, o + 1, regval(a1), a2, info);
      ]
    else if (car(a1) == x86:lseclev)
      [
        assert(register?(a2));
        code[o] = 0xb8 | regval(a2);
	info[mc:a_seclevs] = (cdr(a1) . (o + 1)) . info[mc:a_seclevs]
      ]
    else if (register?(a2))
      [
	// missing move from eax <- abs32 case
	code[o] = 0x8b;
	setea1(code, o + 1, regval(a2), a1, info);
      ]
    else fail();

  igen[x86:op_movbyte] = fn (code, a1, a2, o, info)
    if (register?(a1))
      [
	code[o] = 0x88;
	setea1(code, o + 1, regval(a1), a2, info);
      ]
    else fail();

  generic2 = fn (uimm8op, imm8op, imm32op, immextraop,
                 imm32eaxop, regmemop, memregop)
    fn (code, a1, a2, o, info)
      if (uimm8op != null && uimmediate8?(a1) && byte_reg?(a2))
        [
          if (eax?(a2))
            [
              code[o] = car(uimm8op);
              ++o;
            ]
          else
            [
              code[o] = cdr(uimm8op);
              o = setea1(code, o + 1, immextraop, a2, info);
            ];
          code[o] = uimmval8(a1);
        ]
      else if (imm8op != null && immediate8?(a1))
	[
	  code[o] = imm8op;
	  o = setea1(code, o + 1, immextraop, a2, info);
	  code[o] = immval8(a1);
	]
      else if (immediate?(a1))
	[
          if (eax?(a2))
            [
              code[o] = imm32eaxop;
              ++o
            ]
          else
            [
              code[o] = imm32op;
              o = setea1(code, o + 1, immextraop, a2, info);
            ];
	  setimm(code, o, a1, info);
	]
      else if (register?(a1))
	[
	  code[o] = regmemop;
	  setea1(code, o + 1, regval(a1), a2, info);
	]
      else if (register?(a2))
	[
	  code[o] = memregop;
	  setea1(code, o + 1, regval(a2), a1, info);
	]
      else fail();

  generic2byte = fn (imm8op, immextraop, regmemop, memregop)
    fn (code, a1, a2, o, info)
      if (immediate8?(a1))
	[
	  // missing al op imm8 case
	  code[o] = imm8op;
	  o = setea1(code, o + 1, immextraop, a2, info);
	  code[o] = immval8(a1);
	]
      else if (register?(a1))
	[
	  code[o] = regmemop;
	  setea1(code, o + 1, regval(a1), a2, info);
	]
      else if (register?(a2))
	[
	  code[o] = memregop;
	  setea1(code, o + 1, regval(a2), a1, info);
	]
      else fail();

  // cannot use 8-bit unsigned ops for math since flags aren't set
  // as they would for 32-bit ops
  generic2math = fn (n) generic2(null, 0x83, 0x81, n, 0x05 | (n << 3),
                                 0x01 | (n << 3), 0x03 | (n << 3));
  generic2mathbyte =
    fn (n) generic2byte(0x80, n, (n << 3), 0x02 | (n << 3));

  igen[x86:op_adc] = generic2math(2);
  igen[x86:op_add] = generic2math(0);
  igen[x86:op_and] = generic2math(4);
  igen[x86:op_cmp] = generic2math(7);
  igen[x86:op_or]  = generic2math(1);
  igen[x86:op_sub] = generic2math(5);
  igen[x86:op_xor] = generic2math(6);

  igen[x86:op_andbyte] = generic2mathbyte(4);
  igen[x86:op_cmpbyte] = generic2mathbyte(7);
  igen[x86:op_orbyte] = generic2mathbyte(1);

  igen[x86:op_test] = generic2(0xa8 . 0xf6, null, 0xf7, 0, 0xa9, 0x85, null);

  igen[x86:op_imul] = fn (code, a1, a2, o, info)
    [
      | imm2, i8? |
      @(,x86:lidx . (a2 . imm2)) = a2;
      code[o] = if (i8? = byte?(imm2)) 0x6b else 0x69;
      o = setea1(code, o + 1, a2, a1, info);
      if (i8?)
        code[o] = imm2
      else
        setword(code, o, imm2)
    ];

  generic1 = fn (memop, memextraop, regop)
    fn (code, a1, a2, o, info)
      if (regop != null && register?(a1))
	code[o] = regop | regval(a1)
      else
	[
	  code[o] = memop;
	  setea1(code, o + 1, memextraop, a1, info);
	];

  igen[x86:op_dec] = generic1(0xff, 1, 0x48);
  igen[x86:op_inc] = generic1(0xff, 0, 0x40);
  igen[x86:op_neg] = generic1(0xf7, 3, null);
  igen[x86:op_not] = generic1(0xf7, 2, null);

  genericbit = fn (n)
    fn (code, a1, a2, o, info)
      if (immediate8?(a1))
	[
	  code[o] = 0x0f;
	  code[o + 1] = 0xba;
	  o = setea1(code, o + 2, n, a2, info);
	  code[o] = immval8(a1);
	]
      else
	[
	  code[o] = 0x0f;
	  code[o + 1] = 0x83 | (n << 3);
	  setea1(code, o + 2, regval(a1), a2, info);
	];

  igen[x86:op_bt] = genericbit(4);
  igen[x86:op_bts] = genericbit(5);
  igen[x86:op_btr] = genericbit(6);
  igen[x86:op_btc] = genericbit(7);

  genericshift = fn (op)
    fn (code, a1, a2, o, info)
      [
	| n |

	n = immval8(a1);
	if (n == 1)
	  [
	    code[o] = 0xd1;
	    setea1(code, o + 1, op, a2, info);
	  ]
	else
	  [
	    code[o] = 0xc1;
	    o = setea1(code, o + 1, op, a2, info);
	    code[o] = n;
	  ]
      ];

  igen[x86:op_shl] = genericshift(4);
  igen[x86:op_shr] = genericshift(5);
  igen[x86:op_sar] = genericshift(7);

  igen[x86:op_setcc] = fn (code, a1, a2, o, info)
    [
      code[o] = 0x0f;
      code[o + 1] = 0x90 | a1;
      setea1(code, o + 2, 0, a2, info);
    ];
  igen[x86:op_cmovcc] = fn (code, a1, a2, o, info)
    [
      | r2, cc |
      @(,x86:lidx . (r2 . cc)) = a2;
      code[o] = 0x0f;
      code[o + 1] = 0x40 | cc;
      setea1(code, o + 2, r2, a1, info);
    ];

  igen[x86:op_movzxbyte] = fn (code, a1, a2, o, info)
    [
      code[o] = 0x0f;
      code[o + 1] = 0xb6;
      setea1(code, o + 2, regval(a2), a1, info);
    ];

  igen[x86:op_movzxword] = fn (code, a1, a2, o, info)
    [
      code[o] = 0x0f;
      code[o + 1] = 0xb7;
      setea1(code, o + 2, regval(a2), a1, info);
    ];

  igen[x86:op_xchg] = fn (code, a1, a2, o, info)
    [
      if (!register?(a1))
	[
	  | t |
	  t = a1; a1 = a2; a2 = t;
	];
      if (eax?(a2))
        code[o] = 0x90 | regval(a1)
      else if (eax?(a1) && register?(a2))
        code[o] = 0x90 | regval(a2)
      else
        [
          code[o] = 0x87;
          setea1(code, o + 1, regval(a1), a2, info);
        ]
    ];
  igen[x86:op_op16] = fn (code, a1, a2, o, info)
    code[o] = 0x66;


  ins_gen = fn (code, ins, offset, info)
    igen[ins[x86:i_op]](code, ins[x86:i_arg1], ins[x86:i_arg2], offset, info);

  setea1 = fn (code, o, extraop, arg, info)
    // Effects: Codes the effective address for arg1 in code at offset o
    //   extraop is a 3-bit value representing the extra opcode information
    //   info remembers any relocation info for arg
    [
      | m, a |

      m = car(arg); a = cdr(arg);
      if (m == x86:lreg)
        [
	  code[o] = 0xc0 | (extraop << 3) | a;
	  o + 1
	]
      else if (m == x86:lidx)
	[
	  | r, offset |

	  r = car(a); offset = cdr(a);
	  if (r == x86:reg_esp)
	    [
	      if (offset == 0)
		[
		  code[o] = (extraop) << 3 | 4;
		  code[o + 1] = 0x24;
		  o + 2
		]
	      else if (byte?(offset))
		[
		  code[o] = (extraop) << 3 | 0x44;
		  code[o + 1] = 0x24;
		  code[o + 2] = offset;
		  o + 3
		]
	      else
		[
		  code[o] = (extraop) << 3 | 0x84;
		  code[o + 1] = 0x24;
		  setword(code, o + 2, offset);
		  o + 6
		]
	    ]
	  else if (offset == 0 && r != x86:reg_ebp)
	    [
	      code[o] = (extraop << 3) | r;
	      o + 1
	    ]
	  else if (integer?(offset) && byte?(offset))
	    [
	      code[o] = 0x40 | (extraop << 3) | r;
	      code[o + 1] = offset;
	      o + 2
	    ]
	  else
	    [
	      code[o] = 0x80 | (extraop << 3) | r;
              if (function?(offset))
                offset = offset(false);
              setword(code, o + 1, offset);
	      o + 5
	    ]
	]
      else if (m == x86:lridx)
	[
	  | d, r1, r2, s, mod |

	  d = cdddr(a);
	  r1 = car(a);
	  s = cadr(a);
	  r2 = caddr(a);

	  if (d == 0 && r2 != x86:reg_ebp) mod = 0
	  else if (byte?(d)) mod = 1
	  else mod = 2;

	  if (s == 1) s = 0
	  else if (s == 2) s = 1
	  else if (s == 4) s = 2
	  else if (s == 8) s = 3
	  else fail();

	  if (r1 == x86:reg_esp) fail();

	  code[o] = (mod << 6) | (extraop << 3) | 4;
	  code[o + 1] = (s << 6) | (r1 << 3) | r2;
	  if (mod == 0)
	    o + 2
	  else if (mod == 1)
	    [
	      code[o + 2] = d;
	      o + 3
	    ]
	  else
	    [
	      setword(code, o + 2, d);
	      o + 6
	    ]
	]
      else if (m == x86:lqidx)
	[
	  | d, r1, s |

	  d = cddr(a);
	  r1 = car(a);
	  s = cadr(a);

	  if (s == 1) s = 0
	  else if (s == 2) s = 1
	  else if (s == 4) s = 2
	  else if (s == 8) s = 3
	  else fail();

	  if (r1 == x86:reg_esp) fail();

	  code[o] = (extraop << 3) | 4;
	  code[o + 1] = (s << 6) | (r1 << 3) | 5;
	  setword(code, o + 2, d);
	  o + 6
	]
      else if (m == x86:lprimitive)
	[
	  code[o] = 5 | (extraop << 3);
	  info[mc:a_primitives] = (a . o + 1) . info[mc:a_primitives];
	  o + 5
	]
      else if (m == x86:lspecial)
	[
	  code[o] = 5 | (extraop << 3);
	  info[mc:a_builtins] = (a . o + 1) . info[mc:a_builtins];
	  o + 5
	]
      else if (m == x86:lglobal)
	[
	  code[o] = 0x80 | x86:reg_globals | (extraop << 3);
	  info[mc:a_globals] = (a . o + 1) . info[mc:a_globals];
	  o + 5
	]
      else fail();
    ];

  setimm = fn (code, o, arg, info)
    // Requires: arg = limmediate . n, where n is an integer or a . b,
    //   representing 2 * a + b. b == 0 or b == 1.
    //   OR: arg = lglobal_constant . name
    //   OR: arg = lcst . mudlle constant
    // Effects: Encodes arg in code at offset o
    // Modifies: code
    [
      | m, a |

      m = car(arg); a = cdr(arg);
      if (m == x86:lglobal_constant)
	info[mc:a_kglobals] = (a . o) . info[mc:a_kglobals]
      else if (m == x86:lcst)
	info[mc:a_constants] = (a . o) . info[mc:a_constants]
      else if (m == x86:lfunction)
        if (string?(a))
          info[mc:a_kglobal_code] = (a . o) . info[mc:a_kglobal_code]
        else
          // positive offset is the code
          info[mc:a_subfns] = (a . o) . info[mc:a_subfns]
      else if (m == x86:lclosure)
        // negative offset is parsed by link() and converted into a closure
	info[mc:a_subfns] = (a . -o) . info[mc:a_subfns]
      else if (m == x86:lspecial)
        info[mc:a_builtins] = (a . o) . info[mc:a_builtins]
      else if (m == x86:lprimitive)
        info[mc:a_primitives] = (a . o) . info[mc:a_primitives]
      else if (m == x86:lglobal)
        info[mc:a_globals] = (a . o) . info[mc:a_globals]
      else if (m == x86:limm)
	[
	  if (pair?(a)) // 2*n or 2*n+1
	    [
	      | n |

	      n = car(a);
	      code[o + 3] = n >> 23;
	      code[o + 2] = n >> 15;
	      code[o + 1] = n >> 7;
	      code[o + 0] = (n << 1) | cdr(a);
	    ]
	  else
	    setword(code, o, a);
	]
      else fail();
    ];

  setword = fn (code, offset, word)
    [
      code[offset + 3] = word >> 24; // signed!
      code[offset + 2] = word >> 16;
      code[offset + 1] = word >> 8;
      code[offset + 0] = word;
    ];

  x86:reset_counters();

];
