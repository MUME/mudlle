library a68k // The 68k assembler
requires system, sequences, dlist, m68k, misc
defines m68k:assemble
[
  | resize_branches, set_offsets, increase_branches, remove_aliases, assemble,
    ins_size, ea_size, mode1, mode2, mode3, basic_code, basic_code_imm, ins_gen,
    ea_code, ea_extension, setword |

  m68k:assemble = fn "68kcode -> 68kasm" (fcode)
    [
      | ilist |

      ilist = fcode[0];
      remove_aliases(ilist);
      resize_branches(ilist);
      assemble(ilist)
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
	      il[m68k:il_offset] = offset;
	      offset + ins_size(il[m68k:il_ins])
	    ], 0, ilist);

  increase_branches = fn (ilist)
    [
      | change |

      change = false;
      dforeach(fn (il)
	       [
		 | ins, delta, size |

		 ins = il[m68k:il_ins];

		 if (ins[m68k:i_type] == m68k:ins_branch &&
		     ins[m68k:i_size] == m68k:sbyte)
		   [
		     delta = ins[m68k:i_label][m68k:l_ins][m68k:il_offset] -
		       (il[m68k:il_offset] + 2);

		     if (delta < -128 || delta >= 128 || delta == 0)
		       [
			 change = true;
			 ins[m68k:i_size] = m68k:sword;
		       ]
		   ]
	       ], ilist);
      change
    ];
		       

  remove_aliases = fn (ilist)
    // Types: ilist: list of 68k instructions
    // Effects: Removes aliased labels from ilist
    dforeach(fn (il)
	     [
	       | ins, label, nlabel |
	       ins = il[m68k:il_ins];

	       if (ins[m68k:i_type] == m68k:ins_branch)
		 [
		   label = ins[m68k:i_label];
		   while (vector?(nlabel = label[m68k:l_alias])) label = nlabel;
		   ins[m68k:i_label] = label;
		 ]
	     ], ilist);


  assemble = fn (ilist)
    [
      | size, last, code, info |

      last = dget(dprev(ilist));
      size = last[m68k:il_offset] + ins_size(last[m68k:il_ins]);
      code = make_string(size);

      info = vector(null, null, null, null, null, null, null);
      dforeach(fn (il) ins_gen(code, il[m68k:il_ins], il[m68k:il_offset], info),
	       ilist);

      code . info
    ];

  // low-level instruction generation

  ins_size = fn (ins)
    [
      | type, ea1, ea2, size |

      type = ins[m68k:i_type];
      ea1 = ins[m68k:i_arg1];
      ea2 = ins[m68k:i_arg2];
      size = ins[m68k:i_size];

      if (type == m68k:ins_move) 2 + ea_size(ea1, size) + ea_size(ea2, size)
      else if (type == m68k:ins_jump || type == m68k:ins_jsr ||
	       type == m68k:ins_clr) 2 + ea_size(ea1, size)
      else if (type == m68k:ins_exchange || type == m68k:ins_rts) 2
      else if (type == m68k:ins_branch)
        if (size == m68k:sbyte) 2 else 4
      else if (type == m68k:ins_compare || type == m68k:ins_and ||
	       type == m68k:ins_add || type == m68k:ins_subtract)
        if (ea2[m68k:ea_class] == m68k:lregister) 2 + ea_size(ea1, size)
	else  2 + ea_size(ea1, size) + ea_size(ea2, size)
      else fail()
    ];

  ea_size = fn (ea, isize)
    [
      | class |

      class = ea[m68k:ea_class];

      if (class == m68k:lregister) 0
      else if (class == m68k:limmediate) '[2 2 4][isize]
      else if (class == m68k:lindexed) 2
      else if (class == m68k:lbuiltin) 4
      else if (class == m68k:lglobal || class == m68k:lglobal_offset) 2
      else if (class == m68k:lfunction || class == m68k:lconstant ||
	       class == m68k:lglobal_constant) 4
      else fail()
    ];

  mode1 = vector(0 << 6, 1 << 6, 2 << 6);
  mode2 = vector(0, 3 << 6, 7 << 6);
  mode3 = vector(1 << 12, 3 << 12, 2 << 12);
  basic_code = vector(11 << 12, // cmp
		      4 << 12 | 14 << 8 | 12 << 4, // jmp
		      4 << 12 | 14 << 8 | 8 << 4, // jsr
		      6 << 12, // bcc
		      12 << 12 | 1 << 8, // exg
		      4 << 12 | 14 << 8 | 7 << 4 | 5, // rts
		      0, // move
		      12 << 12, // and
		      4 << 12 | 2 << 8, // clr
		      13 << 12, // add
		      9 << 12); // sub

  basic_code_imm = vector(12 << 8, // cmp
			  0, 0, 0, 0, 0, 0,
			  2 << 8, // and
			  0,
			  6 << 8,  // add
			  4 << 8); // sub

  ins_gen = fn (code, ins, offset, info)
    [
      | type, ea1, ea2, size |

      type = ins[m68k:i_type];
      ea1 = ins[m68k:i_arg1];
      ea2 = ins[m68k:i_arg2];
      size = ins[m68k:i_size];

      if (type == m68k:ins_move) 
	[
	  | dea |

	  dea = ea_code(ea2);
	  dea = (dea & 7) << 3 | dea >> 3;

	  setword(code, offset, basic_code[type] | mode3[size] | dea << 6 |
		  ea_code(ea1));
	  offset = ea_extension(code, offset + 2, ea1, size, info);
	  ea_extension(code, offset, ea2, size, info)
	]
      else if (type == m68k:ins_jump || type == m68k:ins_jsr) 
	[
	  setword(code, offset, basic_code[type] | mode1[size] | ea_code(ea1));
	  ea_extension(code, offset + 2, ea1, size, info)
	]
      else if (type == m68k:ins_clr)
	[
	  | eacode |

	  eacode = ea_code(ea1);
	  if (eacode >= m68k:reg_a0 && eacode <= m68k:reg_a7) // use suba an,an
	    setword(code, offset, 8 << 12 | eacode << 9 | mode2[size] | eacode)
	  else
	    [
	      setword(code, offset, basic_code[type] | mode1[size] | eacode);
	      ea_extension(code, offset + 2, ea1, size, info)
	    ]
	]
      else if (type == m68k:ins_exchange)
	[
	  | r1, r2 |

	  r1 = ea1[m68k:er_reg];
	  r2 = ea2[m68k:er_reg];
	  if (r1 < m68k:reg_a0 && r2 < m68k:reg_a0 ||
	      r1 >= m68k:reg_a0 && r2 >= m68k:reg_a0)
	    setword(code, offset, basic_code[type] | 1 << 6 | (r1 & 7) << 9 | r2)
	  else
	    [
	      if (r1 >= m68k:reg_a0)
		[
		  | t |
		  t = r1; r1 = r2; r2 = t;
		];
	      setword(code, offset, basic_code[type] | 1 << 7 | r1 << 9 | r2)
	    ]
	]
      else if (type == m68k:ins_rts)
	setword(code, offset, basic_code[type])
      else if (type == m68k:ins_branch)
        if (size == m68k:sbyte)
	  setword(code, offset, basic_code[type] | ins[m68k:i_cond] << 8 |
		  (ins[m68k:i_label][m68k:l_ins][m68k:il_offset] - (offset + 2)) & 255)
	else
	  [
	    setword(code, offset, basic_code[type] | ins[m68k:i_cond] << 8);
	    setword(code, offset + 2,
		    (ins[m68k:i_label][m68k:l_ins][m68k:il_offset] - (offset + 2)) & 65535)
	  ]
      else if (type == m68k:ins_compare || type == m68k:ins_and ||
	       type == m68k:ins_add || type == m68k:ins_subtract)
        if (ea2[m68k:ea_class] == m68k:lregister) 
	  [
	    | r2 |

	    r2 = ea2[m68k:er_reg];
	    setword(code, offset, basic_code[type] | ea_code(ea1) |
		    (if (r2 < m68k:reg_a0) mode1 else mode2)[size] | (r2 & 7) << 9);
	    ea_extension(code, offset + 2, ea1, size, info);
	  ]
	else
	  [
	    setword(code, offset, basic_code_imm[type] | mode1[size] | ea_code(ea2));
	    offset = ea_extension(code, offset + 2, ea1, size, info);
	    ea_extension(code, offset, ea2, size, info)
	  ]
      else fail()
    ];

  ea_code = fn (ea)
    [
      | class |

      class = ea[m68k:ea_class];

      if (class == m68k:lregister) ea[m68k:er_reg]
      else if (class == m68k:limmediate) 60
      else if (class == m68k:lindexed) 40 | (ea[m68k:ei_reg] & 7)
      else if (class == m68k:lbuiltin) 57 // absolute
      else if (class == m68k:lglobal) 40 | (m68k:reg_globals & 7)
      else if (class == m68k:lfunction || class == m68k:lconstant ||
	       class == m68k:lglobal_constant || class == m68k:lglobal_offset)
	60 // immediate
      else fail()
    ];

  ea_extension = fn (code, offset, ea, isize, info)
    [
      | class |

      class = ea[m68k:ea_class];

      if (class == m68k:lregister) offset
      else if (class == m68k:limmediate)
	if (isize == m68k:slongword)
	  [
	    setword(code, offset, ea[m68k:ek_value] >> 16);
	    setword(code, offset + 2, ea[m68k:ek_value] & 65535);
	    offset + 4
	  ]
	else
	  [
	    setword(code, offset, ea[m68k:ek_value]);
	    offset + 2
	  ]
      else if (class == m68k:lindexed)
	[
	  setword(code, offset, ea[m68k:ei_offset]);
	  offset + 2
	]
      else if (class == m68k:lbuiltin)
	[
	  // remember builtin offsets
	  info[0] = (ea[m68k:eb_builtin] . offset) . info[0];
	  offset + 4
	]
      else if (class == m68k:lfunction)
	[
	  // remember function offsets
	  info[2] = (ea[m68k:ef_function] . offset) . info[2];
	  offset + 4
	]
      else if (class == m68k:lglobal || class == m68k:lglobal_offset)
	[
	  // remember global var offsets
	  info[3] = (ea[m68k:eg_name] . offset) . info[3];
	  offset + 2
	]
      else if (class == m68k:lglobal_constant)
	[
	  // remember global var offsets
	  info[4] = (ea[m68k:eg_name] . offset) . info[4];
	  offset + 4
	]
      else if (class == m68k:lconstant)
	[
	  // remember constant offsets
	  info[1] = (ea[m68k:ec_value] . offset) . info[1];
	  offset + 4
	]
      else fail()
    ];

  setword = fn (code, offset, word)
    [
      code[offset] = word >> 8;
      code[offset + 1] = word & 255;
    ];
];
