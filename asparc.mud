library asparc // The sparc assembler
requires system, dlist, msparc, sequences
defines sparc:assemble
reads mc:verbose
writes nfilled, nunfilled_branch, nunfilled_call, nslots, nins
[
  | set_offsets, remove_aliases, assemble, basic_code, ins_gen, setword,
    mapoffsets!, intreg, high_bit, schedule |

  nfilled = nunfilled_branch = nunfilled_call = nslots = nins = 0;

  sparc:assemble = fn "sparcode -> sparcasm" (fcode)
    [
      | ilist |

      ilist = fcode[0];
      fcode[0] = ilist = schedule(ilist);
      if (mc:verbose >= 4)
	[
	  sparc:ins_list(fcode);
	  newline();
	];
      nins = nins + dlength(ilist);
      remove_aliases(ilist);
      set_offsets(ilist);

      assemble(ilist) .
        vector(mapoffsets!(fcode[2]),
	       mapoffsets!(fcode[3]),
	       mapoffsets!(fcode[4]),
	       mapoffsets!(fcode[5]),
	       mapoffsets!(fcode[6]),
	       mapoffsets!(fcode[7]))
    ];

  mapoffsets! = fn (offsets)
    [
      lforeach(fn (info) set_cdr!(info, cdr(info)[sparc:il_offset]), offsets);
      offsets
    ];

  set_offsets = fn (ilist)
    dreduce(fn (il, offset)
	    [
	      il[sparc:il_offset] = offset;
	      offset + 1 // all instructions are the same size, just count them
	    ], 0, ilist);

  remove_aliases = fn (ilist)
    // Types: ilist: list of sparc instructions
    // Effects: Removes aliased labels from ilist
    dforeach(fn (il)
	     [
	       | ins, label, nlabel |
	       ins = il[sparc:il_ins];

	       if (ins[sparc:i_type] == sparc:ins_branch)
		 [
		   label = ins[sparc:i_arg1];
		   while (vector?(nlabel = label[sparc:l_alias])) label = nlabel;
		   ins[sparc:i_arg1] = label;
		 ]
	     ], ilist);

  schedule = fn (ilist)
    // Types: ilist: list of sparc instructions
    // Effects: Does code scheduling on ilist and fills delay slots
    //   Currently very simple: fill delay slots after call from
    //   instruction before it (if possible).
    [
      | scan, candidate, delay_call, delay_jump, delay_branch |

      scan = ilist;
      candidate = false;

      loop
	[
	  | next, il, ins, itype, nop, ndelay_call, ndelay_branch, ndelay_jump,
	    ncandidate |

	  next = dnext(scan);
	  il = dget(scan);
	  ins = il[sparc:il_ins];
	  itype = ins[sparc:i_type];

	  nop = itype == sparc:ins_sethi && ins[sparc:i_arg2] == sparc:reg_g0;

	  ndelay_call = ndelay_branch = ndelay_jump = ncandidate = false;

	  if (delay_call && nop && candidate)
	    [
	      | cil |

	      // any instruction except ones that use o7 can move into a
	      // call delay slot. As we don't generate those, we don't
	      // bother to check for them.

	      // move candidate into delay slot
	      cil = dget(candidate);
	      // move any label to next instruction
	      if (cil[sparc:il_label])
		sparc:set_label(cil[sparc:il_label], dget(dnext(candidate)));

	      ilist = dremove!(candidate, ilist);
	      dremove!(scan, scan);
	      dcons!(dget(candidate), next);
	      nfilled = nfilled + 1;
	    ]
	  else
	    [
	      // candidates survive by default
	      ncandidate = candidate;

	      // can't move through a label
	      if (il[sparc:il_label]) ncandidate = false;

	      if (itype == sparc:ins_call)
		ndelay_call = true
	      else if (itype == sparc:ins_branch)
		// unconditional annulled branches have no delay slot ...
		ndelay_branch = 
		  !(ins[sparc:i_arg2] && ins[sparc:i_op] == sparc:balways)
	      else if (itype == sparc:ins_jmpl)
		ndelay_jump = true
	      else if (delay_branch || delay_call || delay_jump)
		// candidates don't survive past delay slots
		// also instruction in delay slot is not a candidate
		[
		  ncandidate = false;
		  if (nop)
		    if (delay_branch) nunfilled_branch = nunfilled_branch + 1
		    else if (delay_call) nunfilled_call = nunfilled_call + 1;
		  nslots = nslots + 1;
		]
	      else if (itype == sparc:ins_save || itype == sparc:ins_restore)
		// save & restore are not good instructions to attempt to move...
		ncandidate = false
	      else
		// all other instructions are candidates
		// (alu, load, store, trap, sethi)
		ncandidate = scan;
	    ];

	  delay_call = ndelay_call;
	  delay_branch = ndelay_branch;
	  delay_jump = ndelay_jump;
	  candidate = ncandidate;
	  
	  scan = next;
	  if (scan == ilist) exit ilist;
	]
    ];

  assemble = fn (ilist)
    [
      | size, last, code |

      last = dget(dprev(ilist));
      size = (1 + last[sparc:il_offset]) << 2;
      code = make_string(size);

      dforeach(fn (il) ins_gen(code, il[sparc:il_ins], il[sparc:il_offset]),
	       ilist);

      code
    ];

  // low-level instruction generation

  basic_code = sequence
    (2 << 30,			// alu
     3 << 30,			// load
     3 << 30,			// store
     0 << 30 | 4 << 22,		// sethi
     2 << 30 | 60 << 19,	// save
     2 << 30 | 61 << 19,	// restore
     2 << 30 | 58 << 19,	// trap
     0 << 30 | 2 << 22,		// branch
     1 << 30,			// call
     2 << 30 | 56 << 19);	// jmpl

  high_bit = sequence
    (true,			// alu
     true,			// load
     true,			// store
     false,			// sethi
     true,			// save
     true,			// restore
     true,			// trap
     false,			// branch
     false,			// call
     true);			// jmpl

  ins_gen = fn (code, ins, offset)
    [
      | type, op, arg1, arg2, arg3 |

      type = ins[sparc:i_type];
      op = ins[sparc:i_op];
      arg1 = ins[sparc:i_arg1];
      arg2 = ins[sparc:i_arg2];
      arg3 = ins[sparc:i_arg3];

      setword
	(code, offset << 2, high_bit[type],
	 if (type == sparc:ins_call)
	   basic_code[type] | arg1
	 else if (type == sparc:ins_sethi)
	   basic_code[type] | arg1 | car(arg2) << 25
	 else if (type == sparc:ins_trap)
	   basic_code[type] | car(arg1) << 14 | intreg(arg2) | op << 25
	 else if (type == sparc:ins_branch)
	   basic_code[type] | op << 25 | arg2 << 29 |
	   (arg1[sparc:l_ins][sparc:il_offset] - offset) & ((1 << 22) - 1)
	 else // default format for everything else
	   basic_code[type] |
	   op << 19 |
	   car(arg1) << 14 |
	   intreg(arg2) |
	   car(arg3) << 25)
    ];

  intreg = fn (arg)
    if (integer?(arg)) 1 << 13 | arg & ((1 << 13) - 1)
    else car(arg);

  setword = fn (code, offset, highbit, word)
    [
      code[offset] = (word >> 24) & 127 | (if (highbit) 128 else 0);
      code[offset + 1] = word >> 16;
      code[offset + 2] = word >> 8;
      code[offset + 3] = word;
    ];
];
