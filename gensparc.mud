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

library gensparc // Code generation for Sparc V8
requires system, sequences, misc, dlist,
  compiler, vars, ins3, msparc, inference, asparc

defines
  sparc:nscratch, sparc:ncaller, sparc:nregargs, sparc:ncallee,
  sparc:select_registers, sparc:mgen_preamble, sparc:mgen_instruction,
  sparc:migrate, sparc:gassemble

reads mc:verbose, mc:myself
writes nops_called, nops_inlined
[
  | object_flags, reg_argcount, reg_closure_in, regs_args_in, relop, setint,
    reg_extra_args_in, reg_result, reg_gclimit, reg_gcbase, regs_scratch,
    regs_caller, regs_callee, in_reg, get_reg, used_regs, extra_args,
    select_registers, mgen_return, relops, mgen_branch, builtins, reg_dest,
    mgen_compute, mgen_memory, mgen_closure, mgen_call, callop1, callop2,
    shuffle, nregargs, rev_relops, fetch1, fetch2, move, commute, mov, cmp,
    lvar, lregister, lindexed, lconstant, lfunction, lprimitive, zero,
    lglobal, lglobal_constant, object_offset, spillreg, resolve, clearint,
    reg_xcount, reg_scratch, trap_offset, min_mudlle13, max_mudlle13,
    reg_arg0, reg_arg1, reg_closure, reg_fp, reg_sp, reg_globals, round,
    reg_result_out, regs_args, minframe, argstart, mgen_trap, object_type,
    type_branch, get_type, typearg1, typearg2, inline1?, inline2?, setup_args,
    mgen_inline1, mgen_inline2, type_trap, makeint, intval, mfalse, mtrue,
    function_offset, call, call_closure, call_primitive, nop, ret, make_leaf,
    object_size, gtype, object_info, infotype, call_basic |

  nops_inlined = nops_called = 0;

  // Error n is signaled by trap trap_offset+n
  trap_offset = 16;

  // mudlle value rep info
  object_offset = // offset till actual start of mudlle objects
    if (OPTION_GCDEBUG) 12 else 8;
  function_offset = 32; // offset to start of machine code in code objects
  object_info = 4;
  object_type = 5;
  object_flags = 6;
  object_size = 0;

  // integers
  makeint = fn (n) n << 1 | 1;
  intval = fn (n) n >> 1;
  min_mudlle13 = intval(-(1 << 12)); // min mudlle int that fits in 13 bits
  max_mudlle13 = intval((1 << 12) - 1); // max mudlle int that fits in 13 bits
  mfalse = makeint(false);
  mtrue = makeint(true);

  argstart = 68;
  minframe = argstart + 4 * 6; // always leave space for 6 args

  round = fn (n, m) // round to power of 2
    ((n + (m - 1)) & ~(m - 1));

  // Calling convention
  // Note: difference with standard sparc convention:
  //  6th argument is on stack
  //  o5 contains the called closure
  nregargs = 5;
  reg_argcount = sparc:reg_g3; // same as reg_scratch, beware
  reg_closure = sparc:reg_i5;
  reg_closure_in = sparc:reg_o5; // when passed as parameter
  regs_args = //rprotect
    (sparc:reg_i0 . sparc:reg_i1 . sparc:reg_i2 . sparc:reg_i3 . sparc:reg_i4 . null);
  regs_args_in = //rprotect
    (sparc:reg_o0 . sparc:reg_o1 . sparc:reg_o2 . sparc:reg_o3 . sparc:reg_o4 . null);
  reg_result = sparc:reg_o0;
  reg_result_out = sparc:reg_i0;

  // Global regs
  zero = sparc:reg_g0;
  reg_xcount = sparc:reg_g1;
  reg_globals = sparc:reg_g2;
  reg_fp = sparc:reg_i6;
  reg_sp = sparc:reg_o6;
  // Arguments for builtins
  reg_arg0 = sparc:reg_l0;
  reg_arg1 = sparc:reg_l1;
  reg_scratch = sparc:reg_g3;

  // General regs
  regs_scratch = sequence
    (sparc:reg_l0);
  regs_caller = sequence
    (sparc:reg_o0, sparc:reg_o1, sparc:reg_o2, sparc:reg_o3,
     sparc:reg_o4, sparc:reg_o5);
  regs_callee = sequence
    (sparc:reg_i0, sparc:reg_i1, sparc:reg_i2, sparc:reg_i3,
     sparc:reg_i4, sparc:reg_l2, sparc:reg_l3, sparc:reg_l4,
     sparc:reg_l5, sparc:reg_l6, sparc:reg_l7);
  
  // nb of registers of each category available
  sparc:nscratch = fn (ifn) 1;
  sparc:ncaller = fn (ifn) 6;
  sparc:nregargs = fn (ifn) nregargs;
  sparc:ncallee = fn (ifn) 11;

  // sources & destinations for moves
  // (only lvar, lregister and lindexed are allowed as destinations)
  // (destination variables may not be global)
  lvar = 0;
  lregister = 1;
  lindexed = 2;
  lconstant = 3;
  lfunction = 4;
  lglobal = 5;
  lglobal_constant = 7;
  lprimitive = 8;

  in_reg = fn (var)
    var[mc:v_location] && var[mc:v_location][mc:v_lclass] == mc:v_lregister ||
    // null is "in" %g0
    var[mc:v_class] == mc:v_constant && var[mc:v_kvalue] == null;

  get_reg = fn (var)
    // null is "in" %g0
    if (var[mc:v_class] == mc:v_constant && var[mc:v_kvalue] == null)
      zero
    else if (in_reg(var)) var[mc:v_location][mc:v_lrnumber]
    else -1;

  // Common pseudo-instructions
  mov = fn (code, from, to) sparc:or(code, zero, from, to);
  cmp = fn (code, s1, s2) sparc:subcc(code, s1, s2, zero);
  nop = fn (fcode) sparc:sethi(fcode, 0, zero);
  ret = fn (fcode) sparc:jmpl(fcode, sparc:reg_i7, 8, zero);

  setint = fn (code, d) // set integer tag bit
    sparc:or(code, d, 1, d);

  clearint = fn (code, d) // clear integer tag bit
    sparc:andn(code, d, 1, d);

  get_type = fn (v) // minimalistic type inference ;-)
    [
      | t |

      if (t = mc:constant?(v)) t
      else itype_any
    ];

  extra_args = fn (ifn)
    // Returns: The number of cells needed in the stack frame
    //   for extra arguments (beyond the 6th)
    //   (space is always reserved for the 6th arg to follow the
    //   sparc calling convention)
    [
      | maxargs |

      // Note: The function called is included in the cargs list, hence an
      // offset of one in most computations
      maxargs = dreduce(fn (il, max)
			[
			  | ins, nargs |
			  ins = il[mc:il_ins];
			  if (ins[mc:i_class] == mc:i_call &&
			      (nargs = llength(ins[mc:i_cargs])) > max)
			    nargs
			  else
			    max
			], 1, ifn[mc:c_fvalue]);
      if (maxargs > nregargs + 2) maxargs - (nregargs + 2)
      else 0
    ];

  select_registers = fn (ifn, type, regs)
    [
      | select |

      select = fn (var)
        [
          | vloc |

          vloc = var[mc:v_location];

          if (vloc && vloc[mc:v_lclass] == mc:v_lregister &&
              vloc[mc:v_lrtype] == type)
            vloc[mc:v_lrnumber] = regs[vloc[mc:v_lrnumber]];
        ];

      lforeach(select, ifn[mc:c_flocals]);
      lforeach(select, ifn[mc:c_fclosure]);
    ];

  sparc:select_registers = fn (ifn, ainfo)
    // Effects: Selects registers for ifn
    // Returns: Information on allocation count
    [
      | regs, nargs, args, rargs, arg, aloc, offset, loffset,
	cvars, locals, max_extra, spill_offset |

      // select callee regs
      // default assignment:
      regs = vcopy(regs_callee);

      // arguments that are to live in callee vars should stay where they
      // started
      args = ifn[mc:c_fargs];
      rargs = regs_args;

      while (args != null && rargs != null)
	[
	  arg = car(args);
	  aloc = arg[mc:v_location];
	  if (mc:verbose >= 3)
	    [
	      display(format("checking %s", mc:svar(arg)));
	      newline();
	    ];
	  if (aloc && aloc[mc:v_lclass] == mc:v_lregister &&
	      aloc[mc:v_lrtype] == mc:reg_callee)
	    [
	      | creg, rarg |

	      rarg = car(rargs);
	      creg = aloc[mc:v_lrnumber];
	      if (mc:verbose >= 3)
		[
		  display(format("%s better in %s",
				 mc:svar(arg), rarg));
		  newline();
		];
	      if (regs[creg] != rarg) // not there
		[
		  | swap |
		  // adjust correspondence to make it work
		  // find where rarg is in regs:
		  swap = vector_index(rarg, regs);
		  regs[swap] = regs[creg];
		  regs[creg] = rarg;
		];
	    ];
	  args = cdr(args); rargs = cdr(rargs);
	];

      select_registers(ifn, mc:reg_callee, regs);

      // Allocate caller regs

      // support leaf routines:
      // if function has no spills and ncaller + ncallee <= 5
      //   then allocate caller regs from callee regs
      // (this means that the function will only use i0-i5 and so has
      // a chance of being leaf - see msparc)
      if (ainfo[3] == 0 && ainfo[1] + ainfo[2] <= 5)
	regs = list_to_vector(nth_pair(ainfo[2] + 1, vector_to_list(regs)))
      else
	regs = regs_caller;

      select_registers(ifn, mc:reg_caller, regs);

      // Maybe something cleverer should be done (try & reduce
      // stalls ?)
      select_registers(ifn, mc:reg_scratch, regs_scratch);

      // set/adjust offsets of all spilled variables
      cvars = ifn[mc:c_fclosure];
      offset = object_offset + 4; // skip over function
      while (cvars != null)
	[
	  | cvar, cvarloc |

	  cvar = car(cvars);
	  // myself is not present in closures, it is the closure
	  if (cvar[mc:v_cparent] == mc:myself)
	    cvar[mc:v_location] = vector(mc:v_lregister, mc:reg_callee, reg_closure)
	  else
	    [
	      cvarloc = cvar[mc:v_location];
	      if (cvarloc[mc:v_lclass] == mc:v_lspill)
		cvarloc[mc:v_lsoffset] = offset;
	      offset = offset + 4;
	    ];
	  cvars = cdr(cvars);
	];

      args = ifn[mc:c_fargs];
      offset = argstart;
      while (args != null)
	[
	  | argloc |

	  argloc = car(args)[mc:v_location];
	  if (argloc && argloc[mc:v_lclass] == mc:v_lspill &&
	      argloc[mc:v_lstype] == mc:spill_args)
	    argloc[mc:v_lsoffset] = offset;
	  offset = offset + 4;
	  args = cdr(args);
	];

      // spilled locals are stored after the extra args
      max_extra = extra_args(ifn);
      loffset = argstart + 4 * (nregargs + 1 + max_extra);

      locals = ifn[mc:c_flocals];
      while (locals != null)
	[
	  | localoc |

	  localoc = car(locals)[mc:v_location];
	  if (localoc[mc:v_lclass] == mc:v_lspill &&
	      localoc[mc:v_lstype] == mc:spill_spill)
	    localoc[mc:v_lsoffset] = 4 * localoc[mc:v_lsoffset] + loffset;
	  locals = cdr(locals);
	];

      vector(ainfo[0], ainfo[1], ainfo[2], ainfo[3], max_extra);
    ];

  sparc:migrate = fn (ifn, vars, notspilt, spilt, locals, temps)
    // Effetcs: Receives the grouping of variables used by the
    //   register allocator. May move some variables between the group to
    //   better suit the processor. May only strengthen needs ...
    //   On sparc: first 5 arguments are moved to notspilt (where they
    //   start)
    [
      lforeach
	(fn (arg)
	 [
	   set_bit!(notspilt, arg[mc:v_number]);
	   clear_bit!(spilt, arg[mc:v_number]);
	   clear_bit!(locals, arg[mc:v_number]);
	   clear_bit!(temps, arg[mc:v_number]);
	 ], 
	 list_first_n!(nregargs, lcopy(ifn[mc:c_fargs])));
    ];

  make_leaf = fn (code)
    // Effects: Makes code into a leaf routine, ie:
    //   Removes save, bcleargc. Transforms restore. Replaces i<n> by o<n>.
    [
      | ilist |

      // Remove save, bcleargc
      sparc:rem_instruction(code, sparc:get_instructions(code));
      sparc:rem_instruction(code, sparc:get_instructions(code));

      // Add argument count trap
      ilist = sparc:get_instructions(code);
      sparc:set_instruction(code, dnext(ilist));
      sparc:trap(code, sparc:bne, zero, error_wrong_parameters + trap_offset);
      sparc:set_instruction(code, ilist);

      // And convert all the instructions
      dforeach(fn (il) sparc:make_leaf(il[sparc:il_ins]), ilist);
    ];

  sparc:gassemble = fn (code)
    [
      if (sparc:leaf?(code)) make_leaf(code);
      sparc:assemble(code)
    ];

  sparc:mgen_preamble = fn (ifn, ainfo)
    [
      | code, ilist, argcheck, locals, cvars, rargs, n, offset, args, creg,
	framesize |

      code = sparc:new_code();
      ilist = ifn[mc:c_fvalue];

      // Allocate & clear stack frame
      framesize = round(minframe + (ainfo[3] + ainfo[4]) * 4, 16);
      sparc:save(code, reg_sp, -framesize, reg_sp);

      // Make argument vector for varargs functions
      if (ifn[mc:c_fvarargs])
	[
	  sparc:call_builtin(code, "bvarargs");
	  nop(code);
	];

      sparc:call_builtin(code, "bcleargc");

      // Clear Z if argument count incorrect (bcleargc will trap)
      argcheck = dget(ilist)[mc:il_ins];
      if (argcheck[mc:i_class] == mc:i_trap &&
	  argcheck[mc:i_top] == mc:trap_argcheck)
	[
	  cmp(code, reg_argcount, car(argcheck[mc:i_targs])[mc:v_kvalue]);
		   
	  // Remove trap ins
	  ifn[mc:c_fvalue] = ilist = dremove!(ilist, ilist);
	]
      else
	// No argcheck, set Z so that no trap is taken
	cmp(code, zero, 0);

      if (!ifn[mc:c_fvarargs]) sparc:set_leaf!(code, true); // ok so far ...

      // setup variables:

      // setup arguments, add indirection
      args = ifn[mc:c_fargs];
      rargs = regs_args;
      offset = argstart;
      while (args != null)
	[
	  | arg, loc, locarg |

	  arg = car(args);
	  args = cdr(args);

	  if (rargs != null)
	    [
	      loc = lregister;
	      locarg = car(rargs);
	      rargs = cdr(rargs);
	    ]
	  else
	    [
	      loc = lindexed;
	      locarg = reg_fp . offset;
	    ];

	  offset = offset + 4;

	  if (arg[mc:v_location]) // ignore unused arguments
	    [
	      // if variable is indirect create indirection record
	      if (arg[mc:v_indirect])
		[
		  sparc:call_builtin(code, "balloc_variable");
		  nop(code);
		  move(code, loc, locarg, lindexed, reg_arg0 . object_offset);
		  loc = lregister;
		  locarg = reg_arg0;
		];

	      // & copy to correct location
	      move(code, loc, locarg, lvar, arg);
	    ];
	];

      // unspill closure vars that need it
      cvars = ifn[mc:c_fclosure];
      offset = object_offset + 4;
      while (cvars != null)
	[
	  | cvar, cvarloc |

	  cvar = car(cvars);

	  if (cvar[mc:v_cparent] != mc:myself)
	    [
	      cvarloc = cvar[mc:v_location];
	      if (cvarloc[mc:v_lclass] == mc:v_lregister) // unspill closure entry
		move(code, lindexed, reg_closure . offset,
		     lregister, cvarloc[mc:v_lrnumber]);
	      offset = offset + 4;
	    ];
	  cvars = cdr(cvars);
	];

      // add indirection for other local variables
      locals = ifn[mc:c_flocals];
      while (locals != null)
	[
	  | local |

	  local = car(locals);
	  locals = cdr(locals);

	  if (local[mc:v_indirect] && !memq(local, ifn[mc:c_fargs]))
	    [
	      sparc:call_builtin(code, "balloc_variable");
	      nop(code);
	      move(code, lregister, zero, lindexed, reg_arg0 . object_offset);
	      move(code, lregister, reg_arg0, lvar, local);
	    ];
	];

      code
    ];
      

  sparc:mgen_instruction = fn (code, ifn, ainfo, il)
    [
      | ins, class |

      if (il[mc:il_label])
	sparc:label(code, il[mc:il_label][mc:l_mclabel]);

      ins = il[mc:il_ins];
      class = ins[mc:i_class];

      if (class == mc:i_compute) mgen_compute(code, ins)
      else if (class == mc:i_branch) mgen_branch(code, ins)
      else if (class == mc:i_trap) mgen_trap(code, ins)
      else if (class == mc:i_memory) mgen_memory(code, ins)
      else if (class == mc:i_closure) mgen_closure(code, ins)
      else if (class == mc:i_call) mgen_call(code, ins)
      else if (class == mc:i_return) mgen_return(code, ifn, ainfo, ins)
      else fail()
    ];

  mgen_trap = fn (code, ins)
    [
      | trap, nerror, args, types, arg1, type1, arg2, type2 |

      trap = ins[mc:i_top];
      nerror = ins[mc:i_tdest];
      args = ins[mc:i_targs];
      types = ins[mc:i_ttypes];
      if (args != null)
	[
	  arg1 = car(args);
	  if (!types) type1 = get_type(arg1)
	  else type1 = car(types);

	  if (cdr(args) != null)
	    [
	      arg2 = cadr(args);
	      if (!types) type2 = get_type(arg2)
	      else type2 = cadr(types);
	    ];
	];

      if (trap == mc:trap_always)
	sparc:ins_trap(code, sparc:balways, zero, trap_offset + nerror)
      else if (trap == mc:trap_argcheck)
	[
	  // Must be done in preamble actually, as argcount doesn't survive
	  cmp(code, reg_argcount, arg1[mc:v_kvalue]);
	  sparc:trap(code, sparc:bne, zero, trap_offset + nerror);
	]
      else if (trap == mc:trap_loop)
	[
	  sparc:subcc(code, reg_xcount, 1, reg_xcount);
	  sparc:trap(code, sparc:beq, zero, trap_offset + nerror);
	]
      else if (trap == mc:trap_global_write)
	[
	  // ignores nerror value
	  sparc:sethi(code, 0, reg_arg0);
	  sparc:register_global(code, arg1[mc:v_name]);
	  sparc:or(code, reg_arg0, 0, reg_arg0);
	  sparc:call_builtin(code, "bwglobal");
	  nop(code);
	]
      else if (trap == mc:trap_type)
	// ignores nerror value
	type_trap(code, arg2[mc:v_kvalue], fn () fetch1(code, arg1), type1)
      else fail();
    ];
	  

  mgen_return = fn (code, ifn, ainfo, ins)
    [
      | r |

      r = fetch1(code, ins[mc:i_rvalue]);
      ret(code);
      sparc:restore(code, r, 0, reg_result);
    ];

  relops = sequence
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     sparc:blt, sparc:bge, sparc:ble, sparc:bgt);

  rev_relops = sequence
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     sparc:bgt, sparc:ble, sparc:bge, sparc:blt);

  mgen_branch = fn (code, ins)
    [
      | op, dest, args, arg1, arg2, types, type1, type2, r1, r2, rboth |

      op = ins[mc:i_bop];
      dest = ins[mc:i_bdest][mc:l_mclabel];
      args = ins[mc:i_bargs];
      types = ins[mc:i_btypes];
      if (args != null)
	[
	  arg1 = car(args);
	  if (!types) type1 = get_type(arg1)
	  else type1 = car(types);

	  if (cdr(args) != null)
	    [
	      arg2 = cadr(args);
	      if (!types) type2 = get_type(arg2)
	      else type2 = cadr(types);
	    ];
	];

      if (op == mc:branch_always)
	sparc:branch(code, sparc:balways, dest, true)
      else if (op == mc:branch_true)
	[
	  r1 = fetch1(code, arg1);
	  cmp(code, r1, mfalse);
	  sparc:branch(code, sparc:bne, dest, false);
	  nop(code);
	]
      else if (op == mc:branch_false)
	[
	  r1 = fetch1(code, arg1);
	  cmp(code, r1, mfalse);
	  sparc:branch(code, sparc:beq, dest, false);
	  nop(code);
	]
      else if (op == mc:branch_or)
	[
	  rboth = fetch2(code, arg1, arg2, min_mudlle13, max_mudlle13);
	  sparc:sub(code, car(rboth), 1, reg_scratch);
	  sparc:or(code, reg_scratch, cdr(rboth), reg_scratch);
	  cmp(code, reg_scratch, mfalse);
	  sparc:branch(code, sparc:bne, dest, false);
	  nop(code);
	]
      else if (op == mc:branch_nor)
	[
	  rboth = fetch2(code, arg1, arg2, min_mudlle13, max_mudlle13);
	  sparc:sub(code, car(rboth), 1, reg_scratch);
	  sparc:or(code, reg_scratch, cdr(rboth), reg_scratch);
	  cmp(code, reg_scratch, mfalse);
	  sparc:branch(code, sparc:beq, dest, false);
	  nop(code);
	]
      else if (op == mc:branch_and)
	[
	  | l |

	  l = sparc:new_label(code);
	  r1 = fetch1(code, arg1);
	  cmp(code, r1, mfalse);
	  sparc:branch(code, sparc:beq, l, false);
	  r2 = fetch1(code, arg2);
	  cmp(code, r2, mfalse);
	  sparc:branch(code, sparc:bne, dest, false);
	  nop(code);
	  sparc:label(code, l);
	]
      else if (op == mc:branch_nand)
	[
	  r1 = fetch1(code, arg1);
	  cmp(code, r1, mfalse);
	  sparc:branch(code, sparc:beq, dest, false);
	  r2 = fetch1(code, arg2);
	  cmp(code, r2, mfalse);
	  sparc:branch(code, sparc:beq, dest, false);
	  nop(code);
	]
      else if (op == mc:branch_eq || op == mc:branch_ne)
	[
	  rboth = fetch2(code, arg1, arg2, min_mudlle13, max_mudlle13);
	  cmp(code, car(rboth), cdr(rboth));
	  sparc:branch(code, if (op == mc:branch_eq) sparc:beq else sparc:bne,
		       dest, false);
	  nop(code);
	]
      else if (op < mc:branch_type?) // relop
	[
	  if (arg1[mc:v_class] == mc:v_constant) // swap arguments
	    [
	      | t |
	      // swap arg1 and arg2 (sparc likes constants as arg2)
	      t = arg1; arg1 = arg2; arg2 = t;
	      t = type1; type1 = type2; type2 = t;
	      op = rev_relops[op];
	    ]
	  else
	    op = relops[op];

	  rboth = fetch2(code, arg1, arg2, min_mudlle13, max_mudlle13);
	  type_trap(code, type_integer, fn () car(rboth), type1);
	  type_trap(code, type_integer, fn () cdr(rboth), type2);
	  cmp(code, car(rboth), cdr(rboth));
	  sparc:branch(code, op, dest, false);
	  nop(code);
	]
      else if (op < mc:branch_ntype?)
	type_branch(code, op - mc:branch_type?, false, arg1, type1, dest)
      else
	type_branch(code, op - mc:branch_ntype?, true, arg1, type1, dest)
    ];

  builtins = sequence
    ("bor", "band", 0, 0,
     "bleq", "blne", "bllt", "blle", "blgt", "blge",
     "bbitor", "bbitxor", "bbitand", "bshift_left", "bshift_right",
     "badd", "bsubtract", "bmultiply", "bdivide", "bremainder",
     "bnegate", "bnot", "bbitnot", 0, 0, 0, 0, "bref", 0, 0, 0,
     "bcar", "bcdr");

  commute = sequence
    (true, true, 0, 0,
     false, false, false, false, false, false,
     true, true, true, false, false,
     false, false, true, false, false,
     0, 0, 0, 0, 0, 0, 0, false, 0, false, 0,
     0, 0);

  // Type of compute op arguments (uses type_xxx/stype_xxx sets)
  typearg1 = sequence
    (stype_any, stype_any, 0, 0,
     stype_any, stype_any, type_integer, type_integer, type_integer, type_integer,
     type_integer, type_integer, type_integer, type_integer, type_integer,
     stype_any, type_integer, type_integer, type_integer, type_integer, 
     type_integer, stype_any, type_integer, 0, 0, 0, 0, stype_any, 0, 0, 0,
     type_pair, type_pair, type_string, type_vector, type_integer);

  typearg2 = sequence
    (stype_any, stype_any, 0, 0,
     stype_any, stype_any, type_integer, type_integer, type_integer, type_integer,
     type_integer, type_integer, type_integer, type_integer, type_integer,
     stype_any, type_integer, type_integer, type_integer, type_integer, 
     0, 0, 0, 0, 0, 0, 0, stype_any, 0, 0, 0,
     0, 0, 0, 0, type_integer);

  reg_dest = fn (dvar)
    if (in_reg(dvar)) get_reg(dvar)
    else  reg_arg0;

  inline1? = fn (op, arg, type) 
    // Returns: True if op should be inlined on arg
    true;

  mgen_inline1 = fn (code, op, r, type, d) 
    // Effects: Generates code for d = op r
    //   d, r are registers
    if (op == mc:b_negate)
      [
	sparc:sub(code, zero, r, d);
	sparc:add(code, d, 2, d);
      ]
    else if (op == mc:b_bitnot)
      sparc:xorn(code, r, 1, d)
    else if (op == mc:b_not)
      [
	| skip |

	skip = sparc:new_label(code);
	cmp(code, r, mfalse);
	mov(code, mtrue, d);
	sparc:branch(code, sparc:bne, skip, true);
	mov(code, mfalse, d);
	sparc:label(code, skip)
      ]
    else if (op == mc:b_car)
      sparc:load_word(code, r, object_offset, d)
    else if (op == mc:b_cdr)
      sparc:load_word(code, r, object_offset + 4, d)
    else if (op == mc:b_slength)
      [
	sparc:load_word(code, r, object_size, d);
	sparc:sll(code, d, 1, d);
	sparc:sub(code, d, 2 * object_offset + 1, d);
      ]
    else if (op == mc:b_vlength)
      [
	sparc:load_word(code, r, object_size, d);
	sparc:srl(code, d, 1, d);
	sparc:sub(code, d, (object_offset >> 1) - 1, d);
      ]
    else fail();

  inline2? = fn (op, arg1, type1, arg2, type2)
    // Returns: True if op should be inlined on arg1, arg2
    if (op == mc:b_add) !(type1 & itype_string) || !(type2 & itype_string)
    else if (op == mc:b_ref) // may inline vector_ref or string_ref
      // inline if itype1 is known and index is int constant >= 0
      (!(type1 & itype_vector) || !(type1 & itype_string)) &&
      type2 == itype_integer &&
      (arg2[mc:v_class] != mc:v_constant ||
       arg2[mc:v_kvalue] >= 0 && arg2[mc:v_kvalue] <= 1020)
    else op != mc:b_divide && op != mc:b_remainder;

  relop = fn (code, branch, r1, r2, d)
    [
      | skip |
      
      skip = sparc:new_label(code);
      cmp(code, r1, r2);
      mov(code, mfalse, d);
      sparc:branch(code, branch, skip, true);
      mov(code, mtrue, d);
      sparc:label(code, skip);
    ];

  mgen_inline2 = fn (code, op, r1, type1, r2, type2, d)
    // Effects: Generates code for d = r1 op r2
    //   type1 & type2 are provided to help generate code for polymorphic ops
    //   r1, r2, d are registers
    if (op == mc:b_or)
      [
	| skip |

	// (v1-1) | v2 == false iff v1 == false && v2 == false
	// (this assumes that 2 is an illegal value)
	skip = sparc:new_label(code);
	sparc:sub(code, r1, 1, reg_scratch);
	sparc:or(code, reg_scratch, r2, d);
	cmp(code, d, mfalse);
	sparc:branch(code, sparc:bne, skip, true);
	mov(code, mtrue, d);
	sparc:label(code, skip);
      ]
    else if (op == mc:b_and)
      if (integer?(r2))
	  if (r2 == mfalse) mov(code, mfalse, d)
	  else // x and true
	    [
	      | skip |

	      skip = sparc:new_label(code);

	      cmp(code, r1, mfalse);
	      mov(code, mtrue, d);
	      sparc:branch(code, sparc:beq, skip, true);
	      mov(code, mfalse, d);
	      sparc:label(code, skip);
	    ]
      else
	[
	  | skip1, skip2 |

	  skip1 = sparc:new_label(code);
	  skip2 = sparc:new_label(code);
	  if (r2 == d) // oops
	    [
	      | t |
	      
	      t = r1; r1 = r2; r2 = t;
	    ];
	  // works even if r1 == r2 == d
	  cmp(code, r1, mfalse);
	  mov(code, mtrue, d);
	  sparc:branch(code, sparc:beq, skip1, true);
	  mov(code, mfalse, d);
	  sparc:label(code, skip1);
	  
	  cmp(code, r2, mfalse);
	  sparc:branch(code, sparc:beq, skip2, true);
	  mov(code, mfalse, d);
	  sparc:label(code, skip2);
	]
    else if (op == mc:b_eq) relop(code, sparc:beq, r1, r2, d)
    else if (op == mc:b_ne) relop(code, sparc:bne, r1, r2, d)
    else if (op == mc:b_lt) relop(code, sparc:blt, r1, r2, d)
    else if (op == mc:b_le) relop(code, sparc:ble, r1, r2, d)
    else if (op == mc:b_gt) relop(code, sparc:bgt, r1, r2, d)
    else if (op == mc:b_ge) relop(code, sparc:bge, r1, r2, d)
    else if (op == mc:b_bitor) sparc:or(code, r1, r2, d)
    else if (op == mc:b_bitand) sparc:and(code, r1, r2, d)
    else if (op == mc:b_bitxor)
      if (integer?(r2))
	sparc:xor(code, r1, r2 - 1, d)
      else
	[
	  sparc:xor(code, r1, r2, d);
	  setint(code, d);
	]
    else if (op == mc:b_shift_left)
      if (integer?(r2))
	[
	  | n |

	  n = intval(r2);
	  if (n == 1)
	    [
	      sparc:add(code, r1, r1, d);
	      sparc:sub(code, d, 1, d);
	    ]
	  else
	    [
	      sparc:andn(code, r1, 1, d);
	      sparc:sll(code, d, n & 31, d);
	      setint(code, d);
	    ]
	]
      else
	[
	  sparc:sra(code, r2, 1, reg_scratch);
	  sparc:andn(code, r1, 1, d);
	  sparc:sll(code, d, reg_scratch, d);
	  setint(code, d);
	]
    else if (op == mc:b_shift_right)
      if (integer?(r2))
	[
	  sparc:sra(code, r1, intval(r2) & 31, d);
	  setint(code, d);
	]
      else
	[
	  sparc:sra(code, r2, 1, reg_scratch);
	  sparc:sra(code, r1, reg_scratch, d);
	  setint(code, d);
	]
    else if (op == mc:b_add || op == mc:b_iadd) // integer only
      [
	if (op == mc:b_add)
	  [
	    type_trap(code, type_integer, fn () r1, type1);
	    type_trap(code, type_integer, fn () r2, type2);
	  ];
	if (integer?(r2))
	  sparc:add(code, r1, r2 - 1, d)
	else
	  [
	    sparc:add(code, r1, r2, d);
	    sparc:sub(code, d, 1, d);
	  ]
      ]
    else if (op == mc:b_subtract)
      if (integer?(r2))
	sparc:sub(code, r1, r2 - 1, d)
      else
	[
	  sparc:sub(code, r1, r2, d);
	  sparc:add(code, d, 1, d);
	]
    else if (op == mc:b_multiply)
      if (integer?(r2))
	[
	  sparc:smul(code, r1, intval(r2), d);
	  sparc:sub(code, d, intval(r2) - 1, d);
	]
      else
	[
	  sparc:sra(code, r2, 1, reg_scratch);
	  sparc:andn(code, r1, 1, d);
	  sparc:smul(code, d, reg_scratch, d);
	  setint(code, d);
	]
    else if (op == mc:b_ref) // simple cases only
      if (!(type1 & itype_string)) // vector
	if (integer?(r2)) // is >= 0
	  [
	    | offset |

	    type_trap(code, type_vector, fn () r1, type1);
	    // r2 is a positive constant
	    offset = ((r2 - 1) << 1) + object_offset;
	    sparc:load_word(code, r1, object_size, reg_scratch);
	    cmp(code, reg_scratch, offset);
	    sparc:trap(code, sparc:bleu, zero, error_bad_index + trap_offset);
	    sparc:load_word(code, r1, offset, d);
	  ]
	else
	  [
	    type_trap(code, type_vector, fn () r1, type1);
	    type_trap(code, type_integer, fn () r2, type2);
	    sparc:load_word(code, r1, object_size, reg_scratch);
	    sparc:sub(code, reg_scratch, object_offset, reg_scratch);
	    sparc:srl(code, reg_scratch, 1, reg_scratch);
	    cmp(code, r2, reg_scratch);
	    sparc:trap(code, sparc:bgeu, zero, error_bad_index + trap_offset);
	    sparc:sll(code, r2, 1, reg_scratch);
	    sparc:add(code, reg_scratch, object_offset - 2, reg_scratch);
	    sparc:load_word(code, r1, reg_scratch, d);
	  ]
      else if (!(type1 & itype_vector)) // string
	if (integer?(r2)) // is >= 0
	  [
	    | offset |

	    type_trap(code, type_string, fn () r1, type1);
	    // r2 is a positive constant
	    offset = (r2 >> 1) + object_offset;
	    sparc:load_word(code, r1, object_size, reg_scratch);
	    cmp(code, reg_scratch, offset + 1);
	    sparc:trap(code, sparc:bleu, zero, error_bad_index + trap_offset);
	    sparc:load_ubyte(code, r1, offset, d); // unsigned
	    sparc:sll(code, d, 1, d);
	    setint(code, d);
	  ]
	else
	  [
	    type_trap(code, type_string, fn () r1, type1);
	    type_trap(code, type_integer, fn () r2, type2);
	    sparc:load_word(code, r1, object_size, reg_scratch);
	    sparc:sub(code, reg_scratch, object_offset + 1, reg_scratch);
	    sparc:sll(code, reg_scratch, 1, reg_scratch);
	    cmp(code, r2, reg_scratch);
	    sparc:trap(code, sparc:bgeu, zero, error_bad_index + trap_offset);
	    sparc:srl(code, r2, 1, reg_scratch);
	    sparc:add(code, reg_scratch, object_offset, reg_scratch);
	    sparc:load_ubyte(code, r1, reg_scratch, d); // unsigned
	    sparc:sll(code, d, 1, d);
	    setint(code, d);
	  ]
    else fail();

  
  mgen_compute = fn (code, ins)
    [
      | args, arg1, arg2, r1, r2, rd, op, dest, types, type1, type2 |

      op = ins[mc:i_aop];
      dest = ins[mc:i_adest];
      args = ins[mc:i_aargs];
      types = ins[mc:i_atypes];
      if (args != null)
	[
	  arg1 = car(args);
	  if (!types) type1 = get_type(arg1)
	  else type1 = car(types);

	  if (cdr(args) != null)
	    [
	      arg2 = cadr(args);
	      if (!types) type2 = get_type(arg2)
	      else type2 = cadr(types);
	    ]
	];

      if (op == mc:b_assign)
	move(code, lvar, arg1, lvar, dest)
      else if (op == mc:b_cons)
	[
	  // If l0 needs preserving, use special alloc_cons
	  if (get_reg(arg1) == reg_arg0 || get_reg(arg2) == reg_arg0) fail();
	  sparc:call_builtin(code, "balloc_cons");
	  nop(code);
	  move(code, lvar, arg1, lindexed, reg_arg1 . object_offset);
	  move(code, lvar, arg2, lindexed, reg_arg1 . object_offset + 4);
	  move(code, lregister, reg_arg1, lvar, dest);
	]
      else if (arg2 == null) // 1-argument ops
	if (inline1?(op, arg1, type1))
	  [
	    r1 = fetch1(code, arg1);
	    rd = reg_dest(dest);
	    type_trap(code, typearg1[op], fn () r1, type1);
	    mgen_inline1(code, op, r1, type1, rd);
	    move(code, lregister, rd, lvar, dest);
	    nops_inlined = nops_inlined + 1;
	  ]
	else
	  [
	    nops_called = nops_called + 1;
	    callop1(code, builtins[op], arg1);
	    move(code, lregister, reg_arg0, lvar, dest);
	  ]
      else // 2-argument ops
	if (inline2?(op, arg1, type1, arg2, type2))
	  [
	    | t |

	    if (arg1[mc:v_class] == mc:v_constant && commute[op])
	      [
		// swap arg1 and arg2 (sparc likes constants as arg2)
		t = arg1; arg1 = arg2; arg2 = t;
		t = type1; type1 = type2; type2 = t;
	      ];
	    t = fetch2(code, arg1, arg2, min_mudlle13, max_mudlle13);
	    r1 = car(t); r2 = cdr(t);
	    rd = reg_dest(dest);
	    type_trap(code, typearg1[op], fn () r1, type1);
	    type_trap(code, typearg2[op], fn () r2, type2);
	    mgen_inline2(code, op, r1, type1, r2, type2, rd);
	    move(code, lregister, rd, lvar, dest);
	    nops_inlined = nops_inlined + 1;
	  ]
	else
	  [
	    nops_called = nops_called + 1;
	    callop2(code, builtins[op], commute[op], arg1, arg2);
	    move(code, lregister, reg_arg0, lvar, dest);
	  ]
    ];

  mgen_memory = fn (code, ins)
    [
      | array, areg, scalar, offset |

      array = ins[mc:i_marray];
      areg = get_reg(array);
      scalar = ins[mc:i_mscalar];
      offset = object_offset + 4 * ins[mc:i_mindex];

      areg = fetch1(code, array);
      if (ins[mc:i_mop] == mc:memory_read)
	move(code, lindexed, areg . offset, lvar, scalar)
      else // write
	move(code, lvar, scalar, lindexed, areg . offset);
    ];

  mgen_closure = fn (code, ins)
    [
      | cvars, f, offset, cdest |

      f = ins[mc:i_ffunction];
      cvars = lfilter(fn (cvar) cvar[mc:v_cparent] != mc:myself,
		      f[mc:c_fclosure]);

      sparc:call_builtin(code, "balloc_closure");
      mov(code, 4 * (1 + llength(cvars)) + object_offset, reg_arg1);
      move(code, lfunction, f, lindexed, reg_arg0 . object_offset);
      offset = object_offset + 4;
      cdest = ins[mc:i_fdest];
      while (cvars != null)
	[
	  | cvar |

	  cvar = car(cvars)[mc:v_cparent];
	  if (cvar == cdest) // place ourselves in closure
	    move(code, lregister, reg_arg0, lindexed, reg_arg0 . offset)
	  else
	    move(code, lvar, cvar, lindexed, reg_arg0 . offset);
	  offset = offset + 4;
	  cvars = cdr(cvars);
	];
      move(code, lregister, reg_arg0, lvar, cdest);
    ];

  setup_args = fn (code, args, nargs, nregargs)
    [
      | extra, moves, rargs, offset |

      if (nargs > nregargs)	// copy extra args to spill area for call
	[
	  offset = argstart + nregargs * 4;
	  extra = nth_pair(nregargs + 1, args);
	  while (extra != null)
	    [
	      move(code, lvar, car(extra), lindexed, reg_sp . offset);
	      offset = offset + 4;
	      extra = cdr(extra);
	    ];
	];
      
      // first nregargs go into regs_args_in
      rargs = regs_args_in;
      while (rargs != null && args != null)
	[
	  moves = (car(args) . car(rargs)) . moves;
	  args = cdr(args);
	  rargs = cdr(rargs);
	];

      moves
    ];

  call_basic = fn (code, called, args, primcallop)
    [
      | nargs, moves |

      // Move variables to argument slots
      moves = setup_args(code, args, nargs = llength(args), nregargs + 1);
      if (nargs >= 6) // 6th arg in o5
	moves = (nth(6, args) . sparc:reg_o5) . moves;
      shuffle(code, moves);
      
      move(code, lprimitive, called[mc:v_name], lregister, reg_scratch);
      sparc:call_builtin(code, primcallop);
      nop(code);

      reg_result
    ];
  
  call_primitive = fn (code, called, prim, args)
    [
      | flags |

      flags = primitive_flags(prim);
      call_basic(code, called, args, 
		 if (flags & OP_LEAF)
		   if (flags & OP_NOALLOC) "bcall_primitive_leaf_noalloc"
		   else "bcall_primitive_leaf"
		 else "bcall_primitive")
    ];
  
  call_closure = fn (code, called, args)
    [
      | nargs, moves |
      
      // Move arguments, function into their respective slots
      nargs = llength(args);
      moves = setup_args(code, args, nargs, nregargs);
      moves = (called . reg_closure_in) . moves;
      shuffle(code, moves);
      
      sparc:load_word(code, reg_closure_in, object_offset, reg_arg0);
      sparc:jmpl(code, reg_arg0, object_offset + function_offset, sparc:reg_o7);
      mov(code, nargs, reg_argcount);

      reg_result
    ];
  
  call = fn (code, called, args, callprimop)
    [
      | nargs, moves |
      
      // Move arguments, function into their respective slots
      nargs = llength(args);
      moves = setup_args(code, args, nargs, nregargs);
      moves = (called . reg_closure_in) . moves;
      shuffle(code, moves);

      sparc:call_builtin(code, callprimop);
      mov(code, nargs, reg_argcount);

      reg_result
    ];

  mgen_call = fn (code, ins)
    [
      | args, called, prim, dest, done |
      
      args = ins[mc:i_cargs];
      called = car(args);
      args = cdr(args);
      dest = ins[mc:i_cdest];
      done = false;
      
      // Optimise calls to global constants
      if (called[mc:v_class] == mc:v_global_constant)
	[
	  | f, t |
	  
	  f = global_value(called[mc:v_goffset]);
	  if (primitive?(f) && primitive_nargs(f) == llength(args))
	    done = call_primitive(code, called, f, args)
	  else if ((t = typeof(f)) == type_secure)
	    done = call(code, called, args, "bcall_secure")
	  else if (t == type_varargs)
	    done = call(code, called, args, "bcall_varargs")
	  else if (closure?(f))
	    done = call_closure(code, called, args);
	];

      if (!done) done = call(code, called, args, "bcall");

      // hack of the day: if dest is in reg_arg0, change it to use done.
      if (get_reg(dest) == reg_arg0)
	dest[mc:v_location][mc:v_lrnumber] = done
      else
	move(code, lregister, done, lvar, dest);
    ];


  type_branch = fn (code, type, reversed, arg, typeset, dest)
    // Effects: Generates:
    //   !reversed: if typeof(arg) == type goto dest
    //   reversed: if typeof(arg) != type goto dest
    //   typeset is inferred type information on arg
    [
      | mtype |

      mtype = mc:itypemap[type];
      
      if ((mtype & typeset) == itype_none) // is not of given type
	[
	  if (reversed) sparc:branch(code, sparc:balways, dest, true)
	]
      else if ((~mtype & typeset) == itype_none) // is of given type
	[
	  if (!reversed) sparc:branch(code, sparc:balways, dest, true)
	]
      else
	[
	  | r, success, commit, abort, fail |
	  
	  fail = sparc:new_label(code);
	  
	  // Handle the 3 cases that arise in positive/negative typechecks:
	  //   - value *not* of type (abort)
	  //     flow of control must not proceed (eg not pointer)
	  //   - value *of* type (success)
	  //   - end of type check, final test (commit)
	  if (reversed)
	    [
	      commit = fn (cc) 
		sparc:branch(code, cc ^ 8, dest, false); // ^ 8 reverses condition
	      
	      success = fn (cc)
		sparc:branch(code, cc, fail, false);
	      
	      abort = fn (cc)
		sparc:branch(code, cc, dest, false);
	    ]
	  else
	    [
	      commit = fn (cc) 
		sparc:branch(code, cc, dest, false);
	      
	      success = fn (cc)
		sparc:branch(code, cc, dest, false);
	      
	      abort = fn (cc)
		sparc:branch(code, cc, fail, false);
	    ];

	  // checks can now assume the !reversed case
	  
	  r = fetch1(code, arg);
	  
	  if (type == type_integer)
	    [
	      sparc:andcc(code, r, 1, zero);
	      commit(sparc:bne);
	      nop(code);
	    ]
	  else if (type == type_null)
	    [
	      sparc:orcc(code, r, 0, zero);
	      commit(sparc:beq);
	      nop(code);
	    ]
	  else if (type == stype_function)
	    [
	      if (typeset & itype_integer)
		[
		  sparc:andcc(code, r, 1, zero);
		  abort(sparc:bne);
		];
	      if (typeset & itype_null)
		[
		  sparc:orcc(code, r, 0, zero);
		  abort(sparc:beq);
		];
	      if (typeset & (itype_integer | itype_null)) nop(code);

	      sparc:load_ubyte(code, r, object_type, reg_scratch);
	      cmp(code, reg_scratch, type_closure);
	      success(sparc:beq);
	      // primitive, varargs & secure are contiguous
	      cmp(code, reg_scratch, type_primitive);
	      abort(sparc:bltu);
	      cmp(code, reg_scratch, type_secure);
	      commit(sparc:bleu);
	      nop(code);
	    ]
	  else if (type == stype_list)
	    [
	      if (typeset & itype_null)
		[
		  sparc:orcc(code, r, 0, zero);
		  success(sparc:beq);
		];
	      if (typeset & itype_integer)
		[
		  sparc:andcc(code, r, 1, zero);
		  abort(sparc:bne);
		];
	      if (typeset & (itype_integer | itype_null)) nop(code);

	      sparc:load_ubyte(code, r, object_type, reg_scratch);
	      cmp(code, reg_scratch, type_pair);
	      commit(sparc:beq);
	      nop(code);
	    ]
	  else			// generic type check
	    [
	      if (typeset & itype_null)
		[
		  sparc:orcc(code, r, 0, zero);
		  abort(sparc:beq);
		];
	      if (typeset & itype_integer)
		[
		  sparc:andcc(code, r, 1, zero);
		  abort(sparc:bne);
		];
	      if (typeset & (itype_integer | itype_null)) nop(code);

	      sparc:load_ubyte(code, r, object_type, reg_scratch);
	      cmp(code, reg_scratch, type);
	      commit(sparc:beq);
	      nop(code);
	    ];
	  
	  sparc:label(code, fail);
	];
    ];
  
  gtype = sequence // garbage type map
    (garbage_code, garbage_record, garbage_record, garbage_record, 
     garbage_permanent, garbage_permanent, garbage_permanent, 
     0, garbage_string, garbage_record, garbage_record, garbage_record, garbage_record, 
     garbage_record, 
     garbage_temp, garbage_temp, garbage_temp, 
     garbage_record, garbage_mcode, 0);

  infotype = fn (type) type | gtype[type] << 8;

  type_trap = fn (code, type, lazy_r, typeset)
    // Effects: Generate typecheck for value in register r, given type
    //   knowledge typeset.
    //   r may also be an integer constant
    //   summary: trap if !type
    [
      | trap, mtype |
      
      trap = fn (cc) 
	sparc:trap(code, cc, zero, trap_offset + error_bad_type);
      
      // if value cannot pass trap, trap unconditionally
      mtype = mc:itypemap[type];
      
      if ((mtype & typeset) == itype_none)
	trap(sparc:balways)
      // if value must pass trap, don't test
      else if ((~mtype & typeset) == itype_none)
	0
      else
	[
	  | r |
	      
	  r = lazy_r();
	      
	  if (type == type_integer)
	    [
	      sparc:andcc(code, r, 1, zero);
	      trap(sparc:beq);
	    ]
	  else if (type == type_null)
	    [
	      sparc:orcc(code, r, 0, zero);
	      trap(sparc:bne);
	    ]
	  else if (type == stype_function)
	    [
	      | ok |
		  
	      ok = sparc:new_label(code);
	      sparc:load_uhword(code, r, object_info, reg_scratch);
	      cmp(code, reg_scratch, infotype(type_closure));
	      sparc:branch(code, sparc:beq, ok, false);
	      // primitive, varargs, secure are contiuguous and have same
	      // garbage type
	      cmp(code, reg_scratch, infotype(type_primitive));
	      trap(sparc:bltu);
	      cmp(code, reg_scratch, infotype(type_secure));
	      trap(sparc:bgtu);
	      sparc:label(code, ok);
	    ]
	  else if (type == stype_list)
	    [
	      | ok |
	      
	      ok = sparc:new_label(code);
	      if (itype_null & typeset)
		[
		  sparc:orcc(code, r, 0, zero);
		  sparc:branch(code, sparc:beq, ok, false);
		  nop(code);
		];
		  
	      sparc:load_uhword(code, r, object_info, reg_scratch);
	      cmp(code, reg_scratch, infotype(type_pair));
	      trap(sparc:bne);
	      sparc:label(code, ok);
	    ]
	  else
	    [
	      // relies on unaligned reads, and reads from address
	      // null + object_info causing traps
	      sparc:load_uhword(code, r, object_info, reg_scratch);
	      cmp(code, reg_scratch, infotype(type));
	      trap(sparc:bne);
	    ];
	];
    ];
  
  fetch1 = fn (code, var)
    [
      if (in_reg(var)) get_reg(var)
      else
	[
	  move(code, lvar, var, lregister, reg_arg1);
	  reg_arg1
	]
    ];
  
  fetch2 = fn (code, var1, var2, min2, max2)
    [
      | cst, avail, r1, r2 |
      
      avail = reg_arg0 . reg_arg1 . null;
      if (in_reg(var1)) avail = ldelete!(get_reg(var1), avail);
      if (in_reg(var2)) avail = ldelete!(get_reg(var2), avail);
      
      // Select r1
      if (in_reg(var1)) r1 = get_reg(var1)
      else
	[
	  r1 = car(avail);
	  move(code, lvar, var1, lregister, r1);
	];
      avail = ldelete!(r1, avail);
      
      if (var1 == var2) r2 = r1
      else if (in_reg(var2)) r2 = get_reg(var2)
      else if (var2[mc:v_class] == mc:v_constant &&
	       integer?(cst = var2[mc:v_kvalue]) &&
	       cst >= min2 && cst <= max2) r2 = makeint(cst)
      else
	[
	  r2 = car(avail);
	  move(code, lvar, var2, lregister, r2);
	];
      
      r1 . r2
    ];
  
  callop1 = fn (code, builtin, arg)
    // Scratch register usage: reg_scratch
    [
      shuffle(code, list(vector(arg, reg_arg0)));
      sparc:call_builtin(code, builtin);
      nop(code);
    ];
  
  callop2 = fn (code, builtin, commutes, arg1, arg2)
    // Scratch register usage: reg_scratch
    [
      | switched |
      
      switched = false;
      if (get_reg(arg2) == reg_arg0 && commutes) // better to switch
	[
	  | t |
	  
	  switched = true;
	  t = arg1; arg1 = arg2; arg2 = t;
	];
      shuffle(code, list((arg1 . reg_arg0),
			 (arg2 . reg_arg1)));
      sparc:call_builtin(code, builtin);
      nop(code);
      switched
    ];
  
  
  shuffle = fn (code, moves)
    // Scratch register usage: reg_scratch
    [
      | copies, cstspills, regmoves, m, same_move, amove, m2, xfers,
	l1_used, l1_dest |
      
      same_move = fn (move2) car(amove) == car(move2);
      
      // find copies, constants, spills, etc
      m = moves;
      while (m != null)
	[
	  amove = car(m);
	  
	  // is amove same as a prior move ?
	  if ((m2 = lexists?(same_move, cstspills)) ||
	      (m2 = lexists?(same_move, regmoves)))
	    copies = (m2 . amove) . copies
	  else if (!in_reg(car(amove)))
	    cstspills = amove . cstspills
	  else
	    regmoves = amove . regmoves;
	  
	  m = cdr(m);
	];
      // regmoves is a summary of the actual register - register moves.
      // do these first.
      
      // a: build reg1 -> reg2 list from regmoves & remove vacuous moves
      xfers = lreduce
	(fn (move, xfers)
	 [
	   | dest |
	   
	   dest = get_reg(car(move));
	   if (dest == cdr(move)) xfers
	   else (dest . cdr(move)) . xfers
	 ], null, regmoves);
      
      // b: while xfers remain, try & do a move. if none possible
      //    move first reg to reg_scratch
      //    move it from reg_scratch next time no moves are possible
      //    (it'sparc destination should be free then)
      l1_used = false;
      while (xfers != null)
	[
	  amove = lexists?(fn (xfer) !assq(cdr(xfer), xfers), xfers);
	  if (amove)
	    [
	      mov(code, car(amove), cdr(amove));
	      xfers = ldelete!(amove, xfers);
	    ]
	  else
	    [
	      amove = car(xfers);
	      
	      if (l1_used)
		[
		  // reg for reg_scratch should be free
		  assert(!assq(l1_dest, xfers));
		  mov(code, reg_scratch, l1_dest)
		];
	      
	      l1_used = true;
	      // move first reg to reg_scratch
	      mov(code, car(amove), reg_scratch);
	      l1_dest = cdr(amove);
	      xfers = cdr(xfers);
	    ];
	];
      if (l1_used) mov(code, reg_scratch, l1_dest);
      
      // Set constants and spilled variables
      lforeach(fn (m) move(code, lvar, car(m), lregister, cdr(m)),
	       cstspills);
      
      // Make copies
      lforeach(fn (copy) move(code, lregister, cdar(copy), lregister, cddr(copy)),
	       copies);
    ];
  
  spillreg = sequence(reg_closure, reg_fp, reg_sp);
  
  resolve = fn (type, arg)
    [
      if (type == lvar)
	[
	  | loc |
	  
	  if (in_reg(arg))
	    lregister . get_reg(arg)
	  else if (loc = arg[mc:v_location])
	    lindexed . (spillreg[loc[mc:v_lstype]] . loc[mc:v_lsoffset])
	  else			// no register: global or constant
	    if (arg[mc:v_class] == mc:v_constant)
	      lconstant . arg[mc:v_kvalue]
	    else if (arg[mc:v_class] == mc:v_global ||
		     arg[mc:v_class] == mc:v_global_define)
	      lglobal . arg[mc:v_name]
	    else if (arg[mc:v_class] == mc:v_global_constant)
	      if (immutable?(global_value(arg[mc:v_goffset])))
		lglobal_constant . arg[mc:v_name]
	      else
		lglobal . arg[mc:v_name]
	    else fail()
	]
      else if (type == lconstant && arg == null)
	lregister . zero
      else
	type . arg
    ];
  
  move = fn (code, stype, source, dtype, dest)
    // Scratch register usage:
    //   none if destination is in a register
    //   if source is in a register:
    //     none if destination is in a register or indexed
    //     1 otherwise (reg_scratch) (ie dest is global)
    //   if neither source or dest is in a register:
    //     1 for source (reg_scratch)
    //     1 for destination if it is not indexed (reg_arg0) (ie dest is global)
    [
      | sea, dea, sscratch, dscratch |
      
      // lvar, lregister, lindexed, lfunction
      sea = resolve(stype, source);
      stype = car(sea); source = cdr(sea);
      dea = resolve(dtype, dest);
      dtype = car(dea); dest = cdr(dea);
      
      // limmediate, lconstant, lglobal/constant, lfunction
      // lregister, lindexed
      
      if (!equal?(sea, dea))
	[
	  // destination is register or indexed or global
	  if (stype == lregister)
	    if (dtype == lregister)
	      mov(code, source, dest)
	    else if (dtype == lindexed)
	      sparc:store_word(code, source, car(dest), cdr(dest))
	    else
	      [
		assert(dtype == lglobal);
		// Use reg_scratch as scratch register for global offset
		sparc:sethi(code, 0, reg_scratch);
		sparc:register_global(code, dest);
		sparc:or(code, reg_scratch, 0, reg_scratch);
		sparc:store_word(code, source, reg_globals, reg_scratch);
	      ]
	  else
	    [
	      // Put source in sscratch
	      // If destination is register, use that as scratch for source
	      if (dtype == lregister) sscratch = dest
	      else sscratch = reg_scratch;
	      
	      if (stype == lindexed)
		sparc:load_word(code, car(source), cdr(source), sscratch)
	      else if (stype == lconstant && integer?(source))
		[
		  | mint |
		  
		  mint = makeint(source);
		  
		  if (source >= min_mudlle13 && source <= max_mudlle13)
		    mov(code, mint, sscratch)
		  else
		    [
		      sparc:sethi(code, (source >> 9) & ((1 << 22) - 1), sscratch);
		      sparc:or(code, sscratch, mint & ((1 << 10) - 1), sscratch);
		    ]
		]
	      else
		[
		  sparc:sethi(code, 0, sscratch);
		  
		  // Note relocation info
		  if (stype == lconstant)
		    sparc:register_constant(code, source)
		  else if (stype == lfunction)
		    sparc:register_function(code, source)
		  else if (stype == lglobal)
		    sparc:register_global(code, source)
		  else if (stype == lglobal_constant)
		    sparc:register_global_constant(code, source)
		  else if (stype == lprimitive)
		    sparc:register_primitive(code, source)
		  else fail();
		  
		  sparc:or(code, sscratch, 0, sscratch);
		  
		  if (stype == lglobal)
		    sparc:load_word(code, reg_globals, sscratch, sscratch);
		];
	      
	      // If destination is indexed add store
	      if (dtype == lindexed)
		sparc:store_word(code, sscratch, car(dest), cdr(dest))
	      else if (dtype == lglobal)
		[
		  dscratch = reg_arg0;	// use reg_arg0 as scratch
		  sparc:sethi(code, 0, dscratch);
		  sparc:register_global(code, dest);
		  sparc:or(code, dscratch, 0, dscratch);
		  sparc:store_word(code, sscratch, reg_globals, dscratch);
		]
	    ]
	]
    ];
  
];
