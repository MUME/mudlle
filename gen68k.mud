/* 
 * Copyright (c) 1993-2004 David Gay
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

library gen68k // Code generation for 68k family
requires system, sequences, misc, dlist,
  compiler, vars, ins3, m68k
defines
  m68k:object_offset, m68k:nscratch, m68k:ncaller, m68k:nregargs, m68k:ncallee,
  m68k:select_registers, m68k:mgen_preamble, m68k:mgen_instruction

reads mc:verbose, mc:myself,
  // type inference stuff (somewhat optional)
  itype_integer, itype_pair
writes ninferred

[
  | object_flags, reg_argcount, reg_closure_in, regs_args_in,
    reg_extra_args_in, reg_result, reg_gclimit, reg_gcbase, regs_scratch,
    regs_caller, regs_callee, used_regs, extra_args, get_reg, in_reg,
    select_registers, mgen_return, relops, mgen_branch, builtins,
    mgen_compute, mgen_memory, mgen_closure, mgen_call, callop1, callop2,
    allocate, shuffle, nregargs, allocate_readonly, mgen_trap, mgen_type_check,
    mgen_ntype_check |

  // mudlle value rep info
  // offset till actual start of mudlle objects
  m68k:object_offset = 12;

  ninferred = 0;

  object_flags = 6;

  nregargs = 3;
  reg_argcount = m68k:reg_d0;
  reg_closure_in = m68k:reg_d1;
  regs_args_in = m68k:reg_d2 . m68k:reg_d3 . m68k:reg_d4 . null;
  reg_extra_args_in = m68k:reg_d5;
  reg_result = m68k:reg_d0;
  reg_gclimit = m68k:reg_d6;
  reg_gcbase = m68k:reg_d7;

  regs_scratch = vector(m68k:reg_d0, m68k:reg_d1, m68k:reg_a0);
  regs_caller = vector(m68k:reg_d2, m68k:reg_d3, m68k:reg_d4, m68k:reg_d5);
  regs_callee = vector(m68k:reg_a1, m68k:reg_a2, m68k:reg_a3, m68k:reg_a4);
  
  // nb of registers of each category available
  m68k:nscratch = fn (ifn) 3;
  m68k:ncaller = fn (ifn) 4;
  m68k:nregargs = fn (ifn) 
    [

      // if function has (nregargs + 1) arguments, they are all in registers
      // otherwise all arguments beyond the third are in a vector
      if (llength(ifn[mc:c_fargs]) > nregargs + 1)
	[
	  if (mc:verbose >= 3)
	    [
	      display("argsbase");
	      newline();
	    ];
	  ifn[mc:c_fmisc][mc:c_fm_argsbase] = true;
	  nregargs
	]
      else nregargs + 1
    ];

  m68k:ncallee = fn (ifn)
    [
      | ncallee |

      ncallee = 4;

      // reserve one for closure base
      // Would be better to consider closurebase as var and allocate it
      // with other vars, but ...
      if (ifn[mc:c_fclosure] != null)
	[
	  if (mc:verbose >= 3)
	    [
	      display("closurebase");
	      newline();
	    ];
	  ifn[mc:c_fmisc][mc:c_fm_closurebase] = true;
	  ncallee = ncallee - 1;
	];

      // and for argument base
      // Note: same comment as for closurebase
      if (ifn[mc:c_fmisc][mc:c_fm_argsbase]) ncallee = ncallee - 1;
      ncallee
    ];


  in_reg = fn (var)
    var[mc:v_location] && var[mc:v_location][mc:v_lclass] == mc:v_lregister;

  get_reg = fn (var)
    if (in_reg(var)) var[mc:v_location][mc:v_lrnumber]
    else -1;

  used_regs = fn (ifn, ainfo)
    [
      | ncallee_used, i, cregs |

      ncallee_used = ainfo[2];
      i = 0;
      while (i < ncallee_used)
	[
	  cregs = regs_callee[i] . cregs;
	  i = i + 1;
	];

      // rather hackish
      if (ifn[mc:c_fmisc][mc:c_fm_closurebase])
	cregs = m68k:reg_closure . cregs;
      if (ifn[mc:c_fmisc][mc:c_fm_argsbase])
	if (memq(m68k:reg_args, cregs))
	  cregs = m68k:reg_closure . cregs // correct for current defs
	else
	  cregs = m68k:reg_args . cregs;

      cregs
    ];

  extra_args = fn (ifn)
    // Returns: The number of cells needed in the spill vector
    //   for use as an argument passing vector
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
      if (maxargs > nregargs + 2) maxargs - (nregargs + 1)
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

  m68k:select_registers = fn (ifn, ainfo)
    // Effects: Selects registers for ifn
    // Returns: Information on allocation count
    [
      | regs, nargs, args, rargs, arg, aloc, offset,
	cvars, locals, max_extra, spill_offset |

      // select caller regs
      // default assignment:
      regs = vcopy(regs_caller);

      // arguments that are to live in caller vars should stay where they
      // started
      args = ifn[mc:c_fargs];
      rargs = regs_args_in;

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
	      aloc[mc:v_lrtype] == mc:reg_caller)
	    [
	      | creg, rarg |

	      rarg = car(rargs);
	      creg = aloc[mc:v_lrnumber];
	      if (mc:verbose >= 3)
		[
		  display(format("%s in %s, better in %s",
				 mc:svar(arg), creg, rarg));
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

      select_registers(ifn, mc:reg_caller, regs);

      // Allocate callee regs
      // reserve special purpose registers
      regs = regs_callee;
      if (ifn[mc:c_fmisc][mc:c_fm_closurebase])
	regs = vdelete(m68k:reg_closure, regs);
      if (ifn[mc:c_fmisc][mc:c_fm_argsbase])
	regs = vdelete(m68k:reg_args, regs);

      select_registers(ifn, mc:reg_callee, regs);

      // scratch registers are ignored, they get selected when the code
      // that defines them is generated <- CHANGED! not true any more
      select_registers(ifn, mc:reg_scratch, regs_scratch);

      // set/adjust offsets of all spilled variables
      cvars = ifn[mc:c_fclosure];
      offset = 1; // skip over function
      while (cvars != null)
	[
	  | cvarloc, cvar |

	  cvar = car(cvars);
	  // myself is not present in closures, it is the closure
	  if (cvar[mc:v_cparent] == mc:myself)
	    cvar[mc:v_location] = vector(mc:v_lregister, mc:reg_callee, m68k:reg_closure)
	  else
	    [
	      cvarloc = cvar[mc:v_location];
	      if (cvarloc[mc:v_lclass] == mc:v_lspill)
		cvarloc[mc:v_lsoffset] = offset;
	      offset = offset + 1;
	    ];
	  cvars = cdr(cvars);
	];

      args = ifn[mc:c_fargs];
      offset = -nregargs;
      while (args != null)
	[
	  | argloc |

	  argloc = car(args)[mc:v_location];
	  if (argloc && argloc[mc:v_lclass] == mc:v_lspill &&
	      argloc[mc:v_lstype] == mc:spill_args)
	    argloc[mc:v_lsoffset] = offset;
	  offset = offset + 1;
	  args = cdr(args);
	];

      // spilled locals are stored after the extra args
      max_extra = extra_args(ifn);

      locals = ifn[mc:c_flocals];
      while (locals != null)
	[
	  | localoc |

	  localoc = car(locals)[mc:v_location];
	  if (localoc[mc:v_lclass] == mc:v_lspill &&
	      localoc[mc:v_lstype] == mc:spill_spill)
	    localoc[mc:v_lsoffset] = localoc[mc:v_lsoffset] + max_extra;
	  locals = cdr(locals);
	];

      vector(ainfo[0], ainfo[1], ainfo[2], ainfo[3], max_extra);
    ];

  m68k:mgen_preamble = fn (ifn, ainfo)
    [
      | code, ilist, argcheck, locals, cvars, cregs, rargs, n, offset, args, creg |

      code = m68k:new_code();
      ilist = ifn[mc:c_fvalue];

      // start with the argument count check, which should be the first
      // instruction
      argcheck = dget(ilist)[mc:il_ins];
      if (argcheck[mc:i_class] == mc:i_trap &&
	  argcheck[mc:i_top] == mc:trap_argcheck)
	[
	  | l |

	  ifn[mc:c_fvalue] = ilist = dremove!(ilist, ilist);
	  m68k:compareimm(code, m68k:sword,
			  car(argcheck[mc:i_targs])[mc:v_kvalue],
			  m68k:lregister, reg_argcount);
	  l = m68k:new_label(code);
	  m68k:branch(code, m68k:beq, l);
	  m68k:jump(code, m68k:lbuiltin, "bbadargs");
	  m68k:label(code, l);
	]
      else if (ifn[mc:c_fvarargs]) // generate varargs prelude
	m68k:jsr(code, m68k:lbuiltin, "bvarargs");

      cregs = used_regs(ifn, ainfo);

      // skip spill(etc) stuff if not needed
      if (ainfo[4] > 0 || // extra args
	  ainfo[3] > 0 || // spill variables
	  cregs != null) // regs need saving
	[
	  // allocate our spill
	  allocate(code, m68k:reg_a0, 4 * (ainfo[4] + ainfo[3] + 1 + llength(cregs)),
		   garbage_record, type_internal);

	  // init spill:
	  //   set extra args to null
	  offset = m68k:object_offset;
	  n = ainfo[4];
	  while ((n = n - 1) >= 0)
	    [
	      m68k:move(code, m68k:slongword, m68k:limmediate, 0,
			m68k:lindexed, m68k:reg_a0 . offset);
	      offset = offset + 4;
	    ];

	  // set local spill entries to null
	  n = ainfo[3];
	  while ((n = n - 1) >= 0)
	    [
	      m68k:move(code, m68k:slongword, m68k:limmediate, 0,
			m68k:lindexed, m68k:reg_a0 . offset);
	      offset = offset + 4;
	    ];

	  // save callee-saved registers

	  m68k:move(code, m68k:slongword, m68k:lregister, m68k:reg_spill,
		    m68k:lindexed, m68k:reg_a0 . offset);
	  while (cregs != null)
	    [
	      offset = offset + 4;
	      m68k:move(code, m68k:slongword, m68k:lregister, car(cregs),
			m68k:lindexed, m68k:reg_a0 . offset);
	      cregs = cdr(cregs);
	    ];

	  m68k:move(code, m68k:slongword, m68k:lregister, m68k:reg_a0,
		    m68k:lregister, m68k:reg_spill);
	];
      // setup spill bases
      if (ifn[mc:c_fmisc][mc:c_fm_closurebase])
	m68k:move(code, m68k:slongword, m68k:lregister, reg_closure_in,
		  m68k:lregister, m68k:reg_closure);
      if (ifn[mc:c_fmisc][mc:c_fm_argsbase])
	m68k:move(code, m68k:slongword, m68k:lregister, reg_extra_args_in,
		  m68k:lregister, m68k:reg_args);


      // setup variables:

      // setup arguments, add indirection
      args = ifn[mc:c_fargs];
      rargs = regs_args_in;
      // special case: last arg instead of extra_args vector
      if (llength(args) == nregargs + 1)
	rargs = lappend(rargs, reg_extra_args_in . null);
      offset = m68k:object_offset - 4 * nregargs;
      while (args != null)
	[
	  | arg, loc, locarg |

	  arg = car(args);
	  args = cdr(args);

	  if (rargs != null)
	    [
	      loc = m68k:lregister;
	      locarg = car(rargs);
	      rargs = cdr(rargs);
	    ]
	  else
	    [
	      loc = m68k:lindexed;
	      locarg = m68k:reg_args . offset;
	    ];

	  offset = offset + 4;

	  if (arg[mc:v_location]) // ignore unused arguments
	    [
	      // if variable is indirect create indirection record
	      if (arg[mc:v_indirect])
		[
		  allocate(code, m68k:reg_a0, 4, garbage_record, type_variable);
		  m68k:move(code, m68k:slongword, loc, locarg,
			    m68k:lindexed, m68k:reg_a0 . m68k:object_offset);
		  loc = m68k:lregister;
		  locarg = m68k:reg_a0;
		];

	      // & copy to correct location
	      m68k:move(code, m68k:slongword, loc, locarg, m68k:lvar, arg);
	    ];
	];

      // unspill closure vars that need it
      cvars = ifn[mc:c_fclosure];
      offset = m68k:object_offset + 4;
      if (ifn[mc:c_fmisc][mc:c_fm_closurebase])
	creg = m68k:reg_closure
      else if (cvars != null)
	[
	  m68k:move(code, m68k:slongword, m68k:lregister, reg_closure_in,
		    m68k:lregister, m68k:reg_a0);
	  creg = m68k:reg_a0;
	];
      while (cvars != null)
	[
	  | cvarloc, cvar |

	  cvar = car(cvars);
	  // myself is not present in closures, it is the closure
	  if (cvar[mc:v_cparent] != mc:myself)
	    [
	      cvarloc = cvar[mc:v_location];
	      if (cvarloc[mc:v_lclass] == mc:v_lregister) // unspill closure entry
		m68k:move(code, m68k:slongword, m68k:lindexed, creg . offset,
			  m68k:lregister, cvarloc[mc:v_lrnumber]);
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
	      allocate(code, m68k:reg_a0, 4, garbage_record, type_variable);
	      m68k:move(code, m68k:slongword, m68k:limmediate, 0,
			m68k:lindexed, m68k:reg_a0 . m68k:object_offset);
	      m68k:move(code, m68k:slongword, m68k:lregister, m68k:reg_a0,
			m68k:lvar, local);
	    ];
	];

      code
    ];
      

  m68k:mgen_instruction = fn (code, ifn, ainfo, il)
    [
      | ins, class |

      if (il[mc:il_label])
	m68k:label(code, il[mc:il_label][mc:l_mclabel]);

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
      | trap, nerror, args |

      trap = ins[mc:i_top];
      nerror = ins[mc:i_tdest];
      args = ins[mc:i_targs];
      
      // trap_loop ignored
      if (trap == mc:trap_type)
	[
	  | ok |

	  ok = m68k:new_label(code);
	  mgen_type_check(code, cadr(args)[mc:v_kvalue], car(args), ok);
	  m68k:move(code, m68k:sword, m68k:limmediate, nerror,
		    m68k:lregister, m68k:reg_d0);
	  m68k:jump(code, m68k:lbuiltin, "berror");
	  m68k:label(code, ok);
	]
      else if (trap == mc:trap_global_write)
	[
	  // check that global is writable (hopefully rare ;-))
	  m68k:move(code, m68k:sword, m68k:lglobal_offset, car(args)[mc:v_name],
		    m68k:lregister, m68k:reg_d0);
	  m68k:jsr(code, m68k:lbuiltin, "bwglobal");
	];
    ];
	  

  mgen_return = fn (code, ifn, ainfo, ins)
    [
      | cregs, offset |

      m68k:move(code, m68k:slongword, m68k:lvar, ins[mc:i_rvalue],
		m68k:lregister, reg_result);

      // restore callee-saved registers
      cregs = used_regs(ifn, ainfo);
      if (cregs != null || ainfo[3] > 0 || ainfo[4] > 0) // spill base exist
	[
	  offset = m68k:object_offset + 4 * (ainfo[4] + ainfo[3]);
	  m68k:move(code, m68k:slongword, m68k:lregister, m68k:reg_spill,
		    m68k:lregister, m68k:reg_a0);
	  m68k:move(code, m68k:slongword, m68k:lindexed, m68k:reg_a0 . offset,
		    m68k:lregister, m68k:reg_spill);
	  while (cregs != null)
	    [
	      offset = offset + 4;
	      m68k:move(code, m68k:slongword, m68k:lindexed, m68k:reg_a0 . offset,
			m68k:lregister, car(cregs));
	      cregs = cdr(cregs);
	    ];
	];

      m68k:rts(code);
    ];

  relops = vector(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  m68k:blt, m68k:bge, m68k:ble, m68k:bgt);

  mgen_branch = fn (code, ins)
    [
      | op, dest, args, arg1, arg2 |


      op = ins[mc:i_bop];
      dest = ins[mc:i_bdest][mc:l_mclabel];
      args = ins[mc:i_bargs];
      if (args != null)
	[
	  arg1 = car(args);
	  if (cdr(args) != null) arg2 = cadr(args);
	];
      if (op == mc:branch_always)
	m68k:branch(code, m68k:balways, dest)
      else if (op == mc:branch_true)
	[
	  m68k:compareimm(code, m68k:slongword, 1, m68k:lvar, arg1);
	  m68k:branch(code, m68k:bne, dest);
	]
      else if (op == mc:branch_false)
	[
	  m68k:compareimm(code, m68k:slongword, 1, m68k:lvar, arg1);
	  m68k:branch(code, m68k:beq, dest);
	]
      else if (op == mc:branch_or)
	[
	  m68k:compareimm(code, m68k:slongword, 1, m68k:lvar, arg1);
	  m68k:branch(code, m68k:bne, dest);
	  m68k:compareimm(code, m68k:slongword, 1, m68k:lvar, arg2);
	  m68k:branch(code, m68k:bne, dest);
	]
      else if (op == mc:branch_nor)
	[
	  | l |

	  l = m68k:new_label(code);
	  m68k:compareimm(code, m68k:slongword, 1, m68k:lvar, arg1);
	  m68k:branch(code, m68k:bne, l);
	  m68k:compareimm(code, m68k:slongword, 1, m68k:lvar, arg2);
	  m68k:branch(code, m68k:beq, dest);
	  m68k:label(code, l);
	]
      else if (op == mc:branch_and)
	[
	  | l |

	  l = m68k:new_label(code);
	  m68k:compareimm(code, m68k:slongword, 1, m68k:lvar, arg1);
	  m68k:branch(code, m68k:beq, l);
	  m68k:compareimm(code, m68k:slongword, 1, m68k:lvar, arg2);
	  m68k:branch(code, m68k:bne, dest);
	  m68k:label(code, l);
	]
      else if (op == mc:branch_nand)
	[
	  m68k:compareimm(code, m68k:slongword, 1, m68k:lvar, arg1);
	  m68k:branch(code, m68k:beq, dest);
	  m68k:compareimm(code, m68k:slongword, 1, m68k:lvar, arg2);
	  m68k:branch(code, m68k:beq, dest);
	]
      else if (op == mc:branch_eq || op == mc:branch_ne)
	[
	  if (in_reg(arg1))
	    m68k:compare(code, m68k:slongword, get_reg(arg1), m68k:lvar, arg2)
	  else if (in_reg(arg2))
	    m68k:compare(code, m68k:slongword, get_reg(arg2), m68k:lvar, arg1)
	  else
	    [
	      m68k:move(code, m68k:slongword, m68k:lvar, arg1,
			m68k:lregister, m68k:reg_d0);
	      m68k:compare(code, m68k:slongword, m68k:reg_d0, m68k:lvar, arg2)
	    ];
	  m68k:branch(code, if (op == mc:branch_eq) m68k:beq else m68k:bne, dest);
	]
      else if (op == mc:branch_gone?)
	mgen_type_check(code, type_gone, arg1, dest)
      else if (op == mc:branch_ngone?)
	mgen_ntype_check(code, type_gone, arg1, dest)
      else // relop
	[
	  | types |

	  types = ins[mc:i_btypes];
	  if (types && car(types) == itype_integer && cadr(types) == itype_integer)
	    [
	      | r1 |

	      ninferred = ninferred + 1;
	      r1 = get_reg(arg1);
	      if (r1 < 0) // r1 not in reg, move it to a0
		[
		  m68k:move(code, m68k:slongword, m68k:lvar, arg1,
			    m68k:lregister, m68k:reg_a0);
		  r1 = m68k:reg_a0;
		];
	      m68k:compare(code, m68k:slongword, r1, m68k:lvar, arg2);
	    ]
	  else
	    callop2(code, "bcompare", arg1, arg2);

	  m68k:branch(code, relops[op], dest);
	];
    ];

  builtins = vector("bor", "band", 0, 0,
		    "bleq", "blne", "bllt", "blle", "blgt", "blge",
		    "bbitor", "bbitxor", "bbitand", "bshift_left", "bshift_right",
		    "badd", "bsubtract", "bmultiply", "bdivide", "bremainder",
		    "bnegate", "bnot", "bbitnot", 0, 0, 0, 0, "bref", 0, "bcons", 0,
		    "bcar", "bcdr" );
  
  mgen_compute = fn (code, ins)
    [
      | args, arg1, arg2, op, dest, types, type1, type2 |

      op = ins[mc:i_aop];
      dest = ins[mc:i_adest];
      args = ins[mc:i_aargs];
      types = ins[mc:i_atypes];
      if (args != null)
	[
	  arg1 = car(args);
	  if (types) type1 = car(types);
	  if (cdr(args) != null)
	    [
	      arg2 = cadr(args);
	      if (types) type2 = cadr(types);
	    ];
	];

      if (op == mc:b_assign)
	m68k:move(code, m68k:slongword, m68k:lvar, arg1, m68k:lvar, dest)
      else if (arg2 == null)
	[
	  if ((op == mc:b_car || op == mc:b_cdr) && type1 == itype_pair)
	    [
	      | r |

	      ninferred = ninferred + 1;
	      r = get_reg(arg1);
	      if (r < m68k:reg_a0)
		[
		  r = m68k:reg_a0;
		  m68k:move(code, m68k:slongword, m68k:lvar, arg1,
			    m68k:lregister, m68k:reg_a0);
		];
	      m68k:move(code, m68k:slongword,
			m68k:lindexed, r . (if (op == mc:b_car) 12 else 16),
			m68k:lvar, dest);
	    ]
	  else
	    [
	      callop1(code, builtins[op], arg1);
	      m68k:move(code, m68k:slongword, m68k:lregister, m68k:reg_d0,
			m68k:lvar, dest);
	    ];
	]
      else
	[
	  | done |

	  done = false;
	  if (type1 == itype_integer && type2 == itype_integer)
	    if (op == mc:b_add)
	      [
		| r1, r2, rd |

		ninferred = ninferred + 1;
		r1 = get_reg(arg1); r2 = get_reg(arg2); rd = get_reg(dest);
		if (arg1[mc:v_class] == mc:v_constant || r2 == rd && r2 >= 0)
		  [
		    | t |

		    t = arg1; arg1 = arg2; arg2 = t;
		    t = r1; r1 = r2; r2 = t;
		  ];

		if (rd < 0) rd = m68k:reg_a0;
		m68k:move(code, m68k:slongword, m68k:lvar, arg1, m68k:lregister, rd);
		if (arg2[mc:v_class] == mc:v_constant)
		  m68k:add(code, m68k:slongword, rd,
			   m68k:limmediate, mc:var_value(arg2) << 1)
		else
		  [
		    m68k:add(code, m68k:slongword, rd, m68k:lvar, arg2);
		    m68k:subtract(code, m68k:slongword, rd, m68k:limmediate, 1);
		  ];
		if (rd == m68k:reg_a0)
		  m68k:move(code, m68k:slongword, m68k:lregister, rd,
			    m68k:lvar, dest);
		done = true;
	      ]
	    else if (op == mc:b_subtract)
	      [
		| r1, r2, rd |

		ninferred = ninferred + 1;
		r1 = get_reg(arg1); r2 = get_reg(arg2); rd = get_reg(dest);

		if (rd < 0 || r2 == rd) rd = m68k:reg_a0;
		m68k:move(code, m68k:slongword, m68k:lvar, arg1, m68k:lregister, rd);
		if (arg2[mc:v_class] == mc:v_constant)
		  m68k:subtract(code, m68k:slongword, rd,
				m68k:limmediate, mc:var_value(arg2) << 1)
		else
		  [
		    m68k:subtract(code, m68k:slongword, rd, m68k:lvar, arg2);
		    m68k:add(code, m68k:slongword, rd, m68k:limmediate, 1);
		  ];
		if (rd == m68k:reg_a0)
		  m68k:move(code, m68k:slongword, m68k:lregister, rd,
			    m68k:lvar, dest);

		done = true;
	      ];
	  if (!done)
	    [
	      callop2(code, builtins[op], arg1, arg2);
	      m68k:move(code, m68k:slongword, m68k:lregister, m68k:reg_d0,
			m68k:lvar, dest);
	    ];
	]
    ];

  mgen_memory = fn (code, ins)
    [
      | array, areg, scalar, offset |

      array = ins[mc:i_marray];
      areg = get_reg(array);
      scalar = ins[mc:i_mscalar];
      offset = m68k:object_offset + 4 * ins[mc:i_mindex];

      if (areg == m68k:reg_d0 && get_reg(scalar) == m68k:reg_a0) fail()
      else if (!(areg >= m68k:reg_a0 && areg <= m68k:reg_a7))
	[
	  m68k:move(code, m68k:slongword, m68k:lvar, array,
		    m68k:lregister, m68k:reg_a0);
	  areg = m68k:reg_a0;
	];

      if (ins[mc:i_mop] == mc:memory_read)
	m68k:move(code, m68k:slongword, m68k:lindexed, areg . offset,
		  m68k:lvar, scalar)
      else
	m68k:move(code, m68k:slongword, m68k:lvar, scalar,
		  m68k:lindexed, areg . offset)
    ];

  mgen_closure = fn (code, ins)
    [
      | cvars, f, offset, cdest |

      f = ins[mc:i_ffunction];
      cvars = lfilter(fn (cvar) cvar[mc:v_cparent] != mc:myself,
		      f[mc:c_fclosure]);

      // paranoia
      m68k:move(code, m68k:slongword, m68k:limmediate, 0,
		m68k:lregister, m68k:reg_d1);

      allocate_readonly(code, m68k:reg_a0, 4 * (1 + llength(cvars)),
			garbage_record, type_closure);
      m68k:move(code, m68k:slongword, m68k:lfunction, f,
		m68k:lindexed, m68k:reg_a0 . m68k:object_offset);
      offset = m68k:object_offset + 4;
      cdest = ins[mc:i_fdest];
      while (cvars != null)
	[
	  | cvar |

	  cvar = car(cvars)[mc:v_cparent];
	  m68k:move(code, m68k:slongword, m68k:lvar, cvar,
		    m68k:lindexed, m68k:reg_a0 . offset);
	  offset = offset + 4;
	  cvars = cdr(cvars);
	];
      m68k:move(code, m68k:slongword, m68k:lregister, m68k:reg_a0,
		m68k:lvar, cdest);
    ];

  mgen_call = fn (code, ins)
    [
      | args, nargs, extra, moves, rargs, offset |

      args = ins[mc:i_cargs];

      // function goes in reg_closure_in
      moves = vector(m68k:lvar, car(args), reg_closure_in) . moves;
      args = cdr(args);

      nargs = llength(args);
      if (nargs > nregargs + 1) // copy extra args to spill base for call
	[
	  offset = m68k:object_offset;
	  extra = nth_pair(nregargs + 1, args);
	  while (extra != null)
	    [
	      m68k:move(code, m68k:slongword, m68k:lvar, car(extra),
			m68k:lindexed, m68k:reg_spill . offset);
	      offset = offset + 4;
	      extra = cdr(extra);
	    ];
	  moves = vector(m68k:lregister, m68k:reg_spill, reg_extra_args_in) . moves;
	]
      else if (nargs == nregargs + 1) // arg goes directly into extra args in
        moves = vector(m68k:lvar, nth_element(nregargs + 1, args), reg_extra_args_in) . moves;

      // first nregargs go into regs_args_in
      rargs = regs_args_in;
      while (rargs != null && args != null)
	[
	  moves = vector(m68k:lvar, car(args), car(rargs)) . moves;
	  args = cdr(args);
	  rargs = cdr(rargs);
	];

      // rearrange variables as requested
      shuffle(code, moves);

      m68k:move(code, m68k:sword, m68k:limmediate, nargs,
		m68k:lregister, reg_argcount);
      m68k:jsr(code, m68k:lbuiltin, "bcall");
      m68k:move(code, m68k:slongword, m68k:lregister, reg_result,
		m68k:lvar, ins[mc:i_cdest]);
    ];

  mgen_type_check = fn (code, type, arg, dest)
    if (type == type_integer)
      [
	m68k:move(code, m68k:sword, m68k:lvar, arg, m68k:lregister, m68k:reg_d0);
	m68k:andimm(code, m68k:sbyte, 1, m68k:lregister, m68k:reg_d0);
	m68k:branch(code, m68k:bne, dest);
      ]
    else if (type == type_null)
      [
	m68k:move(code, m68k:slongword, m68k:lvar, arg, m68k:lregister, m68k:reg_d0);
	m68k:branch(code, m68k:beq, dest);
      ]
    else if (type == stype_none)
      0 // easy...
    else if (type == stype_any)
      m68k:branch(code, m68k:balways, dest) // quite easy...
    else if (type == stype_function)
      [
	| fail |

	fail = m68k:new_label(code);
	m68k:move(code, m68k:slongword, m68k:lvar, arg, m68k:lregister, m68k:reg_d0);
	m68k:branch(code, m68k:beq, fail);
	m68k:move(code, m68k:slongword, m68k:lregister, m68k:reg_d0,
		  m68k:lregister, m68k:reg_a0);
	m68k:andimm(code, m68k:sbyte, 1, m68k:lregister, m68k:reg_d0);
	m68k:branch(code, m68k:bne, fail);
	m68k:move(code, m68k:sbyte, m68k:lindexed, m68k:reg_a0 . 5,
		  m68k:lregister, m68k:reg_d0);
	m68k:compareimm(code, m68k:sbyte, type_closure, m68k:lregister, m68k:reg_d0);
	m68k:branch(code, m68k:beq, dest);
	m68k:compareimm(code, m68k:sbyte, type_primitive, m68k:lregister, m68k:reg_d0);
	m68k:branch(code, m68k:beq, dest);
	m68k:compareimm(code, m68k:sbyte, type_secure, m68k:lregister, m68k:reg_d0);
	m68k:branch(code, m68k:beq, dest);
	m68k:compareimm(code, m68k:sbyte, type_varargs, m68k:lregister, m68k:reg_d0);
	m68k:branch(code, m68k:beq, dest);
	m68k:label(code, fail);
      ]
    else if (type == stype_list)
      [
	| fail |

	fail = m68k:new_label(code);
	m68k:move(code, m68k:slongword, m68k:lvar, arg, m68k:lregister, m68k:reg_d0);
	m68k:branch(code, m68k:beq, dest);
	m68k:move(code, m68k:slongword, m68k:lregister, m68k:reg_d0,
		  m68k:lregister, m68k:reg_a0);
	m68k:andimm(code, m68k:sbyte, 1, m68k:lregister, m68k:reg_d0);
	m68k:branch(code, m68k:bne, fail);
	m68k:move(code, m68k:sbyte, m68k:lindexed, m68k:reg_a0 . 5,
		  m68k:lregister, m68k:reg_d0);
	m68k:compareimm(code, m68k:sbyte, type_pair, m68k:lregister, m68k:reg_d0);
	m68k:branch(code, m68k:beq, dest);
	m68k:label(code, fail);
      ]
    else 
      [
	| fail |

	fail = m68k:new_label(code);
	m68k:move(code, m68k:slongword, m68k:lvar, arg, m68k:lregister, m68k:reg_d0);
	m68k:branch(code, m68k:beq, fail);
	m68k:move(code, m68k:slongword, m68k:lregister, m68k:reg_d0,
		  m68k:lregister, m68k:reg_a0);
	m68k:andimm(code, m68k:sbyte, 1, m68k:lregister, m68k:reg_d0);
	m68k:branch(code, m68k:bne, fail);
	m68k:move(code, m68k:sbyte, m68k:lindexed, m68k:reg_a0 . 5,
		  m68k:lregister, m68k:reg_d0);
	m68k:compareimm(code, m68k:sbyte, type, m68k:lregister, m68k:reg_d0);
	m68k:branch(code, m68k:beq, dest);
	m68k:label(code, fail);
      ];

  mgen_ntype_check = fn (code, type, arg, dest)
    if (type == type_integer)
      [
	m68k:move(code, m68k:sword, m68k:lvar, arg, m68k:lregister, m68k:reg_d0);
	m68k:andimm(code, m68k:sbyte, 1, m68k:lregister, m68k:reg_d0);
	m68k:branch(code, m68k:beq, dest);
      ]
    else if (type == type_null)
      [
	m68k:move(code, m68k:slongword, m68k:lvar, arg, m68k:lregister, m68k:reg_d0);
	m68k:branch(code, m68k:bne, dest);
      ]
    else if (type == stype_none)
      m68k:branch(code, m68k:balways, dest) // quite easy...
    else if (type == stype_any)
      0 // easy...
    else if (type == stype_function)
      [
	| fail |

	fail = m68k:new_label(code);
	m68k:move(code, m68k:slongword, m68k:lvar, arg, m68k:lregister, m68k:reg_d0);
	m68k:branch(code, m68k:beq, dest);
	m68k:move(code, m68k:slongword, m68k:lregister, m68k:reg_d0,
		  m68k:lregister, m68k:reg_a0);
	m68k:andimm(code, m68k:sbyte, 1, m68k:lregister, m68k:reg_d0);
	m68k:branch(code, m68k:bne, dest);
	m68k:move(code, m68k:sbyte, m68k:lindexed, m68k:reg_a0 . 5,
		  m68k:lregister, m68k:reg_d0);
	m68k:compareimm(code, m68k:sbyte, type_closure, m68k:lregister, m68k:reg_d0);
	m68k:branch(code, m68k:beq, fail);
	m68k:compareimm(code, m68k:sbyte, type_primitive, m68k:lregister, m68k:reg_d0);
	m68k:branch(code, m68k:beq, fail);
	m68k:compareimm(code, m68k:sbyte, type_secure, m68k:lregister, m68k:reg_d0);
	m68k:branch(code, m68k:beq, fail);
	m68k:compareimm(code, m68k:sbyte, type_varargs, m68k:lregister, m68k:reg_d0);
	m68k:branch(code, m68k:bne, dest);
	m68k:label(code, fail);
      ]
    else if (type == stype_list)
      [
	| fail |

	fail = m68k:new_label(code);
	m68k:move(code, m68k:slongword, m68k:lvar, arg, m68k:lregister, m68k:reg_d0);
	m68k:branch(code, m68k:beq, fail);
	m68k:move(code, m68k:slongword, m68k:lregister, m68k:reg_d0,
		  m68k:lregister, m68k:reg_a0);
	m68k:andimm(code, m68k:sbyte, 1, m68k:lregister, m68k:reg_d0);
	m68k:branch(code, m68k:bne, dest);
	m68k:move(code, m68k:sbyte, m68k:lindexed, m68k:reg_a0 . 5,
		  m68k:lregister, m68k:reg_d0);
	m68k:compareimm(code, m68k:sbyte, type_pair, m68k:lregister, m68k:reg_d0);
	m68k:branch(code, m68k:bne, dest);
	m68k:label(code, fail);
      ]
    else 
      [
	| fail |

	fail = m68k:new_label(code);
	m68k:move(code, m68k:slongword, m68k:lvar, arg, m68k:lregister, m68k:reg_d0);
	m68k:branch(code, m68k:beq, dest);
	m68k:move(code, m68k:slongword, m68k:lregister, m68k:reg_d0,
		  m68k:lregister, m68k:reg_a0);
	m68k:andimm(code, m68k:sbyte, 1, m68k:lregister, m68k:reg_d0);
	m68k:branch(code, m68k:bne, dest);
	m68k:move(code, m68k:sbyte, m68k:lindexed, m68k:reg_a0 . 5,
		  m68k:lregister, m68k:reg_d0);
	m68k:compareimm(code, m68k:sbyte, type, m68k:lregister, m68k:reg_d0);
	m68k:branch(code, m68k:bne, dest);
	m68k:label(code, fail);
      ];

  callop1 = fn (code, builtin, arg)
    [
      m68k:move(code, m68k:slongword, m68k:lvar, arg, m68k:lregister, m68k:reg_d0);
      m68k:jsr(code, m68k:lbuiltin, builtin);
    ];

  callop2 = fn (code, builtin, arg1, arg2)
    [
      | r1, r2 |

      r1 = get_reg(arg1); r2 = get_reg(arg2);
      if (r1 == m68k:reg_d1 && r2 == m68k:reg_d0)
	m68k:exchange(code, m68k:reg_d0, m68k:reg_d1)
      else if (r2 == m68k:reg_d0)
	[
	  m68k:move(code, m68k:slongword, m68k:lvar, arg2,
		    m68k:lregister, m68k:reg_d1);
	  m68k:move(code, m68k:slongword, m68k:lvar, arg1,
		    m68k:lregister, m68k:reg_d0);
	]
      else
	[
	  m68k:move(code, m68k:slongword, m68k:lvar, arg1,
		    m68k:lregister, m68k:reg_d0);
	  m68k:move(code, m68k:slongword, m68k:lvar, arg2,
		    m68k:lregister, m68k:reg_d1);
	];
      m68k:jsr(code, m68k:lbuiltin, builtin);
    ];
  

  allocate = fn (code, dest, size, gtype, mtype)
    [
      | magic |

      m68k:move(code, m68k:slongword, m68k:limmediate, size + m68k:object_offset,
		m68k:lregister, m68k:reg_d0);
      m68k:jsr(code, m68k:lbuiltin, "balloc");

      magic = gtype << 8 | mtype;
      m68k:move(code, m68k:sword, m68k:limmediate, magic,
		m68k:lindexed, m68k:reg_a0 . 4);
      m68k:move(code, m68k:slongword, m68k:lregister, m68k:reg_a0,
		m68k:lregister, dest);
    ];

  allocate_readonly = fn (code, dest, size, gtype, mtype)
    [
      | magic |

      m68k:move(code, m68k:slongword, m68k:limmediate, size + m68k:object_offset,
		m68k:lregister, m68k:reg_d0);
      m68k:jsr(code, m68k:lbuiltin, "balloc_readonly");

      magic = gtype << 8 | mtype;
      m68k:move(code, m68k:sword, m68k:limmediate, magic,
		m68k:lindexed, m68k:reg_a0 . 4);
      m68k:move(code, m68k:slongword, m68k:lregister, m68k:reg_a0,
		m68k:lregister, dest);
    ];

  shuffle = fn (code, moves)
    [
      | copies, cstspills, newmoves, m, same_move, move, m2, xfers, exchange |

      same_move = fn (move2) move[0] == move2[0] && move[1] == move2[1];

      // find copies, constants, spills, etc
      m = moves;
      while (m != null)
	[
	  move = car(m);
	  
	  // is move same as a prior move ?
	  if ((m2 = lexists?(same_move, cstspills)) ||
	      (m2 = lexists?(same_move, newmoves)))
	    copies = (m2 . move) . copies
	  else if (move[0] == m68k:lvar && !in_reg(move[1]))
	    cstspills = move . cstspills
	  else
	    newmoves = move . newmoves;
	    
	  m = cdr(m);
	];
      // newmoves is a summary of the actual register - register moves.
      // do these first.

      // a: build reg1 -> reg2 list from newmoves
      xfers = lmap(fn (move)
		     (if (move[0] == m68k:lregister) move[1]
		     else get_reg(move[1])) . move[2],
		   newmoves);

      // b: while xfers remain, try & do a move. if none possible
      //    do an exchange.
      while (xfers != null)
	[
	  move = lexists?(fn (xfer) car(xfer) == cdr(xfer) || !assq(cdr(xfer), xfers),
			  xfers);
	  if (move)
	    [
	      m68k:move(code, m68k:slongword, m68k:lregister, car(move),
			m68k:lregister, cdr(move));
	      xfers = ldelete(move, xfers);
	    ]
	  else
	    [
	      // do an exchange to place the first element of xfers
	      // in the right place

	      exchange = car(xfers);
	      m68k:exchange(code, car(exchange), cdr(exchange));

	      // And update the xfers list
	      // the value that was in cdr(exchange) is now in car(exchange)
	      xfers = cdr(xfers);
	      lexists?(fn (xfer)
		         if (car(xfer) == cdr(exchange))
		           [
			     set_car!(xfer, car(exchange));
			     true
			   ]
			 else false,
		       xfers);
	    ];
	];

      // Set constants and spilled variables
      lforeach(fn (move)
	         m68k:move(code, m68k:slongword, move[0], move[1],
			   m68k:lregister, move[2]), cstspills);

      // Make copies
      lforeach(fn (copy)
	         m68k:move(code, m68k:slongword, m68k:lregister, car(copy)[2],
			   m68k:lregister, cdr(copy)[2]),
	       copies);
    ];

];
