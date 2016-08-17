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

library genx86 // Code generation for x86 (not particularly good)
requires sequences, misc, dlist, compiler, vars, ins3, mx86, inference,
  ax86, flow, graph

defines
  x86:nscratch, x86:ncaller, x86:nregargs, x86:ncallee,
  x86:select_registers, x86:mgen_preamble, x86:mgen_instruction,
  x86:migrate, x86:gassemble, x86:spillreg, x86:reg_globals,
  x86:uses_scratch?

reads mc:verbose, mc:myself, mc:update_maxseclev
writes mc:nops_called, mc:nops_inlined, mc:framesizes, mc:lineno
[

  | reg_argcount, reg_closure_in, relop, reg_result,
    regs_scratch, regs_caller, select_registers,
    builtins, relops, var_in_reg, reg_dest, mgen_return, mgen_branch,
    mgen_compute, mgen_memory, mgen_closure, mgen_vref, mgen_call,
    callop1, callop2, callop3, rev_x86relop, fetch1, move, commute, compare,
    reg_scratch, reg_arg0, reg_arg1, reg_closure, reg_fp,
    reg_sp, mgen_trap, regs_allscratch, type_branch, get_type,
    typearg1, typearg2, inline1?, inline2?, mgen_inline1, mgen_inline2,
    type_trap, call, set_seclev, mzero, mfalse,
    call_closure, call_primitive, call_varargs,
    call_builtin, perform3, perform3cst, setcc, intcst?, argstart, enames,
    commute_x86relop, in_scratch?, logical_or, safemove,
    push_args,
    cmpeq, kset, kequal?, kseclevel, kmaxseclevel, call_kset, call_seclevel,
    kconcat_strings, call_bconcat, kglobal_lookup, maybe_call_global_lookup,
    needs_closure?, is_leaf?, update_maxseclev?,
    needs_global?, leaaddcst, fetch2, fetch_for_dest,
    fake_prim_type |

  // used to indicate any primitive; i.e., stype_function - type_closure
  fake_prim_type = '[];

  mc:nops_inlined = mc:nops_called = 0;
  mc:framesizes = make_vector(10);
  vector_fill!(mc:framesizes, 0);

  mfalse = mzero = x86:mudlleint(0);

  // Register summary:
  //   ax: argument count, function result, scratch, first args to builtins
  //   bx: current closure
  //   cx: temp use, closure in, 2nd arg to builtins
  //   dx, di: caller-save register
  //   si: pointer to globals
  //   bp: frame pointer
  //   sp: stack pointer

  // Calling convention is standard C + closure in reg_closure_in
  // i.e. args in right to left order on stack
  reg_argcount = x86:reg_eax; // same as reg_scratch, beware
  reg_closure = x86:reg_ebx;
  reg_closure_in = x86:reg_edx; // when passed as parameter
  reg_result = x86:reg_eax;
  argstart = 8; // offset of first argument on stack

  // Global regs
  x86:reg_globals = x86:reg_esi;
  reg_sp = x86:reg_esp;
  reg_fp = x86:reg_ebp;
  reg_scratch = x86:reg_eax;

  // Arguments for builtins
  reg_arg0 = x86:reg_eax;
  reg_arg1 = x86:reg_ecx;

  // General regs
  regs_scratch = '[,reg_scratch];
  regs_allscratch = '[,reg_scratch ,x86:reg_ecx];
  regs_caller = '[,x86:reg_edx ,x86:reg_edi];

  x86:spillreg = '[,reg_closure ,reg_fp ,reg_fp];

  needs_closure? = fn (ifn)
    (ifn[mc:c_fclosure] != null);

  needs_global? = fn (ifn)
    // Types: ifn : intermediate function
    // Returns: true if ifn reads or writes a global variable
    [
      | is_global?, uses_global |

      is_global? = fn (v)
	[
	  | class |

	  class = v[mc:v_class];
	  (class == mc:v_global || class == mc:v_global_define
           || (class == mc:v_global_constant
               && !immutable?(global_value(v[mc:v_goffset]))))
	];

      uses_global = fn (il)
	[
	  | dvar, ins |

	  ins = il[mc:il_ins];
	  dvar = mc:defined_var(ins);
	  if (dvar && is_global?(dvar))
	    true
	  else
	    lexists?(is_global?, mc:arguments(ins, null))
	];

      graph_nodes_exists?(fn (n) [
        dexists?(uses_global, graph_node_get(n)[mc:f_ilist])
      ], cdr(ifn[mc:c_fvalue]));
    ];

  // nb of registers of each category available
  x86:nscratch = fn (ifn) 1;    // eax
  x86:ncaller = fn (ifn) 2;     // edx, edi
  x86:nregargs = fn (ifn) 0;
  x86:ncallee = fn (ifn)        // esi (unless globals), ebx (unless closure)
    [
      | ncallee |

      ncallee = 0;

      if (!(ifn[mc:c_fmisc][mc:c_fm_globalsbase] = needs_global?(ifn)))
        ++ncallee;

      if (!(ifn[mc:c_fmisc][mc:c_fm_closurebase] = needs_closure?(ifn)))
        ++ncallee;

      ncallee
    ];

  kset            = mc:make_kglobal("set!");
  kequal?         = mc:make_kglobal("equal?");
  kseclevel       = mc:make_kglobal("seclevel");
  kmaxseclevel    = mc:make_kglobal("maxseclevel");
  kglobal_lookup  = mc:make_kglobal("global_lookup");
  kconcat_strings = mc:make_kglobal("concat_strings");

  x86:uses_scratch? = fn (ins)
    [
      | class, op |
      class = ins[mc:i_class];

      // These operations must not clobber the scratch register
      if (class == mc:i_branch)
        !((op = ins[mc:i_bop]) == mc:branch_always
          || op == mc:branch_true
          || op == mc:branch_false
          || op == mc:branch_and
          || op == mc:branch_nand
          || (op >= mc:branch_eq && op <= mc:branch_gt)
          || (op >= mc:branch_type?
              && op < mc:branch_ntype? + last_synthetic_type)
          || op == mc:branch_any_prim
          || op == mc:branch_not_prim)
      else if (class == mc:i_trap)
        ins[mc:i_top] != mc:trap_type
      else
        true
    ];

  get_type = fn (v) // minimalistic type inference ...
    [
      | t |

      if (t = mc:constant?(v)) t
      else itype_any
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

  x86:select_registers = fn (ifn, ainfo)
    // Effects: Selects registers for ifn
    // Returns: Information on allocation count
    [
      | args, offset, cvars, locals, regs_callee |

      // Allocate caller regs
      select_registers(ifn, mc:reg_caller, regs_caller);

      // Allocate callee regs
      // set regs_callee to the callee registers actually needed
      // (used to generate correct push/pops)
      if (ainfo[2] > 0) // list_first_n! doesn't handle 0. grr.
	[
	  regs_callee = null;
	  if (!ifn[mc:c_fmisc][mc:c_fm_closurebase])
	    regs_callee = reg_closure . regs_callee;
	  if (!ifn[mc:c_fmisc][mc:c_fm_globalsbase])
	    regs_callee = x86:reg_globals . regs_callee;
	  regs_callee = list_to_vector(list_first_n!(ainfo[2], regs_callee));
	]
      else
	regs_callee = '[];
      ifn[mc:c_fmisc][mc:c_fm_regs_callee] = regs_callee;
      select_registers(ifn, mc:reg_callee, regs_callee);

      // Alocate scratch regs
      select_registers(ifn, mc:reg_scratch, regs_scratch);

      // set/adjust offsets of all spilled variables
      cvars = ifn[mc:c_fclosure];
      offset = x86:object_offset + 4; // skip over function
      while (cvars != null)
	[
	  | cvar, cvarloc |

	  cvar = car(cvars);
	  // myself is not present in closures, it is the closure
	  if (cvar[mc:v_cparent] == mc:myself)
	    cvar[mc:v_location] = vector(mc:v_lregister, mc:reg_callee,
                                         reg_closure)
	  else
	    [
	      cvarloc = cvar[mc:v_location];
	      if (cvarloc[mc:v_lclass] == mc:v_lspill)
		cvarloc[mc:v_lsoffset] = offset;
	      offset += 4;
	    ];
	  cvars = cdr(cvars);
	];

      | localofs |
      // [0] is old bp, [1] is vararg vector (optionally), [2+] are locals
      localofs = if (ifn[mc:c_fvarargs]) 2 else 1;

      // spilled locals are stored below bp
      locals = ifn[mc:c_flocals];
      while (locals != null)
	[
	  | localoc |

	  localoc = car(locals)[mc:v_location];
	  if (localoc[mc:v_lclass] == mc:v_lspill &&
	      localoc[mc:v_lstype] == mc:spill_spill)
	    localoc[mc:v_lsoffset] = -4 * (localofs + localoc[mc:v_lsoffset]);
	  locals = cdr(locals);
	];

      args = ifn[mc:c_fargs];
      if (ifn[mc:c_fvarargs])
	[
	  // varargs is special: we force it onto the local var stack frame
	  | vargloc |

	  vargloc = car(args)[mc:v_location];
	  ++ainfo[3];
	  if (vargloc)
	    [
	      vargloc[mc:v_lclass] = mc:v_lspill;
	      vargloc[mc:v_lstype] = mc:spill_spill;
	      vargloc[mc:v_lsoffset] = -4;
	    ]
	]
      else
	[
	  offset = argstart;
	  while (args != null)
	    [
	      | argloc |

	      argloc = car(args)[mc:v_location];
	      if (argloc && argloc[mc:v_lclass] == mc:v_lspill &&
		  argloc[mc:v_lstype] == mc:spill_args)
		argloc[mc:v_lsoffset] = offset;
	      offset += 4;
	      args = cdr(args);
	    ];
	];

      ainfo
    ];

  x86:migrate = fn (ifn, vars, notspilt, spilt, locals, temps)
    // Effetcs: Receives the grouping of variables used by the
    //   register allocator. May move some variables between the group to
    //   better suit the processor. May only strengthen needs ...
    //   On x86: move any closure or argument in temps to locals
    [
      | detemp |

      detemp = fn (l)
        lforeach(fn (v) [
	  | n |
	  n = v[mc:v_number];
	  if (bit_set?(temps, n))
	    [
	      clear_bit!(temps, n);
	      set_bit!(locals, n);
	    ]
	], l);

      detemp(ifn[mc:c_fargs]);
      detemp(ifn[mc:c_fclosure]);
    ];

  enames =
    '[
      "berror_bad_function"
      "berror_stack_underflow"
      "berror_bad_type"
      "berror_divide_by_zero"
      "berror_bad_index"
      "berror_bad_value"
      "berror_variable_read_only"
      "berror_loop"
      "berror_recurse"
      "berror_wrong_parameters"
      "berror_security_violation"
      "berror_value_read_only"
      "berror_user_interrupt"
      "berror_no_match"
      "berror_compile"
      "berror_abort"
    ];
  assert(vlength(enames) == last_runtime_error);

  x86:gassemble = fn (code)
    [
      // Generate error calls
      lforeach
	(fn (err)
	 [
           | errno, label |
           @[errno mc:lineno label] = err;
	   x86:label(code, label);
           if (errno == -error_wrong_parameters)
             call_builtin(code, "bearly_error_wrong_parameters")
           else
             [
               assert(errno >= 0);
               call_builtin(code, enames[errno]);
             ];
	 ],
	 code[2]);
      x86:assemble(code);
    ];

  is_leaf? = fn (ifn)
    !dexists?(fn (il) [
      | ins |
      ins = il[mc:il_ins];
      if (ins[mc:i_class] == mc:c_execute)
        [
          | called, args |
          @(called . args) = ins[mc:i_cargs];
          if (called == kseclevel || called == kmaxseclevel)
            [
              if (args == null)
                exit<function> false;
            ]

          else if (called == kset)
            [
              if (llength(args) == 3)
                [
                  | types, type |
                  types = ins[mc:i_ctypes];
                  type = if (types) cadr(types) else get_type(car(args));
                  // count vector/string assignment as a leaf operation
                  if (type & ~(itype_vector | itype_string) == 0)
                    exit<function> false;
                ]
            ]
          else if (called == kglobal_lookup)
            [
              if (llength(args) == 1
                  && get_type(car(args)) == itype_string)
                exit<function> false;
            ]
          else if (called == kconcat_strings)
            exit<function> false;

          exit<function> true;
        ];

      false
    ], ifn[mc:c_fvalue]);

  update_maxseclev? = fn (ifn)
    (mc:update_maxseclev == true && !is_leaf?(ifn));

  set_seclev = fn (code, dreg, type)
    [
      if (type == x86:sl_c)
        x86:op16(code);
      x86:mov(code, x86:lseclev, type, x86:lreg, dreg);
    ];

  x86:mgen_preamble = fn (ifn, ainfo)
    [
      | code, ilist, argcheck, locals, cvars, offset, args, fmisc |

      if (mc:verbose < 1)
        x86:reset_ins_count();

      code = x86:new_code();
      ilist = ifn[mc:c_fvalue];

      mc:lineno = ifn[mc:c_flineno];

      // Allocate & clear stack frame
      x86:push(code, x86:lreg, reg_fp);
      move(code, x86:lreg, reg_sp, x86:lreg, reg_fp);
      if (ainfo[3] > 0)
	x86:sub(code, x86:limm, ainfo[3] * 4, x86:lreg, reg_sp);

      // Make argument vector for varargs functions
      if (ifn[mc:c_fvarargs])
	[
          set_seclev(code, reg_arg1, x86:sl_c);
	  // Must call even if arg is unused to check seclevel, etc
	  call_builtin(code, "bvarargs");
          offset = -4;
	  move(code, x86:lreg, reg_arg0, x86:lidx, reg_fp . offset);
	]
      else
	[
	  // Clear Z if argument count incorrect (bcleargc will trap)
	  argcheck = dget(ilist)[mc:il_ins];
	  if (argcheck[mc:i_class] == mc:i_trap &&
	      argcheck[mc:i_top] == mc:trap_argcheck)
	    [
	      | argcount |

	      argcount = car(argcheck[mc:i_targs])[mc:v_kvalue];
              cmpeq(code, x86:limm, argcount, x86:lreg, reg_argcount);

	      // Remove trap ins
	      ifn[mc:c_fvalue] = ilist = dremove!(ilist, ilist);
	    ]
	  else
	    fail(); // only vararg fns should have no check

	  if (ainfo[3] < 10)
	    ++mc:framesizes[ainfo[3]]
	  else
	    ++mc:framesizes[9];

	  if (is_leaf?(ifn) && ainfo[3] < 3)
	    [
	      | i |

	      // skip the infinite loop and minlevel security checks for leaf
	      // functions (note that this is a minor security hole)
	      x86:trap(code, x86:bne, -error_wrong_parameters, mc:lineno);
	      i = ainfo[3];
	      while (i > 0)
		[
		  // initialise stack frame to legal mudlle values
		  x86:mov(code, x86:lreg, reg_closure_in,
                          x86:lidx, reg_fp . -(i << 2));
		  --i;
		];
	    ]
	  else
	    [
              set_seclev(code, reg_arg1, x86:sl_c);
              | f |
              f = if (ainfo[3] >= 4)
                "bcleargc"
              else
                '["bcleargc0" "bcleargc1" "bcleargc2" "bcleargc3"][ainfo[3]];
              call_builtin(code, f);
	    ];

	  offset = argstart;
	];

      if (update_maxseclev?(ifn))
        [
          set_seclev(code, reg_arg1, x86:sl_maxlev);
          x86:mov(code, x86:lspecial, "maxseclevel",
                  x86:lreg, reg_arg0);
          x86:cmp(code, x86:lreg, reg_arg0, x86:lreg, reg_arg1);
          | l |
          l = x86:new_label(code);
          x86:jcc(code, x86:bge, l);
          x86:mov(code, x86:lreg, reg_arg1, x86:lspecial,
                  "maxseclevel");
          x86:label(code, l);
          x86:push(code, x86:lreg, reg_arg0);
        ];

      // Save callee-saved registers
      fmisc = ifn[mc:c_fmisc];
      if (fmisc[mc:c_fm_closurebase])
	[
	  x86:push(code, x86:lreg, reg_closure);
	  move(code, x86:lreg, reg_closure_in, x86:lreg, reg_closure);
	]
      else if (vfind?(reg_closure, fmisc[mc:c_fm_regs_callee]))
	x86:push(code, x86:lreg, reg_closure);

      if (fmisc[mc:c_fm_globalsbase])
	[
	  x86:push(code, x86:lreg, x86:reg_globals);
	  x86:mov(code, x86:lspecial, "env_values", x86:lreg, x86:reg_globals);
	]
      else if (vfind?(x86:reg_globals, fmisc[mc:c_fm_regs_callee]))
	x86:push(code, x86:lreg, x86:reg_globals);

      // setup variables:

      // setup arguments, add indirection
      args = ifn[mc:c_fargs];
      while (args != null)
	[
	  | arg, loc, locarg |

	  arg = car(args);
	  args = cdr(args);

	  loc = x86:lidx;
	  locarg = reg_fp . offset;

	  offset += 4;

	  if (arg[mc:v_location]) // ignore unused arguments
	    [
	      // if variable is indirect create indirection record
	      if (arg[mc:v_indirect])
		[
		  call_builtin(code, "balloc_variable");
		  move(code, loc, locarg, x86:lidx,
                       reg_arg1 . x86:object_offset);
		  loc = x86:lreg;
		  locarg = reg_arg1;
		];

	      // & copy to correct location
	      move(code, loc, locarg, x86:lvar, arg);
	      assert_message(!in_scratch?(arg),
                             "oops - argument unspilt to scratch");
	    ];
	];

      // unspill closure vars that need it
      cvars = ifn[mc:c_fclosure];
      offset = x86:object_offset + 4;
      while (cvars != null)
	[
	  | cvar, cvarloc |

	  cvar = car(cvars);

	  if (cvar[mc:v_cparent] != mc:myself)
	    [
	      cvarloc = cvar[mc:v_location];
	      if (cvarloc[mc:v_lclass] == mc:v_lregister)
                // unspill closure entry
		move(code, x86:lidx, reg_closure . offset,
		     x86:lreg, cvarloc[mc:v_lrnumber]);
	      assert_message(!in_scratch?(cvar),
                             "oops - closure var unspilt to scratch");
	      offset += 4;
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
	      call_builtin(code, "balloc_variable");
	      move(code, x86:limm, 0, x86:lidx, reg_arg1 . x86:object_offset);
	      move(code, x86:lreg, reg_arg1, x86:lvar, local);
	      assert_message(!in_scratch?(local),
                             "oops - local var unspilt to scratch");
	    ];
	];

      code
    ];


  x86:mgen_instruction = fn (code, ifn, ainfo, il)
    [
      | ins, class |

      if (il[mc:il_label])
	x86:label(code, il[mc:il_label][mc:l_mclabel]);

      ins = il[mc:il_ins];
      class = ins[mc:i_class];

      mc:lineno = il[mc:il_lineno];

      if (class == mc:i_compute) mgen_compute(code, ins)
      else if (class == mc:i_branch) mgen_branch(code, ins)
      else if (class == mc:i_trap) mgen_trap(code, ins)
      else if (class == mc:i_memory) mgen_memory(code, ins)
      else if (class == mc:i_closure) mgen_closure(code, ins)
      else if (class == mc:i_call) mgen_call(code, ins)
      else if (class == mc:i_return) mgen_return(code, ifn, ainfo, ins)
      else if (class == mc:i_vref) mgen_vref(code, ins)
      else fail()
    ];

  mgen_trap = fn (code, ins)
    [
      | trap, nerror, args, types, arg1, type1, arg2 |

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
	    arg2 = cadr(args);
	];

      if (trap == mc:trap_always)
	x86:trap(code, x86:balways, nerror, mc:lineno)
      else if (trap == mc:trap_loop)
	[
	  x86:dec(code, x86:lspecial, "xcount");
	  x86:trap(code, x86:be, nerror, mc:lineno);
	]
      else if (trap == mc:trap_global_write || trap == mc:trap_global_read)
	[
          // Don't check reads for >= V
          if (trap == mc:trap_global_write || mc:update_maxseclev == true)
            [
              | greg |
              greg = reg_arg0;
              if (trap == mc:trap_global_write)
                [
                  x86:mov(code, x86:lvar, arg2, x86:lreg, reg_arg0);
                  greg = reg_arg1;
                ];
              // ignores nerror value
              x86:mov(code, x86:lglobal, arg1[mc:v_name] . x86:gl_c,
                      x86:lreg, greg);
              call_builtin(code,
                           if (trap == mc:trap_global_write) "bwglobal"
                           else "brglobal");
            ];
	]
      else if (trap == mc:trap_type)
	// ignores nerror value
	type_trap(code, arg2[mc:v_kvalue], arg1, type1)
      else fail();
      // argcheck must be done in preamble as argcount doesn't survive
    ];


  mgen_return = fn (code, ifn, ainfo, ins)
    [
      | fmisc |

      fmisc = ifn[mc:c_fmisc];
      move(code, x86:lvar, ins[mc:i_rvalue], x86:lreg, reg_result);
      if (fmisc[mc:c_fm_globalsbase] ||
	  vfind?(x86:reg_globals, fmisc[mc:c_fm_regs_callee]))
	x86:pop(code, x86:lreg, x86:reg_globals);
      if (fmisc[mc:c_fm_closurebase] ||
	  vfind?(reg_closure, fmisc[mc:c_fm_regs_callee]))
	x86:pop(code, x86:lreg, reg_closure);
      if (update_maxseclev?(ifn))
        x86:pop(code, x86:lspecial, "maxseclevel");
      x86:leave(code);
      x86:ret(code);
    ];

  // map relops to x86 relops
  relops = '[0 0 0 0 0 0 0 0
             ,x86:be ,x86:bne ,x86:bl ,x86:bge ,x86:ble ,x86:bg];
  rev_x86relop = fn (op) op ^ 1; // reverse meaning of x86 branch
  // change op assuming cmp operands have been commuted
  commute_x86relop = '[-1 -1              // meaningless
                       ,x86:ba ,x86:bbe
                       ,x86:be ,x86:bne   // no change
                       ,x86:bae ,x86:bb   // swap
                       -1 -1              // meaningless
                       -1 -1              // meaningless
                       ,x86:bg ,x86:ble   // swap
                       ,x86:bge ,x86:bl]; // swap


  mgen_branch = fn (code, ins)
    [
      | op, dest, args, arg1, arg2, types, type1, type2 |

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
	x86:jmp(code, dest)
      else if (op == mc:branch_never)
        null
      else if (op == mc:branch_true)
	[
          | cop |
          cop = compare(code, mc:var_make_constant(false), arg1, x86:bne);
	  x86:jcc(code, cop, dest);
	]
      else if (op == mc:branch_false)
	[
          | cop |
          cop = compare(code, mc:var_make_constant(false), arg1, x86:be);
	  x86:jcc(code, cop, dest);
	]
      else if (op == mc:branch_or)
	[
	  logical_or(code, arg1, arg2);
	  x86:jcc(code, x86:bne, dest);
	]
      else if (op == mc:branch_nor)
	[
	  logical_or(code, arg1, arg2);
	  x86:jcc(code, x86:be, dest);
	]
      else if (op == mc:branch_and)
	[
	  | l, cop |

	  l = x86:new_label(code);
          cop = compare(code, mc:var_make_constant(false), arg1, x86:be);
	  x86:jcc(code, cop, l);
          cop = compare(code, mc:var_make_constant(false), arg2, x86:bne);
	  x86:jcc(code, cop, dest);
	  x86:label(code, l);
	]
      else if (op == mc:branch_nand)
	[
          | cop |
          cop = compare(code, mc:var_make_constant(false), arg1, x86:be);
	  x86:jcc(code, cop, dest);
          cop = compare(code, mc:var_make_constant(false), arg2, x86:be);
	  x86:jcc(code, cop, dest);
	]
      else if (op == mc:branch_eq || op == mc:branch_ne)
	[
          | cop |
	  cop = compare(code, arg1, arg2, relops[op]);
	  x86:jcc(code, cop, dest);
	]
      else if (op == mc:branch_equal || op == mc:branch_nequal)
        [
          push_args(code, ins[mc:i_bargs]);
          call_primitive(code, kequal?, equal?, null);
          x86:add(code, x86:limm, 4 * 2, x86:lreg, reg_sp); // pop args
          x86:cmp(code, x86:limm, mfalse, x86:lreg, reg_result);
          x86:jcc(code, if (op == mc:branch_equal) x86:bne else x86:be, dest);
        ]
      else if (op >= mc:branch_lt && op <= mc:branch_gt) // relop
	[
	  | x86op |

	  type_trap(code, type_integer, arg1, type1);
	  type_trap(code, type_integer, arg2, type2);

	  x86op = compare(code, arg2, arg1, relops[op]);
	  x86:jcc(code, x86op, dest);
	]
      else if (op >= mc:branch_slength && op < mc:branch_equal)
        [
          | n |
          n = mc:var_value(arg2);
          assert(integer?(n));

          | areg |
          arg1 = fetch_for_dest(code, arg1, arg1);
          areg = mc:get_reg(arg1);

          if (op >= mc:branch_vlength)
            [
              op -= mc:branch_vlength;
              n *= 4;
              type_trap(code, type_vector, arg1, type1);
            ]
          else
            [
              op -= mc:branch_slength;
              ++n;              // trailing zero
              type_trap(code, type_string, arg1, type1);
            ];
          op = relops[op + mc:branch_eq];
          n += x86:object_offset;

          x86:cmp(code, x86:limm, n, x86:lidx, areg . x86:object_size);
          x86:jcc(code, op, dest);
        ]
      else if (op >= mc:branch_immutable && op <= mc:branch_writable)
        [
          | inv?, flag |

          inv? = (op == mc:branch_mutable || op == mc:branch_writable);
          flag =
            if (op == mc:branch_immutable || op == mc:branch_mutable)
              MUDLLE_IMMUTABLE
            else
              MUDLLE_READONLY;

          if (!(type1 & ~(itype_integer | itype_null)))
            [
              if (!inv?) x86:jmp(code, dest);
            ]
          else
            [
              | xr, fdest, usef |

              fdest = x86:new_label(code);
              usef = false;

              xr = fetch1(code, arg1);

              if (type1 & itype_integer)
                [
                  x86:test(code, x86:limm, 1, x86:lreg, xr);
                  x86:jcc(code, x86:bne, if (inv?) fdest else dest);
                  if (inv?) usef = true;
                ];
              if (type1 & itype_null)
                [
                  x86:test(code, x86:lreg, xr, x86:lreg, xr);
                  x86:jcc(code, x86:be, if (inv?) fdest else dest);
                  if (inv?) usef = true;
                ];
              x86:test(code, x86:limm, flag, x86:lidx, xr . x86:object_flags);
              x86:jcc(code, if (inv?) x86:be else x86:bne, dest);

              if (usef) x86:label(code, fdest);
            ];
        ]
      else if (op == mc:branch_any_prim || op == mc:branch_not_prim)
        type_branch(code, fake_prim_type, op == mc:branch_not_prim,
                    arg1, type1, dest)
      else if (op >= mc:branch_type? && op < mc:branch_ntype?)
	type_branch(code, op - mc:branch_type?, false, arg1, type1, dest)
      else if (op >= mc:branch_ntype?
               && op < mc:branch_ntype? + last_synthetic_type)
	type_branch(code, op - mc:branch_ntype?, true, arg1, type1, dest)
      else
        fail();
    ];

  builtins =
    '[0 0
      "bleq" "blne" "bllt" "blle" "blgt" "blge"
      "bbitor" "bbitxor" "bbitand" "bshift_left" "bshift_right"
      "badd" "bsubtract" "bmultiply" "bdivide" "bremainder"
      "bnegate" "bnot" "bbitnot" 0 0 0 0 "bref" 0 0 0
      "bcar" "bcdr" 0 0 0 "btypeof" 0 0 0 0 0 0 0];
  assert(vlength(builtins) == mc:builtins);

  commute =
    '[0 0
      ,false ,false ,false ,false ,false ,false
      ,true ,true ,true ,false ,false
      ,false ,false ,true ,false ,false
      0 0 0 0 0 0 0 ,false 0 ,false 0
      0 0 0 0 0 0 0 0 0 0 0 0 0];
  assert(vlength(commute) == mc:builtins);

  // Type of compute op arguments (uses type_xxx/stype_xxx sets)
  typearg1 =
    '[0             // sc_or
      0             // sc_and
      ,stype_any    // eq
      ,stype_any    // ne
      ,type_integer // lt
      ,type_integer // le
      ,type_integer // gt
      ,type_integer // ge
      ,type_integer // bitor
      ,type_integer // bitxor
      ,type_integer // bitand
      ,stype_any    // shift_left; special handling
      ,type_integer // shift_right
      ,stype_any    // add
      ,type_integer // subtract
      ,stype_any    // multiply; special handling
      ,type_integer // divide
      ,type_integer // remainder
      ,type_integer // negate
      ,stype_any    // not
      ,type_integer // bitnot
      0             // ifelse
      0             // if
      0             // while
      0             // loop
      ,stype_any    // ref
      0             // set
      0             // cons
      0             // assign
      ,type_pair    // car
      ,type_pair    // cdr
      ,type_string  // slength
      ,type_vector  // vlength
      ,type_integer // iadd
      ,stype_any    // typeof
      0             // loop_count
      0             // max_loop_count
      ,type_symbol  // symbol_name
      ,type_symbol  // symbol_get
      0             // vector
      0             // sequence
      0             // pcons
    ];
  assert(vlength(typearg1) == mc:builtins);

  typearg2 =
    '[0             // sc_or
      0             // sc_and
      ,stype_any    // eq
      ,stype_any    // ne
      ,type_integer // lt
      ,type_integer // le
      ,type_integer // gt
      ,type_integer // ge
      ,type_integer // bitor
      ,type_integer // bitxor
      ,type_integer // bitand
      ,type_integer // shift_left
      ,type_integer // shift_right
      ,stype_any    // add
      ,type_integer // subtract
      ,stype_any    // multiply; special handling
      ,type_integer // divide
      ,type_integer // remainder
      0             // negate
      0             // not
      0             // bitnot
      0             // ifelse
      0             // if
      0             // while
      0             // loop
      ,stype_any    // ref
      0             // set
      0             // cons
      0             // assign
      0             // car
      0             // cdr
      0             // slength
      0             // vlength
      ,type_integer // iadd
      0             // typeof
      0             // loop_count
      0             // max_loop_count
      0             // symbol_name
      0             // symbol_get
      0             // vector
      0             // sequence
      0             // pcons
    ];
  assert(vlength(typearg2) == mc:builtins);

  var_in_reg = fn (var, reg)
    if (mc:in_reg(var) && mc:get_reg(var) == reg)
      var
    else
      [
        var = vcopy(var);
        var[mc:v_location] = vector(mc:v_lregister, mc:reg_scratch, reg);
        var
      ];

  reg_dest = fn (dvar)
    if (mc:in_reg(dvar)) mc:get_reg(dvar)
    else reg_scratch;

  inline1? = fn (op, arg, type)
    // Returns: True if op should be inlined on arg
    if (op == mc:b_typeof)
      (type & (itype_null | itype_integer)) == 0
    else
      true;

  mgen_inline1 = fn (code, op, r, type, d)
    // Effects: Generates code for d = op r
    //   d, r are variables
    if (op == mc:b_negate)
      [
	move(code, x86:lvar, r, x86:lvar, d);
	x86:neg(code, x86:lvar, d);
	x86:add(code, x86:limm, 2, x86:lvar, d);
      ]
    else if (op == mc:b_bitnot)
      [
	move(code, x86:lvar, r, x86:lvar, d);
	x86:not(code, x86:lvar, d);
	x86:or(code, x86:limm, 1, x86:lvar, d);
      ]
    else if (op == mc:b_not)
      [
	x86:cmp(code, x86:limm, mfalse, x86:lvar, r);
	setcc(code, x86:be, d);
      ]
    else if (op == mc:b_car || op == mc:b_symbol_name)
      [
	| xr |

	xr = fetch1(code, r);
	move(code, x86:lidx, xr . x86:object_offset, x86:lvar, d)
      ]
    else if (op == mc:b_cdr || op == mc:b_symbol_get)
      [
	| xr |

	xr = fetch1(code, r);
	move(code, x86:lidx, xr . x86:object_offset + 4, x86:lvar, d)
      ]
    else if (op == mc:b_slength)
      [
	| xr, dr |

	xr = fetch1(code, r);
	dr = reg_dest(d);

	move(code, x86:lidx, xr . x86:object_size, x86:lreg, dr);
	x86:lea(code, x86:lridx, dr . 1 . dr . -(2 * x86:object_offset + 1),
		x86:lreg, dr);
	move(code, x86:lreg, dr, x86:lvar, d);
      ]
    else if (op == mc:b_vlength)
      [
	| xr, dr |

	xr = fetch1(code, r);
	dr = reg_dest(d);

	move(code, x86:lidx, xr . x86:object_size, x86:lreg, dr);
	x86:shr(code, x86:limm, 1, x86:lreg, dr);
	x86:sub(code, x86:limm, (x86:object_offset >> 1) - 1, x86:lreg, dr);
	move(code, x86:lreg, dr, x86:lvar, d);
      ]
    else if (op == mc:b_typeof)
      [
        | dr, rr |

        // we know it's a pointer type; cf. inline1?()
        dr = reg_dest(d);
        rr = fetch1(code, r);
        x86:movzxbyte(code, x86:lidx, rr . x86:object_type, x86:lreg, dr);
        x86:lea(code, x86:lridx, dr . 1 . dr . 1, x86:lreg, dr);
        move(code, x86:lreg, dr, x86:lvar, d);
      ]
    else fail();

  inline2? = fn (op, arg1, type1, arg2, type2)
    // Returns: True if op should be inlined on arg1, arg2
    if (op == mc:b_add) !(type1 & itype_string) || !(type2 & itype_string)
    else if (op == mc:b_shift_left || op == mc:b_shift_right)
      intcst?(arg2)
    else if (op == mc:b_ref) // may inline vector_ref or string_ref
      [
        | c |
        c = mc:var_value(arg2);
        ((integer?(c) && c >= 0 && c < (1 << 27)) &&
         (!(type1 & itype_vector) || !(type1 & itype_string)))
      ]
    else if (op == mc:b_multiply)
      intcst?(arg1) || intcst?(arg2)
    else
      !(op == mc:b_divide || op == mc:b_remainder);

  relop = fn (code, branch, r1, r2, d)
    [
      branch = compare(code, r2, r1, branch);
      setcc(code, branch, d);
    ];

  intcst? = fn (v)
    v[mc:v_class] == mc:v_constant && integer?(v[mc:v_kvalue]);

  leaaddcst = fn (code, r1, r2, d, subtract)
    [
      | cst |

      if (!mc:in_reg(d)) exit<function> false;

      if (!subtract && integer?(cst = mc:var_value(r1)))
	r1 = r2
      else if (!integer?(cst = mc:var_value(r2)))
	exit<function> false;

      if (!mc:in_reg(r1)) exit<function> false;
      r1 = mc:get_reg(r1);
      d = mc:get_reg(d);
      if (d == r1) exit<function> false;

      // Doesn't leave much, does it ?
      if (cst <= -(1 << 29) || cst >= (1 << 29))
	exit<function> false;
      if (subtract) cst = -cst;
      x86:lea(code, x86:lidx, r1 . (cst << 1), x86:lreg, d);
      true
    ];

  // return list of factors of n:
  //   9, 8, 5, 4, 3, 2, or -n for other powers of 2: (1 << n)
  // or null if we should use imul
  | multiply_factorize |
  multiply_factorize = fn (n, neg?)
    [
      assert(n > 0);

      | f, n2, n3, n5, d |
      n2 = 0;                   // number of powers of 2
      while (!(n & 255))
        [
          n2 += 8;
          n >>= 8;
        ];
      while (!(n & 1))
        [
          ++n2;
          n >>= 1;
        ];
      n3 = 0;                   // number of powers of 3
      while ((d = n / 3) * 3 == n)
        [
          ++n3;
          n = d;
        ];
      n5 = 0;                   // number of powers of 5
      while ((d = n / 5) * 5 == n)
        [
          ++n5;
          n = d;
        ];

      if (n != 1)
        // other prime factors than 2, 3, and 5
        exit<function> null;
      if (n5 + (n3 + 1) / 2 + (n2 > 0) > (if (neg?) 2 else 3))
        // too expensive, use imul
        exit<function> null;

      while (--n5 >= 0)
        f = 5 . f;
      while (n3 >= 2)
        [
          f = 9 . f;
          n3 -= 2;
        ];
      if (n3)
        f = 3 . f;
      if (n2 > 3)
        f = -n2 . f
      else if (n2 > 0)
        f = (1 << n2) . f;
      f
    ];

  mgen_inline2 = fn (code, op, r1, type1, r2, type2, d, ins)
    // Effects: Generates code for d = r1 op r2
    //   type1 & type2 are provided to help generate code for polymorphic ops
    //   r1, r2, d are variables
    if (op == mc:b_eq) relop(code, x86:be, r1, r2, d)
    else if (op == mc:b_ne) relop(code, x86:bne, r1, r2, d)
    else if (op == mc:b_lt) relop(code, x86:bl, r1, r2, d)
    else if (op == mc:b_le) relop(code, x86:ble, r1, r2, d)
    else if (op == mc:b_gt) relop(code, x86:bg, r1, r2, d)
    else if (op == mc:b_ge) relop(code, x86:bge, r1, r2, d)
    else if (op == mc:b_bitor)
      perform3(code, r1, r2, d, x86:or, x86:or)
    else if (op == mc:b_bitand)
      perform3(code, r1, r2, d, x86:and, x86:and)
    else if (op == mc:b_bitxor)
      [
	| cstxor, normalxor |

	cstxor = fn (code, i, m1, a1)
	  x86:xor(code, x86:limm, x86:doubleint(i), m1, a1);

	normalxor = fn (code, m1, a1, m2, a2)
	  [
	    x86:xor(code, m1, a1, m2, a2);
	    x86:or(code, x86:limm, 1, m2, a2);
	  ];

	perform3cst(code, r1, r2, d, normalxor, normalxor,
		    cstxor, cstxor);
      ]
    else if (op == mc:b_shift_left)
      mgen_inline2(code, mc:b_multiply, r1, type1,
                   mc:var_make_constant(1 << (r2[mc:v_kvalue] & 31)),
                   itype_integer, d, ins)
    else if (op == mc:b_shift_right)
      [
	| i |

	i = r2[mc:v_kvalue] & 31;
	move(code, x86:lvar, r1, x86:lvar, d);
        if (i != 0)
          [
            x86:sar(code, x86:limm, i & 31, x86:lvar, d);
            x86:or(code, x86:limm, 1, x86:lvar, d);
          ]
      ]
    else if (op == mc:b_multiply) // integer multiply with constant
      [
        | c, neg? |
        c = mc:var_value(r2);
        assert(integer?(c));

        if (c >= -1 && c <= 1)
          [
            type_trap(code, type_integer, r1, type1);
            if (c == 0)
              move(code, x86:limm, mzero, x86:lvar, d)
            else if (c == 1)
              move(code, x86:lvar, r1, x86:lvar, d)
            else
              mgen_inline1(code, mc:b_negate, r1, type1, d);
            exit<function> null;
          ];

        if (neg? = (c < 0))
          c = -c;

        | dr, factors |
        dr = reg_dest(d);
        if (c == minint)
          [
            neg? = false;
            factors = '(,(-(intbits - 1)))
          ]
        else
          [
            factors = multiply_factorize(c, neg?);
            if (factors == null)
              [
                type_trap(code, type_integer, r1, type1);
                if (neg?)
                  c = -c;
                x86:imul(code, x86:lvar, r1, x86:lidx, dr . c);
                x86:sub(code, x86:limm, c - 1, x86:lreg, dr);
                exit<function> move(code, x86:lreg, dr, x86:lvar, d);
              ];
          ];

        | r |
        r = fetch1(code, r1);
        type_trap(code, type_integer, var_in_reg(r1, r), type1);

        | excess |
        excess = 1;
        while (factors != null)
          <next> [
            | f |
            @(f . factors) = factors;

            if (f < 0)
              [
                f = -f;
                move(code, x86:lreg, r, x86:lreg, dr);
                x86:shl(code, x86:limm, f, x86:lreg, dr);
                excess <<= f;
              ]
            else
              [
                | mod |
                excess *= f;
                mod = 0;
                if (factors == null)
                  [
                    mod = (if (neg?) -1 else 1) - excess;
                    excess += mod;
                  ];

                if (f == 2 && r == dr && mod == 0)
                  x86:add(code, x86:lreg, r, x86:lreg, dr)
                else if (f == 2 || f == 3 || f == 5 || f == 9)
                  x86:lea(code, x86:lridx, r . (f - 1) . r . mod, x86:lreg, dr)
                else if (f == 4 || f == 8)
                  x86:lea(code, x86:lqidx, r . f . mod, x86:lreg, dr)
                else
                  fail();
              ];
            r = dr;
          ];

        assert(r == dr);
        if (neg?)
          [
            x86:neg(code, x86:lreg, dr);
            excess = -excess;
          ];
        --excess;               // need one left
        if (excess > 0)
          x86:sub(code, x86:limm, excess, x86:lreg, dr)
        else if (excess == -1)
          x86:or(code, x86:limm, 1, x86:lreg, dr)
        else if (excess < 0)
          x86:add(code, x86:limm, -excess, x86:lreg, dr);
        exit<function> move(code, x86:lreg, dr, x86:lvar, d);
      ]
    else if (op == mc:b_add || op == mc:b_iadd) // integer addition only
      [
	if (op == mc:b_add)
	  [
	    type_trap(code, type_integer, r1, type1);
	    type_trap(code, type_integer, r2, type2);
	  ];

	if (!leaaddcst(code, r1, r2, d, false))
	  if (mc:in_reg(r1) && mc:in_reg(r2))
	    [
	      | dr |

	      dr = reg_dest(d);
	      x86:lea(code, x86:lridx,
                      mc:get_reg(r1) . 1 . mc:get_reg(r2) . -1,
		      x86:lreg, dr);
	      move(code, x86:lreg, dr, x86:lvar, d);
	    ]
	  else
	    [
	      | nadd, cstadd |


	      nadd = fn (code, m1, a1, m2, a2)
		[
		  x86:add(code, m1, a1, m2, a2);
		  x86:dec(code, m2, a2);
		];

	      cstadd = fn (code, i, m1, a1)
		x86:add(code, x86:limm, x86:doubleint(i), m1, a1);

	      perform3cst(code, r1, r2, d, nadd, nadd, cstadd, cstadd);
	    ];
      ]
    else if (op == mc:b_subtract)
      [
	if (!leaaddcst(code, r1, r2, d, true))
	  perform3cst(code, r1, r2, d,
		      fn (code, m1, a1, m2, a2)
		      [
			x86:sub(code, m1, a1, m2, a2);
                        x86:or(code, x86:limm, 1, m2, a2);
		      ],
		      false,
		      fn (code, i, m1, a1)
		        x86:sub(code, x86:limm, x86:doubleint(i), m1, a1),
		      false)
      ]
    else if (op == mc:b_ref)
      [
	| c, reg1 |

	c = mc:var_value(r2);
        r1 = fetch_for_dest(code, r1, r1);
        reg1 = mc:get_reg(r1);

	if (!(type1 & itype_string)) // vector
	  [
	    type_trap(code, type_vector, r1, type1 & ~itype_null);

	    if (integer?(c) && c >= 0 && c < (1 << 27))
	      [
                | unsafe? |
                unsafe? = c >= ins[mc:i_asizeinfo];
		c = 4 * c + x86:object_offset;
                if (unsafe?)
                  [
                    x86:cmp(code, x86:limm, c,
                            x86:lidx, reg1 . x86:object_size);
                    x86:trap(code, x86:bbe, error_bad_index, mc:lineno);
                  ];
		move(code, x86:lidx, reg1 . c, x86:lvar, d);
	      ]
	    else
              fail();
	  ]
	else if (!(type1 & itype_vector)) // string
	  [
	    | dr |

	    type_trap(code, type_string, r1, type1 & ~itype_null);
	    dr = reg_dest(d);

	    if (integer?(c) && c >= 0 && c < (1 << 27))
	      [
                | unsafe? |
                unsafe? = c >= ins[mc:i_asizeinfo];
		c += x86:object_offset;
                if (unsafe?)
                  [
                    x86:cmp(code, x86:limm, c + 1, x86:lidx,
                            reg1 . x86:object_size);
                    x86:trap(code, x86:bbe, error_bad_index, mc:lineno);
                  ];
		x86:movzxbyte(code, x86:lidx, reg1 . c, x86:lreg, dr);
	      ]
	    else
              fail();

	    x86:lea(code, x86:lridx, dr . 1 . dr . 1, x86:lreg, dr);
	    move(code, x86:lreg, dr, x86:lvar, d);
	  ]
	else fail()
      ]
    else fail();


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
	move(code, x86:lvar, arg1, x86:lvar, dest)
      else if (op == mc:b_loop_count)
        [
          | dr |
          dr = reg_dest(dest);
          x86:mov(code, x86:lspecial, "xcount", x86:lreg, dr);
          x86:lea(code, x86:lridx, dr . 1 . dr . 1, x86:lreg, dr);
          move(code, x86:lreg, dr, x86:lvar, dest);
        ]
      else if (op == mc:b_max_loop_count)
        [
          | dr |
          dr = reg_dest(dest);
          x86:lea(code, x86:lspecial, "max_loop_count", x86:lreg, dr);
          move(code, x86:lreg, dr, x86:lvar, dest);
        ]
      else if (op == mc:b_cons || op == mc:b_pcons)
	[
	  if (in_scratch?(arg2) || in_scratch?(arg1)) fail();
	  call_builtin(code, "balloc_cons");
	  move(code, x86:lvar, arg1, x86:lidx,
               reg_arg1 . x86:object_offset);
	  move(code, x86:lvar, arg2, x86:lidx,
               reg_arg1 . x86:object_offset + 4);
          if (op == mc:b_pcons)
            x86:or(code, x86:limm, MUDLLE_READONLY, x86:lidx,
                   reg_arg1 . x86:object_flags);
	  move(code, x86:lreg, reg_arg1, x86:lvar, dest);
	]
      else if (op == mc:b_vector || op == mc:b_sequence)
	[
          | nargs, ofs |
          nargs = llength(args);
          move(code, x86:limm, 4 * nargs + x86:object_offset,
               x86:lreg, reg_arg0);
	  call_builtin(code, "balloc_vector");
          ofs = 0;
          lforeach(fn (arg) [
            assert(!in_scratch?(arg));
            move(code, x86:lvar, arg, x86:lidx,
                 reg_arg1 . x86:object_offset + ofs);
            ofs += 4
          ], args);
          if (op == mc:b_sequence)
            x86:or(code, x86:limm, MUDLLE_READONLY, x86:lidx,
                   reg_arg1 . x86:object_flags);
	  move(code, x86:lreg, reg_arg1, x86:lvar, dest);
	]
      else if (arg2 == null)	// 1-argument ops
	if (inline1?(op, arg1, type1))
	  [
            if (op == mc:b_car || op == mc:b_cdr
                || op == mc:b_symbol_name
                || op == mc:b_symbol_get
                || op == mc:b_slength
                || op == mc:b_vlength)
              [
                // will cause error later on otherwise
                type1 &= ~itype_null;
                arg1 = fetch_for_dest(code, arg1, dest);
              ];
	    type_trap(code, typearg1[op], arg1, type1);
	    mgen_inline1(code, op, arg1, type1, dest);
	    ++mc:nops_inlined;
	  ]
	else
	  [
	    ++mc:nops_called;
            callop1(code, builtins[op], arg1);
	    move(code, x86:lreg, reg_arg0, x86:lvar, dest);
	  ]
      else // 2-argument ops
	if (inline2?(op, arg1, type1, arg2, type2))
	  [
	    | t |

	    if (arg1[mc:v_class] == mc:v_constant && commute[op])
	      [
		// swap arg1 and arg2 (x86 prefers constants as arg2)
		t = arg1; arg1 = arg2; arg2 = t;
		t = type1; type1 = type2; type2 = t;
	      ];
	    type_trap(code, typearg1[op], arg1, type1);
	    type_trap(code, typearg2[op], arg2, type2);
	    mgen_inline2(code, op, arg1, type1, arg2, type2, dest, ins);
	    ++mc:nops_inlined;
	  ]
	else
	  [
            | c |
	    ++mc:nops_called;
            // empty string addition commutes as well
            c = (commute[op]
                 || (op == mc:b_add
                     && (equal?(mc:var_value(arg1), "")
                         || equal?(mc:var_value(arg1), ""))));
	    callop2(code, builtins[op], c, arg1, arg2);
	    move(code, x86:lreg, reg_arg0, x86:lvar, dest);
	  ]
    ];

  mgen_memory = fn (code, ins)
    [
      | array, areg, scalar, offset |

      array = ins[mc:i_marray];
      areg = mc:get_reg(array);
      scalar = ins[mc:i_mscalar];
      offset = x86:object_offset + 4 * ins[mc:i_mindex];

      areg = fetch1(code, array);

      if (ins[mc:i_mop] == mc:memory_read)
	move(code, x86:lidx, areg . offset, x86:lvar, scalar)
      else
        [
          if (ins[mc:i_mop] == mc:memory_write_safe)
            [
              x86:test(code, x86:limm, MUDLLE_READONLY,
                       x86:lidx, areg . x86:object_flags);
              x86:trap(code, x86:bne, error_value_read_only, mc:lineno);
            ];
          // write - note: reg_scratch may be in use, allow reg_ecx too
          safemove(code, x86:lvar, scalar,
                   x86:lidx, areg . offset, regs_allscratch);
        ];
    ];

  mgen_closure = fn (code, ins)
    [
      | cvars, f, offset, cdest |

      f = ins[mc:i_ffunction];
      cvars = lfilter(fn (cvar) cvar[mc:v_cparent] != mc:myself,
		      f[mc:c_fclosure]);

      cdest = ins[mc:i_fdest];

      if (cvars == null)
        [
          move(code, x86:lclosure, f, x86:lvar, cdest);
          exit<function> null;
        ];

      move(code, x86:limm, 4 * (1 + llength(cvars)) + x86:object_offset,
	   x86:lreg, reg_arg0);
      call_builtin(code, "balloc_closure");
      move(code, x86:lfunction, f, x86:lidx, reg_arg1 . x86:object_offset);
      offset = x86:object_offset + 4;
      while (cvars != null)
	[
	  | cvar |

	  cvar = car(cvars)[mc:v_cparent];
	  if (cvar == cdest) // place ourselves in closure
	    move(code, x86:lreg, reg_arg1, x86:lidx, reg_arg1 . offset)
	  else
	    move(code, x86:lvar, cvar, x86:lidx, reg_arg1 . offset);
	  offset += 4;
	  cvars = cdr(cvars);
	];
      move(code, x86:lreg, reg_arg1, x86:lvar, cdest);
    ];

  mgen_vref = fn (code, ins)
    [
      | arg, dest |
      dest = ins[mc:i_vdest];
      arg = ins[mc:i_varg];
      move(code, x86:lvar, arg, x86:lvar, dest);
    ];

  // Push arguments on the stack, in reverse order
  push_args = fn (code, args)
    lforeach(fn (v) x86:push(code, x86:lvar, v), lreverse(args));

  call_primitive = fn (code, called, prim, {null,vector} dest)
    [
      | flags |

      flags = primitive_flags(prim);

      // Save stack frame address (for GC and call traces)
      call_builtin(
        code,
        if (flags & OP_NOALLOC) "bsave_caller_noalloc" else "bsave_caller");

      x86:callrel_prim(code, called[mc:v_name]);

      | dreg |
      if (vector?(dest) && mc:in_reg(dest))
        dreg = mc:get_reg(dest);

      if (flags & OP_NOALLOC)
        [
          if (dreg != x86:reg_edx)
            x86:xor(code, x86:lreg, x86:reg_edx, x86:lreg, x86:reg_edx);
          // if noalloc, %edi cannot have "gone bad"
          if (dreg != x86:reg_edi
              && !(flags & OP_NOALLOC))
            x86:xor(code, x86:lreg, x86:reg_edi, x86:lreg, x86:reg_edi);
        ]
      else
        call_builtin(code, "brestore_caller");

      true
    ];

  call_closure = fn (code, called, nargs, f)
    [
      if (called[mc:v_class] == mc:v_function)
        move(code, x86:lvar, called, x86:lreg, x86:reg_ecx)
      else if (closure?(f) && (closure_flags(f) & clf_noclosure))
        [
          assert(called[mc:v_class] == mc:v_global_constant);
          move(code, x86:lfunction, called[mc:v_name], x86:lreg, x86:reg_ecx);
        ]
      else
        [
          move(code, x86:lvar, called, x86:lreg, reg_closure_in);
          move(code, x86:lidx, reg_closure_in . x86:object_offset,
               x86:lreg, x86:reg_ecx);
        ];
      move(code, x86:limm, nargs, x86:lreg, reg_argcount);
      x86:add(code, x86:limm, x86:object_offset + x86:function_offset,
              x86:lreg, x86:reg_ecx);
      x86:call(code, x86:lreg, x86:reg_ecx);
      true
    ];

  call = fn (code, called, nargs, callprimop, seclev?)
    [
      // update find_mcode() in error.c if these instructions change
      move(code, x86:lvar, called, x86:lreg, reg_closure_in);
      move(code, x86:limm, nargs, x86:lreg, reg_argcount);
      if (seclev?)
        set_seclev(code, reg_arg1, x86:sl_c);
      call_builtin(code, callprimop);

      true
    ];

  call_varargs = fn (code, called, func, args, dest)
    [
      | nargs |

      nargs = llength(args);

      if (lforall?(fn (v) v[mc:v_class] == mc:v_constant, args))
        [
          | argv |

          argv = check_immutable(protect(
            list_to_vector(lmap(fn (v) v[mc:v_kvalue], args))));
          assert(immutable?(argv));

          move(code, x86:lvar, mc:var_make_constant(argv), x86:lreg, reg_arg0);
          x86:lea(code, x86:lprimitive, called[mc:v_name], x86:lreg,
                  reg_closure_in);
          set_seclev(code, reg_arg1, x86:sl_c);
          call_builtin(code, "bapply_varargs");
          move(code, x86:lreg, reg_result, x86:lvar, dest);

          exit<function> true;
        ];

      push_args(code, args);
      x86:lea(code, x86:lprimitive, called[mc:v_name],
              x86:lreg, reg_closure_in);
      move(code, x86:limm, nargs, x86:lreg, reg_argcount);
      set_seclev(code, reg_arg1, x86:sl_c);
      call_builtin(code, "bcall_varargs");
      if (nargs > 0)
	x86:add(code, x86:limm, 4 * nargs, x86:lreg, reg_sp); // pop args
      move(code, x86:lreg, reg_result, x86:lvar, dest);
    ];

  call_bconcat = fn (code, args, dest)
    [
      | nargs |
      nargs = llength(args);
      push_args(code, args);
      move(code, x86:limm, nargs, x86:lreg, reg_arg0);
      call_builtin(code, "bconcat");
      if (nargs > 0)
        x86:add(code, x86:limm, 4 * nargs, x86:lreg, reg_sp);
      move(code, x86:lreg, reg_arg0, x86:lvar, dest);
    ];

  call_kset = fn (code, args, dest, types, sizeinfo)
    [
      | obj, idx, nidx, val, type1 |
      @(obj idx val) = args;

      if (!(types
            && ((type1 = car(types = cdr(types))) == itype_vector
                || type1 == itype_string)
            && integer?(nidx = mc:var_value(idx))
            && nidx >= 0 && nidx < sizeinfo))
        [
          callop3(code, "bset", obj, idx, val);
          move(code, x86:lreg, reg_result, x86:lvar, dest);
          exit<function> null;
        ];

      | robj, rval, rdest |

      rval = fetch1(code, val);
      if (rval == reg_scratch)
        robj = fetch1(code, obj)
      else
        robj = fetch2(code, obj);
      // robj == rval is still possible for obj[idx] = obj

      x86:test(code, x86:limm, MUDLLE_READONLY,
               x86:lidx, robj . x86:object_flags);
      x86:trap(code, x86:bne, error_value_read_only, mc:lineno);

      rdest = reg_dest(dest);
      if (type1 == itype_vector)
        [
          nidx = nidx * 4 + x86:object_offset;
          x86:mov(code, x86:lreg, rval, x86:lidx, robj . nidx);
          move(code, x86:lreg, rval, x86:lvar, dest);
          exit<function> null;
        ];

      // it's a string
      nidx += x86:object_offset;

      // make sure we have an 8-bit capable register that isn't
      // GC-protected as it will end up with a non-mudlle value
      if (!vfind?(rval, regs_allscratch))
        [
          | nr |
          nr = if (robj == reg_scratch) reg_arg1 else reg_scratch;
          move(code, x86:lreg, rval, x86:lreg, nr);
          rval = nr
        ];

      type_trap(code, type_integer, var_in_reg(val, rval),
                caddr(types));

      assert(rval != robj);

      | rval8 |
      rval8 = x86:reg8[rval];

      x86:shr(code, x86:limm, 1, x86:lreg, rval);
      x86:movbyte(code, x86:lreg, rval8, x86:lidx, robj . nidx);
      x86:movzxbyte(code, x86:lreg, rval8, x86:lreg, rdest);
      x86:lea(code, x86:lridx, rdest . 1 . rdest . 1,
              x86:lreg, rdest);
      move(code, x86:lreg, rdest, x86:lvar, dest);
    ];

  call_seclevel = fn (code, dest, type)
    move(code, x86:lseclev, type, x86:lvar, dest);

  maybe_call_global_lookup = fn (code, arg, dest)
    [
      | val |
      if (!string?(val = mc:var_value(arg)))
        exit<function> false;
      move(code, x86:lglobal, val . x86:gl_mudlle, x86:lvar, dest);
      exit<function> true;
    ];

  mgen_call = fn (code, ins)
    [
      | args, nargs, called, dest, done |

      args = ins[mc:i_cargs];
      called = car(args);
      args = cdr(args);
      nargs = llength(args);
      dest = ins[mc:i_cdest];
      done = false;

      if (called == kset)
        [
          if (nargs == 3)
            exit<function> call_kset(code, args, dest, ins[mc:i_ctypes],
                                     ins[mc:i_csizeinfo]);
        ]
      else if (called == kseclevel)
        [
          if (nargs == 0)
            exit<function> call_seclevel(code, dest, x86:sl_mudlle);
        ]
      else if (called == kmaxseclevel)
        [
          if (nargs == 0)
            exit<function> move(code, x86:lspecial, "maxseclevel",
                                x86:lvar, dest);
        ]
      else if (called == kglobal_lookup)
        [
          if (nargs == 1
              && maybe_call_global_lookup(code, car(args), dest))
            exit<function> null;
        ]
      else if (called == kconcat_strings)
        exit<function> call_bconcat(code, args, dest);

      // Optimise calls to global constants
      if (called[mc:v_class] == mc:v_global_constant)
	[
	  | f, t |

	  f = global_value(called[mc:v_goffset]);
	  t = typeof(f);
	  if (t == type_varargs)
	    exit<function> call_varargs(code, called, f, args, dest);

          push_args(code, args);

	  if (t == type_primitive && primitive_nargs(f) == nargs)
            done = call_primitive(code, called, f, dest)
	  else if (t == type_secure)
	    done = call(code, called, nargs, "bcall_secure", true)
	  else if (t == type_closure)
	    done = call_closure(code, called, nargs, f);
	]
      else
        push_args(code, args);

      if (!done)
        if (vector?(ins[mc:i_cfunction]))
          call_closure(code, called, nargs, null)
        else
          call(code, called, nargs, "bcall", false);

      if (nargs > 0)
	x86:add(code, x86:limm, 4 * nargs, x86:lreg, reg_sp); // pop args
      move(code, x86:lreg, reg_result, x86:lvar, dest);
    ];

  type_branch = fn (code, type, reversed, arg, typeset, dest)
    // Effects: Generates:
    //   !reversed: if typeof(arg) == type goto dest
    //   reversed: if typeof(arg) != type goto dest
    //   typeset is inferred type information on arg
    [
      | success, commit, abort, fail, abort_notptr, usedfail, r |

      fail = x86:new_label(code);
      usedfail = false;

      // Handle the 3 cases that arise in positive/negative typechecks:
      //   - value *not* of type (abort)
      //     flow of control must not proceed (eg not pointer)
      //   - value *of* type (success)
      //   - end of type check, final test (commit)
      if (reversed)
	[
	  commit = fn (cc)
	    x86:jcc(code, rev_x86relop(cc), dest);

	  success = fn (cc)
	    [
	      x86:jcc(code, cc, fail);
	      usedfail = true;
	    ];

	  abort = fn (cc)
	    x86:jcc(code, cc, dest);
	]
      else
	[
	  commit = fn (cc)
	    x86:jcc(code, cc, dest);

	  success = fn (cc)
	    x86:jcc(code, cc, dest);

	  abort = fn (cc)
	    [
	      x86:jcc(code, cc, fail);
	      usedfail = true;
	    ];
	];

      // checks can now assume the !reversed case

      abort_notptr = fn (r, typeset)
	[
	  if (typeset & itype_integer)
	    [
	      x86:test(code, x86:limm, 1, x86:lreg, r);
	      abort(x86:bne);
	    ];
	  if (typeset & itype_null)
	    [
	      x86:test(code, x86:lreg, r, x86:lreg, r);
	      abort(x86:be);
	    ];
	];

      for (;;)
	[
          | itype, itype_inv |
          if (type == fake_prim_type)
            [
              itype     = mc:itypemap[stype_function];
              itype_inv = itype_any;
            ]
          else
            [
              itype     = mc:itypemap[type];
              itype_inv = mc:itypemap_inverse[type];
            ];
	  if ((itype & typeset) == itype_none)
	    [
	      // is not of given type
	      if (reversed) x86:jmp(code, dest)
	    ]
	  else if ((itype_inv & typeset) == itype_none)
	    [
	      // is of given type
	      if (!reversed) x86:jmp(code, dest)
	    ]
	  else if (type == type_integer)
	    [
	      x86:test(code, x86:limm, 1, x86:lvar, arg);
	      commit(x86:bne);
	    ]
	  else if (type == type_null)
	    [
              cmpeq(code, x86:limm, 0, x86:lvar, arg);
	      commit(x86:be);
	    ]
	  else if (type == stype_list)
	    [
	      if (r == null)
		r = fetch1(code, arg);

	      // if null or int are possible, check for those and try
	      // again
	      if (typeset & (itype_null | itype_integer))
		[
		  // Note: success for null...
		  if (typeset & itype_null)
		    [
		      x86:test(code, x86:lreg, r, x86:lreg, r);
		      success(x86:be);
		    ];
		  if (typeset & itype_integer)
		    [
		      x86:test(code, x86:limm, 1, x86:lreg, r);
		      abort(x86:bne);
		    ];
		  // stype_list becomes type_pair as we now know it's
		  // not null
		  type = type_pair;
		  typeset &= ~(itype_null | itype_integer);
		  exit<continue> null;
		];

	      x86:cmpbyte(code, x86:limm, type_pair,
			  x86:lidx, r . x86:object_type);
	      commit(x86:be);
	    ]
	  else if (typeset & (itype_null | itype_integer))
	    [
	      // if null or int are possible, check for those and try
	      // again
	      if (r == null)
		r = fetch1(code, arg);
	      abort_notptr(r, typeset);
	      typeset &= ~(itype_null | itype_integer);
	      exit<continue> null;
	    ]
	  else if (type == stype_function || type == fake_prim_type)
	    [
	      if (r == null)
		r = fetch1(code, arg);
              if (type == stype_function)
                [
                  x86:cmpbyte(code, x86:limm, type_closure,
                              x86:lidx, r . x86:object_type);
                  success(x86:be);
                ];
	      x86:cmpbyte(code, x86:limm, garbage_primitive,
			  x86:lidx, r . x86:object_info);
	      commit(x86:be);
	    ]
	  else			// generic type check
	    [
	      if (r == null)
		r = fetch1(code, arg);
	      x86:cmpbyte(code, x86:limm, type, x86:lidx, r . x86:object_type);
	      commit(x86:be);
	    ];
	  exit<break> null;
	];

      if (usedfail) x86:label(code, fail);
    ];

  type_trap = fn (code, type, var, typeset)
    // Effects: Generate typecheck for value in var, given type
    //   knowledge typeset.
    [
      | trap, trap_unaligned |

      // null traps are caught by the signal handler (see runtime.c)
      trap = fn (cc) x86:trap(code, cc, error_bad_type, mc:lineno);
      trap_unaligned = fn (r)
	[
	  x86:test(code, x86:limm, 1, x86:lreg, r);
	  trap(x86:bne);
	];

      // if value cannot pass trap, trap unconditionally
      if ((mc:itypemap[type] & typeset) == itype_none)
	trap(x86:balways)
      // if value must pass trap, don't test
      else if ((mc:itypemap_inverse[type] & typeset) == itype_none)
	0
      else
	[
	  if (type == type_integer)
	    [
	      x86:test(code, x86:limm, 1, x86:lvar, var);
	      exit<function> trap(x86:be);
	    ];
	  if (type == type_null)
	    [
	      x86:cmp(code, x86:limm, 0, x86:lvar, var);
	      exit<function> trap(x86:bne);
	    ];

	  if (type == stype_function)
	    [
	      | r |

	      r = fetch1(code, var);
	      if (typeset & itype_integer)
                [
                  trap_unaligned(r);
                  var = var_in_reg(var, r);
                  exit<function>
                    type_trap(code, type, var, typeset & ~itype_integer);
                ];

              x86:cmpbyte(code, x86:limm, type_closure,
                          x86:lidx, r . x86:object_type);

	      | ok |
              ok = x86:new_label(code);
	      x86:jcc(code, x86:be, ok);
              x86:cmpbyte(code, x86:limm, garbage_primitive,
                          x86:lidx, r . x86:object_info);
	      trap(x86:bne);
	      x86:label(code, ok);
	    ]
	  else if (type == stype_list)
	    [
              if (!(typeset & itype_null))
                exit<function> type_trap(code, type_pair, var, typeset);

	      | ok, r |
	      ok = x86:new_label(code);
	      r = fetch1(code, var);
              x86:test(code, x86:lreg, r, x86:lreg, r);
              x86:jcc(code, x86:be, ok);

              var = var_in_reg(var, r);
              type_trap(code, type_pair, var, typeset & ~itype_null);
	      x86:label(code, ok);
	    ]
	  else
	    [
	      | r |

	      // relies on reads from address null + x86:object_type causing
	      // traps
	      r = fetch1(code, var);
	      if (typeset & itype_integer)
                [
                  trap_unaligned(r);
                  var = var_in_reg(var, r);
                  exit<function>
                    type_trap(code, type, var, typeset & ~itype_integer);
                ];
              x86:cmpbyte(code, x86:limm, type, x86:lidx, r . x86:object_type);
              if ((typeset & ~itype_null & mc:itypemap_inverse[type])
                  != itype_none)
                // only trap if the test could conceivably fail; the cmp is
                // there to test for null
                trap(x86:bne);
	    ];
	];
    ];

  setcc = fn (code, cc, dest)
    [
      | r, r8 |

      r = reg_dest(dest);
      r8 = x86:reg8[if (r < vlength(x86:reg8)) r else reg_scratch];

      x86:setcc(code, cc, x86:lreg, r8);
      x86:movzxbyte(code, x86:lreg, r8, x86:lreg, r);
      x86:lea(code, x86:lridx, r . 1 . r . 1, x86:lreg, r);
      move(code, x86:lreg, r, x86:lvar, dest);
    ];

  in_scratch? = fn (v)
    mc:in_reg(v) && mc:get_reg(v) == reg_scratch;

  fetch1 = fn (code, var)
    [
      if (mc:in_reg(var)) mc:get_reg(var)
      else
	[
	  // reg_arg0 is reg_scratch so may be in use. reg_arg1 never is.
	  move(code, x86:lvar, var, x86:lreg, reg_arg1);
	  reg_arg1
	]
    ];

  // uses reg_scratch if var not in register
  fetch2 = fn (code, var)
    [
      if (mc:in_reg(var)) mc:get_reg(var)
      else
	[
	  move(code, x86:lvar, var, x86:lreg, reg_scratch);
	  reg_scratch
	]
    ];

  // fetch var into dest's register, or reg_arg1
  fetch_for_dest = fn (code, var, dest)
    if (mc:in_reg(var))
      var
    else if (mc:in_reg(dest))
      [
        move(code, x86:lvar, var, x86:lvar, dest);
        dest
      ]
    else
      var_in_reg(var, fetch1(code, var));

  call_builtin = fn (code, op)
    x86:callrel(code, op);

  callop1 = fn (code, builtin, arg)
    // Scratch register usage: reg_scratch
    [
      move(code, x86:lvar, arg, x86:lreg, reg_arg0);
      call_builtin(code, builtin);
    ];

  callop2 = fn (code, builtin, commutes, arg1, arg2)
    // Scratch register usage: reg_scratch
    [
      if (mc:get_reg(arg2) == reg_arg0)
	if (commutes) // better to switch
	  [
	    | t |

	    t = arg1; arg1 = arg2; arg2 = t;
	  ]
	else
	  [
	    move(code, x86:lvar, arg1, x86:lreg, reg_arg1);
	    x86:xchg(code, x86:lreg, reg_arg0, x86:lreg, reg_arg1);
	    call_builtin(code, builtin);
	    exit<function> 42;
	  ];
      move(code, x86:lvar, arg1, x86:lreg, reg_arg0);
      move(code, x86:lvar, arg2, x86:lreg, reg_arg1);
      call_builtin(code, builtin);
    ];

  [
    | regs, allregs |
    regs = '[,reg_arg0 ,reg_arg1 ,x86:reg_edx];
    allregs = make_vector(x86:nregs);
    for (|i|i = vlength(regs); --i >= 0; )
      allregs[regs[i]] = i;
    protect(allregs);

    callop3 = fn (code, builtin, arg1, arg2, arg3)
      // Scratch register usage: reg_scratch
      [
        | regargs, args, aregs |
        regargs = make_vector(vlength(regs));
        args = vector(arg1, arg2, arg3);
        aregs = vmap(fn (arg) [
          | r |
          r = mc:get_reg(arg);
          if (r >= 0 && integer?(r = allregs[r]))
            r
          else
            null
        ], args);
        for (|i, n| i = vlength(regs); --i >= 0; )
          if ((n = aregs[i]) != null)
            regargs[n] = i;

        // move vars to empty registers
        for (|i| i = vlength(regs); --i >= 0; )
          if (regargs[i] == null)
            loop <again> [
              | ni |
              if (integer?(ni = aregs[i]))
                regargs[ni] = null;
              // will not clobber any register
              move(code, x86:lvar, args[i], x86:lreg, regs[i]);
              aregs[i] = i;
              regargs[i] = i;
              if (integer?(ni) && ni > i)
                exit<again> i = ni;
              exit null;
            ];

        // if necessary, exchange reg_arg0 and edx; reg_arg1 (scratch)
        // must be correct already
        if (aregs[0] != 0)
          x86:xchg(code, x86:lreg, reg_arg0, x86:lreg, x86:reg_edx);

        call_builtin(code, builtin);
      ];
  ];

  move = fn (code, stype, source, dtype, dest)
    safemove(code, stype, source, dtype, dest, regs_scratch);

  safemove = fn (code, stype, source, dtype, dest, scratchregs)
    // Scratch register usage:
    //   none if destination is in a register
    //   if source is in a register:
    //     none if destination is in a register or indexed
    //     1 otherwise (reg_scratch) (ie dest is global)
    //   if neither source or dest is in a register:
    //     1 for source (reg_scratch)
    //     1 for destination if it is not indexed (reg_arg0); ie dest is global
    [
      | sea, dea |

      // type: x86:limm, x86:lcst, x86:lfunction, x86:lreg,
      // x86:lidx, x86:l[rq]idx, x86:lvar
      sea = x86:resolve(stype, source);
      stype = car(sea); source = cdr(sea);
      dea = x86:resolve(dtype, dest);
      dtype = car(dea); dest = cdr(dea);

      // type: x86:limm, x86:lcst, x86:lfunction, x86:lreg,
      // x86:lidx, x86:l[rq]idx, x86:lglobal(_constant)

      if (equal?(sea, dea)) exit<function> null;

      // can use mov if source or destination is a register or if
      // source is a constant

      if (dtype == x86:lreg && stype == x86:limm && source == 0)
        x86:xor(code, dtype, dest, dtype, dest)
      else if (dtype == x86:lreg
               || (stype != x86:lidx && stype != x86:lridx
                   && stype != x86:lqidx && stype != x86:lglobal
                   && stype != x86:lglobal_constant
                   && stype != x86:lspecial))
        x86:mov(code, stype, source, dtype, dest)
      else
        [
          assert_message(dtype != x86:lridx && dtype != x86:lqidx,
                         "scaled destination unsupported (for now)");
          // find a usable scratch register
          | sr |
          for (|i| i = 0; i < vlength(scratchregs); ++i)
            [
              | r |
              r = scratchregs[i];
              if (!(dtype == x86:lidx && car(dest) == r))
                exit<break> sr = r;
            ];
          assert(sr != null);
          x86:mov(code, stype, source, x86:lreg, sr);
          x86:mov(code, x86:lreg, sr, dtype, dest);
        ];
    ];

  compare = fn (code, arg1, arg2, relop)
    [
      | sea1, sea2, stype1, source1, stype2, source2, cmpop, cst1, cst2 |

      if (relop == x86:be || relop == x86:bne) cmpop = cmpeq
      else cmpop = x86:cmp;

      sea1 = x86:resolve(x86:lvar, arg1);
      stype1 = car(sea1); source1 = cdr(sea1);
      sea2 = x86:resolve(x86:lvar, arg2);
      stype2 = car(sea2); source2 = cdr(sea2);

      cst1 = (stype1 == x86:limm || stype1 == x86:lcst
              || stype1 == x86:lglobal_constant);
      cst2 = (stype2 == x86:limm || stype2 == x86:lcst
              || stype2 == x86:lglobal_constant);
      if (cst1 && cst2) // oops, cheat
        cst1 = cst2 = false;

      if (stype2 == x86:lreg || cst1)
	cmpop(code, stype1, source1, stype2, source2)
      else if (stype1 == x86:lreg || cst2)
	[
	  cmpop(code, stype2, source2, stype1, source1);
	  exit<function> commute_x86relop[relop]
	]
      else
	[
	  x86:mov(code, stype2, source2, x86:lreg, x86:reg_ecx);
	  cmpop(code, stype1, source1, x86:lreg, x86:reg_ecx);
	];
      relop
    ];

  cmpeq = fn (fcode, m1, a1, m2, a2)
    [
      if ((m1 == x86:limm && a1 == 0 ||
	   m1 == x86:lcst && a1 == null) &&
	  (m2 == x86:lreg || m2 == x86:lvar && mc:in_reg(a2)))
	x86:test(fcode, m2, a2, m2, a2)
      else
	x86:cmp(fcode, m1, a1, m2, a2)
    ];

  logical_or = fn (code, v1, v2)
    [
      if (in_scratch?(v2))
	[
	  | t |
	  t = v1; v1 = v2; v2 = t;
	];
      move(code, x86:lvar, v1, x86:lreg, reg_scratch);
      // ((v1-1) | v2) - 1 == 0 iff v1 == false && v2 == false
      // (this assumes that 2 is an illegal value)
      x86:dec(code, x86:lreg, reg_scratch);

      if (v1 != v2)
        [
          x86:or(code, x86:lvar, v2, x86:lreg, reg_scratch);
          x86:dec(code, x86:lreg, reg_scratch);
        ];
    ];


  // TODO: in some cases, we can use the EA op immediate insn form
  // (though it may often be bad, because it generates instructions
  // > 7 bytes (e.g. global = global + 5120))
  perform3 = fn (code, arg1, arg2, dest, op, commute_op)
    [
      | stype1, source1, stype2, source2, dtype |

      @(stype1 . source1) = x86:resolve(x86:lvar, arg1);
      @(stype2 . source2) = x86:resolve(x86:lvar, arg2);
      if (integer?(dest))
        dtype = x86:lreg
      else
        @(dtype . dest) = x86:resolve(x86:lvar, dest);

      if (dtype == x86:lreg)
	[
	  if (dtype == stype2 && dest == source2)
            // cannot overwrite dest yet
	    if (commute_op)
	      // no need for move, arg2 is already in dest
	      commute_op(code, stype1, source1, dtype, dest)
	    else
	      [
                // if we know that source1 is dead, we can skip the move
                if (stype1 != x86:lreg || source1 != reg_scratch)
                  [
                    move(code, stype1, source1, x86:lreg, x86:reg_ecx);
                    stype1 = x86:lreg;
                    source1 = x86:reg_ecx;
                  ];
                op(code, stype2, source2, stype1, source1);
		move(code, stype1, source1, dtype, dest);
	      ]
	  else
	    [
	      move(code, stype1, source1, dtype, dest);
	      op(code, stype2, source2, dtype, dest);
	    ]
	]
      else
	[
          // if we know that source1 is dead, we can skip the move
          if (stype1 != x86:lreg || source1 != reg_scratch)
            [
              move(code, stype1, source1, x86:lreg, x86:reg_ecx);
              stype1 = x86:lreg;
              source1 = x86:reg_ecx;
            ];
	  op(code, stype2, source2, stype1, source1);
	  move(code, stype1, source1, dtype, dest);
	];
    ];

  perform3cst = fn (code, arg1, arg2, dest, op, commute_op,
		    cstop, commute_cstop)
    [
      | cst |

      if (integer?(cst = mc:var_value(arg2)))
	[
	  move(code, x86:lvar, arg1, x86:lvar, dest);
	  cstop(code, cst, x86:lvar, dest);
	]
      else if (commute_cstop && integer?(cst = mc:var_value(arg1)))
	[
	  move(code, x86:lvar, arg2, x86:lvar, dest);
	  commute_cstop(code, cst, x86:lvar, dest);
	]
      else
	perform3(code, arg1, arg2, dest, op, commute_op)
    ];
];
