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

library genx64 // Code generation for x86-64 (not particularly good)
requires ax64, compiler, dlist, flow, graph, inference, ins3, misc, mx64,
  sequences, vars

defines
  x64:nscratch, x64:ncaller, x64:nregargs, x64:ncallee,
  x64:select_registers, x64:mgen_preamble, x64:mgen_instruction,
  x64:migrate, x64:gassemble, x64:spillreg, x64:reg_globals,
  x64:uses_scratch?

reads mc:verbose, mc:myself, mc:update_maxseclev
writes mc:nops_called, mc:nops_inlined, mc:framesizes
[

  | reg_argcount, reg_closure_in, relop, reg_result,
    regs_scratch, regs_caller, select_registers,
    builtins, relops, var_in_reg, reg_dest, mgen_return, mgen_branch,
    mgen_compute, mgen_memory, mgen_closure, mgen_vref, mgen_call,
    callop1, callop2, callop3, rev_x64relop, fetch1, move, commute, compare,
    reg_scratch, reg_scratch2,
    reg_arg0, reg_arg1, reg_arg2, reg_arg3, reg_arg4, reg_arg5,
    reg_closure, regs_native_args, reg_fp, reg_sp,
    mgen_trap, regs_allscratch, type_branch, get_type,
    typearg1, typearg2, inline1?, inline2?, mgen_inline1, mgen_inline2,
    type_trap, typeset_trap, call, set_seclev, mzero, mfalse, cfalse,
    call_closure, call_primitive, call_varargs,
    call_builtin,
    perform3, perform3cst, setcc, intcst?, int31?, int32?,
    argstart, enames,
    commute_x64relop, in_scratch?, logical_or, safemove,
    push_args, pop_args, move_native_args,
    cmpeq, kset, kequal?, kseclevel, kmaxseclevel, call_kset, call_seclevel,
    kconcat_strings, call_bconcat, kglobal_lookup, maybe_call_global_lookup,
    needs_closure?, is_leaf?, update_maxseclev?,
    needs_global?, leaaddcst, fetch2, fetch_for_dest,
    fake_prim_type,
    immutable_itypes,
    word_size,
    stack_frame_size |

  word_size = 8;

  // used to indicate any primitive; i.e., stype_function - type_closure
  fake_prim_type = '[];

  mc:nops_inlined = mc:nops_called = 0;
  mc:framesizes = make_vector(10);
  vector_fill!(mc:framesizes, 0);

  // Register usage for mudlle
  //   rax: scratch, argument count, function result
  //   rbx: callee-saved, (opt.) globals
  //   rdi,rsi: args 0-1, scratch
  //   rdx,rcx,r8,r9: caller-saved, native args 2-5
  //   r10: caller-saved, closure-in
  //   r11: scratch
  //   r12-r14: callee-saved
  //   r15: callee-saved, (opt.) closure
  //   rbp: frame pointer
  //   rsp: stack pointer
  // Arguments to mudlle functions are on the stack (for debuggability).

  reg_argcount   = x64:reg_rax; // same as reg_scratch, beware
  reg_closure    = x64:reg_r15;
  reg_closure_in = x64:reg_r10; // when passed as parameter
  reg_result     = x64:reg_rax;
  argstart       = 2 * word_size; // offset of first argument on stack

  // Global regs
  x64:reg_globals = x64:reg_rbx;
  reg_sp          = x64:reg_rsp;
  reg_fp          = x64:reg_rbp;
  reg_scratch     = x64:reg_rax;
  reg_scratch2    = x64:reg_r11;

  // Arguments for builtins
  reg_result = x64:reg_rax;
  reg_arg0 = x64:reg_rdi;
  reg_arg1 = x64:reg_rsi;
  reg_arg2 = x64:reg_rdx;
  reg_arg3 = x64:reg_rcx;
  reg_arg4 = x64:reg_r8;
  reg_arg5 = x64:reg_r9;

  regs_native_args = '[
    ,reg_arg0 ,reg_arg1 ,reg_arg2 ,reg_arg3 ,reg_arg4 ,reg_arg5
  ];

  // General regs
  regs_scratch    = '[,reg_scratch];
  regs_allscratch = '[,reg_scratch ,reg_scratch2];
  regs_caller     = '[,reg_arg2 ,reg_arg3 ,reg_arg4 ,reg_arg5 ,x64:reg_r10];

  x64:spillreg    = '[,reg_closure ,reg_fp ,reg_fp];

  mfalse = mzero = x64:mudlleint(0);
  cfalse = mc:var_make_constant(false);

  int31? = fn (int n) n >= -0x40000000 && n <= 0x3fffffff;
  int32? = fn (int n) n >= -0x80000000 && n <= 0x7fffffff;

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
  x64:nscratch = fn (ifn) 1;    // rax
  x64:ncaller = fn (ifn) 5;     // arg2-arg5, r10
  x64:nregargs = fn (ifn) 0;    // arguments on stack
  x64:ncallee = fn (ifn)        // rbx (unless globals), r12-r14,
                                // r15 (unless closure)
    [
      | ncallee |

      ncallee = 3;

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

  x64:uses_scratch? = fn (ins)
    [
      | class, op |
      class = ins[mc:i_class];

      // These operations must not clobber the scratch registers
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
        match (ins[mc:i_top])
          [
            ,mc:trap_type    => false;
            ,mc:trap_typeset => false;
            _ => true
          ]
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

  x64:select_registers = fn (ifn, ainfo)
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
	  regs_callee = list(x64:reg_r12, x64:reg_r13, x64:reg_r14);
	  if (!ifn[mc:c_fmisc][mc:c_fm_closurebase])
	    regs_callee = reg_closure . regs_callee;
	  if (!ifn[mc:c_fmisc][mc:c_fm_globalsbase])
	    regs_callee = x64:reg_globals . regs_callee;
	  regs_callee = list_to_vector(list_first_n!(ainfo[2], regs_callee));
	]
      else
	regs_callee = '[];
      ifn[mc:c_fmisc][mc:c_fm_regs_callee] = regs_callee;
      select_registers(ifn, mc:reg_callee, regs_callee);

      // Allocate scratch regs
      select_registers(ifn, mc:reg_scratch, regs_scratch);

      // set/adjust offsets of all spilled variables
      cvars = ifn[mc:c_fclosure];
      offset = x64:object_offset + word_size; // skip over function
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
	      offset += word_size;
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
	    localoc[mc:v_lsoffset]
              = -word_size * (localofs + localoc[mc:v_lsoffset]);
	  locals = cdr(locals);
	];

      args = ifn[mc:c_fargs];
      if (ifn[mc:c_fvarargs])
	[
	  // varargs is special: we force it onto the local var stack frame
	  | var, vargloc |
          @([var _ _]) = args;
	  vargloc = var[mc:v_location];
	  ++ainfo[3];
	  if (vargloc)
	    [
	      vargloc[mc:v_lclass] = mc:v_lspill;
	      vargloc[mc:v_lstype] = mc:spill_spill;
	      vargloc[mc:v_lsoffset] = -word_size;
	    ]
	]
      else
	[
	  offset = argstart;
	  while (args != null)
	    [
	      | var, argloc |
              @([var _ _] . args) = args;
	      argloc = var[mc:v_location];
	      if (argloc && argloc[mc:v_lclass] == mc:v_lspill
                  && argloc[mc:v_lstype] == mc:spill_args)
		argloc[mc:v_lsoffset] = offset;
	      offset += word_size;
	    ];
	];

      ainfo
    ];

  x64:migrate = fn (ifn, vars, notspilt, spilt, locals, temps)
    // Effetcs: Receives the grouping of variables used by the
    //   register allocator. May move some variables between the group to
    //   better suit the processor. May only strengthen needs ...
    //   On x64: move any closure or argument in temps to locals
    [
      | detemp |
      detemp = fn (v)
        [
	  | n |
	  n = v[mc:v_number];
	  if (bit_set?(temps, n))
	    [
	      clear_bit!(temps, n);
	      set_bit!(locals, n);
	    ]
	];

      lforeach(fn (@[var _ _]) detemp(var), ifn[mc:c_fargs]);
      lforeach(detemp, ifn[mc:c_fclosure]);
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

  x64:gassemble = fn (code)
    [
      // Generate error calls
      lforeach
	(fn (err)
	 [
           | errno, label, args, loc |
           @[errno loc args label] = err;
           mc:set_loc(loc);
	   x64:label(code, label);
           if (errno == -error_wrong_parameters)
             call_builtin(code, "bearly_error_wrong_parameters")
           else if (args == null)
             [
               assert(errno >= 0);
               call_builtin(code, enames[errno]);
             ]
           else if (errno == error_bad_type)
             [
               | type, var |
               @(type . var) = args;
               move(code, x64:lvar, var, x64:lreg, reg_arg0);
               move(code, x64:limm, type, x64:lreg, reg_arg1);
               call_builtin(code, "btype_error");
             ]
           else
             fail();
	 ],
	 code[2]);
      x64:assemble(code);
    ];

  is_leaf? = fn (ifn)
    !dexists?(fn (il) [
      | ins, class |
      ins = il[mc:il_ins];
      class = ins[mc:i_class];
      if (class == mc:i_call)
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

      if (class == mc:i_compute
          && ins[mc:i_aop] == mc:b_symbol_ref)
        exit<function> true;

      false
    ], ifn[mc:c_fvalue]);

  update_maxseclev? = fn (ifn)
    (mc:update_maxseclev == true && !is_leaf?(ifn));

  set_seclev = fn (code, dreg, type)
    x64:mov(code, x64:lseclev, type, x64:lreg, dreg);

  pop_args = fn (code, n)
    [
      if ((n + stack_frame_size) & 1)
        ++n;
      if (n > 0)
        x64:add(code, x64:limm, word_size * n, x64:lreg, reg_sp);
    ];

  | saved_callee_regs |
  saved_callee_regs = fn (fmisc)
    [
      | regs |
      regs = vector_to_list(fmisc[mc:c_fm_regs_callee]);
      if (fmisc[mc:c_fm_closurebase])
        regs = reg_closure . regs;
      if (fmisc[mc:c_fm_globalsbase])
        regs = x64:reg_globals . regs;
      regs
    ];

  x64:mgen_preamble = fn (ifn, ainfo)
    [
      | code, ilist, argcheck, locals, cvars, offset, fmisc |

      if (mc:verbose < 1)
        x64:reset_ins_count();

      code = x64:new_code();
      ilist = ifn[mc:c_fvalue];

      mc:set_loc(ifn[mc:c_loc]);

      // Allocate & clear stack frame
      x64:push(code, x64:lreg, reg_fp);
      stack_frame_size = 0;
      move(code, x64:lreg, reg_sp, x64:lreg, reg_fp);

      | argc |
      argc = ainfo[3];
      if (argc > 0)
	x64:sub(code, x64:limm, argc * word_size, x64:lreg, reg_sp);
      stack_frame_size += argc;

      // Make argument vector for varargs functions
      if (ifn[mc:c_fvarargs])
	[
          set_seclev(code, reg_arg1, x64:sl_c);
	  // Must call even if arg is unused to check seclevel, etc
	  call_builtin(code, "bvarargs");
          offset = -word_size;
	  move(code, x64:lreg, reg_result, x64:lidx, reg_fp . offset);
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
              cmpeq(code, x64:limm, argcount, x64:lreg, reg_argcount);

	      // Remove trap ins
	      ifn[mc:c_fvalue] = ilist = dremove!(ilist, ilist);
	    ]
	  else
	    fail(); // only vararg fns should have no check

          ++mc:framesizes[if (argc < 10) argc else 9];

	  if (is_leaf?(ifn) && argc < 3)
	    [
	      | i |

	      // skip the infinite loop and minlevel security checks for leaf
	      // functions (note that this is a minor security hole)
	      x64:trap(code, x64:bne, -error_wrong_parameters, null);
	      i = argc;
	      while (i > 0)
		[
		  // initialise stack frame to legal mudlle values
		  x64:mov(code, x64:lreg, reg_closure_in,
                          x64:lidx, reg_fp . (-WORD_SIZE * i));
		  --i;
		];
	    ]
	  else
	    [
              set_seclev(code, reg_arg1, x64:sl_c);
              | f |
              f = if (argc >= 4)
                "bcleargc"
              else
                '["bcleargc0" "bcleargc1" "bcleargc2" "bcleargc3"][argc];
              call_builtin(code, f);
	    ];

	  offset = argstart;
	];

      if (update_maxseclev?(ifn))
        [
          set_seclev(code, reg_arg1, x64:sl_maxlev);
          x64:mov(code, x64:lspecial, "maxseclevel", x64:lreg, reg_scratch);
          x64:push(code, x64:lidx, reg_scratch . 0);
          ++stack_frame_size;
          x64:cmp(code, x64:lreg, reg_arg1, x64:lidx, reg_scratch . 0);
          | l |
          l = x64:new_label(code);
          x64:jcc(code, x64:bge, l);
          x64:mov(code, x64:lreg, reg_arg1, x64:lidx, reg_scratch . 0);
          x64:label(code, l);
        ];

      // Save callee-saved registers
      fmisc = ifn[mc:c_fmisc];
      lforeach(fn (r) [
        x64:push(code, x64:lreg, r);
        ++stack_frame_size;
      ], saved_callee_regs(fmisc));
      if (fmisc[mc:c_fm_closurebase])
        move(code, x64:lreg, reg_closure_in, x64:lreg, reg_closure);
      if (fmisc[mc:c_fm_globalsbase])
	[
	  x64:mov(code, x64:lspecial, "env_values", x64:lreg, x64:reg_globals);
          x64:mov(code, x64:lidx, x64:reg_globals . 0,
                  x64:lreg, x64:reg_globals);
	];

      // setup variables:

      // setup arguments, add indirection
      for (| args, argn | [ argn = 0; args = ifn[mc:c_fargs] ];
           args != null;
           ++argn)
	[
	  | arg, loc, locarg |

	  @([arg _ _] . args) = args;

	  loc = x64:lidx;
	  locarg = reg_fp . offset;

	  offset += word_size;

	  if (arg[mc:v_location]) // ignore unused arguments
	    [
	      // if variable is indirect create indirection record
	      if (arg[mc:v_indirect])
		[
		  call_builtin(code, "balloc_variable");
		  move(code, loc, locarg, x64:lidx,
                       reg_arg1 . x64:object_offset);
		  loc = x64:lreg;
		  locarg = reg_arg1;
		];

	      // & copy to correct location
	      move(code, loc, locarg, x64:lvar, arg);
	      assert_message(!in_scratch?(arg),
                             "oops - argument unspilt to scratch");
	    ];
	];

      // unspill closure vars that need it
      cvars = ifn[mc:c_fclosure];
      offset = x64:object_offset + word_size;
      while (cvars != null)
	[
	  | cvar, cvarloc |

	  cvar = car(cvars);

	  if (cvar[mc:v_cparent] != mc:myself)
	    [
	      cvarloc = cvar[mc:v_location];
	      if (cvarloc[mc:v_lclass] == mc:v_lregister)
                // unspill closure entry
		move(code, x64:lidx, reg_closure . offset,
		     x64:lreg, cvarloc[mc:v_lrnumber]);
	      assert_message(!in_scratch?(cvar),
                             "oops - closure var unspilt to scratch");
	      offset += word_size;
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

	  if (local[mc:v_indirect] && !lexists?(fn (@[var _ _]) var == local,
						ifn[mc:c_fargs]))
	    [
	      call_builtin(code, "balloc_variable");
	      move(code, x64:limm, 0, x64:lidx, reg_arg1 . x64:object_offset);
	      move(code, x64:lreg, reg_arg1, x64:lvar, local);
	      assert_message(!in_scratch?(local),
                             "oops - local var unspilt to scratch");
	    ];
	];

      code
    ];


  x64:mgen_instruction = fn (code, ifn, ainfo, il)
    [
      | ins, class |

      if (il[mc:il_label])
	x64:label(code, il[mc:il_label][mc:l_mclabel]);

      ins = il[mc:il_ins];
      class = ins[mc:i_class];

      mc:set_loc(il[mc:il_loc]);

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
	x64:trap(code, x64:balways, nerror, null)
      else if (trap == mc:trap_loop)
	[
          x64:mov(code, x64:lspecial, "xcount", x64:lreg, reg_scratch);
          x64:dec(code, x64:lidx, reg_scratch . 0);
	  x64:trap(code, x64:be, nerror, null);
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
                  x64:mov(code, x64:lvar, arg2, x64:lreg, reg_arg0);
                  greg = reg_arg1;
                ];
              // ignores nerror value
              x64:mov(code, x64:lglobal_index, arg1[mc:v_name] . x64:gl_c,
                      x64:lreg, greg);
              call_builtin(code,
                           if (trap == mc:trap_global_write) "bwglobal"
                           else "brglobal");
            ];
	]
      else if (trap == mc:trap_type)
	// ignores nerror value
	type_trap(code, arg2[mc:v_kvalue], arg1, type1)
      else if (trap == mc:trap_typeset)
	// ignores nerror value
	typeset_trap(code, arg2[mc:v_kvalue], arg1, type1, true)
      else fail();
      // argcheck must be done in preamble as argcount doesn't survive
    ];


  mgen_return = fn (code, ifn, ainfo, ins)
    [
      move(code, x64:lvar, ins[mc:i_rvalue], x64:lreg, reg_result);
      | fmisc |
      fmisc = ifn[mc:c_fmisc];
      lforeach(fn (r) x64:pop(code, x64:lreg, r),
               lreverse!(saved_callee_regs(fmisc)));
      if (update_maxseclev?(ifn))
        [
          x64:mov(code, x64:lspecial, "maxseclevel", x64:lreg, reg_scratch2);
          x64:pop(code, x64:lidx, reg_scratch2 . 0);
        ];
      x64:leave(code);
      x64:ret(code);
    ];

  // map relops to x64 relops
  relops = '[0 0 0 0 0 0 0 0 0 0 0 0
             ,x64:be ,x64:bne ,x64:bl ,x64:bge ,x64:ble ,x64:bg];
  assert(vlength(relops) == mc:branch_gt + 1);
  rev_x64relop = fn (op) op ^ 1; // reverse meaning of x64 branch
  // change op assuming cmp operands have been commuted
  commute_x64relop = '[-1 -1              // meaningless
                       ,x64:ba ,x64:bbe
                       ,x64:be ,x64:bne   // no change
                       ,x64:bae ,x64:bb   // swap
                       -1 -1              // meaningless
                       -1 -1              // meaningless
                       ,x64:bg ,x64:ble   // swap
                       ,x64:bge ,x64:bl]; // swap


  // some strings have their immutable bit removed; don't add itype_string
  immutable_itypes = itype_null | itype_integer;

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
	x64:jmp(code, dest)
      else if (op == mc:branch_never)
        null
      else if (op == mc:branch_true)
	[
          | cop |
          cop = compare(code, cfalse, arg1, x64:bne);
	  x64:jcc(code, cop, dest);
	]
      else if (op == mc:branch_false)
	[
          | cop |
          cop = compare(code, cfalse, arg1, x64:be);
	  x64:jcc(code, cop, dest);
	]
      else if (op == mc:branch_or)
	[
	  logical_or(code, arg1, arg2);
	  x64:jcc(code, x64:bne, dest);
	]
      else if (op == mc:branch_nor)
	[
	  logical_or(code, arg1, arg2);
	  x64:jcc(code, x64:be, dest);
	]
      else if (op == mc:branch_and)
	[
	  | l, cop |

	  l = x64:new_label(code);
          cop = compare(code, cfalse, arg1, x64:be);
	  x64:jcc(code, cop, l);
          cop = compare(code, cfalse, arg2, x64:bne);
	  x64:jcc(code, cop, dest);
	  x64:label(code, l);
	]
      else if (op == mc:branch_nand)
	[
          | cop |
          cop = compare(code, cfalse, arg1, x64:be);
	  x64:jcc(code, cop, dest);
          cop = compare(code, cfalse, arg2, x64:be);
	  x64:jcc(code, cop, dest);
	]
      else if (op == mc:branch_bitand || op == mc:branch_nbitand)
        [
          | cst |
          type_trap(code, type_integer, arg1, type1);
          type_trap(code, type_integer, arg2, type2);
          if (integer?(cst = mc:var_value(arg1)))
            null
          else if (integer?(cst = mc:var_value(arg2)) || in_scratch?(arg2))
            [
              | t |
              t = arg1; arg1 = arg2; arg2 = t;
            ];
          if (integer?(cst))
            [
              if (int31?(cst))
                x64:test(code, x64:limm, x64:doubleint(cst), x64:lvar, arg2)
              else
                [
                  | cr |
                  cr = if (in_scratch?(arg2)) reg_scratch2 else reg_scratch;
                  x64:mov(code, x64:limm64, x64:doubleint(cst), x64:lreg, cr);
                  x64:test(code, x64:lreg, cr, x64:lvar, arg2);
                ]
            ]
          else
            [
              if (!in_scratch?(arg1) && mc:in_reg(arg1))
                x64:lea(code, x64:lidx, mc:get_reg(arg1) . -1,
                        x64:lreg, reg_scratch)
              else
                [
                  move(code, x64:lvar, arg1, x64:lreg, reg_scratch);
                  x64:xor(code, x64:limm, 1, x64:lreg, reg_scratch);
                ];
              x64:test(code, x64:lreg, reg_scratch, x64:lvar, arg2);
            ];
          x64:jcc(code, if (op == mc:branch_bitand) x64:bne else x64:be, dest)
        ]
      else if (op == mc:branch_bitset || op == mc:branch_bitclear)
        [
	  type_trap(code, type_string, arg1, type1);
	  type_trap(code, type_integer, arg2, type2);
          callop2(code, "bbitref", false, arg1, arg2);
          // sets carry flag
          x64:jcc(code, if (op == mc:branch_bitset) x64:bb else x64:bae, dest)
        ]
      else if (op == mc:branch_eq || op == mc:branch_ne)
	[
          | cop |
	  cop = compare(code, arg1, arg2, relops[op]);
	  x64:jcc(code, cop, dest);
	]
      else if (op == mc:branch_equal || op == mc:branch_nequal)
        [
          call_primitive(code, kequal?, equal?, null, 2, args);
          x64:cmp(code, x64:limm, mfalse, x64:lreg, reg_result);
          x64:jcc(code, if (op == mc:branch_equal) x64:bne else x64:be, dest);
        ]
      else if (op >= mc:branch_lt && op <= mc:branch_gt) // relop
	[
	  | x64op |

	  type_trap(code, type_integer, arg1, type1);
	  type_trap(code, type_integer, arg2, type2);

	  x64op = compare(code, arg2, arg1, relops[op]);
	  x64:jcc(code, x64op, dest);
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
              n *= word_size;
              type_trap(code, type_vector, arg1, type1);
            ]
          else
            [
              op -= mc:branch_slength;
              ++n;              // trailing zero
              type_trap(code, type_string, arg1, type1);
            ];
          op = relops[op + mc:branch_eq];
          n += x64:object_offset;

          x64:cmp(code, x64:limm, n, x64:lidx, areg . x64:object_size);
          x64:jcc(code, op, dest);
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

          if (!(type1 & ~immutable_itypes))
            [
              if (!inv?) x64:jmp(code, dest);
            ]
          else
            [
              | xr, fdest, usef |

              fdest = x64:new_label(code);
              usef = false;

              xr = fetch1(code, arg1);

              if (type1 & itype_integer)
                [
                  x64:test(code, x64:limm, 1, x64:lreg, xr);
                  x64:jcc(code, x64:bne, if (inv?) fdest else dest);
                  if (inv?) usef = true;
                ];
              if (type1 & itype_null)
                [
                  x64:test(code, x64:lreg, xr, x64:lreg, xr);
                  x64:jcc(code, x64:be, if (inv?) fdest else dest);
                  if (inv?) usef = true;
                ];
              x64:test(code, x64:limm, flag, x64:lidx, xr . x64:object_flags);
              x64:jcc(code, if (inv?) x64:be else x64:bne, dest);

              if (usef) x64:label(code, fdest);
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
      "bleq" "blne" "bllt" "blge" "blle" "blgt"
      "bbitor" "bbitxor" "bbitand" "bshift_left" "bshift_right"
      "badd" "bsubtract" "bmultiply" "bdivide" "bremainder"
      "bnegate" "bnot" "bbitnot" 0 0 0 0 "bref" 0 "bcons" 0
      "bcar" "bcdr" 0 0 0 "btypeof" 0 0 0 0 0 0 "bpcons" "bsymbol_ref"];
  assert(vlength(builtins) == mc:builtins);

  commute =
    '[0 0
      ,false ,false ,false ,false ,false ,false
      ,true ,true ,true ,false ,false
      ,false ,false ,true ,false ,false
      0 0 0 0 0 0 0 ,false 0 ,false 0
      0 0 0 0 0 0 0 0 0 0 0 0 0 0];
  assert(vlength(commute) == mc:builtins);

  // Type of compute op arguments (uses type_xxx/stype_xxx sets)
  typearg1 =
    '[0             // sc_or
      0             // sc_and
      ,stype_any    // eq
      ,stype_any    // ne
      ,type_integer // lt
      ,type_integer // ge
      ,type_integer // le
      ,type_integer // gt
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
      0             // symbol_ref
    ];
  assert(vlength(typearg1) == mc:builtins);

  typearg2 =
    '[0             // sc_or
      0             // sc_and
      ,stype_any    // eq
      ,stype_any    // ne
      ,type_integer // lt
      ,type_integer // ge
      ,type_integer // le
      ,type_integer // gt
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
      0             // symbol_ref
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
	move(code, x64:lvar, r, x64:lvar, d);
	x64:neg(code, x64:lvar, d);
	x64:add(code, x64:limm, 2, x64:lvar, d);
      ]
    else if (op == mc:b_bitnot)
      [
	move(code, x64:lvar, r, x64:lvar, d);
	x64:not(code, x64:lvar, d);
	x64:or(code, x64:limm, 1, x64:lvar, d);
      ]
    else if (op == mc:b_not)
      [
	x64:cmp(code, x64:limm, mfalse, x64:lvar, r);
	setcc(code, x64:be, d);
      ]
    else if (op == mc:b_car || op == mc:b_symbol_name)
      [
	| xr |

	xr = fetch1(code, r);
	move(code, x64:lidx, xr . x64:object_offset, x64:lvar, d)
      ]
    else if (op == mc:b_cdr || op == mc:b_symbol_get)
      [
	| xr |

	xr = fetch1(code, r);
	move(code, x64:lidx, xr . x64:object_offset + word_size, x64:lvar, d)
      ]
    else if (op == mc:b_slength)
      [
	| xr, dr |

	xr = fetch1(code, r);
	dr = reg_dest(d);

	move(code, x64:lidx, xr . x64:object_size, x64:lreg, dr);
	x64:lea(code, x64:lridx, dr . 1 . dr . -(2 * x64:object_offset + 1),
                x64:lreg, dr);
	move(code, x64:lreg, dr, x64:lvar, d);
      ]
    else if (op == mc:b_vlength)
      [
	| xr, dr |

	xr = fetch1(code, r);
	dr = reg_dest(d);

        // (vecsize / 4) - (objsize / 4) - 1 = vecelems * 2 + 1
	move(code, x64:lidx, xr . x64:object_size, x64:lreg, dr);
	x64:shr(code, x64:limm, 2, x64:lreg, dr);
	x64:sub(code, x64:limm, (x64:object_offset >> 2) - 1, x64:lreg, dr);
	move(code, x64:lreg, dr, x64:lvar, d);
      ]
    else if (op == mc:b_typeof)
      [
        | dr, rr |

        // we know it's a pointer type; cf. inline1?()
        dr = reg_dest(d);
        rr = fetch1(code, r);
        x64:movzxbyte(code, x64:lidx, rr . x64:object_type, x64:lreg, dr);
        x64:lea32(code, x64:lridx, dr . 1 . dr . 1, x64:lreg, dr);
        move(code, x64:lreg, dr, x64:lvar, d);
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
      !(op == mc:b_divide || op == mc:b_remainder || op == mc:b_cons
        || op == mc:b_pcons || op == mc:b_symbol_ref);

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

      if (cst <= -(1 << 29) || cst >= (1 << 29))
	exit<function> false;
      if (subtract) cst = -cst;
      x64:lea(code, x64:lidx, r1 . (cst << 1), x64:lreg, d);
      true
    ];

  // return list of factors of n:
  //   9, 8, 7, 5, 4, 3, 2, or -n for other powers of 2: (1 << n)
  // or null if we should use imul
  | multiply_factorize |
  multiply_factorize = fn (n, neg?)
    [
      assert(n > 0);

      | f, n2, n3, n5, n7, d |
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

      n7 = 0;                  // number of powers of 7
      if ((d = n / 7) * 7 == n)
        [
          ++n7;
          n = d;
        ];

      if (n != 1)
        // other prime factors than 2, 3, 5, and 7
        exit<function> null;
      if (n5 + (n3 + 1) / 2 + (n2 > 0) + n7 > (if (neg?) 2 else 3))
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
      if (n2 >= 2 && n2 <= 3)
        [
          f = (1 << n2) . f;
          n2 = 0;
        ];
      if (n2 > 0)
        [
          // try to shift (shl) or multiply by 2 (add) in the middle;
          // may save moves or explicit subs
          | e |
          e = if (n2 > 3) -n2 else (1 << n2);
          if (f != null && cdr(f) != null)
            set_cdr!(f, e . cdr(f))
          else
            f = e . f;
        ];
      // multiply by 7 first, as it may save a move
      while (--n7 >= 0)
        f = 7 . f;
      f
    ];

  | mgen_multiply |
  mgen_multiply = fn (code, r1, type1, r2, type2, d)
    [
      // multiply by constant (32-bit or a power of two)

      | c, neg? |
      c = mc:var_value(r2);

      if (c >= -1 && c <= 1)
        [
          type_trap(code, type_integer, r1, type1);
          if (c == 0)
            move(code, x64:limm, mzero, x64:lvar, d)
          else if (c == 1)
            move(code, x64:lvar, r1, x64:lvar, d)
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
              assert(int32?(c));

              type_trap(code, type_integer, r1, type1);
              if (neg?)
                c = -c;
              x64:imul(code, x64:lvar, r1, x64:lreg, dr, x64:limm, c);
              --c;
              if (int32?(c))
                x64:sub(code, x64:limm, c, x64:lreg, dr)
              else
                [
                  x64:mov(code, x64:limm64, c, x64:lreg, reg_scratch2);
                  x64:sub(code, x64:lreg, reg_scratch2, x64:lreg, dr);
                ];
              exit<function> move(code, x64:lreg, dr, x64:lvar, d);
            ];
        ];

      | r |
      r = fetch1(code, r1);
      type_trap(code, type_integer, var_in_reg(r1, r), type1);

      | excess |
      excess = 1;
      while (factors != null)
        [
          | f |
          @(f . factors) = factors;

          if (f < 0)
            [
              f = -f;
              | nexcess |
              nexcess = excess << f;
              // make a subtraction here if it's worth it
              if (!int32?(c) && int32?(c >> f))
                [
                  assert(excess > 0 && excess < 0x7fffffff);
                  if (r != dr)
                    x64:lea(code, x64:lidx, r . -excess, x64:lreg, dr)
                  else
                    x64:sub(code, x64:limm, excess, x64:lreg, dr);
                  nexcess = 0;
                ]
              else
                move(code, x64:lreg, r, x64:lreg, dr);
              excess = nexcess;
              x64:shl(code, x64:limm, f, x64:lreg, dr);
            ]
          else
            [
              | mod |
              excess *= f;
              mod = 0;
              if (factors == null)
                [
                  mod = (if (neg?) -1 else 1) - excess;
                  if (!int32?(mod))
                    mod = 0;
                  excess += mod;
                ];

              if (f == 2 && r == dr && mod == 0)
                x64:add(code, x64:lreg, r, x64:lreg, dr)
              else if (f == 2 || f == 3 || f == 5 || f == 9)
                x64:lea(code, x64:lridx, r . (f - 1) . r . mod, x64:lreg, dr)
              else if (f == 4 || f == 8)
                x64:lea(code, x64:lqidx, r . f . mod, x64:lreg, dr)
              else if (f == 7)
                [
                  | tr |
                  tr = r;
                  if (tr == dr)
                    [
                      assert(dr != reg_scratch2);
                      // shouldn't happen!
                      x64:mov(code, x64:lreg, tr, x64:lreg, reg_scratch2);
                      tr = reg_scratch2;
                    ];
                  x64:lea(code, x64:lqidx, r . 8 . mod, x64:lreg, dr);
                  x64:sub(code, x64:lreg, tr, x64:lreg, dr);
                ]
              else
                fail();
            ];
          r = dr;
        ];

      assert(r == dr);
      if (neg?)
        [
          x64:neg(code, x64:lreg, dr);
          excess = -excess;
        ];
      --excess;               // need one left
      if (excess == 0)
        null
      else if (excess == -1)
        x64:orbyte(code, x64:limm, 1, x64:lreg, dr)
      else
        [
          | op |
          op = if (excess < 0)
            [
              excess = -excess;
              x64:add
            ]
          else
            x64:sub;

          if (int32?(excess))
            op(code, x64:limm, excess, x64:lreg, dr)
          else
            [
              x64:mov(code, x64:limm64, excess, x64:lreg, reg_scratch2);
              op(code, x64:lreg, reg_scratch2, x64:lreg, dr);
            ];
        ];
      move(code, x64:lreg, dr, x64:lvar, d);
    ];

  | mgen_iadd |
  mgen_iadd = fn (code, r1, r2, d)
    if (leaaddcst(code, r1, r2, d, false))
      null
    else if (mc:in_reg(r1) && mc:in_reg(r2))
      [
        | dr |

        dr = reg_dest(d);
        x64:lea(code, x64:lridx,
                mc:get_reg(r1) . 1 . mc:get_reg(r2) . -1,
                x64:lreg, dr);
        move(code, x64:lreg, dr, x64:lvar, d);
      ]
    else
      [
        | nadd, cstadd |

        nadd = fn (code, m1, a1, m2, a2)
          if (m1 == x64:lreg && m2 == x64:lreg)
            x64:lea(code, x64:lridx, a1 . 1 . a2 . -1, m2, a2)
          else
            [
              x64:add(code, m1, a1, m2, a2);
              x64:dec(code, m2, a2);
            ];

        cstadd = fn (code, i, m1, a1)
          [
            assert(int31?(i));
            x64:add(code, x64:limm, x64:doubleint(i), m1, a1);
          ];

        perform3cst(code, r1, r2, d, nadd, nadd, cstadd, cstadd);
      ];

  | shift_mask |
  shift_mask = ((word_size * 8) - 1); // bits used in shift left/right

  mgen_inline2 = fn (code, op, r1, type1, r2, type2, d, ins)
    // Effects: Generates code for d = r1 op r2
    //   type1 & type2 are provided to help generate code for polymorphic ops
    //   r1, r2, d are variables
    if (op == mc:b_eq) relop(code, x64:be, r1, r2, d)
    else if (op == mc:b_ne) relop(code, x64:bne, r1, r2, d)
    else if (op == mc:b_lt) relop(code, x64:bl, r1, r2, d)
    else if (op == mc:b_le) relop(code, x64:ble, r1, r2, d)
    else if (op == mc:b_gt) relop(code, x64:bg, r1, r2, d)
    else if (op == mc:b_ge) relop(code, x64:bge, r1, r2, d)
    else if (op == mc:b_bitor)
      perform3(code, r1, r2, d, x64:or, x64:or)
    else if (op == mc:b_bitand)
      perform3(code, r1, r2, d, x64:and, x64:and)
    else if (op == mc:b_bitxor)
      [
	| cstxor, normalxor |

	cstxor = fn (code, i, m1, a1)
          [
            assert(int31?(i));
            x64:xor(code, x64:limm, x64:doubleint(i), m1, a1);
          ];

	normalxor = fn (code, m1, a1, m2, a2)
	  [
	    x64:xor(code, m1, a1, m2, a2);
	    x64:or(code, x64:limm, 1, m2, a2);
	  ];

	perform3cst(code, r1, r2, d, normalxor, normalxor,
		    cstxor, cstxor);
      ]
    else if (op == mc:b_shift_left)
      mgen_multiply(code, r1, type1,
                    mc:var_make_constant(1 << (r2[mc:v_kvalue] & shift_mask)),
                    itype_integer, d)
    else if (op == mc:b_shift_right)
      [
	| i |

	i = r2[mc:v_kvalue] & shift_mask;
	move(code, x64:lvar, r1, x64:lvar, d);
        if (i != 0)
          [
            x64:sar(code, x64:limm, i, x64:lvar, d);
            x64:or(code, x64:limm, 1, x64:lvar, d);
          ]
      ]
    else if (op == mc:b_multiply) // integer multiply with constant
      mgen_multiply(code, r1, type1, r2, type2, d)
    else if (op == mc:b_add || op == mc:b_iadd) // integer addition only
      [
        if (op == mc:b_add)
          [
            type_trap(code, type_integer, r1, type1);
            type_trap(code, type_integer, r2, type2);
          ];

        mgen_iadd(code, r1, r2, d)
      ]
    else if (op == mc:b_subtract)
      [
	if (!leaaddcst(code, r1, r2, d, true))
	  perform3cst(code, r1, r2, d,
		      fn (code, m1, a1, m2, a2)
		      [
			x64:sub(code, m1, a1, m2, a2);
                        x64:or(code, x64:limm, 1, m2, a2);
		      ],
		      false,
		      fn (code, i, m1, a1)
                      [
                        assert(int31?(i));
                        x64:sub(code, x64:limm, x64:doubleint(i), m1, a1)
                      ],
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
		c = word_size * c + x64:object_offset;
                if (unsafe?)
                  [
                    assert(int31?(c));
                    x64:cmp(code, x64:limm, c,
                            x64:lidx, reg1 . x64:object_size);
                    x64:trap(code, x64:bbe, error_bad_index, null);
                  ];
		move(code, x64:lidx, reg1 . c, x64:lvar, d);
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
		c += x64:object_offset;
                if (unsafe?)
                  [
                    assert(int31?(c + 1));
                    x64:cmp(code, x64:limm, c + 1, x64:lidx,
                            reg1 . x64:object_size);
                    x64:trap(code, x64:bbe, error_bad_index, null);
                  ];
		x64:movzxbyte(code, x64:lidx, reg1 . c, x64:lreg, dr);
	      ]
	    else
              fail();

	    x64:lea32(code, x64:lridx, dr . 1 . dr . 1, x64:lreg, dr);
	    move(code, x64:lreg, dr, x64:lvar, d);
	  ]
	else fail()
      ]
    else fail();

  | args_immutable? |
  args_immutable? = fn (list args, {list,int} types)
    loop
      [
        if (args == null) exit true;

        | t, arg |
        @(arg . args) = args;
        if (types)
          @(t . types) = types
        else
          t = get_type(arg);

        if ((t & ~immutable_itypes) != 0
            && arg[mc:v_class] != mc:v_constant)
          exit false;
      ];

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
	move(code, x64:lvar, arg1, x64:lvar, dest)
      else if (op == mc:b_loop_count)
        [
          | dr |
          dr = reg_dest(dest);
          x64:mov(code, x64:lspecial, "xcount", x64:lreg, dr);
          x64:mov(code, x64:lidx, dr . 0, x64:lreg, dr);
          x64:lea(code, x64:lridx, dr . 1 . dr . 1, x64:lreg, dr);
          move(code, x64:lreg, dr, x64:lvar, dest);
        ]
      else if (op == mc:b_max_loop_count)
        [
          | dr |
          dr = reg_dest(dest);
          x64:mov(code, x64:lspecial, "max_loop_count", x64:lreg, dr);
          move(code, x64:lreg, dr, x64:lvar, dest);
        ]
      else if (op == mc:b_vector || op == mc:b_sequence)
	[
          | nargs |
          nargs = llength(args);
          move(code, x64:limm, word_size * nargs + x64:object_offset,
               x64:lreg, reg_arg0);
	  call_builtin(code, "balloc_vector");

          lreduce(fn (arg, ofs) [
            assert(!in_scratch?(arg));
            move(code, x64:lvar, arg, x64:lidx,
                 reg_arg1 . x64:object_offset + ofs);
            ofs + word_size
          ], 0, args);

          if (op == mc:b_sequence)
            [
              | flags |
              flags = if (args_immutable?(args, types))
                MUDLLE_READONLY | MUDLLE_IMMUTABLE
              else
                MUDLLE_READONLY;
              x64:orbyte(code, x64:limm, flags, x64:lidx,
                         reg_arg1 . x64:object_flags);
            ];
	  move(code, x64:lreg, reg_arg1, x64:lvar, dest);
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
	    move(code, x64:lreg, reg_result, x64:lvar, dest);
	  ]
      else // 2-argument ops
	if (inline2?(op, arg1, type1, arg2, type2))
	  [
	    | t |

	    if (arg1[mc:v_class] == mc:v_constant && commute[op])
	      [
		// swap arg1 and arg2 (x64 prefers constants as arg2)
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
            if (op == mc:b_cons || op == mc:b_pcons)
              [
                // cons takes arguments in reverse order; optimize
                // for cons(x, cons(...))
                | t |
                t = arg1;
                arg1 = arg2;
                arg2 = t;
              ];

            // empty string addition commutes as well
            c = (commute[op]
                 || (op == mc:b_add
                     && (equal?(mc:var_value(arg1), "")
                         || equal?(mc:var_value(arg2), ""))));
	    callop2(code, builtins[op], c, arg1, arg2);
	    move(code, x64:lreg, reg_result, x64:lvar, dest);
	  ]
    ];

  mgen_memory = fn (code, ins)
    [
      | array, areg, scalar, offset |

      array = ins[mc:i_marray];
      areg = mc:get_reg(array);
      scalar = ins[mc:i_mscalar];
      offset = x64:object_offset + word_size * ins[mc:i_mindex];

      areg = fetch1(code, array);

      if (ins[mc:i_mop] == mc:memory_read)
	move(code, x64:lidx, areg . offset, x64:lvar, scalar)
      else
        [
          if (ins[mc:i_mop] == mc:memory_write_safe)
            [
              x64:test(code, x64:limm, MUDLLE_READONLY,
                       x64:lidx, areg . x64:object_flags);
              x64:trap(code, x64:bne, error_value_read_only, null);
            ];
          // write - note: reg_scratch may be in use, allow reg_scratch2 too
          safemove(code, x64:lvar, scalar,
                   x64:lidx, areg . offset, regs_allscratch);
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
          move(code, x64:lclosure, f, x64:lvar, cdest);
          exit<function> null;
        ];

      move(code, x64:limm,
           word_size * (1 + llength(cvars)) + x64:object_offset,
	   x64:lreg, reg_arg0);
      call_builtin(code, "balloc_closure");
      move(code, x64:lfunction, f, x64:lidx, reg_arg1 . x64:object_offset);
      offset = x64:object_offset + word_size;
      while (cvars != null)
	[
	  | cvar |

	  cvar = car(cvars)[mc:v_cparent];
	  if (cvar == cdest) // place ourselves in closure
	    move(code, x64:lreg, reg_arg1, x64:lidx, reg_arg1 . offset)
	  else
	    move(code, x64:lvar, cvar, x64:lidx, reg_arg1 . offset);
	  offset += word_size;
	  cvars = cdr(cvars);
	];
      move(code, x64:lreg, reg_arg1, x64:lvar, cdest);
    ];

  mgen_vref = fn (code, ins)
    [
      | arg, dest |
      dest = ins[mc:i_vdest];
      arg = ins[mc:i_varg];
      move(code, x64:lvar, arg, x64:lvar, dest);
    ];

  // Push arguments on the stack, in reverse order
  push_args = fn (code, int nargs, args)
    [
      if ((nargs + stack_frame_size) & 1)
        x64:push(code, x64:limm, 0);
      lforeach(fn (v) x64:push(code, x64:lvar, v), lreverse(args));
    ];

  move_native_args = fn (code, int nargs, args)
    [
      // move registers around to their right places
      | idx, simple, regv |

      idx = 0;
      lforeach(fn (arg) [
        | reg, type, loc, didx |
        @(type . loc) = x64:resolve(x64:lvar, arg);
        didx = idx++;
        reg = match (type)
          [
            ,x64:lreg => [
              if (loc == regs_native_args[didx])
                // already in its correct place
                exit<function> null;
              loc
            ];
            ,x64:lqidx => car(loc);
            ,x64:lridx => fail();
            _ => null
          ];

        | regidx, this |
        regidx = vector_index(reg, regs_native_args);
        this = vector(didx, reg, type, loc);
        if (regidx >= 0 && regidx < nargs)
          [
            assert(type == x64:lreg);
            if (regv == null)
              regv = make_vector(nargs);
            | old |
            old = regv[regidx];
            if (old != null)
              [
                // the same value is being copied to several args
                this[3] = regs_native_args[old[0]];
                simple = this . simple
              ]
            else
              regv[regidx] = this;
          ]
        else
          simple = this . simple
      ], args);

      if (regv != null)
        loop <again> [
          | xchg, again? |
          again? = false;
          for (|i| i = 0; i < nargs; ++i)
            [
              | this, didx, type, loc |
              this = regv[i];
              if (this == null) exit<continue> null;
              @[didx _ type loc] = this;
              if (regv[didx] == null)
                [
                  move(code, type, loc, x64:lreg, regs_native_args[didx]);
                  regv[i] = null;
                  again? = true;
                ]
              else
                xchg = i;
              ];
          if (again?)
            exit<again> null;

          if (xchg == null)
            exit null;

          | other, tloc, tdidx, oloc |
          @[tdidx _ ,x64:lreg tloc] = regv[xchg];
          @[_ _ ,x64:lreg oloc] = other = regv[tdidx];
          x64:xchg(code, x64:lreg, tloc, x64:lreg, oloc);
          regv[tdidx] = null;
          if (tloc == regs_native_args[xchg])
            regv[xchg] = null
          else
            [
              other[3] = tloc;
              regv[xchg] = other
            ];
        ];

      lforeach(fn (@[didx _ type loc]) [
        // move the remaining arguments into place
        move(code, type, loc, x64:lreg, regs_native_args[didx]);
      ], simple);
    ];

  call_primitive = fn (code, called, prim, {null,vector} dest, int nargs,
                       args)
    [
      move_native_args(code, nargs, args);
      | flags, call |
      flags = primitive_flags(prim);
      // Save stack frame address (for GC and call traces)
      x64:mov(code, x64:lprimitive, called[mc:v_name],
              x64:lreg, reg_closure_in);
      call = if (flags & OP_NOALLOC) "bcall_prim_noalloc" else "bcall_prim";
      call_builtin(code, call);
    ];

  call_closure = fn (code, called, nargs, f)
    [
      if (called[mc:v_class] == mc:v_function)
        move(code, x64:lvar, called, x64:lreg, reg_scratch2)
      else if (closure?(f) && (closure_flags(f) & clf_noclosure))
        [
          assert(called[mc:v_class] == mc:v_global_constant);
          move(code, x64:lfunction, called[mc:v_name], x64:lreg, reg_scratch2);
        ]
      else
        [
          move(code, x64:lvar, called, x64:lreg, reg_closure_in);
          move(code, x64:lidx, reg_closure_in . x64:object_offset,
               x64:lreg, reg_scratch2);
        ];
      move(code, x64:limm, nargs, x64:lreg, reg_argcount);
      x64:add(code, x64:limm, x64:mcode_code_offset,
              x64:lreg, reg_scratch2);
      x64:call(code, x64:lreg, reg_scratch2);
    ];

  call = fn (code, called, nargs, callprimop, seclev?)
    [
      // update find_mcode() in error.c if these instructions change
      move(code, x64:lvar, called, x64:lreg, reg_closure_in);
      move(code, x64:limm, nargs, x64:lreg, reg_argcount);
      if (seclev?)
        set_seclev(code, reg_scratch2, x64:sl_c);
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

          push_args(code, 1, list(mc:var_make_constant(argv)));
          move(code, x64:lprimitive, called[mc:v_name], x64:lreg, reg_arg0);
          set_seclev(code, reg_arg2, x64:sl_c);
          call_builtin(code, "bapply_varargs");
          pop_args(code, 1);
          move(code, x64:lreg, reg_result, x64:lvar, dest);

          exit<function> true;
        ];

      push_args(code, nargs, args);
      move(code, x64:lprimitive, called[mc:v_name], x64:lreg, reg_arg0);
      move(code, x64:limm, nargs, x64:lreg, reg_arg1);
      set_seclev(code, reg_arg2, x64:sl_c);
      call_builtin(code, "bcall_varargs");
      pop_args(code, nargs);
      move(code, x64:lreg, reg_result, x64:lvar, dest);
    ];

  call_bconcat = fn (code, args, dest)
    [
      | nargs |
      nargs = llength(args);
      push_args(code, nargs, args);
      move(code, x64:limm, nargs, x64:lreg, reg_arg0);
      call_builtin(code, "bconcat");
      pop_args(code, nargs);
      move(code, x64:lreg, reg_result, x64:lvar, dest);
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
          move(code, x64:lreg, reg_result, x64:lvar, dest);
          exit<function> null;
        ];

      | robj, rval, rdest |

      rval = fetch1(code, val);
      if (rval == reg_scratch)
        robj = fetch1(code, obj)
      else
        robj = fetch2(code, obj);
      // robj == rval is still possible for obj[idx] = obj

      x64:test(code, x64:limm, MUDLLE_READONLY,
               x64:lidx, robj . x64:object_flags);
      x64:trap(code, x64:bne, error_value_read_only, null);

      rdest = reg_dest(dest);
      if (type1 == itype_vector)
        [
          nidx = nidx * word_size + x64:object_offset;
          x64:mov(code, x64:lreg, rval, x64:lidx, robj . nidx);
          move(code, x64:lreg, rval, x64:lvar, dest);
          exit<function> null;
        ];

      // it's a string
      nidx += x64:object_offset;

      // make sure we have a register that isn't GC-protected as it
      // will end up with a non-mudlle value
      if (!vfind?(rval, regs_allscratch))
        [
          | nr |
          nr = if (robj == reg_scratch) reg_scratch2 else reg_scratch;
          move(code, x64:lreg, rval, x64:lreg, nr);
          rval = nr
        ];

      type_trap(code, type_integer, var_in_reg(val, rval),
                caddr(types));

      assert(rval != robj);

      x64:shr(code, x64:limm, 1, x64:lreg, rval);
      x64:movbyte(code, x64:lreg, rval, x64:lidx, robj . nidx);
      x64:movzxbyte(code, x64:lreg, rval, x64:lreg, rdest);
      x64:lea32(code, x64:lridx, rdest . 1 . rdest . 1,
                x64:lreg, rdest);
      move(code, x64:lreg, rdest, x64:lvar, dest);
    ];

  call_seclevel = fn (code, dest, type)
    [
      | dreg |
      dreg = reg_dest(dest);
      set_seclev(code, dreg, type);
      move(code, x64:lreg, dreg, x64:lvar, dest);
    ];

  maybe_call_global_lookup = fn (code, arg, dest)
    [
      | val |
      if (!string?(val = mc:var_value(arg)))
        exit<function> false;
      move(code, x64:lglobal_index, val . x64:gl_mudlle, x64:lvar, dest);
      exit<function> true;
    ];

  mgen_call = fn (code, ins)
    [
      | args, nargs, called, dest |

      args = ins[mc:i_cargs];
      called = car(args);
      args = cdr(args);
      nargs = llength(args);
      dest = ins[mc:i_cdest];

      if (called == kset)
        [
          if (nargs == 3)
            exit<function> call_kset(code, args, dest, ins[mc:i_ctypes],
                                     ins[mc:i_csizeinfo]);
        ]
      else if (called == kseclevel)
        [
          if (nargs == 0)
            exit<function> call_seclevel(code, dest, x64:sl_mudlle);
        ]
      else if (called == kmaxseclevel)
        [
          if (nargs == 0)
            [
              | dreg |
              dreg = reg_dest(dest);
              move(code, x64:lspecial, "maxseclevel", x64:lreg, dreg);
              move(code, x64:lidx, dreg . 0, x64:lvar, dest);
              exit<function> null;
            ]
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
      | closure |
      if (called[mc:v_class] == mc:v_global_constant)
	[
	  | f, t |

	  f = global_value(called[mc:v_goffset]);
	  t = typeof(f);
	  if (t == type_varargs)
	    exit<function> call_varargs(code, called, f, args, dest);

          if ((t == type_secure || t == type_primitive)
              && primitive_nargs(f) == nargs)
            [
              if (t == type_secure)
                [
                  move_native_args(code, nargs, args);
                  call(code, called, nargs, "bcall_secure", true);
                ]
              else
                call_primitive(code, called, f, dest, nargs, args);
              exit<function> move(code, x64:lreg, reg_result,
                                  x64:lvar, dest);
            ];

          if (t == type_closure)
            closure = f;
        ];

      push_args(code, nargs, args);
      if (closure != null || vector?(ins[mc:i_cfunction]))
        call_closure(code, called, nargs, closure)
      else
        call(code, called, nargs, "bcall", false);
      pop_args(code, nargs);
      move(code, x64:lreg, reg_result, x64:lvar, dest);
    ];

  type_branch = fn (code, type, reversed, arg, typeset, dest)
    // Effects: Generates:
    //   !reversed: if typeof(arg) == type goto dest
    //   reversed: if typeof(arg) != type goto dest
    //   typeset is inferred type information on arg
    [
      | success, commit, abort, fail, abort_notptr, usedfail, r |

      fail = x64:new_label(code);
      usedfail = false;

      // Handle the 3 cases that arise in positive/negative typechecks:
      //   - value *not* of type (abort)
      //     flow of control must not proceed (eg not pointer)
      //   - value *of* type (success)
      //   - end of type check, final test (commit)
      if (reversed)
	[
	  commit = fn (cc)
	    x64:jcc(code, rev_x64relop(cc), dest);

	  success = fn (cc)
	    [
	      x64:jcc(code, cc, fail);
	      usedfail = true;
	    ];

	  abort = fn (cc)
	    x64:jcc(code, cc, dest);
	]
      else
	[
	  commit = fn (cc)
	    x64:jcc(code, cc, dest);

	  success = fn (cc)
	    x64:jcc(code, cc, dest);

	  abort = fn (cc)
	    [
	      x64:jcc(code, cc, fail);
	      usedfail = true;
	    ];
	];

      // checks can now assume the !reversed case

      abort_notptr = fn (r, typeset)
	[
	  if (typeset & itype_integer)
	    [
	      x64:test(code, x64:limm, 1, x64:lreg, r);
	      abort(x64:bne);
	    ];
	  if (typeset & itype_null)
	    [
	      x64:test(code, x64:lreg, r, x64:lreg, r);
	      abort(x64:be);
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
	      if (reversed) x64:jmp(code, dest)
	    ]
	  else if ((itype_inv & typeset) == itype_none)
	    [
	      // is of given type
	      if (!reversed) x64:jmp(code, dest)
	    ]
	  else if (type == type_integer)
	    [
	      x64:test(code, x64:limm, 1, x64:lvar, arg);
	      commit(x64:bne);
	    ]
	  else if (type == type_null)
	    [
              cmpeq(code, x64:limm, 0, x64:lvar, arg);
	      commit(x64:be);
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
		      x64:test(code, x64:lreg, r, x64:lreg, r);
		      success(x64:be);
		    ];
		  if (typeset & itype_integer)
		    [
		      x64:test(code, x64:limm, 1, x64:lreg, r);
		      abort(x64:bne);
		    ];
		  // stype_list becomes type_pair as we now know it's
		  // not null
		  type = type_pair;
		  typeset &= ~(itype_null | itype_integer);
		  exit<continue> null;
		];

	      x64:cmpbyte(code, x64:limm, type_pair,
			  x64:lidx, r . x64:object_type);
	      commit(x64:be);
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
                  x64:cmpbyte(code, x64:limm, type_closure,
                              x64:lidx, r . x64:object_type);
                  success(x64:be);
                ];
	      x64:cmpbyte(code, x64:limm, garbage_primitive,
			  x64:lidx, r . x64:object_info);
	      commit(x64:be);
	    ]
	  else			// generic type check
	    [
	      if (r == null)
		r = fetch1(code, arg);
	      x64:cmpbyte(code, x64:limm, type, x64:lidx, r . x64:object_type);
	      commit(x64:be);
	    ];
	  exit<break> null;
	];

      if (usedfail) x64:label(code, fail);
    ];

  // Check if 'var' is one of the types in 'typeset', given type
  // inference information in 'itypeset'.
  // If 'check_null?' is true, explicitly test for null. This gives
  // better error messages.
  typeset_trap = fn (code, int typeset, var, int itypeset, check_null?)
    [
      if (typeset == typeset_any)
        exit<function> null;

      | orig_typeset, possible |
      orig_typeset = typeset;

      // 'possible' is the typeset of (inferred) current possible types in var
      possible = if (itypeset == itype_any)
        typeset_any
      else
        bits_reduce(fn (it, n) n | mc:itype_typeset[it], 0, itypeset);

      // don't bother about impossible types
      typeset &= possible;

      // do nothing if no types can trap
      if ((possible & ~typeset) == 0)
        exit<function> null;

      | trap, jmpok, ok, r |
      jmpok = fn (cc)
        [
          if (ok == null)
            ok = x64:new_label(code);
          x64:jcc(code, cc, ok);
        ];
      trap = fn (cc) x64:trap(code, cc, error_bad_type, orig_typeset . var);

      // no valid types: always trap
      if (typeset == 0)
        exit<function> trap(x64:balways);

      | typeset_int, typeset_null |
      typeset_int = 1 << type_integer;
      typeset_null = 1 << type_null;

      // skip move to register if only testing for one of integer or null
      if (typeset != typeset_null && typeset != typeset_int)
        [
          r = fetch1(code, var);
          var = var_in_reg(var, r);
        ];

      <done> [
        if (typeset & typeset_int)
          [
            x64:test(code, x64:limm, 1, x64:lvar, var);
            typeset &= ~typeset_int;
            if (typeset == 0)
              exit<done> trap(x64:be);
            jmpok(x64:bne);
            possible &= ~typeset_int;
          ];
        if (typeset & typeset_null)
          [
            cmpeq(code, x64:limm, 0, x64:lvar, var);
            typeset &= ~typeset_null;
            if (typeset == 0)
              exit<done> trap(x64:bne);
            jmpok(x64:be);
            possible &= ~typeset_null;
          ];

        if (possible & typeset_int)
          [
            x64:test(code, x64:limm, 1, x64:lvar, var);
            trap(x64:bne);
            possible &= ~typeset_int;
          ];
        if (check_null? && (possible & typeset_null))
          [
            cmpeq(code, x64:limm, 0, x64:lvar, var);
            trap(x64:be);
            possible &= ~typeset_null;
          ];

        // done if no types left that can trap
        if ((possible & ~typeset) == 0)
          exit<done> null;

        assert(r != null);

        | typeset_primitive |
        typeset_primitive = typeset_function & ~(1 << type_closure);
        if ((typeset & typeset_primitive) == typeset_primitive)
          [
            x64:cmpbyte(code, x64:limm, garbage_primitive,
                        x64:lidx, r . x64:object_info);
            typeset &= ~typeset_primitive;
            if (typeset == 0)
              exit<done> trap(x64:bne);
            // the compare will trap on null
            jmpok(x64:be);
            possible &= ~(typeset_primitive | typeset_null);
          ];

        // don't have spare register for bit test here, so do a series
        // of compares
        | types |
        types = bits_reduce(cons, null, typeset);
        loop
          [
            | type |
            @(type . types) = types;
            x64:cmpbyte(code, x64:limm, type, x64:lidx, r . x64:object_type);

            // the compare will trap on null; done if that was the
            // last possible invalid type
            possible &= ~typeset_null;
            if ((possible & ~typeset) == 0)
              exit<done> null;

            | tmask |
            tmask = ~(1 << type);
            typeset &= tmask;
            if (typeset == 0)
              exit<done> trap(x64:bne);
            jmpok(x64:be);
            possible &= tmask;
          ];
      ];                        // end of <done>

      if (ok != null)
        x64:label(code, ok);
    ];

  type_trap = fn (code, type, var, itypeset)
    // Effects: Generate typecheck for value in var, given type
    //   knowledge itypeset.
    typeset_trap(code, mc:type_typeset(type), var, itypeset, false);

  setcc = fn (code, cc, dest)
    [
      | r |
      r = reg_dest(dest);
      // must avoid xor here
      x64:mov(code, x64:limm, 0, x64:lreg, r);
      x64:setcc(code, cc, x64:lreg, r);
      x64:lea32(code, x64:lridx, r . 1 . r . 1, x64:lreg, r);
      move(code, x64:lreg, r, x64:lvar, dest);
    ];

  in_scratch? = fn (v)
    mc:in_reg(v) && mc:get_reg(v) == reg_scratch;

  fetch1 = fn (code, var)
    if (mc:in_reg(var))
      mc:get_reg(var)
    else
      [
        // reg_scratch might be in use
        move(code, x64:lvar, var, x64:lreg, reg_scratch2);
        reg_scratch2
      ];

  // uses reg_scratch if var not in register
  fetch2 = fn (code, var)
    if (mc:in_reg(var))
      mc:get_reg(var)
    else
      [
        move(code, x64:lvar, var, x64:lreg, reg_scratch);
        reg_scratch
      ];

  // fetch var into dest's register, or reg_scratch2
  fetch_for_dest = fn (code, var, dest)
    if (mc:in_reg(var))
      var
    else if (mc:in_reg(dest))
      [
        move(code, x64:lvar, var, x64:lvar, dest);
        dest
      ]
    else
      var_in_reg(var, fetch1(code, var));

  call_builtin = fn (code, string op)
    x64:call(code, x64:lindirect, op);

  callop1 = fn (code, builtin, arg)
    // Scratch register usage: reg_scratch
    [
      move_native_args(code, 1, list(arg));
      call_builtin(code, builtin);
    ];

  callop2 = fn (code, builtin, commutes, arg1, arg2)
    // Scratch register usage: reg_scratch
    [
      move_native_args(code, 2, list(arg1, arg2));
      call_builtin(code, builtin);
    ];

  callop3 = fn (code, builtin, arg1, arg2, arg3)
    // Scratch register usage: reg_scratch
    [
      move_native_args(code, 3, list(arg1, arg2, arg3));
      call_builtin(code, builtin);
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
    //     1 for destination if it is not indexed (reg_scratch);
    //       i.e., dest is global
    [
      | sea, dea |

      // type: x64:limm, x64:lcst, x64:lfunction, x64:lreg,
      // x64:lidx, x64:l[rq]idx, x64:lvar
      sea = x64:resolve64(stype, source, true);
      stype = car(sea); source = cdr(sea);
      dea = x64:resolve(dtype, dest);
      dtype = car(dea); dest = cdr(dea);

      // type: x64:limm, x64:lcst, x64:lfunction, x64:lreg,
      // x64:lidx, x64:l[rq]idx, x64:lglobal(_constant)

      if (equal?(sea, dea)) exit<function> null;

      // can use mov if source or destination is a register or if
      // source is a constant

      if (dtype == x64:lreg && stype == x64:limm && source == 0)
        x64:xor(code, dtype, dest, dtype, dest)
      else if (dtype == x64:lreg
               || stype == x64:lreg
               || stype == x64:lseclev
               || stype == x64:lglobal_index
               || stype == x64:limm)
        x64:mov(code, stype, source, dtype, dest)
      else
        [
          assert_message(dtype != x64:lridx && dtype != x64:lqidx,
                         "scaled destination unsupported (for now)");
          // find a usable scratch register
          | sr |
          for (|i| i = 0; i < vlength(scratchregs); ++i)
            [
              | r |
              r = scratchregs[i];
              if (!(dtype == x64:lidx && car(dest) == r))
                exit<break> sr = r;
            ];
          assert(sr != null);
          x64:mov(code, stype, source, x64:lreg, sr);
          x64:mov(code, x64:lreg, sr, dtype, dest);
        ];
    ];

  compare = fn (code, arg1, arg2, relop)
    [
      | sea1, sea2, stype1, source1, stype2, source2, cmpop, cst1, cst2 |

      cmpop = if (relop == x64:be || relop == x64:bne)
        cmpeq
      else
        x64:cmp;

      sea1 = x64:resolve64(x64:lvar, arg1, true);
      stype1 = car(sea1); source1 = cdr(sea1);
      sea2 = x64:resolve64(x64:lvar, arg2, true);
      stype2 = car(sea2); source2 = cdr(sea2);

      if (stype1 == x64:limm64)
        [
          x64:mov(code, stype1, source1, x64:lreg, reg_scratch2);
          stype1 = x64:lreg;
          source1 = reg_scratch2;
        ]
      else if (stype2 == x64:limm64)
        [
          x64:mov(code, stype2, source2, x64:lreg, reg_scratch2);
          stype2 = x64:lreg;
          source2 = reg_scratch2;
        ];

      cst1 = stype1 == x64:limm;
      cst2 = stype2 == x64:limm;
      if (cst1 && cst2) // oops, cheat
        cst1 = cst2 = false;

      if (stype2 == x64:lreg || cst1)
        cmpop(code, stype1, source1, stype2, source2)
      else if (stype1 == x64:lreg || cst2)
        [
          cmpop(code, stype2, source2, stype1, source1);
          exit<function> commute_x64relop[relop]
        ]
      else
        [
          x64:mov(code, stype2, source2, x64:lreg, reg_scratch2);
          cmpop(code, stype1, source1, x64:lreg, reg_scratch2);
        ];
      relop
    ];

  cmpeq = fn (fcode, m1, a1, m2, a2)
    [
      if (m1 == x64:limm && a1 == 0
	  && (m2 == x64:lreg || m2 == x64:lvar && mc:in_reg(a2)))
	x64:test(fcode, m2, a2, m2, a2)
      else
	x64:cmp(fcode, m1, a1, m2, a2)
    ];

  logical_or = fn (code, v1, v2)
    [
      if (in_scratch?(v2))
	[
	  | t |
	  t = v1; v1 = v2; v2 = t;
	];
      move(code, x64:lvar, v1, x64:lreg, reg_scratch);
      // ((v1-1) | v2) - 1 == 0 iff v1 == false && v2 == false
      // (this assumes that 2 is an illegal value)
      x64:dec(code, x64:lreg, reg_scratch);

      if (v1 != v2)
        [
          x64:or(code, x64:lvar, v2, x64:lreg, reg_scratch);
          x64:dec(code, x64:lreg, reg_scratch);
        ];
    ];

  perform3 = fn (code, arg1, arg2, dest, op, commute_op)
    [
      | stype1, source1, stype2, source2, dtype |

      @(stype1 . source1) = x64:resolve(x64:lvar, arg1);
      @(stype2 . source2) = x64:resolve(x64:lvar, arg2);

      if (integer?(dest))
        dtype = x64:lreg
      else
        @(dtype . dest) = x64:resolve(x64:lvar, dest);

      if (dtype == x64:lreg)
	[
	  if (dtype == stype2 && dest == source2)
            // cannot overwrite dest yet
	    if (commute_op)
	      // no need for move, arg2 is already in dest
	      commute_op(code, stype1, source1, dtype, dest)
	    else
	      [
                // if we know that source1 is dead, we can skip the move
                if (stype1 != x64:lreg || source1 != reg_scratch)
                  [
                    move(code, stype1, source1, x64:lreg, reg_scratch2);
                    stype1 = x64:lreg; source1 = reg_scratch2;
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
          if (!(stype1 == x64:lreg && source1 == reg_scratch))
            [
              move(code, stype1, source1, x64:lreg, reg_scratch2);
              stype1 = x64:lreg;
              source1 = reg_scratch2;
            ];
	  op(code, stype2, source2, stype1, source1);
	  move(code, stype1, source1, dtype, dest);
	];
    ];

  perform3cst = fn (code, arg1, arg2, dest, op, commute_op,
		    cstop, commute_cstop)
    [
      | cst |
      if (integer?(cst = mc:var_value(arg2)) && int31?(cst))
        [
          move(code, x64:lvar, arg1, x64:lvar, dest);
          cstop(code, cst, x64:lvar, dest);
	]
      else if (commute_cstop && integer?(cst = mc:var_value(arg1))
               && int31?(cst))
        [
          move(code, x64:lvar, arg2, x64:lvar, dest);
          commute_cstop(code, cst, x64:lvar, dest);
        ]
      else
        perform3(code, arg1, arg2, dest, op, commute_op)
    ];
];
