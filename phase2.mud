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

library phase2 // Phase 2: 3-address (not really) generation
requires system, sequences, misc, compiler, vars, ins3
defines mc:phase2, mc:inline_builtin_call, mc:reset_closure_count
reads mc:this_module, mc:verbose
writes mc:this_function, mc:lineno
// See ins3.mud for details on these instructions

// temporaries are added to the local variables of each function
// there is no distinction between compiler-generated temps and user
// variables.

// A function is represented using the same structure as the tree structure,
// with the following change:
//   [mc:c_fvalue] contains the intermediate code
//   [mc:c_flocals] contains the extra local variables

// The result of phase2 is the converted top-level function. All other
// functions are reachable through the constant variables that contain
// them.

[

  | cundefined, ctrue, cfalse, czero, closure_count, builtin_branch,
    builtin_call?, builtin_branch_not, comp_true, comp_false, vset, phase2,
    gen_clist, gen_partial_clist, gen_component, gen_if, gen_while,
    gen_condition, make_bf, builtin_functions, builtin_branches, make_bb,
    make_btype, gen_abs_comp, gen_list, gen_vector, gen_set_mem!, vlist,
    vplist, vvector, vsequence, gen_const, ins_typeset_trap, verror, gen_error,
    gen_fail, gen_scopy, vassert, gen_assert, gen_builtin, fold_add,
    vstring_equal?, vstring_iequal?, vstring_cmp, vstring_icmp |

  verror          = mc:make_kglobal("error");
  vlist           = mc:make_kglobal("list");
  vplist          = mc:make_kglobal("plist");
  vsequence       = mc:make_kglobal("sequence");
  vset            = mc:make_kglobal("set!");
  vvector         = mc:make_kglobal("vector");
  vassert         = mc:make_kglobal("assert");
  vstring_cmp     = mc:make_kglobal("string_cmp");
  vstring_icmp    = mc:make_kglobal("string_icmp");
  vstring_equal?  = mc:make_kglobal("string_equal?");
  vstring_iequal? = mc:make_kglobal("string_iequal?");

  // Mapping of tree-operators to intermediate code branch operators
  builtin_branch = sequence
    (false, false,
     mc:branch_eq, mc:branch_ne,
     mc:branch_lt, mc:branch_le,
     mc:branch_gt, mc:branch_ge,
     false, false, false, false, false, false,
     false, false, false, false, false, false,
     false, false, false, false, false, false, false, false);
  assert(vlength(builtin_branch) == mc:parser_builtins);
  builtin_branch_not = sequence
    (false, false,
     mc:branch_ne, mc:branch_eq,
     mc:branch_ge, mc:branch_gt,
     mc:branch_le, mc:branch_lt);
  for (|i, l| [ i = vlength(builtin_branch_not); l = vlength(builtin_branch) ];
       i < l;
       ++i)
    assert(!builtin_branch[i]);

  make_bf = fn (name, op, nargs)
    sequence(mc:make_kglobal(name), nargs, op);

  gen_abs_comp = fn (fcode, result, args)
    [
      | var0, var1 |
      var0 = mc:new_local(fcode);
      var1 = mc:new_local(fcode);
      // var0 = arg;
      mc:ins_assign(fcode, var0, car(args));
      // var1 = var0 >> 30 (-1 for negative, 0 otherwise)
      mc:ins_compute(fcode, mc:b_shift_right, var1,
                     list(var0, mc:var_make_constant(INTBITS - 1)));
      // var0 = var0 ^ var1
      mc:ins_compute(fcode, mc:b_bitxor, var0, list(var0, var1));
      // result = var0 - var1
      mc:ins_compute(fcode, mc:b_subtract, result, list(var0, var1));
      result
    ];

  gen_list = fn (prot?) fn (fcode, result, args)
    [
      if (args == null)
        mc:ins_assign(fcode, result, mc:var_make_constant(null))
      else if (cdr(args) == null)
        mc:ins_compute(fcode, if (prot?) mc:b_pcons else mc:b_cons, result,
                       list(car(args), mc:var_make_constant(null)))
      else
        mc:ins_call(fcode, result, (if (prot?) vplist else vlist) . args);
      result
    ];

  gen_vector = fn (seq?) fn (fcode, result, args)
    [
      // slightly arbitrary cutoff
      if (llength(args) <= 8)
        mc:ins_compute(fcode,
                       if (seq?) mc:b_sequence else mc:b_vector,
                       result, args)
      else
        mc:ins_call(fcode, result,
                    (if (seq?) vsequence else vvector) . args);
      result
    ];

  gen_set_mem! = fn (offset, type) fn (fcode, result, args)
    [
      mc:ins_trap(fcode, mc:trap_type, error_bad_type,
                  list(car(args), mc:var_make_constant(type)));
      mc:ins_assign(fcode, result, cadr(args));
      mc:ins_memory(fcode, mc:memory_write_safe, car(args), offset, result);
    ];

  gen_const = fn (val) fn (fcode, result, args)
    mc:ins_assign(fcode, result, mc:var_make_constant(val));

  gen_scopy = fn (fcode, result, args)
    [
      mc:ins_compute(fcode, mc:b_add,
                     result, mc:var_make_constant("") . args);
    ];

  gen_error = fn (fcode, result, args)
    [
      | arg, val |
      @(arg) = args;
      if (arg[mc:v_class] == mc:v_constant)
        val = arg[mc:v_kvalue]
      else if (arg[mc:v_class] == mc:v_global_constant)
        val = global_value(arg[mc:v_goffset]);
      if (integer?(val) && val >= 0 && val < last_runtime_error)
        mc:ins_trap(fcode, mc:trap_always, val, null)
      else
        mc:ins_call(fcode, result, verror . args);
    ];

  gen_fail = fn (fcode, result, args)
    mc:ins_trap(fcode, mc:trap_always, error_abort, null);

  builtin_functions = sequence
    (make_bf("car",            mc:b_car,                     1),
     make_bf("cdr",            mc:b_cdr,                     1),
     make_bf("string_length",  mc:b_slength,                 1),
     make_bf("slength",        mc:b_slength,                 1),
     make_bf("vector_length",  mc:b_vlength,                 1),
     make_bf("vlength",        mc:b_vlength,                 1),
     make_bf("scopy",          gen_scopy,                    1),
     make_bf("bcopy",          gen_scopy,                    1),
     make_bf("typeof",         mc:b_typeof,                  1),
     make_bf("list",           gen_list(false),              -1),
     make_bf("plist",          gen_list(true),               -1),
     make_bf("vector",         gen_vector(false),            -1),
     make_bf("sequence",       gen_vector(true),             -1),
     make_bf("abs",            gen_abs_comp,                 1),
     make_bf("set_car!",       gen_set_mem!(0, type_pair),   2),
     make_bf("set_cdr!",       gen_set_mem!(1, type_pair),   2),
     make_bf("symbol_name",    mc:b_symbol_name,             1),
     make_bf("symbol_get",     mc:b_symbol_get,              1),
     make_bf("symbol_set!",    gen_set_mem!(1, type_symbol), 2),
     make_bf("cons",           mc:b_cons,                    2),
     make_bf("pcons",          mc:b_pcons,                   2),
     make_bf("compiled?",      gen_const(true),              0),
     make_bf("loop_count",     mc:b_loop_count,              0),
     make_bf("max_loop_count", mc:b_max_loop_count,          0),
     make_bf("error",          gen_error,                    1),
     make_bf("fail",           gen_fail,                     0));

  make_bb = fn (name, op, notop, nargs)
    sequence(mc:make_kglobal(name), nargs, op, notop);

  make_btype = fn (name, type)
    make_bb(name, mc:branch_type? + type, mc:branch_ntype? + type, 1);

  builtin_branches = sequence
    (make_btype("function?", stype_function),
     make_btype("closure?", type_closure),
     make_btype("secure?", type_secure),
     make_btype("varargs?", type_varargs),
     make_btype("primitive?", type_primitive),
     make_btype("integer?", type_integer),
     make_btype("float?", type_float),
     make_btype("bigint?", type_bigint),
     make_btype("string?", type_string),
     make_btype("vector?", type_vector),
     make_btype("pair?", type_pair),
     make_btype("symbol?", type_symbol),
     make_btype("table?", type_table),
     make_btype("null?", type_null),
     make_btype("list?", stype_list),

     make_btype("object?", type_object),
     make_btype("character?", type_character),
     make_btype("gone?", type_gone),

     make_bb("immutable?", mc:branch_immutable, mc:branch_mutable, 1),
     make_bb("readonly?", mc:branch_readonly, mc:branch_writable, 1),
     make_bb("any_primitive?", mc:branch_any_prim, mc:branch_not_prim, 1),

     make_bb("equal?", mc:branch_equal, mc:branch_nequal, 2));

  builtin_call? = fn (function, args, builtins)
    [
      | fc |
      fc = function[mc:v_class];
      if (fc == mc:v_global_define)
        [
          | g, gs |
          g = function[mc:v_goffset];
          gs = module_vstatus(g);
          if (string?(gs) && module_status(gs) == module_protected)
            function = mc:var_make_kglobal(global_name(g), g)
          else
            exit<function> false;
        ]
      else if (fc != mc:v_global_constant)
        exit<function> false;

      | bf |
      bf = vexists?(fn (builtin) builtin[0] == function, builtins);
      if (bf && (bf[1] < 0 || bf[1] == llength(args) - 1))
	bf
      else
	false
    ];

  closure_count = 0;
  mc:reset_closure_count = fn () closure_count = 0;

  // gniark, gniark, guess the values
  cundefined = mc:var_make_constant(42);
  ctrue = mc:var_make_constant(true);
  comp_true = sequence(mc:c_recall, -1, ctrue);
  cfalse = czero = mc:var_make_constant(false);
  comp_false = sequence(mc:c_recall, -1, cfalse);

  mc:phase2 = fn (mod)
    mod[mc:m_body] = phase2(mod[mc:m_body]);

  mc:inline_builtin_call = fn (il)
    [
      | ins, args, result, function, bf |

      ins = il[mc:il_ins];
      assert(ins[mc:i_class] == mc:i_call);
      args = ins[mc:i_cargs];
      result = ins[mc:i_cdest];
      function = car(args);

      if ((bf = builtin_call?(function, args, builtin_functions)) &&
          !function?(bf[2]))
        il[mc:il_ins] = mc:make_compute_ins(bf[2], result, cdr(args));
    ];

  ins_typeset_trap = fn (topf, arg, typeset)
    [
      // common case
      if (typeset == mc:typeset_any)
        exit<function> null;

      | types |
      types = mc:types_from_typeset(typeset);
      if (cdr(types) == null)
        [
          mc:ins_trap(topf, mc:trap_type, error_bad_type,
                      list(arg, mc:var_make_constant(car(types))));
          exit<function> null;
        ];

      | oklab, elab, int_ok? |
      elab = mc:new_label(topf);
      oklab = mc:new_label(topf);
      int_ok? = typeset & (1 << type_integer);
      mc:ins_branch(topf, mc:branch_type? + type_integer,
                    if (int_ok?) oklab else elab,
                    list(arg));
      if (int_ok?
          && cdr(types = lfilter(fn (n) n != type_integer, types)) == null)
        [
          mc:ins_trap(topf, mc:trap_type, error_bad_type,
                      list(arg, mc:var_make_constant(car(types))));
          mc:ins_branch(topf, mc:branch_always, oklab, null);
        ]
      else
        [
          | null_ok? |
          null_ok? = typeset & (1 << type_null);
          mc:ins_branch(topf, mc:branch_type? + type_null,
                        if (null_ok?) oklab else elab,
                        list(arg));
          if (null_ok?
              && cdr(types = lfilter(fn (n) n != type_null, types)) == null)
            [
              mc:ins_trap(topf, mc:trap_type, error_bad_type,
                          list(arg, mc:var_make_constant(car(types))));
              mc:ins_branch(topf, mc:branch_always, oklab, null);
            ]
          else
            [
              loop
                [
                  if (types == null) exit null;
                  mc:ins_branch(topf, mc:branch_type? + car(types), oklab,
                                list(arg));
                  types = cdr(types);
                ];
            ];
        ];

      mc:ins_label(topf, elab);
      mc:ins_trap(topf, mc:trap_always, error_bad_type, null);
      mc:ins_label(topf, oklab);
    ];

  phase2 = fn (top)
    // Returns: intermediate rep of function top
    [
      | clist, topf, result, args, typesets |

      mc:this_function = top;
      clist = top[mc:c_fvalue];
      topf = mc:new_fncode(top);
      args = top[mc:c_fargs];
      typesets = top[mc:c_fargtypesets];

      mc:lineno = top[mc:c_flineno];

      // generate argument count check trap (not for varargs functions)
      if (!top[mc:c_fvarargs])
	mc:ins_trap(topf, mc:trap_argcheck, error_wrong_parameters,
		    list(mc:var_make_constant(llength(top[mc:c_fargs]))));

      // argument type checks
      while (args != null)
	[
	  | typeset |

	  typeset = car(typesets);
          ins_typeset_trap(topf, car(args), typeset);
	  args = cdr(args);
	  typesets = cdr(typesets);
	];

      // compute expression
      result = gen_clist(topf, clist);

      // return result
      ins_typeset_trap(topf, result, top[mc:c_freturn_typeset]);
      mc:ins_return(topf, result);

      top[mc:c_fvalue] =
	mc:remove_var_aliases(mc:remove_labels(mc:remove_aliases(
          mc:remove_branches(mc:get_instructions(topf)))));
      top[mc:c_fnumber] = ++closure_count;

      mc:this_function = null;

      top
    ];

  // Generate code for all but last expression, and return the last component
  gen_partial_clist = fn (fcode, clist)
    loop
      [
        | c |
        @(c . clist) = clist;
        if (clist == null)
          exit c;
        gen_component(fcode, c);
      ];

  gen_clist = fn (fcode, clist)
    // Types: fcode : fncode, clist : list of component
    // Requires: clist not be empty
    // Effects: Generates 3-address code to evaluate clist in fcode
    // Returns: The variable that contains the expression's value
    gen_component(fcode, gen_partial_clist(fcode, clist));

  gen_builtin = fn (fcode, op, args)
    [
      | result |
      result = mc:new_local(fcode);
      mc:ins_compute(fcode, op, result, args);
      result
    ];

  // constant-fold (nested) addition of args (must be a list of two
  // elements); if partial?, the top-level add will never be folded
  fold_add = fn (fcode, args, partial?)
    [
      | terms, recurse, oline |

      // terms contains terms to be added; one of
      //   null
      //   arg1 . null
      //   arg2 . (lineno . arg1)

      // updates terms with the remaining terms to be added
      recurse = fn (args, partial?, lineno)
        lforeach(fn (argl) [
          | arg |
          arg = gen_partial_clist(fcode, argl);

          if (arg[mc:c_class] == mc:c_builtin
              && arg[mc:c_bfn] == mc:b_add)
            exit<function> recurse(arg[mc:c_bargs], false, arg[mc:c_lineno]);

          | r |
          r = gen_component(fcode, arg);
          if (terms != null)
            [
              // check if r can be constant folded with the most recent
              // term
              if (!(partial? && cdr(terms) == null)
                  && r[mc:v_class] == mc:v_constant)
                [
                  | r2 |
                  r2 = car(terms);
                  if (r2[mc:v_class] == mc:v_constant)
                    [
                      | v, v2 |
                      v = r[mc:v_kvalue];
                      v2 = r2[mc:v_kvalue];
                      if (integer?(v) && integer?(v2)
                          || string?(v) && string?(v2))
                        [
                          if (mc:verbose >= 3)
                            dformat("%d: %s FOLD%s\n",
                                    if (lineno > 0) lineno
                                    else mc:lineno,
                                    if (string?(v)) "STR" else "INT",
                                    if (partial?) " (partial)" else "");
                          exit<function>
                            terms = mc:var_make_constant(v2 + v)
                              . cdr(terms)
                        ]
                    ]
                ];
              // if there were already two terms in the list, add them
              // together now
              if (cdr(terms) != null)
                [
                  if (cadr(terms) > 0)
                    mc:lineno = cadr(terms);
                  terms = gen_builtin(fcode, mc:b_add,
                                      list(cddr(terms), car(terms)))
                    . null
                ]
            ];
          if (terms != null)
            terms = lineno . car(terms);
          terms = r . terms;
        ], args);

      oline = mc:lineno;
      recurse(args, partial?, mc:lineno);
      mc:lineno = oline;

      if (cdr(terms) == null)
        car(terms)
      else
        [
          | result |
          if (cadr(terms) > 0)
            mc:lineno = cadr(terms);
          result = gen_builtin(fcode, mc:b_add, list(cddr(terms), car(terms)));
          mc:lineno = oline;
          result
        ]
    ];

  gen_assert = fn (fcode, result, arg)
    [
      | slab, flab |
      slab = mc:new_label(fcode);
      flab = mc:new_label(fcode);
      gen_condition(fcode, arg, slab, false,
                    flab, fn () [
                      mc:ins_label(fcode, flab);
                      gen_fail(fcode, null, null);
                    ]);
      mc:ins_label(fcode, slab);
      mc:ins_assign(fcode, result, cundefined);
    ];

  gen_component = fn (fcode, c)
    // Types: fcode : fncode, c : component
    // Effects: Generates 3-address code to evaluate c in fcode
    // Returns: The variable that contains the expression's value
    [
      | class, olineno, result |

      olineno = mc:lineno;
      if (c[mc:c_lineno] > 0)
	mc:lineno = c[mc:c_lineno];

      class = c[mc:c_class];

      result = if (class == mc:c_assign)
	[
	  | var |
	  var = c[mc:c_asymbol];

	  // check global writes (except top-level defines and system-mutable)
	  if (var[mc:v_class] == mc:v_global
              && module_vstatus(var[mc:v_goffset]) != var_system_mutable
              && !lexists?(fn (v) v[mc:mv_gidx] == var[mc:v_goffset],
                           mc:this_module[mc:m_defines]))
            mc:ins_trap(fcode, mc:trap_global_write, 0, list(var));

	  mc:ins_assign(fcode, var, gen_clist(fcode, c[mc:c_avalue]));
	  var
	]
      else if (class == mc:c_recall)
        [
	  | var, vstat |
	  var = c[mc:c_rsymbol];

	  if (var[mc:v_class] == mc:v_global
              && ((vstat = module_vstatus(var[mc:v_goffset]))
                  != var_system_write)
              && vstat != var_system_mutable)
	    mc:ins_trap(fcode, mc:trap_global_read, 0, list(var));

	  var
        ]
      else if (class == mc:c_vref)
        [
	  | var |
	  var = mc:new_local(fcode);
	  mc:ins_vref(fcode, var, c[mc:c_rsymbol]);
	  var
        ]
      else if (class == mc:c_closure)
	[
	  | closure, f |
	  closure = mc:new_local(fcode);
	  f = mc:this_function;
	  mc:ins_closure(fcode, closure, phase2(c));
	  mc:this_function = f;
	  closure
	]
      else if (class == mc:c_execute)
	<done> [
	  | args, result, function, bf, fval |

	  result = mc:new_local(fcode);
          args = lcopy(c[mc:c_efnargs]);
          function = set_car!(args, gen_clist(fcode, car(args)));

          if (function == vassert && llength(cdr(args)) == 1)
            [
              gen_assert(fcode, result, cadr(args));
              exit<done> result;
            ];

          // fold string concatenations for calls to OP_STR_READONLY primitives
          if (function[mc:v_class] == mc:v_global_constant
              && any_primitive?(fval = global_value(function[mc:v_goffset]))
              && primitive_flags(fval) & OP_STR_READONLY)
            lmap!(fn (argl) [
              | arg |
              arg = gen_partial_clist(fcode, argl);
              if (arg[mc:c_class] == mc:c_builtin
                  && arg[mc:c_bfn] == mc:b_add)
                fold_add(fcode, arg[mc:c_bargs], false)
              else
                gen_component(fcode, arg);
            ], cdr(args))
          else
            lmap!(fn (arg) gen_clist(fcode, arg), cdr(args));

	  // Check for builtin functions
	  if (bf = builtin_call?(function, args, builtin_functions))
            if (function?(bf[2]))
              bf[2](fcode, result, cdr(args))
            else
              mc:ins_compute(fcode, bf[2], result, cdr(args))
	  else
	    mc:ins_call(fcode, result, args);
	  result
	]
      else if (class == mc:c_labeled)
	[
	  mc:start_block(fcode, c[mc:c_lname]);
	  mc:end_block(fcode, gen_clist(fcode, c[mc:c_lexpression]))
	]
      else if (class == mc:c_exit)
	[
	  if (!mc:exit_block(fcode, c[mc:c_ename],
                             gen_clist(fcode, c[mc:c_eexpression])))
	    if (c[mc:c_ename] == null) mc:error("no loop to exit from")
	    else mc:error("no block labeled %s", c[mc:c_ename]);
	  cundefined // but an exit never returns ...
	]
      else if (class == mc:c_builtin)
	[
	  | op, args |
	  op = c[mc:c_bfn];
	  args = c[mc:c_bargs];
	  if (op == mc:b_if)
	    [
	      gen_if(fcode, car(args), cadr(args), false);
	      cundefined
	    ]
	  else if (op == mc:b_ifelse)
	    gen_if(fcode, car(args), cadr(args), caddr(args))
	  else if (op == mc:b_sc_and || op == mc:b_sc_or)
	    gen_if(fcode, c . null, comp_true . null, comp_false . null)
	  else if (op == mc:b_while)
	    [
	      gen_while(fcode, car(args), cadr(args));
	      cundefined
	    ]
	  else if (op == mc:b_loop)
	    [
	      | looplab |

	      looplab = mc:new_label(fcode);
	      mc:ins_label(fcode, looplab);
	      mc:start_block(fcode, null);
	      gen_clist(fcode, car(args));
	      mc:ins_trap(fcode, mc:trap_loop, error_loop, null);
	      mc:ins_branch(fcode, mc:branch_always, looplab, null);
	      mc:end_block(fcode, false)
	    ]
	  else if (op == mc:b_set) // op with side effects
	    [
	      | result, vargs |
	      result = mc:new_local(fcode);
	      vargs = lmap(fn (arg) gen_clist(fcode, arg), args);
	      mc:ins_call(fcode, result, vset . vargs);
	      result
	    ]
	  else if (op == mc:b_add)
            fold_add(fcode, args, true)
          else
            [
              | vargs |
              vargs = lmap(fn (arg) gen_clist(fcode, arg), args);
              gen_builtin(fcode, op, vargs)
	    ]
	]
      else fail();

      mc:lineno = olineno;
      result
    ];

  gen_if = fn (fcode, condition, success, failure)
    // Types: fcode : fncode
    //        condition, success, failure : list of component or 'false'
    // Effects: generates code for 'if (condition) success else failure'
    // Returns: The variable that contains the if's result
    //   (if neither of success or failure is false)
    [
      | slab, flab, endlab, result |

      slab = mc:new_label(fcode);
      flab = mc:new_label(fcode);
      endlab = mc:new_label(fcode);
      if (success && failure) result = mc:new_local(fcode);
      gen_condition(fcode, condition,
		    slab, fn () [
		      | sresult |
		      mc:ins_label(fcode, slab);
		      if (success)
			[
			  sresult = gen_clist(fcode, success);
			  if (failure) // if not an 'ifelse' result is discarded
			    mc:ins_assign(fcode, result, sresult)
			];
		      mc:ins_branch(fcode, mc:branch_always, endlab, null);
		    ],
		    flab, fn () [
		      | fresult |
		      mc:ins_label(fcode, flab);
		      if (failure)
			[
			  fresult = gen_clist(fcode, failure);
			  if (success) // if not an 'ifelse' result is discarded
			    mc:ins_assign(fcode, result, fresult)
			];
		      mc:ins_branch(fcode, mc:branch_always, endlab, null);
		    ]);
      mc:ins_label(fcode, endlab);
      result
    ];

  gen_while = fn (fcode, condition, iteration)
    // Types: fcode : fncode
    //	      condition, iteration : list of component
    // Effects: Generates code for 'while (condition) iteration'
    [
      | looplab, mainlab, exitlab, endlab |
      looplab = mc:new_label(fcode);
      mainlab = mc:new_label(fcode);
      exitlab = mc:new_label(fcode);
      endlab = mc:new_label(fcode);

      mc:ins_label(fcode, looplab);
      gen_condition(fcode, condition,
		    mainlab, fn () [
		      mc:ins_label(fcode, mainlab);
		      gen_clist(fcode, iteration);
		      mc:ins_trap(fcode, mc:trap_loop, error_loop, null);
		      mc:ins_branch(fcode, mc:branch_always, looplab, null);
		    ],
		    exitlab, fn () [
		      mc:ins_label(fcode, exitlab);
		      mc:ins_branch(fcode, mc:branch_always, endlab, null);
		    ]);
      mc:ins_label(fcode, endlab);
    ];

  gen_condition = fn (fcode, condition, slab, success, flab, failure)
    // Types: fcode : fncode
    //        condition : list of component
    //        slab, flab : label
    //        success, failure : 0-argument functions or false
    // Effects: generates code to evaluate condition and branch to
    //   slab (respectively flab) on success (failure).
    //   success() and failure() are called to generate the actual code
    //   for theses cases.
    [
      | class, bargs, branch_succeed, branch_fail |

      condition = gen_partial_clist(fcode, condition);

      // We may want to restore the previous line number
      if (condition[mc:c_lineno] > 0)
	mc:lineno = condition[mc:c_lineno];

      class = condition[mc:c_class];
      if (class == mc:c_builtin)
	[
	  | op, args |
	  op = condition[mc:c_bfn];
	  args = condition[mc:c_bargs];
	  if (op == mc:b_sc_and)
	    [
	      // Tricky ...
	      | label |
	      label = mc:new_label(fcode);
	      gen_condition(fcode, car(args),
			    label, fn () [
			      mc:ins_label(fcode, label);
			      gen_condition(fcode, cadr(args),
					    slab, success, flab, failure);
			    ],
			    flab, false);
	      exit<function> null;
	    ]
	  else if (op == mc:b_sc_or)
	    [
	      // Tricky ...
	      | label |
	      label = mc:new_label(fcode);
	      gen_condition(fcode, car(args),
			    slab, false,
			    label, fn () [
			      mc:ins_label(fcode, label);
			      gen_condition(fcode, cadr(args),
					    slab, success, flab, failure);
			    ]);
	      exit<function> null;
	    ]
	  else if (op == mc:b_not)
	    [
	      // Swap conclusions
	      gen_condition(fcode, car(args), flab, failure, slab, success);
	      exit<function> null;
	    ]
	  else if (builtin_branch[op])
	    [
              | is_recall_zero? |

              is_recall_zero? = fn (c)
                (c[mc:c_class] == mc:c_recall
                 && mc:var_value(c[mc:c_rsymbol]) == 0);

	      // Generate specialised branch for certain operators
	      // (eg <, <=, ...)
              args = lmap(fn (arg) gen_partial_clist(fcode, arg), args);

              if (op == mc:b_eq || op == mc:b_ne)
                <normal> [
                  | arg1, arg2 |
                  @(arg1 arg2) = args;
                  if (is_recall_zero?(arg2))
                    null
                  else if (is_recall_zero?(arg1))
                    arg1 = arg2
                  else
                    exit<normal> null;

                  args = list(arg1);
                  exit<function>
                    if (op == mc:b_eq)
                      gen_condition(fcode, args, flab, failure, slab, success)
                    else
                      gen_condition(fcode, args, slab, success, flab, failure);
                ];

              bargs = lmap(fn (arg) gen_component(fcode, arg), args);
              branch_fail = builtin_branch_not[op];
              branch_succeed = builtin_branch[op];
	    ]
	  else
	    [
	      // default code
	      bargs = gen_component(fcode, condition) . null;
	      branch_fail = mc:branch_false;
	      branch_succeed = mc:branch_true;
	    ]
	]
      else if (class == mc:c_execute)
	[
	  | function, bf |

	  bargs = lmap(fn (arg) gen_clist(fcode, arg),
                       condition[mc:c_efnargs]);
	  function = car(bargs);

	  // Check for builtin functions
	  if (bf = builtin_call?(function, bargs, builtin_branches))
	    [
	      bargs = cdr(bargs);
	      branch_succeed = bf[2];
	      branch_fail = bf[3];

              if (branch_succeed == mc:branch_equal)
                <done> [
                  | arg1, arg2, val, ok?, type |
                  ok? = fn (v) string?(v) || equal?(v, '[]);
                  @(arg1 arg2) = bargs;
                  if (ok?(val = mc:var_value(arg2)))
                    null
                  else if (ok?(val = mc:var_value(arg1)))
                    [
                      | t |
                      t = arg1;
                      arg1 = arg2;
                      arg2 = t;
                    ]
                  else
                    exit<done> null;

                  // arg1 is variable, arg2 is the constant 'val'
                  if (vector?(val))
                    [
                      type = type_vector;
                      branch_succeed = mc:branch_vlength;
                      branch_fail = (mc:branch_vlength
                                     + mc:branch_ne - mc:branch_eq);
                    ]
                  else
                    [
                      type = type_string;
                      branch_succeed = mc:branch_slength;
                      branch_fail = (mc:branch_slength
                                     + mc:branch_ne - mc:branch_eq);
                    ];

                  mc:ins_branch(fcode, mc:branch_ntype? + type, flab,
                                list(arg1));
                  if (type == type_string && slength(val) > 0)
                    [
                      | res |
                      res = mc:new_local(fcode);
                      mc:ins_call(fcode, res,
                                  list(vstring_equal?, arg1, arg2));
                      bargs = list(res);
                      branch_succeed = mc:branch_true;
                      branch_fail = mc:branch_false;
                    ]
                  else
                    bargs = list(arg1, czero);
                ]
	    ]
	  else
	    <done> [
	      | result, swap? |

              swap? = false;
	      result = mc:new_local(fcode);
	      if (bf = builtin_call?(function, bargs, builtin_functions))
                [
                  | op |
                  op = bf[2];
                  if (function?(op))
                    op(fcode, result, cdr(bargs))
                  else if (op == mc:b_slength || op == mc:b_vlength)
                    [
                      if (op == mc:b_slength)
                        [
                          branch_fail = mc:branch_slength; // eq
                          branch_succeed = (mc:branch_slength
                                            + mc:branch_ne - mc:branch_eq);
                        ]
                      else
                        [
                          branch_fail = mc:branch_vlength; // eq
                          branch_succeed = (mc:branch_vlength
                                            + mc:branch_ne - mc:branch_eq);
                        ];
                      bargs = list(cadr(bargs), czero);
                      exit<done> null;
                    ]
                  else
                    mc:ins_compute(fcode, op, result, cdr(bargs))
                ]
	      else
                [
                  if (function == vstring_cmp)
                    [
                      swap? = true;
                      set_car!(bargs, vstring_equal?);
                    ]
                  else if (function == vstring_icmp)
                    [
                      swap? = true;
                      set_car!(bargs, vstring_iequal?);
                    ];

                  mc:ins_call(fcode, result, bargs);
                ];
	      bargs = result . null;
              if (swap?)
                [
                  branch_fail = mc:branch_true;
                  branch_succeed = mc:branch_false;
                ]
              else
                [
                  branch_fail = mc:branch_false;
                  branch_succeed = mc:branch_true;
                ]
            ];
	]
      else
	[
	  // default code, again
	  bargs = gen_component(fcode, condition) . null;
	  branch_fail = mc:branch_false;
	  branch_succeed = mc:branch_true;
	];

      // generate basic code
      if (success)
        [
          mc:ins_branch(fcode, branch_fail, flab, bargs);
          success();
          if (failure) failure();
        ]
      else
        [
          mc:ins_branch(fcode, branch_succeed, slab, bargs);
          if (failure) failure()
          else mc:ins_branch(fcode, mc:branch_always, flab, null);
        ];
    ];

];
