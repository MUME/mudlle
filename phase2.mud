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

library phase2 // Phase 2: 3-address (not really) generation
requires system, sequences, misc, compiler, vars, ins3
defines mc:phase2
reads mc:this_module
writes mc:this_function
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
  | cundefined, ctrue, cfalse, closure_count, builtin_branch, builtin_call?,
    builtin_branch_not, comp_undefined, comp_true, comp_false, vset, phase2,
    gen_clist, gen_component, gen_if, gen_while, gen_condition, make_bf,
    builtin_functions, builtin_branches, make_bb, make_btype |

  // Mapping of tree-operators to intermediate code branch operators
  builtin_branch = sequence
    (mc:branch_or, mc:branch_and,
     false, false,
     mc:branch_eq, mc:branch_ne,
     mc:branch_lt, mc:branch_le,
     mc:branch_gt, mc:branch_ge,
     false, false, false, false, false, false,
     false, false, false, false, false, false,
     false, false, false, false, false, false, false);
  builtin_branch_not = sequence
    (mc:branch_nor, mc:branch_nand,
     false, false,
     mc:branch_ne, mc:branch_eq,
     mc:branch_ge, mc:branch_gt,
     mc:branch_le, mc:branch_lt);

  make_bf = fn (name, op, nargs)
    sequence(mc:var_make_kglobal(name, global_lookup(name)), nargs, op);

  builtin_functions = sequence
    (make_bf("car", mc:b_car, 1),
     make_bf("cdr", mc:b_cdr, 1),
     make_bf("string_length", mc:b_slength, 1),
     make_bf("vector_length", mc:b_vlength, 1));

  make_bb = fn (name, op, notop, nargs)
    sequence(mc:var_make_kglobal(name, global_lookup(name)), nargs, op, notop);

  make_btype = fn (name, type)
    make_bb(name, mc:branch_type? + type, mc:branch_ntype? + type, 1);

  builtin_branches = sequence
    (make_btype("function?", stype_function),
     make_btype("closure?", type_closure),
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
     make_btype("gone?", type_gone));

  builtin_call? = fn (function, args, builtins)
    [
      | bf |

      if ((bf = vexists?(fn (builtin) builtin[0] == function, builtins)) &&
	  (bf[1] < 0 || bf[1] == llength(args) - 1))
	bf
      else
	false
    ];

  closure_count = 0;

  // gniark, gniark, guess the values
  cundefined = mc:var_make_constant(42);
  comp_undefined = sequence(mc:c_recall, cundefined) . null;
  ctrue = mc:var_make_constant(true);
  comp_true = sequence(mc:c_recall, ctrue);
  cfalse = mc:var_make_constant(false);
  comp_false = sequence(mc:c_recall, cfalse);

  vset = mc:var_make_kglobal("set!", global_lookup("set!"));

  mc:phase2 = fn (mod)
    mod[mc:m_body] = phase2(mod[mc:m_body]);

  phase2 = fn (top)
    // Returns: intermediate rep of function top
    [
      | clist, topf, result, args, types |

      mc:this_function = top;
      clist = top[mc:c_fvalue];
      topf = mc:new_fncode(top);
      args = top[mc:c_fargs];
      types = top[mc:c_fargtypes];

      // generate argument count check trap (not for varargs functions)
      if (!top[mc:c_fvarargs])
	mc:ins_trap(topf, mc:trap_argcheck, error_wrong_parameters,
		    list(mc:var_make_constant(llength(top[mc:c_fargs]))));

      // argument type checks
      while (args != null)
	[
	  | type |

	  type = car(types);
	  if (type != stype_any)
	    mc:ins_trap(topf, mc:trap_type, error_bad_type,
			list(car(args), mc:var_make_constant(type)));
	  args = cdr(args);
	  types = cdr(types);
	];

      // compute expression
      result = gen_clist(topf, clist);

      // return result
      if (top[mc:c_freturn_type] != stype_any)
	mc:ins_trap(topf, mc:trap_type, error_bad_type,
		    list(result, mc:var_make_constant(top[mc:c_freturn_type])));
      mc:ins_return(topf, result);

      top[mc:c_fvalue] =
	mc:remove_var_aliases(mc:remove_labels(mc:remove_aliases(mc:remove_branches(mc:get_instructions(topf)))));
      top[mc:c_fnumber] = (closure_count = closure_count + 1);
      top
    ];

  gen_clist = fn (fcode, clist)
    // Types: fcode : fncode, clist : list of component
    // Requires: clist not be empty
    // Effects: Generates 3-address code to evaluate clist in fcode
    // Returns: The variable that contains the expression's value
    [
      | result |
      while (clist != null)
	[
	  result = gen_component(fcode, car(clist));
	  clist = cdr(clist)
	];
      result
    ];

  gen_component = fn (fcode, c)
    // Types: fcode : fncode, c : component
    // Effects: Generates 3-address code to evaluate c in fcode
    // Returns: The variable that contains the expression's value
    [
      | class |
      class = c[mc:c_class];
      if (class == mc:c_assign)
	[
	  | var |
	  var = c[mc:c_asymbol];

	  // check global writes (except top-level defines)
	  if (var[mc:v_class] == mc:v_global &&
	      !assq(var[mc:v_goffset], mc:this_module[mc:m_defines]))
	    mc:ins_trap(fcode, mc:trap_global_write, error_variable_read_only,
			list(var));

	  mc:ins_assign(fcode, var, gen_clist(fcode, c[mc:c_avalue]));
	  var
	]
      else if (class == mc:c_recall)
	c[mc:c_rsymbol]
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
	[
	  | args, result, function, bf |

	  result = mc:new_local(fcode);
	  args = lmap(fn (arg) gen_clist(fcode, arg), c[mc:c_efnargs]);
	  function = car(args);

	  // Check for builtin functions
	  if (bf = builtin_call?(function, args, builtin_functions))
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
	  if (!mc:exit_block(fcode, c[mc:c_ename], gen_clist(fcode, c[mc:c_eexpression])))
	    if (c[mc:c_ename] == null) mc:error("No loop to exit from")
	    else mc:error("No block labeled %s", c[mc:c_ename]);
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
	  else
	    [
	      | result, vargs |
	      result = mc:new_local(fcode);
	      vargs = lmap(fn (arg) gen_clist(fcode, arg), c[mc:c_bargs]);
	      mc:ins_compute(fcode, op, result, vargs);
	      result
	    ]
	]
      else 1/0;
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
      | done, class, bargs, branch_succeed, branch_fail |

      // Generate code for all but last expression
      while (cdr(condition) != null)
	[
	  gen_component(fcode, car(condition));
	  condition = cdr(condition);
	];
      condition = car(condition); // The actual condition

      class = condition[mc:c_class];
      done = false;
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
	      done = true;
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
	      done = true;
	    ]
	  else if (op == mc:b_not)
	    [
	      // Swap conclusions
	      gen_condition(fcode, car(args), flab, failure, slab, success);
	      done = true;
	    ]
	  else if (builtin_branch[op])
	    [
	      // Generate specialised branch for certain operators
	      // (eg <, <=, ...)
	      bargs = lmap(fn (arg) gen_clist(fcode, arg), args);
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

	  bargs = lmap(fn (arg) gen_clist(fcode, arg), condition[mc:c_efnargs]);
	  function = car(bargs);

	  // Check for builtin functions
	  if (bf = builtin_call?(function, bargs, builtin_branches))
	    [
	      bargs = cdr(bargs);
	      branch_succeed = bf[2];
	      branch_fail = bf[3];
	    ]
	  else
	    [
	      | result |

	      result = mc:new_local(fcode);
	      if (bf = builtin_call?(function, bargs, builtin_functions))
		mc:ins_compute(fcode, bf[2], result, cdr(bargs))
	      else
		mc:ins_call(fcode, result, bargs);
	      bargs = result . null;
	      branch_fail = mc:branch_false;
	      branch_succeed = mc:branch_true;
	    ]
	]
      else
	[
	  // default code, again
	  bargs = gen_component(fcode, condition) . null;
	  branch_fail = mc:branch_false;
	  branch_succeed = mc:branch_true;
	];
      
      if (!done) // generate basic code
	[
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
	]
    ];

];
