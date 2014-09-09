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

library optimise // The actual optimisations
requires compiler, vars, flow, ins3, phase2, graph, dlist, sequences, misc
defines mc:optimise_functions, mc:recompute_vars, mc:fold_branch
reads mc:verbose
writes mc:lineno, mc:this_function
[
  | fold, compute_ops, branch_ops, useless_instructions, remove_instruction,
    fold_constants, fold_length_branch, propagate_copies,
    propagate_closure_constants, propagate_closure_constant,
    eliminate_dead_code, change, pfoldbranch, partialfold, replace_use,
    replace_fn_use, remaining_fns, optimise_function, compute_trap_types,
    check_compute_trap, convert_to_type_trap, consttype, simple_equal?,
    really_useless |

  fold = fn (ops, op, args, dofold)
    // Types: ops: array of function
    //        op : integer (index valid for ops)
    //        args : list of var
    //        dofold : fn (any)
    // Effects: If all args are constants, operation can be folded.
    //   ops[op](args) is called to do the actual folding, and the
    //   result is passed to dofold.
    // Returns: true if the operation was folded
    if (lforall?(fn (v) v[mc:v_class] == mc:v_constant, args) &&
	op < vector_length(ops))
      [
	| val, doop |

	doop = ops[op];
        if (!doop)
          exit<function> false;
	if (args == null)
	  val = doop()
	else if (cdr(args) == null)
	  val = doop(car(args)[mc:v_kvalue])
	else if (cddr(args) == null)
	  val = doop(car(args)[mc:v_kvalue], cadr(args)[mc:v_kvalue])
	else
          val = apply(doop,
                      list_to_vector(lmap(fn (arg) arg[mc:v_kvalue], args)));
	if (val) [ dofold(cdr(val)); true ]
	else false
      ]
    else
      false;

  compute_ops = sequence
    (false,                     // sc_or
     false,                     // sc_and
     fn (x, y) true . x == y,
     fn (x, y) true . x != y,
     fn (x, y) if (integer?(x) && integer?(y)) true . x < y else false,
     fn (x, y) if (integer?(x) && integer?(y)) true . x <= y else false,
     fn (x, y) if (integer?(x) && integer?(y)) true . x > y else false,
     fn (x, y) if (integer?(x) && integer?(y)) true . x >= y else false,
     fn (x, y) if (integer?(x) && integer?(y)) true . x | y else false,
     fn (x, y) if (integer?(x) && integer?(y)) true . x ^ y else false,
     fn (x, y) if (integer?(x) && integer?(y)) true . x & y else false,
     fn (x, y) if (integer?(x) && integer?(y)) true . x << y else false,
     fn (x, y) if (integer?(x) && integer?(y)) true . x >> y else false,
     fn (x, y) if (integer?(x) && integer?(y)) true . x + y else false,
     fn (x, y) if (integer?(x) && integer?(y)) true . x - y else false,
     fn (x, y) if (integer?(x) && integer?(y)) true . x * y else false,
     fn (x, y) if (!integer?(x) || !integer?(y) || y == 0) false else true . x / y,
     fn (x, y) if (!integer?(x) || !integer?(y) || y == 0) false else true . x % y,
     fn (x) if (integer?(x)) true . -x else false,
     fn (x) true . !x,
     fn (x) if (integer?(x)) true . ~x else false,
     false,                     // ifelse
     false,                     // if
     false,                     // while
     false,                     // loop
     fn (x, y)
       if (integer?(y))
         if (vector?(x))
           if (y >= -vector_length(x) && y < vector_length(x)) true . x[y]
	   else false
         else if (string?(x))
           if (y >= -string_length(x) && y < string_length(x)) true . x[y]
	   else false
         else false
       else if (string?(y) && table?(x)) true . x[y]
       else false,
     false,                     // set
     fn (x, y) false,           // cons's are mutable
     fn (x) false,              // assign
     fn (x) if (pair?(x)) true . car(x) else false,
     fn (x) if (pair?(x)) true . cdr(x) else false,
     fn (x) if (string?(x)) true . string_length(x) else false,
     fn (x) if (vector?(x)) true . vector_length(x) else false,
     fn (x, y) if (integer?(x) && integer?(y)) true . x + y else false,
     fn (x) true . typeof(x),
     fn () false,               // loop_count
     fn () false,               // max_loop_count
     fn (x) if (symbol?(x)) true . symbol_name(x) else false,
     fn (x) if (symbol?(x)) true . symbol_get(x) else false,
     false,                     // vectors are mutable
     fn v true . check_immutable(protect(v)), // sequence
     fn (x, y) true . check_immutable(pcons(x, y)) // pcons
     );
  assert(vlength(compute_ops) == mc:builtins);

  branch_ops = sequence
    (fn () false,
     fn () false,
     fn (x) true . x,
     fn (x) true . !x,
     fn (x, y) true . (x || y),
     fn (x, y) true . !(x || y),
     fn (x, y) true . (x && y),
     fn (x, y) true . !(x && y),
     fn (x, y) true . x == y,
     fn (x, y) true . x != y,
     fn (x, y) if (integer?(x) && integer?(y)) true . x < y else false,
     fn (x, y) if (integer?(x) && integer?(y)) true . x >= y else false,
     fn (x, y) if (integer?(x) && integer?(y)) true . x <= y else false,
     fn (x, y) if (integer?(x) && integer?(y)) true . x > y else false,
     fn (x, y) if (string?(x) && integer?(y)) true . slength(x) == y else false,
     fn (x, y) if (string?(x) && integer?(y)) true . slength(x) != y else false,
     fn (x, y) if (string?(x) && integer?(y)) true . slength(x) < y else false,
     fn (x, y) if (string?(x) && integer?(y)) true . slength(x) >= y else false,
     fn (x, y) if (string?(x) && integer?(y)) true . slength(x) <= y else false,
     fn (x, y) if (string?(x) && integer?(y)) true . slength(x) > y else false,
     fn (x, y) if (vector?(x) && integer?(y)) true . vlength(x) == y else false,
     fn (x, y) if (vector?(x) && integer?(y)) true . vlength(x) != y else false,
     fn (x, y) if (vector?(x) && integer?(y)) true . vlength(x) < y else false,
     fn (x, y) if (vector?(x) && integer?(y)) true . vlength(x) >= y else false,
     fn (x, y) if (vector?(x) && integer?(y)) true . vlength(x) <= y else false,
     fn (x, y) if (vector?(x) && integer?(y)) true . vlength(x) > y else false,
     fn (x, y) true . equal?(x, y),
     fn (x, y) true . !equal?(x, y),
     fn (x) true . immutable?(x),
     fn (x) true . !immutable?(x),
     fn (x) true . readonly?(x),
     fn (x) true . !readonly?(x),
     fn (x) true . any_primitive?(x),
     fn (x) true . !any_primitive?(x));
  assert(vlength(branch_ops) == mc:branch_type?);

  // false means cannot trap
  // stype_any means always perform operation
  // type_xxx means both arguments have to be of this type
  compute_trap_types = sequence
    (false,        // sc_or
     false,        // sc_and
     false,        // eq
     false,        // ne
     type_integer, // lt
     type_integer, // le
     type_integer, // gt
     type_integer, // ge
     type_integer, // bitor
     type_integer, // bitxor
     type_integer, // bitand
     type_integer, // shift_left
     type_integer, // shift_right
     null,         // add -- special case below
     type_integer, // subtract
     type_integer, // multiply
     stype_any,    // divide -- may div zero trap
     stype_any,    // remainder -- may div zero trap
     type_integer, // negate
     false,        // not
     type_integer, // bitnot
     false,        // ifelse
     false,        // if
     false,        // while
     false,        // loop
     stype_any,    // ref
     false,        // set
     false,        // cons
     false,        // assign
     type_pair,    // car
     type_pair,    // cdr
     type_string,  // slength
     type_vector,  // vlength
     type_integer, // iadd
     false,        // typeof
     false,        // loop_count
     false,        // max_loop_count
     type_symbol,  // symbol_name
     type_symbol,  // symbol_get
     false,        // vector
     false,        // sequence
     false         // pcons
     );
  assert(vlength(compute_trap_types) == mc:builtins);

  mc:fold_branch = fn (il, val) // folding function for branches
    [
      | ins, toins |

      ins = il[mc:il_ins];

      // change branch & remove graph edge
      if (val)
	[
	  ins[mc:i_bop] = mc:branch_always;
	  // remove fall through edge
	  graph_edges_out_apply(fn (e) [
            if (graph_edge_get(e))
              [
                | toil |
                toil = dget(graph_node_get(graph_edge_to(e))[mc:f_ilist]);
                assert(toins == null);
                toins = toil[mc:il_ins];
                graph_remove_edge(e)
              ]
          ], il[mc:il_node]);
	]
      else
	[
          | toil, tonode |
	  ins[mc:i_bop] = mc:branch_never;
          toil = ins[mc:i_bdest][mc:l_ilist];
	  tonode = toil[mc:il_node];
          toins = toil[mc:il_ins];
	  // remove label's edge
	  graph_edges_out_apply(
            fn (e) if (graph_edge_to(e) == tonode) graph_remove_edge(e),
            il[mc:il_node]);
	];
      ins[mc:i_bargs] = null;
      change = true;

      if (!(toins[mc:i_class] == mc:i_trap
            && toins[mc:i_top] == mc:trap_always))
        [
          | prevline |
          prevline = mc:lineno;
          mc:lineno = il[mc:il_lineno];
          mc:warning("branch is %s taken", if (val) "always" else "never");
          mc:lineno = prevline;
        ];

      if (mc:verbose >= 3)
	[
	  dformat("FOLD %s\n", il[mc:il_number]);
	];
    ];

  partialfold = fn (args, pfold)
    [
      | arg1, arg2, t |

      arg1 = car(args); arg2 = cadr(args);
      if (arg1[mc:v_class] == mc:v_constant)
	[
	  t = arg1; arg1 = arg2; arg2 = t;
	];
      if (arg2[mc:v_class] == mc:v_constant)
	pfold(arg1, arg2[mc:v_kvalue]);
    ];

  pfoldbranch = fn (il, bop, arg)
    [
      | ins |

      ins = il[mc:il_ins];
      ins[mc:i_bop] = bop;
      ins[mc:i_bargs] = arg . null;
      if (mc:verbose >= 3)
	[
	  dformat("FOLDOR %s\n", il[mc:il_number]);
	];
    ];

  // true if arg can use '==' instead of 'equal?()'
  simple_equal? = fn (arg)
    [
      | t |
      t = consttype(arg);
      (t >= 0
       && !((1 << t) & (1 << type_symbol | 1 << type_vector | 1 << type_pair
                        | 1 << type_table | 1 << type_string | 1 << type_float
                        | 1 << type_bigint)))
    ];

  // 'ins' is a comparison branch; check if it is comparing a string/vector
  // length to a constant integer
  fold_length_branch = fn (il, previl, ins, bop)
    [
      if (previl == null) exit<function> null;

      | bvar0, bvar1, val |
      @(bvar0 bvar1) = ins[mc:i_bargs];
      val = mc:var_value(bvar1);
      if (!integer?(val))
        [
          val = mc:var_value(bvar0);
          if (!integer?(val))
            exit<function> null;
          @(bvar1 bvar0) = ins[mc:i_bargs];
          if (bop >= mc:branch_lt && bop <= mc:branch_gt)
            // flip logic
            bop = mc:branch_gt + mc:branch_lt - bop;
        ];
      // avoid problems with overflow
      if (val < 0 || val > MAX_STRING_SIZE)
        exit<function> null;

      // check if the previous instruction was {vector,string}_length
      | pins, lop |
      pins = previl[mc:il_ins];

      if (!(pins[mc:i_class] == mc:i_compute
            && ((lop = pins[mc:i_aop]) == mc:b_vlength
                || lop == mc:b_slength)
            && pins[mc:i_adest] == bvar0))
        exit<function> null;

      ins[mc:i_bop] = (bop - mc:branch_eq
                       + (if (lop == mc:b_vlength) mc:branch_vlength
                          else mc:branch_slength));
      ins[mc:i_bargs] = list(car(pins[mc:i_aargs]), bvar1);
      assert(!ins[mc:i_btypes]);

      | args |
      args = previl[mc:il_arguments];
      il[mc:il_arguments] = if (args) bcopy(args) else false;

      // We have to leave the length instruction behind in case its result is
      // needed, but allow for it to be completely optimized away.
      really_useless = pins . really_useless;

      change = true;
    ];

  fold_constants = fn (il, previl)
    // Types: il : instruction
    // Effects: Does constant folding on given instruction
    [
      | ins, class |

      ins = il[mc:il_ins];
      class = ins[mc:i_class];

      if (class == mc:i_compute)
	[
	  | op, args, foldtocst |

	  op = ins[mc:i_aop]; args = ins[mc:i_aargs];
	  foldtocst = fn (val) // folding function
	    [
	      ins[mc:i_aop] = mc:b_assign;
	      ins[mc:i_aargs] = mc:var_make_constant(val) . null;
	      if (mc:verbose >= 3)
		[
		  dformat("FOLD %s\n", il[mc:il_number]);
		];
	      change = true;
	    ];
	  fold(compute_ops, op, args, foldtocst)
	]
      else if (class == mc:i_branch)
	[
	  | op, args |

	  op = ins[mc:i_bop]; args = ins[mc:i_bargs];
	  if (!fold(branch_ops, ins[mc:i_bop], ins[mc:i_bargs],
		    fn (val) mc:fold_branch(il, val)))
	    // special cases for partial constant folding
	    if (op == mc:branch_or || op == mc:branch_nor ||
		op == mc:branch_and || op == mc:branch_nand)
	      partialfold
		(args,
		 fn (arg1, x)
		 [
		   if (op == mc:branch_nand && !x ||
		       op == mc:branch_or && x)
		     mc:fold_branch(il, true)
		   else if (op == mc:branch_and && !x ||
			    op == mc:branch_nor && x)
		     mc:fold_branch(il, false)
		   else if (op == mc:branch_or || op == mc:branch_and)
		     pfoldbranch(il, mc:branch_true, arg1)
		   else
		     pfoldbranch(il, mc:branch_false, arg1)
		 ])
          else if ((op == mc:branch_equal || op == mc:branch_nequal) &&
                   (simple_equal?(car(args)) || simple_equal?(cadr(args))))
            // change into regular == or !=
            ins[mc:i_bop] += mc:branch_eq - mc:branch_equal
          else if (op >= mc:branch_eq && op <= mc:branch_gt)
            fold_length_branch(il, previl, ins, op);
	]
      else if (class == mc:i_call)
        [
          | fun, args |
          @(fun . args) = ins[mc:i_cargs];
          if (fun[mc:v_class] == mc:v_global_constant
              && any_primitive?(fun = global_value(fun[mc:v_kvalue]))
              && (primitive_flags(fun) & OP_CONST)
              && lforall?(fn (v) v[mc:v_class] == mc:v_constant, args))
            [
              | result, got_error |
              args = list_to_vector(lmap(fn (v) v[mc:v_kvalue], args));
              if (callable?(fun, vlength(args)))
                result = trap_error(fn () apply(fun, args),
                                    fn (n) got_error = n,
                                    call_trace_off,
                                    true)
              else
                got_error = error_wrong_parameters;

              | prevline |
              prevline = mc:lineno;
              mc:lineno = il[mc:il_lineno];
              if (got_error != null)
                [
                  mc:warning("call to %s() causes %s error",
                             function_name(fun), error_messages[got_error]);
                  ins = mc:make_trap_ins(mc:trap_always, got_error, null);
                ]
              else
                [
                  result = check_immutable(result);
                  if (mc:verbose >= 3)
                    mc:warning("fold const call %s() -> %w",
                               function_name(fun), result);
                  assert(immutable?(result) && readonly?(result));
                  ins = mc:make_compute_ins(
                    mc:b_assign, ins[mc:i_cdest],
                    list(mc:var_make_constant(result)));
                ];
              mc:lineno = prevline;
              il[mc:il_ins] = ins;
              change = true
            ]
        ];
    ];

  // return true if all uses were replaced
  replace_use = fn (use, dvar, repvar, closure_ok?)
    [
      | ins, class, rep, replist |

      rep = fn (v) if (v == dvar) repvar else v;
      replist = fn (s)
        while (s != null)
          [
            if (car(s) == dvar) set_car!(s, repvar);
            s = cdr(s);
          ];

      ins = use[mc:il_ins];
      if (ins == null) exit<function> 0; // instruction is gone

      class = ins[mc:i_class];

      if (mc:verbose >= 3)
        dformat("COPY %s to %s in %s\n",
                mc:svar(dvar), mc:svar(repvar), use[mc:il_number]);
      change = true;

      if (class == mc:i_compute)
        replist(ins[mc:i_aargs])
      else if (class == mc:i_branch)
        replist(ins[mc:i_bargs])
      else if (class == mc:i_trap)
        replist(ins[mc:i_targs])
      else if (class == mc:i_memory)
        [
          ins[mc:i_marray] = rep(ins[mc:i_marray]);
          ins[mc:i_mindex] = rep(ins[mc:i_mindex]);
          if (ins[mc:i_mop] == mc:memory_write ||
              ins[mc:i_mop] == mc:memory_write_safe)
            ins[mc:i_mscalar] = rep(ins[mc:i_mscalar])
        ]
      else if (class == mc:i_call)
        [
          replist(ins[mc:i_cargs]);
          if (car(ins[mc:i_cargs]) == repvar)
            mc:inline_builtin_call(use);
        ]
      else if (class == mc:i_vref)
        ins[mc:i_varg] = rep(ins[mc:i_varg])
      else if (class == mc:i_return)
        ins[mc:i_rvalue] = rep(ins[mc:i_rvalue])
      else if (class == mc:i_closure && closure_ok?)
        [
          | clvar, func |

          func = ins[mc:i_ffunction];

          clvar = lexists?(fn (var) [
            if (var[mc:v_cparent] == dvar)
              var
            else
              false;
          ], func[mc:c_fclosure]);

          if (!clvar) exit<function> null;

          assert(propagate_closure_constant(func, clvar, repvar, replace_use));
          func[mc:c_fclosure] = ldelete!(clvar, func[mc:c_fclosure]);
        ]
      else
        fail();

      true
    ];

  // return true if all uses were replaced
  replace_fn_use = fn (use, dvar, repvar, closure_ok?)
    [
      assert(closure_ok?);

      | ins, class |

      ins = use[mc:il_ins];
      if (ins == null) exit<function> false; // instruction is gone

      class = ins[mc:i_class];

      if (class == mc:i_closure)
        [
          | clvar, func |

          func = ins[mc:i_ffunction];

          clvar = lexists?(fn (var) [
            if (var[mc:v_cparent] == dvar)
              var
            else
              false;
          ], func[mc:c_fclosure]);

          if (!clvar) exit<function> true;

          if (!propagate_closure_constant(func, clvar, repvar, replace_fn_use))
            exit<function> false;

          func[mc:c_fclosure] = ldelete!(clvar, func[mc:c_fclosure]);
          exit<function> true;
        ];

      if (class == mc:i_call)
        [
          | fvar, fun |
          fvar = car(ins[mc:i_cargs]);
          if (fvar == dvar)
            [
              if (pair?(ins[mc:i_cfunction]))
                set_car!(ins[mc:i_cfunction], repvar)
              else
                ins[mc:i_cfunction] = repvar;
            ]
          else if (fvar[mc:v_class] == mc:v_global_constant
                   && (any_primitive?(fun = global_value(fvar[mc:v_goffset]))
                       && (primitive_flags(fun) & OP_APPLY)))
            [
              | fidx |
              @[_ fidx _] = vexists?(fn (v) v[0] == fun, mc:apply_functions);
              if (dvar == nth(fidx + 2, ins[mc:i_cargs]))
                [
                  if (!pair?(ins[mc:i_cfunction]))
                    ins[mc:i_cfunction] = ins[mc:i_cfunction] . null;
                  set_cdr!(ins[mc:i_cfunction], repvar)
                ];
            ];
        ];
      false
    ];

  | for_ils |
  for_ils = fn (ifn, f)
    graph_nodes_apply(fn (n) dforeach(f, graph_node_get(n)[mc:f_ilist]),
                      cdr(ifn[mc:c_fvalue]));

  propagate_closure_constant = fn (ifn, dvar, repvar, replacer)
    [
      | ndvar, change_any, change_all |
      ndvar = dvar[mc:v_number];

      change_any = false;
      change_all = true;
      for_ils(ifn, fn (il) [
        if (bit_set?(il[mc:il_arguments], ndvar))
          [
            if (replacer(il, dvar, repvar, true))
              change_any = true
            else
              change_all = false
          ]
      ]);

      if (change_any)
        if (ifn == car(remaining_fns))
          change = true
        else if (!lfind?(ifn, remaining_fns))
          remaining_fns = car(remaining_fns) . ifn . cdr(remaining_fns);

      change_all
    ];

  | is_set_null?, find_set_null_ins |
  is_set_null? = fn (ins)
    if (ins[mc:i_class] == mc:i_compute
        && ins[mc:i_aop] == mc:b_assign)
      [
        | var |
        var = car(ins[mc:i_aargs]);
        (var[mc:v_class] == mc:v_constant
         && var[mc:v_kvalue] == null)
      ]
    else
      false;

  find_set_null_ins = fn (ifn)
    [
      | insmap |
      insmap = make_vector(ifn[mc:c_fnvars]);
      lforeach(fn (var) [
        | uses |
        uses = var[mc:v_lclosure_uses];
        if (uses & ~(mc:local_write_once | mc:local_write_many
                     | mc:closure_read))
          exit<function> null;
        insmap[var[mc:v_number]] = false;
      ], ifn[mc:c_flocals_write]);

      | vmap |
      vmap = ifn[mc:c_fallvars];
      for_ils(ifn, fn (il) [
        | vnum |
        vnum = il[mc:il_defined_var];
        if (insmap[vnum] == null)
          exit<function> null;

        // try to find a set-to-not-null instruction if
        // local_write_many (implies !local_write_many_nonnull)
        if (!insmap[vnum]
            || (~vmap[vnum][mc:v_lclosure_uses] & mc:local_write_many)
            || !is_set_null?(il[mc:il_ins]))
          insmap[vnum] = il;
      ]);

      insmap
    ];

  // Constant-propagates single-assignment locals and that are read from
  // closures
  propagate_closure_constants = fn (ifn)
    [
      | vars, vars_set_not_null |
      vars_set_not_null = find_set_null_ins(ifn);

      lforeach (fn (var) [
        if (var[mc:v_class] != mc:v_local)
          exit<function> null;

        | dins, vnum, value, uses, replacer |

        // Only propagate if the variable is closure_read.
        uses = var[mc:v_lclosure_uses];
        // For "regular" constants, propagate local_write_once.
        // For closures, propagate unless local_write_many_nonnull.
        if (uses & ~(mc:local_write_once | mc:local_write_many
                     | mc:closure_read))
          exit<function> null;

        vnum = var[mc:v_number];
        dins = vars_set_not_null[vnum][mc:il_ins];

        replacer = replace_use;
        if (dins[mc:i_class] == mc:i_closure)
          [
            replacer = replace_fn_use;
            value = dins[mc:i_ffunction];
          ]
        else if ((uses & mc:local_write_many)
                 || (~uses & mc:closure_read)
                 || dins[mc:i_class] != mc:i_compute
                 || dins[mc:i_aop] != mc:b_assign)
          exit<function> null
        else
          [
            value = car(dins[mc:i_aargs]);
            if (value[mc:v_class] != mc:v_constant
                && value[mc:v_class] != mc:v_global_constant)
              exit<function> null;
          ];

        vars = vector(var, replacer, value) . vars;
      ], ifn[mc:c_flocals_write]);

      if (vars == null) exit<function> false;

      lforeach (fn (pvar) [
        | dvar, replacer, repvar |

        @[dvar replacer repvar] = pvar;

        if (propagate_closure_constant(ifn, dvar, repvar, replacer))
          dvar[mc:v_lclosure_uses] &= ~(mc:closure_read | mc:closure_write);
      ], vars);

      false
    ];

  propagate_copies = fn (f)
    // Misses copies that are born and die within a basic block
    // This would not seem to be a great problem, as phase2 generates an
    // extra temporary for assignments that stops this being a problem,
    // except for ambiguous variables (closures, globals), for which the
    // extra step is necessary anyway.
    [
      | propagate_copy, all_copies, globals |

      all_copies = mc:flow_copies(f);
      globals = mc:set_vars!(mc:new_varset(f), f[mc:c_fglobals]);
      mc:set_vars!(globals, f[mc:c_fclosure]);

      propagate_copy = fn (ncopy)
	[
	  | cblock, dvar, cins, repvar, cfirst, uses, local_uses, nrepvar,
	    replaceable_use?, can_repall, scan, ndvar, umap, copy, repuse |

          repuse = fn (use) replace_use(use, dvar, repvar, false);

	  replaceable_use? = fn (use)
	    [
	      | ublock, uins, first, uclass, ambvars |

	      uins = use[mc:il_ins];
	      if (uins == null)
                // instruction was removed...
                exit<function> true;

	      uclass = uins[mc:i_class];
	      ublock = graph_node_get(use[mc:il_node]);
	      first = ublock[mc:f_ilist];
	      ambvars = ublock[mc:f_ambiguous_w][mc:flow_out];

	      // Note: can't replace use in closures
	      if (uclass != mc:i_closure &&
                  uclass != mc:i_vref &&
		  bit_set?(ublock[mc:f_copies][mc:flow_in], ncopy) &&
		  !((uclass == mc:i_return ||
		     uclass == mc:i_call) && // escapes from function ?
		    (bit_set?(ambvars, ndvar) || bit_set?(globals, ndvar))))
		loop		// there must be no prior replacements
		  [
		    | fil, fins, fclass |

		    fil = dget(first);
		    fins = fil[mc:il_ins];
		    fclass = fins[mc:i_class];
		    if (fil == use) exit true;
		    // check if dvar or repvar is defined in this ins
		    // (no need to check explicit def of dvar, as in that case
		    // dvar would not be in 'uses').
		    if (fil[mc:il_defined_var] == nrepvar ||
			(fclass == mc:i_call && mc:call_escapes?(fins)
                         && (bit_set?(ambvars, nrepvar)
                             || bit_set?(globals, nrepvar)
                             || bit_set?(ambvars, ndvar)
                             || bit_set?(globals, ndvar))))
		      exit false;

		    first = dnext(first);
		  ]
	      else
		false
	    ];

	  copy = all_copies[ncopy];
	  cblock = graph_node_get(copy[mc:il_node]);
	  cins = copy[mc:il_ins];
	  dvar = cins[mc:i_adest];
	  ndvar = dvar[mc:v_number];
	  repvar = car(cins[mc:i_aargs]);
	  nrepvar = repvar[mc:v_number];
	  umap = cblock[mc:f_uses][mc:flow_map];

	  // Find all uses of this copy
	  // we know that the copy statement defines all uses of x
	  // which are available at the end of its block (otherwise
	  // it would not be a member of all_copies)

	  uses = breduce(fn (nuse, l)
			 [
			   | use |

			   use = umap[nuse];
			   if (car(use) == dvar) cdr(use) . l
			   else l
			 ], null, cblock[mc:f_uses][mc:flow_out]);

	  // add all local uses (in block of copy)
	  cfirst = scan = cblock[mc:f_ilist];
	  while (dget(scan) != copy) scan = dnext(scan);

	  can_repall = true;
	  // local uses may make complete replacement impossible
	  loop
	    [
	      | il, ins, class |

	      scan = dnext(scan);
	      if (scan == cfirst) exit 0;

	      il = dget(scan);
	      ins = il[mc:il_ins];
	      class = ins[mc:i_class];
	      if ((class == mc:i_closure || class == mc:i_vref) &&
		  bit_set?(il[mc:il_arguments], ndvar))
                can_repall = false
	      else if (((class == mc:i_return ||
			 class == mc:i_call) && // escapes from function ?
			(bit_set?(cblock[mc:f_ambiguous_w][mc:flow_out],
                                  ndvar) ||
			 bit_set?(globals, ndvar))))
		[
		  can_repall = false;
		  exit 0		// and no further uses are legal
		]
	      else if (bit_set?(il[mc:il_arguments], ndvar))
		local_uses = il . local_uses;
	    ];

	  // if repvar is a constant, propagate it anywhere it reaches
	  if (repvar[mc:v_class] == mc:v_constant)
	    [
	      lforeach(fn (use) if (replaceable_use?(use)) repuse(use), uses);
	      lforeach(repuse, local_uses);
	    ]
	  else if (can_repall && lforall?(replaceable_use?, uses))
	    [
              // we can replace; replace all uses of dvar in uses by repvar
	      lforeach(repuse, uses);
	      lforeach(repuse, local_uses);
	    ]
	];

      for (|i| i = 0; i < vector_length(all_copies); ++i)
        propagate_copy(i)
    ];

  consttype = fn (arg)
    if (arg[mc:v_class] == mc:v_constant)
      typeof(arg[mc:v_kvalue])
    else if (arg[mc:v_class] == mc:v_global_constant)
      typeof(global_value(arg[mc:v_goffset]))
    else
      -1;

  convert_to_type_trap = fn (il, arg, type)
    [
      il[mc:il_ins] = mc:make_trap_ins(mc:trap_type, error_bad_type,
                                       list(arg, mc:var_make_constant(type)));
      il[mc:il_defined_var] = false;
      set_bit!(bclear(il[mc:il_arguments]), arg[mc:v_number]);
      change = true
    ];

  // Checks if "ins" has to be replaced by a type trap if removed by
  // the optimiser. If so, (maybe) changes the instruction and returns true.
  check_compute_trap = fn (il)
    [
      | ttype, cargs, op, ins |

      ins = il[mc:il_ins];

      if (ins[mc:i_class] != mc:i_compute || lfind?(ins, really_useless))
        exit<function> false;

      op = ins[mc:i_aop];
      if (op != mc:b_assign)
        mc:warning("result of operation (%s) unused%s",
                   mc:builtin_names[op],
                   match (ins[mc:i_adest][mc:v_name]) [
                     "" => "";
                     s => format(" (in %s)", s)
                   ]);

      ttype = compute_trap_types[op];
      if (!ttype)
        exit<function> false;

      cargs = ins[mc:i_aargs];

      if (op == mc:b_add)
        [
          | ct |
          if ((ct = consttype(car(cargs))) >= 0)
            if (ct == type_integer || ct == type_string)
              convert_to_type_trap(il, cadr(cargs), ct)
            else
              convert_to_type_trap(il, car(cargs), type_integer)
          else if ((ct = consttype(cadr(cargs))) >= 0)
            if (ct == type_integer || ct == type_string)
              convert_to_type_trap(il, car(cargs), ct)
            else
              convert_to_type_trap(il, cadr(cargs), type_integer)
          // else fallthrough
        ]
      else if (cdr(cargs) == null)              // unary op?
        if (consttype(car(cargs)) == ttype)
          exit<function> false
        else
          convert_to_type_trap(il, car(cargs), ttype)
      else if (consttype(car(cargs)) == ttype)
        if (consttype(cadr(cargs)) == ttype)
          exit<function> false
        else
          convert_to_type_trap(il, cadr(cargs), ttype)
      else if (consttype(cadr(cargs)) == ttype)
        convert_to_type_trap(il, car(cargs), ttype);
      // otherwise, we have to test both arguments; can just as well
      // do the real operation...

      if (mc:verbose >= 3)
        [
          dformat("Trap prevent USELESS: %w\n", ins);
        ];

      true
    ];

  useless_instructions = fn (ifn, globals)
    [
      | useless, closure_uses |

      closure_uses = mc:new_varset(ifn);
      lforeach(fn (var) [
        if (var[mc:v_class] == mc:v_local &&
            (var[mc:v_lclosure_uses] & (mc:closure_read | mc:closure_write)))
          set_bit!(closure_uses, var[mc:v_number]);
      ], ifn[mc:c_flocals]);

      graph_nodes_apply
	(fn (n)
	 [
	   | block, uses, ilist, scan, defined, amblist, local_uses, vmap,
             rw_amblist |

	   block = graph_node_get(n);
	   uses = bitset_to_list(block[mc:f_uses][mc:flow_out],
				 block[mc:f_uses][mc:flow_map]);
	   ilist = block[mc:f_ilist];

	   // Precompute ambiguous information for each instruction,
	   // in reverse order
	   amblist = mc:build_ambiguous_list(block, globals, mc:f_ambiguous_w);

	   rw_amblist = mc:build_ambiguous_list(block, globals,
                                                mc:f_ambiguous_rw);

	   defined = mc:new_varset(ifn);
	   vmap = ifn[mc:c_fallvars];
	   scan = ilist;

           local_uses = bcopy(closure_uses);
           bintersection!(local_uses, block[mc:f_ambiguous_rw][mc:flow_out]);

	   loop
	     [
	       | il, ins, class, ndvar |

	       scan = dprev(scan);
	       il = dget(scan);
	       ins = il[mc:il_ins];
	       class = ins[mc:i_class];

               mc:lineno = il[mc:il_lineno];

	       ndvar = il[mc:il_defined_var];

	       if (class == mc:i_branch &&
		   ins[mc:i_bop] == mc:branch_never ||

		   class == mc:i_trap &&
		   ins[mc:i_top] == mc:trap_never ||

		   (class == mc:i_compute && // a = a is useless
		    ins[mc:i_aop] == mc:b_assign &&
		    ins[mc:i_adest] == car(ins[mc:i_aargs])) ||

		   (class == mc:i_compute ||
                    class == mc:i_closure ||
		    class == mc:i_memory && ins[mc:i_mop] == mc:memory_read) &&
		   bit_clear?(local_uses, ndvar) &&
		   (bit_set?(defined, ndvar) || !assq(vmap[ndvar], uses)))
                 if (!check_compute_trap(il))
                   useless = scan . useless;

	       if (ndvar)
		 [
		   set_bit!(defined, ndvar);
		   clear_bit!(local_uses, ndvar);
		 ];

	       if (class != mc:i_closure)
                 bunion!(local_uses, mc:barguments(il, car(amblist)));

               <skip> [
                 | amb |
                 if (class == mc:i_return)
                   amb = car(rw_amblist)
                 else if (class != mc:i_call)
                   exit<skip> null
                 else if (mc:call_escapes?(ins))
                   amb = car(rw_amblist)
                 else
                   amb = globals;
                 bunion!(local_uses, amb)
               ];

	       if (scan == ilist) exit 0;

               rw_amblist = cdr(rw_amblist);
	       amblist = cdr(amblist);
	     ];
	 ], cdr(ifn[mc:c_fvalue]));
      useless
    ];

  remove_instruction = fn (f, ilpos)
    [
      | il, block, first, nblock, olabel |

      il = dget(ilpos);
      il[mc:il_ins] = null; // note removal of instruction
      block = il[mc:il_node];
      first = graph_node_get(block)[mc:f_ilist];

      if (dnext(ilpos) != ilpos) // remains more than one instruction
	[
	  if (ilpos == first) // move second instruction up
	    [
	      | second, il2 |

	      second = dnext(first);
              il2 = dget(second);
              for (|i| i = vlength(il2); --i >= 0; )
                if (i != mc:il_label)
                  il[i] = il2[i];
              il2[mc:il_ins] = null; // note removal
	      dremove!(second, second)
	    ]
	  else
	    dremove!(ilpos, ilpos)
	]
      else
	[
	  // find fall through block
	  graph_edges_out_apply(fn (e) if (graph_edge_get(e))
				nblock = graph_edge_to(e),
				block);
	  assert(nblock != null);

	  // if block was entry block, change
	  if (car(f[mc:c_fvalue]) == block)
	    set_car!(f[mc:c_fvalue], nblock);

	  // edges into block becomes edges into nblock
	  lforeach(fn (e)
		   [
		     graph_add_edge(graph_edge_from(e), nblock,
                                    graph_edge_get(e));
		     graph_remove_edge(e)
		   ], graph_edges_in(block));

	  // set label (if any) to first instruction of nblock
	  if (olabel = il[mc:il_label])
	    mc:set_label(olabel, dget(graph_node_get(nblock)[mc:f_ilist]));
	]
    ];

  eliminate_dead_code = fn (f)
    [
      | fg, entry, useless, fval |

      // assumes that edges removed because of branch changes
      // have already been handled

      // remove:
      //   useless branches
      //   useless traps
      //   unused definitions

      // as a result:
      //   some labels may become unused
      //   some blocks may be mergeable (not done for now)
      //   some blocks may disappear (no instructions left)
      //   some blocks may be unreachable

      fval = f[mc:c_fvalue];
      fg = cdr(fval);

      useless = useless_instructions
	(f, mc:set_vars!(mc:set_vars!(mc:new_varset(f),
                                      f[mc:c_fglobals_write]),
			 f[mc:c_fclosure_write]));
      if (useless != null)
	[
	  if (mc:verbose >= 3)
	    [
	      dformat("USELESS %s\n",
                      concat_words(lmap(fn (i) itoa(dget(i)[mc:il_number]),
                                        useless),
                                   " "));
	    ];
	  change = true;
	];
      lforeach(fn (ins) remove_instruction(f, ins), useless);

      entry = car(fval);
      // remove useless nodes (never the entry node!)
      lforeach(fn (n) if (n != entry && graph_edges_in(n) == null)
	       [
		 // unreachable
		 change = true;
		 if (mc:verbose >= 3)
		   [
		     display("UNREACHABLE\n");
		   ];
		 lforeach(fn (e) graph_remove_edge(e), graph_edges_out(n));
		 graph_remove_node(n)
	       ], graph_nodes(fg));

      // remove aliases and useless labels
      graph_nodes_apply(
        fn (n) mc:remove_aliases(graph_node_get(n)[mc:f_ilist]), fg);
      graph_nodes_apply(
        fn (n) mc:remove_labels(graph_node_get(n)[mc:f_ilist]), fg);
    ];

  mc:recompute_vars = fn (ifn, dglobals?)
    // Types: ifn : intermediate function, dglobals?: boolean
    // Effects: Recomputes the variable lists (locals, closures, globals) of
    //   ifn (may have changed because of optimisation)
    //   Also renumbers these variables from 1 (for use in bitmasks)
    //   Numbers dglobals if dglobals? is true
    [
      | locals, closures, globals, vars_read, vars_written, args_ins,
        ins_escapes?, allvars,
	defined, wlocals, wnonnulllocals, wclosures, wglobals, vindex,
        dglobals_read, add_var!, var_set? |

      // set bit for var in varset, expanding varset as necessary;
      // returns the possibly updated varset
      add_var! = fn (varset, var)
        [
          allvars = var . allvars;
          | sz, n |
          // temporarily use negatives to indicate actually found this
          // time around
          var[mc:v_number] = -(n = ++vindex);
          sz = slength(varset) * 8;
          if (n >= sz)
            loop
              [
                sz = if (sz) sz * 2 else 16;
                if (n < sz)
                  [
                    | newset |
                    newset = new_bitset(sz);
                    for (|i| i = slength(varset); --i >= 0; )
                      newset[i] = varset[i];
                    exit varset = newset;
                  ]
              ];
          set_bit!(varset, n);
          varset
        ];

      var_set? = fn (varset, var)
        [
          | n |
          n = var[mc:v_number];
          if (n >= 0)
            exit<function> false;
          n = -n;
          n < slength(varset) * 8 && bit_set?(varset, n)
        ];

      // n.b., these may be too small (empty) if we have never counted
      // variables before
      wglobals = mc:new_varset(ifn);
      wlocals = mc:new_varset(ifn);
      wclosures = mc:new_varset(ifn);

      vars_written = fn (il)
	[
	  | ins, dvar, class |

	  ins = il[mc:il_ins];

	  dvar = mc:defined_var(ins);
	  if (dvar)
	    [
	      class = dvar[mc:v_class];
	      if (class == mc:v_global)
		[
		  if (!var_set?(wglobals, dvar))
                    wglobals = add_var!(wglobals, dvar);
		]
	      else if (class == mc:v_local)
		[
		  if (!var_set?(wlocals, dvar))
                    [
                      | ouse |
                      wlocals = add_var!(wlocals, dvar);
                      ouse = dvar[mc:v_lclosure_uses];
                      ouse &= ~(mc:local_write_once | mc:local_write_many
                                | mc:local_write_many_nonnull);

                      if (lfind?(dvar, ifn[mc:c_fargs]))
                        dvar[mc:v_lclosure_uses]
                          = (ouse | mc:local_write_many
                             | mc:local_write_many_nonnull)
                      else
                        dvar[mc:v_lclosure_uses] = ouse | mc:local_write_once;
                    ]
                  else
                    dvar[mc:v_lclosure_uses] |= mc:local_write_many;

                  if ((~dvar[mc:v_lclosure_uses] & mc:local_write_many_nonnull)
                      && !is_set_null?(ins))
                    if (!memq(dvar, wnonnulllocals))
                      wnonnulllocals = dvar . wnonnulllocals
                    else
                      dvar[mc:v_lclosure_uses] |= mc:local_write_many_nonnull;
		]
	      else if (class == mc:v_closure)
		[
		  if (!var_set?(wclosures, dvar))
                    wclosures = add_var!(wclosures, dvar);
		];

	      il[mc:il_defined_var] = -dvar[mc:v_number];
	    ]
	  else
	    il[mc:il_defined_var] = false;

	];

      vars_read = fn (il)
	[
	  | ins, args |

	  ins = il[mc:il_ins];

	  args = mc:arguments(ins, null);
	  lforeach(fn (v)
		   [
		     | class |

		     class = v[mc:v_class];
		     if (class == mc:v_global)
		       [
			 if (!var_set?(globals, v))
                           globals = add_var!(globals, v);
		       ]
		     else if (class == mc:v_local)
		       [
			 if (!var_set?(locals, v))
                           locals = add_var!(locals, v);
		       ]
		     else if (class == mc:v_closure)
		       [
			 if (!var_set?(closures, v))
                           closures = add_var!(closures, v);
		       ]
                     else
                       v[mc:v_number] = 0
		   ], args);
	  il[mc:il_arguments] = args;
	];

      dglobals_read = fn (il)
	lforeach(fn (v)
		 if (v[mc:v_class] == mc:v_global_define
		     && !var_set?(globals, v))
                 globals = add_var!(globals, v),
		 il[mc:il_arguments]);

      args_ins = fn (il)
	[
	  | args, ndvar |

	  args = mc:new_varset(ifn);
	  il[mc:il_arguments] = mc:set_vars!(args, il[mc:il_arguments]);
	  clear_bit!(args, 0); // remove ref to constants

	  if (ndvar = il[mc:il_defined_var])
	    set_bit!(defined, ndvar);
	];

      ins_escapes? = fn (il)
        [
          | ins |
          ins = il[mc:il_ins];
          (ins[mc:i_class] == mc:i_call && mc:call_escapes?(ins))
        ];

      // clear the index of argument variables (so that unused arguments
      // are easily ignored)
      lforeach(fn (arg) arg[mc:v_number] = 0, ifn[mc:c_fargs]);

      // first find all variables that are written
      // then prepend those that are read (but not written)
      // the whole list, and the sublist of written vars are saved
      vindex = 0;

      for_ils(ifn, vars_written);

      locals = bcopy(wlocals);
      closures = bcopy(wclosures);
      globals = bcopy(wglobals);
      for_ils(ifn, vars_read);

      if (dglobals?)
        for_ils(ifn, dglobals_read);

      lforeach(fn (var) var[mc:v_number] = -var[mc:v_number], allvars);
      ifn[mc:c_fallvars] = allvars = list_to_vector(null . lreverse!(allvars));

      ifn[mc:c_flocals] = bitset_to_list(locals, allvars);
      ifn[mc:c_fglobals] = bitset_to_list(globals, allvars);
      ifn[mc:c_fclosure] = bitset_to_list(closures, allvars);
      ifn[mc:c_flocals_write] = bitset_to_list(wlocals, allvars);
      ifn[mc:c_fglobals_write] = wglobals = bitset_to_list(wglobals, allvars);
      ifn[mc:c_fclosure_write] = wclosures
        = bitset_to_list(wclosures, allvars);
      ifn[mc:c_fnvars] = vindex + 1;

      | escapes |
      escapes = wclosures != null || wglobals != null;
      graph_nodes_apply
	(fn (n)
	 [
	   | block |

	   block = graph_node_get(n);
           block[mc:f_dvars] = defined = mc:new_varset(ifn);
	   dforeach(args_ins, block[mc:f_ilist]);
           escapes ||= dexists?(ins_escapes?, block[mc:f_ilist]);
	 ], cdr(ifn[mc:c_fvalue]));
      ifn[mc:c_fnoescape] = !escapes;
    ];

  optimise_function = fn (f)
    [
      really_useless = null;

      mc:this_function = f;
      if (mc:verbose >= 2)
	[
	  dformat("Optimising %s\n", mc:fname(f));
	];
      change = true;
      while (change)
	<continue> [
	  change = false;

	  // fold constants in all basic blocks
	  graph_nodes_apply(fn (n) [
            | this, prev, ilist |
            ilist = graph_node_get(n)[mc:f_ilist];
            if (ilist == null) exit<function> null;
            this = ilist;
            loop
              [
                | il |
                il = dget(this);
                fold_constants(il, prev);
                prev = il;
                this = dnext(this);
                if (this == ilist) exit null;
              ];
          ], cdr(f[mc:c_fvalue]));

	  // compute basic information
	  mc:recompute_vars(f, false);
          if (propagate_closure_constants(f))
            [
              change = true;
              exit<continue> null;
            ];

	  mc:flow_ambiguous(f, mc:f_ambiguous_w);
	  mc:flow_ambiguous(f, mc:f_ambiguous_rw);
	  mc:flow_uses(f);

	  eliminate_dead_code(f);
	  propagate_copies(f);
	];

      mc:flow_sizes(f);

      // clear data-flow information
      mc:clear_dataflow(f);
      mc:this_function = null;
    ];

  mc:optimise_functions = fn (fns)
    [
      remaining_fns = fns;
      while (remaining_fns != null)
        [
          optimise_function(car(remaining_fns));
          remaining_fns = cdr(remaining_fns);
        ];
    ];

];
