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

library phase4 // Phase 4: code generation
requires compiler, flow, graph, ins3, misc, mp, optimise, phase3, sequences,
  vars
defines mc:phase4
reads mc:verbose, mc:disassemble
[
  | clear_igraph, make_igraph, allocate_registers, cgen_function, cgen_code |


  clear_igraph = fn (vars)
    // Effects: Removes all references to the interference graph
    //  from vars
    [
      | remvar |

      remvar = fn (v) v[mc:v_neighbours] = null;
      lforeach(remvar, vars);
    ];

  make_igraph = fn (ifn)
    [
      | vars, map, add_interferences |

      map = ifn[mc:c_fallvars];
      // Make a neighbour bitset for every variable (except for 'myself')
      vars = lappend(lfilter(fn (cvar) cvar[mc:v_cparent] != mc:myself,
			     ifn[mc:c_fclosure]),
		     ifn[mc:c_flocals]);

      lforeach(fn (v) v[mc:v_neighbours] = mc:new_varset(ifn), vars);

      // Then set bits in neighbours for variables that are simultaneously
      // live. (these represent flow graph edges)

      add_interferences = fn (ins, live_in, live_out, x)
	// add edges between all variables in live_out
	bforeach(fn (nv1)
		 [
		   | neighbours |

		   neighbours = map[nv1][mc:v_neighbours];
		   if (neighbours != null)
		     bunion!(neighbours, live_out)
		 ], live_out);

      graph_nodes_apply(fn (n)
			[
			  | block |

			  block = graph_node_get(n);
			  mc:rscan_live(add_interferences, null, block);
			  add_interferences(null, null,
                                            block[mc:f_live][mc:flow_in],
                                            null);
			],
			cdr(ifn[mc:c_fvalue]));

      lforeach(fn (v) clear_bit!(v[mc:v_neighbours], v[mc:v_number]), vars);

      vars
    ];

  allocate_registers = fn (ifn)
    // Types: ifn: intermediate function with flow graph
    // Effects: Allocates registers for the variables of ifn. Adds spills if
    //   necessary.
    [
      // Note that a lot of values start off spilled:
      //   arguments beyond the 3rd (except if function has 4 args)
      //   closure variables
      // These should be spilled first if necessary
      // To be considered: variables that are only used once should not be
      // unspilled if they start that way. 1st approximation: count static
      // uses (fails for loops).

      | groups, no, nob, spiltargs, ncallee, ncaller, nscratch, nregargs,
	spill, easy_spill, changes, color_graph, color_order, ainfo,
	notspilt, spilt, temps, locals, map, vars, group_variables,
        select_colors, select_spill, localsb,
        vg_notspilt, vg_spilt, vg_local, vg_temp |

      vg_notspilt = 0;
      vg_spilt    = 1;
      vg_local    = 2;
      vg_temp     = 3;

      group_variables = fn (vars)
	[
	  | notspilt, spilt, temps, locals, group, bvars |

	  group = fn (il, live_in, live_out, x)
	    [
	      | ins, class, dvar |

	      ins = il[mc:il_ins];
              if (!mp:uses_scratch?(ins))
                exit<function> null;

	      class = ins[mc:i_class];
              dvar = il[mc:il_defined_var];

	      if (class == mc:i_call
                  || (class == mc:i_branch
                      && (ins[mc:i_bop] == mc:branch_equal
                          || ins[mc:i_bop] == mc:branch_nequal))
                  || (class == mc:i_trap
                      && (ins[mc:i_top] == mc:trap_global_write
                          || ins[mc:i_top] == mc:trap_global_read)))
		[
		  | survive_call |

		  // everything live after the call (except the result)
		  // belongs either in notspilt or in spilt
                  survive_call = live_out;
                  if (dvar && bit_set?(survive_call, dvar))
                    clear_bit!(survive_call = bcopy(survive_call), dvar);

		  bdifference!(temps, survive_call);
		  bdifference!(locals, survive_call);
		  bforeach
		    (fn (nlive)
		       set_bit!(if (map[nlive][mc:v_class] == mc:v_closure ||
				    bit_set?(spiltargs, nlive)) // already spilt
				  spilt
		                else
				  notspilt,
				nlive),
		     survive_call);
		]
	      else
		[
		  | survives |

		  // if live on the way in & out then can't be temp
		  // this misses some possible scratch vars (and
		  // makes register allocation for them rather pointless),
		  // but is the simplest test

		  // operations that imply allocation use scratch regs
                  if (class == mc:i_closure
                                 || (class == mc:i_compute
                                     && (match (ins[mc:i_aop]) [
                                       ,mc:b_vector || ,mc:b_sequence => true
                                     ])))
                    [
                      survives = live_in;
                      // test before making an otherwise-unnecessary copy
                      if (dvar && bit_set?(live_in, dvar))
                        clear_bit!(survives = bcopy(live_in), dvar)
                    ]
		  else
                    [
                      survives = bintersection(live_in, live_out);
                      if (dvar)
                        clear_bit!(survives, dvar);
                    ];

		  // those temps that survive move to locals
		  bunion!(locals, bintersection(temps, survives));
		  bdifference!(temps, survives);
		]
	    ];

	  // assume everything is a temp, and migrate it as forced
	  notspilt = mc:new_varset(ifn);
	  spilt = bcopy(notspilt);
	  locals = bcopy(notspilt);
	  bvars = mc:set_vars!(bcopy(notspilt), vars);
	  temps = bcopy(bvars);

	  graph_nodes_apply(fn (n) [
            mc:rscan_live(group, null, graph_node_get(n))
          ], cdr(ifn[mc:c_fvalue]));

	  mp:migrate(ifn, vars, notspilt, spilt, locals, temps);

	  // remove extraneous variables
	  bintersection!(notspilt, bvars);
	  bintersection!(spilt, bvars);

	  vector(notspilt, spilt, locals, temps);
	];

      easy_spill = fn (vars, bvars)
	[
	  | v |
	  if (v = lexists?(fn (v)
			   !v[mc:v_location] &&
			   (v[mc:v_class] == mc:v_closure ||
			    bit_set?(spiltargs, v[mc:v_number])),
			   vars))
	    [
	      // spill the already spilt variable
	      v[mc:v_location] = vector(mc:v_lspill,
					if (v[mc:v_class] == mc:v_closure)
					  mc:spill_closure
					else
					  mc:spill_args,
					0); // spill offset not yet selected
	      clear_bit!(bvars, v[mc:v_number]);
	      true
	    ]
	  else
	    false
	];

      spill = fn (vars, bvars)
	[
          // find variable with most neighbours to spill

	  | best, maxneigh, bvars2 |
          bvars2 = bcopy(bvars); // create one copy to save some memory allocation
          maxneigh = -1;

          loop
            [
              | v |
              @(v . vars) = vars;
              if (!v[mc:v_location])
                [
                  | n |
                  n = bcount(bintersection!(bvars2, v[mc:v_neighbours]));
                  if (n > maxneigh)
                    [
                      maxneigh = n;
                      best = v;
                    ];
                ];
              if (vars == null)
                exit null;
              bassign!(bvars2, bvars);
            ];

          if (best == null)
            exit<function> false;

          best[mc:v_location] = vector(mc:v_lspill, mc:spill_spill, 0);
          clear_bit!(bvars, best[mc:v_number]);
          true
        ];

      color_order = vector(0, 0, 0);
      color_graph = fn (vars, bvars, regtype, nregs)
	// Types: vars: list of var
	//        bvars: varset
	//        regtype: mc:reg_xxx
	//        nregs: int
	// Effects: Allocates registers for variables in vars from amongst
	//   nregs available ones. Conflicts between a variable in vars and
	//   one outside are ignored.
	//   The algorithm ignores all nodes that are already allocated
	[
	  | count |

	  count = 0;

	  loop
	    [
	      | v |

	      // look for a node of vars with less than nregs neighbours
	      v = lexists?(fn (v) [
                (!v[mc:v_location]
                 && bcount(bintersection(v[mc:v_neighbours], bvars)) < nregs)
              ], vars);

	      if (!v) exit bempty?(bvars);

	      count = count + 1;
	      changes = true;
	      v[mc:v_location] = vector(mc:v_lregister, regtype,
					color_order[regtype]);
	      clear_bit!(bvars, v[mc:v_number]);
	      color_order[regtype] = color_order[regtype] + 1;
	    ]
	];

      select_colors = fn (vars, regtype, nregs)
	// Types: vars: list of var
	//        regtype: mc:reg_xxx
	//        nregs: int
	// Effects: Selects colors for the variables of the interference
	//   graph, of type regtype. nregs variables of that type are
	//   assumed available.
	//   This function is called once all variables have been allocated
	//   with color_graph or spilled.
	[
	  | ovars, i, nused, allocated |

	  nused = -1;
	  // Find order of variables for given type
	  i = color_order[regtype];
	  ovars = make_vector(i);
	  lforeach(fn (var)
		  [
		    | vloc |

		    vloc = var[mc:v_location];
		    if (vloc[mc:v_lclass] == mc:v_lregister &&
			vloc[mc:v_lrtype] == regtype)
		      ovars[vloc[mc:v_lrnumber]] = var
		  ], vars);

	  // Allocate variables in order from highest to lowest
	  // (reverse of graph-removal order)
	  // nodes are marked once they have been allocated

	  allocated = mc:new_varset(ifn);
	  while (i > 0)
	    [
	      | var, colors, color |

	      var = ovars[i = i - 1];
	      // choose lowest available color
	      colors = make_string(nregs);
	      string_fill!(colors, true);

	      // remove colors used by allocated neighbours
	      bforeach
		(fn (nneighbour)
		   colors[map[nneighbour][mc:v_location][mc:v_lrnumber]] = false,
		 bintersection(allocated, var[mc:v_neighbours]));

	      color = 0;
	      while (!colors[color]) color = color + 1;
	      var[mc:v_location][mc:v_lrnumber] = color;
	      set_bit!(allocated, var[mc:v_number]); // var is now allocated
	      if (color > nused) nused = color;
	    ];

	  nused + 1
	];

      select_spill = fn (vars, maxspill)
	// Types: vars: list of var
	//        maxspill: int
	// Effects: Selects offsets for spilled variables (spill_spill),
	//   with an effort at minimising the number of spill entries
	//   required.
	//   This function is called once all variables have been allocated
	//   with color_graph or spilled.
	[
	  | allocate_spill, nspilled, allocated |

	  nspilled = -1;
	  allocate_spill = fn (var)
	    [
	      | colors, color |

	      colors = make_string(maxspill);
	      string_fill!(colors, true);

	      // remove colors used by allocated neighbours
	      bforeach
		(fn (nneighbour)
		   colors[map[nneighbour][mc:v_location][mc:v_lsoffset]] = false,
		 bintersection(allocated, var[mc:v_neighbours]));

	      color = 0;
	      while (!colors[color]) color = color + 1;
	      var[mc:v_location][mc:v_lsoffset] = color;
	      set_bit!(allocated, var[mc:v_number]); // var is now allocated
	      if (color > nspilled) nspilled = color;
	    ];

	  // Allocate all spilled variables, in an arbitrary order
	  allocated = mc:new_varset(ifn);
	  lforeach(fn (var)
		  [
		    | vloc |

		    vloc = var[mc:v_location];
		    if (vloc[mc:v_lclass] == mc:v_lspill &&
			vloc[mc:v_lrtype] == mc:spill_spill)
		      allocate_spill(var)
		  ], vars);

	  nspilled + 1
	];

      map = ifn[mc:c_fallvars];
      // discover how many registers are available for this function
      ifn[mc:c_fmisc] = vector(false, false, false, false);
      nregargs = mp:nregargs(ifn);
      nscratch = mp:nscratch(ifn);
      ncaller = mp:ncaller(ifn);
      ncallee = mp:ncallee(ifn);

      // first nregargs are in registers
      spiltargs = mc:new_varset(ifn);
      mc:set_vars!(spiltargs, lmap(fn (@[var _ _]) var,
				   nth_pair(nregargs + 1, ifn[mc:c_fargs])));

      vars = make_igraph(ifn);
      // separate variables into 4 groups:
      //   0: notspilt: those that live across procedure calls and are
      //      not spilled
      //   1: spilt: those that live across procedure calls but are spilled
      //   3: temps: those that can live in the scratch registers
      //   2: locals: all the others
      groups = group_variables(vars);
      temps    = bitset_to_list(groups[vg_temp],     map);
      locals   = bitset_to_list(groups[vg_local],    map);
      spilt    = bitset_to_list(groups[vg_spilt],    map);
      notspilt = bitset_to_list(groups[vg_notspilt], map);

      if (mc:verbose >= 3)
	[
	  dformat("AVAILABLE: scratch: %s, caller: %s callee: %s\n",
                  nscratch, ncaller, ncallee);
	];

      // Do scratch registers first, as they should normally all
      // be successful
      if (!color_graph(temps, groups[vg_temp], mc:reg_scratch, nscratch))
	[
	  // if fail, add unallocated temps to locals
	  locals = lappend(lfilter(fn (v) !v[mc:v_location], temps), locals);
	  bunion!(groups[vg_local], groups[vg_temp]);
	];

      localsb = groups[vg_local];
      <allocate_locals> loop
	[
	  loop
	    [
	      changes = false;
	      if (color_graph(locals, localsb, mc:reg_caller, ncaller))
		exit<allocate_locals> 0;
	      if (!changes) exit 0
	    ];

	  // spill somebody, preferably already spilled
	  // beyond that, the heuristic needs much more thought
	  // (e.g. which is better: spill long-lived or short-lived vars ?)
	  if (!easy_spill(locals, localsb))
            spill(locals, localsb)
	];

      // Note: see old-allocate.mud for an idea that doesn't work
      // (summary: try & place locals in callee registers when caller
      // ones all full)
      // (strange things happen and registers go unused ...)
      // This needs further investigation.

      no = lappend(notspilt, spilt);
      nob = bunion(groups[vg_notspilt], groups[vg_spilt]);
      <allocate_others> loop
	[
	  loop
	    [
	      changes = false;
	      if (color_graph(no, nob, mc:reg_callee, ncallee))
		exit<allocate_others> 0;

	      if (!changes) exit 0;
	    ];

	  // spill somebody, preferably already spilled
	  // beyond that, the heuristic needs much more thought
	  // (e.g. which is better: spill long-lived or short-lived vars ?)
	  if (!easy_spill(spilt, nob))
            spill(notspilt, nob)
	];

      ainfo = vector(
        select_colors(vars, mc:reg_scratch, nscratch),
        select_colors(vars, mc:reg_caller,  ncaller),
        select_colors(vars, mc:reg_callee,  ncallee),
        select_spill(vars, llength(ifn[mc:c_flocals])));
      if (mc:verbose >= 3)
	[
	  dformat("USED: scratch: %d, caller: %d callee: %d, spilt %d\n",
                  ainfo[0], ainfo[1], ainfo[2], ainfo[3]);
	];
      clear_igraph(vars);
      ainfo
    ];

  cgen_function = fn (ifn)
    // Types: ifn: intermediate function with flow graph
    // Effects: Generates the actual machine code function for ifn, and stores
    //   it in ifn[mc:c_fvalue]
    [
      | ainfo |

      if (mc:verbose >= 2)
	[
	  dformat("Generating %s\n", mc:fname(ifn));
	];

      mc:recompute_vars(ifn, false);
      mc:flow_live(ifn);

      ainfo = allocate_registers(ifn);
      if (mc:verbose >= 3)
	[
	  dformat("ainfo is %s\n", ainfo);
	];
      //mc:display_blocks(ifn);
      mc:flatten_blocks(ifn);
      cgen_code(ifn, ainfo);
    ];

  cgen_code = fn (ifn, ainfo)
    [
      | code |

      ainfo = mp:select_registers(ifn, ainfo);
      if (mc:verbose >= 3)
	[
	  dformat("selected ainfo is %s\n", ainfo);
	];

      if (mc:verbose >= 4 || mc:disassemble)
	[
	  dformat("Code of function %s(%s)\n",
                  ifn[mc:c_fnumber], mc:fname(ifn));
          if (mc:verbose >= 4)
            [
              mc:ins_list1(ifn[mc:c_fvalue]);
              newline();
            ]
        ];

      code = mp:mgen_preamble(ifn, ainfo);
      dforeach(fn (il)
	         if (il[mc:il_label])
	           il[mc:il_label][mc:l_mclabel] = mp:new_label(code),
	       ifn[mc:c_fvalue]);
      dforeach(fn (il) mp:mgen_instruction(code, ifn, ainfo, il),
               ifn[mc:c_fvalue]);

      if (mc:verbose >= 5)
	[
	  mp:ins_list(code);
	  newline();
	];

      mc:set_loc(ifn[mc:c_loc]);
      ifn[mc:c_fvalue] = mp:assemble(code);
    ];

  mc:phase4 = fn "intermediate -> fn. Generates code for the function" (fns)
    [
      lforeach(cgen_function, fns);
    ];
];
