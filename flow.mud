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

library flow // Data-flow analysis
requires compiler, vars, ins3, graph,
  system, dlist, sequences, misc
defines
  mc:flow_gen, mc:flow_kill, mc:flow_in, mc:flow_out, mc:flow_map,
  mc:flow_ambiguous, mc:scan_ambiguous, mc:build_ambiguous_list,
  mc:flow_uses, mc:flow_copies,
  mc:flow_live, mc:rscan_live, mc:flow_display, mc:clear_dataflow,
  mc:split_blocks, mc:flatten_blocks, mc:display_blocks, mc:f_ilist,
  mc:f_ambiguous_w, mc:f_ambiguous_rw, mc:f_uses, mc:f_copies, mc:f_live,
  mc:f_dvars, mc:f_types, mc:f_sizes, mc:all_functions, mc:flow_sizes

reads mc:show_type_info, mc:verbose

[
  | intersection_predecessors, union_predecessors, union_successors,
    bflow_display, clear_nodes, set_ilist_node, order_nodes, new_block |

  // Flow graph node representation:
  mc:f_ilist        = 0; // the dlist of the ilists
  mc:f_ambiguous_w  = 1; // data-flow: variables which are written from
                         // a closure (list of var)
  mc:f_ambiguous_rw = 2; // data-flow: variables which have escaped into
                         // a closure (list of var)
  mc:f_uses         = 3; // data-flow: definition use information
  mc:f_copies       = 4; // data-flow: copy propagation information
  mc:f_live         = 5; // data-flow: live variable information
  mc:f_dvars        = 6; // varset of vars definitely assigned in block
  mc:f_types        = 7; // data-flow: inferred types
  mc:f_sizes        = 8; // data-flow: min/max sizes of strings and vectors

  // All data-flow problems use the same basic structure:
  //  [0]: generated info
  //  [1]: killed info
  //  [2]: in
  //  [3]: out
  //  [4]: map from bit indexes to actual values (for bitset-based data-flows)

  mc:flow_gen = 0;
  mc:flow_kill = 1;
  mc:flow_in = 2;
  mc:flow_out = 3;
  mc:flow_map = 4;

  | kset, kstring_set, kvector_set, kref, kvector_ref, kstring_ref,
    referencers,
    kmake_vector, kmake_string, kvappend, ksappend, kvcopy, kscopy |
  kset         = mc:make_kglobal("set!");
  kstring_set  = mc:make_kglobal("string_set!");
  kvector_set  = mc:make_kglobal("vector_set!");
  kref         = mc:make_kglobal("ref");
  kstring_ref  = mc:make_kglobal("string_ref");
  kvector_ref  = mc:make_kglobal("vector_ref");
  referencers  = sequence(kset, kstring_set, kvector_set,
                          kref, kstring_ref, kvector_ref);
  kmake_vector = mc:make_kglobal("make_vector");
  kmake_string = mc:make_kglobal("make_string");
  kvappend     = mc:make_kglobal("vappend");
  ksappend     = mc:make_kglobal("sappend");
  kscopy       = mc:make_kglobal("scopy");
  kvcopy       = mc:make_kglobal("vcopy");

  // Part1: data-flow graph creation, destruction, display

  new_block = fn (ilist)
    vector(ilist, false, false, false, false, false, false, false, false);

  mc:clear_dataflow = fn (ifn)
    // Effects: Clears data-flow information from ifn
    // Modifies: ifn
    graph_nodes_apply(fn (n)
		      [
			| block |

			block = graph_node_get(n);
			block[mc:f_ambiguous_w] = block[mc:f_ambiguous_rw]
                          = block[mc:f_uses] = block[mc:f_copies]
                          = block[mc:f_live] = block[mc:f_types] = false;
		      ], cdr(ifn[mc:c_fvalue]));

  // basic block handling

  mc:split_blocks = fn (ifn)
    // Types: ifn : function component
    // Effects: Splits the instructions of ifn into a flow-graph of basic
    //   blocks. ifn[mc:c_fvalue] becomes a pair (first-node . graph)
    [
      | scan, ilist, flow, bnode, previous_bnode, entry_node |

      ilist = ifn[mc:c_fvalue];
      flow = new_graph();
      entry_node = previous_bnode = false; // no previous block

      // scan instructions and build basic blocks
      scan = ilist;
      loop
	[
	  | block, ins, il, ilnext, first |

	  block = dcons!(null, null); // a new block, with placeholder

	  loop
	    [
	      // add instruction to block
	      dcons!(il = dget(scan), block);
	      ins = il[mc:il_ins];

	      // and see if this instruction is a branch,
	      // unconditional trap, or if next instruction has a
	      // label (block end conditions)
	      scan = dnext(scan);
	      if (scan == ilist) exit 0;
	      ilnext = dget(scan);

	      if (ins[mc:i_class] == mc:i_branch ||
                  ilnext[mc:il_label] ||
                  (ins[mc:i_class] == mc:i_trap &&
                   ins[mc:i_top] == mc:trap_always))
                exit 0
	    ];

	  // we have a block, add it to the graph
	  first = dremove!(block, block);
	  bnode = graph_add_node(flow, new_block(first));
	  set_ilist_node(first, bnode);
	  if (!entry_node) entry_node = bnode;

	  // and add an edge from the previous node if:
	  //  - it existed
	  //  - it did not end in an unconditional branch or trap
	  if (previous_bnode) graph_add_edge(previous_bnode, bnode, true);

	  if ((ins[mc:i_class] == mc:i_branch &&
               ins[mc:i_bop] == mc:branch_always)
              || (ins[mc:i_class] == mc:i_trap &&
                  ins[mc:i_top] == mc:trap_always))
            // no edge to next node
	    previous_bnode = false
	  else
	    previous_bnode = bnode;

	  if (scan == ilist) exit 0 // end of function
	];

      // Now add edges for branches in the flow graph
      graph_nodes_apply(fn (node)
			[
			  | last |
			  last = dget(dprev(graph_node_get(node)[mc:f_ilist]))[mc:il_ins];
			  if (last[mc:i_class] == mc:i_branch)
			    graph_add_edge(node, last[mc:i_bdest][mc:l_ilist][mc:il_node], false)
			], flow);

      ifn[mc:c_fvalue] = entry_node . flow
    ];

  set_ilist_node = fn (ilist, node)
    // Types: ilist: instruction list, node: flow graph node
    // Effects: Sets the node of all instructions in ilist to node
    dforeach(fn (il) il[mc:il_node] = node, ilist);

  mc:flatten_blocks = fn (ifn)
    // Types: ifn : intermediate function
    // Effects: Flatten the basic blocks of ifn's flow graph
    //   Sets ifn[mc:c_fvalue] to the instruction list.
    [
      | nodes, fg, ilist |

      fg = ifn[mc:c_fvalue];
      nodes = order_nodes(car(fg), cdr(fg));

      while (nodes != null)
	[
	  ilist = dappend!(ilist, graph_node_get(car(nodes))[mc:f_ilist]);
	  nodes = cdr(nodes);
	];

      ifn[mc:c_fvalue] = mc:remove_labels(mc:remove_aliases(mc:remove_branches(clear_nodes(ilist))));
    ];

  clear_nodes = fn (ilist)
    // Types: ilist: list of instruction
    // Effects: Clears flow graph backpointers from instructions in ilist
    // Returns: ilist
    [
      dforeach(fn (il) il[mc:il_node] = null, ilist);
      ilist
    ];

  order_nodes = fn (entry, flow)
    // Types: entry: node, flow: graph
    // Returns: A list of the nodes of the flow-graph, in an executable
    //   order starting with entry.
    [
      | nodes, allnodes |

      allnodes = graph_nodes(flow); // nodes not yet ordered

      loop
	[
	  // next node is entry
	  | next |

	  nodes = entry . nodes;
	  allnodes = ldelete!(entry, allnodes);
	  if (allnodes == null) exit lreverse(nodes);

	  // Find next node:
	  next = null;
	  graph_edges_out_apply(fn (edge) if (graph_edge_get(edge))
				  next = graph_edge_to(edge), entry);

	  if (next != null) entry = next
	  else
	    [
	      | nodes, lastins |

	      // find nodes that have no direct predecessors
	      nodes = lfilter(fn (node)
			        lforall?(fn (edge) !graph_edge_get(edge),
					 graph_edges_in(node)),
			      allnodes);

	      // prefer the destination of previous block
	      lastins = dget(dprev(graph_node_get(entry)[mc:f_ilist]))[mc:il_ins];
	      if (lastins[mc:i_class] == mc:i_branch &&
		  memq(lastins[mc:i_bdest][mc:l_ilist][mc:il_node], nodes))
		next = lastins[mc:i_bdest][mc:l_ilist][mc:il_node]
	      else
		next = car(nodes);
	    ];
	  entry = next
	]
    ];

  // Part 2: actual data-flow problems

intersection_predecessors = fn (n, problem)
  // Types: n: flow graph node
  //        problem: a data-flow problem index
  // Effects: Computes one step of iterative solution of dataflow equations
  //   for the specified problem, assuming the following equations:
  //     in(n) = intersection{p:predecessors of n} out(p)
  //     out(n) = gen(n) U (in(n) - kill(n))
  // Returns: true if out(n) changes
  [
    | info, new_in, new_out |

    info = graph_node_get(n)[problem];

    // in = union{p:predecessors of n} out(p)
    new_in = info[mc:flow_in];
    graph_edges_in_apply(fn (predecessor) bintersection!(new_in, graph_node_get(graph_edge_from(predecessor))[problem][mc:flow_out]), n);
    info[mc:flow_in] = new_in;

    // out(i) = gen(i) U (in(i) - kill(i))
    new_out = bunion!(bdifference(new_in, info[mc:flow_kill]), info[mc:flow_gen]);
    if (bitset_eq?(info[mc:flow_out], new_out)) false
    else
      [
	info[mc:flow_out] = new_out;
	true
      ]
  ];

union_predecessors = fn (n, problem)
  // Types: n: flow graph node
  //        problem: a data-flow problem index
  // Effects: Computes one step of iterative solution of dataflow equations
  //   for the specified problem, assuming the following equations:
  //     in(n) = union{p:predecessors of n} out(p)
  //     out(n) = gen(n) U (in(n) - kill(n))
  // Returns: true if out(n) changes
  [
    | info, new_in, new_out |

    info = graph_node_get(n)[problem];

    // in = union{p:predecessors of n} out(p)
    new_in = info[mc:flow_in];
    graph_edges_in_apply(fn (predecessor) bunion!(new_in, graph_node_get(graph_edge_from(predecessor))[problem][mc:flow_out]), n);
    info[mc:flow_in] = new_in;

    // out(i) = gen(i) U (in(i) - kill(i))
    new_out = bunion!(bdifference(new_in, info[mc:flow_kill]), info[mc:flow_gen]);
    if (bitset_eq?(info[mc:flow_out], new_out)) false
    else
      [
	info[mc:flow_out] = new_out;
	true
      ]
  ];

union_successors = fn (n, problem)
  // Types: n: flow graph node
  //        problem: a data-flow problem index
  // Effects: Computes one step of iterative solution of dataflow equations
  //   for the specified problem, assuming the following equations:
  //     out(n) = union{s:successors of n} in(s)
  //     in(n) = gen(n) U (out(n) - kill(n))
  // Returns: true if out(n) changes
  [
    | info, new_in, new_out |

    info = graph_node_get(n)[problem];

    // out = union{s:successors of n} in(s)
    new_out = info[mc:flow_out];
    graph_edges_out_apply(fn (successor) bunion!(new_out, graph_node_get(graph_edge_to(successor))[problem][mc:flow_in]), n);
    info[mc:flow_out] = new_out;

    // in(i) = gen(i) U (out(i) - kill(i))
    new_in = bunion!(bdifference(new_out, info[mc:flow_kill]), info[mc:flow_gen]);
    if (bitset_eq?(info[mc:flow_in], new_in)) false
    else
      [
	info[mc:flow_in] = new_in;
	true
      ]
  ];

// flow minimum known sizes of vectors and strings
mc:flow_sizes = fn (ifn)
  [
    | fg, change, new_sizeset |
    fg = ifn[mc:c_fvalue];

    new_sizeset = fn (ifn) make_string(ifn[mc:c_fnvars]);

    // initialise data-flow problem
    graph_nodes_apply(fn (n)
		      [
			| block |
			block = graph_node_get(n);
                        block[mc:f_sizes] = vector
                          (null, null,
                           new_sizeset(ifn),    // in
                           new_sizeset(ifn),    // out
                           ifn[mc:c_fallvars]); // map
		      ], cdr(fg));

    | size_block, merge_in_sizes |

    merge_in_sizes = fn (predecessor, first_il, new_in)
      [
        | pnode, pout, override_var, override_value |
        pnode = graph_node_get(graph_edge_from(predecessor));
        pout = pnode[mc:f_sizes][mc:flow_out];

        override_var = 0;

        <done> [
          // check if the 'predecessor' block ended in a size-length
          // branch
          | plast, pins |
          plast = dprev(pnode[mc:f_ilist]);
          pins = dget(plast)[mc:il_ins];
          if (pins[mc:i_class] != mc:i_branch)
            exit<done> null;

          | bop |
          bop = pins[mc:i_bop];
          if (bop < mc:branch_slength || bop >= mc:branch_equal)
            exit<done> null;

          // it's either an slength or vlength branch
          | bvar1, val |
          @(_ bvar1) = pins[mc:i_bargs];
          val = mc:var_value(bvar1);
          assert(integer?(val));

          // avoid overflow problems
          if (val < 0 || val >= (1 << 20))
            exit<done> null;

          if (bop >= mc:branch_vlength)
            bop += mc:branch_eq - mc:branch_vlength
          else
            bop += mc:branch_eq - mc:branch_slength;

          | blabel, nlabel |
          blabel = pins[mc:i_bdest];
          while (nlabel = blabel[mc:l_alias]) blabel = nlabel;
          if (blabel[mc:l_ilist] != first_il)
            [
              // the fall-through is to our block; invese operator logic
              bop ^= 1;
            ];

          if (bop == mc:branch_eq)
            null
          else if (bop == mc:branch_gt)
            ++val
          else if (bop == mc:branch_ge)
            null
          else
            exit<done> null;

          if (val > 255)
            val = 255;

          | pnvar |
          pnvar = car(pins[mc:i_aargs])[mc:v_number];
          if (val <= pout[pnvar])
            exit<done> null;

          // 'override_var' is known to at least be of size 'override_value'
          override_var = pnvar;
          override_value = val
        ];

        if (new_in == null)
          [
            new_in = scopy(pout);
            if (override_var > 0)
              new_in[override_var] = override_value;
          ]
        else
          for (|i| i = slength(new_in); --i > 0; )
            new_in[i] = min(new_in[i],
                            if (i == override_var) override_value
                            else pout[i]);
        new_in
      ];

    size_block = fn (n)
      [
        | info, new_in, block, ilist, first_il |
        block = graph_node_get(n);
        info = block[mc:f_sizes];
        ilist = block[mc:f_ilist];
        first_il = dget(ilist);
        new_in = null;
        graph_edges_in_apply(fn (predecessor) [
          new_in = merge_in_sizes(predecessor, first_il, new_in)
        ], n);

        if (new_in == null)
          new_in = new_sizeset(ifn);
        info[mc:flow_in] = new_in;

        | new_out, amb |
        new_out = scopy(new_in);
        amb = block[mc:f_ambiguous_w][mc:flow_in];
        for (|scan| scan = ilist; ;)
          [
            | il, ins, class |
            il = dget(scan);
            ins = il[mc:il_ins];
            class = ins[mc:i_class];

            <noref> [
              | rvar, ivar, sizefield |
              if (class == mc:i_compute && ins[mc:i_aop] == mc:b_ref)
                [
                  @(rvar ivar) = ins[mc:i_aargs];
                  sizefield = mc:i_asizeinfo;
                ]
              else if (class == mc:i_call
                       && vfind?(car(ins[mc:i_cargs]), referencers)
                       && llength(cdr(ins[mc:i_cargs])) > 2)
                [
                  @(rvar ivar . _) = cdr(ins[mc:i_cargs]);
                  sizefield = mc:i_csizeinfo;
                ]
              else
                exit<noref> null;

              // 'rvar' will be at least of size 'ivar'; record previous size
              // information in the 'sizefield' of the instruction
              | nrvar |
              nrvar = rvar[mc:v_number];
              if (nrvar <= 0)
                [
                  ins[sizefield] = 0;
                  exit<noref> null;
                ];

              ins[sizefield] = new_out[nrvar];

              | ival |
              ival = mc:var_value(ivar);
              if (!integer?(ival))
                exit<noref> null;

              // convoluted to avoid the -minint problem
              if (ival <= -255 || ival >= 254)
                ival = 255
              else if (ival < 0)
                ival = -ival
              else
                ++ival;

              if (ival > new_out[nrvar])
                new_out[nrvar] = ival;
            ];

	    if (class == mc:i_closure)
	      mc:set_closure_vars!(ins, mc:closure_write, amb)
            else if (class == mc:i_vref)
              set_bit!(amb, ins[mc:i_varg][mc:v_number])
            else if (class == mc:i_call && mc:call_escapes?(ins)
                     || class == mc:i_return)
              [
                // if call escapes, reset information for all variables that
                // may change
                lforeach(fn (v) new_out[v[mc:v_number]] = 0,
                         ifn[mc:c_fglobals]);
                bforeach(fn (n) new_out[n] = 0, amb);
              ];

            // check for known calls or operations that return objects of known
            // sizes
            | dvar, ndvar |
            dvar = mc:defined_var(ins);
            if (dvar && (ndvar = dvar[mc:v_number]) > 0)
              [
                | new, get_size |

                get_size = fn (vec, var)
                  [
                    if (var[mc:v_class] == mc:v_constant)
                      [
                        | val |
                        val = var[mc:v_kvalue];
                        if (string?(val))
                          exit<function> slength(val);
                        if (vector?(val))
                          exit<function> vlength(val);
                      ];
                    | num |
                    num = var[mc:v_number];
                    if (num > 0)
                      vec[num]
                    else
                      0
                  ];

                new = 0;
                if (class == mc:i_compute)
                  [
                    | aop |
                    aop = ins[mc:i_aop];
                    if (aop == mc:b_assign)
                      new = get_size(new_out, car(ins[mc:i_aargs]))
                    else if (aop == mc:b_add)
                      new = (get_size(new_out, car(ins[mc:i_aargs]))
                             + get_size(new_out, cadr(ins[mc:i_aargs])))
                    else if (aop == mc:b_vector || aop == mc:b_sequence)
                      new = llength(ins[mc:i_aargs]);
                  ]
                else if (class == mc:i_call)
                  [
                    | f, args |
                    @(f . args) = ins[mc:i_cargs];
                    if (f == kmake_vector || f == kmake_string)
                      match (args) [
                        (arg) => [
                          | n |
                          n = mc:var_value(arg);
                          if (integer?(n))
                            new = n
                        ]
                      ]
                    else if (f == kvappend || f == ksappend)
                      match (args) [
                        (arg1 arg2) => [
                          new = (get_size(new_out, arg1)
                                 + get_size(new_out, arg2));
                        ]
                      ]
                    else if (f == kvcopy || f == kscopy)
                      match (args) [
                        (arg) => [
                          new = get_size(new_out, arg);
                        ]
                      ];
                  ];
                new_out[ndvar] = new;
              ];

            scan = dnext(scan);
            if (scan == ilist)
              exit<break> null;
          ];

        if (string_cmp(new_out, info[mc:flow_out]) != 0)
          [
            info[mc:flow_out] = new_out;
            change = true;
          ];
      ];

    change = true;
    while (change)
      [
	change = false;
	graph_nodes_apply(size_block, cdr(fg));
      ];
  ];

| type_rwmask |
type_rwmask = fn (type)
  if (type == mc:f_ambiguous_rw)
    mc:closure_write | mc:closure_read
  else if (type == mc:f_ambiguous_w)
    mc:closure_write
  else
    fail();

mc:flow_ambiguous = fn (ifn, type)
  // Types: ifn: intermediate function, rwmask: int
  // Effects: A simple data-flow problem (no info killed):
  //   computes which variables have escaped into closures by the start
  //   of each block. This is used to detect ambiguous use/definition at
  //   function call time.
  //   rwmask is used to select the variables that are considered ambiguous:
  //     mc:closure_read: variables read in some closure
  //     mc:closure_write: variables written in some closure
  //     mc:closure_read|mc:closure_write: all variables that escape
  [
    | fg, merge_block, change, block_ambiguous, rwmask |

    rwmask = type_rwmask(type);
    fg = ifn[mc:c_fvalue];

    block_ambiguous = fn (ilist)
      // Types: ilist: instruction list
      // Returns: bitset of variables added to closures in ilist
      [
	| amb, scan |

	scan = ilist;
	amb = mc:new_varset(ifn);
	loop
	  [
	    | ins |

	    ins = dget(scan)[mc:il_ins];
	    if (ins[mc:i_class] == mc:i_closure)
	      mc:set_closure_vars!(ins, rwmask, amb)
            else if (ins[mc:i_class] == mc:i_vref)
              set_bit!(amb, ins[mc:i_varg][mc:v_number]);

	    scan = dnext(scan);
	    if (scan == ilist) exit amb
	  ]
      ];

    // initialise data-flow problem
    graph_nodes_apply(fn (n)
		      [
			| block |
			block = graph_node_get(n);
			block[type] = vector
			  (block_ambiguous(block[mc:f_ilist]),
			   mc:new_varset(ifn), // nothing is ever killed
			   mc:new_varset(ifn),
			   mc:new_varset(ifn),
			   ifn[mc:c_fallvars]);
		      ], cdr(fg));

    // equations:
    //   in(i) = union{p:predecessors of i} out(p)
    //   out(i) = in(i) U gen(i)

    merge_block = fn (n)
      if (union_predecessors(n, type)) change = true;

    loop
      [
	change = false;
	graph_nodes_apply(merge_block, cdr(fg));
	if (!change) exit 0
      ];
  ];

mc:scan_ambiguous = fn (f, x, block, globals, type)
  // Types: f : function (see effects)
  //        x : any
  //        block : flow graph node
  //        globals : bitset
  //        rwmask : int
  // Effects: Scans the instructions of the block in order, doing
  //     x = f(ins, ambiguous, x)
  //   at each instruction, where ambiguous is the current "ambiguous"
  //   information (represented as a mutable bitset)
  //   rwmask is used to select the variables that are considered ambiguous:
  //     mc:closure_read: variables read in some closure
  //     mc:closure_write: variables written in some closure
  //     mc:closure_read|mc:closure_write: all variables that escape
  // Returns: The final x
  [
    | scan, ilist, ambiguous, rwmask |

    rwmask = type_rwmask(type);
    ilist = block[mc:f_ilist];
    scan = ilist;
    ambiguous = bunion(block[type][mc:flow_in], globals);
    loop
      [
	| ins, il |

	il = dget(scan);
	ins = il[mc:il_ins];

	x = f(il, ambiguous, x);
	if (ins[mc:i_class] == mc:i_closure)
	  mc:set_closure_vars!(ins, rwmask, ambiguous);

	scan = dnext(scan);
	if (scan == ilist) exit x
      ]
  ];

mc:build_ambiguous_list = fn (block, globals, type)
  // same as mc:scan_ambiguous() but returns a list of ambiguous
  // bitset per ilist
  [
    | scan, ilist, ambiguous, rwmask, result |

    rwmask = type_rwmask(type);
    ilist = block[mc:f_ilist];
    scan = ilist;
    ambiguous = bunion(block[type][mc:flow_in], globals);
    loop
      [
	| ins, il |

	il = dget(scan);
	ins = il[mc:il_ins];

        result = ambiguous . result;

	scan = dnext(scan);
	if (scan == ilist) exit result;

	if (ins[mc:i_class] == mc:i_closure)
          [
            | vars, new? |
            new? = false;
            vars = ins[mc:i_ffunction][mc:c_fclosure];
            while (vars != null)
              [
                | var |
                @(var . vars) = vars;
                if (rwmask == (mc:closure_read | mc:closure_write)
                    || (mc:var_base(var)[mc:v_lclosure_uses] & rwmask))
                  [
                    | vnum |
                    vnum = var[mc:v_cparent][mc:v_number];
                    if (!new? && bit_clear?(ambiguous, vnum))
                      [
                        ambiguous = bcopy(ambiguous);
                        new? = true;
                      ];
                    set_bit!(ambiguous, vnum);
                  ];
              ];
          ];
      ]
  ];

mc:flow_uses = fn (ifn)
  // Types: ifn: intermediate function
  // Requires: ambiguous data-flow information
  // Effects: Computes definition use information (for du-chains)
  //   Uses of global variables for which there are no assignments
  //   within ifn are ignored (otherwise the use set explodes).
  //   As the uses are for du-chains, this is no loss.
  [
    | fg, merge_block, change, block_uses, all_uses, vmap, uindex,
      new_use, new_use_set, use_set, uses, uset, rcount, globals, i, vars |

    fg = ifn[mc:c_fvalue];
    vmap = ifn[mc:c_fallvars];
    uindex = -1;
    vars = lappend(ifn[mc:c_fclosure],
                   lappend(ifn[mc:c_fglobals], ifn[mc:c_flocals]));
    // not interested in uses of variables which are not written in ifn
    globals = mc:set_vars!(mc:new_varset(ifn), ifn[mc:c_fglobals_write]);
    mc:set_vars!(globals, ifn[mc:c_fclosure_write]);

    new_use = fn (nv, il)
      (uindex = uindex + 1) . vmap[nv] . il;

    new_use_set = fn () new_bitset(uindex);
    use_set = fn (uses)
      [
	| uset |

	uset = new_use_set();
	while (uses != null)
	  [
	    set_bit!(uset, caar(uses));
	    uses = cdr(uses);
	  ];
	uset
      ];

    block_uses = fn (block)
      // Types: block: flow graph node
      //        globals : list of var
      // Returns: list of (variable . statement) pairs of uses of var in block
      //   On function calls, all the ambiguous variables must be
      //   considered used.
      [
	| use1, defined |

	use1 = fn (il, ambiguous, uses)
	  [
	    | ndvar |

	    uses = breduce(fn (nv, l) new_use(nv, il) . l, uses,
			   bdifference(mc:barguments(il, ambiguous), defined));

	    if (ndvar = il[mc:il_defined_var])
	      set_bit!(defined, ndvar);

	    uses
	  ];

	defined = mc:new_varset(ifn);
	mc:scan_ambiguous(use1, null, block, globals, mc:f_ambiguous_w)
      ];

    // initialise data-flow problem
    graph_nodes_apply(fn (n)
		      [
			| block, uses |
			block = graph_node_get(n);
			uses = block_uses(block);
			all_uses = lappend(uses, all_uses);
			block[mc:f_uses] = uses;
		      ], cdr(fg));

    // make map of use number to uses (all_uses is in reverse order of use number)
    uindex = uindex + 1;
    i = uindex;
    uses = all_uses;
    all_uses = make_vector(uindex);

    uset = new_use_set();
    lforeach(fn (v) v[mc:v_uses] = bcopy(uset), vars);

    while (uses != null)
      [
	| use |

	use = cdar(uses);
	all_uses[i = i  - 1] = use;
	set_bit!(car(use)[mc:v_uses], i);
	uses = cdr(uses);
      ];
    assert(i == 0);

    // setup flow information and compute killed uses
    graph_nodes_apply
      (fn (n)
       [
	 | block, killed |
	 block = graph_node_get(n);

	 killed = bcopy(uset);
	 bforeach(fn (ndvar) bunion!(killed, vmap[ndvar][mc:v_uses]),
		  block[mc:f_dvars]);

	 block[mc:f_uses] = vector(use_set(block[mc:f_uses]),
				   killed,
				   bcopy(uset),
				   bcopy(uset),
				   all_uses);
       ], cdr(fg));

    // equations:
    //   out(i) = union{s:successors of i} in(s)
    //   in(i) = gen(i) U (out(i) - kill(i))

    merge_block = fn (n)
      if (union_successors(n, mc:f_uses)) change = true;

    rcount = 0;
    change = true;
    while (change)
      [
	change = false;
	graph_nodes_apply(merge_block, cdr(fg));
	rcount = rcount + 1;
      ];
    if (mc:verbose >= 3)
      [
	dformat("use resolution iterations: %s\n", rcount);
      ];
    lforeach(fn (v) v[mc:v_uses] = null, vars);
  ];

mc:flow_copies = fn (ifn)
  // Types: ifn: intermediate function
  // Requires: ambiguous data-flow information
  // Effects: Computes reaching copy information (for copy propagation)
  [
    | fg, merge_block, change, block_copies, all_copies, entry, eblock, copy1,
      globals, cindex, vmap, new_copy_set, copy_set, i, copies, copyset |

    fg = ifn[mc:c_fvalue];
    entry = car(fg);
    cindex = -1;
    vmap = ifn[mc:c_fallvars];

    new_copy_set = fn () new_bitset(cindex);
    copy_set = fn (copies)
      [
	| copyset |

	copyset = new_copy_set();
	while (copies != null)
	  [
	    set_bit!(copyset, caar(copies));
	    copies = cdr(copies);
	  ];
	copyset
      ];

    copy1 = fn (il, ambiguous, copies)
      [
	| class, ins, ndvar |

	ins = il[mc:il_ins];
	class = ins[mc:i_class];

	// kill copies of assigned variables
	if (ndvar = il[mc:il_defined_var])
	  [
	    | dvar |

	    dvar = vmap[ndvar];
	    copies = lfilter
	      (fn (il)
	       [
		 | ins |
		 ins = il[mc:il_ins];
		 ins[mc:i_adest] != dvar && car(ins[mc:i_aargs]) != dvar
	       ], copies);
	  ];

	if (class == mc:i_call && mc:call_escapes?(ins))
          [
            | block |
            block = graph_node_get(il[mc:il_node]);
            // add 'ambiguous' to the block's ambiguous set
            if (block[mc:f_copies])
              bunion!(block[mc:f_copies], ambiguous)
            else
              block[mc:f_copies] = bcopy(ambiguous);

            // all ambiguous variables may be assigned
            copies = lfilter
              (fn (il)
               [
                 | ins |
                 ins = il[mc:il_ins];
                 bit_clear?(ambiguous, ins[mc:i_adest][mc:v_number]) &&
                   bit_clear?(ambiguous, car(ins[mc:i_aargs])[mc:v_number])
               ], copies);
          ];

	if (class == mc:i_compute && ins[mc:i_aop] == mc:b_assign)
	  copies = il . copies;

	copies
      ];

    block_copies = fn (block, globals)
      // Types: block: flow graph node
      //        globals : list of var
      // Returns: ambiguous set . list of copy statement of ilist
      //   On function calls, all the variables in ambiguous must be
      //   considered used.
      [
        // use this to hold ambiguous set
        block[mc:f_copies] = false;
        | copies |
        copies = lmap(fn (copy) ++cindex . copy,
                      mc:scan_ambiguous(copy1, null, block,
                                        globals, mc:f_ambiguous_w));
        block[mc:f_copies] . copies
      ];


    // initialise data-flow problem
    globals = mc:set_vars!(mc:new_varset(ifn), ifn[mc:c_fglobals]);
    mc:set_vars!(globals, ifn[mc:c_fclosure]);

    graph_nodes_apply(fn (n)
		      [
			| block, copies |
			block = graph_node_get(n);
			copies = block_copies(block, globals);
			all_copies = lappend(cdr(copies), all_copies);
			block[mc:f_copies] = copies;
n		      ], cdr(fg));

    // make map of use number to uses (all_uses is in reverse order of use number)
    ++cindex;
    i = cindex;
    copies = all_copies;
    all_copies = make_vector(cindex);

    copyset = new_copy_set();

    while (copies != null)
      [
	all_copies[--i] = cdar(copies);
	copies = cdr(copies);
      ];
    assert(i == 0);

    // setup flow information and compute killed copies
    graph_nodes_apply
      (fn (n)
       [
	 | block, killed, copies, kvars, firstout, firstin, i, ambiguous |

	 block = graph_node_get(n);
	 kvars = block[mc:f_dvars];
	 @(ambiguous . copies) = block[mc:f_copies];
         copies = copy_set(copies);

         if (ambiguous)
           kvars = bunion(kvars, ambiguous);

	 // killed = all copies whose vars are assigned or ambiguous
	 // in the block
	 i = 0;
	 killed = bcopy(copyset);
	 while (i < cindex)
	   [
	     | il, copy |

	     il = all_copies[i];
	     copy = il[mc:il_ins];
	     if (bit_set?(kvars, copy[mc:i_adest][mc:v_number]) ||
		 bit_set?(kvars, car(copy[mc:i_aargs])[mc:v_number]))
	       set_bit!(killed, i);

	     ++i;
	   ];
	 // except those generated here
	 bdifference!(killed, copies);

         // start value for flow_in: all_copies
	 firstin = string_fill!(bcopy(copyset), 255);
	 // start value for flow_out: all_copies - kill
	 firstout = bdifference(firstin, killed);

	 block[mc:f_copies] = vector(copies,
				     killed,
				     firstin,
				     firstout,
				     all_copies);
       ], cdr(fg));

    // equations:
    //   out(i) = gen(i) U (in(i) - kill(i))
    //   in(i) = intersection{p: predecessor of i} out(p) (i != entry)
    //   in(entry) = 0
    //   out(entry) = gen(entry)

    eblock = graph_node_get(entry)[mc:f_copies];
    eblock[mc:flow_in] = copyset;
    eblock[mc:flow_out] = eblock[mc:flow_gen];

    merge_block = fn (n)
      if (n != entry && intersection_predecessors(n, mc:f_copies)) change = true;

    change = true;
    while (change)
      [
	change = false;
	graph_nodes_apply(merge_block, cdr(fg));
      ];

    all_copies
  ];

mc:flow_live = fn (ifn)
  // Types: ifn: intermediate function
  // Effects: Computes liveness information (for register allocation)
  [
    | fg, merge_block, change, block_live, globals |

    fg = ifn[mc:c_fvalue];
    globals = mc:set_vars!(mc:new_varset(ifn), ifn[mc:c_fglobals]);

    block_live = fn (block)
      // Types: block: flow graph node
      // Returns: variables used before definition (bitset)
      //   No ambiguous information is used, as this is done after indirection
      //   is added.
      [
	| used, defined, live1 |

	live1 = fn (il)
	  [
	    | ndvar |

	    bunion!(used, bdifference(il[mc:il_arguments], defined));
	    if (ndvar = il[mc:il_defined_var])
	      set_bit!(defined, ndvar);
	  ];
	used = mc:new_varset(ifn); defined = mc:new_varset(ifn);
	dforeach(live1, block[mc:f_ilist]);
	used
      ];

    // initialise data-flow problem
    graph_nodes_apply
      (fn (n)
       [
	 | block |

	 block = graph_node_get(n);
	 block[mc:f_live] = vector
	   (bdifference!(block_live(block), globals),
	    block[mc:f_dvars],
	    mc:new_varset(ifn),
	    mc:new_varset(ifn),
	    ifn[mc:c_fallvars]);
       ], cdr(fg));

    // equations:
    //   out(i) = union{s:successors of i} in(s)
    //   in(i) = gen(i) U (out(i) - kill(i))

    merge_block = fn (n)
      if (union_successors(n, mc:f_live)) change = true;

    change = true;
    while (change)
      [
	change = false;
	graph_nodes_apply(merge_block, cdr(fg));
      ];
  ];

mc:rscan_live = fn (f, x, block)
  // Types: f : function (see effects)
  //        x : any
  //        block : flow graph node
  // Effects: Scans the instructions of the block in reverse order, doing
  //     x = f(ins, live_in, live_out, x)
  //   at each instruction, where live_in and live_out is the liveness
  //   information (bitsets)
  //   WARNING: live_in and live_out contain references to the global
  //     variables of the block (function), even though these are not
  //     to be considered live. It is up to 'f' to ignore them.
  // Returns: The final x
  [
    | scan, ilist, live_out, live_in |

    ilist = block[mc:f_ilist];
    scan = ilist;
    live_out = bcopy(block[mc:f_live][mc:flow_out]);
    loop
      [
	| il, ndvar |

	scan = dprev(scan);
	il = dget(scan);

	live_in = bcopy(live_out);
	// if var defined, it wasn't live going in
	if (ndvar = il[mc:il_defined_var])
	  [
	    clear_bit!(live_in, ndvar);
	    // dvar is live as the result will be stored there
	    // (even if this result is never used)
	    set_bit!(live_out, ndvar);
	  ];

	// Cf warning above, some global vars may be added here:
	bunion!(live_in, il[mc:il_arguments]);

	x = f(il, live_in, live_out, x);
	live_out = live_in;

	if (scan == ilist) exit x
      ]
  ];


  // Part 3: display of computed data-flow information

  mc:display_blocks = fn (ifn)
    [
      | fg |

      fg = ifn[mc:c_fvalue];

      dformat("Closure %s(%s) has %s block(s):\n", ifn[mc:c_fnumber],
		     mc:fname(ifn),
		     llength(graph_nodes(cdr(fg))));
      lforeach(fn (node)
	       [
                 if (mc:verbose > 4)
                   mc:flow_display(graph_node_get(node));

                 if (mc:verbose > 3)
                   [
                     display("  from ins");
                     graph_edges_in_apply(fn (edge) [
                       | fnode |
                       fnode = graph_edge_from(edge);
                       dformat(" %d", dget(dprev(graph_node_get(fnode)[mc:f_ilist]))[mc:il_number]);
                     ], node);
                     newline();
                   ];

		 mc:ins_list1(graph_node_get(node)[mc:f_ilist]);

                 if (mc:verbose > 3)
                   [
                     display("  to ins");
                     graph_edges_out_apply(fn (edge) [
                       | tnode |
                       tnode = graph_edge_to(edge);
                       dformat(" %d", dget(graph_node_get(tnode)[mc:f_ilist])[mc:il_number]);
                     ], node);
                     newline();
                   ];

		 newline();
	       ], order_nodes(car(fg), cdr(fg)));
      newline();
    ];

  | display_sizes |
  display_sizes = fn (info)
    [
      if (!info) exit<function> null;

      | dlist |
      dlist = fn (name, s)
        [
          display(name);
          for (|i, l| [ l = slength(s); i = 0 ]; i < l; ++i)
            [
              | v |
              v = s[i];
              if (v == 0)
                exit<continue> null;
              dformat("  %d [%d]", i, v)
            ];
          newline();
        ];

      display("Size information:\n");
      dlist("in:  ", info[mc:flow_in]);
      dlist("out: ", info[mc:flow_out]);
    ];

  mc:flow_display = fn (fnode)
    [
      bflow_display("ambiguous(w)", fnode[mc:f_ambiguous_w],
                    fn (x) display(mc:svar(x)));
      bflow_display("ambiguous(rw)", fnode[mc:f_ambiguous_rw],
                    fn (x) display(mc:svar(x)));
      bflow_display("uses", fnode[mc:f_uses],
		    fn (use)
		    [
		      dformat("%s: ", mc:svar(car(use)));
		      display(cdr(use)[mc:il_number]);
		    ]);
      bflow_display("copies", fnode[mc:f_copies],
                    fn (copy) display(copy[mc:il_number]));
      bflow_display("live", fnode[mc:f_live], fn (x) display(mc:svar(x)));
      display_sizes(fnode[mc:f_sizes]);
      mc:show_type_info(fnode[mc:f_types]);
    ];

  bflow_display = fn (name, info, pfn)
    if (info)
      [
	| dlist |

	dlist = fn (name, l)
	  [
	    dformat("%s: ", name);
	    if (bempty?(l)) display("none")
	    else
	      [
		| first |
		bforeach(fn (i)
			 [
			   if (!first) display(", ");
			   pfn(info[mc:flow_map][i]);
			   first = false;
			 ], l);
	      ];
	    newline();
	  ];

	dformat("Data-flow information: %s\n", name);
	dlist("gen", info[mc:flow_gen]);
	dlist("kill", info[mc:flow_kill]);
	dlist("in", info[mc:flow_in]);
	dlist("out", info[mc:flow_out]);
	newline();
      ];

  mc:all_functions = fn (ifn)
    // Types: ifn : intermediate function
    // Effects: Returns all the functions in ifn
    [
      | todo, fns, check_ins |

      check_ins = fn (ins)
        [
          | i, cls |
          i = ins[mc:il_ins];
          cls = i[mc:i_class];
          if (cls == mc:i_closure)
            todo = i[mc:i_ffunction] . todo
          else if (cls == mc:i_call)
            [
              | called |
              called = car(i[mc:i_cargs]);
              if (called[mc:v_class] == mc:v_function)
                todo = called[mc:v_fvalue] . todo
            ]
        ];

      todo = ifn . null;

      while (todo != null)
	[
	  | first |
	  first = car(todo);
	  todo = cdr(todo);

	  fns = first . fns;

          if (pair?(first[mc:c_fvalue]))
            [
              graph_nodes_apply(fn (node) [
                node = graph_node_get(node);
                dforeach(check_ins, node[mc:f_ilist]);
              ], cdr(first[mc:c_fvalue]));
            ]
          else
            dforeach(check_ins, first[mc:c_fvalue]);
	];
      fns
    ];

];
