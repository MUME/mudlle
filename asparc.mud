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

library asparc // The sparc assembler
requires system, dlist, msparc, sequences, graph, compiler
defines sparc:assemble, sparc:reset_counters
reads mc:verbose
writes nins, nslots, nfilled, nbranches, nbalways, njumps, ncalls, 
  nunfilled_branch, nunfilled_jump, nunfilled_call, nannull_filled
[
  | set_offsets, remove_aliases, assemble, basic_code, ins_gen, setword,
    mapoffsets!, intreg, high_bit, block_scheduler, simple_scheduler,
    delayslotok |

  sparc:reset_counters = fn ()
    [
      nins = 0;
      nslots = nbalways = nbranches = njumps = ncalls = 0;
      nannull_filled = 0;
      nfilled = nunfilled_branch = nunfilled_jump = nunfilled_call = 0;
    ];

  sparc:reset_counters();

  sparc:assemble = fn "sparcode -> sparcasm" (fcode)
    [
      | ilist |

      ilist = sparc:get_instructions(fcode);
      if (mc:verbose >= 4)
	[
	  sparc:ins_list(fcode);
	  newline();
	];
      fcode[0] = ilist = simple_scheduler(fcode, ilist);
      if (mc:verbose >= 4)
	[
	  sparc:ins_list(fcode);
	  newline();
	  display(format("nins %s, dlength %s\n", nins, dlength(ilist)));
	];
      nins = nins + dlength(ilist);
      remove_aliases(ilist);
      set_offsets(ilist);

      assemble(ilist) .
	vector(mapoffsets!(fcode[mc:a_builtins + 2]),
	       mapoffsets!(fcode[mc:a_constants + 2]),
	       mapoffsets!(fcode[mc:a_subfns + 2]),
	       mapoffsets!(fcode[mc:a_globals + 2]),
	       mapoffsets!(fcode[mc:a_kglobals + 2]),
	       mapoffsets!(fcode[mc:a_primitives + 2]))
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

/*
  block_scheduler = fn (fcode, ilist)
    // Types: ilist: list of sparc instructions
    // Effects: Schedule ilist. This is a simple, basic block list
    //   scheduler. Traps do not interrupt a basic block.
    [
      | newlist, start, icount, is_il, is_delay, is_delayfree, is_time,
	is_critical, is_depends, schedule_dependences, schedule_costs,
	schedule_generate, graph_roots, graph_purge_node |

      is_il = 0; // The actual instruction
      is_delay = 1; // Any delay slot instruction that goes with it
      is_delayfree = 2; // True if delay slot is available
      is_time = 3; // Cycles for this instruction
      is_critical = 4; // Critical path from this instruction
      is_depends = 5; // nb instructions that depend on this one

      // Hmm: don't distinguish between register dependences (that
      //   require waiting for the operations latency) and other
      //   dependences (traps to stores, loads to stores, etc) that don't
      //   require this wait (i.e. can be scheduled immediately)
      schedule_dependences = fn (ilist, depgraph)
	// Types: ilist: list of sparc instructions
	//        depgraph: an empty graph
	// Effects: Builds a dependency graph for the basic block starting
	//   at ilist
	// Returns: The remains of ilist, i.e. all but the first basic block
	[
	  | readers, writer, ccreg, loads, stores, traps, inode, 
	    adddep, readreg, writereg, addalldeps, setdelay |

	  adddep = fn (from, to)
	    if (from != null && from != to) graph_add_edge(from, to, null);

	  addalldeps = fn (fromlist, to)
	    lforeach(fn (f) adddep(f, to), fromlist);

	  readreg = fn (inode, reg)
	    if (pair?(reg) && reg != sparc:reg_g0)
	      [
		| r |

		r = car(reg);
		//display("reading "); display(r); newline();
		adddep(writer[r], inode); // r after w dependency
		readers[r] = inode . readers[r];
	      ];

	  writereg = fn (inode, reg)
	    if (reg != sparc:reg_g0)
	      [
		| r |

		r = car(reg);
		//display("writing "); display(r); newline();
		adddep(writer[r], inode); // w after w dependency
		addalldeps(readers[r], inode); // w after r dependency
		writer[r] = inode;
		readers[r] = null;
	      ];
	  
	  setdelay = fn (is, ilist)
	    [
	      | il, ins, nop |
	      
	      ilist = dnext(ilist);
	      il = dget(ilist);
	      ins = il[sparc:il_ins];
	      
	      is[is_delay] = il;
	      is[is_delayfree] = ins[sparc:i_type] == sparc:ins_sethi &&
		ins[sparc:i_arg2] == sparc:reg_g0; // a nop
	      
	      dnext(ilist)
	    ];
	  
	  icount = 0;
	  writer = make_vector(sparc:nregs + 1);
	  readers = make_vector(sparc:nregs + 1);
	  ccreg = sparc:nregs . 0; // pseudo-cc reg
	  loop
	    [
	      | il, inode, is, ins, itype |
	      
	      il = dget(ilist);
	      ins = il[sparc:il_ins];
	      //sparc:print_ins(ins, false, false); newline();
	      itype = ins[sparc:i_type];
	      
	      is = vector(il, false, false, 0, 0, 0);
	      inode = graph_add_node(depgraph, is);
	      icount = icount + 1;
	      if (itype == sparc:ins_alu)
		[
		  if ((ins[sparc:i_op] & sparc:op_cc) != 0)
		    writereg(inode, ccreg);
		  readreg(inode, ins[sparc:i_arg1]);
		  readreg(inode, ins[sparc:i_arg2]);
		  writereg(inode, ins[sparc:i_arg3]);
		]
	      else if (itype == sparc:ins_sethi)
		[
		  writereg(inode, ins[sparc:i_arg2]);
		]
	      else if (itype == sparc:ins_load)
		[
		  readreg(inode, ins[sparc:i_arg1]);
		  readreg(inode, ins[sparc:i_arg2]);
		  writereg(inode, ins[sparc:i_arg3]);
		  addalldeps(stores, inode);
		  addalldeps(traps, inode);
		  loads = inode . loads;
		]
	      else if (itype == sparc:ins_store)
		[
		  readreg(inode, ins[sparc:i_arg1]);
		  readreg(inode, ins[sparc:i_arg2]);
		  readreg(inode, ins[sparc:i_arg3]);
		  addalldeps(loads, inode);
		  addalldeps(stores, inode);
		  addalldeps(traps, inode);
		  stores = inode . stores;
		]
	      else if (itype == sparc:ins_save || itype == sparc:ins_restore)
		[
		  | r |
		  
		  readreg(inode, ins[sparc:i_arg1]);
		  readreg(inode, ins[sparc:i_arg2]);
		  
		  r = car(sparc:reg_o0) . 0;
		  while (car(r) <= car(sparc:reg_i7))
		    [
		      readreg(inode, r);
		      writereg(inode, r);
		      set_car!(r, car(r) + 1);
		    ];
		  
		  writereg(inode, ins[sparc:i_arg3]);
		]
	      else if (itype == sparc:ins_trap)
		[
		  readreg(inode, ins[sparc:i_arg1]);
		  readreg(inode, ins[sparc:i_arg2]);
		  addalldeps(stores, inode);
		  readreg(inode, ccreg);
		  traps = inode . traps;
		]
	      else if (itype == sparc:ins_branch)
		[
		  readreg(inode, ccreg);
		  if (ins[sparc:i_op] == sparc:balways && ins[sparc:i_arg2])
		    [
		      nbalways = nbalways + 1;
		      exit dnext(ilist); // no delay slot
		    ];
		  nbranches = nbranches + 1;
		  // assumes nop not placed in an annulled slot
		  // (an annulled nop should have false for is_delayfree)
		  exit setdelay(is, ilist);
		]
	      else if (itype == sparc:ins_call)
		[
		  ncalls = ncalls + 1;
		  writereg(inode, sparc:reg_o7);
		  exit setdelay(is, ilist);
		]
	      else if (itype == sparc:ins_jmpl)
		[
		  njumps = njumps + 1;
		  readreg(inode, ins[sparc:i_arg1]);
		  readreg(inode, ins[sparc:i_arg2]);
		  writereg(inode, ins[sparc:i_arg3]);
		  exit setdelay(is, ilist);
		];
	      ilist = dnext(ilist);
	      if (dget(ilist)[sparc:il_label]) exit ilist;
	    ]
	];
      
      graph_roots = fn (graph)
	// Types: graph: a graph
	// Returns: The roots of graph, i.e. all nodes with no in-edges
	lfilter(fn (node) graph_edges_in(node) == null, graph_nodes(graph));
      
      graph_purge_node = fn (node)
	// Types: node: a graph node
	// Effects: Removes node and all its edges from the graph
	[
	  lforeach(graph_remove_edge, graph_edges_in(node));
	  lforeach(graph_remove_edge, graph_edges_out(node));
	  graph_remove_node(node);
	];
      
      schedule_costs = fn (depgraph, roots)
	// Types: depgraph: dependency graph
	// Effects: Assigns scheduling costs to instructions in depgraph
	[
	  | setcost, compute_critical, compute_depends |

	  setcost = fn (is)
	    [
	      | ins, itype, cost, op |
	      
	      ins = is[is_il][sparc:il_ins];
	      //display("set cost "); sparc:print_ins(ins, false, false); newline();
	      itype = ins[sparc:i_type];
	      
	      cost = 1;	// default cost
	      if (itype == sparc:ins_alu)
		[
		  op = ins[sparc:i_op] & ~sparc:op_cc;
		  if (op == sparc:op_udiv || op == sparc:op_sdiv)
		    cost = 10;	// guess
		  if (op == sparc:op_umul || op == sparc:op_smul)
		    cost = 3;	// guess
		]
	      else if (itype == sparc:ins_load)
		cost = 2
	      else if (itype == sparc:ins_call || itype == sparc:ins_branch ||
		       itype == sparc:ins_jmpl)
		cost = 2; // encourages use of delay slot
	      
	      cost = cost * 2; // ultrasparc, 2-way superscalar
	      
	      is[is_time] = cost;
	    ];
	  
	  compute_critical = fn (root)
	    [
	      | maxtime, is |
	      
	      is = graph_node_get(root);
	      if (is[is_critical]) is[is_critical]
	      else
		[
		  maxtime =
		    lreduce(fn (edge, max)
			    [
			      | subtime |
			      subtime = compute_critical(graph_edge_to(edge));
			      if (subtime > max) subtime
			      else max
			    ], 0,
			    graph_edges_out(root));
		  is[is_critical] = is[is_time] + maxtime
		]
	    ];
	  
	  compute_depends = fn (from)
	    [
	      | count |

	      count = fn (node)
		if (graph_node_marked?(node)) 0
		else
		  [
		    graph_mark_node(node);
		    1 + lreduce(fn (edge, sum)
				sum + count(graph_edge_to(edge)),
				0,
				graph_edges_out(from))
		  ];
	      
	      graph_clear_all_marks(depgraph);
	      graph_node_get(from)[is_depends] = count(from);
	    ];
	  
	  graph_nodes_apply(fn (node) setcost(graph_node_get(node)), depgraph);
	  // The obvious n^2 algorithm. Is there a better one ?
	  //graph_nodes_apply(fn (node) compute_depends(node), depgraph);
	  lforeach(compute_critical, roots);
	];
      
      schedule_generate = fn (depgraph, roots)
	[
	  | t, newlist, ready, running, better, canrun, pick_ins, completed |
	  
	  t = 0;		// current "time"
	  ready = roots;
	  running = null;
	  newlist = null;
	  
	  better = fn (i1, i2)
	    [
	      | is1, is2 |
	      
	      is1 = graph_node_get(i1); is2 = graph_node_get(i2);
	      is1[is_critical] > is2[is_critical] ||
	      is1[is_critical] == is2[is_critical] &&
		is1[is_depends] > is2[is_depends]
	    ];
	  
	  canrun = fn (i)
	    [
	      | is, ins, itype |
		  
	      is = graph_node_get(i);
	      ins = is[is_il][sparc:il_ins];
	      itype = ins[sparc:i_type];
	      if (itype == sparc:ins_branch || itype == sparc:ins_jmpl ||
		  itype == sparc:ins_call)
		// Schedule as last instruction, or next to last if
		// delay slot available
		if (is[is_delayfree]) icount <= 2
		else icount == 1
		else
		  true	// can run anytime
	    ];
	      
	  pick_ins = fn ()
	    [
	      | try, best |
	      
	      try = ready;
	      best = false;
	      while (try != null)
		[
		  | itry |
		  
		  itry = car(try);
		  try = cdr(try);
		  if (canrun(itry))
		    if (!best || better(itry, best)) best = itry;
		];
	      best
	    ];
	  
	  completed = fn (i)
	    [
	      | successors |
	      
	      successors = lmap(graph_edge_to, graph_edges_out(i));
	      graph_purge_node(i);
	      // Add all successors with no remaining dependences
	      lforeach(fn (newi)
		       if (graph_edges_in(newi) == null)
		       ready = newi . ready,
		       successors);
	    ];
	  
	  while (icount > 0)
	    [
	      | scheduled |
	      
	      if (scheduled = pick_ins())
		[
		  | delayed, sis, sil, label |
		  
		  ready = ldelete!(scheduled, ready);
		  sis = graph_node_get(scheduled);
		  sil = sis[is_il];
		  //display("picked "); sparc:print_ins(sis[is_il][sparc:il_ins], false, false); newline();
		  running = (scheduled . t + sis[is_time]) . running;
		  if (newlist != null && (label = sil[sparc:il_label]))
		    [
		      // Label on this instruction should be moved to
		      // first instruction (there is at most one label
		      // per basic block)
		      sparc:set_label(label, dget(newlist));
		      sil[sparc:il_label] = false;
		    ];
		  newlist = dmerge!(newlist, dcons!(sil, null));
		  icount = icount - 1;

		  // add the explicit delay slot instruction if necessary
		  delayed = sis[is_delay];
		  if (delayed && (icount == 0 || !sis[is_delayfree]))
		    newlist = dmerge!(newlist, dcons!(delayed, null));

		  if (delayed)
		    [
		      nslots = nslots + 1;
		      // If delay slot is used, or will be used:
		      if (icount == 1 || !sis[is_delayfree])
			nfilled = nfilled + 1
		      else
			[
			  | sitype |

			  sitype = sil[sparc:il_ins][sparc:i_type];
			  if (sitype == sparc:ins_branch)
			    nunfilled_branch = nunfilled_branch + 1
			  else if (sitype == sparc:ins_call)
			    nunfilled_call = nunfilled_call + 1
			  else
			    nunfilled_jump = nunfilled_jump + 1;
			];
		    ];
	      ];
	      t = t + 1;
	      // Check completed instructions
	      running = lfilter(fn (runs)
				  if (cdr(runs) <= t)
				    [
				      completed(car(runs));
				      false
				    ]
				  else
				    true,
				running);
	    ];
	  //display(format("adding %d\n", dlength(newlist)));
	  newlist
	];

      start = ilist;
      loop
	[
	  | depgraph, roots |

	  depgraph = new_graph();
	  ilist = schedule_dependences(ilist, depgraph);
	  roots = graph_roots(depgraph);
	  schedule_costs(depgraph, roots);
	  //display(format("before %d\n", dlength(newlist)));
	  newlist = dmerge!(newlist, schedule_generate(depgraph, roots));
	  //display(format("after %d\n", dlength(newlist)));

	  if (ilist == start) exit newlist;
	]
    ];
*/

  delayslotok = fn (ins)
    // Returns: True if ins can live in a delay slot
    [
      | itype, iop |

      itype = ins[sparc:i_type];
      iop = ins[sparc:i_op];
      
      itype == sparc:ins_alu && !(iop == sparc:op_sdiv || iop == sparc:op_udiv)
      // || itype == sparc:ins_sethi -- not ok, linker/gc need to find sethi/or
      // pair (one exception: sethi for a large integer constant, but not
      // common enough to be work checking for)
    ];

  simple_scheduler = fn (fcode, ilist)
    // Types: ilist: list of sparc instructions
    // Effects: Does code scheduling on ilist and fills delay slots
    //   Currently very simple: fill delay slots after call from
    //   instruction before it (if possible).
    [
      | scan, candidate, delay_call, delay_jump, delay_branch |

      scan = ilist;
      candidate = false;

      remove_aliases(ilist);

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
		[
		  sparc:set_label(cil[sparc:il_label], dget(dnext(candidate)));
		  cil[sparc:il_label] = false;
		];

	      ilist = dremove!(candidate, ilist);
	      dremove!(scan, scan);
	      dcons!(dget(candidate), next);
	      nfilled = nfilled + 1;
	      nslots = nslots + 1;
	    ]
	  else
	    [
	      // candidates survive by default
	      ncandidate = candidate;

	      // can't move through a label
	      if (il[sparc:il_label]) ncandidate = false;

	      if (itype == sparc:ins_call)
		[
		  ndelay_call = true;
		  ncalls = ncalls + 1;
		]
	      else if (itype == sparc:ins_branch)
		[
		  // unconditional annulled branches have no delay slot ...
		  ndelay_branch = 
		    !(ins[sparc:i_arg2] && ins[sparc:i_op] == sparc:balways);
		  if (ndelay_branch)
		    nbranches = nbranches + 1
		  else
		    nbalways = nbalways + 1;
		]
	      else if (itype == sparc:ins_jmpl)
		[
		  ndelay_jump = true;
		  njumps = njumps + 1;
		]
	      else if (delay_branch || delay_call || delay_jump)
		// candidates don't survive past delay slots
		// also instruction in delay slot is not a candidate
		[
		  ncandidate = false;
		  if (nop)
		    if (delay_branch)
		      [
			| branch, target, targetins |

			branch = dget(dprev(scan))[sparc:il_ins];
			target = branch[sparc:i_arg1][sparc:l_ins];
			targetins = target[sparc:il_ins];
			// Steal instruction from destination and annul
			// branch, if possible
			if (delayslotok(targetins))
			  [
			    | newdest, successor |

			    newdest = sparc:new_label(fcode);
			    // urk: no back pointers around :-(
			    successor = ilist;
			    while (dget(successor) != target)
			      successor = dnext(successor);
			    successor = dnext(successor);
			    sparc:set_label(newdest, dget(successor));
			    branch[sparc:i_arg1] = newdest;
			    branch[sparc:i_arg2] = true; // annul
			    nannull_filled = nannull_filled + 1;
			    nfilled = nfilled + 1;

			    sparc:set_instruction(fcode, next);
			    sparc:copy_instruction(fcode, target);
			    sparc:set_instruction(fcode, ilist);
			    dremove!(scan, scan); // remove nop
			  ]
			else
			  nunfilled_branch = nunfilled_branch + 1
		      ]
		    else if (delay_call) nunfilled_call = nunfilled_call + 1
		    else nunfilled_jump = nunfilled_jump + 1
		  else nfilled = nfilled + 1;
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
