library compile // call actual compiler
requires system, sequences, compiler, phase1, phase2, phase3, phase4, link, ins3
defines mc:compile
writes mc:verbose, mc:erred, mc:this_module
[
  | all_functions |

  mc:verbose = 2; // default verbosity level

  mc:compile = fn (mod, protect)
    if (mod)
      [
	mc:erred = false;
	mc:this_module = mod;

	if (mc:verbose >= 1)
	  [
	    display("PHASE1");
	    newline();
	  ];
	mc:phase1(mod);

	if (mc:verbose >= 1)
	  [
	    display("PHASE2");
	    newline();
	  ];
	mc:phase2(mod);

	if (!mc:erred)
	  [
	    | fns |

	    if (mc:verbose >= 4)
	      [
		mc:ins_list(mod[mc:m_body]);
		newline();
	      ];

	    fns = all_functions(mod[mc:m_body]);

	    if (mc:verbose >= 1)
	      [
		display("PHASE3");
		newline();
	      ];
	    mc:phase3(fns);

	    if (mc:verbose >= 1)
	      [
		display("PHASE4");
		newline();
	      ];
	    mc:phase4(fns);

	    mc:prelink(mod, protect)
	  ]
	else
	  false
      ]
    else
      false;

  all_functions = fn (ifn)
    // Types: ifn : intermediate function
    // Effects: Returns all the functions in ifn
    [
      | todo, fns |

      todo = ifn . null;

      while (todo != null)
	[
	  | first |
	  first = car(todo);
	  todo = cdr(todo);

	  fns = first . fns;
	  dforeach(fn (ins) [
	             | i |
		     i = ins[mc:il_ins];
		     if (i[mc:i_class] == mc:i_closure)
		       todo = i[mc:i_ffunction] . todo;
		   ], first[mc:c_fvalue]);
	];
      fns
    ];

];
