// A mudlle profiler for use with compiled code (for which the interpeter's
// profiler isn't appropriate)

profile_table = make_table();
profile_size = 32;
profile_used = 0;
profile_data = make_vector(profile_size);
profile_count = make_vector(profile_size);

mprofile = fn (name, f)
  [
    | index |

    if ((index = profile_table[name]) == null)
      [
	index = profile_used;
	profile_used = profile_used + 1;
	if (index >= profile_size)
	  [
	    | nprofile_size, nprofile, ncprofile |
	    nprofile_size = profile_size * 2;
	    nprofile = make_vector(nprofile_size);
	    ncprofile = make_vector(nprofile_size);
	    for(0, profile_size - 1,
		fn (i) [ ncprofile[i] = profile_count[i];
			 nprofile[i] = profile_data[i] ]);
	    profile_size = nprofile_size;
	    profile_count = ncprofile;
	    profile_data = nprofile;
	  ];
	profile_table[name] = index;
	profile_data[index] = 0;
	profile_count[index] = 0;
      ];

    fn args
      [
	| start, result |

	start = ctime();
	result = apply(f, args);
	profile_data[index] = profile_data[index] + (ctime() - start);
	profile_count[index] = profile_count[index] + 1;
	result
      ]
  ];

mprofile_show = fn (name)
  [
    | index |

    index = profile_table[name];
    if (index == null) display("No such function")
    else display(format("%s: %s calls for %s ms", name,
			profile_count[index], profile_data[index]));
    newline();
  ];

mprofile_show_all = fn ()
  foreach(fn (sym) mprofile_show(symbol_name(sym)), table_list(profile_table));
