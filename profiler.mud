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
