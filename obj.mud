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

[
  | fname |

  fname = fn (prelinked_fn)
    format("%s at %s:%s",
	   if (prelinked_fn[2] != null) prelinked_fn[2] else "<fn>",
	   prelinked_fn[3], prelinked_fn[4]);

  mc:obj_find = fn (prelinked_fn, name)
    [
      | csts, info, see_global, done |

      done = false;

      foreach(fn (off)
	        if (!done && string_icmp(car(off), name) == 0)
	          [
		    display("constant "); display(fname(prelinked_fn));
		    newline();
		    done = true;
		  ],
	      prelinked_fn[10]);

      if (!done)
	[
	  see_global = fn (off)
	    if (!done && string_icmp(car(off), name) == 0)
	      [
		display(fname(prelinked_fn));
		newline();
		done = true;
	      ];

	  foreach(see_global, prelinked_fn[9]);
	  foreach(see_global, prelinked_fn[8]);
	];

      foreach(fn (off) mc:obj_find(car(off), name), prelinked_fn[5]);

    ];

  mc:obj_global_write = fn (prelinked_fn)
    [
      | gw |

      gw = fn (pfn)
	[
	  if (pfn[9] != null)
	    [
	      display(format("%s: %s", fname(pfn),
			     concat_words(mapcar(car, pfn[9]), " ")));
	      newline();
	    ];
	  foreach(fn (off) gw(car(off)), pfn[5]);
	];
      // Ignore top-level assigns
      foreach(fn (off) gw(car(off)), prelinked_fn[5]);
    ];

  mc:obj_find_names = fn (fnames, vname)
    foreach(fn (fname)
	    [
	      display(fname); newline();
	      mc:obj_find(load_data(fname), vname)
	    ], fnames);

  mc:obj_names_op = fn (fnames, op)
    foreach(fn (fname)
	    [
	      display(fname); newline();
	      op(load_data(fname))
	    ], fnames);

  mc:size = fn (prelinked_fn)
    string_length(prelinked_fn[0]) +
      lreduce(fn (x, sum) sum + mc:size(car(x)), 0, prelinked_fn[5]);
];
