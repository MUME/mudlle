/* 
 * Copyright (c) 1993-2006 David Gay
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

library compile // call actual compiler
requires system, compiler, phase1, phase2, phase3, phase4, link, ins3, flow
defines mc:compile
writes mc:verbose, mc:erred, mc:this_module
[
  mc:verbose = 2; // default verbosity level

  mc:compile = fn (mod, protect)
    if (mod)
      [
        | result |

	mc:erred = false;
	mc:this_module = mod;

        mc:sort_messages(true);

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

        result = if (!mc:erred)
	  [
	    | fns |

	    if (mc:verbose >= 4)
	      [
		mc:ins_list(mod[mc:m_body]);
		newline();
	      ];

	    fns = mc:all_functions(mod[mc:m_body]);

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
	  false;
        
        mc:sort_messages(false);

        result
      ]
    else
      false;

];
