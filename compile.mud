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

library compile // call actual compiler
requires compiler, flow, ins3, link, phase1, phase2, phase3, phase4, sequences,
  vars
defines mc:compile
writes mc:verbose, mc:erred, mc:this_module, mc:disassemble
[
  mc:verbose = 2; // default verbosity level
  mc:disassemble = false;

  mc:compile = fn (mod, protect, int seclev)
    [
      if (!mod) exit<function> false;

      if (mc:disassemble && mc:verbose < 1)
        mc:reset_closure_count();

      mc:erred = false;
      mc:this_module = mod;

      mc:sort_messages(true);

      | result |
      result = false;

      <erred> [
        if (mc:verbose >= 1)
          [
            display("PHASE1\n");
          ];
        mc:phase1(mod, seclev);
        if (mc:erred) exit<erred> null;

        if (mc:verbose >= 1)
          display("PHASE2\n");
        mc:phase2(mod);
        if (mc:erred) exit<erred> null;

        if (mc:verbose >= 4)
          [
            mc:ins_list(mod[mc:m_body]);
            newline();
          ];

        | fns |
        fns = mc:all_functions(mod[mc:m_body]);

        if (mc:verbose >= 1)
          [
            display("PHASE3\n");
          ];
        mc:phase3(fns);

        | remaining_fns |
        remaining_fns = mc:all_functions(mod[mc:m_body]);
        fns = lfilter!(fn (f) lfind?(f, remaining_fns), fns);

        if (mc:verbose >= 1)
          [
            display("PHASE4\n");
          ];
        mc:phase4(fns);

        result = mc:prelink(mod, protect)
      ];

      mc:sort_messages(false);

      mc:reset_var_cache();

      result
    ];

];
