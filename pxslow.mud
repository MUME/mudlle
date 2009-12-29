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

  // Compiler x86 compiler as protected modules

trap_error(fn() [
  mc:verbose = 0;
  x86:reset_counters();
  start = ctime();

  safecomp = fn (s)
    if (!pcompile(s))
      [
        display("Failed!"); newline();
        quit(1) 
      ];
  
  safecomp("compiler/dihash.mud");
  safecomp("compiler/compiler.mud");
  safecomp("compiler/link.mud");
  safecomp("compiler/misc.mud");
  safecomp("compiler/sequences.mud");
  safecomp("compiler/dlist.mud");
  safecomp("compiler/graph.mud");
  safecomp("compiler/ax86.mud");
  safecomp("compiler/vars.mud");
  safecomp("compiler/flow.mud");
  safecomp("compiler/optimise.mud");
  safecomp("compiler/ins3.mud");
  safecomp("compiler/mx86.mud");
  safecomp("compiler/phase1.mud");
  safecomp("compiler/phase2.mud");
  safecomp("compiler/phase3.mud");
  safecomp("compiler/phase4.mud");
  safecomp("compiler/genx86.mud");
  safecomp("compiler/x86.mud");
  safecomp("compiler/compile.mud");
  safecomp("compiler/noinf.mud");
  safecomp("compiler/inference.mud");

  display(format("cpu time: %s ms%n", ctime() - start));
  display(format("nins:   %s%n", nins));
  display(format("nbytes: %s%n", nbytes));
], fn (n) [
  quit(1);
], call_trace_on);
