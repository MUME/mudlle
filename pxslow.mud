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

  // Compiler x86 compiler as protected modules

trap_error(fn() [
  mc:verbose = 0;
  x86:reset_counters();
  start = ctime();

  safecomp = fn (s)
    if (!pcompile(s))
      [
        display("Failed!\n");
        quit(1)
      ];

  safecomp("dihash.mud");
  safecomp("compiler.mud");
  safecomp("link.mud");
  safecomp("misc.mud");
  safecomp("sequences.mud");
  safecomp("dlist.mud");
  safecomp("graph.mud");
  safecomp("ax86.mud");
  safecomp("vars.mud");
  safecomp("flow.mud");
  safecomp("optimise.mud");
  safecomp("ins3.mud");
  safecomp("mx86.mud");
  safecomp("phase1.mud");
  safecomp("phase2.mud");
  safecomp("phase3.mud");
  safecomp("phase4.mud");
  safecomp("genx86.mud");
  safecomp("x86.mud");
  safecomp("compile.mud");
  safecomp("noinf.mud");
  safecomp("inference.mud");

  dformat("cpu time: %s ms%n", ctime() - start);
  dformat("nins:   %s%n", nins);
  dformat("nbytes: %s%n", nbytes);
], fn (n) [
  quit(1);
], call_trace_on, true);
