/* 
 * Copyright (c) 1993-2004 David Gay
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
mc:verbose = 0;
x86:reset_counters();
start = ctime();
pcompile("compiler/ihash.mud");
pcompile("compiler/compiler.mud");
pcompile("compiler/link.mud");
pcompile("compiler/misc.mud");
pcompile("compiler/sequences.mud");
pcompile("compiler/dlist.mud");
pcompile("compiler/graph.mud");
pcompile("compiler/ax86.mud");
pcompile("compiler/vars.mud");
pcompile("compiler/flow.mud");
pcompile("compiler/optimise.mud");
pcompile("compiler/ins3.mud");
pcompile("compiler/mx86.mud");
pcompile("compiler/phase1.mud");
pcompile("compiler/phase2.mud");
pcompile("compiler/phase3.mud");
pcompile("compiler/phase4.mud");
pcompile("compiler/genx86.mud");
pcompile("compiler/x86.mud");
pcompile("compiler/compile.mud");
pcompile("compiler/noinf.mud");
pcompile("compiler/inference.mud");
display(format("cpu time: %s ms", ctime() - start)); newline();
display(format("nins: %s", nins)); newline();

