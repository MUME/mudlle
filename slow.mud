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

// Compiler sparc compiler
mc:verbose = 0;
sparc:reset_counters();
start = ctime();
fcompile("compiler.mud");
fcompile("link.mud");
fcompile("misc.mud");
fcompile("sequences.mud");
fcompile("dlist.mud");
fcompile("graph.mud");
fcompile("asparc.mud");
fcompile("vars.mud");
fcompile("flow.mud");
fcompile("optimise.mud");
fcompile("ins3.mud");
fcompile("msparc.mud");
fcompile("phase1.mud");
fcompile("phase2.mud");
fcompile("phase3.mud");
fcompile("phase4.mud");
fcompile("gensparc.mud");
fcompile("sparc.mud");
fcompile("compile.mud");
fcompile("noinf.mud");
fcompile("inference.mud");
display(format("cpu time: %s ms", ctime() - start)); newline();
display(format("nins: %s", nins)); newline();

