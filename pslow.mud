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

// Compiler sparc compiler as protected modules
mc:verbose = 0;
start = ctime();
pcompile("dlist.mud");
pcompile("sequences.mud");
pcompile("misc.mud");
pcompile("compiler.mud");
pcompile("link.mud");
pcompile("graph.mud");
pcompile("asparc.mud");
pcompile("vars.mud");
pcompile("flow.mud");
pcompile("optimise.mud");
pcompile("ins3.mud");
pcompile("msparc.mud");
pcompile("phase1.mud");
pcompile("phase2.mud");
pcompile("phase3.mud");
pcompile("phase4.mud");
pcompile("gensparc.mud");
pcompile("sparc.mud");
pcompile("compile.mud");
pcompile("noinf.mud");
pcompile("inference.mud");
display(format("cpu time: %s ms", ctime() - start)); newline();
display(format("nins: %s", nins)); newline();

