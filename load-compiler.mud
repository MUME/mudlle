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
  // Bootstrap via interpreter (load linker)
  load_library = fn (s) basic_load("mudlle/compiler/" + s + ".mud",
				   "compiler/" + s + ".mud", LVL_IMPLEMENTOR, true);
  load_library("link");
  
  // Now (re)load compiled compiler
  load_library = fn (s) mc:linkrun(load_data("obj-mudlle/compiler/" + s + ".obj"),
				   LVL_IMPLEMENTOR, true);

  // Reload interpreted modules. Order is important (the compiler uses
  // protected modules, and must not see the previously loaded ones).
  load_library("dlist");
  load_library("sequences");
  load_library("misc");
  load_library("vars");
  load_library("compiler");
  load_library("link");

  // Load compiler
  load_library("noinf"); // no-inference version of inference library
  load_library("x86"); // x86 back-end
  load_library("compile"); // hurrah!

  mc:verbose = 0;
];

