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

| arch |
arch = if (INTBITS == 31) "x86" else "x64";

trap_error(fn() [
  | slice, slices |
  slice = 0;
  slices = 1;

  // optional two arguments N and M lets the user pick the N'th out of
  // M subsets of files to compile
  match (argv)
    [
      [_ s ss] => [
        slice = atoi(s);
        slices = atoi(ss);
        assert(slice >= 0 && slice < slices);
      ];
      [_] => null;
      _ => fail();
    ];

  mc:verbose = 0;
  mp:reset_counters();
  start = ctime();

  safecomp = fn (s)
    if (!pcompile(s))
      [
        display("Failed!\n");
        quit(1)
      ];

  | mfiles |
  // sort by size
  mfiles = vmap(fn (f) [ f += ".mud"; f . file_stat(f)[fs_size] ], '[
    ,("a" + arch)
    "compile"
    "compiler"
    "dihash"
    "dlist"
    "flow"
    ,("gen" + arch)
    "graph"
    "inference"
    "ins3"
    "link"
    "misc"
    ,("m" + arch)
    "noinf"
    "optimise"
    "phase1"
    "phase2"
    "phase3"
    "phase4"
    "sequences"
    "vars"
    ,arch
  ]);
  vqsort!(fn (a, b) cdr(a) > cdr(b), mfiles);

  for (| n | n = slice; n < vlength(mfiles); n += slices)
    safecomp(car(mfiles[n]));

  if (silent != true)
    [
      dformat("cpu time: %s ms%n", ctime() - start);
      dformat("nins:   %s%n", mc:nins);
      dformat("nbytes: %s%n", mc:nbytes);
    ]
], fn (n) [
  quit(1);
], call_trace_on);
