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

eval = fn (s)
  [
    | prelinked |

    if (prelinked = mc:compile(mudlle_parse(s, null), false, 1))
      mc:linkrun(prelinked, 1, true)
    else
      false
  ];

lcompile = fn (s, protect)
  [
    | objname, dot, prelinked |

    dot = string_index(s, ?.);
    if (dot < 0) objname = s + ".obj"
    else objname = substring(s, 0, dot) + ".obj";

    silent == true || dformat("compiling %s\n", s);
    | cs |
    cs = "compiler/" + s;
    if (prelinked = mc:compile(mudlle_parse_file(s, cs, cs),
                               protect, 1))
      save_data(objname, prelinked);
    !!prelinked
  ];

fcompile = fn (s) lcompile(s, false);
pcompile = fn (s) lcompile(s, true);
fload = fn (s) mc:linkrun(load_data(s), 1, true);
test = fn (s) mc:compile(mudlle_parse(s, null), false, 1);
ftest = fn (s) mc:compile(mudlle_parse_file(s, s, s), false, 1);

protect_compiler_libs = fn()
[
  | cmods |
  cmods = '[
    "dihash"
    "compiler"
    "link"
    "misc"
    "sequences"
    "dlist"
    "graph"
    "ax86"
    "vars"
    "flow"
    "optimise"
    "ins3"
    "mx86"
    "phase1"
    "phase2"
    "phase3"
    "phase4"
    "genx86"
    "x86"
    "compile"
    "noinf"
    "inference"
  ];

  for (|i, l| [ l = vlength(cmods); i = 0 ]; i < l; ++i)
    module_set!(cmods[i], module_protected, 1);
  detect_immutability();
];
