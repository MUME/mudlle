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

    if (prelinked = mc:compile(mudlle_parse(s), false, 1))
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
test = fn (s) mc:compile(mudlle_parse(s), false, 1);
ftest = fn (s) mc:compile(mudlle_parse_file(s, s, s), false, 1);

protect_compiler_libs = fn()
  [
    module_set!("dihash",    module_protected, 1);
    module_set!("compiler",  module_protected, 1);
    module_set!("link",      module_protected, 1);
    module_set!("misc",      module_protected, 1);
    module_set!("sequences", module_protected, 1);
    module_set!("dlist",     module_protected, 1);
    module_set!("graph",     module_protected, 1);
    module_set!("ax86",      module_protected, 1);
    module_set!("vars",      module_protected, 1);
    module_set!("flow",      module_protected, 1);
    module_set!("optimise",  module_protected, 1);
    module_set!("ins3",      module_protected, 1);
    module_set!("mx86",      module_protected, 1);
    module_set!("phase1",    module_protected, 1);
    module_set!("phase2",    module_protected, 1);
    module_set!("phase3",    module_protected, 1);
    module_set!("phase4",    module_protected, 1);
    module_set!("genx86",    module_protected, 1);
    module_set!("x86",       module_protected, 1);
    module_set!("compile",   module_protected, 1);
    module_set!("noinf",     module_protected, 1);
    module_set!("inference", module_protected, 1);
    detect_immutability();
  ];
