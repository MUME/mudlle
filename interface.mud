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

eval = fn (s)
  [
    | prelinked |

    if (prelinked = mc:compile(mudlle_parse(s), false))
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

    display(format("compiling %s", s)); newline();
    if (prelinked = mc:compile(mudlle_parse_file(s, s), protect))
      save_data(objname, prelinked)
  ];

fcompile = fn (s) lcompile(s, false);
pcompile = fn (s) lcompile(s, true);
fload = fn (s) mc:linkrun(load_data(s), 1, true);
test = fn (s) mc:compile(mudlle_parse(s), false);
ftest = fn (s) mc:compile(mudlle_parse_file(s, s), false);
