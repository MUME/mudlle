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

load_library = fn (string s)
[
  silent == true || dformat("loading %s%n", s);
  if (load(s + ".mud"))
    silent == true || dformat("loaded %s%n", s)
  else
    error(error_bad_value);
];

[
  // this is just a dummy implementation for testing purposes
  | st |
  st = make_table();
  get_static = symbol fn (string name)
    [
      | sym |
      sym = table_lookup(st, name);
      if (!sym)
        [
          st[name] = 0;
          sym = table_lookup(st, name);
          st[name] = null;
        ];
      sym
    ];
];
