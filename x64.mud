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

library mp
requires ax64, genx64, mx64
defines mp:nscratch, mp:ncaller, mp:ncallee, mp:nregargs, mp:new_label,
  mp:ins_list, mp:assemble, mp:mgen_preamble, mp:mgen_instruction,
  mp:select_registers, mp:migrate, mp:uses_scratch?, mp:reset_counters
[
  mp:nscratch = fn (ifn) x64:nscratch(ifn);
  mp:ncaller = fn (ifn) x64:ncaller(ifn);
  mp:ncallee = fn (ifn) x64:ncallee(ifn);
  mp:nregargs = fn (ifn) x64:nregargs(ifn);

  mp:new_label = fn (x) x64:new_label(x);
  mp:ins_list = fn (x) x64:ins_list(x);
  mp:assemble = fn (x) x64:gassemble(x);
  mp:mgen_preamble = fn (x1, x2) x64:mgen_preamble(x1, x2);
  mp:mgen_instruction = fn (x1, x2, x3, x4)
    x64:mgen_instruction(x1, x2, x3, x4);
  mp:select_registers = fn (x1, x2) x64:select_registers(x1, x2);
  mp:migrate = fn v apply(x64:migrate, v);
  mp:uses_scratch? = fn (ins) x64:uses_scratch?(ins);
  mp:reset_counters = fn () x64:reset_counters();
];
