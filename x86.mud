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

library mp
requires system, genx86, mx86, ax86
defines mp:nscratch, mp:ncaller, mp:ncallee, mp:nregargs, mp:new_label,
  mp:ins_list, mp:assemble, mp:mgen_preamble, mp:mgen_instruction, 
  mp:select_registers, mp:migrate
[

  /*
  mp:nscratch = x86:nscratch;
  mp:ncaller = x86:ncaller;
  mp:ncallee = x86:ncallee;
  mp:nregargs = x86:nregargs;

  mp:new_label = x86:new_label;
  mp:ins_list = x86:ins_list;
  mp:assemble = x86:gassemble;
  mp:mgen_preamble = x86:mgen_preamble;
  mp:mgen_instruction = x86:mgen_instruction;
  mp:select_registers = x86:select_registers;
  mp:migrate = x86:migrate;
  */

  mp:nscratch = fn (ifn) x86:nscratch(ifn);
  mp:ncaller = fn (ifn) x86:ncaller(ifn);
  mp:ncallee = fn (ifn) x86:ncallee(ifn);
  mp:nregargs = fn (ifn) x86:nregargs(ifn);

  mp:new_label = fn (x) x86:new_label(x);
  mp:ins_list = fn (x) x86:ins_list(x);
  mp:assemble = fn (x) x86:gassemble(x);
  mp:mgen_preamble = fn (x1, x2) x86:mgen_preamble(x1, x2);
  mp:mgen_instruction = fn (x1, x2, x3, x4) x86:mgen_instruction(x1, x2, x3, x4);
  mp:select_registers = fn (x1, x2) x86:select_registers(x1, x2);
  mp:migrate = fn v apply(x86:migrate, v);
];
