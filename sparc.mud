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
requires system, gensparc, msparc, asparc
defines mp:nscratch, mp:ncaller, mp:ncallee, mp:nregargs, mp:new_label,
  mp:ins_list, mp:assemble, mp:mgen_preamble, mp:mgen_instruction, 
  mp:select_registers, mp:migrate
[

  /*
  mp:nscratch = sparc:nscratch;
  mp:ncaller = sparc:ncaller;
  mp:ncallee = sparc:ncallee;
  mp:nregargs = sparc:nregargs;

  mp:new_label = sparc:new_label;
  mp:ins_list = sparc:ins_list;
  mp:assemble = sparc:gassemble;
  mp:mgen_preamble = sparc:mgen_preamble;
  mp:mgen_instruction = sparc:mgen_instruction;
  mp:select_registers = sparc:select_registers;
  mp:migrate = sparc:migrate;
  */

  mp:nscratch = fn (ifn) sparc:nscratch(ifn);
  mp:ncaller = fn (ifn) sparc:ncaller(ifn);
  mp:ncallee = fn (ifn) sparc:ncallee(ifn);
  mp:nregargs = fn (ifn) sparc:nregargs(ifn);

  mp:new_label = fn (x) sparc:new_label(x);
  mp:ins_list = fn (x) sparc:ins_list(x);
  mp:assemble = fn (x) sparc:gassemble(x);
  mp:mgen_preamble = fn (x1, x2) sparc:mgen_preamble(x1, x2);
  mp:mgen_instruction = fn (x1, x2, x3, x4) sparc:mgen_instruction(x1, x2, x3, x4);
  mp:select_registers = fn (x1, x2) sparc:select_registers(x1, x2);
  mp:migrate = fn v apply(sparc:migrate, v);
];
