library mp
requires gen68k, m68k, a68k
defines mp:nscratch, mp:ncaller, mp:ncallee, mp:nregargs, mp:new_label,
  mp:ins_list, mp:assemble, mp:mgen_preamble, mp:mgen_instruction, 
  mp:select_registers
[

  /*
  mp:nscratch = m68k:nscratch;
  mp:ncaller = m68k:ncaller;
  mp:ncallee = m68k:ncallee;
  mp:nregargs = m68k:nregargs;

  mp:new_label = m68k:new_label;
  mp:ins_list = m68k:ins_list;
  mp:assemble = m68k:assemble;
  mp:mgen_preamble = m68k:mgen_preamble;
  mp:mgen_instruction = m68k:mgen_instruction;
  mp:select_registers = m68k:select_registers;
  */

  mp:nscratch = fn (ifn) m68k:nscratch(ifn);
  mp:ncaller = fn (ifn) m68k:ncaller(ifn);
  mp:ncallee = fn (ifn) m68k:ncallee(ifn);
  mp:nregargs = fn (ifn) m68k:nregargs(ifn);

  mp:new_label = fn (x) m68k:new_label(x);
  mp:ins_list = fn (x) m68k:ins_list(x);
  mp:assemble = fn (x) m68k:assemble(x);
  mp:mgen_preamble = fn (x1, x2) m68k:mgen_preamble(x1, x2);
  mp:mgen_instruction = fn (x1, x2, x3, x4) m68k:mgen_instruction(x1, x2, x3, x4);
  mp:select_registers = fn (x1, x2) m68k:select_registers(x1, x2);

];
