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

library compiler
requires system, vars

defines mc:c_class, mc:c_assign, mc:c_asymbol, mc:c_avalue, mc:c_recall,
  mc:c_rsymbol, mc:c_constant, mc:c_cvalue, mc:c_closure, mc:c_freturn_type,
  mc:c_fhelp, mc:c_fargs, mc:c_fvarargs, mc:c_fvalue, mc:c_flineno,
  mc:c_ffilename, mc:c_fargtypes, mc:c_fvar, mc:c_flocals, mc:c_flocals_write,
  mc:c_fclosure, mc:c_fclosure_write, mc:c_fglobals, mc:c_fglobals_write,
  mc:c_fnumber, mc:c_fmisc, mc:c_fm_argsbase, mc:c_fm_closurebase, mc:c_fnvars,
  mc:c_fallvars, mc:c_execute, mc:c_efnargs, mc:c_builtin, mc:c_bfn, mc:c_bargs,
  mc:c_block, mc:c_klocals, mc:c_ksequence, mc:c_labeled, mc:c_lname,
  mc:c_lexpression, mc:c_exit, mc:c_ename, mc:c_eexpression, mc:b_or, mc:b_and,
  mc:b_sc_or, mc:b_sc_and, mc:b_eq, mc:b_ne, mc:b_lt, mc:b_le, mc:b_gt, mc:b_ge,
  mc:b_bitor, mc:b_bitxor, mc:b_bitand, mc:b_shift_left, mc:b_shift_right,
  mc:b_add, mc:b_subtract, mc:b_multiply, mc:b_divide, mc:b_remainder,
  mc:b_negate, mc:b_not, mc:b_bitnot, mc:b_ifelse, mc:b_if, mc:b_while,
  mc:b_loop, mc:b_ref, mc:b_set, mc:b_cons, mc:b_assign, mc:b_car, mc:b_cdr,
  mc:m_class, mc:m_plain, mc:m_module, mc:m_library, mc:m_name, mc:m_imports, 
  mc:m_defines, mc:m_reads, mc:m_writes, mc:m_body, mc:b_slength, mc:b_vlength,
  mc:b_iadd, mc:c_fm_globalsbase, mc:c_fm_regs_callee,

  mc:a_builtins, mc:a_constants, mc:a_subfns, mc:a_globals, mc:a_kglobals, 
  mc:a_primitives,

  mc:fname, mc:error, mc:warning

reads mc:this_module, mc:this_function
writes mc:erred

[
  | message, m_name, fname, c_flineno, c_ffilename, c_fvar, v_name |

  // Structure of parse tree returned by mudlle_parse:
  //   7-element vector:
  mc:m_class = 0;		// module class
   mc:m_plain = 0;		// old-style, no defines, requires, etc
   mc:m_module = 1;		// a library w/o any defines (name optional)
   mc:m_library = 2;		// a full library
  
  mc:m_name = 1;		// module name (string or false)
  mc:m_imports = 2;		// imported modules (list of (string . type)) (type meaningless)
  mc:m_defines = 3;		// defined variables (list of (string . type))
  mc:m_reads = 4;		// read variables (list of (string . type))
  mc:m_writes = 5;		// written variables (list of (string . type))
  mc:m_body = 6;		// module body (component)
  
  // Component structure:
  //   It is a vector whose first element is one of mc:c_assign, c_recall, etc
  //   The remaining elements depend on the value of the first, as follows:
  
  mc:c_class = 0;		// class of component
  
  mc:c_assign = 0;		// assignment statement
   mc:c_asymbol = 1;		// var name (string, after phase1: var)
   mc:c_avalue = 2;		// value (component)
  
  mc:c_recall = 1;		// value of a variable
   mc:c_rsymbol = 1;		// var name (string, after phase1: var)
  
  mc:c_constant = 2;		// a constant
   mc:c_cvalue = 1;		// value of constant (any type)
  
  mc:c_closure = 3;		// a function
   mc:c_freturn_type = 1;	// return type
   mc:c_fhelp = 2;		// help string (string or null)
   mc:c_fargs = 3;		// argument names (list of (string . type), after phase1: list of var)
   mc:c_fvarargs = 4;		// true if this is a varargs function
   mc:c_fvalue = 5;		// function value (component)
   mc:c_flineno = 6;		// line number (integer)
   mc:c_ffilename = 7;		// filename (string)
  // The next fields are added by phase1
   mc:c_fargtypes = 8;		// argument types (list of type)
   mc:c_fvar = 9;		// variable in which function is stored
   mc:c_flocals = 10;		// local variables (list of var)
   mc:c_flocals_write = 11;	// local variables (list of var) (those written)
   mc:c_fclosure = 12;		// closure variables (list of var)
   mc:c_fclosure_write = 13;	// closure variables (list of var) (those written)
   mc:c_fglobals = 14;		// the global variables used (list of var)
   mc:c_fglobals_write = 15;	// the global variables written (list of var)
   // The next field is set by phase2
   mc:c_fnumber = 0;		// a unique number for this closure (int, display)
   // Set by phase 4:
   mc:c_fmisc = 16;		// miscellaneous info
   mc:c_fm_argsbase = 0;	// true if function needs arguments base
   mc:c_fm_closurebase = 1;	// true if function needs closure base
   mc:c_fm_globalsbase = 2;	// true if function needs globals base
   mc:c_fm_regs_callee = 3;     // callee registers used by the backend
   mc:c_fnvars = 17;		// number of vars (global, closure, local) used (int, phase3)
   mc:c_fallvars = 18;		// all vars (global, closure, local) used (vector, phase3)
  
  mc:c_execute = 4;		// execute a function
   mc:c_efnargs = 1;		// list of function, followed by arguments (list of component)
  
  mc:c_builtin = 5;		// execute a primitive
   mc:c_bfn = 1;		// number of primitive (integer, see below)
   mc:c_bargs = 2;		// arguments (list of component)
  
  mc:c_block = 6;		// a block
   mc:c_klocals = 1;		// local variables (list of string)
   mc:c_ksequence = 2;		// code (list of component)
  
  mc:c_labeled = 7;		// labeled expression
   mc:c_lname = 1;		// label name (string)
   mc:c_lexpression = 2;	// expression value (component)
  
  mc:c_exit = 8;
   mc:c_ename = 1;		// label name (string or null)
   mc:c_eexpression = 2;	// exit expression (component)
  
  // language primitives, some are functions, others are control structures
  mc:b_or = 0;
  mc:b_and = 1;
  mc:b_sc_or = 2;
  mc:b_sc_and = 3;
  mc:b_eq = 4;
  mc:b_ne = 5;
  mc:b_lt = 6;
  mc:b_le = 7;
  mc:b_gt = 8;
  mc:b_ge = 9;
  mc:b_bitor = 10;
  mc:b_bitxor = 11;
  mc:b_bitand = 12;
  mc:b_shift_left = 13;
  mc:b_shift_right = 14;
  mc:b_add = 15;
  mc:b_subtract = 16;
  mc:b_multiply = 17;
  mc:b_divide = 18;
  mc:b_remainder = 19;
  mc:b_negate = 20;
  mc:b_not = 21;
  mc:b_bitnot = 22;
  mc:b_ifelse = 23;
  mc:b_if = 24;
  mc:b_while = 25;
  mc:b_loop = 26;
  mc:b_ref = 27;
  mc:b_set = 28;
  mc:b_cons = 29;
  // Compiler generated ops
  mc:b_assign = 30;
  mc:b_car = 31;
  mc:b_cdr = 32;
  mc:b_slength = 33;
  mc:b_vlength = 34;
  mc:b_iadd = 35; // integer addition

  // Format of information returned with assembled code
  mc:a_builtins = 0;
  mc:a_constants = 1;
  mc:a_subfns = 2;
  mc:a_globals = 3;
  mc:a_kglobals = 4;
  mc:a_primitives = 5;
  
  m_name = mc:m_name; // strange unload effects (see comment before linkun in link.mud)
  c_flineno = mc:c_flineno;
  c_ffilename = mc:c_ffilename;
  c_fvar = mc:c_fvar;
  v_name = mc:v_name;

  fname = mc:fname = fn (ifn)
    // Types: ifn: intemediate function
    // Returns: A printable name for ifn
    format("%s[%s:%s]",
	   if (c_fvar < vector_length(ifn) && ifn[c_fvar])
	     ifn[c_fvar][v_name] else "<fn>",
	   ifn[c_ffilename], ifn[c_flineno]);

  mc:error = fn args
    [
      message("error", args);
      mc:erred = true;
    ];

  mc:warning = fn args
    message("warning", args);

  message = fn (type, args)
    [
      | msg |

      msg = apply(format, args);
      display(format("%s:%s %s: %s",
		     if (vector?(mc:this_module) && mc:this_module[m_name])
		       mc:this_module[m_name] else "?",
		     if (vector?(mc:this_function) && mc:this_function[c_flineno] >= 0)
		       fname(mc:this_function) else "",
		     type, msg));
      newline();
    ];
];
