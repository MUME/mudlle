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

library compiler
requires vars, misc, sequences

defines mc:c_class, mc:c_lineno,

  mc:c_asymbol, mc:c_avalue,

  mc:c_rsymbol,

  mc:c_cvalue,

  mc:c_freturn_typeset, mc:c_fhelp, mc:c_fargs, mc:c_fvarargs, mc:c_fvalue,
  mc:c_flineno, mc:c_ffilename, mc:c_fnicename, mc:c_fargtypesets, mc:c_fvar,
  mc:c_flocals, mc:c_flocals_write, mc:c_fclosure, mc:c_fclosure_write,
  mc:c_fglobals, mc:c_fglobals_write, mc:c_fnoescape, mc:c_fnumber, mc:c_fmisc,
  mc:c_fnvars, mc:c_fallvars,

  mc:c_fm_argsbase, mc:c_fm_closurebase, mc:c_fm_globalsbase,
  mc:c_fm_regs_callee,

  mc:c_efnargs,

  mc:c_bfn, mc:c_bargs,

  mc:c_klocals, mc:c_ksequence,

  mc:c_lname, mc:c_lexpression,

  mc:c_ename, mc:c_eexpression,

  mc:b_sc_or, mc:b_sc_and, mc:b_eq, mc:b_ne, mc:b_lt, mc:b_le, mc:b_gt,
  mc:b_ge, mc:b_bitor, mc:b_bitxor, mc:b_bitand, mc:b_shift_left,
  mc:b_shift_right, mc:b_add, mc:b_subtract, mc:b_multiply, mc:b_divide,
  mc:b_remainder, mc:b_negate, mc:b_not, mc:b_bitnot, mc:b_ifelse, mc:b_if,
  mc:b_while, mc:b_loop, mc:b_ref, mc:b_set, mc:b_cons, mc:b_assign, mc:b_car,
  mc:b_cdr, mc:parser_builtins,

  mc:b_slength, mc:b_vlength, mc:b_iadd, mc:b_typeof, mc:b_loop_count,
  mc:b_max_loop_count, mc:b_symbol_name, mc:b_symbol_get, mc:builtins,
  mc:builtin_names,

  mc:m_class, mc:m_plain, mc:m_module, mc:m_library, mc:m_name, mc:m_imports,
  mc:m_defines, mc:m_reads, mc:m_writes, mc:m_body, mc:m_filename,
  mc:m_nicename,

  mc:a_builtins, mc:a_constants, mc:a_subfns, mc:a_globals, mc:a_kglobals,
  mc:a_primitives, mc:a_linenos, mc:a_rel_primitives,

  mc:fname, mc:error, mc:warning, mc:sort_messages,

  mc:mv_gidx, mc:mv_name, mc:mv_used, mc:mv_lineno,
  mc:muse_read, mc:muse_write,

  mc:register_call_check, mc:lookup_call_check

reads mc:this_filenames, mc:this_module, mc:this_function, mc:lineno
writes mc:erred

[
  | message, m_filename, m_nicename, simple_fname, c_flineno, c_ffilename,
    c_fnicename, c_fvar, v_name, pending_messages, sort_messages?,
    message_count |

  sort_messages? = false;
  message_count = 0;

  // Structure of parse tree returned by mudlle_parse:
  mc:m_class = 0;		// module class
   mc:m_plain = 0;		// old-style, no defines, requires, etc
   mc:m_module = 1;		// a library w/o any defines (name optional)
   mc:m_library = 2;		// a full library

  mc:m_name = 1;		// module name (string or false)
  mc:m_imports = 2;		// imported modules
                                //   vector(module_status, lineno, syms)
  mc:m_defines = 3;		// defined variables
  mc:m_reads = 4;		// read variables
  mc:m_writes = 5;		// written variables
  mc:m_body = 6;		// module body (component)
  mc:m_filename = 7;            // module file name
  mc:m_nicename = 8;            // module pretty-printed file name

  // the variable lists above are lists of (name . type) from mudlle_parse,
  // and vector(gidx, name, used) after mstart()
  mc:mv_gidx = 0;
  mc:mv_name = 1;
  mc:mv_used = 2;
    // flags in mc:mv_used
    mc:muse_read  = 1;
    mc:muse_write = 2;
  mc:mv_lineno = 3;


  // Component structure:
  //   It is a vector whose first element is one of mc:c_assign, c_recall, etc
  //   The remaining elements depend on the value of the first, as follows:

  mc:c_class = 0;		// class of component
  mc:c_lineno = 1;		// line number of component

  // mc:c_assign - assignment statement
   mc:c_asymbol = 2;		// var name (string, after phase1: var)
   mc:c_avalue = 3;		// value (component)

  // mc:c_recall - value of a variable
  // mc:c_vref   - (container of) a variable
   mc:c_rsymbol = 2;		// var name (string, after phase1: var)

  // mc:c_constant - a constant
   mc:c_cvalue = 2;		// value of constant (any type)

  // mc:c_closure - a function
   mc:c_freturn_typeset = 2;	// return typeset
   mc:c_fhelp = 3;		// help string (string or null)
   mc:c_fargs = 4;		// argument names (list of (string . type),
                                //     after phase1: list of var)
   mc:c_fvarargs = 5;		// true if this is a varargs function
   mc:c_fvalue = 6;		// function value (component)
   mc:c_flineno = 7;		// line number (integer)
   mc:c_ffilename = 8;		// filename on disk (string)
   mc:c_fnicename = 9;          // pretty-printed filename (string)

   // Added by phase1
   mc:c_fargtypesets = 10;	// argument typesets (list of typeset)
   mc:c_fvar = 11;		// variable in which function is stored
   mc:c_flocals = 12;		// local variables (list of var)
   mc:c_flocals_write = 13;	// local variables (list of var) (those
                                // written)
   mc:c_fclosure = 14;		// closure variables (list of var)
   mc:c_fclosure_write = 15;	// closure variables (list of var) (those
                                // written)
   mc:c_fglobals = 16;		// the global variables used (list of var)
   mc:c_fglobals_write = 17;	// the global variables written (list of var)

   mc:c_fnoescape = 21;         // true if no calls from here escapes (does not
                                // call op_noescape); may still write closure
                                // variables in mc:c_fclosure_write

   // Set by phase2:
   mc:c_fnumber = 0;		// a unique number for this closure (int,
                                // display)

   // Set by phase 4:
   mc:c_fmisc = 18;		// miscellaneous info
   mc:c_fm_argsbase = 0;	// true if function needs arguments base
   mc:c_fm_closurebase = 1;	// true if function needs closure base
   mc:c_fm_globalsbase = 2;	// true if function needs globals base
   mc:c_fm_regs_callee = 3;     // callee registers used by the backend
   mc:c_fnvars = 19;		// number of vars (global, closure, local) used
                                // (int, phase3)
   mc:c_fallvars = 20;		// all vars (global, closure, local) used
                                // (vector, phase3)

  // mc:c_execute - execute a function
   mc:c_efnargs = 2;		// list of function, followed by arguments
                                // (list of component)

  // mc:c_builtin - execute a primitive
   mc:c_bfn = 2;		// number of primitive (integer, see below)
   mc:c_bargs = 3;		// arguments (list of component)

  // mc:c_block - a block
   mc:c_klocals = 2;		// local variables (list of string)
   mc:c_ksequence = 3;		// code (list of component)

  // mc:c_labeled - labeled expression
   mc:c_lname = 2;		// label name (string)
   mc:c_lexpression = 3;	// expression value (component)

  // mc:c_exit
   mc:c_ename = 2;		// label name (string or null)
   mc:c_eexpression = 3;	// exit expression (component)

  // language primitives, some are functions, others are control structures
  mc:b_sc_or = 0;
  mc:b_sc_and = 1;
  mc:b_eq = 2;
  mc:b_ne = 3;
  mc:b_lt = 4;
  mc:b_le = 5;
  mc:b_gt = 6;
  mc:b_ge = 7;
  mc:b_bitor = 8;
  mc:b_bitxor = 9;
  mc:b_bitand = 10;
  mc:b_shift_left = 11;
  mc:b_shift_right = 12;
  mc:b_add = 13;
  mc:b_subtract = 14;
  mc:b_multiply = 15;
  mc:b_divide = 16;
  mc:b_remainder = 17;
  mc:b_negate = 18;
  mc:b_not = 19;
  mc:b_bitnot = 20;
  mc:b_ifelse = 21;
  mc:b_if = 22;
  mc:b_while = 23;
  mc:b_loop = 24;
  mc:b_ref = 25;
  mc:b_set = 26;
  mc:b_cons = 27;
  mc:parser_builtins = 28;
  // Compiler generated ops
  mc:b_assign = 28;
  mc:b_car = 29;
  mc:b_cdr = 30;
  mc:b_slength = 31;
  mc:b_vlength = 32;
  mc:b_iadd = 33; // integer addition
  mc:b_typeof = 34;
  mc:b_loop_count = 35;
  mc:b_max_loop_count = 36;
  mc:b_symbol_name = 37;
  mc:b_symbol_get = 38;
  mc:builtins = 39;

  mc:builtin_names =
    '[ 0 0 "==" "!=" "<" "<=" ">" ">="
       "|" "^" "&" "<<" ">>" "+" "-" "*" "/" "%"
       "-" "!" "~" 0 0 0 0 "ref" "set" "." ""
       "car" "cdr" "slength" "vlength" "i+"
       "typeof" "loop_count" "max_loop_count"
       "symbol_name" "symbol_get" ];
  assert(vlength(mc:builtin_names) == mc:builtins);

  // Format of information returned with assembled code
  mc:a_builtins = 0;
  mc:a_constants = 1;
  mc:a_subfns = 2;
  mc:a_globals = 3;
  mc:a_kglobals = 4;
  mc:a_primitives = 5;
  mc:a_linenos = 6;
  mc:a_rel_primitives = 7;

 // strange unload effects (see comment before linkun in link.mud)
  m_filename = mc:m_filename;
  m_nicename = mc:m_nicename;
  c_flineno = mc:c_flineno;
  c_ffilename = mc:c_ffilename;
  c_fnicename = mc:c_fnicename;
  c_fvar = mc:c_fvar;
  v_name = mc:v_name;

  simple_fname = fn (ifn)
    if (c_fvar < vector_length(ifn) && ifn[c_fvar])
      ifn[c_fvar][v_name]
    else
      "<fn>";

  mc:fname = fn (ifn)
    // Types: ifn: intemediate function
    // Returns: A printable name for ifn
    [
      | fname |
      if (use_nicename())
        fname = ifn[c_fnicename];
      if (!string?(fname))
        fname = ifn[c_ffilename];
      format("%s[%s:%d]", simple_fname(ifn), fname, ifn[c_flineno])
    ];

  mc:error = fn args
    [
      message("error", args);
      mc:erred = true;
    ];

  mc:warning = fn args
    message("warning", args);

  message = fn (type, args)
    [
      | msg, filename, lineno, func, nicename |

      lineno = -1;
      if (vector?(mc:this_function))
        [
          filename = mc:this_function[c_ffilename];
          nicename = mc:this_function[c_fnicename];
          lineno = if (integer?(mc:lineno))
            mc:lineno
          else
            mc:this_function[c_flineno];
          func = simple_fname(mc:this_function)
        ]
      else if (vector?(mc:this_module))
        [
          filename = mc:this_module[m_filename];
          nicename = mc:this_module[m_nicename];
          if (integer?(mc:lineno))
            lineno = mc:lineno
      ]
      else if (pair?(mc:this_filenames))
        @(filename . nicename) = mc:this_filenames;

      if (!string?(filename))
        filename = "?";

      | use_fname? |
      use_fname? = ((!use_nicename() || !string?(nicename))
                    && string_cmp(filename, nicename) != 0);

      msg = make_string_oport();
      pprint(msg, if (use_fname?) filename else nicename);
      pputc(msg, ?:);
      if (lineno >= 0)
        pformat(msg, "%d:", lineno);
      pformat(msg, " %s: ", type);
      if (use_fname?)
        pformat(msg, "[%s] ", nicename);
      if (func != null)
        pformat(msg, "%s: ", func);

      | fmtargs |
      fmtargs = make_vector(vector_length(args) + 1);
      fmtargs[0] = msg;
      for (|i| i = vector_length(args); i > 0; --i)
        fmtargs[i] = args[i - 1];

      apply(pformat, fmtargs);
      msg = port_string(msg);

      if (sort_messages?)
        pending_messages = vector(filename, lineno, message_count++, msg)
          . pending_messages
      else
        [
          display(msg);
          newline()
        ]
    ];

  mc:sort_messages = fn "`b -> . If `b, turn on message sorting. Flush messages when `b is set to false." (on?)
    if (!(sort_messages? = on?))
      [
        | last |
        last = "";
        lforeach(fn (v) [
          | msg |
          msg = v[3];
          if (string_cmp(last, msg) != 0)
            [
              display(msg); newline();
              last = msg;
            ]
        ], lqsort(fn (a, b) [
          | c |
          c = string_cmp(a[0], b[0]);
          if (c < 0)
            true
          else if (c > 0)
            false
          else if (a[1] == b[1])
            a[2] < b[2]
          else
            a[1] < b[1]
        ], pending_messages));
        pending_messages = null;
      ];

  | call_checks |

  call_checks = make_table();

  mc:register_call_check = fn "`f0 `f1 -> . Register `f1 as a call verification function for function `f0.\n"
    + "When the compiler detects a call to `f0 (as identified by its function name), `f1(`f2, `l) is called, where `f2 is the called function (likely the same as `f0) and `l is the list of arguments in the call.\n"
    + "Each argument is either a bitwise or of `itype_xxx constants (for an argument where the value is not known), or `cons(`itype_xxx, `x) when the value is known to be `x.\n"
    + "`f1 should return a warning string, or `false if the call seems correct.\n"
    + "N.b., this function is called under `unlimited_execution(), so take care not to write infinite loops or recursions." (function f, function test)
    call_checks[function_name(f)] = test;

  mc:lookup_call_check = fn (function f)
    [
      | n |
      if (n = function_name(f))
        call_checks[n]
      else
        null
    ];

];
