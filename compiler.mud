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

defines

  mc:c_freturn_itype, mc:c_fvar,
  mc:c_flocals, mc:c_flocals_write, mc:c_fclosure, mc:c_fclosure_write,
  mc:c_fglobals, mc:c_fglobals_write, mc:c_fnoescape, mc:c_fnumber, mc:c_fmisc,
  mc:c_fnvars, mc:c_fallvars,

  mc:c_fm_argsbase, mc:c_fm_closurebase, mc:c_fm_globalsbase,
  mc:c_fm_regs_callee,

  mc:b_assign, mc:b_car, mc:b_cdr,
  mc:b_slength, mc:b_vlength, mc:b_iadd, mc:b_typeof, mc:b_loop_count,
  mc:b_max_loop_count, mc:b_symbol_name, mc:b_symbol_get, mc:b_vector,
  mc:b_sequence, mc:b_pcons, mc:b_symbol_ref,
  mc:builtins, mc:builtin_names,

  mc:a_builtins, mc:a_constants, mc:a_subfns, mc:a_globals, mc:a_kglobals,
  mc:a_primitives, mc:a_linenos, mc:a_rel_primitives, mc:a_seclevs,
  mc:a_kglobal_code, mc:a_info_fields,

  mc:fname, mc:error, mc:warning, mc:sort_messages,

  mc:mv_gidx, mc:mv_name, mc:mv_used, mc:mv_loc,
  mc:muse_read, mc:muse_write,

  mc:register_call_check, mc:lookup_call_check,

  mc:apply_functions

reads mc:this_filenames, mc:this_module, mc:this_function
writes mc:erred

[
  | message, m_filename, m_nicename, simple_fname, c_loc, c_ffilename,
    c_fnicename, c_fvar, v_name, pending_messages, sort_messages?,
    message_count |

  sort_messages? = false;
  message_count = 0;

  // the variable lists above are lists of (name . type) from mudlle_parse,
  // and vector(gidx, name, used) after mstart()
  mc:mv_gidx = 0;
  mc:mv_name = 1;
  mc:mv_used = 2;
    // flags in mc:mv_used
    mc:muse_read  = 1;
    mc:muse_write = 2;
  mc:mv_loc = 3;                // (line . column)


  // Component structure:
  //   It is a vector whose first element is one of mc:c_assign, c_recall, etc.
  //   See FOR_COMPONENT_CLASSES in tree.h
  //   The remaining elements depend on the value of the first, as follows:

  // mc:c_class			// class of component
  // mc:c_loc			// location (line . column) of component

  // mc:c_assign - assignment statement
  //  mc:c_asymbol		// var name (string, after phase1: var)
  //  mc:c_avalue		// value (component)

  // mc:c_recall - value of a variable
  // mc:c_vref	 - (container of) a variable
  //  mc:c_rsymbol		// var name (string, after phase1: var)

  // mc:c_constant - a constant
  //  mc:c_cvalue		// value of constant (any type)

  // mc:c_closure - a function
  //  mc:c_freturn_typeset	// return value typeset
  //  mc:c_fhelp		// help string (string or null)
  //  mc:c_fargs		// argument names (list of [string, type, loc]
  //				//     after phase1: list of var)
  //  mc:c_fvarargs		// true if this is a varargs function
  //  mc:c_fvalue		// function value (component)
  //  mc:c_ffilename		// filename on disk (string)
  //  mc:c_fnicename		// pretty-printed filename (string)

   // Added by phase1
   | ccf |
   ccf = mc:c_closure_fields;
   mc:c_freturn_itype  = ccf + 0; // return value itypes
   mc:c_fvar           = ccf + 1; // variable in which function is stored
   mc:c_flocals        = ccf + 2; // local variables (list of var)
   mc:c_flocals_write  = ccf + 3; // local variables (list of var) (those
                                  // written)
   mc:c_fclosure       = ccf + 4; // closure variables (list of var)
   mc:c_fclosure_write = ccf + 5; // closure variables (list of var) (those
                                  // written)
   mc:c_fglobals       = ccf + 6; // the global variables used (list of var)
   mc:c_fglobals_write = ccf + 7; // the global variables written (list of var)

   mc:c_fnoescape      = ccf + 8; // true if no calls from here escapes (does
                                  // not call op_noescape); may still write
                                  // closure variables in mc:c_fclosure_write

   // Set by phase2:
   mc:c_fnumber = 0;		// a unique number for this closure (int)

   // Set by phase 4:
   mc:c_fmisc = ccf + 9;	// miscellaneous info
    mc:c_fm_argsbase = 0;	// true if function needs arguments base
    mc:c_fm_closurebase = 1;	// true if function needs closure base
    mc:c_fm_globalsbase = 2;	// true if function needs globals base
    mc:c_fm_regs_callee = 3;	// callee registers used by the backend
   mc:c_fnvars = ccf + 10;	// number of vars (global, closure, local)
				// used (int, phase3)
   mc:c_fallvars = ccf + 11;	// all vars (global, closure, local) used
				// (vector, phase3)

  // mc:c_execute - execute a function
  //  mc:c_efnargs		// list of function, followed by arguments
  //				// (list of component)

  // mc:c_builtin - execute a primitive
  //  mc:c_bfn			// number of primitive (integer, see below)
  //  mc:c_bargs		// arguments (list of component)

  // mc:c_block - a block
  //  mc:c_klocals		// local variables (list of string)
  //  mc:c_ksequence		// code (list of component)

  // mc:c_labeled - labeled expression
  //  mc:c_lname		// label name (string)
  //  mc:c_lexpression		// expression value (component)

  // mc:c_exit
  //  mc:c_ename		// label name (string or null)
  //  mc:c_eexpression		// exit expression (component)

  // language primitives, some are functions, others are control structures

  // Compiler generated ops
  assert(mc:parser_builtins == 28);
  mc:b_assign         = 28;
  mc:b_car            = 29;
  mc:b_cdr            = 30;
  mc:b_slength        = 31;
  mc:b_vlength        = 32;
  mc:b_iadd           = 33;     // integer addition
  mc:b_typeof         = 34;
  mc:b_loop_count     = 35;
  mc:b_max_loop_count = 36;
  mc:b_symbol_name    = 37;
  mc:b_symbol_get     = 38;
  mc:b_vector         = 39;
  mc:b_sequence       = 40;
  mc:b_pcons          = 41;
  mc:b_symbol_ref     = 42;
  mc:builtins         = 43;

  mc:builtin_names =
    '[ 0 0 "==" "!=" "<" ">=" "<=" ">"
       "|" "^" "&" "<<" ">>" "+" "-" "*" "/" "%"
       "-" "!" "~" 0 0 0 0 "ref" "set" "." ""
       "car" "cdr" "slength" "vlength" "i+"
       "typeof" "loop_count" "max_loop_count"
       "symbol_name" "symbol_get" "vector" "sequence" "pcons" "symbol_ref" ];
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
  mc:a_seclevs = 8;
  mc:a_kglobal_code = 9;
  mc:a_info_fields = 10;

 // strange unload effects (see comment before linkun in link.mud)
  m_filename = mc:m_filename;
  m_nicename = mc:m_nicename;
  c_loc = mc:c_loc;
  c_ffilename = mc:c_ffilename;
  c_fnicename = mc:c_fnicename;
  c_fvar = mc:c_fvar;
  v_name = mc:v_name;

  // [ function f-index argv-index ] where argv-index null means no arguments
  mc:apply_functions = '[
    [,apply 0 1]
    [,session 0 ()]
    [,with_minlevel 1 ()]
    [,with_output 1 ()]
  ];
  vforeach(fn (@[p ...]) [
    assert(primitive_flags(p) & OP_APPLY);
    lforeach(fn (sig) assert(sig[-1] == ?x), primitive_type(p))
  ], mc:apply_functions);

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
      format("%s[%s:%d]", simple_fname(ifn), fname,
             mc:loc_line(ifn[c_loc]))
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
      | msg, filename, loc, func, nicename |

      loc = mc:no_loc;
      if (vector?(mc:this_function))
        [
          filename = mc:this_function[c_ffilename];
          nicename = mc:this_function[c_fnicename];
          loc = mc:get_loc();
          if (mc:loc_line(loc) <= 0)
            loc = mc:this_function[c_loc];
          func = simple_fname(mc:this_function)
        ]
      else if (vector?(mc:this_module))
        [
          filename = mc:this_module[m_filename];
          nicename = mc:this_module[m_nicename];
          loc = mc:get_loc()
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
      if (mc:loc_line(loc) >= 0)
        [
          pformat(msg, "%d:", mc:loc_line(loc));
          if (mc:loc_column(loc) > 0)
            pformat(msg, "%d:", mc:loc_column(loc));
        ];
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
        pending_messages = vector(filename, loc, message_count++, msg)
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
          if (c != 0)
            exit<function> c < 0;
          c = mc:loc_line(a[1]) - mc:loc_line(b[1]);
          if (c != 0)
            exit<function> c < 0;
          c = mc:loc_column(a[1]) - mc:loc_column(b[1]);
          if (c != 0)
            exit<function> c < 0;
          a[2] < b[2]
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
