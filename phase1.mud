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

library phase1 // Phase 1: name resolution
requires system, sequences, dlist, misc, compiler, vars, ins3
defines mc:phase1
reads mc:this_module, mc:describe_seclev
writes mc:this_function, mc:lineno

// Takes a parse tree as returned by mudlle_parse, and makes the following
// changes:
//   a) all variable names are replaced by a vector. Two uses of the same variable
//      in the same function will refer to the same vector.

//      There are 4 kinds of variable:
//        global: a global variable
//        global_constant: a constant global variable
//        local: local to a block or a parameter
//        closure: non-local variables (but not global) used in a function

//   b) The header of a function is changed to include:
//      all its local variables (parameters, locals)
//      the variables of its parent which will form its closure
//      the variable in which the function is stored (debugging)
//      the types of its arguments (instead of being in the argument list)

//   c) All block constructs are removed
//      as a result, all components become lists of components
//      explicit initialisation to null is added for all variables

//   d) a statement labeled "function" is added around every function

//   e) the module header and all global variable references are checked
//      errors:
//        the required modules must be present (successfully loaded)
//        no clashes between writes and defines
//        reads & writes must be legal, as specified by the header
//      warnings:
//        stealing an identifier from another module
//	  stealing a writable identifier
//      Note that the loader has a different concept of errors and
//      warnings for these cases (eg stealing an identifier is an error)


[
  | resolve_component, env_init, env, topenv, comp_null, env_add_block,
    env_lookup, env_enter_function, env_leave_function, env_enter_block,
    env_leave_block, mstart, mcleanup, mlookup, imported?, all_readable,
    all_writable, readable, writable, definable, imported_modules, import,
    env_toplevel?, import_protected, env_global_prefix, assv,
    warn_bad_module_variables |

  comp_null = list(vector(mc:c_recall, -1, mc:var_make_constant(null)));

  // find vector v in l, for which v[0] == n
  assv = fn (n, l)
    loop
      [
        if (l == null) exit false;
        if (car(l)[0] == n) exit car(l);
        l = cdr(l);
      ];

  // environment handling

  env_init = fn ()
    // Effects: Initialises an empty environment
    [
      env = topenv = null;
    ];

  env_enter_function = fn (f)
    // Types: f : function
    // Modifies: env, topenv
    // Requires: the names in args be distinct
    // Effects: Adds a new function to the environment stack, with parameters
    //   called args
    [
      | vargs |

      mc:this_function = f;
      vargs = lmap(fn (v) mc:var_make_local(v[0]), f[mc:c_fargs]);
      env = dcons!(topenv = vector(null, vargs, null, f), env);
      env_add_block(f[mc:c_fargs], vargs);
    ];

  env_toplevel? = fn ()
    // Returns: true if we are in the toplevel function
    dnext(env) == env;

  // remove function at top of env and return vector of its
  // argument, local and closure vars
  env_leave_function = fn ()
    // Types : result : [ list of var, list of var, list of var ]
    // Modifies: env, topenv
    // Requires: All blocks of the top-level function must be exited
    // Effects: Pops the top level function from the environment stack
    // Returns: args, where args is a list of variables representing the
    //   function's parameters
    [
      | oldtop |

      oldtop = topenv;
      env = dremove!(env, env);
      if (env != null)
	[
	  topenv = dget(env);
	  mc:this_function = topenv[3];
	]
      else
        mc:this_function = null;

      oldtop[1]
    ];

  env_enter_block = fn (locals)
    // Types: locals : list of (string . type)
    // Modifies: topenv
    // Requires: the names in locals be distinct
    // Effects: A new block is entered in the current function with the
    //   specified local variables.
    // Returns: a list of components initialising the variables to null
    [
      | vlocals |
      vlocals = lmap(fn (v) mc:var_make_local(v[0]), locals);
      env_add_block(locals, vlocals);
      lmap(fn (vl) [
        | line |
        line = car(locals)[2];
        locals = cdr(locals);
        vector(mc:c_assign, line, vl, comp_null)
      ], vlocals)
    ];

  env_leave_block = fn ()
    // Modifies: topenv
    // Requires: At least one block must have been entered in the top level
    //   function
    // Effects: The top level block of the current function is exited
    [
      topenv[0] = cdr(topenv[0])
    ];

  // Add a new block of local vars
  env_add_block = fn (names, vars)
    [
      | vtable |
      vtable = make_table();
      while (names != null)
	[
	  vtable[car(names)[0]] = car(vars);
	  names = cdr(names);
	  vars = cdr(vars);
	];
      topenv[0] = vtable . topenv[0]
    ];

  env_global_prefix = ":";

  env_lookup = fn (name, write)
    // Types: name : string
    //        result : var
    //	      write : boolean
    // Modifies: env
    // Effects: Looks for the variable in the current environment that
    //   corresponds to name, starting in the innermost block of the top
    //   level function and working outwards.

    //   If not found, the variable is global, and library processing
    //   is done with mlookup (write is true for writes, false for reads)

    //   Otherwise appropriate closure variables are added to bring the
    //   variable to the top level function.

    // Returns: the var structure representing the variable in the top-level
    //   function.
    [
      | inenv, found, blocks |

      if (abbrev?(env_global_prefix, name))
	exit<function>
	  mlookup(substring(name, slength(env_global_prefix),
			    slength(name) - slength(env_global_prefix)),
		  write);

      inenv = env;

      <search> loop
	[
	  // search all blocks of inenv function
	  blocks = dget(inenv)[0];
	  loop
	    if (blocks == null) exit 0
	    else if ((found = car(blocks)[name]) != null) exit<search> 0 // found!
	    else blocks = cdr(blocks);

	  // not found, try next function
	  inenv = dnext(inenv);
	  if (inenv == env) // not found at all, is global
	    exit<function> mlookup(name, write);
	];

      // Add found to all appropriate closures
      while (inenv != env)
	[
	  | cname, thisenv |
	  inenv = dprev(inenv);
	  thisenv = dget(inenv);
	  // Is found already in the closure of the next function ?
	  cname = lexists?(fn (cv) cv[mc:v_cparent] == found,
			   thisenv[2]);
	  if (!cname)		// No, add it
	    thisenv[2] = (found = mc:var_make_closure(name, found)) . thisenv[2]
	  else
	    found = cname;
	  // And repeat with new variable for next level
	];
      found
    ];

  // Module handling

  mstart = fn (m)
    // Types: m : module
    // Effects: Does module-entry checks
    // Modifies: this_module, imported_modules, readable, writable,
    //  definable, all_writable, all_readable
    [
      | all_loaded, mname |

      mname = m[mc:m_name];

      // Check that all modules are loaded. As opposed to the interpreter,
      // the compiler does *not* load missing modules. It is not in the
      // business of executing code.

      all_loaded = true;
      imported_modules = make_table();
      lforeach
	(fn (required)
	 [
	   | name, s |

	   @[name _ mc:lineno] = required;
	   s = module_status(name);

	   if (s < module_loaded)
	     [
	       mc:error("%s not loaded", name);
	       all_loaded = false
	     ];
	   imported_modules[name] = vector(s, mc:lineno, null);
	 ],
	 m[mc:m_imports]);

      all_writable = m[mc:m_class] == mc:m_plain;
      all_readable = m[mc:m_class] == mc:m_plain || !all_loaded;

      /* Check status of variables (gives warnings, not errors) */

      /* defines must be ours, or normal, or write */
      definable = lmap
	(fn (var)
	 [
	   | status, name, n |

           @[name _ mc:lineno] = var;
	   n = global_lookup(name);
	   status = module_vstatus(n);

	   if (string?(status) && string_icmp(status, mname) != 0)
	     mc:warning("cannot define %s: belongs to module %s", name, status)
	   else if (status == var_write)
	     mc:warning("%s is writable", name)
	   else if (status == var_system_write)
	     mc:warning("cannot write %s from mudlle", name);

	   vector(n, name, 0, mc:lineno)
	 ],
	 m[mc:m_defines]);

      /* writes must not belong to a module */
      writable = lmap
	(fn (var)
	 [
	   | status, name, n |

	   @[name _ mc:lineno] = var;
	   n = global_lookup(name);
	   status = module_vstatus(n);

	   if (assv(n, definable))
	     mc:error("cannot write and define %s", name)
	   else if (status == var_system_write)
	     mc:warning("cannot write %s from mudlle", name)
           else if (string?(status))
	     mc:warning("cannot write %s: belongs to module %s", name, status);

	   vector(n, name, 0, mc:lineno)
	 ],
	 m[mc:m_writes]);

      /* reads */
      readable = lmap(fn (var) [
        | name, lineno |
        @[name _ lineno] = var;
        vector(global_lookup(name),
               name,
               0,
               lineno)
      ], m[mc:m_reads]);

      m[mc:m_imports] = imported_modules;
      m[mc:m_defines] = definable;
      m[mc:m_writes] = writable;
      m[mc:m_reads] = readable;
    ];

  mcleanup = fn ()
    // Effects: Clean up module variables
    readable = definable = writable = imported_modules = null;

  imported? = fn (mod)
    // Returns: status of mod if it is in imported_modules, false otherwise
    // Modifies: imported_modules
    match (imported_modules[mod])
      [
        [m _ _] => m;
        () => false;
        _ => fail()
      ];

  import = fn (v, mod)
    // Effects: Marks v as being imported from mod
    // Modifies: imported_modules
    // Returns: v
    [
      | m |

      m = imported_modules[mod];
      if (m == null)
	// implicitly import m
	imported_modules[mod] = vector(module_status(mod), -1,  v . null)
      else if (!memq(v, m[2]))
	m[2] = v . m[2];
      v
    ];

  import_protected = fn (name, n, mod)
    [
      | v, val |

      v = import(mc:var_make_kglobal(name, n), mod);
      // inline integers, floats and null
      val = global_value(n);
      if (integer?(val) || float?(val) || val == null) mc:var_make_constant(val)
      else v
    ];

  mlookup = fn (name, write)
    // Types: name: string
    //        write: boolean
    // Effects: Checks read/write accesses to global name for validity
    // Returns: an appropriate variable
    [
      | vstatus, n, vent |

      n = global_lookup(name);
      if (!write)
	[
	  // now: name is not imported
	  if (assv(n, definable))
	    [
	      // local define, a dglobal except at top-level
	      if (!env_toplevel?())
		exit<function> mc:var_make_dglobal(name, n);
	    ]
	  else if ((vent = assv(n, readable))
                   || (vent = assv(n, writable)))
            vent[mc:mv_used] |= mc:muse_read
          else
	    [
	      vstatus = module_vstatus(n);
	      if (string?(vstatus))
		[
		  // implicitly import protected modules
		  if (module_status(vstatus) == module_protected &&
		      (!mc:this_module[mc:m_name] ||
		       string_icmp(mc:this_module[mc:m_name], vstatus) != 0))
		    exit<function> import_protected(name, n, vstatus);

		  if (imported?(vstatus) == module_loaded)
		    exit<function> import(mc:var_make_dglobal(name, n),
                                          vstatus);
		  if (!all_readable)
		    mc:error("read of global %s (module %s)", name, vstatus)
		]
	      else if (!all_readable)
		mc:error("read of global %s", name);
	    ]
	]
      else
	[
	  if (vent = assv(n, definable))
	    [
              vent[mc:mv_used] |= mc:muse_write;
	      if (!env_toplevel?())
		mc:error("define of %s not at top level", name);
	    ]
	  else if (vent = assv(n, writable))
            vent[mc:mv_used] |= mc:muse_write
          else
	    [
	      vstatus = module_vstatus(n);
	      if (all_writable)
		[
		  if (string?(vstatus))
		    mc:warning("write of global %s (module %s)",
                               name, vstatus);
		]
	      else
		[
		  if (string?(vstatus))
		    mc:error("write of global %s (module %s)", name, vstatus)
		  else
		    mc:error("write of global %s", name);
		]
	    ]
	];
      // usual result, even for error cases
      mc:var_make_global(name, n)
    ];

  // Scan component tree

  resolve_component = fn (c)
    [
      | class, prevline, result |
      prevline = mc:lineno;
      if (c[mc:c_lineno] > 0)
	mc:lineno = c[mc:c_lineno];

      class = c[mc:c_class];
      result = if (class == mc:c_assign)
	[
	  | val, var, function |

	  c[mc:c_asymbol] = var = env_lookup(c[mc:c_asymbol], true);
	  c[mc:c_avalue] = val = resolve_component(c[mc:c_avalue]);

	  // Try & get a value for the "varname" field of a function
	  // recognises: "<name> = fn ..."
	  if (cdr(val) == null
              && (function = car(val))[mc:c_class] == mc:c_closure)
	    function[mc:c_fvar] = var;

	  list(c)
	]
      else if (class == mc:c_recall || class == mc:c_vref)
	[
          | vref?, var |
          vref? = class == mc:c_vref;
	  c[mc:c_rsymbol] = var = env_lookup(c[mc:c_rsymbol], vref?);
          if (vref?)
            [
              | arg |
              if (var[mc:v_class] == mc:v_global)
                arg = resolve_component(vector(mc:c_constant, -1,
                                               var[mc:v_goffset]))
              else
                arg = list(c);
              c = vector(mc:c_execute, c[mc:c_lineno],
                         list(list(vector(mc:c_recall, -1,
                                          mlookup("make_variable_ref",
                                                  false))),
                              arg))
            ];
	  list(c)
	]
      else if (class == mc:c_constant)
	list(vector(mc:c_recall, -1,
		    mc:var_make_constant(c[mc:c_cvalue])))
      else if (class == mc:c_closure)
	[
	  | components, args |

	  env_enter_function(c);
	  components = resolve_component(c[mc:c_fvalue]);
	  args = env_leave_function();

	  // add a "function" label
	  components = list(vector(mc:c_labeled, -1,
				   "function",
				   components));

	  list(vector(mc:c_closure, c[mc:c_flineno],
		      c[mc:c_freturn_typeset],
		      c[mc:c_fhelp],
		      args,	// parameters
		      c[mc:c_fvarargs],
		      components,
		      c[mc:c_flineno],
		      c[mc:c_ffilename],
		      c[mc:c_fnicename],
		      lmap(fn (v) v[1], c[mc:c_fargs]), // arg types
		      false,	// var name
		      null, null, null, null, null, null, // var lists
		      null,
		      0,
		      null,
                      0))       // flags
	]
      else if (class == mc:c_execute)
	[
	  c[mc:c_efnargs] = lmap(resolve_component, c[mc:c_efnargs]);
	  list(c)
	]
      else if (class == mc:c_builtin)
	[
	  c[mc:c_bargs] = lmap(resolve_component, c[mc:c_bargs]);
	  list(c)
	]
      else if (class == mc:c_block)
	[
	  | components, init |

	  init = env_enter_block(c[mc:c_klocals]);
	  components = lappend(init,
			       mappend(resolve_component, c[mc:c_ksequence]));
	  env_leave_block();

	  components
	]
      else if (class == mc:c_labeled)
	[
	  c[mc:c_lexpression] = resolve_component(c[mc:c_lexpression]);
	  list(c)
	]
      else if (class == mc:c_exit)
	[
	  c[mc:c_eexpression] = resolve_component(c[mc:c_eexpression]);
	  list(c)
	]
      else
        fail();

      mc:lineno = prevline;
      result
    ];

  warn_bad_module_variables = fn (int seclev)
    [
      lforeach(fn (var) [
        mc:lineno = var[mc:mv_lineno];
        if (~var[mc:mv_used] & mc:muse_read)
          mc:warning("readable global %s was never %s",
                     var[mc:mv_name],
                     if (var[mc:mv_used]) "read" else "used");
        | n, vstatus |
        n = global_lookup(var[mc:mv_name]);
        vstatus = module_vstatus(n);
        if (string?(vstatus))
          [
            | mseclev |
            mseclev = module_seclevel(vstatus);
            mseclev = if (mseclev < seclev)
              [
                mseclev = if (function?(mc:describe_seclev))
                  mc:describe_seclev(mseclev)
                else
                  seclev;
                format(" (lvl %s)", mseclev)
              ]
            else
              "";
            mc:warning("reads global variable %s defined in %s%s",
                       var[mc:mv_name], vstatus, mseclev);
          ]
      ], readable);
      lforeach(fn (var) [
        mc:lineno = var[mc:mv_lineno];
        if (~var[mc:mv_used] & mc:muse_write)
          mc:warning("writable global %s was never %s",
                     var[mc:mv_name],
                     if (var[mc:mv_used]) "written" else "used")
      ], writable);
      table_foreach(fn (imp) [
        | syms |
        @[_ mc:lineno syms] = symbol_get(imp);
        if (syms == null)
          mc:warning("symbols from required module %s were never used",
                     symbol_name(imp));
      ], imported_modules);
    ];

  mc:phase1 = fn (m, int seclev)
    [
      | components, fname, nname, top_var |

      top_var = vector(mc:v_global, "top-level");

      fname = m[mc:m_filename];
      nname = m[mc:m_nicename];

      mstart(m);
      env_init();
      env_enter_function(vector(mc:c_closure, -1,
				mc:typeset_any, // return typeset
				null,           // help
				null,           // arguments
				false,          // vararg?
				null,           // value
                                m[mc:m_body][mc:c_lineno], // lineno
				fname,          // filename
				nname,          // nicename
				null,           // argtypes
				top_var));      // variable name
      components = resolve_component(m[mc:m_body]);
      env_leave_function();
      warn_bad_module_variables(seclev);
      mcleanup();

      // make a top-level function
      m[mc:m_body] =
	vector(mc:c_closure, -1,
	       mc:typeset_any,                  // return typeset
	       null,                            // help
	       null,                            // arguments
	       false,                           // vararg?
	       components,                      // value
	       -1,                              // lineno
	       fname,                           // filename
	       nname,                           // nicename
	       null,                            // argtypes
	       top_var,                         // variable name
	       null, null, null, null, null, null, // var lists
	       null,                            // misc
	       0,                               // # fnvars
	       null,                            // # allvars
               0)                               // flags
    ];
];
