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
requires compiler, dlist, ins3, misc, sequences, vars
defines mc:phase1
reads mc:this_module, mc:describe_seclev, mc:erred
writes mc:this_function

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
  | resolve_component, resolve_block, env_init, env, topenv, comp_null,
    env_add_block, env_lookup, env_enter_function, env_leave_function,
    env_enter_block, env_leave_block, mstart, mcleanup, mlookup, imported?,
    all_readable, all_writable, readable, writable, definable, statics,
    required_modules, import, system_import, env_toplevel?, import_protected,
    env_global_prefix, assv, warn_bad_module_variables,
    ksymbol_get, kget_static, kmake_variable_ref, ksymbol_set!,
    kmake_symbol_ref,
    es_blocks, es_nlocals, es_args, es_closure, es_fn |

  // environment stack (topenv) indices
  es_blocks  = 0;
  es_args    = 1;
  es_closure = 2;               // closure vars
  es_fn      = 3;
  es_nlocals = 4;

  ksymbol_get        = mc:make_kglobal("symbol_get");
  ksymbol_set!       = mc:make_kglobal("symbol_set!");
  kget_static        = mc:make_kglobal("get_static");
  kmake_variable_ref = mc:make_kglobal("make_variable_ref");
  kmake_symbol_ref   = mc:make_kglobal("make_symbol_ref");

  comp_null = list(vector(mc:c_recall, mc:no_loc, mc:var_make_constant(null)));

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
      vargs = f[mc:c_fargs];
      lforeach(fn (v) v[mc:vl_var] = mc:var_make_local(v[mc:vl_var]), vargs);
      env = dcons!(topenv = vector(null, vargs, null, f, '(0)), env);
      env_add_block(vargs);
    ];

  env_toplevel? = fn ()
    // Returns: true if we are in the toplevel function
    dnext(env) == env;

  // remove function at top of env and return vector of its
  // argument, local and closure vars
  env_leave_function = fn ()
    // Types : result : list of args . list of locs
    // Modifies: env, topenv
    // Requires: All blocks of the top-level function must be exited
    // Effects: Pops the top level function from the environment stack
    // Returns: args, where args is a list of variables representing the
    //   function's parameters
    [
      | args |
      args = topenv[es_args];
      env = dremove!(env, env);
      if (env != null)
	[
	  topenv = dget(env);
	  mc:this_function = topenv[es_fn];
	]
      else
        mc:this_function = null;

      args
    ];

  env_enter_block = fn (locals, top?)
    // Types: locals : list of (string . type)
    // Modifies: topenv
    // Requires: the names in locals be distinct
    // Effects: A new block is entered in the current function with the
    //   specified local variables.
    // Returns: a list of components initialising the variables to null
    [
      lforeach(fn (v) v[mc:vl_var] = mc:var_make_local(v[mc:vl_var]), locals);
      if (top?)
        lforeach(fn (v) v[mc:vl_var] = mc:var_make_static(v[mc:vl_var]),
                 locals);
      env_add_block(locals);
      lmap(fn (@[var _ loc]) [
        | cl |
        if (top?)
          [
            assert(var[mc:v_class] == mc:v_static);
            var = var[mc:v_sparent];
            cl = list(vector(
              mc:c_execute, loc,
              list(list(vector(mc:c_recall, loc, kget_static)),
                   list(vector(mc:c_recall, loc,
                               mc:var_make_constant(var[mc:v_name]))))));
          ]
        else
          cl = comp_null;
        vector(mc:c_assign, loc, var, cl)
      ], locals)
    ];

  env_leave_block = fn ()
    // Modifies: topenv
    // Requires: At least one block must have been entered in the top level
    //   function
    // Effects: The top level block of the current function is exited
    [
      if (!mc:erred)
        table_foreach(fn (sym) [
          | var, err |
          var = car(symbol_get(sym));
          err = match (var[mc:mv_used])
            [
              0 => "unused";
              ,mc:muse_read => "never written";
              ,mc:muse_write => "never read";
              ,(mc:muse_read | mc:muse_write) => exit<function> null;
              _ => fail();
            ];
          mc:set_loc(var[mc:mv_loc]);
          mc:warning("local variable %s is %s", var[mc:mv_name], err);
        ], car(topenv[es_blocks]));

      topenv[es_blocks] = cdr(topenv[es_blocks]);
      topenv[es_nlocals] = cdr(topenv[es_nlocals]);
    ];

  // Add a new block of local vars
  env_add_block = fn (vars)
    [
      | vtable, nv |

      assert(vector?(topenv));

      vtable = make_table();
      nv = car(topenv[es_nlocals]);
      while (vars != null)
	[
          | var, loc, name |
          @([var _ loc] . vars) = vars;
          // cons(mc:mv_xxx vector, mc:v_xxx vector)

          if (nv == mc:max_local_vars)
            [
              mc:set_loc(loc);
              mc:error("too many local variables");
            ];

          ++nv;
	  name = var[mc:v_name];
	  vtable[name] = vector(null, name, 0, loc) . var;
	];
      topenv[es_blocks] = vtable . topenv[es_blocks];
      topenv[es_nlocals] = nv . topenv[es_nlocals];
    ];

  env_global_prefix = ":";

  env_lookup = vector fn (name, write)
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
	  blocks = dget(inenv)[es_blocks];
	  loop
	    if (blocks == null) exit 0
	    else if ((found = car(blocks)[name]) != null) exit<search> 0 // found!
	    else blocks = cdr(blocks);

	  // not found, try next function
	  inenv = dnext(inenv);
	  if (inenv == env) // not found at all, is global
	    exit<function> mlookup(name, write);
	];

      // found is cons(mvar, var)
      car(found)[mc:mv_used] |= if (write) mc:muse_write else mc:muse_read;
      found = cdr(found);

      // Add found to all appropriate closures
      while (inenv != env)
	[
	  | cname, thisenv |
	  inenv = dprev(inenv);
	  thisenv = dget(inenv);
	  // Is found already in the closure of the next function ?
	  cname = lexists?(fn (cv) cv[mc:v_cparent] == found,
			   thisenv[es_closure]);
	  if (!cname)		// No, add it
            [
              found = if (found[mc:v_class] == mc:v_static)
                mc:var_make_static(
                  mc:var_make_closure(name, found[mc:v_sparent]))
              else
                mc:var_make_closure(name, found);
              thisenv[es_closure] = found . thisenv[es_closure]
            ]
	  else
	    found = cname;
	  // And repeat with new variable for next level
	];

      found
    ];

  // Module handling

  mstart = fn (m, int seclev)
    // Types: m : module
    // Effects: Does module-entry checks
    // Modifies: this_module, required_modules, readable, writable,
    //  definable, all_writable, all_readable
    [
      | all_loaded, mname |

      mname = m[mc:m_name];

      // Check that all modules are loaded. As opposed to the interpreter,
      // the compiler does *not* load missing modules. It is not in the
      // business of executing code.

      all_loaded = true;
      required_modules = make_table();
      lforeach
	(fn (required)
	 [
	   | name, s, loc |

	   @[name _ loc] = required;
           mc:set_loc(loc);
	   s = module_status(name);

	   if (s < module_loaded)
	     [
	       mc:error("%s not loaded", name);
	       all_loaded = false
	     ];
	   required_modules[name] = vector(s, mc:get_loc(), null);
	 ],
	 m[mc:m_requires]);

      all_writable = m[mc:m_class] == mc:m_plain;
      all_readable = m[mc:m_class] == mc:m_plain || !all_loaded;

      /* Check status of variables (gives warnings, not errors) */

      /* defines must be ours, or normal, or write */
      definable = lmap
	(fn (var)
	 [
	   | status, name, n, loc |

           @[name _ loc] = var;
           mc:set_loc(loc);
	   n = global_lookup(name);
	   status = module_vstatus(n);

	   if (string?(status) && string_icmp(status, mname) != 0)
	     mc:warning("cannot define %s: belongs to module %s", name, status)
	   else if (status == var_write)
	     [
	       if (seclev < SECLEVEL_GLOBALS)
	         mc:error("cannot define %s: exists and is writable", name)
	       else
	         mc:warning("%s is writable", name);
             ]
	   else if (status == var_system_write || status == var_system_mutable)
	     mc:error("cannot define %s in mudlle", name);

	   vector(n, name, 0, mc:get_loc())
	 ],
	 m[mc:m_defines]);

      /* writes must not belong to a module */
      writable = lreduce(fn (var, l) [
        | status, name, n, loc |

        @[name _ loc] = var;
        mc:set_loc(loc);
        n = global_lookup(name);
        status = module_vstatus(n);

        if (assv(n, definable))
          [
            mc:error("cannot write and define %s", name);
            exit<function> l;
          ];
        if (status == var_system_write)
          [
            mc:error("cannot write %s from mudlle", name);
            exit<function> l;
          ];
        if (status == var_system_mutable)
          [
            mc:warning("%s is always writable", name);
            exit<function> l;
          ];
        if (string?(status))
          mc:warning("cannot write %s: belongs to module %s", name, status);

        vector(n, name, 0, mc:get_loc()) . l
      ], null, m[mc:m_writes]);

      /* reads */
      readable = lreduce(fn (@[name _ loc], l) [
        mc:set_loc(loc);

        | n, status |

        n = global_lookup(name);
        status = module_vstatus(n);

        if (status == var_system_write
            || status == var_system_mutable
            || status == var_system_mutable
            || equal?(status, "system"))
          [
            mc:warning("%s is always readable", name);
            exit<function> l;
          ];

        vector(global_lookup(name), name, 0, mc:get_loc()) . l
      ], null, m[mc:m_reads]);

      statics = m[mc:m_statics];
      lforeach(fn (@[name _ _]) [
        | n |
        // avoid global_lookup here as that would create a global
        n = global_table()[name];
        if (!integer?(n)) exit<function> null;
        lforeach(fn (l) [
          match (assv(n, cdr(l)))
            [
              [ _ _ _ line ] => [
                mc:set_loc(line);
                mc:error("cannot %s static %s", car(l), name);
              ];
            ]
        ], list("define" . definable,
                "write" . writable,
                "read" . readable));
      ], statics);

      m[mc:m_requires] = required_modules;
      m[mc:m_defines] = definable;
      m[mc:m_writes] = writable;
      m[mc:m_reads] = readable;
      m[mc:m_statics] = statics;
    ];

  mcleanup = fn ()
    // Effects: Clean up module variables
    readable = definable = writable = required_modules = null;

  imported? = fn (mod)
    // Returns: status of mod if it is in required_modules, false otherwise
    // Modifies: required_modules
    match (required_modules[mod])
      [
        [m _ _] => m;
        () => false;
        _ => fail()
      ];

  import = fn (vector v, string mod)
    // Effects: Marks v as being imported from mod
    // Modifies: required_modules
    // Returns: v
    [
      | m |

      m = required_modules[mod];
      if (m == null)
	// implicitly import m
	required_modules[mod] = vector(module_status(mod), mc:no_loc, v . null)
      else if (!memq(v, m[2]))
	m[2] = v . m[2];
      v
    ];

  system_import = fn (int n, string name)
    import(mc:var_make_global(name, n), "system");

  import_protected = fn (name, n, mod)
    [
      | v, val |

      v = import(mc:var_make_kglobal(name, n), mod);
      // inline integers, floats and null
      val = global_value(n);
      if (val == null || integer?(val) || float?(val))
        mc:var_make_constant(val)
      else
        v
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
              | st |
              st = module_vstatus(n);
              if (string?(st) && module_status(st) == module_protected)
                [
                  | val |
                  val = global_value(n);
                  // inline simple constants in their own library
                  if (val == null || integer?(val) || float?(val))
                    exit<function> mc:var_make_constant(val);
                ];

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
	      else if (vstatus == var_system_write
                       || vstatus == var_system_mutable)
                exit<function> system_import(n, name)
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
              if (vstatus == var_system_write)
                mc:error("cannot write global %s (module system)", name)
              else if (vstatus == var_system_mutable)
                exit<function> system_import(n, name)
              else if (all_writable)
		[
		  if (string?(vstatus))
		    mc:warning("write of global %s (module %s)",
                               name, vstatus);
		]
	      else if (string?(vstatus))
                mc:error("write of global %s (module %s)", name, vstatus)
              else
                mc:error("write of global %s", name);
	    ]
	];
      // usual result, even for error cases
      mc:var_make_global(name, n)
    ];

  // Scan component tree

  resolve_block = fn (vars, comps, top?)
    [
      | components, init |
      init = env_enter_block(vars, top?);
      components = lappend(init, mappend(resolve_component, comps));
      env_leave_block();
      components
    ];

  resolve_component = fn (vector c)
    [
      | class, prevline, result |
      prevline = mc:get_loc();
      mc:maybe_set_loc(c[mc:c_loc]);

      class = c[mc:c_class];
      result = if (class == mc:c_assign)
	[
	  | val, var, function |

	  c[mc:c_asymbol] = var = env_lookup(c[mc:c_asymbol], true);
	  c[mc:c_avalue] = val = resolve_component(c[mc:c_avalue]);

          if (var[mc:v_class] == mc:v_static)
            [
              var = var[mc:v_sparent];
              c = vector(
                mc:c_execute, c[mc:c_loc],
                list(list(vector(mc:c_recall, c[mc:c_loc], ksymbol_set!)),
                     list(vector(mc:c_recall, c[mc:c_loc], var)),
                     val));
            ];

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
          if (var[mc:v_class] == mc:v_static)
            [
              | fun |
              fun = if (class == mc:c_vref) kmake_symbol_ref else ksymbol_get;
              c[mc:c_rsymbol] = var = var[mc:v_sparent];
              c[mc:c_class] = mc:c_recall;
              c = vector(mc:c_execute, c[mc:c_loc],
                         list(list(vector(mc:c_recall, mc:no_loc, fun)),
                              list(c)))
            ]
          else if (vref?)
            [
              | arg |
              if (var[mc:v_class] == mc:v_global)
                arg = resolve_component(
                  vector(mc:c_execute, mc:no_loc,
                         list(vector(mc:c_recall, mc:no_loc,
                                     env_global_prefix + "global_lookup"),
                              vector(mc:c_constant, mc:no_loc,
                                     var[mc:v_name]))))
              else
                arg = list(c);
              c = vector(
                mc:c_execute, c[mc:c_loc],
                list(list(vector(mc:c_recall, mc:no_loc,
                                 kmake_variable_ref)),
                     arg))
            ];

	  list(c)
	]
      else if (class == mc:c_constant)
	list(vector(mc:c_recall, mc:no_loc,
		    mc:var_make_constant(c[mc:c_cvalue])))
      else if (class == mc:c_closure)
	[
	  | components, args |

	  env_enter_function(c);
	  components = resolve_component(c[mc:c_fvalue]);
	  args = env_leave_function();

	  // add a "function" label
	  components = list(vector(mc:c_labeled, mc:no_loc,
				   "function",
				   components));

	  list(vector(mc:c_closure, c[mc:c_loc],
		      c[mc:c_freturn_typeset],
		      c[mc:c_fhelp],
		      args,	// parameters
		      c[mc:c_fvarargs],
		      components,
		      c[mc:c_ffilename],
		      c[mc:c_fnicename],
                      mc:itypeset_from_typeset(c[mc:c_freturn_typeset]),
                      false,	// noescape
		      false,	// var name
		      null, null, null, null, null, null, // var lists
		      null,
		      0,
		      null))
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
        resolve_block(c[mc:c_klocals], c[mc:c_ksequence], false)
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

      mc:set_loc(prevline);
      result
    ];

  warn_bad_module_variables = fn (int seclev)
    [
      lforeach(fn (var) [
        mc:set_loc(var[mc:mv_loc]);
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
            mseclev = if (mseclev && mseclev < seclev)
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
        mc:set_loc(var[mc:mv_loc]);
        if (~var[mc:mv_used] & mc:muse_write)
          mc:warning("writable global %s was never %s",
                     var[mc:mv_name],
                     if (var[mc:mv_used]) "written" else "used")
      ], writable);
      table_foreach(fn (@<name = [_ loc syms]>) [
        mc:set_loc(loc);
        if (syms == null)
          mc:warning("symbols from required module %s were never used",
                     name);
      ], required_modules);
    ];

  mc:phase1 = fn (m, int seclev)
    [
      | components, fname, nname, top_var, loc |

      top_var = vector(mc:v_global, "top-level");

      fname = m[mc:m_filename];
      nname = m[mc:m_nicename];

      mstart(m, seclev);
      env_init();
      loc = m[mc:m_body][mc:c_loc];
      env_enter_function(vector(mc:c_closure, loc,
				typeset_any,    // return typeset
				null,           // help
				null,           // arguments
				false,          // vararg?
				null,           // value
				fname,          // filename
				nname,          // nicename
                                itype_any,      // return itype
				false,		// noescape
				top_var));      // variable name
      components = resolve_block(m[mc:m_statics], list(m[mc:m_body]), true);
      env_leave_function();
      warn_bad_module_variables(seclev);
      mcleanup();

      // make a top-level function
      m[mc:m_body] =
	vector(mc:c_closure, loc,
	       typeset_any,                     // return typeset
	       null,                            // help
	       null,                            // arguments
	       false,                           // vararg?
	       components,                      // value
	       fname,                           // filename
	       nname,                           // nicename
	       itype_any,                       // return itypeset
	       false,				// noescape
	       top_var,                         // variable name
	       null, null, null, null, null, null, // var lists
	       null,                            // misc
	       0,                               // # fnvars
	       null)                            // # allvars
    ];
];
