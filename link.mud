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

library link // A linker.
requires compiler, vars, system, sequences
defines mc:prelink
writes mc:this_module, mc:erred, mc:linkrun
[
  | make_code, prelink, mstart, error, warning, append!, pmodule_class, 
    pmodule_name, pmodule_imports, pmodule_defines, pmodule_reads, 
    pmodule_writes, pmodule_body, pfn_code, pfn_help, pfn_varname, pfn_filename,
    pfn_lineno, pfn_subfns, pfn_constants, pfn_builtins, pfn_globals, 
    pfn_kglobals, pfn_primitives, check_presence, check_dependencies, 
    dependencies, depend_immutable, depend_none, depend_primitive, depend_type,
    depend_integer, depend_signature, dependency, check_dependency, typenames,
    check_present, map, foreach, pmodule_protect |

  // Linker:

  // The prelinker accepts the results of phase4 and produces a
  // prelinked module (pmodule), which can be saved with save_data.
  
  // The linker takes a prelinked module, checks everything, and then
  // links the module into the current environment. Finally it runs
  // the generated code, and updates the module's status.

  // prelinked module structure (similar to module):
  pmodule_class = 0;		// module class
  pmodule_protect = 1;		// true if module should be protected
  pmodule_name = 2;		// module name (or false)
  pmodule_imports = 3;		// dependencies (details below)
  pmodule_defines = 4;		// defined symbols (list of string)
  pmodule_reads = 5;		// read symbols (list of string) -- unused
  pmodule_writes = 6;		// written symbols (list of string)
  pmodule_body = 7;		// top-level function (prelinked function)

  // The dependencies for a module is the set of symbols (defines) imported
  // from modules that it requires, along with any checks on the actual values
  // of these symbols.
  // The linker requires that these symbols belong to the same modules as at
  // compile time.
  // Complex dependencies are only present for "protected" modules.

  // imports is thus an associative array, with the imported modules as keys
  // Associated with each entry is a list:
  //   car: status of imported module at compile time (loaded or protected)
  //   cdr: list of imports
  //     if loaded: simply list of symbol names
  //     if protected: list of dependencies (see end of this file)

  // All the functions are also transformed into a "prelinked" form:
  pfn_code = 0;			// machine code (string)
  pfn_help = 1;			// help (string or null)
  pfn_varname = 2;		// varname (string or null)
  pfn_filename = 3;		// filename (string or null)
  pfn_lineno = 4;		// line number (int)
  // references (all of the form list of <info> . offset,
  // where offset is an integer offset into code
  pfn_subfns = 5;		// sub-functions (list of pfn . offset)
  pfn_constants = 6;		// constants (list of value . offset)
  pfn_builtins = 7;		// builtins (list of string . offset)
  pfn_globals = 8;		// globals (list of string . offset)
  pfn_kglobals = 9;		// constant globals (list of string . offset)
  pfn_primitives = 10;		// primitives (list of string . offset)

  // Linking
  // -------

  // linkrun can unload anything, hence it should only depend on stuff in
  // system and local vars while running
  error = mc:error;
  warning = mc:warning;
  append! = lappend!;
  map = lmap;
  foreach = lforeach;

  mc:linkrun = fn (prelinked_module, seclev, reload)
    [
      | ok, mname |

      mname = prelinked_module[pmodule_name];
      if (!reload && mname && module_status(mname) != module_unloaded)
	exit<function> true;

      ok = false;
      mc:erred = false;
      mc:this_module = prelinked_module;
      if (mstart(prelinked_module))
	[
	  if (!mc:erred) // run only if no errors
	    [
	      | code |
	  
	      code = make_code(prelinked_module[pmodule_body], seclev);
	      ok = true;
	      catch_error(fn () make_closure(code)(), false);
	    ];

	  if (mname)
	    if (ok && prelinked_module[pmodule_protect])
	      [
		module_set!(mname, module_protected);
		// check immutability, otherwise dependencies on immutable
		// symbols of the loaded module might fail
		detect_immutability();
	      ]
	    else
	      module_set!(mname, if (ok) module_loaded else module_error);
	];

      mc:this_module = null;

      ok
    ];

  mstart = fn (m)
    [
      | mname |

      mname = m[pmodule_name];
      if (mname)
	[
	  // unload it
	  if (!module_unload(mname)) exit<function> false; // can't unload, stop
	  module_set!(mname, module_loading);
	];

      // load & check imported modules
      foreach
	(fn (mod)
	 [
	   | mstatus, iname, minfo, erred |

	   iname = symbol_name(mod);
	   minfo = symbol_get(mod);
	   erred = mc:erred;
	   mstatus = module_require(iname);
	   mc:this_module = m; // could be squashed by module_require
	   mc:erred = erred;

	   if (mstatus < module_loaded)
	     [
	       if (mstatus == module_loading)
		 error("loop in requires of %s", iname)
	       else
		 error("failed to load %s", iname);
	     ]
	   else if (car(minfo) == module_loaded)
	     check_presence(cdr(minfo), iname)
	   else if (car(minfo) == module_protected)
	     if (mstatus != module_protected)
	       error("%s is not protected anymore", iname)
	     else
	       check_dependencies(cdr(minfo), iname);
	 ],
	 table_list(m[pmodule_imports]));

      // Change status of variables
      foreach
	(fn (var)
	 [
	   | n, vstatus |

	   n = global_lookup(var);
	   vstatus = module_vstatus(n);

	   if (!module_vset!(n, mname))
	     error("cannot define %s: belongs to module %s", var, vstatus)
	   else if (vstatus == var_write)
	     warning("%s was writable", var);
	 ],
	 m[pmodule_defines]);

      foreach
	(fn (var)
	 [
	   | n |

	   n = global_lookup(var);

	   if (!module_vset!(n, var_write))
	     error("cannot write %s: belongs to module %s",
		   var, module_vstatus(n));
	 ],
	 m[pmodule_writes]);

      true // can proceed
    ];

  check_presence = fn (vars, mod)
    // Types: vars: list of string, mod: string
    // Effects: Checks that all of vars belong to module mod
    foreach(fn (v) check_present(v, global_lookup(v), mod), vars);

  check_dependencies = fn (deps, mod)
    // Types: deps: list of dependencies, mod: string
    // Effects: Checks that all vars belong to module mod and that the
    //   depdendencies are respected
    foreach(fn (d)
	     [
	       | n, v |

	       v = d[0];
	       n = global_lookup(v);
	       if (check_present(v, n, mod)) check_dependency(d, n);
	     ],
	     deps);
		   
  make_code = fn (prelinked_fn, seclev)
    [
      | csts, info |

      // make code for all sub-functions
      csts =
	append!(map(fn (off) make_code(car(off), seclev) . cdr(off),
		     prelinked_fn[pfn_subfns]),
	append!(map(fn (kglobal)
		       global_value(global_lookup(car(kglobal))) . cdr(kglobal),
		     prelinked_fn[pfn_kglobals]),
		prelinked_fn[pfn_constants]));

      link(prelinked_fn[pfn_code], seclev, prelinked_fn[pfn_help],
	   prelinked_fn[pfn_varname],
	   prelinked_fn[pfn_filename], prelinked_fn[pfn_lineno],
	   csts,
	   prelinked_fn[pfn_builtins],
	   prelinked_fn[pfn_globals],
	   prelinked_fn[pfn_primitives])
    ];

  // Prelinking
  // ----------

  mc:prelink = fn (mod, protect)
    // Types: mod: module after phase4, protect: boolean
    // Effects: Does a "prelink" of module and returns a value that may be:
    //   a) saved to disk with obj_save
    //   b) made into an executable function with mc:link
    //   If protect is true, module is marked as "protected"
    // Returns: Prelinked module
    vector(mod[mc:m_class],
	   protect and true,
	   mod[mc:m_name],
	   dependencies(mod[mc:m_imports]),
	   lmap(fn (v) cdr(v), mod[mc:m_defines]),
	   lmap(fn (v) cdr(v), mod[mc:m_reads]),
	   lmap(fn (v) cdr(v), mod[mc:m_writes]),
	   prelink(mod[mc:m_body]));

  dependencies = fn (imports)
    // Types: imports: table of import lists
    // Effects: Returns dependency information for this module, based
    //   on the current state of the symbols from the modules that it
    //   required.
    [
      lforeach
	(fn (imp)
	 [
	   | minfo |

	   minfo = symbol_get(imp);
	   if (car(minfo) == module_loaded)
	     // weak dependencies: symbols must simply exist
	     // simply keep list of symbol names
	     set_cdr!(minfo, lmap(fn (v) v[mc:v_name], cdr(minfo)))
	   else if (car(minfo) == module_protected)
	     // strong dependencies: may depend on type, value, etc of symbol
	     set_cdr!(minfo, lmap(fn (v) dependency(v), cdr(minfo)));
	 ],
	 table_list(imports));

      imports
    ];

  prelink = fn (top)
    // Types: top: assembled intermediate function
    // Returns: Prelinked form of function top
    [
      | info |

      info = cdr(top[mc:c_fvalue]);

      vector(car(top[mc:c_fvalue]), // code as string
	     top[mc:c_fhelp], // help string
	     if (top[mc:c_fvar]) top[mc:c_fvar][mc:v_name] else null, // variable function stored in
	     top[mc:c_ffilename],
	     top[mc:c_flineno],
	     lmap(fn (foffset) prelink(car(foffset)) . cdr(foffset),
		  info[mc:a_subfns]), // sub-functions
	     info[mc:a_constants], // constants
	     info[mc:a_builtins], // builtins
	     info[mc:a_globals], // globals (offsets)
	     info[mc:a_kglobals], // globals (constant)
	     info[mc:a_primitives]) // primitives
    ];


  // Dependency checking
  // -------------------

  // dependency types:
  depend_immutable = 1;		// or'ed into any other depend_xxx

  depend_none = 0;		// no checks
  depend_primitive = 2;		// primitive with args & flags (later: signature)
  // future dependencies:
  depend_type = 4;		// type
  depend_integer = 6;		// integer with given value
  depend_signature = 8;		// function with compatible signature

  dependency = fn (v)
    // Types: v: global constant variable
    // Returns: dependency information for constant global v
    [
      | val, name, type |

      // Currently, we extract the maximum possible dependencies.
      // However, the code generation could simply record in v what
      // assumptions it needs.

      name = v[mc:v_name];
      val = global_value(v[mc:v_goffset]);
      type = typeof(val);
      if (type == type_primitive)
	vector(name, depend_primitive, primitive_nargs(val), primitive_flags(val))
      else if (type == type_integer)
	vector(name, depend_integer, val)
      else // type dependency
	vector(name, depend_type | (if (immutable?(val)) depend_immutable else 0),
	       type)
    ];

  typenames = 
    '["code" "closure" "variable" "internal" "primitive" "varargs" "secure"
      "integer" "string" "vector" "list" "symbol" "table" "private"
      "object" "character" "gone" "output-port" "mcode" "null" ];


  check_dependency = fn (d, n)
    // Types: d: dependency, n: int (index of d[0])
    // Effects: checks that dependency d is still valid
    //   giving appropriate error messages
    [
      | name, val, dtype |

      name = d[0];
      val = global_value(n);
      dtype = d[1] & ~depend_immutable;

      if ((d[1] & depend_immutable) && !immutable?(val))
	error("%s is not a constant anymore", name)
      else if (dtype == depend_primitive)
	[
	  if (!primitive?(val) ||
	      primitive_nargs(val) != d[2] ||
	      d[3] & ~primitive_flags(val))
	    error("primitive %s has suffered an incompatible change", name);
	]
      else if (dtype == depend_integer)
	[
	  if (val != d[2])
	    error("%s is not %s anymore", name, d[2]);
	]
      else if (dtype == depend_type)
	[
	  if (typeof(val) != d[2])
	    error("the type of %s has changed (was %s, now %s)",
		  name, typenames[d[2]], typenames[typeof(val)]);
	];
      // add other dtypes here...
    ];

  check_present = fn (v, n, mod)
    // Types: v: string, n: int, mod: string
    // Effects: Checks that v (index n) belongs to mod (sends error message)
    // Returns: false if not
    [
      | vstatus |

      vstatus = module_vstatus(n);
      if (!string?(vstatus) || string_icmp(vstatus, mod) != 0)
	[
	  error("%s does not belong to %s anymore", v, mod);
	  false
	]
      else
	true
    ];	

];
