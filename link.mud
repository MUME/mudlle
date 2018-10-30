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

library link // A linker.
requires compiler, misc, sequences, vars
defines mc:prelink, mc:display_as
reads mc:describe_seclev, mc:compiler_mcode_version
writes mc:this_filenames, mc:this_module, mc:erred, mc:linkrun
[

  | make_code, seen_code, prelink, mstart,
    pmodule_name, pmodule_imports, pmodule_defines, pmodule_writes,
    pmodule_protect, pmodule_class, pmodule_reads, pmodule_body,
    pmodule_version, prelinked_module_fields,
    pfn_code, pfn_help, pfn_varname, pfn_filename, pfn_loc, pfn_subfns,
    pfn_constants, pfn_builtins, pfn_globals, pfn_kglobals, pfn_kglobal_code,
    pfn_primitives, pfn_rel_primitives, pfn_return_typeset, pfn_return_itype,
    pfn_linenos, pfn_arguments, pfn_seclevs, pfn_flags, pfn_nicename,
    prelinked_function_fields,
    check_presence, check_dependencies, dependencies, depend_immutable,
    depend_primitive, depend_type, depend_closure, depend_value,
    depend_vstatus, dependency, check_dependency, check_present,
    set_this_filenames,
    prelinked_fns,
    describe_seclev,
    my:assq, my:lappend!, my:lforeach, my:lmap, my:mc:error, my:mc:loc_line,
    my:mc:warning, my:mcode_version, my:vmap,
    word_size |

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
  pmodule_reads = 5;		// read symbols (list of string)
  pmodule_writes = 6;		// written symbols (list of string)
  pmodule_body = 7;		// top-level function (prelinked function)
  pmodule_version = 8;          // mc:arch | (mc:mcode_version << 8)

  prelinked_module_fields = 9;

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
  pfn_code = 0;		   // machine code (string)
  pfn_help = 1;		   // help (string or null)
  pfn_varname = 2;	   // varname (string or null)
  pfn_nicename = 3;	   // pretty-printed filename (string or null)
  pfn_loc = 4;             // (line . column)
  // references (all of the form list of <info> . offset,
  // where offset is an integer offset into code
  pfn_subfns = 5;	   // sub-functions list(pfn . offset)
  pfn_constants = 6;	   // constants list(value . offset)
  pfn_builtins = 7;	   // builtins list(string . offset)
  pfn_globals = 8;	   // globals list(string . offset)
  pfn_kglobals = 9;	   // constant globals list(string . offset)
  pfn_kglobal_code = 10;   // constant global code list(string . offset)
  pfn_primitives = 11;	   // absolute prims list(string . offset)
  pfn_rel_primitives = 12; // (x86) relative prims list(string . offset)
                           // (x64) number of PC-relative addresses after code
  pfn_seclevs = 13;	   // seclevs list(offset)
  pfn_return_typeset = 14; // return typeset
  pfn_return_itype = 15;   // return itype
  pfn_linenos = 16;        // DWARF line number information
  pfn_arguments = 17;      // vector(name|false . typeset); name for varargs
  pfn_flags = 18;          // clf_xxx flags
  pfn_filename = 19;	   // filename (string or null)

  prelinked_function_fields = 20;

  // Linking
  // -------

  // linkrun can unload anything, hence it should only depend on stuff in
  // system and local vars while running
  my:assq        = assq;
  my:lappend!    = lappend!;
  my:lforeach    = lforeach;
  my:lmap        = lmap;
  my:vmap        = vmap;
  my:mc:error    = mc:error;
  my:mc:loc_line = mc:loc_line;
  my:mc:warning  = mc:warning;

  my:mcode_version = if (integer?(mc:compiler_mcode_version))
    mc:compiler_mcode_version
  else
    mc:mcode_version;

  word_size = (INTBITS + 1) / 8;

  describe_seclev = fn (int lev)
    if (function?(mc:describe_seclev))
      mc:describe_seclev(lev)
    else
      lev;

  mc:display_as = fn "`v -> . Displays GNU assembler corresponding to prelinked object file `v." (prelink)
    [
      | all_fn_names, all_fns, fns, print_fn, func_name, sname |
      all_fn_names = make_table();

      sname = fn (s)
        smap(fn (c) [
          if (calpha?(c) || cdigit?(c))
            c
          else if (c == ??)
            ?p
          else if (c == ?!)
            ?b
          else
            ?_
        ], s);

      func_name = fn (f)
        [
          | p |
          p = my:assq(f, all_fns);
          if (p)
            exit<function> cdr(p);

          | s, base, n |
          s = f[pfn_varname];
          s = base = if (s == null)
            format("fn_%d", my:mc:loc_line(f[pfn_loc]))
          else
            sname(s);
          n = 0;
          while (all_fn_names[s] != null)
            s = format("%s_%d", base, ++n);
          all_fn_names[s] = f;
          all_fns = (f . s) . all_fns;
          s
        ];

      print_fn = fn (f)
        [
          | data |
          data = lreduce(fn (@(s . o), d) [
            if (!my:assq(s, all_fns))
              fns = s . fns;
            s = func_name(s);
            if (o < 0)
              [
                s = "closure_" + s;
                o = -o;
              ];
            (s . o) . d
          ], data, f[pfn_subfns]);
          data = lreduce(fn (@(c . o), d) [
            (("cst_" + type_names[typeof(c)]) . o) . d
          ], data, f[pfn_constants]);
          data = lappend(f[pfn_builtins], data);
          data = lreduce(fn (g, d) [
            | n |
            n = match (car(g))
              [
                (name . 0) => "gidx_" + name;
                (name . 1) => "mgidx_" + name;
                name => "glb_" + name;
              ];
            ((n . 4) . cdr(g)) . d
          ], data, f[pfn_globals]);
          data = lappend(f[pfn_kglobals], data);
          data = lreduce(fn (@(k . o), d) [
            (("code-" + k) . o) . d
          ], data, f[pfn_kglobal_code]);
          data = lappend(f[pfn_primitives], data);
          if (!integer?(f[pfn_rel_primitives]))
            data = lappend(f[pfn_rel_primitives], data);
          data = lreduce(fn (@(l . o), d) [
            (('["seclev" "mseclev" "maxlev"][l] . '[2 4 4][l]) . o)
              . d
          ], data, f[pfn_seclevs]);

          data = '(() . ,maxint) . data;

          data = lqsort(fn (a, b) cdr(a) < cdr(b), data);

          | fname |
          fname = func_name(f);
          dformat(".align 16\n.globl %s\n%s:\n", fname, fname);

          | code, nextra |
          code = f[pfn_code];
          nextra = if (integer?(f[pfn_rel_primitives]))
            [
              // pad if necessary
              ((-slength(code) & (word_size - 1))
               + f[pfn_rel_primitives] * ((INTBITS + 1) / 8))
            ]
          else
            0;
          for (|i, l, cl| [ i = 0; cl = slength(code); l = cl + nextra ]; ; )
            [
              | e |
              e = min(cdar(data), l);
              | prefix |
              prefix = ".byte ";
              <at_e> while (i < e)
                [
                  if (i == cl)
                    [
                      dformat("\n.size %s, . - %s\n", fname, fname);
                      if (nextra > 0)
                        [
                          dformat("\n.align %d\n", word_size);
                          i = (i + word_size - 1) & -word_size;
                          if (i == e)
                            exit<at_e> null;
                        ];
                      prefix = ".byte ";
                    ];
                  dformat("%s%d", prefix, if (i < cl) code[i] else 0);
                  ++i;
                  prefix = ", ";
                ];
              display("\n");
              if (i == l) exit<break> null;
              | arg |
              arg = caar(data);
              if (!pair?(arg))
                arg = arg . ((INTBITS + 1) / 8);
              match (arg)
                [
                  (s . 2) => [ dformat(".word %s\n", sname(s)); i += 2 ];
                  (s . 4) => [ dformat(".long %s\n", sname(s)); i += 4 ];
                  (s . 8) => [ dformat(".quad %s\n", sname(s)); i += 8 ];
                  _ => fail();
                ];
              data = cdr(data);
            ];
          display("\n");
        ];

      fns = prelink[pmodule_body] . null;
      while (fns != null)
        [
          | f |
          @(f . fns) = fns;
          print_fn(f);
        ];
    ];

  // false - failed linking
  // null  - failed running
  // (true . result)  - ok
  mc:linkrun = fn (prelinked_module, seclev, reload)
    [
      | res, mname |

      mname = prelinked_module[pmodule_name];
      if (!reload && mname && module_status(mname) != module_unloaded)
	exit<function> '(,true . ());

      res = false;
      mc:erred = false;
      mc:this_module = null;
      if (mstart(prelinked_module, seclev))
	[
	  if (!mc:erred) // run only if no errors
	    [
	      | code, codev |

              seen_code = null;
	      code = make_code(prelinked_module[pmodule_body], seclev, true);
              codev = my:lmap(fn (x) cdr(cdr(x)), seen_code);
              register_mcode_module(codev);
              seen_code = null;
              res = true;
              | val |
	      val = with_maxseclevel(
                seclev,
                fn() trap_error(code, fn (n) res = null, call_trace_barrier));
              if (res == true)
                res = true . val;
	    ];

	  if (mname)
	    if (pair?(res) && prelinked_module[pmodule_protect])
	      [
		module_set!(mname, module_protected, seclev);
		// check immutability, otherwise dependencies on immutable
		// symbols of the loaded module might fail
		detect_immutability();
	      ]
	    else
	      module_set!(mname,
			  if (pair?(res)) module_loaded else module_error,
			  seclev);
	];

      mc:this_module = null;
      mc:this_filenames = null;

      res
    ];

  set_this_filenames = fn (m)
    [
      | b |
      b = m[pmodule_body];
      mc:this_filenames = b[pfn_filename] . b[pfn_nicename]
    ];

  | module_describe |
  module_describe = fn (vstatus)
    if (string?(vstatus))
      format("belongs to the %s module", vstatus)
    else if (vstatus == var_system_write)
      "cannot be written from mudlle"
    else if (vstatus == var_system_mutable)
      "belongs to the system module"
    else
      fail();

  mstart = fn (m, seclev)
    [
      if (vlength(m) != prelinked_module_fields
          || vlength(m[pmodule_body]) != prelinked_function_fields
          || (m[pmodule_version] & 0xff) != mc:arch
          || (m[pmodule_version] >> 8) < mc:mcode_version)
        [
          dformat("%s: object file has bad version\n", m[pmodule_name]);
          exit<function> false;
        ];

      set_this_filenames(m);

      | mname |
      mname = m[pmodule_name];
      if (mname)
	[
	  // unload it
	  if (module_seclevel(mname) > seclev)
	    [
	      dformat("cannot unload module %s (seclevel %s > %s)\n",
                      mname, describe_seclev(module_seclevel(mname)),
                      describe_seclev(seclev));
	      exit<function> false;
	    ];
	  if (!module_unload(mname))
            [
              dformat("cannot unload protected module %s\n", mname);
              exit<function> false;
            ];
	  module_set!(mname, module_loading, seclev);
	];

      if (seclev < SECLEVEL_GLOBALS)
        [
          if (m[pmodule_class] == mc:m_plain)
            my:mc:error("cannot create 'plain' modules at seclevel %s"
                        +" (use a module or lib instead)",
                        describe_seclev(seclev));

          if (m[pmodule_writes] != null)
            my:mc:error("cannot write globals at seclevel %s",
                        describe_seclev(seclev));

          if (m[pmodule_reads] != null)
            my:mc:error("cannot read globals at seclevel %s",
                        describe_seclev(seclev));
        ];


      // load & check imported modules
      table_foreach
	(fn (@<iname = minfo>)
	 [
	   | mstatus, erred |
	   erred = mc:erred;
	   mstatus = module_require(iname);
	   mc:this_module = null; // could be set by module_require
           set_this_filenames(m);
	   mc:erred = erred;

	   if (mstatus < module_loaded)
	     [
	       if (mstatus == module_loading)
		 my:mc:error("loop in requires of %s", iname)
	       else
		 my:mc:error("failed to load %s", iname);
	     ]
	   else if (car(minfo) == module_loaded)
	     check_presence(cdr(minfo), iname)
	   else if (car(minfo) == module_protected)
	     if (mstatus != module_protected)
	       my:mc:error("%s is not protected anymore", iname)
	     else
	       check_dependencies(cdr(minfo), iname);
	 ],
	 m[pmodule_imports]);

      // Change status of variables
      my:lforeach
	(fn (var)
	 [
	   | n, vstatus |

	   n = global_lookup(var);
	   vstatus = module_vstatus(n);

	   if (vstatus == var_write && seclev < SECLEVEL_GLOBALS)
	     my:mc:error("cannot define %s: exists and is writable", var)
	   else if (!module_vset!(n, mname))
	     my:mc:error("cannot define %s: %s", var, module_describe(vstatus))
	   else if (vstatus == var_write)
	     my:mc:warning("%s was writable", var);
	 ],
	 m[pmodule_defines]);

      my:lforeach
	(fn (var)
	 [
	   | n |

	   n = global_lookup(var);

	   if (!module_vset!(n, var_write))
             my:mc:error("cannot write %s: %s", var,
                      module_describe(module_vstatus(n)));
	 ],
	 m[pmodule_writes]);

      true // can proceed
    ];

  check_presence = fn (vars, mod)
    // Types: vars: list of string, mod: string
    // Effects: Checks that all of vars belong to module mod
    my:lforeach(fn (v) check_present(v, global_lookup(v), mod), vars);

  check_dependencies = fn (deps, mod)
    // Types: deps: list of dependencies, mod: string
    // Effects: Checks that all vars belong to module mod and that the
    //   depdendencies are respected
    my:lforeach(fn (d)
	     [
	       | n, v |

	       v = d[0];
	       n = global_lookup(v);
	       if (check_present(v, n, mod)) check_dependency(d, n);
	     ],
	     deps);

  make_code = fn (prelinked_fn, seclev, want_closure)
    [
      // seen_code is a list of (prelinked . (closure . mcode))

      | found |
      if (found = my:assq(prelinked_fn, seen_code))
        exit<function> [
          found = cdr(found);
          if (want_closure)
            [
              if (!car(found))
                set_car!(found, make_closure(cdr(found)))
              else
                car(found)
            ]
          else
            cdr(found)
        ];

      found = prelinked_fn . false;
      seen_code = found . seen_code;

      | csts |

      // make code for all sub-functions
      csts = prelinked_fn[pfn_constants];

      csts = my:lappend!(my:lmap(fn (@(name . ofs)) [
        global_value(global_lookup(name)) . ofs
      ], prelinked_fn[pfn_kglobals]), csts);

      csts = my:lappend!(my:lmap(fn (@(name . ofs)) [
        | cl |
        cl = global_value(global_lookup(name));
        // assert() may not be available
        if (!(closure?(cl) && (closure_flags(cl) & clf_noclosure)))
          error(error_abort);
        closure_code(cl) . ofs
      ], prelinked_fn[pfn_kglobal_code]), csts);

      csts = my:lappend!(my:lmap(fn (@(f . ofs)) [
        | c? |
        if (ofs < 0)
          [
            c? = true;
            ofs = -ofs;
          ]
        else
          c? = false;
        make_code(f, seclev, c?) . ofs
      ], prelinked_fn[pfn_subfns]), csts);

      | f |
      f = link(
        prelinked_fn[pfn_code], seclev, prelinked_fn[pfn_help],
        prelinked_fn[pfn_varname],
        prelinked_fn[pfn_filename], prelinked_fn[pfn_loc],
        csts,
        prelinked_fn[pfn_builtins],
        prelinked_fn[pfn_globals],
        prelinked_fn[pfn_primitives],
        prelinked_fn[pfn_rel_primitives],
        prelinked_fn[pfn_seclevs],
        prelinked_fn[pfn_return_typeset],
        prelinked_fn[pfn_return_itype],
        prelinked_fn[pfn_linenos],
        prelinked_fn[pfn_arguments],
        prelinked_fn[pfn_flags],
        prelinked_fn[pfn_nicename]);
      if (want_closure)
        [
          | c |
          c = make_closure(f);
          set_cdr!(found, c . f);
          c
        ]
      else
        [
          set_cdr!(found, false . f);
          f
        ]
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
    [
      prelinked_fns = null;
      sequence(
        mod[mc:m_class],
        !!protect,
        mod[mc:m_name],
        dependencies(mod[mc:m_requires]),
        lmap(fn (v) v[mc:mv_name], mod[mc:m_defines]),
        lmap(fn (v) v[mc:mv_name], mod[mc:m_reads]),
        lmap(fn (v) v[mc:mv_name], mod[mc:m_writes]),
        prelink(mod[mc:m_body]),
        mc:arch | (my:mcode_version << 8))
    ];

  dependencies = fn (imports)
    // Types: imports: table of import lists
    // Effects: Returns dependency information for this module, based
    //   on the current state of the symbols from the modules that it
    //   required.
    [
      table_foreach
	(fn (imp)
	 [
	   | mstatus, syms |

	   @[mstatus _ syms] = symbol_get(imp);
	   if (mstatus == module_loaded)
	     // weak dependencies: symbols must simply exist
	     // simply keep list of symbol names
             symbol_set!(imp, mstatus . lmap(fn (v) v[mc:v_name], syms))
	   else if (mstatus == module_protected)
	     // strong dependencies: may depend on type, value, etc of symbol
             symbol_set!(imp, mstatus . lmap(fn (v) dependency(v), syms))
	 ],
	 imports);

      imports
    ];

  prelink = fn (top)
    // Types: top: assembled intermediate function
    // Returns: Prelinked form of function top
    [
      | info, arguments, args_from_list, flags, pfn_marker |

      match (my:assq(top, prelinked_fns))
        [
          ,false => null;           // fallthrough
          (_ . v) && vector?(v) => exit<function> v;
          (_ . ,false) => fail_message("recursion in prelink()");
          _ => fail();
        ];

      // add marker to catch any recursion
      pfn_marker = top . false;
      prelinked_fns = pfn_marker . prelinked_fns;

      args_from_list = fn (list args)
        [
          | v |
          v = make_vector(llength(args));
          for (|i| i = 0; args != null; ++i)
            [
              | var, ts, name |
              @([ var ts _ ] . args) = args;
              name = var[mc:v_name];
              if (name[0] == ?$)
                name = false;
              v[i] = '(,name . ,ts);
            ];
          check_immutable(protect(v))
        ];

      if (top[mc:c_fvarargs])
        [
          | var |
          @([var _ _]) = top[mc:c_fargs];
          arguments = var[mc:v_name];
        ]
      else
        arguments = args_from_list(top[mc:c_fargs]);

      assert(immutable?(arguments));

      info = cdr(top[mc:c_fvalue]);

      flags = clf_compiled;
      if (top[mc:c_fnoescape])
        flags |= clf_noescape;
      if (top[mc:c_fclosure] == null)
        flags |= clf_noclosure;

      | linenos, var, subfns |
      linenos = dwarf_line_number_info(info[mc:a_linenos]);
      var     = if (top[mc:c_fvar]) top[mc:c_fvar][mc:v_name] else null;
      subfns  = lmap(fn (foffset) prelink(car(foffset)) . cdr(foffset),
                     info[mc:a_subfns]);

      set_cdr!(pfn_marker, sequence(
        car(top[mc:c_fvalue]),      // code as string
        top[mc:c_fhelp],            // help string
        var,                        // variable function stored in
        top[mc:c_fnicename],        // human-friendly file name
        top[mc:c_loc],              // location (line . column)
        subfns,                     // sub-functions
        info[mc:a_constants],       // constants
        info[mc:a_builtins],        // builtins
        info[mc:a_globals],         // globals (offsets)
        info[mc:a_kglobals],        // globals (constant)
        info[mc:a_kglobal_code],    // globals (code object)
        info[mc:a_primitives],      // absolute primitives
        info[mc:a_rel_primitives],  // relative primitives
        info[mc:a_seclevs],         // seclevels
        top[mc:c_freturn_typeset],  // return typeset
        top[mc:c_freturn_itype],    // return itype
        linenos,                    // line numbers
        arguments,                  // arguments
        flags,                      // flags
        top[mc:c_ffilename]))       // file name on disk
    ];


  // Dependency checking
  // -------------------

  // dependency types:
  depend_immutable = 1;		// or'ed into any other depend_xxx

  // depend_none = 0;		// no checks
  depend_primitive = 2;		// primitive: type, args, flags, and signature
  depend_type = 4;		// type
  depend_value = 6;		// variable with given value compared using
  				// equal?()
  depend_closure = 8;		// closure with argument and return types
  depend_vstatus = 10;          // depend on module_vstatus()

  | closure_arg_depends |
  closure_arg_depends = fn (c)
    [
      | val |
      val = closure_arguments(c);
      if (vector?(val))
        protect(my:vmap(cdr, val))
      else
        null
    ];

  dependency = fn (v)
    // Types: v: global constant or var_system_xxx variable
    // Returns: dependency information for constant global v
    [
      | val, name, type, immutable_flag, vstat |

      // Currently, we extract the maximum possible dependencies.
      // However, the code generation could simply record in v what
      // assumptions it needs.

      name = v[mc:v_name];

      vstat = module_vstatus(v[mc:v_goffset]);
      if (vstat == var_system_mutable || vstat == var_system_write)
        exit<function> vector(name, depend_vstatus, vstat);

      val = global_value(v[mc:v_goffset]);
      type = typeof(val);

      immutable_flag = if (immutable?(val)) depend_immutable else 0;

      if (type == type_primitive || type == type_varargs
          || type == type_secure)
	vector(name, depend_primitive, type, primitive_nargs(val),
               primitive_flags(val) & ~COMPILER_SAFE_OP_FLAGS,
	       primitive_type(val))
      else if (type == type_closure)
        vector(name,
               depend_closure | immutable_flag,
               closure_return_typeset(val),
               closure_arg_depends(val),
               closure_flags(val))
      else if (type == type_integer || type == type_float)
	vector(name, depend_value, val)
      else // type dependency
	vector(name, depend_type | immutable_flag, type)
    ];

  check_dependency = fn (d, n)
    // Types: d: dependency, n: int (index of d[0])
    // Effects: checks that dependency d is still valid
    //   giving appropriate error messages
    [
      | name, val, dtype, check_cargs |

      // return true if 'dep' and 'current' closure arguments are
      // compatible
      check_cargs = fn ({null,vector} dep, {null,vector} current)
        [
          if (dep == null)
            exit<function> current == null;
          if (current == null)
            exit<function> false;

          | len |
          len = vector_length(dep);
          if (len != vector_length(current))
            exit<function> false;
          for (|i| i = 0; i < len; ++i)
            if (~dep[i] & current[i])
              exit<function> false;
          true
        ];

      name = d[0];
      val = global_value(n);
      dtype = d[1] & ~depend_immutable;

      if ((d[1] & depend_immutable) && !immutable?(val))
	my:mc:error("%s is not immutable anymore", name)
      else if (dtype == depend_primitive)
	[
	  if (typeof(val) != d[2] ||
	      primitive_nargs(val) != d[3] ||
	      d[4] & ~COMPILER_SAFE_OP_FLAGS & ~primitive_flags(val) ||
              !equal?(d[5], primitive_type(val)))
	    my:mc:error("primitive %s has suffered an incompatible change", name);
	]
      else if (dtype == depend_closure)
	[
	  if (vector_length(d) != 5 ||
              !closure?(val) ||
              (~d[2] & closure_return_typeset(val)) ||
              !check_cargs(d[3], closure_arg_depends(val)) ||
              (d[4] & ~closure_flags(val)))
	    my:mc:error("closure %s has suffered an incompatible change", name);
	]
      else if (dtype == depend_value)
	[
	  if (!equal?(val, d[2]))
	    my:mc:error("%s is not %w anymore", name, d[2]);
	]
      else if (dtype == depend_type)
	[
	  if (typeof(val) != d[2])
	    my:mc:error("the type of %s has changed (was %s, now %s)",
		  name, type_names[d[2]], type_names[typeof(val)]);
	]
      else if (dtype == depend_vstatus)
        [
          | ovstat |
          ovstat = d[2];
          if (equal?(module_vstatus(n), ovstat))
            null
          else if (ovstat == var_system_write || ovstat == var_system_mutable)
            my:mc:error("%s is not a system variable anymore", name)
          else
            fail()
        ]
      else
        fail()
    ];

  check_present = fn (string vname, int n, string mod)
    // Effects: Checks that vname (global n) belongs to mod
    // Returns: false if not
    [
      | vstatus |

      vstatus = module_vstatus(n);
      if (vstatus == var_system_write || vstatus == var_system_mutable)
        vstatus = "system";
      if (equal?(vstatus, mod))
        exit<function> true;
      my:mc:error("%s does not belong to %s anymore", vname, mod);
      false
    ];

];
