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

library vars
requires system, misc
defines
  mc:v_class, mc:v_name, mc:v_number, mc:v_indirect, mc:v_uses, mc:svar,
  mc:v_neighbours, mc:v_alias, mc:v_location, mc:v_lclass, mc:v_lregister,
  mc:v_lrtype, mc:v_lrnumber, mc:v_lspill, mc:v_lstype, mc:v_lsoffset,
  mc:reg_scratch, mc:reg_caller, mc:reg_callee, mc:spill_closure, mc:spill_args,
  mc:spill_spill, mc:v_global, mc:v_goffset, mc:v_local, mc:v_lclosure_uses,
  mc:closure_read, mc:closure_write, mc:v_closure, mc:v_cparent, mc:v_constant,
  mc:v_kvalue, mc:v_global_constant, mc:var_make_local, mc:var_make_dglobal,
  mc:var_make_global, mc:var_make_constant, mc:var_base, mc:var_value,
  mc:alias_base, mc:alias, mc:var_make_closure, mc:var_make_kglobal, 
  mc:v_global_define, mc:in_reg, mc:get_reg

[
// Variable handling.

// Variables are represented by a vector, as follows:
// Variable structure:
//   [0] : class (mc:v_global, mc:v_local, mc:v_closure, mc:v_constant)
//   [1] : name (string)
//   [2] : class dependent data
//   [3..n] : other data

mc:v_class = 0; // class of variable
mc:v_name = 1; // name of variable (string)
//mc:v_data = 2; // class dependent data
mc:v_number = 3; // unique number (for display)
mc:v_indirect = 4; // true if variable is indirect
mc:v_uses = 5; // du-chain for this variable
mc:v_neighbours = 5; // varset of interfering variables (implicit graph,  register allocation)
mc:v_alias = 5; // variable this is an alias of


mc:v_location = 6; // where variable is stored (register, spill, etc)
 mc:v_lclass = 0;
  mc:v_lregister = 0;
   mc:v_lrtype = 1; // type of register (scratch, etc)
   mc:v_lrnumber = 2; // it's sequence number
  mc:v_lspill = 1;
   mc:v_lstype = 1; // where variable spilled (closure, etc)
   mc:v_lsoffset = 2; // offset in spill record

mc:reg_scratch = 0;
mc:reg_caller = 1;
mc:reg_callee = 2;

mc:spill_closure = 0;
mc:spill_args = 1;
mc:spill_spill = 2;

mc:v_global = 0;
 mc:v_goffset = 2; // data: offset of global variable (integer)

mc:v_local = 1;
 mc:v_lclosure_uses = 2; // data: how variable is used in closures (read, write, bitmask)
 mc:closure_read = 1; // bitmasks for above field
 mc:closure_write = 2;

mc:v_closure = 2;
 mc:v_cparent = 2; // data: variable of parent (variable)

mc:v_constant = 3;
 mc:v_kvalue = 2; // data: value of constant

// global variables imported from modules 

mc:v_global_constant = 4; // from protected module
 // uses mc:v_goffset

mc:v_global_define = 5; // from own or loaded module
 // uses mc:v_goffset

[
  | globals, kglobals, dglobals, vindex, make_global |

  vindex = 0;
  globals = make_table();
  dglobals = make_table();
  kglobals = make_table();

  make_global = 
    fn (globals, type)
      fn (name, n)
	[
	  | gv |

	  gv = globals[name];
	  if (gv != null)
	    [
	      gv[mc:v_uses] = null; // reset in case of bug
	      gv // reuse previously created global var
	    ]
	  else 
	    [
	      | gvar |

	      gvar = vector(type, name, n,
			    vindex = vindex + 1, false, null, false);
	      globals[name] = gvar
	    ]
	];

  mc:var_make_global = make_global(globals, mc:v_global);

  mc:var_make_kglobal = make_global(kglobals, mc:v_global_constant);

  mc:var_make_dglobal = make_global(dglobals, mc:v_global_define);

  mc:var_make_constant = fn "x -> var. Returns a new constant variable with value x" (x)
    vector(mc:v_constant, "", x, vindex = vindex + 1, false, null, false);

  mc:var_make_local = fn "s -> var. Returns a new local variable (name s)" (name)
    vector(mc:v_local, name, 0, vindex = vindex + 1, false, null, false);

  mc:var_make_closure = fn "s var1 -> var2. Returns a new closure variable (name s) with parent var1" (name, parent)
    vector(mc:v_closure, name, parent, vindex = vindex + 1, false, null, false);

  mc:var_base = fn "var1 -> var2. Returns the real local variable of closure var var1" (v)
    [
      while (v[mc:v_class] == mc:v_closure)
        v = v[mc:v_cparent];
      v
    ];

  mc:var_value = fn "var -> x. Returns value of constant variable var" (v)
    v[mc:v_kvalue];

  mc:alias_base = fn "var1 -> var2. Returns the variable var1 is aliased to" (v)
    [
      | n |

      while ((n = v[mc:v_alias]) != null) v = n;
      v
    ];

  mc:alias = fn "var1 var2 -> . Makes var1 an alias for var2" (v1, v2)
    v1[mc:v_alias] = v2;


  mc:svar = fn (var)
    [
      | class, base, loc |

      base = 
	[
	  class = var[mc:v_class];
	  if (class == mc:v_global) format("global %s(%s)", var[mc:v_name], var[mc:v_number])
	  else if (class == mc:v_global_constant) format("kglobal %s", var[mc:v_name])
	  else if (class == mc:v_global_define) format("dglobal %s", var[mc:v_name])
	  else if (class == mc:v_constant) format("%s", var[mc:v_kvalue])
	  else if (class == mc:v_local)
	    [
	      | type |
	      type = if (var[mc:v_indirect]) "indirect" else "local";

	      if (string_length(var[mc:v_name]) > 0)
		format("%s %s(%s)", type, var[mc:v_number], var[mc:v_name])
	      else
		format("%s %s", type, var[mc:v_number])
	    ]
	  else if (class == mc:v_closure)
	    format("%s %s(%s)", if (var[mc:v_indirect]) "indclosure" else "closure",
		   var[mc:v_number], mc:svar(var[mc:v_cparent]))
	  else fail();
	];

      if (loc = var[mc:v_location])
	base + "{" +
	  [
	    class = loc[mc:v_lclass];
	    if (class == mc:v_lregister)
	      format("%s %s", '["scratch" "caller" "callee"][loc[mc:v_lrtype]],
		     loc[mc:v_lrnumber])
	    else if (class == mc:v_lspill)
	      format("%s %s", '["closure" "args" "spill"][loc[mc:v_lstype]],
		     loc[mc:v_lsoffset])
	    else fail();
	  ] + "}"
      else
        base
    ];

  mc:in_reg = fn (var)
    var[mc:v_location] && var[mc:v_location][mc:v_lclass] == mc:v_lregister;

  mc:get_reg = fn (var)
    if (mc:in_reg(var)) var[mc:v_location][mc:v_lrnumber]
    else -1;
];

];
