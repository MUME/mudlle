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

/* Simple type inference for mudlle

  Based on a "constraint model":
    - a first pass deduces the constraints on the types of variables induced
      by each intermediate instruction
    - a second pass solves these constraints, using standard data-flow
      techniques (the constraints are such that this is possible)
      this produces possible types for each variable at the start of each
      block, this can then easily be used in the code generation phase to
      generate better code for the intermediate instructions

  A constraint expresses the idea that if the arguments of an instruction
  follow certain type relations, the result will follow some (possibly
  distinct) relation.

  Types
  -----

  This simple type inference scheme has a simple notion of the "possible type"
  of a variable: a subset of the base mudlle types. To simplify things,
  some types that are considered distinct by the implementation are merged
  into a single type. So the possible type is actually a subset of:

   { function (= { closure, primitive, varargs, secure })
     integer
     string
     vector
     null
     symbol
     table
     pair
     other (= { object, character, gone, private })
   }

  'function' is a group as the differences between these types are
  supposed to be invisible (hmm).

  'other' represents types that are both not usefully inferred (see below),
  and which can not be distinguished anyway (values of type character or
  object can mutate into values of type gone, invisibly)

  So for example, the possible type of variable x after:

    if (a) x = 3
    else x = "fun";

  is: { integer, string }

  If a variable is used as an argument and has an empty type set then the
  function contains a type error. One special type set is important: 
  "any", ie all the above types.

  The inferred types serve only to improve the code for branch and
  compute operations:
    - primitives are written in C, making specialised versions without
      (some) type-checking would be prohibitive
    - global mudlle variables may change at anytime after compile & link, 
      thus nothing useful can be done with calls to their contents
    - the compiler does no inter-procedural analysis


  Constraints
  -----------

  Back to constraints: for each instruction, a set of constraints is
  generated, the instruction will produce no type error if any of them
  is satisfied (this reflects the fact that operators and functions may
  be dynamically overloaded). All constraints are of the following form:

    condition1 & condition2 & ... => consequence

  where a condition is:

    var1 /\ var2 /\ ... /\ constant-set

  and a consequence:

    destvar contains (var1 /\ var2 /\ ... /\ constant-set)

  /\ is set-intersection. The conditions are a test that the
  result of the intersection is not the empty set, thus the
  two common conditions:

    var /\ { integer }: means var can be an integer
    var1 /\ var2: means var1 can be the same type as var2

  The number of conditions can be 0, the consequence can be absent
  (for branches).

  An example should help:

    a = b + c

  generates:

    b /\ { integer } & c /\ { integer } => a contains { integer }
    b /\ { string } & c /\ { string } => a contains { string }

  (with /\ = set intersection, and an implicit comparison to the
  empty set in each test). This means that if b can be an integer
  and c can be an integer, then after this instruction a can be an
  integer (and the same for 'string'). But, importantly it also
  implies: if before the instriuction b and c could be integers then
  after the instruction, b and c can also be integers (the main
  consequence of this apparent tautology is that if before the +
  b could be an integer or a string, and c just a string, then
  afterwards b can only be a string).

  The semantics of the set of constraints for an instruction is thus
  the following:

    let f be a function which uses variables v1, ..., vn,
    containing instruction i with constraints c1, ..., ck.

    let type_before(i, v) represent the possible type for v
    before instruction i, and type_after(i, v) the possible
    type afterwards.

    the contraints specify the relation between type_before and
    type_after, as follows:

      a) forall v not mentioned in c1, ..., ck .
           type_after(i, v) = type_before(i, v)

      b) for each constraint ci = 'cond1 & ... & condj => v contains cond'
         the following equations hold:

	   A(cond1) and ... and A(condj) ==> v contains B(cond)
	   for each condition l which refers to variables w1, ..., wm
	   and each of these variables w:
	     A(cond1) and ... and A(condj) ==> w contains B(condl)

           for all variables u mentioned in c1, ..., ck but not
  	   mentioned in condition of ci:
  	     A(cond1) and ... and A(condj) ==> u contains u
	   (ie constraints need not constrain all variables)

	   where A(cond) is B(cond) != empty-set
	   and B(x1 /\ ... /\ xp /\ constant) is
	     type_before(i, x1) /\ ... /\ type_before(i, xp) /\ constant

	 (ommited consequences and constants behave naturally)

      c) type_after(i, v) contains only those elements implied by the
         equations in b, thus the definition of type_after(i, v) is
	 really:

	   type_after(i, v) =
	     union{cond = {condition} ==> v contains S and
	           condition is satisified} S

    explanation:
      a) means that there are no hidden effects on the types of
         variables not mentioned in the constraints
      b) summarises the consequence on the types of the variables
         present in the instruction
      c) means that all possible types of the variables are
         covered by the constraints

  Solving constraints
  -------------------

  The constraints are solved by a standard data-flow framework, which
  computes for each basic_block b, type_entry(b, v) and type_exit(b, v),
  the possible types for each variable v at entry and exit to the block.

  Given type_entry(b, v) it is possible to compute type_exit(b, v) by
  iteratively applying the constraints of the instructions in the block:

    type_before(first instruction of b, v) = type_entry(b, v)
    type_before(successor instruction i, v) = type_after(i, v)
    type_exit(b, v) = type_after(last instruction of b, v)

  The type inference is a forward data-flow problem (see the notes below
  for some justifications), with in(b) = type_entry(b), out(b) = type_exit(b)
  (ie the type sets for all variables of the function). The following
  equations must be satisfied:

    in(b) = union{p:predecessor of b} out(p)
    out(b) = result of applying constraints of b to in(b) (see above)
    in(entry) = all variables have type set "any"

  The union above is done per-variable type set, of course. Initialising
  all type sets to the empty set (except for in(entry)) and applying 
  the standard iterative data-flow solution leads to minimal type
  sets satisfying all the equations [PROOF NEEDED...].  


  Generating constraints
  ----------------------

  Each class of instruction will be considered separately.

  First, compute instructions:

    dest = op v1, ..., vn

  Each operation op has constraint templates, expressed in terms
  of its arguments and destinations. These templates are simply
  instantiated with the arguments and destination the actual
  instruction to produce the real constraints.

  Branches: like compute instructions, these have constraint
  templates, though with no consequence. In addition, basic blocks
  that end in a branch may have an additional constraint for the
  true branch, and another for the false branch.

  Traps: like compute instructions, again with no consequence.

  Memory: these are added after the optimisation phase, so can
  be ignored.

  Closure: in the absence of inter-procedural optimisation these
  just generate the constraint

    => dest contains { function }

  (Optimisation of calls to known functions, ie those within the
  same module which cannot change, is best handled by a separate
  algorithm)

  Return: no constraints.

  Call: function calls can be separated into 3 categories:

    a) those about which nothing is known (eg calls to functions passed
    as parameters, or to functions stored in global variables)

    b) calls to primitives, except those belonging to category c.

    c) calls to primitives that are known to cause no global side
    effects (most primitives except those like 'lforeach' which
    call a function passed as parameter, but also includes those
    that modify the 'actor' variable for instance ...)

  For a call instruction

    i: dest = call f, v1, ..., vn

  the constraints depend on the category of function f:

    if f belongs to categories a or b:
      forall v in ambvars(i) - { dest } .
        => v contains { "any" }

  This reflects the fact that all ambiguous variables may be assigned
  when an unknown function is called.

    if f belongs to categories b or c:
      f has some constraint templates which are instantiated as usual.

    if f belongs to category a:
      => dest contains { "any" }


  A final note about the instantiation of constants in constraint
  templates: they are simply replaced by '{ the-constants-type }',
  and all constants in the constraint are merged.


  Some notes
  ----------

  The system does purely forward type inference. Moving type checks
  backward in the code is tricky as possible globally visible
  side effects must be considered (the whole system does not stop
  at the first type error ...). This is similar to problems with
  exceptions.

  Consequences: type checks cannot be moved out of loops if they
  are not valid at the first iteration. There are however two
  possible ways to reduce these problems:

  a) the programmer can annotate function definitions with type
  information (which is good for documentation anyway), this
  reduces the number of loops were that information is missing
  b) the first iteration of a loop could be unrolled (not done)

  The framework does not consider the use of the same variable
  as multiple arguments (eg a[i] = i). Consider. (Correct solution
  appears to be that typeset for var is *intersection* of the 
  consequences that concern it from a given constraint, and *union*
  between those from different constraints - cf semantics of constraints.
  Hmm, can lead to variables with no type after an operation ... 
  Probably constraint conditions should be merged - is the obvious method
  correct?)

*/


/* Implementation notes.

   Type sets are represented by integers, this makes all the set manipulations
    simple and efficient.

   The itype_xxx constants represent the masks for the various types
   (itype_any being the "full" set, itype_none the empty set).

   The type_before/after/etc relations are represented by vectors indexed
   by the variable number, as produced by recompute_vars. Only type_entry/exit
   are explicitly kept (with the basic blocks, along with the rest of the
   data-flow information).

   constraint templates are represented in a form designed to make their
   entry easy. This form is different from that of the instantiated constraints,
   which is designed to make evaluation efficient.

   The type representation for constraints is as follows:

     block_constraints = list of instruction_constraints

     instruction_constraints =
       sequence(instruction,
		list of integer, // the variables concerned by the constraint
		list of constraint)

     constraint =
       sequence(list of condition,
		integer,	// consequence variable (false if absent)
		condition)	// consequence condition

     condition = pair(itypeset,
		      list of integer) // variables of condition

     itypeset = integer		// set built from the itype_xxx values

   variables are always identified by their index(number)

   See runtime.h for a description of the constraint template representation.
*/

library inference // type inference
requires system, misc, sequences, graph, dlist,
  compiler, vars, flow, optimise, ins3
defines mc:infer_types, mc:show_type_info, mc:constant?, mc:itypemap,
  itype_none, itype_function, itype_integer, itype_string, itype_vector,
  itype_null, itype_symbol, itype_table, itype_pair, itype_other, itype_any
reads mc:verbose
writes tnargs, tncstargs, tnfull, tnpartial
[
  | op_types, branch_types, typesets, make_condition0, make_condition1,
    make_condition2, instantiate_constraint, build_iconstraint, new_typesets,
    generate_constraints, evaluate_condition, apply_iconstraint, typeset_eq?,
    typeset_union!, extract_types, show_typesets, showset, show_constraints,
    show_constraint, show_c, show_condition, generate_branch_constraints |

  itype_none = 0;		// no type

  itype_function = 1;
  itype_integer = 2;
  itype_string = 4;
  itype_vector = 8;
  itype_null = 16;
  itype_symbol = 32;
  itype_table = 64;
  itype_pair = 128;
  itype_other = 256;

  itype_any = 511;		// "any" type

  op_types = // indexed by mc:b_xxx
    '[("xx.n") // or
      ("xx.n") // and
      ()
      ()
      ("xx.n") // ==
      ("xx.n") // !=
      ("nn.n") // <
      ("nn.n") // <=
      ("nn.n") // >
      ("nn.n") // >=
      ("nn.n") // |
      ("nn.n") // ^
      ("nn.n") // &
      ("nn.n") // <<
      ("nn.n") // >>
      ("nn.n" "ss.s") // +
      ("nn.n") // -
      ("nn.n") // *
      ("nn.n") // /
      ("nn.n") // %
      ("n.n") // -
      ("x.n") // not
      ("n.n") // ~
      ()
      ()
      ()
      ()
      ("vn.x" "sn.x" "ts.x" "os.x" "ns.x") // ref
      ()
      ("xx.k") // .
      ("x.1") // =
      ("k.x") // car
      ("k.x") // cdr
      ("s.n") // string_length
      ("v.n") // vector_length
      ("nn.n") // integer addition
    ];

  branch_types = // indexed by mc:branch_xxx
    '[() // never
      () // always
      () // true
      () // false
      () // or
      () // nor
      () // and
      () // nand
      () // ==
      () // !=
      ("nn") // <
      ("nn") // >=
      ("nn") // <=
      ("nn") // >
     ];

  typesets = make_vector(128); // index from character to typeset
  vector_fill!(typesets, itype_any); // play safe...
  typesets[?f] = itype_function;
  typesets[?n] = itype_integer;
  typesets[?s] = itype_string;
  typesets[?v] = itype_vector;
  typesets[?l] = itype_pair | itype_null;
  typesets[?k] = itype_pair;
  typesets[?t] = itype_table;
  typesets[?y] = itype_symbol;
  typesets[?x] = itype_any;
  typesets[?o] = itype_other;
  typesets[?S] = itype_string | itype_integer;
  protect(typesets);

  mc:itypemap = sequence // map from type_xxx/stype_xxx -> itype typesets
    (itype_other,	// type_code
     itype_function,	// type_closure
     itype_other,	// type_variable
     itype_other,	// type_internal
     itype_function,	// type_primitive
     itype_function,	// type_varargs
     itype_function,	// type_secure
     itype_integer,	// type_integer
     itype_string,	// type_string
     itype_vector,	// type_vector
     itype_pair,	// type_pair
     itype_symbol,	// type_symbol
     itype_table,	// type_table
     itype_other,	// type_private
     itype_other,	// type_object
     itype_other,	// type_character
     itype_other,	// type_gone
     itype_other,	// type_outputport
     itype_other,	// type_mcode
     itype_other,	// type_float
     itype_other,	// type_bigint
     itype_null,	// type_null
     itype_none,	// stype_none
     itype_any,		// stype_any
     itype_function,	// stype_function
     itype_pair | itype_null);	// stype_list
  
  // traps are handled explicitly (only trap_type is of interest and
  // it is special)

  mc:constant? = fn (v)
    // Types: v: var
    // Returns: false if v is not a constant
    //   an appropriate itype_xxx otherwise
    [
      | vclass |

      vclass = v[mc:v_class];
      if (vclass == mc:v_constant)
	mc:itypemap[typeof(v[mc:v_kvalue])]
      else if (vclass == mc:v_global_constant) 
	mc:itypemap[typeof(global_value(v[mc:v_goffset]))]
      else
	false
    ];

  make_condition0 = fn (constant) // makes "constant" condition
    constant . null;

  make_condition1 = fn (constant, v) // makes condition v /\ constant
    [
      | type |

      if (type = mc:constant?(v)) constant & type . null
      else constant . v[mc:v_number] . null
    ];

  make_condition2 = fn (constant, v1, v2) // makes condition v1 /\ v2 /\ constant
    [
      | type, vars |

      if (type = mc:constant?(v1))
	constant = constant & type
      else vars = v1[mc:v_number] . vars;

      if (type = mc:constant?(v2))
	constant = constant & type
      else vars = v2[mc:v_number] . vars;

      constant . vars
    ];

  instantiate_constraint = fn (template, args, dest)
    // Types: template: type signature (string)
    //        args: list of var
    //	      dest: var (or false)
    // Requires: llength(args) = #arguments in template
    // Returns: the constraint produced by instantiating template with
    //   args and dest (if not false)
    // TBD: Prune constraints which contain a condition with itype_none.
    [
      | dvar, consequence, conditions, nargs, type, sargs, i |

      // Build conditions of constraint
      nargs = llength(args);
      i = 0;
      sargs = args;
      while (i < nargs)
	[
	  | arg, tsets |

	  arg = car(sargs);
	  type = template[i];

	  if (type >= ?1 && type <= ?9)
	    [
	      | ref, nref, cond |

	      ref = nth(type - ?0, args);
	      nref = ref[mc:v_number];

	      // if ref is already in some condition, just add arg there
	      if (cond = lexists?(fn (c) memq(nref, cdr(c)), conditions))
		set_cdr!(cond, ref . cdr(cond))
	      else
		conditions = make_condition2(itype_any, ref, arg) . conditions
	    ]
	  else //if ((tsets = typesets[type]) != itype_any)
	    conditions = make_condition1(typesets[type], arg) . conditions;

	  i = i + 1;
	  sargs = cdr(sargs);
	];

      // Build consequence
      if (dest)
	[
	  | l |

	  dvar = dest[mc:v_number];

	  l = string_length(template);
	  if ((type = template[l - 1]) == ?.)
	    // destination is undefined, ie type_none
	    consequence = make_condition0(itype_none)
	  else if (type >= ?1 && type <= ?9)
	    [
	      | ref, nref, cond |

	      ref = nth(type - ?0, args);
	      nref = ref[mc:v_number];

	      // if ref is already in some condition, use same condition
	      if (cond = lexists?(fn (c) memq(nref, cdr(c)), conditions))
		consequence = cond
	      else
		consequence = make_condition1(itype_any, ref);
	    ]
	  else 
	    consequence = make_condition0(typesets[type]);
	]
      else
        dvar = false;

      // Finally assemble constraint
      sequence(conditions, dvar, consequence)
    ];

  build_iconstraint = fn (il, cl)
    // Returns: A constraints list for instruction il, given its
    //   constraint list (extracts all vars referred to)
    [
      | vars, addvar, scl |

      addvar = fn (v) if (!memq(v, vars)) vars = v . vars;

      scl = cl;
      while (scl != null)
	[
	  | ovars, c |

	  c = car(scl);
	  ovars = vars;
	  lforeach(fn (cond) lforeach(addvar, cdr(cond)), c[0]);
	  if (c[1])
	    [
	      addvar(c[1]);	// add dest
	      // but not its condition (cf semantics)
	    ];
	  
	  /* Semantics: unused variables in conditions are unaffected.
	     Instead of coding this implicitly, add `u /\ any' conditions
	     for  such variables.
	     The variables between vars & ovars were not present in 
	     constraints prior to c. Add the pseudo-conditions to them. */
/*
	  if (scl != cl && ovars != vars)
	    [
	      | searly, early, add, svars |

	      svars = vars;
	      while (svars != ovars)
		[
		  add = itype_any . car(svars) . null;
		  svars = cdr(svars);
		];
	      searly = cl;
	      while (searly != scl)
		[
		  early = car(searly);
		  early[0] = lappend(add, early[0]);
		  searly = cdr(searly);
		];
	    ];
*/
	  scl = cdr(scl);
	];
      
      sequence(il, vars, cl)
    ];
  
  generate_constraints = fn (il, ambiguous, constraints)
    // Types: il: instruction
    // Returns: (constraints for instruction il) . constraints
    [
      | ins, class, new, args, dest, op |

      ins = il[mc:il_ins];
      class = ins[mc:i_class];
      if (class == mc:i_compute)
	[
	  args = ins[mc:i_aargs];
	  dest = ins[mc:i_adest];
	  new = lmap(fn (sig) instantiate_constraint(sig, args, dest),
		     op_types[ins[mc:i_aop]]);
	]
      else if (class == mc:i_branch)
	[
	  args = ins[mc:i_bargs];
	  op = ins[mc:i_bop];
	  if (op < vector_length(branch_types))
	    new = lmap(fn (sig) instantiate_constraint(sig, args, false),
		       branch_types[op]);
	]
      else if (class == mc:i_call)
	[
	  | escapes, f, prim, ndest |

	  dest = ins[mc:i_cdest];
	  ndest = dest[mc:v_number];
	  args = ins[mc:i_cargs];
	  f = car(args); args = cdr(args);
	  escapes = true;

	  // Call to known function ?
	  if (f[mc:v_class] == mc:v_global_constant &&
	      primitive?(prim = global_value(f[mc:v_goffset])) &&
	      primitive_nargs(prim) == llength(args))
	    [
	      | types |

	      if ((types = primitive_type(prim)) != null)
		new = lmap(fn (sig) instantiate_constraint(sig, args, dest), types);
	      if (primitive_flags(prim) & OP_NOESCAPE) escapes = FALSE;
	    ]
	  else
	    [
	      // destination is any
	      new = sequence(null, ndest, make_condition0(itype_any)) . null;
	    ];

	  if (escapes) // note global side effects
	    bforeach
	      (fn (i) if (i != ndest)
	         new = sequence(null, i, make_condition0(itype_any)) . new,
	       ambiguous);
	]
      else if (class == mc:i_trap)
	[
	  if (ins[mc:i_top] == mc:trap_type)
	    [
	      args = ins[mc:i_targs];
	      dest = car(args)[mc:v_number];
	      new = sequence (null, dest,
			      make_condition0(mc:itypemap[mc:var_value(cadr(args))]))
	        . null;
	    ]
	]
      else if (class == mc:i_closure)
	[
	  dest = ins[mc:i_fdest][mc:v_number];
	  new = sequence(null, dest, make_condition0(itype_function)) . null;
	];

      if (new != null) build_iconstraint(il, new) . constraints
      else constraints
    ];

  generate_branch_constraints = fn (block)
    // Types: block: cfg block
    // Returns: a pair of constraints for blocks that end in "interesting"
    //   branches, false otherwise
    //   The first element of the pair is applied when the branch is taken,
    //   the 2nd when it isn't.
    [
      | lastins, lastil, op, type, reversed, ctrue, cfalse, var |

      lastil = dget(dprev(block[mc:f_ilist]));
      lastins = lastil[mc:il_ins];
      // type branches are interesting, so is == and != null.
      if (lastins[mc:i_class] != mc:i_branch)
	exit<function> false;

      op = lastins[mc:i_bop];
      if (op >= mc:branch_type?)
	[
	  var = car(lastins[mc:i_bargs]);
	  if (op >= mc:branch_ntype?)
	    [
	      type = op - mc:branch_ntype?;
	      reversed = true;
	    ]
	  else
	    [
	      type = op - mc:branch_type?;
	      reversed = false;
	    ]
	]
      else if ((op == mc:branch_eq || op == mc:branch_ne) &&
	       lfind?(fn (v) mc:constant?(v) == itype_null,
		      lastins[mc:i_bargs])) // comparison to null
	[
	  type = type_null;
	  // constant folding prevents null == null
	  var = lfind?(fn (v) mc:constant?(v) != itype_null,
		       lastins[mc:i_bargs]);
	  reversed = op == mc:branch_ne;
	]
      else
	exit<function> false; // not interesting

      type = mc:itypemap[type];
      ctrue = sequence(make_condition1(type, var) . null, false, null);
      ctrue = build_iconstraint(lastil, ctrue . null);
      cfalse = sequence(make_condition1(itype_any & ~type, var) . null,
			false, null);
      cfalse = build_iconstraint(lastil, cfalse . null);

      if (reversed) cfalse . ctrue
      else ctrue . cfalse
    ];

  evaluate_condition = fn (condition, typeset)
    // Types: condition: condition
    //        typeset: vector of typesets
    // Returns: Result of condition given types in typeset
    [
      | x |

      x = car(condition);
      condition = cdr(condition);
      while (condition != null)
	[
	  x = x & typeset[car(condition)];
	  condition = cdr(condition);
	];
      x
    ];

  apply_iconstraint = fn (iconstraint, typeset)
    // Types: iconstraint: instruction_constraint
    //        typeset: vector of itypeset
    // Returns: The typeset resulting from the application of constraint
    //   to typeset
    [
      | new, apply_constraint |

      // clear modified vars
      new = vcopy(typeset);
      lforeach(fn (v) new[v] = itype_none, iconstraint[1]);
      
      apply_constraint = fn (c)
	[
	  | results, conditions |
	  
	  //display(format("applying %s\n", c));
	  conditions = c[0];
	  while (conditions != null)
	    [
	      | x |
	      
	      x = evaluate_condition(car(conditions), typeset);
	      if (x == itype_none) exit<function> 0; // constraint failed
	      results = x . results;
	      conditions = cdr(conditions);
	    ];
	  //display(format("success %s\n", results));
	  
	  // condition successful, modify new typesets
	  // first, destination:
	  if (c[1])
	    new[c[1]] = new[c[1]] | evaluate_condition(c[2], typeset);
	  
	  // then all concerned variables
	  conditions = lreverse(c[0]); // same order as results
	  while (conditions != null)
	    [
	      | x |

	      x = car(results);
	      lforeach(fn (arg) new[arg] = new[arg] | x, cdar(conditions));
	      conditions = cdr(conditions);
	      results = cdr(results);
	    ];
	];

      lforeach(apply_constraint, iconstraint[2]);
      new
    ];

  new_typesets = fn (ifn)
    // Returns: A new sequence of typesets initialised to itype_none
    [
      | v |

      vector_fill!(v = make_vector(ifn[mc:c_fnvars]), itype_none);
      v
    ];

  typeset_eq? = fn (ts1, ts2)
    // Returns: True if all the typesets in ts1 are equal to those in ts2
    [
      | l |

      l = vector_length(ts1);
      while ((l = l - 1) >= 0)
	if (ts1[l] != ts2[l]) exit<function> false;
      
      true
    ];

  typeset_union! = fn (ts1, ts2)
    // Effects: ts1 = ts1 U ts2 (per variable)
    // Modifies: ts1
    [
      | l |

      l = vector_length(ts1);
      while ((l = l - 1) >= 0) ts1[l] = ts1[l] | ts2[l];
    ];

  extract_types = fn (ifn)
    // Types: ifn: intermediate function
    // Modifies: ifn
    // Effects: Sets the type fields of ifn's instructions
    [
      | fg, nargs, ncstargs, npartial, nfull, compute_types |

      fg = ifn[mc:c_fvalue];
      nargs = ncstargs = npartial = nfull = 0;

      compute_types = fn (il, types)
	[
	  | ins, class, vtype, qvtype, iconstraint, typeset |

	  ins = il[mc:il_ins];
	  //mc:print_ins(ins, null);
	  //display("  types:"); show_typesets(car(types));
	  //newline();
	  class = ins[mc:i_class];
	  typeset = car(types);

	  vtype = fn (v)
	    [
	      | type |

	      nargs = nargs + 1;
	      if (type = mc:constant?(v))
		[
		  ncstargs = ncstargs + 1;
		  type
		]
	      else
		[
		  type = typeset[v[mc:v_number]];
		  assert(v[mc:v_number] != 0);
		  if (memq(type, '(1 2 4 8 16 32 64 128 256)))
		    nfull = nfull + 1
		  else if (type != itype_any)
		    npartial = npartial + 1;

		  type
		]
	    ];

	  qvtype = fn (v)
	    [
	      | type |

	      if (type = mc:constant?(v)) type
	      else typeset[v[mc:v_number]]
	    ];

	  if (class == mc:i_compute)
	    [
	      if (ins[mc:i_aop] != mc:b_assign)
		ins[mc:i_atypes] = lmap(vtype, ins[mc:i_aargs])
	    ]
	  else if (class == mc:i_branch)
	    [
	      if (ins[mc:i_bop] >= mc:branch_lt)
		ins[mc:i_btypes] = lmap(vtype, ins[mc:i_bargs]);
	    ]
	  else if (class == mc:i_trap && ins[mc:i_top] == mc:trap_type)
	    ins[mc:i_ttypes] = lmap(vtype, ins[mc:i_targs]);

	  if (cdr(types) != null && (iconstraint = cadr(types))[0] == il)
	    [
	      // this instruction has a constraint
	      //display("applying "); show_constraint(iconstraint);
	      //newline();
	      apply_iconstraint(iconstraint, typeset) . cddr(types)
	    ]
	  else
	    types
	];

      graph_nodes_apply
        (fn (n)
	 [
	   | block, types |

	   block = graph_node_get(n);
	   types = block[mc:f_types];
	   //mc:ins_list1(block[mc:f_ilist]);
	   //mc:show_type_info(types);
	   dreduce(compute_types, types[mc:flow_in] . types[mc:flow_gen],
		   block[mc:f_ilist]);
	 ], cdr(fg));

      if (mc:verbose >= 3)
	[
	  display("Type inference results:"); newline();
	  display(format("%s args, of which %s constant, %s fully inferred, %s partially.", nargs, ncstargs, nfull, npartial));
	  newline();
	];
      tnargs = tnargs + nargs;
      tncstargs = tncstargs + ncstargs;
      tnfull = tnfull + nfull;
      tnpartial = tnpartial + npartial;
    ];

  mc:infer_types = fn (ifn)
    // Types: ifn: intermediate function
    // Modifies: ifn
    // Effects: infers types for the variables of ifn
    [
      | fg, entry, nvars, change, globals, icount, merge_block |

      if (mc:verbose >= 3)
	[
	  display(format("Inferring %s", mc:fname(ifn)));
	  newline();
	];
      mc:recompute_vars(ifn, true);
      mc:flow_ambiguous(ifn, mc:closure_write);

      fg = ifn[mc:c_fvalue];
      nvars = ifn[mc:c_fnvars];
      // Defined globals do not change across functin calls
      globals = mc:set_vars!(mc:new_varset(ifn), ifn[mc:c_fclosure]);
      mc:set_vars!(globals, lfilter(fn (v) v[mc:v_class] != mc:v_global_define,
				    ifn[mc:c_fglobals]));

      graph_nodes_apply
	(fn (n)
	 [
	   | block |

	   block = graph_node_get(n);
	   block[mc:f_types] = vector
	     (lreverse!(mc:scan_ambiguous(generate_constraints, null,
					  block, globals, mc:closure_write)),
	      // use kill slot for per-edge constraint
	      generate_branch_constraints(block),
	      new_typesets(ifn),
	      new_typesets(ifn)); // no map
	 ], cdr(fg));

      // solve data-flow problem

      // init entry node:
      entry = graph_node_get(car(fg));
      lforeach(fn (arg) entry[mc:f_types][mc:flow_in][arg[mc:v_number]] = itype_any,
	       ifn[mc:c_fargs]);
      lforeach(fn (arg) entry[mc:f_types][mc:flow_in][arg[mc:v_number]] = itype_any,
	       ifn[mc:c_fglobals]);
      lforeach(fn (arg) entry[mc:f_types][mc:flow_in][arg[mc:v_number]] = itype_any,
	       ifn[mc:c_fclosure]);

      // iterate till solution found

      merge_block = fn (n)
	[
	  | node, types, new_in, new_out |

	  node = graph_node_get(n);
	  types = node[mc:f_types];

	  // compute in as 'union' of out's of predecessors
	  new_in = types[mc:flow_in];
	  graph_edges_in_apply
	    (fn (predecessor)
	     [
	       | pnode, ptypes, branch_constraints, flow_out |

	       pnode = graph_node_get(graph_edge_from(predecessor));
	       ptypes = pnode[mc:f_types];
	       flow_out = ptypes[mc:flow_out];
	       branch_constraints = ptypes[mc:flow_kill]; // slot reuse
	       if (branch_constraints)
		 flow_out = apply_iconstraint
		   (if (graph_edge_get(predecessor))
		      // fallthrough, ie false edge
		      cdr(branch_constraints)
		    else // branch, ie true edge
		      car(branch_constraints),
		    flow_out);
	       typeset_union!(new_in, flow_out);
	     ], n);
	  types[mc:flow_in] = new_in;

	  // compute new out
	  //display("APPLY"); newline();
	  //show_constraints(types[mc:flow_gen]);
	  //display("TO "); show_typesets(new_in); newline();
	  if (types[mc:flow_gen] == null) new_out = vcopy(new_in)
	  else new_out = lreduce(apply_iconstraint, new_in, types[mc:flow_gen]);
	  //display("-> "); show_typesets(new_out); newline();
	  assert(new_out != types[mc:flow_out]);
	  if (!typeset_eq?(new_out, types[mc:flow_out]))
	    [
	      types[mc:flow_out] = new_out;
	      change = true
	    ]
	];

      icount = 0;
      loop
	[
	  change = false;
	  //display(format("*ITERATION %s*", icount + 1)); newline();
	  graph_nodes_apply(merge_block, cdr(fg));
	  icount = icount + 1;
	  if (!change) exit 0;
	];
      if (mc:verbose >= 3)
	[
	  display(format("Type inference iterations %s", icount));
	  newline();
	];

      extract_types(ifn);

      mc:clear_dataflow(ifn);
    ];

  mc:show_type_info = fn (types)
    if (types)
      [
	display("Types:\n");
	show_constraints(types[mc:flow_gen]);
	display("in:"); show_typesets(types[mc:flow_in]); newline();
	display("out:"); show_typesets(types[mc:flow_out]); newline();
      ];

  show_typesets = fn (typeset)
    for(1, vector_length(typeset) - 1,
	fn (v) display(format(" %s(%s)", v, showset(typeset[v]))));

  showset = fn (tset)
    if (tset == itype_none) "none"
    else if (tset == itype_any) "any"
    else
      [
	| s |

	s = "";

	if (tset & itype_function) s = s + "f";
	if (tset & itype_integer) s = s + "n";
	if (tset & itype_string) s = s + "s";
	if (tset & itype_vector) s = s + "v";
	if (tset & itype_null) s = s + "0";
	if (tset & itype_symbol) s = s + "y";
	if (tset & itype_table) s = s + "t";
	if (tset & itype_pair) s = s + "k";
	if (tset & itype_other) s = s + "o";

	s
      ];

  show_constraints = fn (constraints)
    [
      | i |

      i = 0;
      while (constraints != null)
	[
	  display(format("constraint %s\n", i));
	  show_constraint(car(constraints));
	  i = i + 1;
	  constraints = cdr(constraints);
	];
    ];

  show_constraint = fn (constraint)
    [
      display(format("  vars: %s", concat_words(lmap(itoa, constraint[1]), " ")));
      newline();
      lforeach(show_c, constraint[2]);
    ];

  show_c = fn (c)
    [
      display(format("  %s", concat_words(lmap(show_condition, c[0]), " & ")));
      if (c[1])
	display(format(" => %s contains %s", c[1], show_condition(c[2])));
      newline();
    ];

  show_condition = fn (cond)
    [
      | s |

      s = showset(car(cond));
      lforeach(fn (v) s = s + format(" /\\ %s", v), cdr(cond));
      s
    ];
];
