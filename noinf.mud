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

library inference // type inference, disabled version
requires system, vars, misc
defines mc:infer_types, mc:show_type_info, mc:constant?, mc:itypemap,
  itype_none, itype_function, itype_integer, itype_string, itype_vector,
  itype_null, itype_symbol, itype_table, itype_pair, itype_other, itype_any
[
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

  mc:show_type_info = mc:infer_types = fn (x) 0;

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
  
];
