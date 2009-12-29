/* 
 * Copyright (c) 1993-2006 David Gay
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
requires system, vars, ins3
defines mc:infer_types, mc:show_type_info, mc:constant?
[
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
];
