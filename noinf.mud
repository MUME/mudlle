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
    (itype_other,
     itype_function,
     itype_other,
     itype_other,
     itype_function,
     itype_function,
     itype_function,
     itype_integer,
     itype_string,
     itype_vector,
     itype_pair,
     itype_symbol,
     itype_table,
     itype_other,
     itype_other,
     itype_other,
     itype_other,
     itype_other,
     itype_other,
     itype_null,
     itype_none,
     itype_any,
     itype_function,
     itype_pair | itype_null);
  
];
