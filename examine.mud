// Display sizes of global variables (after specified offset)

gsize = fn (from)
  foreach(fn (gsym)
	  [
	    | n |
	    n = symbol_get(gsym);
	    if (n >= from)
	      [
		| size |
		size = obj_size(global_value(n)) - 4;

		display(format("%s: %s\n", symbol_name(gsym), size))
	      ]			     
	  ], lqsort(fn (s1, s2) string_cmp(symbol_name(s1), symbol_name(s2)) <= 0, table_list(global_table())));

gfind = fn (n)
  lexists?(fn (gs) symbol_get(gs) == n, table_list(global_table()));
