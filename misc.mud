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

library misc // Miscellaneous useful functions
requires system, sequences
defines abbrev?, assert, assert_message, assoc, assq, bcomplement,
  bcomplement!, bforeach, bitset_to_list, breduce, caaar, caadr, caar, cadar,
  caddr, cadr, cdaar, cdadr, cdar, cddar, cdddr, cddr,
  concat_words, difference, equal?, fail, fail_message, find_word?,
  intersection, last_element, last_pair, list_first_n!, list_index,
  list_to_vector, lprotect, lqsort, mappend, mapstr, member, memq, nth,
  nth_element, nth_element_cond, nth_pair, random_element, repeat, rprotect,
  set_eq?, set_in?, skip_white, sorted_table_list, sorted_table_vector,
  string_head?, string_index, string_ljustify, string_ljustify_cut,
  string_rindex, string_rjustify, string_rjustify_cut, string_tail, table_copy,
  union, unquote, vector_exists_index, vector_index, vector_rindex,
  vector_to_list, vequal?, vqsort!, vqsort_slice!, call_cc

reads random // hack to avoid problems w/ standalone version
[

repeat = fn "`n `f -> . Execute `f() `n times" (int n, function f)
  while (--n >= 0) f();

[
  | recurse |
  
  recurse = fn (x1, stack1, x2, stack2)
    if (null?(x1) || null?(x2) || integer?(x1) || integer?(x2))
      x1 == x2
    else if (string?(x1))
      string?(x2) && string_cmp(x1, x2) == 0
    else if (float?(x1))
      if (float?(x2))
	if (fnan?(x1) || fnan?(x2))
	  x1 == x2
	else
	  fcmp(x1, x2) == 0
      else
	false
    else if (bigint?(x1))
      bigint?(x2) && bicmp(x1, x2) == 0
    else if (x1 == x2)
      true
    else
      [
	| i, s1 |

	// If we find x1 in stack1, check that x2 is at the same position
	// in stack2. stack1 and stack2 are guaranteed to be of the same
	// length.

	i = 0;
	s1 = stack1;
	loop
	  [
	    if (s1 == null)
	      exit null;
	    if (car(s1) == x1)
	      [
		| s2 |
		s2 = stack2;
		while (i--)
		  s2 = cdr(s2);
		exit<function> car(s2) == x2;
	      ];
	    s1 = cdr(s1);
	    ++i;
	  ];
	stack1 = x1 . stack1;
	stack2 = x2 . stack2;

	if (pair?(x1))
	  (pair?(x2)
	   && recurse(car(x1), stack1, car(x2), stack2)
	   && recurse(cdr(x1), stack1, cdr(x2), stack2))
	else if (vector?(x1))
	  vector?(x2) &&
	    [
	      | l |
	      l = vector_length(x1);
	      if (l != vector_length(x2)) false
	      else
		[
		  while ((l = l - 1) >= 0)
		    if (!recurse(x1[l], stack1, x2[l], stack2))
		      exit<function> false;
		  true
		]
	    ]
	else if (symbol?(x1))
	  (symbol?(x2)
	   && string_icmp(symbol_name(x1), symbol_name(x2)) == 0
	   && recurse(symbol_get(x1), stack1, symbol_get(x2), stack2))
	else if (table?(x1))
	  table?(x2) &&
	    [
	      | y |
	      y = table_copy(x2);
	      
	      if (table_exists?(fn (sym) [
		| sname |
		if (recurse(symbol_get(sym), stack1,
			    table_ref(y, sname = symbol_name(sym)), stack2))
		  [
		    table_set!(y, sname, null);
		    false
		  ]
		else
		  true
	      ], x1))
		false
	      else if (table_exists?(fn (sym) true, y))
		false
	      else
		true
	    ]
	else
	  false;
      ];

  equal? = fn "`x1 `x2 -> `b. Compares `x1 to `x2 for equality, recursively for container objects" (x1, x2)
    recurse(x1, null, x2, null);
];

rprotect = fn "`x -> `x. Protect `x recursively" (x)
  [
    | pro, seen |

    pro = fn (x)
      if (!immutable?(x) && !memq(x, seen))
	[
	  protect(x);

	  if (pair?(x))
	    [
	      seen = x . seen;
	      pro(car(x));
	      pro(cdr(x));
	    ]
	  else if (vector?(x))
	    [
	      | l |
	      
	      l = vector_length(x);
	      seen = x .  seen;
	      while ((l = l - 1) >= 0) 
		pro(x[l]);
	    ]
	  else if (symbol?(x))
	    [
	      seen = x . seen;
	      pro(symbol_name(x));
	      pro(symbol_get(x));
	    ]
	  else if (table?(x))
	    [
	      seen = x . seen;
	      table_foreach(pro, x)
	    ]
	];

    // Start new session, with no execution limits
    session(fn ()
	    [
	      unlimited_execution();
	      pro(x);
	    ]);
    x
  ];

lprotect = fn "`l -> `l. Protects list `l" (l)
  [
    | t |

    t = l;
    while (l != null)
      [
	protect(l);
	l = cdr(l);
      ];
    t
  ];

caar = fn "`x0 -> `x1. Returns car(car(`x0))" (pair x) car(car(x));
cadr = fn "`x0 -> `x1. Returns car(cdr(`x0))" (pair x) car(cdr(x));
cddr = fn "`x0 -> `x1. Returns cdr(cdr(`x0))" (pair x) cdr(cdr(x));
cdar = fn "`x0 -> `x1. Returns cdr(car(`x0))" (pair x) cdr(car(x));
caaar = fn "`x0 -> `x1. Returns car(car(car(`x0)))" (pair x) car(car(car(x)));
caadr = fn "`x0 -> `x1. Returns car(car(cdr(`x0)))" (pair x) car(car(cdr(x)));
cadar = fn "`x0 -> `x1. Returns car(cdr(car(`x0)))" (pair x) car(cdr(car(x)));
caddr = fn "`x0 -> `x1. Returns car(cdr(cdr(`x0)))" (pair x) car(cdr(cdr(x)));
cdaar = fn "`x0 -> `x1. Returns cdr(car(car(`x0)))" (pair x) cdr(car(car(x)));
cdadr = fn "`x0 -> `x1. Returns cdr(car(cdr(`x0)))" (pair x) cdr(car(cdr(x)));
cddar = fn "`x0 -> `x1. Returns cdr(cdr(car(`x0)))" (pair x) cdr(cdr(car(x)));
cdddr = fn "`x0 -> `x1. Returns cdr(cdr(cdr(`x0)))" (pair x) cdr(cdr(cdr(x)));

assert = fn "`b -> . Fail if `b is false" (b) if (!b) fail();
assert_message = fn "`b `s -> . Fail with message `s if `b is false" (b, s) if (!b) fail_message(s);
fail = fn " -> . Fail." () 1/0;
fail_message = fn "`s -> . Fail with message `s." (s) [ display(format("%s%n", s)); 1/0 ];

union = fn "`l1 `l2 -> `l3. Set union of `l1 and `l2" (list l1, list l2)
  // Types: l1, l2: set
  // Returns: The set union (comparison with ==) of l1 and l2.
  [
    | result |

    result = l1;
    while (l2 != null)
      [
	| f2 |
	
	if (!memq(f2 = car(l2), l1)) result = f2 . result;
	l2 = cdr(l2)
      ];
    result
  ];

intersection = fn "`l1 `l2 -> `l3. Set intersection of `l1 and `l2" (list l1, list l2)
  // Types: l1, l2: set
  // Returns: The set intersection (comparison with ==) of l1 and l2.
  [
    | result |

    while (l1 != null)
      [
	| f |
	if (memq(f = car(l1), l2)) result = f . result;
	l1 = cdr(l1)
      ];
    result
  ];
  
difference = fn "`l1 `l2 -> `l3. Set difference of `l1 and `l2" (list l1, list l2)
  // Types: l1, l2: set
  // Returns: The set difference (comparison with ==) of l1 and l2.
  [
    | result |

    while (l1 != null)
      [
	| f |
	if (!memq(f = car(l1), l2)) result = f . result;
	l1 = cdr(l1)
      ];
    result
  ];

set_in? = fn "`l1 `l2 -> `b. True if `l1 is a subset of `l2" (list s1, list s2)
  // Types: s1, s2: set
  // Returns: TRUE if s1 is a subset of s2
  loop
    if (s1 == null) exit true
    else if (!memq(car(s1), s2)) exit false
    else s1 = cdr(s1);

set_eq? = fn "`l1 `l2 -> `b. True if set `l1 == set `l2" (list s1, list s2)
  // Types: s1, s2: set
  // Returns: TRUE if s1 == s2
  set_in?(s1, s2) && set_in?(s2, s1);

vector_index = fn "`x `v -> `n. Finds index of `x in `v, or -1 if none" (x, vector v)
  [
    | check, max |
    check = 0;
    max = vector_length(v);

    loop
      if (check == max) exit -1
      else if (v[check] == x) exit check
      else check = check + 1
  ];

vector_rindex = fn "`x `v -> `n. Finds index of last `x in `v, or -1 if none" (x, vector v)
  [
    | check |

    check = vector_length(v);
    loop
      if (check == 0) exit -1
      else if (v[check = check - 1] == x) exit check
  ];

vector_exists_index = fn "`f `v -> `n. Returns the index of first element `x of `v for which `f(`x) is true, or -1 if none" (function f, vector v)
  [
    | l, i |

    l = vector_length(v);
    i = 0;
    loop
      if (i == l) exit -1
      else if (f(v[i])) exit i
      else i = i + 1
  ];

string_index = fn "`s `n1 -> `n2. Finds index of char `n1 in `s, or -1 if none"
   (string str, int n) [
   |check, max|
   check = 0;
   max = string_length (str) - 1;
   while (if (check <= max) (str[check] != n) else 0)
      check = check + 1;
   if (check <= max) check else -1;
];

string_rindex = fn "`s `n1 -> `n2. Finds last index of char `n1 in `s, or -1 if none"
   (string str, int n) 
  [
    | max |

    max = string_length (str);
    while (max > 0)
      if (str[max = max - 1] == n) exit<function> max;
    -1
  ];


vequal? = fn "`v1 `v2 -> `b. True if the elements of `v1 are == to those of `v2" (vector v1, vector v2)
  [
    | l |
    l = vector_length(v1);
    if (l != vector_length(v2)) false
    else
      [
	while ((l = l - 1) >= 0)
	  if (v1[l] != v2[l]) exit<function> false;
	true
      ]
  ];

lqsort = fn "`f `l1 -> `l2. Sort `l1 according to `f. `f(`x1, `x2) should return true if `x1 goes before `x2" (function f, list l)
  if (l == null) null
  else
    [
      | pivot, x, l1, l2 |

      pivot = car(l);
      loop
	[
	  l = cdr(l);
	  if (l == null)
	    exit lappend(lqsort(f, l1), pivot . lqsort(f, l2));
	  x = car(l);
	  if (f(x, pivot)) l1 = x . l1
	  else l2 = x . l2
	]
    ];

vqsort_slice! = fn "`f `n0 `n1 `v0 -> `v1. Sort `v0 from index `n0, with length `n1, according to `f. `f(`x1, `x2) should return true if `x1 goes before `x2" (function f, int start, int length, vector v)
  [
    | subsort |

    subsort = fn (low, high)
      if (low < high) // work remains
	[
	  | pivot, free, pos_low, pos_high, x |

	  pivot = v[high];
	  free = high;
	  pos_low = low;
	  pos_high = high - 1;
	  <separate> loop // scan up from low
	    [
	      x = v[pos_low];
	      ++pos_low;
	      if (!f(x, pivot)) // x must move above pivot
		[
		  v[free] = x;
		  // where x was is now free
		  free = pos_low - 1;

		  // scan down from high
		  loop
		    [
		      if (pos_low > pos_high) exit<separate> 0;
		      x = v[pos_high];
		      --pos_high;
		      if (f(x, pivot)) // x must move below pivot
			[
			  v[free] = x;
			  // where x was is now free
			  free = pos_high + 1;
			  exit 0
			]
		    ]
		];
	      if (pos_low > pos_high) exit<separate> 0;
	    ];
	  v[free] = pivot;
	  subsort(low, free - 1);
	  subsort(free + 1, high);
	];

    if (start < 0)
      start += vlength(v);

    subsort(start, start + length - 1);
    v
  ];

vqsort! = vector fn "`f `v -> `v. Sort `v according to `f. `f(`x1, `x2) should return true if `x1 goes before `x2" (function f, vector v)
  vqsort_slice!(f, 0, vlength(v), v);

mapstr = fn "`c `s -> `l. Executes `c(`n) on every character `n in `s and makes list of results" (function f, string s)
  [
    | len, results |
    len = string_length(s); results = null;

    while (len > 0)
      results = f(s[len = len - 1]) . results;

    results
  ];

mappend = fn "`f `l1 -> `l2. Like `lmap, but appends the results of `f(`x) for each element `x in `l1 together" (function f, list l)
  [
    | results |

    l = lreverse(l);
    while (l != null)
      [
        results = lappend(f(car(l)), results);
	l = cdr(l);
      ];
    results
  ];

memq = fn "`x `l1 -> `l2. Returns first sublist `l2 of `l1 which is == to `x, or FALSE if none" (x, list l)
  loop
    if (l == null) exit false
    else if (x == car(l)) exit l
    else l = cdr(l);

member = fn "`x `l1 -> `l2. Returns first sublist `l2 of `l1 which is `equal? to `x, or FALSE if none" (x, list l)
  loop
    if (l == null) exit false
    else if (equal?(x, car(l))) exit l
    else l = cdr(l);

assq = fn "`x1 `l -> `x2. Looks for a pair in `l whose car is == to `x1. Returns that pair, false if not found" (x, list l)
  loop
    if (l == null) exit false
    else if (car(car(l)) == x) exit car(l)
    else l = cdr(l);


assoc = fn "`x1 `l -> `x2. Looks for a pair in `l whose car is `equal? to `x1. Returns that pair, false if not found" (x, list l)
  loop
    if (l == null) exit false
    else if (equal?(car(car(l)), x)) exit car(l)
    else l = cdr(l);


nth = fn "`n `l -> `x. Returns `n'th (one-based) element of list `l" (int n, list l)
  [
    while ((n = n - 1) > 0)
      l = cdr(l);
    car(l)
  ];

list_to_vector = fn "`l -> `v. Makes a vector out of a list" 
   (list lst) [
   |vec, i|
   vec = make_vector (llength (lst));
   i = 0;
   while (pair? (lst)) [
      vec [i] = car (lst);
      i = i + 1;
      lst = cdr (lst);
   ];
   vec;
];

vector_to_list = fn "`v -> `l. Makes a vector into a list" (vector v)
  [
    | len, l |

    len = vector_length(v);
    while ((len = len - 1) >= 0) l = v[len] . l;
    l
  ];

sorted_table_vector = fn "`table -> `v. Returns a vector of the elements of `table, sorted by name" (table table)
  vqsort!(fn (s1, s2) string_icmp(symbol_name(s1), symbol_name(s2)) < 0,
	  table_vector(table));

sorted_table_list = fn "`table -> `l. Returns a list of the elements of `table, sorted by name" (table table)
  vector_to_list(sorted_table_vector(table));

table_copy = fn "`table1 -> `table2. Makes a copy of `table1" (table table)
  [
    | res |
    table_reduce(fn (sym, res) [
      table_set!(res, symbol_name(sym), symbol_get(sym));
      res
    ], res = make_table(), table);
    res                         // tell compiler we're returning a table
  ];

/// Ancalagon's stuff

/// LIST FUNCTIONS

nth_pair = fn "`n `l -> `x. Returns nth pair of `l" (int n, list lst) [
   while (pair? (lst) && (n > 1)) [
      lst = cdr (lst);
      n = n - 1;
   ];
   if (n == 1) lst else null;
];

nth_element = fn "`n `l -> `x. Returns element `n of a list or null" (int n, list lst) [
   |nth|
   nth = nth_pair (n, lst);
   if (pair? (nth)) car (nth) else null;
];

random_element = fn 
   "`l -> `x. Returns a random element in list `l or null" (list lst) [
   |items|
   items = llength (lst);
   if (items >= 1) nth_element (random (1, items), lst)
   else null;
];

last_pair = fn "`l0 -> `l1. Returns the last pair of list `l0" (list lst) [
   |tail|
   while (pair? (lst)) [
      tail = lst;
      lst = cdr (lst);
   ];
   tail;
];

last_element = fn "`l -> `x. Returns the last element of list `l or null" (list lst) [
   |res|
   res = last_pair (lst);
   if (pair? (res)) car (res) else null;
];

list_first_n! = fn "`n `l -> `l. Returns first `n elements of `l" 
   (int n, list lst) [
   |nth|
   nth = nth_pair (n, lst);
   if (pair? (nth)) set_cdr! (nth, null);
   lst;
];

nth_element_cond = fn "`n `l `f -> `x|`b. Returns `n'th `x in `l for which `f(`x) true, or false" (int n, list lst, function func)
  [
    while (pair?(lst) && n > 1)
      [
        if (func(car(lst)))
          --n;
        lst = cdr (lst);
      ];
    while (pair?(lst) && !func(car(lst)))
      lst = cdr(lst);
    if (pair?(lst) && n == 1)
      car(lst)
    else 
      false;
  ];

list_index = fn "`x `l -> `n. Returns the index of `x or false" 
   (x, list lst) [
   |count|
   count = 1;
   while (pair? (lst) && (car (lst) != x)) [
      count = count + 1;
      lst = cdr (lst);
   ];
   if (!pair? (lst)) count = 0;
   count;
];

/// STRING FUNCTIONS

string_tail = fn "`s `n -> `s. Returns the tail of `s starting from position `n" (string str, int from)
  if (from < 0)
    substring(str, from, -from)
  else
    substring(str, from, string_length (str) - from);

string_head? = fn "`s1 `s2 `n -> `b. True if `s1 = first of `s2, min `n characters" (string head, string whole, int n)
  [
    | hlen |

    hlen = string_length(head);

    if (hlen < n || hlen > slength(whole))
      exit<function> false;

    for (|i|i = 0; i < hlen; ++i)
      if (cicmp(head[i], whole[i]) != 0)
        exit<function> false;
    true;
];

abbrev? = fn "`s1 `s2 -> `b. Returns true if `s1 is an abbreviation of `s2" (string a, string b) 
  string_head?(a, b, 1);

find_word? = fn "`s1 `s2 -> `b. True if `s1 is a word in the `s2 sentence" (string word, string sent)
  lexists?(fn (x) !string_icmp(word, x), split_words(sent)) != false;

unquote = fn "`s1 -> `s2. Returns `s1 without any surrounding single or double quotes" (string s)
  [
    | c, l |
    if ((l = string_length(s)) <= 1 || ((c = s[0]) != ?' && c != ?\") ||
	s[l - 1] != c) s
    else substring(s, 1, l - 2)
  ];

skip_white = fn "`s1 -> `s2. Returns `s1 without any leading white space" (string s)
  [
    | n, l |
    n = 0;
    l = string_length(s);
    while (n < l && cspace?(s[n]))
      ++n;
    substring(s, n, l - n)
  ];

string_ljustify = fn "`s1 `n -> `s2. Left justifies `s1 in a field `n characters wide"
  (string s, int n)
  [
    | l |

    l = string_length(s);
    if (l >= n) s
    else string_ljustify_cut(s, n)
  ];

string_ljustify_cut = fn "`s1 `n -> `s2. Left justifies `s1 in a field `n characters wide. Truncates `s if necessary"
  (string s, int n)
  [
    | l, blanks |

    l = string_length(s);
    if (l >= n) substring(s, 0, n)
    else
      [
	string_fill!(blanks = make_string(n - l), ? );
	s + blanks
      ]
  ];

string_rjustify = fn "`s1 `n -> `s2. Right justifies `s1 in a field `n characters wide"
  (string s, int n)
  [
    | l |

    l = string_length(s);
    if (l >= n) s
    else string_rjustify_cut(s, n)
  ];

string_rjustify_cut = fn "`s1 `n -> `s2. Right justifies `s1 in a field `n characters wide. Truncates `s if necessary"
  (string s, int n)
  [
    | l, blanks |

    l = string_length(s);
    if (l >= n) substring(s, 0, n)
    else
      [
	string_fill!(blanks = make_string(n - l), ? );
	blanks + s
      ]
  ];

[
  | op |
  op = make_string_oport();

  concat_words = fn "`l `s1 -> `s2. Assembles a list of a string into a single string with `s1 as separator" (list l, string sep)
    if (l == null)
      ""
    else if (cdr(l) == null)
      car(l)
    else
      [
        port_empty!(op);
        loop
          [
            pprint(op, car(l));
            l = cdr(l);
            if (l == null)
              exit port_string(op);
            pprint(op, sep)
          ]
      ];
];

// Bitsets (basic operations are in C)

bitset_to_list = fn "`bitset `map -> `l. Returns a list of `map[`i] for all bits `i in `bitset" (string set, map)
  breduce(fn (i, l) map[i] . l, null, set);

breduce = fn "`f `x0 `bitset -> `x. Reduces `bitset by `x = `f(`i, `x) for each bit `i, and initial value `x0" (function f, x, string b)
  [
    | l, i, n, bi |

    l = string_length(b);
    i = 0; n = 0;
    while (i < l)
      [
	bi = b[i];
	if (bi & 1) x = f(n, x);
	if (bi & 2) x = f(n + 1, x);
	if (bi & 4) x = f(n + 2, x);
	if (bi & 8) x = f(n + 3, x);
	if (bi & 16) x = f(n + 4, x);
	if (bi & 32) x = f(n + 5, x);
	if (bi & 64) x = f(n + 6, x);
	if (bi & 128) x = f(n + 7, x);
	n = n + 8;
	i = i + 1;
      ];
    x
  ];

bcomplement! = fn "`bitset1 -> `bitset1. `bitset1 = ~`bitset1 (beware extra bits)" (string b1)
  [
    | l |

    l = string_length(b1);
    while ((l = l - 1) >= 0) b1[l] = ~b1[l];
    b1
  ];

bcomplement = fn "`bitset1 -> `bitset2. `bitset2 = ~`bitset1 (beware extra bits)" (string b1)
  [
    | b2 |

    string_fill!(b2 = bcopy(b1), 255);
    bdifference!(b2, b1)
  ];

bforeach = fn "`f `bitset -> . Does `f(`i) for each bit set in `bitset" (function f, string b)
  [
    | l, i, n, bi |

    l = string_length(b);
    i = 0; n = 0;
    while (i < l)
      [
	bi = b[i];
	if (bi & 1) f(n);
	if (bi & 2) f(n + 1);
	if (bi & 4) f(n + 2);
	if (bi & 8) f(n + 3);
	if (bi & 16) f(n + 4);
	if (bi & 32) f(n + 5);
	if (bi & 64) f(n + 6);
	if (bi & 128) f(n + 7);
	n = n + 8;
	i = i + 1;
      ];
  ];

call_cc = fn "`f0 -> `x0. Call `f0(`f1), where `f1(`x1) can be used to return `x1 from the call to `call_cc." (function f)
[
  | call_cc2 |

  call_cc2 = fn (buf)
    f([
        | continuation |
        // "useless" variable to name the function
        continuation = fn "`x -> . Continuation function from `call_cc()" (x)
          longjmp(buf, x)
    ]);

  setjmp(call_cc2)
];

];
