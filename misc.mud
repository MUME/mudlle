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

library misc // Miscellaneous useful functions
requires system, sequences
defines abbrev?, assert, assert_message, assoc, assq, bcomplement,
  bcomplement!, bforeach, bitset_to_list, breduce, caaar, caadr, caar, cadar,
  caddr, cadr, cdaar, cdadr, cdar, cddar, cdddr, cddr,
  concat_words, difference, fail, fail_message, find_word?,
  intersection, last_element, last_pair, list_first_n!, list_index,
  list_to_vector, lprotect, lqsort, mappend, mapstr, member, memq, nth,
  nth_element, nth_element_cond, nth_pair, random_element, repeat,
  set_eq?, set_in?, skip_white, sorted_table_list, sorted_table_vector,
  string_head?, string_index, string_ljustify, string_ljustify_cut,
  string_rindex, string_rjustify, string_rjustify_cut, string_tail, table_copy,
  union, unquote, vector_exists_index, vector_index, vector_rindex,
  vector_to_list, vequal?, vqsort!, vqsort_slice!, call_cc
[

repeat = fn "`n `f -> . Execute `f() `n times" (int n, function f)
  while (n-- > 0) f();

lprotect = fn "`l -> `l. Protects list `l" (list l)
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
assert_message = fn "`b `s -> . Fail with message `s if `b is false" (b, string s)
  if (!b) fail_message(s);
fail = none fn " -> . Fail." () error(error_abort);
fail_message = none fn "`s -> . Fail with message `s." (string s)
  [
    display(s); newline();
    fail()
  ];

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
      else ++check
  ];

vector_rindex = fn "`x `v -> `n. Finds index of last `x in `v, or -1 if none" (x, vector v)
  [
    | check |
    check = vector_length(v);
    loop
      if (check == 0) exit -1
      else if (v[--check] == x) exit check
  ];

vector_exists_index = fn "`f `v -> `n. Returns the index of first element `x of `v for which `f(`x) is true, or -1 if none" (function f, vector v)
  [
    | l, i |

    l = vector_length(v);
    i = 0;
    loop
      if (i == l) exit -1
      else if (f(v[i])) exit i
      else ++i
  ];

string_index = fn "`s `n1 -> `n2. Finds index of char `n1 in `s, or -1 if none" (string str, int n)
  [
    n &= 255;
    for (|l, i| [ l = string_length(str); i = 0 ]; i < l; ++i)
      if (str[i] == n) exit<function> i;
    -1
  ];

string_rindex = fn "`s `n1 -> `n2. Finds last index of char `n1 in `s, or -1 if none" (string str, int n)
  [
    n &= 255;
    for (|i| i = string_length(str); i > 0; )
      if (str[--i] == n) exit<function> i;
    -1
  ];

vequal? = fn "`v1 `v2 -> `b. True if the elements of `v1 are == to those of `v2" (vector v1, vector v2)
  [
    | l |
    l = vector_length(v1);
    if (l != vector_length(v2)) false
    else
      [
	while (--l >= 0)
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
      results = f(s[--len]) . results;

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

memq = fn "`x `l -> `p. Returns the first pair `p of list `l whose `car is == `x, or `false if none" (x, list l)
  loop
    if (l == null) exit false
    else if (x == car(l)) exit l
    else l = cdr(l);

member = fn "`x `l -> `p. Returns the first pair `p of list `l whose `car is `equal? to `x, or `false if none" (x, list l)
  loop
    if (l == null) exit false
    else if (equal?(x, car(l))) exit l
    else l = cdr(l);

assq = fn "`x `l -> `p. Looks for a pair in `l whose `car is == to `x. Returns that pair, false if not found" (x, list l)
  loop
    [
      if (l == null) exit false;
      | item |
      item = car(l);
      if (car(item) == x) exit item;
      l = cdr(l)
    ];

assoc = fn "`x `l -> `p. Looks for a pair in `l whose `car is `equal? to `x. Returns that pair, false if not found" (x, list l)
  loop
    [
      if (l == null) exit false;
      | item |
      item = car(l);
      if (equal?(car(item), x)) exit item;
      l = cdr(l)
    ];

nth = fn "`n `l -> `x. Returns `n'th (one-based) element of list `l" (int n, list l)
  [
    while (--n > 0)
      l = cdr(l);
    car(l)
  ];

list_to_vector = fn "`l -> `v. Makes a vector out of a list" (list lst) [
   |vec, i|
   vec = make_vector (llength (lst));
   i = 0;
   while (pair? (lst)) [
      vec [i] = car (lst);
      ++i;
      lst = cdr (lst);
   ];
   vec;
];

vector_to_list = fn "`v -> `l. Makes a vector into a list" (vector v)
  [
    | len, l |

    len = vector_length(v);
    while (--len >= 0) l = v[len] . l;
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
      --n;
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
      ++count;
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

string_head? = fn "`s1 `s2 `n -> `b. True if `s1 = first of `s2, min `n characters (case and accentuation insensitive)" (string head, string whole, int n)
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

abbrev? = fn "`s1 `s2 -> `b. Returns true if `s1 is an abbreviation of `s2 (case and accentuation insensitive)" (string a, string b)
  [
    | la |
    la = slength(a);
    la && la <= slength(b) && string_nicmp(a, b, la) == 0
  ];

find_word? = fn "`s1 `s2 -> `b. True if `s1 is a word in the `s2 sentence, as seen by `split_words()." (string word, string sent)
  lexists?(fn (x) !string_icmp(word, x), split_words(sent)) != false;

unquote = fn "`s1 -> `s2. Returns `s1 without any surrounding single or double quotes" (string s)
  [
    | c, l |
    if ((l = string_length(s)) <= 1 || ((c = s[0]) != ?' && c != ?\") ||
	s[l - 1] != c) s
    else substring(s, 1, l - 2)
  ];

skip_white = fn "`s1 -> `s2. Returns `s1 without any leading white space. May return `s1 unmodified." (string s)
  [
    | n, l |
    n = 0;
    l = string_length(s);
    while (n < l && cspace?(s[n]))
      ++n;
    if (n == 0)
      s
    else
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
	string_fill!(blanks = make_string(n - l), ?\ );
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
	string_fill!(blanks = make_string(n - l), ?\ );
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
      [
        | s |
        s = car(l);
        if (!string?(s))
          error(error_bad_type);
        s
      ]
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

bitset_to_list = list fn "`bitset `map -> `l. Returns a list of `map[`i] for all bits `i in `bitset. `map must be a vector or a string." (string set, {vector,string} map)
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
	n += 8;
	++i;
      ];
    x
  ];

bcomplement! = fn "`bitset1 -> `bitset1. `bitset1 = ~`bitset1 (beware extra bits)" (string b1)
  [
    | l |

    l = string_length(b1);
    while (--l >= 0) b1[l] = ~b1[l];
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
	n += 8;
	++i;
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
          longjmp(buf, x);
        continuation
    ]);

  setjmp(call_cc2)
];

];
