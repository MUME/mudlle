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

// Provide a consistent set of operations on sequences: 
// lists, dlists, vectors, strings
library sequences
requires system, dlist
defines
  lcopy, dcopy, scopy, vcopy,
  llength, slength, vlength, // dlength in dlist
  lreverse, dreverse, sreverse, vreverse,
  lreverse!, dreverse!, sreverse!, vreverse!,
  lappend, dappend, sappend, vappend,
  lappend!, dappend!, 
  lmap, dmap, smap, vmap,
  lmap!, dmap!, smap!, vmap!,
  lforeach, dforeach, sforeach, vforeach,
  lexists?, dexists?, sexists?, vexists?,
  lforall?, dforall?, sforall?, vforall?,
  lreduce, dreduce, sreduce, vreduce,
  ldelete, ddelete, sdelete, vdelete,
  ldelete!, ddelete!,
  lfilter, dfilter, sfilter, vfilter,
  lfilter!, dfilter!,
  lfind?, dfind?, sfind?, vfind?
[
// copy, reverse, reverse!, append, append!, map, map!, foreach
// exists?, forall?, reduce, delete, delete!, filter, filter!, find?

// Each operation starts with one of 4 letters (l, d, v, s) to indicate the type
// of sequence on which it operates.

// Predicates terminate in ?
// Operations that mutate the structure end in !
// All comparisons are done with ==

// Several of these operations duplicate existing functions


// Implementation note:
//   the dlist functions rely on the dlist rep for performance

// copy: returns a new sequence with the same contents

lcopy = fn "l1 -> l2. Returns a copy of l1" (lst) 
  [
    |res, trail|

    if (pair? (lst)) 
      [
	res = car(lst) . null;
	trail = res;
	lst = cdr(lst);
      ];
    while (pair? (lst))
      [
	set_cdr!(trail, car(lst) . null);
	trail = cdr(trail);
	lst = cdr(lst);
      ];
    res
  ];

dcopy = fn "d1 -> d2. Returns a copy of d1" (d)
  if (d == null) null
  else
    [
      | first, l, next, scan |

      // yuck
      first = l = vector(d[0], null, null);
      first[2] = first;

      scan = d[2];
      while (scan != d)
	[
	  next = vector(scan[0], l, null);
	  l[2] = next;
	  l = next;
	  scan = scan[2];
	];
      l[2] = first;
      first[1] = l;
      first
    ];

scopy = fn "s1 -> s2. Returns a copy of s1" (s) s + ""; // easy ...

vcopy = fn "v1 -> v2. Returns a copy of v1" (v1)
  [
    | l, v2 |
    l = vector_length(v1);
    v2 = make_vector(l);
    while ((l = l - 1) >= 0) v2[l] = v1[l];
    v2
  ];


// length: return length of sequence

llength = fn "l -> n. Returns the length of a list" (lst)
  [
    |result|

    result = 0;
    while (pair? (lst))
      [
	result = result + 1;
	lst = cdr (lst);
      ];
    result
  ];

slength = string_length;
vlength = vector_length;
// dlength is in dlist.mud

// reverse:

lreverse = fn "l1 -> l2. Returns reverse of l1" (lst)
  [
    | result |

    while (pair? (lst))
      [
	result = car(lst) . result;
	lst = cdr (lst);
      ];
    result
  ];

dreverse = fn "d1 -> d2. Returns a list with the contents of d1 in reverse order" (d1)
  if (d1 == null) d1
  else
    [
      | d2, scan |
      scan = d1;
      loop
	[
	  d2 = dcons!(scan[0], d2);
	  scan = dnext(scan);
	  if (scan == d1) exit d2;
	]
    ];

sreverse = fn "s1 -> s2. Returns string with all characters reversed" (s1)
  [
    | l, s2, i |
    i = l = string_length(s1);
    s2 = make_string(l);
    l = l - 1;
    while ((i = i - 1) >= 0) s2[i] = s1[l - i];
    s2
  ];

vreverse = fn "v1 -> v2. Returns vector with all elements reversed" (v1)
  [
    | l, v2, i |
    i = l = vector_length(v1);
    v2 = make_vector(l);
    l = l - 1;
    while ((i = i - 1) >= 0) v2[i] = v1[l - i];
    v2
  ];

// reverse!:

lreverse! = fn "l1 -> l2. Reverses list l1, destructively" (l1)
  if (l1 == null) null
  else
    [
      | prev, next |

      prev = l1;
      l1 = cdr(l1);
      set_cdr!(prev, null);
      while (l1 != null)
	[
	  next = cdr(l1);
	  set_cdr!(l1, prev);
	  prev = l1;
	  l1 = next
	];
      prev
    ];

dreverse! = fn "d1 -> d2. Reverses list d1, destructively" (d1)
  if (d1 == null) null
  else
    [
      | scan |

      scan = d1;
      loop
	[
	  | swap |
	  swap = scan[1];
	  scan[1] = scan[2];
	  scan[2] = swap;

	  scan = swap;
	  if (scan == d1) exit d1[1] // last element is now first
	]
    ];

sreverse! = fn "s1 -> s2. Returns string with all elements reversed" (s1)
  [
    | l, i, swap |
    l = string_length(s1);

    i = l >> 1;
    l = l - 1;

    while ((i = i - 1) >= 0)
      [
	swap = s1[i];
	s1[i] = s1[l - i];
	s1[l - i] = swap;
      ];
    s1
  ];

vreverse! = fn "v1 -> v2. Returns vector with all elements reversed" (v1)
  [
    | l, i, swap |
    l = vector_length(v1);

    i = l >> 1;
    l = l - 1;

    while ((i = i - 1) >= 0)
      [
	swap = v1[i];
	v1[i] = v1[l - i];
	v1[l - i] = swap;
      ];
    v1
  ];

// append:

lappend = fn "l1 l2 -> l3. Appends l2 to l1 as l3 (shares tail with l2)" (l1, l2)
  if (l1 == null) l2
  else
    [
      | l3, scan3, next3 |

      scan3 = l3 = car(l1) . null;
      loop
	[
	  l1 = cdr(l1);
	  if (l1 == null)
	    [
	      set_cdr!(scan3, l2);
	      exit l3
	    ];
	  set_cdr!(scan3, next3 = car(l1) . null);
	  scan3 = next3;
	]
    ];
    
sappend = string_append;

dappend = fn "d1 d2 -> d3. Returns a new list d3 with the contents of d1 and d2" (d1, d2)
  dmerge!(dcopy(d1), dcopy(d2));

vappend = fn "v1 v2 -> v3. Returns a new vector v3 with the contents of v1 and v2" (v1, v2)
  [
    | l, l1, l2, v, i, j |

    l1 = vector_length(v1);
    l2 = vector_length(v2);
    l = l1 + l2;
    v = make_vector(l);

    i = l1;
    while ((i = i - 1) >= 0) v[i] = v1[i];
    i = l2; j = l1 + l2;
    while ((i = i - 1) >= 0) v[j = j - 1] = v2[i];
    v
  ];

// append!:

lappend! = fn "l1 l2 -> l3. l3 is l2 appended to l1, destructively" (l1, l2)
  if (l1 == null) l2
  else
    [
      | end, next |

      end = l1;
      while (null != (next = cdr(end))) end = next;
      set_cdr!(end, l2);
      l1
    ];

dappend! = dmerge!;

// No sappend!, vappend! (no sense)

// map:

lmap = fn "fn l1 -> l2. Filters list l1 according to function fn" (f, l)
  if (l == null) null
  else
    [
      | first, last |

      last = first = f(car(l)) . null;
      l = cdr(l);
      while (l != null)
	[
	  | new |

	  new = f(car(l)) . null;
	  set_cdr!(last, new);
	  last = new;
	  l = cdr(l);
	];
      first
    ];

dmap = fn "fn d1 -> d2. Returns result of applying fn to the elements of d1, in order" (f, d)
  if (d == null) null
  else
    [
      | first, scan |
      scan = d;
      first = dcons!(f(dget(scan)), null);
      loop
	[
	  scan = dnext(scan);
	  if (scan == d) exit first;
	  dcons!(f(dget(scan)), first);
	]
    ];

vmap = fn "c v1 -> v2. Applies c to every element of v1 (from 1st to last) and makes a vector v2 of the results" (f, v)
  [
    | r, i, l |

    l = vector_length(v);
    r = make_vector(l);
    i = 0;
    while (i < l) [ r[i] = f(v[i]); i = i + 1 ];
    r
  ];

smap = fn "fn s1 -> s2. Applies c to every element of s1 (from 1st to last) and makes a string s2 of the results" (f, s)
  [
    | r, i, l |
    l = string_length(s);
    r = make_string(l);
    i = 0;
    while (i < l) [ r[i] = f(s[i]); i = i + 1 ];
    r
  ];


// map!:

lmap! = fn "fn l1 -> l1. Applies fn to every element of l1 (from 1st to last) and returns the modified list with the results" (f, l)
  [
    | s |
    s = l;
    while (l != null)
      [
	set_car!(l, f(car(l)));
	l = cdr(l)
      ];
    s
  ];

dmap! = fn "fn d1 -> d1. Applies fn to every element of d1 (from 1st to last) and returns the modified list with the results" (f, d)
  if (d == null) null
  else
    [
      | scan |
      scan = d;
      loop
	[
	  dset!(scan, f(dget(scan)));
	  scan = dnext(scan);
	  if (scan == d) exit d
	]
    ];

vmap! = fn "c v1 -> v1. Applies c to every element of v1 (from 1st to last) and returns the modified vector with the results" (f, v)
  [
    | i, l |
    l = vector_length(v);
    i = 0;
    while (i < l) [ v[i] = f(v[i]); i = i + 1 ];
    v
  ];

smap! = fn "fn s1 -> s1. Applies c to every element of s1 (from 1st to last) and returns modified string with the results" (f, s)
  [
    | i, l |
    l = string_length(s);
    i = 0;
    while (i < l) [ s[i] = f(s[i]); i = i + 1 ];
    s
  ];

// foreach:

lforeach = fn "fn l -> . Applies fn to every element of l" (f, l)
  while (l != null)
    [
      f(car(l));
      l = cdr(l);
    ];

dforeach = fn "fn d1 -> . Applies fn to every element of d1 (from 1st to last)" (f, d)
  if (d != null)
    [
      | scan |
      scan = d;
      loop
	[
	  f(dget(scan));
	  scan = dnext(scan);
	  if (scan == d) exit 0
	]
    ];

vforeach = fn "c v1 -> . Applies c to every element of v1 (from 1st to last)" (f, v)
  [
    | i, l |
    l = vector_length(v);
    i = 0;
    while (i < l) [ f(v[i]); i = i + 1 ];
  ];

sforeach = fn "fn s1 -> . Applies c to every element of s1 (from 1st to last)" (f, s)
  [
    | i, l |
    l = string_length(s);
    i = 0;
    while (i < l) [ f(s[i]); i = i + 1 ];
  ];

// exists?:

lexists? = fn "fn l -> x. Returns first element x of l for which fn(x) is true, false if none found" (f, l)
  loop
    [
      | x |

      if (l == null) exit false
      else if (f(x = car(l))) exit x
      else l = cdr(l);
    ];

dexists? = fn "fn d -> x. Returns first element x of d for which fn(x) is true, false if none found" (f, d) 
  if (d == null) false
  else
    [
      | scan |
      scan = d;
      loop
	if (f(dget(scan))) exit dget(scan)
	else
	  [
	    scan = dnext(scan);
	    if (scan == d) exit false
	  ]
    ];

vexists? = fn "fn v -> . Returns first element x of v for which fn(x) is true, false if none found" (f, v) 
  [
    | i, l |
    l = vector_length(v);
    i = 0;
    loop
      if (i == l) exit false
      else if (f(v[i])) exit v[i]
      else i = i + 1
  ];

sexists? = fn "fn s -> . Returns first element x of s for which fn(x) is true, false if none found" (f, s)
  [
    | i, l |
    l = string_length(s);
    i = 0;
    loop
      if (i == l) exit false
      else if (f(s[i])) exit s[i]
      else i = i + 1
  ];

// forall?:

lforall? = fn "fn l -> b. Returns true if fn(x) is true for all elements of list l (in order)" (f, l)
  loop
    if (l == null) exit true
    else if (!f(car(l))) exit false
    else l = cdr(l);

dforall? = fn "fn d -> b. Returns true if fn(x) is true for all elements of list d (in order)" (f, d)
  if (d == null) false
  else
    [
      | scan |
      scan = d;
      loop
	if (!f(dget(scan))) exit false
	else
	  [
	    scan = dnext(scan);
	    if (scan == d) exit true
	  ]
    ];

vforall? = fn "fn v -> b. Returns true if fn(x) is true for all elements of v (in order)" (f, v)
  [
    | i, l |
    l = vector_length(v);
    i = 0;
    loop
      if (i == l) exit true
      else if (!f(v[i])) exit false
      else i = i + 1
  ];

sforall? = fn "fn s -> b. Returns true if fn(x) is true for all elements of s (in order)" (f, s)
  [
    | i, l |
    l = string_length(s);
    i = 0;
    loop
      if (i == l) exit true
      else if (!f(s[i])) exit false
      else i = i + 1
  ];

// reduce:

lreduce = fn "fn x l -> . Reduces list l with function fn and initial value x" (f, x, l)
  [
    while (l != null)
      [
	x = f(car(l), x);
	l = cdr(l)
      ];
    x
  ];

dreduce = fn "fn x d -> . Reduces list d with function fn and initial value x" (f, x, d)
  if (d == null) x
  else
    [
      | scan |
      scan = d;
      loop
	[
	  x = f(dget(scan), x);
	  scan = dnext(scan);
	  if (scan == d) exit x
	]
    ];

vreduce = fn "fn x d -> . Reduces v with function fn and initial value x" (f, x, v)
  [
    | i, l |
    l = vector_length(v);
    i = 0;
    while (i < l) [ x = f(v[i], x); i = i + 1 ];
    x
  ];

sreduce = fn "fn x s -> . Reduces s with function fn and initial value x" (f, x, s)
  [
    | i, l |
    l = string_length(s);
    i = 0;
    while (i < l) [ x = f(s[i], x); i = i + 1 ];
    x
  ];

// delete:

// These could be optimised

ldelete = fn "x l1 -> l2. Returns l1 without any occurrences of x" (x, l)
  lfilter(fn (y) y != x, l);

ddelete = fn "x d1 -> d2. Returns d1 without any occurrences of x" (x, d)
  dfilter(fn (y) y != x, d);

vdelete = fn "x v1 -> v2. Returns v1 without any occurrences of x" (x, v)
  vfilter(fn (y) y != x, v);

sdelete = fn "x s1 -> s2. Returns s1 without any occurrences of x" (x, s)
  sfilter(fn (y) y != x, s);


// delete!:

ldelete! = fn "x l1 -> l2. l2 is l1 with all x's deleted" (x, l)
   lfilter!(fn (y) x != y, l);

ddelete! = fn "x d1 -> d2. Returns d1 without any occurrences of x" (x, d)
  dfilter!(fn (y) y != x, d);

// sdelete! and vdelete! make no sense

// filter:

lfilter = fn "fn l1 -> l2. Returns l1 filtered by function fn" (f, l)
  [
    | first, last, x |

    while (l != null)
      [
	if (f(x = car(l)))
	  [
	    | new |

	    new = x . null;
	    if (first == null) first = last = new
	    else
	      [
		set_cdr!(last, new);
		last = new;
	      ];
	  ];

	l = cdr(l);
      ];
    first
  ];

dfilter = fn "fn d1 -> d2. Returns d1 filtered by function fn" (f, d)
  if (d == null) null
  else
    [
      | junk, scan |

      junk = dcons!(null, null); 

      scan = d;
      loop
	[
	  if (f(dget(scan))) dcons!(dget(scan), junk);
	  scan = dnext(scan);
	  if (scan == d) exit dremove!(junk, junk)
	]
    ];

vfilter = fn "fn v1 -> v2. Returns v1 filtered by function fn" (f, v)
  [
    | keep, result, l, i, count |

    // tricky, as f should only be called once

    // find elements to keep
    l = vector_length(v);
    keep = make_string(l);
    i = count = 0;
    while (i < l)
      [
	if (keep[i] = (f(v[i]) != 0)) count = count + 1;
	i = i + 1;
      ];

    // copy to result
    result = make_vector(count);
    while (count > 0)
      [
	// find next element kept
	while (!keep[i = i - 1]) 0;
	result[count = count - 1] = v[i];
      ];
    result
  ];

sfilter = fn "fn s1 -> s2. Returns s1 filtered by function fn" (f, s)
  [
    | keep, result, l, i, count |

    // tricky, as f should only be called once

    // find elements to keep
    l = string_length(s);
    keep = make_string(l);
    i = count = 0;
    while (i < l)
      [
	if (keep[i] = (f(s[i]) != 0)) count = count + 1;
	i = i + 1;
      ];

    // copy to result
    result = make_string(count);
    while (count > 0)
      [
	// find next element kept
	while (!keep[i = i - 1]) 0;
	result[count = count - 1] = s[i];
      ];
    result
  ];

// filter!:

lfilter! = fn "fn l1 -> l2. Returns l1 filtered by function fn" (f, l)
  [
   | check, trail |

   loop // find first
     if (!pair?(l)) exit<function> null
     else if (f(car(l))) exit 0 // found first
     else l = cdr(l);

   check = cdr(l);
   trail = l;

   while (pair?(check))
     [
       if (!f(car(check))) set_cdr!(trail, cdr(check))
       else trail = check;
       check = cdr(check);
     ];
   l
];

dfilter! = fn "fn d1 -> d2. Returns d1 filtered by function fn" (f, d)
  if (d == null) null
  else
    [
      | last |
      last = dprev(d);

      while (d != last)
	[
	  if (!f(dget(d))) d = dremove!(d, d)
	  else d = dnext(d)
	];
      // Handle last element. Return new first element ...
      if (!f(dget(last))) dremove!(last, last)
      else dnext(last)
    ];

// sfilter! and vfilter! make no sense

// find?:

lfind? = fn "x l -> b. Returns TRUE if x is in l" (x, l)
  loop
    if (l == null) exit false
    else if (car(l) == x) exit true
    else l = cdr(l);
    
	
dfind? = fn "x d -> b. Returns TRUE if x is in d" (x, d)
  if (d == null) false
  else
    [
      | scan |

      scan = d;
      loop
	if (dget(scan) == x) exit true
	else
	  [
	    scan = dnext(scan);
	    if (scan == d) exit false
	  ];
    ];

vfind? = fn "x v -> b. Returns TRUE if x is in v" (x, v)
  [
    | i, l |
    l = vector_length(v);
    i = 0;
    loop
      if (i == l) exit false
      else if (v[i] == x) exit true
      else i = i + 1
  ];

sfind? = fn "x s -> b. Returns TRUE if x is in s" (x, s)
  [
    | i, l |
    l = string_length(s);
    i = 0;
    loop
      if (i == l) exit false
      else if (s[i] == x) exit true
      else i = i + 1
  ];

]
