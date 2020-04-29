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

// Provide a consistent set of operations on sequences:
// lists, dlists, vectors, strings
library sequences
requires dlist
defines
  lcopy, dcopy, scopy, vcopy,
  llength, slength, vlength, // dlength in dlist
  lreverse, dreverse, sreverse, vreverse,
  lreverse!, dreverse!, sreverse!, vreverse!,
  lappend, dappend, sappend, vappend,
  lappend!, dappend!,
  lmap, lmapi, dmap, smap, smapi, vmap, vmapi, table_map,
  lmap!, lmapi!, dmap!, smap!, smapi!, vmap!, vmapi!, table_map!,
  lforeach, dforeach, sforeach, vforeach,
  lforeachi, vforeachi, sforeachi,
  lexists?, dexists?, sexists?, vexists?,
  lforall?, dforall?, sforall?, vforall?,
  lreduce, dreduce, sreduce, vreduce,
  lreducei, vreducei,
  ldelete, ddelete, vdelete,
  ldelete!, ddelete!,
  lfilter, dfilter, sfilter, vfilter, table_filter,
  lfilter!, dfilter!, table_filter!,
  lfind?, dfind?, sfind?, vfind?,
  vfill!, sfill!,
  subvector
[
// copy, reverse, reverse!, append, append!, map, map!, foreach
// exists?, forall?, reduce, delete, delete!, filter, filter!, find?

// Each operation starts with one of 4 letters (l, d, v, s) to indicate the
// type of sequence on which it operates.

// Predicates terminate in ?
// Operations that mutate the structure end in !
// All comparisons are done with ==

// Several of these operations duplicate existing functions

// Implementation note:
//   the dlist functions rely on the dlist rep for performance

// copy: returns a new sequence with the same contents

lcopy = fn "`l1 -> `l2. Returns a copy of `l1" (list lst)
  if (lst == null)
    null
  else
    [
      | res, trail |
      res = car(lst) . null;
      trail = res;
      lst = cdr(lst);

      while (pair? (lst))
	[
	  set_cdr!(trail, car(lst) . null);
	  trail = cdr(trail);
	  lst = cdr(lst);
	];
      res
    ];

dcopy = fn "`d1 -> `d2. Returns a copy of `d1" ({null,vector} d)
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

scopy = fn "`s1 -> `s2. Returns a copy of `s1" (string s) s + ""; // easy ...

vcopy = fn "`v1 -> `v2. Returns a copy of `v1" (vector v1)
  [
    | l, v2 |
    l = vector_length(v1);
    v2 = make_vector(l);
    while ((l = l - 1) >= 0) v2[l] = v1[l];
    v2
  ];

// length: return length of sequence

llength = fn "`l -> `n. Returns the length of the list `l" (list lst)
  for (|n| n = 0; ; [ lst = cdr(lst); ++n ])
    if (lst == null)
      exit n;

slength = string_length;
vlength = vector_length;
// dlength is in dlist.mud

// reverse:

lreverse = fn "`l1 -> `l2. Returns reverse of `l1" (list lst)
  [
    | result |

    while (lst != null)
      [
	result = car(lst) . result;
        lst = cdr (lst);
      ];
    result
  ];

dreverse = fn "`d1 -> `d2. Returns a list with the contents of `d1 in reverse order" ({null,vector} d1)
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

sreverse = fn "`s1 -> `s2. Returns string with all characters reversed" (string s1)
  [
    | l, s2, i |
    i = l = string_length(s1);
    s2 = make_string(l);
    l = l - 1;
    while ((i = i - 1) >= 0) s2[i] = s1[l - i];
    s2
  ];

vreverse = fn "`v1 -> `v2. Returns vector with all elements reversed" (vector v1)
  [
    | l, v2, i |
    i = l = vector_length(v1);
    v2 = make_vector(l);
    l = l - 1;
    while ((i = i - 1) >= 0) v2[i] = v1[l - i];
    v2
  ];

// reverse!:

lreverse! = fn "`l1 -> `l2. Reverses list `l1, destructively" (list l1)
  [
    | prev |
    prev = null;
    loop
      [
        if (l1 == null)
          exit prev;
        | next |
        next = cdr(l1);
        set_cdr!(l1, prev);
        prev = l1;
        l1 = next;
      ];
  ];

dreverse! = {vector,null} fn "`d1 -> `d2. Reverses dlist `d1, destructively" ({null,vector} d1)
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

sreverse! = fn "`s -> `s. Returns string with all elements reversed" (string s)
  [
    for (| h, l | [ h = string_length(s); l = 0 ]; --h > l; ++l)
      [
        | swap |
        swap = s[h];
        s[h] = s[l];
        s[l] = swap;
      ];
    s
  ];

vreverse! = fn "`v -> `v. Returns vector with all elements reversed" (vector v)
  [
    for (| h, l | [ h = vector_length(v); l = 0 ]; --h > l; ++l)
      [
        | swap |
        swap = v[h];
        v[h] = v[l];
        v[l] = swap;
      ];
    v
  ];

// append:

lappend = list fn "`l1 `l2 -> `l3. Appends `l2 to `l1 as `l3 (shares tail with `l2)" (list l1, list l2)
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

dappend = fn "`d1 `d2 -> `d3. Returns a new list `d3 with the contents of `d1 and `d2" ({null,vector} d1, {null,vector} d2)
  dmerge!(dcopy(d1), dcopy(d2));

vappend = fn "`v1 `v2 -> `v3. Returns a new vector `v3 with the contents of `v1 and `v2" (vector v1, vector v2)
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

lappend! = fn "`l1 `l2 -> `l3. Return `l2 appended to `l1, destructively" (list l1, list l2)
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

lmap = fn "`f `l1 -> `l2. Applies `f to every element of `l1 (from 1st to last) and returns a new list with the results" (function f, list l)
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


lmapi = fn "`f `l1 -> `l2. Applies `f(`x, `n) to every element `x of `l1 at (zero-based) index `n (from 1st to last) and returns a new list with the results." (function f, list l)
  if (l == null) null
  else
    [
      | first, last, n |

      last = first = f(0, car(l)) . null;
      l = cdr(l);
      n = 1;
      while (l != null)
	[
	  | new |

	  new = f(n, car(l)) . null;
	  set_cdr!(last, new);
	  last = new;
          ++n;
	  l = cdr(l);
	];
      first
    ];

dmap = fn "`f `d1 -> `d2. Returns result of applying `f to the elements of `d1, in order" (function f, {null,vector} d)
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

vmap = fn "`c `v1 -> `v2. Applies `c to every element of `v1 (from 1st to last) and makes a vector `v2 of the results" (function f, vector v)
  [
    | r, i, l |

    l = vector_length(v);
    r = make_vector(l);
    i = 0;
    while (i < l) [ r[i] = f(v[i]); i = i + 1 ];
    r
  ];

smap = fn "`c `s1 -> `s2. Applies `c to every element of `s1 (from 1st to last) and makes a string `s2 of the results" (function f, string s)
  [
    | r, i, l |
    l = string_length(s);
    r = make_string(l);
    i = 0;
    while (i < l) [ r[i] = f(s[i]); i = i + 1 ];
    r
  ];

table_map = fn "`f `t1 -> `t2. Returns a copy of table `t1 with data from `f(`sym) from the elements in `t2" (function f, table t)
  [
    | result |
    result = make_table();
    table_foreach (fn (sym) result[symbol_name(sym)] = f(sym),
		   t);
    result
  ];

// map!:

lmap! = fn "`f `l -> `l. Applies `f to every element of `l (from 1st to last) and returns the modified list with the results" (function f, list l)
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

lmapi! = fn "`f `l -> `l. Applies `f(`n, `x) to every element `x at index `n (zero-based) and returns the modified list with the results." (function f, list l)
  [
    | s, n |
    n = 0;
    s = l;
    while (l != null)
      [
	set_car!(l, f(n, car(l)));
        ++n;
	l = cdr(l)
      ];
    s
  ];

dmap! = fn "`f `d -> `d. Applies `f to every element of `d (from 1st to last) and returns the modified list with the results" (function f, {null,vector} d)
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

vmap! = fn "`f `v -> `v. Applies `f to every element of `v (from 1st to last) and returns the modified vector with the results" (function f, vector v)
  [
    | i, l |
    l = vector_length(v);
    i = 0;
    while (i < l) [ v[i] = f(v[i]); i = i + 1 ];
    v
  ];

vmapi = fn "`f `v0 -> `v1. Applies `f(`n, `x) to every element `x at index `n of `v0 and returns a new vector with the results" (function f, vector v)
  [
    | r, l |
    l = vector_length(v);
    r = make_vector(l);
    for (|i| i = 0; i < l; ++i)
      r[i] = f(i, v[i]);
    r
  ];

vmapi! = fn "`f `v -> `v. Applies `f(`n, `x) to every element `x at index `n of `v and returns the modified vector with the results" (function f, vector v)
  [
    for (|i, l| [ l = vector_length(v); i = 0 ]; i < l; ++i)
      v[i] = f(i, v[i]);
    v
  ];

smap! = fn "`f `s -> `s. Applies `f to every element of `s (from 1st to last) and returns modified string with the results" (function f, string s)
  [
    | i, l |
    l = string_length(s);
    i = 0;
    while (i < l) [ s[i] = f(s[i]); i = i + 1 ];
    s
  ];

smapi = fn "`f `s0 -> `s1. Applies `f(`n0, `n1) to every character `n1 at index `n0 of `s0 and returns a new string with the results" (function f, string s)
  [
    | l, r |
    l = string_length(s);
    r = make_string(l);
    for (|i| i = 0; i < l; ++i)
      r[i] = f(i, s[i]);
    r
  ];

smapi! = fn "`f `s -> `s. Applies `f(`n0, `n1) to every character `n1 at index `n0 of `s and returns the modified string with the results" (function f, string s)
  [
    for (|i, l| [ l = string_length(s); i = 0 ]; i < l; ++i)
      s[i] = f(i, s[i]);
    s
  ];

table_map! = fn "`f `t -> `t. Sets each table entry of `t to `f(`sym)" (function f, table t)
  [
    table_reduce(fn (sym, f) [
      symbol_set!(sym, f(sym));
      f
    ], f, t);
    t
  ];

// foreach:

lforeach = fn "`f `l -> . Applies `f(`e) to every element `e of `l" (function f, list l)
  while (l != null)
    [
      f(car(l));
      l = cdr(l);
    ];

lforeachi = fn "`f `l -> . Applies `f(`n, `e) to every element `e at index `n of `l" (function f, list l)
  for (|i|i = 0; l != null; [ ++i; l = cdr(l) ])
    f(i, car(l));

dforeach = fn "`f `d -> . Applies `f(`e) to every element `e of `d (from 1st to last)" (function f, {null,vector} d)
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

vforeach = fn "`c `v -> . Applies `c(`e) to every element `e of `v (from 1st to last)" (function f, vector v)
  for (| i, l | [ i = 0; l = vector_length(v) ]; i < l; ++i)
    f(v[i]);

vforeachi = fn "`c `v -> . Applies `c(`n, `e) to every element `e at index `n of `v." (function f, vector v)
  for (| i, l | [ i = 0; l = vector_length(v) ]; i < l; ++i)
    f(i, v[i]);

sforeach = fn "`c `s -> . Applies `c(`n) to every characnter `n of `s (from 1st to last)" (function f, string s)
  for (|i, l| [ l = string_length(s); i = 0 ]; i < l; ++i)
    f(s[i]);

sforeachi = fn "`c `s -> . Applies `c(`n0, `n1) to every character `n1 at index `n0 in `s (from 1st to last)" (function f, string s)
  for (|i, l| [ l = string_length(s); i = 0 ]; i < l; ++i)
    f(i, s[i]);

// exists?:

lexists? = fn "`f `l -> `x. Returns first element `x of `l for which `f(`x) is true; false if none found" (function f, list l)
  loop
    [
      | x |

      if (l == null) exit false
      else if (f(x = car(l))) exit x
      else l = cdr(l);
    ];

dexists? = fn "`f `d -> `x. Returns first element `x of `d for which `f(`x) is true; false if none found" (function f, {null,vector} d)
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

vexists? = fn "`f `v -> . Returns first element `x of `v for which `f(`x) is true; false if none found" (function f, vector v)
  [
    | i, l |
    l = vector_length(v);
    i = 0;
    loop
      if (i == l) exit false
      else if (f(v[i])) exit v[i]
      else i = i + 1
  ];

sexists? = fn "`f `s -> `x. Returns first character `n of `s for which `f(`n) is true; false if none found" (function f , string s)
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

lforall? = fn "`f `l -> `b. Returns true if `f(`x) is true for all elements of list `l (in order)" (function f, list l)
  loop
    if (l == null) exit true
    else if (!f(car(l))) exit false
    else l = cdr(l);

dforall? = fn "`f `d -> `b. Returns true if `f(`x) is true for all elements of dlist `d (in order)" (function f, {null,vector} d)
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

vforall? = fn "`f `v -> `b. Returns true if `f(`x) is true for all elements `x of `v (in order)" (function f, vector v)
  [
    | i, l |
    l = vector_length(v);
    i = 0;
    loop
      if (i == l) exit true
      else if (!f(v[i])) exit false
      else i = i + 1
  ];

sforall? = fn "`f `s -> `b. Returns true if `f(`n) is true for all characters `n of `s (in order)" (function f, string s)
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

lreduce = fn "`f `x `l -> `x[last]. Reduces list `l by `x[n+1] = `f(`e, `x[n]) for each element `e and initial value `x." (function f, x, list l)
  [
    while (l != null)
      [
	x = f(car(l), x);
	l = cdr(l)
      ];
    x
  ];

lreducei = fn "`f `x `l -> `x[last]. Reduces list `l by `x[n+1] = `f(`n, `e, `x[n]) for each element `e at index `n and initial value `x." (function f, x, list l)
  [
    for (|i|i = 0; l != null; [ ++i; l = cdr(l) ])
      x = f(i, car(l), x);
    x
  ];

dreduce = fn "`f `x `d -> `x[last]. Reduces dlist `d by `x[n+1] = `f(`e, `x[n]) for each element `e and initial value `x."
  (function f, x, {null,vector} d)
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

vreduce = fn "`f `x `v -> `x[last]. Reduces vector `v by `x[n+1] = `f(`e, `x[n]) for each element `e and initial value `x." (function f, x, vector v)
  [
    | i, l |
    l = vector_length(v);
    i = 0;
    while (i < l) [ x = f(v[i], x); i = i + 1 ];
    x
  ];

vreducei = fn "`f `x `v -> `x[last]. Reduces vector `v by `x[n+1] = `f(`n, `e, `x[n]) for each element `e at index `n and initial value `x." (function f, x, vector v)
  [
    for (| i, l | [ i = 0; l = vector_length(v) ]; i < l; ++i)
      x = f(i, v[i], x);
    x
  ];

sreduce = fn "`f `x `s -> `x[last]. Reduces string `s by `x[i+1] = `f(`n, `x[i]) for each character `n and initial value `x." (function f, x, string s)
  [
    | i, l |
    l = string_length(s);
    i = 0;
    while (i < l) [ x = f(s[i], x); i = i + 1 ];
    x
  ];

// delete:

// These could be optimised

ldelete = fn "`x `l1 -> `l2. Returns a copy of `l1 without any occurrences of `x" (x, list l)
  lfilter(fn (y) y != x, l);

ddelete = fn "`x `d1 -> `d2. Returns a copy of `d1 without any occurrences of `x" (x, {null,vector} d)
  dfilter(fn (y) y != x, d);

vdelete = fn "`x `v1 -> `v2. Returns a copy of `v1 without any occurrences of `x" (x, vector v)
  vfilter(fn (y) y != x, v);

// delete!:

ldelete! = fn "`x `l1 -> `l2. `l2 is `l1 with all `x's destructively deleted." (x, list l)
   lfilter!(fn (y) x != y, l);

ddelete! = fn "`x `d1 -> `d2. Returns `d1 with all `x's destructively deleted." (x, {null,vector} d)
  dfilter!(fn (y) y != x, d);

// sdelete! and vdelete! make no sense

// filter:

lfilter = fn "`f `l1 -> `l2. Returns a copy of `l1, filtered by function `f" (function f, list l)
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

dfilter = {null,vector} fn "`f `d1 -> `d2. Returns a copy of `d1 filtered by function `f" (function f, {null,vector} d)
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

vfilter = fn "`f `v1 -> `v2. Returns a copy of `v1 filtered by function `f" (function f, vector v)
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

sfilter = fn "`f `s1 -> `s2. Returns a copy of `s1 filtered by function `f" (function f, string s)
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

table_filter = fn "`f `t1 -> `t2. Returns a copy of `t1 filtered by function `f(`sym) for each symbol `sym in `t1." (function f, table t)
  [
    | result |
    // explain to compiler that we return a table
    result = make_table();
    table_reduce(fn (sym, result) [
      if (f(sym))
        result[symbol_name(sym)] = symbol_get(sym);
      result
    ], result, t);
    result
  ];

// filter!:

lfilter! = fn "`f `l1 -> `l2. Returns (the possibly new head of) `l1 destructively filtered by function `f(`x) for each element `x in `l1." (function f, list l)
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

dfilter! = fn "`f `d1 -> `d2. Returns (the possibly new head of) `d1 filtered by function `f(`x) for each element `x in `d1." (function f, {null,vector} d)
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

table_filter! = fn "`f `t -> `t. Filters `t by function `f(`sym) for each symbol `sym in `t. Returns `t." (function f, table t)
  [
    // this is actually safe
    table_reduce(fn (sym, f) [
      if (!f(sym)) symbol_set!(sym, null);
      f
    ], f, t);
    t
  ];

// sfilter! and vfilter! make no sense

// find?:

lfind? = fn "`x `l -> `b. Returns TRUE if `x is in `l" (x, list l)
  loop
    if (l == null) exit false
    else if (car(l) == x) exit true
    else l = cdr(l);

dfind? = fn "`x `d -> `b. Returns TRUE if `x is in `d" (x, {null,vector} d)
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

vfind? = fn "`x `v -> `b. Returns TRUE if `x is in `v" (x, vector v)
  [
    for (|i, l| [ i = 0; l = vlength(v) ]; i < l; ++i)
      if (v[i] == x) exit<function> true;
    false
  ];

sfind? = fn "`n `s -> `b. Returns TRUE if `n is in `s" (int n, string s)
  [
    for (|i, l| [ i = 0; l = string_length(s) ]; i < l; ++i)
      if (s[i] == n) exit<function> true;
    false
  ];

subvector = vector fn "`v0 `n0 `n1 -> `v1. Returns a copy of `n1 elements from vector `v0, starting at index `n0" (vector v, int start, int len)
  [
    | r |
    if (start < 0)
      start += vlength(v);
    if (start < 0 || len < 0 || start + len > vlength(v))
      error(error_bad_value);
    r = make_vector(len);
    while (len-- > 0)
      r[len] = v[start + len];
    r
  ];

vfill! = vector_fill!;

sfill! = string_fill!;

]
