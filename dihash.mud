/* 
 * Copyright (c) 1993-2006 David Gay and Gustav H�llberg
 * All rights reserved.
 * 
 * Permission to use, copy, modify, and distribute this software for any
 * purpose, without fee, and without written agreement is hereby granted,
 * provided that the above copyright notice and the following two paragraphs
 * appear in all copies of this software.
 * 
 * IN NO EVENT SHALL DAVID GAY OR GUSTAV HALLBERG BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF DAVID GAY OR
 * GUSTAV HALLBERG HAVE BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * DAVID GAY AND GUSTAV HALLBERG SPECIFICALLY DISCLAIM ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
 * FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS ON AN
 * "AS IS" BASIS, AND DAVID GAY AND GUSTAV HALLBERG HAVE NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

library dihash
requires sequences
defines make_dihash, dihash_ref, dihash_set!, dihash_resize!,
  dihash_foreach, dihash_filter!, dihash_remove!, dihash_map!,
  dihash_entries, dihash_resize, dihash_list, dihash_size,
  dihash_map, ihash_to_dihash, dihash_vector, dihash?, dihash_empty!,
  dihash_reduce
[
  | div_used, div_data, vslot_next, vslot_key, vslot_data, next_size,
    good_size | 

  div_used = 0;
  div_data = 1;

  vslot_next = 0;
  vslot_key = 1;
  vslot_data = 2;

  next_size = fn (int size)
    [
      | sizes, i |

      // random primes near 2^n * 3
      sizes = '[ 3 5 11 19 41 79 163 317 641 1279 2557 5119 10243 20479 40961
		 81919 163841 327673 655357 1310719 2621447 5242883 10485767
		 20971529 41943049 83886091 167772161 335544323 671088637 
		 1073741823 ];
      i = 0;
      while (size >= sizes[i])
	++i;
      sizes[i]
    ];

  // should return a "nice" hash vector size to use given that we have
  // used number of entries
  good_size = fn (int used)
    used;

  make_dihash = fn "[n] -> . Creates an empty dynamic-sized integer indxed hash table, optionally initialized to size n" v
    [
      if (vlength(v) > 1)
	error(error_wrong_parameters);
      vector(0, if (vlength(v) == 0) '[] else make_vector(v[0]))
    ];

  dihash? = fn "x -> b. Returns true if x could be a dihash" (x)
    (vector?(x) && vlength(x) == 2
     && integer?(x[div_used]) && x[div_used] >= 0
     && vector?(x[div_data]));

  dihash_resize = fn "d n -> . Return a copy of dihash d with n slots, or a reasonable amount of slots if n < 0" (vector hash, int size)
    [
      | new |
      if (size < 0)
	size = good_size(hash[div_used]);

      new = make_vector(size);

      dihash_foreach(fn (key, value) [
	| slot, v |
	slot = abs(key) % size;
	v = vector(new[slot], key, value);
	new[slot] = v
      ], hash);

      vector(hash[div_used], new)
    ];

  dihash_resize! = fn "d n -> . Resize dihash d to have n slots, or a reasonable amount of slots if n < 0" (vector hash, int size)
    [
      | new |
      if (size < 0)
	size = good_size(hash[div_used]);

      new = make_vector(size);
      vforeach(fn (v) [
	while (v != null)
	  [
	    | this, slot |
	    this = v;
	    v = v[vslot_next];
	    slot = abs(this[vslot_key]) % size;
	    this[vslot_next] = new[slot];
	    new[slot] = this;
	  ];
      ], hash[div_data]);
      hash[div_data] = new;
    ];

  dihash_set! = fn "d n x -> . Set entry n to x in dihash d" (vector hash, int key, value)
    [
      | slot, idx, size, v |
      size = vlength(hash[div_data]);

      if (size > 0)
	[
	  idx = abs(key) % size;
	  v = hash[div_data][idx];
	  while (v != null)
	    [
	      if (v[vslot_key] == key)
		[
		  v[vslot_data] = value;
		  exit<function> null;
		];
	      v = v[vslot_next];
	    ];
	];

      if (hash[div_used] * 3 >= size * 2)
	[
	  size = next_size(size);
	  dihash_resize!(hash, size);
	  idx = abs(key) % size;
	];

      slot = vector(hash[div_data][idx], key, value);
      hash[div_data][idx] = slot;
      ++hash[div_used]
    ];

  dihash_ref = fn "d n -> x. Returns dihash data for index n or null" (vector hash, int key)
    [
      | size |
      size = vlength(hash[div_data]);
      if (size == 0)
	null
      else
	[
	  | v |
	  v = hash[div_data][abs(key) % size];
	  loop
	    [
	      if (v == null)
		exit null;
	      if (v[vslot_key] == key)
		exit v[vslot_data];
	      v = v[vslot_next];
	    ];
	];
    ];

  dihash_foreach = fn "c d -> . Runs c(n, x) for each entry in the dihash, with key n and value x." (function func, vector hash)
    vforeach(fn (v) [
      while (v != null)
	[
	  func(v[vslot_key], v[vslot_data]);
	  v = v[vslot_next];
	];
    ], hash[div_data]);

  dihash_reduce = fn "c x0 d -> x1. Returns the reduction c(k, e, x) -> x for each element e with key k in dihash d" (function func, x, vector hash)
    [
      | i, data, len |
      i = 0;
      data = hash[div_data];
      len = vlength(data);
      loop
	[
	  | v |
	  if (i == len)
	    exit x;
	  v = data[i++];
	  while (v != null)
	    [
	      x = func(v[vslot_key], v[vslot_data], x);
	      v = v[vslot_next]
	    ]
	]
    ];

  dihash_filter! = fn "c d -> . Filters data in dihash d with function c(n, x)" (function func, vector hash)
    [
      | used, vzero |
      vzero = vector(null);
      used = 0;
      vmap!(fn(v) [
	vzero[0] = v;
	v = vzero;
	while (v[0] != null)
	  [
	    if (func(v[0][vslot_key], v[0][vslot_data]))
	      [
		v = v[0];
		++used;
	      ]
	    else
	      v[0] = v[0][vslot_next];
	  ];
	vzero[0]
      ], hash[div_data]);
      hash[div_used] = used;
    ];

  dihash_remove! = fn "d n -> b. Removes entry n from dihash d. Returns true if the entry was found" (vector hash, int key)
    [
      | slot, vzero, v, size |
      size = vlength(hash[div_data]);
      if (size)
	[
	  slot = abs(key) % size;
	  vzero = vector(hash[div_data][slot]);
	  v = vzero;
	  
	  loop
	    [
	      if (v[0] == null)
		exit 0;
	      if (v[0][vslot_key] == key)
		[
		  v[0] = v[0][vslot_next];
		  hash[div_data][slot] = vzero[0];
		  --hash[div_used];
		  exit 1
		];
	      v = v[0]
	    ];
	]
      else
	false
    ];

  dihash_empty! = fn "d -> . Remove all entries from dihash d" (vector hash)
    [
      hash[div_used] = 0;
      vector_fill!(hash[div_data], null);
    ];

  dihash_map! = fn "c d -> . Maps all entries x with key i to c(i, x) in the dihash d" (function func, vector hash)
    vforeach(fn (v) [
      loop
	[
	  if (v == null)
	    exit 0;
	  v[vslot_data] = func(v[vslot_key], v[vslot_data]);
	  v = v[vslot_next]
	];
    ], hash[div_data]);
  
  dihash_map = fn "c d -> d. Return a new dihash as mapped by c(i, x) for each entry in the dihash" (function func, vector hash)
    [
      | new |
      new = make_dihash(dihash_size(hash));
      dihash_foreach(fn (key, value) dihash_set!(new, key, value),
		     hash);
      new
    ];

  dihash_entries = fn "d -> n. Returns the number of entries in the dihash d" (vector hash)
    hash[div_used];

  dihash_size = fn "d -> n. Returns the current size of the dihash d" (vector hash)
    vlength(hash[div_data]);

  dihash_list = fn "d -> l. Returns a list of (key . value) of the entries in dihash d" (vector hash)
    [
      | res |
      dihash_foreach(fn (key, value) res = (key . value) . res,
		     hash);
      res
    ];

  dihash_vector = fn "d -> v. Returns a vector of (key . value) of the entries in dihash d" (vector hash)
    [
      | res, i |
      res = make_vector(hash[div_used]);
      i = 0;
      dihash_foreach(fn (n, x) [
	res[i++] = n . x
      ], hash);
      res
    ];

  ihash_to_dihash = fn "i -> d. Returns a dihash of the data stored in the ihash i" (ihash)
    [
      | res |
      res = make_dihash();
      
      // not implemented in terms of ihash_foreach to remove dependencies
      vforeach(fn (l) lforeach (fn (x) dihash_set!(res, car(x), cdr(x)),
				l),
	       ihash);

      res
    ];

]