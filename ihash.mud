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

library ihash  // A hash-table indexed by integers  -*- mudlle -*-
               // (represented as a vector of buckets)
requires system, misc, sequences
defines make_ihash, ihash_ref, ihash_set!, ihash_list, ihash_remove!,
  ihash_map!, ihash_foreach, ihash_filter!, ihash_resize, ihash_entries
[
  make_ihash = fn "n -> itable. Make an integer hash table of \"size\" n" (size)
    [
      if (size <= 0)
	error(error_bad_value);
      make_vector(size)
    ];

  ihash_entries = fn "ihash -> n. Returns the number of entries in the table" (itable)
    [
      | n |
      n = 0;
      vforeach(fn(l) n = n + llength(l), itable);
      n
    ];

  ihash_resize = fn "itable n -> ihash. Rehashes the entries in the table with new size n" (vector itable, int size)
    [
      | res |
      res = make_ihash(size);
      ihash_foreach(fn(n, v) ihash_set!(res, n, v),
		    itable);
      res
    ];

  ihash_ref = fn "itable n -> x. Returns entry x of itable (null if absent)" (itable, m)
    [
      | x |

      x = assq(m, itable[abs(m) % vector_length(itable)]);
      if (x) cdr(x)
      else null
    ];

  ihash_set! = fn "itable n x -> . Sets itable[n] = x" (itable, m, x)
    [
      | bucket, i |

      i = abs(m) % vector_length(itable);
      bucket = assq(m, itable[i]);
      if (bucket) set_cdr!(bucket, x)
      else itable[i] = (m . x) . itable[i];
    ];

  ihash_remove! = fn "itable n -> . Removes entry <n> from <itable>" (itable, n)
    [		
      | i |
      i = abs(n) % vlength (itable);
      itable[i] = lfilter (fn(x) car(x) != n, itable[i]);
    ];

  ihash_list = fn "itable -> l. Returns list of entries in table" (itable)
    vreduce(fn (x, l) lappend(lfilter(fn (bucket) cdr(bucket) != null, x), l),
	    null, itable);

  ihash_map! = fn "itable c -> . Maps all entries in <itable> using c(<n>, <value>) -> value" (itable, func)
    [
      for (0, vlength (itable) - 1, fn (n) [
	itable[n] = lmap! (fn(x) car(x) . func(car(x), cdr(x)), itable[n]);
      ]);
    ];

  ihash_filter! = fn "c itable -> . Filter itable with c(n, val)" (fun, itable)
    for(0, vlength(itable) - 1, fn(idx) [
      itable[idx] = lfilter!(fn(x) fun(car(x), cdr(x)), itable[idx]);
    ]);

  ihash_foreach = fn "c itable -> . Runs c(<n>, <val>) for all entries in <itable>" (func, itable)
    for (0, vlength (itable) - 1, fn (n) lforeach (fn (x) func (car(x), 
								cdr(x)), 
						   itable[n]));

];
