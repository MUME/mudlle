/* 
 * Copyright (c) 1993-2004 David Gay
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

// bit-set operations

new_bitset = fn "n -> bitset. Returns a bitset usable for storing n bits" (n)
  make_string((n + 7) >> 3);

bcopy = fn "bitset1 -> bitset2. Makes a copy of bitset1" (b) b + "";

bclear = fn "bitset -> bitset. Clears all bits of bitset and returns it" (b)
  [
    string_fill!(b, 0);
    b
  ];

set_bit! = fn "bitset n -> . Sets bit n of specified bitset" (b, n)
  [
    | i |

    i = n >> 3;
    b[i] = b[i] | 1 << (n & 7);
  ];

clear_bit! = fn "bitset n -> . Clears bit n of specified bitset" (b, n)
  [
    | i |

    i = n >> 3;
    b[i] = b[i] & ~(1 << (n & 7));
  ];

bit_set? = fn "bitset n -> b. True if bit n is set" (b, n)
  b[n >> 3] & 1 << (n & 7);

bit_clear? = fn "bitset n -> b. True if bit n is set" (b, n)
  !(b[n >> 3] & 1 << (n & 7));

breduce = fn "fn x1 bitset -> x2. Does x = fn(i, x) for each bit set in bitset" (f, x, b)
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

bforeach = fn "fn bitset -> . Does fn(i) for each bit set in bitset" (f, b)
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


// All binary ops expect same-sized bitsets

bunion = fn "bitset1 bitset2 -> bitset3. bitset3 = bitset1 U bitset2" (b1, b2)
  [
    | b3, l |

    l = string_length(b1);
    b3 = make_string(l);
    while ((l = l - 1) >= 0) b3[l] = b1[l] | b2[l];
    b3
  ];

bintersection = fn "bitset1 bitset2 -> bitset3. bitset3 = bitset1 /\ bitset2" (b1, b2)
  [
    | b3, l |

    l = string_length(b1);
    b3 = make_string(l);
    while ((l = l - 1) >= 0) b3[l] = b1[l] & b2[l];
    b3
  ];

bdifference = fn "bitset1 bitset2 -> bitset3. bitset3 = bitset1 - bitset2" (b1, b2)
  [
    | b3, l |

    l = string_length(b1);
    b3 = make_string(l);
    while ((l = l - 1) >= 0) b3[l] = b1[l] & ~b2[l];
    b3
  ];

bunion! = fn "bitset1 bitset2 -> bitset1. bitset1 = bitset1 U bitset2" (b1, b2)
  [
    | l |

    l = string_length(b1);
    while ((l = l - 1) >= 0) b1[l] = b1[l] | b2[l];
    b1
  ];

bintersection! = fn "bitset1 bitset2 -> bitset1. bitset1 = bitset1 /\ bitset2" (b1, b2)
  [
    | l |

    l = string_length(b1);
    while ((l = l - 1) >= 0) b1[l] = b1[l] & b2[l];
    b1
  ];

bdifference! = fn "bitset1 bitset2 -> bitset1. bitset1 = bitset1 - bitset2" (b1, b2)
  [
    | l |

    l = string_length(b1);
    while ((l = l - 1) >= 0) b1[l] = b1[l] & ~b2[l];
    b1
  ];

bassign! = fn "bitset1 bitset2 -> bitset1. bitset1 = bitset2" (b1, b2)
  [
    | l |

    l = string_length(b1);
    while ((l = l - 1) >= 0) b1[l] = b2[l];
    b1
  ];

bitset_in? = fn "bitset1 bitset2 -> b. True if bitset1 is a subset of bitset2" (b1, b2)
  [
    | l |

    l = string_length(b1);
    loop
      [
	if ((l = l - 1) < 0) exit true;
	if (b1[l] & ~b2[l]) exit false;
      ]
  ];

bitset_eq? = fn "bitset1 bitset2 -> b. True if bitset1 == bitset2" (b1, b2)
  string_cmp(b1, b2) == 0;

bempty? = fn "bitset -> b. True if bitset has all bits clear" (b)
  [
    | l |

    l = string_length(b);
    loop
      [
	if ((l = l - 1) < 0) exit true;
	if (b[l]) exit false;
      ]
  ];
