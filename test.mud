rfact2 = fn (x) if (x > 1) x * rfact2(x - 1) else 1;
rfact = fn (int x) if (x > 1) x * rfact(x - 1) else 1;
ifact = fn (int x)
  [
    | r |
    r = 1;
    while (x > 1)
      [
	r = r * x;
	x = x - 1;
      ];
    r
  ];

test = fn (f, x, n)
  [
    | s |

    s = ctime();
    while ((n = n - 1) >= 0) f(x);
    ctime() - s
  ];

/*
Times (A2500/30, 25mhz 68030):
heap: 1.2M

unoptimised interpreted ifact(10000): 39200
unoptimised interpreted rfact(10000): 55200
compiled ifact(10000): 1360
compiled rfact(10000): 3160
compiled inferred ifact(10000): 1040
compiled inferred rfact(10000): 2800
compiled inferred+decl rfact(10000): 2060
compiled inferred+decl ifact(10000): 980

*/

ll = list(1, 6, 7, 9, 12, 8, 999, -1);

test1 = fn (x) lmap(fn (a) a + 1, ll);
test2 = fn (x) lforeach(fn (a) a + 1, ll);
test3 = fn (x) mlmap(fn (a) a + 1, ll);
test4 = fn (x) mlforeach(fn (a) a + 1, ll);
test5 = fn (x) lexists?(fn (a) a > 10, ll);
test6 = fn (x) mlexists?(fn (a) a > 10, ll);
test7 = fn (x) memq(x, ll);
test8 = fn (x) mmemq(x, ll);


/* Sparc: 2.1M heap
test(ifact, 12, 1000000): 7.5s
test(rfact, 12, 100000): 4.3s

inferred: ifact: 6.8s
	  rfact: 4.0s

100000 iterations:
test1: 4.0s
test2: 2.0s
test3: 1.8s
test4: 0.7s
*/

mlmap = fn (f, l)
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
      new = last;
      l = cdr(l);
    ];
  first
];

mlforeach = fn (f, l)
while (l != null)
[
  f(car(l));
  l = cdr(l);
];

mlexists? = fn "fn l -> x. Returns first element x of l for which fn(x) is true, false if none found" (f, l)
  loop
    [
      | x |

      if (l == null) exit false
      else if (f(x = car(l))) exit x
      else l = cdr(l);
    ];

mmemq = fn (x, l)
  loop
    if (l == null) exit false
    else if (x == car(l)) exit l
    else l = cdr(l);
