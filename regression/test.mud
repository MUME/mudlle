/* Execute line from other window
(fset 'execline
   [?\C-x ?o ?\C-  ?\C-n ?\M-w ?\C-x ?o ?\C-y backspace backspace return])
(global-set-key "\M-[" 'execline)
*/

regress = fn (name, x, expected)
  display(format("%s: got %s, expected %s: %s%n",
		 name, x, expected,
		 if (equal?(x, expected)) "passed" else "FAILED"));

unparse = fn (x)
  [
    | sp |

    sp = make_string_oport();
    with_output(sp, fn () write(x));
    port_string(sp)
  ];

eval("id = fn (x) x");

regresseval = fn (name, x, expected)
  [
    | val |

    resvar1 = 44;
    resvar2 = 445385798 . 23;
    resvar3 = "fufweoin";
    eval("resvar1 = (" + x + ")");
    eval("resvar2 = id(" + x + ")");
    eval("resvar3 = [ | tmp | tmp = " + x + "; dummy(); tmp]");
    display(format("%sv1: got %s, expected %s: %s%n",
		   name, resvar1, expected,
		   if (equal?(resvar1, expected)) "passed" else "FAILED"));
    display(format("%sv2: got %s, expected %s: %s%n",
		   name, resvar2, expected,
		   if (equal?(resvar2, expected)) "passed" else "FAILED"));
    display(format("%sv3: got %s, expected %s: %s%n",
		   name, resvar3, expected,
		   if (equal?(resvar3, expected)) "passed" else "FAILED"));
  ];

regressfail = fn (name, failingfn)
  handle_error(fn () [ failingfn();
		       display(format("%s FAILED to fail%n", name));
		     ],
	       fn (err) display(format("%s passed%n", name)));

regresspass = fn (name, failingfn)
  handle_error(fn () [ failingfn();
		       display(format("%s passed%n", name));
		     ],
	       fn (err) display(format("%s FAILED%n", name)));

regressqfail = fn (name, failstr)
  [
    eval("failfn = fn () (" + failstr + ")");
    regressfail(name, failfn);
  ];

regressqpass = fn (name, failstr)
  [
    eval("failfn = fn () (" + failstr + ")");
    regresspass(name, failfn);
  ];

eval("dummy = fn () 1");
wrong = 0;
regresslocal = fn (name, arg, x, expected)
  [
    regresseval(name+"_l1", format("[ | li | li = %s; if (wrong) li = 0; dummy(); %s ]", unparse(arg), x), expected);
    regresseval(name+"_l2", format("[ | li | li = %s; if (wrong) li = 0; %s ]", unparse(arg), x), expected);
  ];

regress2args = fn (name, a1, a2, x, expected)
  [
    arg1 = a1; arg2 = a2;
    regresseval(name + "_gg", // global global
		x, expected);
    regresseval(name + "_gl1", // global local1
		format("[ | arg2 | arg2 = %s; if (wrong) arg2 = 0; %s ]",
		       unparse(a2), x),
		expected);
    regresseval(name + "_gl2", // global local2
		format("[ | arg2 | arg2 = %s; if (wrong) arg2 = 0; dummy(); %s ]",
		       unparse(a2), x),
		expected);
    regresseval(name + "_l1g", // local1 global
		format("[ | arg1 | arg1 = %s; if (wrong) arg1 = 0; %s ]",
		       unparse(a1), x),
		expected);
    regresseval(name + "_l1l1", // local1 local1
		format("[ | arg1, arg2 | arg1 = %s; arg2 = %s; if (wrong) arg1 = arg2 = 0; %s ]",
		       unparse(a1), unparse(a2), x),
		expected);
    regresseval(name + "_l1l2", // local1 local2
		format("[ | arg1, arg2 | arg1 = %s; dummy(); arg2 = %s; if (wrong) arg1 = arg2 = 0; %s ]",
		       unparse(a1), unparse(a2), x),
		expected);
    regresseval(name + "_l2g", // local2 global
		format("[ | arg1 | arg1 = %s; if (wrong) arg1 = 0; dummy(); %s ]",
		       unparse(a1), x),
		expected);
    regresseval(name + "_l2l1", // local2 local1
		format("[ | arg1, arg2 | arg2 = %s; dummy(); arg1 = %s; if (wrong) arg1 = arg2 = 0; %s ]",
		       unparse(a2), unparse(a1), x),
		expected);
    regresseval(name + "_l2l2", // local2 local2
		format("[ | arg1, arg2 | arg1 = %s; arg2 = %s; dummy(); if (wrong) arg1 = arg2 = 0; %s ]",
		       unparse(a1), unparse(a2), x),
		expected);
  ];

regress1arg = fn (name, a1, x, expected)
  [
    | s1, v1 |

    if (pair?(a1) && !pair?(cdr(a1)))
      [
	s1 = cdr(a1); v1 = car(a1);
      ]
    else
      [
	s1 = unparse(a1); v1 = a1;
      ];
    arg1 = v1; 
    regresseval(name + "_g", // global
		x, expected);
    regresseval(name + "_l1", // local1
		format("[ | arg1 | arg1 = %s; if (wrong) arg1 = 0; %s ]",
		       s1, x),
		expected);
    regresseval(name + "_l2", // local2
		format("[ | arg1 | arg1 = %s; if (wrong) arg1 = 0; dummy(); %s ]",
		       s1, x),
		expected);
  ];

