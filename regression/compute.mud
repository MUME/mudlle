// for all ops, all combinations of kinds of args
// kinds: constant / arg / global / temp-result
// (arg is like local var, but is always spilled on x86...)
[
  eval("dummy = fn () 1");

  regressunary =
    fn (lv, gv)
      fn (op, fop)
	[
	  gi = gv; regresseval(op + "_g", op + "(gi)", fop(gi));
	  regresslocal(op, lv, op + "(li)", fop(lv));
	];

  regressaunary = regressunary(33, 22);

  regressaunary("~", fn (x) ~x);
  regressaunary("-", fn (x) -x);
  regressaunary("!", fn (x) !x);
  regressunary(0, 0)("!", fn (x) !x);
  regressunary("fun", null)("!", fn (x) !x);

  regress1arg("car", '(20 34), "car(arg1)", 20);
  regress1arg("cdr", '(20 34), "cdr(arg1)", '(34));
  regressunary("feroh", "fhritgfriugf")("string_length", fn (x) string_length(x));
  regressunary('[], '[ 1 3 77])("vector_length", fn (x) vector_length(x));
  regressunary('[99], '[])("vector_length", fn (x) vector_length(x));

  gi = 41;
  regresseval("*", "gi * 3", gi * 3);
  regresseval("/", "gi / 3", gi / 3);
  regresseval("%", "gi % 3", gi % 3);
  eval("gf = fn (x) gi/x"); regressfail("/0", fn () gf(0));
  eval("gf = fn (x) gi%x"); regressfail("%0", fn () gf(0));
  gi = 2;
  regresseval("<<b", "12 << gi", 12 << gi);
  regresseval(">>b", "12 >> gi", 12 >> gi);

  gi = 23; gf = 12;
  regresseval("+b", "gi + gf", gi + gf);
  gi = "fun"; gf = "ick";
  regresseval("+s", "gi + gf", gi + gf);

  gi = '[12 "fun" -3];
  regresseval("vref0", "gi[0]", gi[0]);
  regresseval("vref1", "gi[1]", gi[1]);
  regresseval("vref2", "gi[2]", gi[2]);
  regressqfail("vref-1", "gi[-1]");
  regressqfail("vref3", "gi[3]");
  regressqfail("vref221", "gi[221]");

  gi = "blurb";
  regresseval("sref0", "gi[0]", gi[0]);
  regresseval("sref1", "gi[1]", gi[1]);
  regresseval("sref3", "gi[3]", gi[3]);
  regressqfail("sref-1", "gi[-1]");
  regressqfail("sref5", "gi[5]");
  regressqfail("sref221", "gi[221]");
  gi = "";
  regressqfail("sref0f", "gi[0]");

  gi = 22;
  regressqfail("reff0", "gi[0]");
  gi = 1 . 2;
  regressqfail("reff1", "gi[0]");
  gi = make_table(); gi["fun"] = 99;
  regresseval("tref0", "gi[\"fun\"]", gi["fun"]);
  regresseval("tref0", "gi[\"fund\"]", gi["fund"]);

  gi = 12; gj = 22;
  regresseval("cons0", "gi . gj", gi . gj);
  // TBD: a cons causing GC

  regress2args("or1", 7, "fun", "arg1 or arg2", 7 or "fun");
  regress2args("or2", false, false, "arg1 or arg2", false);
  regress2args("or3", false, null, "arg1 or arg2", true);
  regress2args("or4", '(1 2), false, "arg1 or arg2", true);
  regress1arg("or5", '(1 2), "arg1 or false", true);
  regress1arg("or6", false, "arg1 or false", false);

  regress2args("and1", 7, "fun", "arg1 and arg2", 7 and "fun");
  regress2args("and2", false, false, "arg1 and arg2", false);
  regress2args("and3", false, null, "arg1 and arg2", false);
  regress2args("and4", '(1 2), false, "arg1 and arg2", false);
  regress2args("and5", true, true, "arg1 and arg2", true);
  regress1arg("and6", '(1 2), "arg1 and true", true);
  regress1arg("and7", false, "arg1 and true", false);

  regress2args("==0", true, true, "arg1 == arg2", true);
  regress2args("==1", true, '(1 2), "arg1 == arg2", false);
  regress2args("!=0", "fun", '(1 2), "arg1 != arg2", true);
  regress2args("!=1", 23, 23, "arg1 != arg2", false);

  regress2args("<0", 6, 6, "arg1 < arg2", 6 < 6);
  regress2args("<1", 6, 7, "arg1 < arg2", 6 < 7);
  regress2args("<2", 6, 765, "arg1 < arg2", 6 < 765);
  regress2args("<3", 7, 6, "arg1 < arg2", 7 < 6);
  regress2args("<4", 7, -1, "arg1 < arg2", 7 < -1);
  regress2args("<5", -10, -1, "arg1 < arg2", -10 < -1);

  regress2args(">0", 6, 6, "arg1 > arg2", 6 > 6);
  regress2args(">1", 6, 7, "arg1 > arg2", 6 > 7);
  regress2args(">2", 6, 765, "arg1 > arg2", 6 > 765);
  regress2args(">3", 7, 6, "arg1 > arg2", 7 > 6);
  regress2args(">4", 7, -1, "arg1 > arg2", 7 > -1);
  regress2args(">5", -10, -1, "arg1 > arg2", -10 > -1);

  regress2args("<=0", 6, 6, "arg1 <= arg2", 6 <= 6);
  regress2args("<=1", 6, 7, "arg1 <= arg2", 6 <= 7);
  regress2args("<=2", 6, 765, "arg1 <= arg2", 6 <= 765);
  regress2args("<=3", 7, 6, "arg1 <= arg2", 7 <= 6);
  regress2args("<=4", 7, -1, "arg1 <= arg2", 7 <= -1);
  regress2args("<=5", -10, -1, "arg1 <= arg2", -10 <= -1);

  regress2args(">=0", 6, 6, "arg1 >= arg2", 6 >= 6);
  regress2args(">=1", 6, 7, "arg1 >= arg2", 6 >= 7);
  regress2args(">=2", 6, 765, "arg1 >= arg2", 6 >= 765);
  regress2args(">=3", 7, 6, "arg1 >= arg2", 7 >= 6);
  regress2args(">=4", 7, -1, "arg1 >= arg2", 7 >= -1);
  regress2args(">=5", -10, -1, "arg1 >= arg2", -10 >= -1);

  regress2args("|0", 7, 12, "arg1 | arg2", 7 | 12);
  regress2args("|1", 333, 0, "arg1 | arg2", 333 | 0);

  regress2args("&0", 7, 12, "arg1 & arg2", 7 & 12);
  regress2args("&1", 333, 0, "arg1 & arg2", 333 & 0);

  regress2args("^0", 7, 12, "arg1 ^ arg2", 7 ^ 12);
  regress2args("^1", 333, 0, "arg1 ^ arg2", 333 ^ 0);
  gi = 7;
  regresseval("^2", "gi ^ 12", gi ^ 12);
  regresseval("^3", "88 ^ gi", 88 ^ gi);

  regresseval("<<1", "gi << 1", gi << 1);
  regresseval("<<3", "gi << 3", gi << 3);
  regresseval(">>1", "gi >> 1", gi >> 1);
  regresseval(">>3", "gi >> 3", gi >> 3);
  gi=1200;
  regresseval(">>4", "gi >> 2", gi >> 2);
  gi=1200;
  regresseval("<<4", "gi << 2", gi << 2);
  gi = -1;
  regresseval(">>5", "gi >> 2", gi >> 2);


  // requires inference.mud to do proper tests...
  regress2args("+0", 7, 12, "arg1 + arg2", 19);
  regresseval("+1", "gi + 12", gi + 12);
  regresseval("+2", "112 + gi", 112 + gi);

  regress2args("-0", 7, 12, "arg1 - arg2", -5);
  regresseval("-1", "gi - 12", gi - 12);
  regresseval("-2", "112 - gi", 112 - gi);
];
