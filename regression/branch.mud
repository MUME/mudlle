vdummy = false;
regress1arg("bfalse1", true, "if (vdummy || arg1) 33 else 22", 33);
regress1arg("bfalse2", false, "if (vdummy || arg1) 33 else 22", 22);
regress1arg("btrue1", true, "if (vdummy || !arg1) 33 else 22", 22);
regress1arg("btrue2", false, "if (vdummy || !arg1) 33 else 22", 33);

regress2branch = fn (name, a1, a2, op, exptruth)
  [
    regress2args("b1" + name, a1, a2,
		 format("if (vdummy || (%s)) 33 else 22", op),
		 if (exptruth) 33 else 22);
    regress2args("b2" + name, a1, a2,
		 format("if ((%s) || vdummy) 33 else 22", op),
		 if (exptruth) 33 else 22);
  ];
					   
regress2branch("or1", 7, "fun", "arg1 or arg2", 7 or "fun");
regress2branch("or2", false, false, "arg1 or arg2", false);
regress2branch("or3", false, null, "arg1 or arg2", true);
regress2branch("or4", '(1 2), false, "arg1 or arg2", true);

regress2branch("and1", 7, "fun", "arg1 and arg2", 7 and "fun");
regress2branch("and2", false, false, "arg1 and arg2", false);
regress2branch("and3", false, null, "arg1 and arg2", false);
regress2branch("and4", '(1 2), false, "arg1 and arg2", false);
regress2branch("and5", true, true, "arg1 and arg2", true);

regress2branch("==0", true, true, "arg1 == arg2", true);
regress2branch("==1", true, '(1 2), "arg1 == arg2", false);
regress2branch("!=0", "fun", '(1 2), "arg1 != arg2", true);
regress2branch("!=1", 23, 23, "arg1 != arg2", false);

regress2branch("<0", 6, 6, "arg1 < arg2", 6 < 6);
regress2branch("<1", 6, 7, "arg1 < arg2", 6 < 7);
regress2branch("<2", 6, 765, "arg1 < arg2", 6 < 765);
regress2branch("<3", 7, 6, "arg1 < arg2", 7 < 6);
regress2branch("<4", 7, -1, "arg1 < arg2", 7 < -1);
regress2branch("<5", -10, -1, "arg1 < arg2", -10 < -1);

regress2branch(">0", 6, 6, "arg1 > arg2", 6 > 6);
regress2branch(">1", 6, 7, "arg1 > arg2", 6 > 7);
regress2branch(">2", 6, 765, "arg1 > arg2", 6 > 765);
regress2branch(">3", 7, 6, "arg1 > arg2", 7 > 6);
regress2branch(">4", 7, -1, "arg1 > arg2", 7 > -1);
regress2branch(">5", -10, -1, "arg1 > arg2", -10 > -1);

regress2branch("<=0", 6, 6, "arg1 <= arg2", 6 <= 6);
regress2branch("<=1", 6, 7, "arg1 <= arg2", 6 <= 7);
regress2branch("<=2", 6, 765, "arg1 <= arg2", 6 <= 765);
regress2branch("<=3", 7, 6, "arg1 <= arg2", 7 <= 6);
regress2branch("<=4", 7, -1, "arg1 <= arg2", 7 <= -1);
regress2branch("<=5", -10, -1, "arg1 <= arg2", -10 <= -1);

regress2branch(">=0", 6, 6, "arg1 >= arg2", 6 >= 6);
regress2branch(">=1", 6, 7, "arg1 >= arg2", 6 >= 7);
regress2branch(">=2", 6, 765, "arg1 >= arg2", 6 >= 765);
regress2branch(">=3", 7, 6, "arg1 >= arg2", 7 >= 6);
regress2branch(">=4", 7, -1, "arg1 >= arg2", 7 >= -1);
regress2branch(">=5", -10, -1, "arg1 >= arg2", -10 >= -1);

