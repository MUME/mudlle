vdummy = false;

regress1branch = fn (name, a1, op, exptruth)
  [
    regress1arg("b1" + name, a1,
		format("if (vdummy || (%s)) 33 else 22", op),
		if (exptruth) 33 else 22);
    regress1arg("b2" + name, a1,
		format("if ((%s) || vdummy) 33 else 22", op),
		if (exptruth) 33 else 22);
  ];
					   

regress1branch("integer?1", 7, "integer?(arg1)", true);
regress1branch("integer?2", null, "integer?(arg1)", false);
regress1branch("integer?3", "fun", "integer?(arg1)", false);
regress1branch("integer?4", 10, "integer?(arg1 + 1)", true);
regress1branch("integer?5", 10, "integer?(arg1 . 1)", false);

regress1branch("null?1", 7, "null?(arg1)", false);
regress1branch("null?2", null, "null?(arg1)", true);
regress1branch("null?3", "fun", "null?(arg1)", false);

regress1branch("string?1", 7, "string?(arg1)", false);
regress1branch("string?2", null, "string?(arg1)", false);
regress1branch("string?3", "fun", "string?(arg1)", true);

regress1branch("list?1", 7, "list?(arg1)", false);
regress1branch("list?2", null, "list?(arg1)", true);
regress1branch("list?3", "fun", "list?(arg1)", false);
regress1branch("list?4", '(1 2), "list?(arg1)", true);

regress1branch("function?1", 7, "function?(arg1)", false);
regress1branch("function?2", null, "function?(arg1)", false);
regress1branch("function?3", "fun", "function?(arg1)", false);
regress1branch("function?4", (fn () 1) . "fn () 1", "function?(arg1)", true);
regress1branch("function?5", remove . "remove", "function?(arg1)", true);
regress1branch("function?6", sequence . "sequence", "function?(arg1)", true);
regress1branch("function?7", string_length . "string_length", "function?(arg1)", true);

regresstypetrap = fn (name, a1, type, passes)
  [
    eval(format("trapfn = fn (%s arg) 0", type));
    if (passes)
      regresspass(name, fn () trapfn(a1))
    else
      regressfail(name, fn () trapfn(a1))
  ];

regresstypetrap("integer1", 7, "int", true);
regresstypetrap("integer2", null, "int", false);
regresstypetrap("integer3", "fun", "int", false);

regresstypetrap("null1", 7, "null", false);
regresstypetrap("null2", null, "null", true);
regresstypetrap("null3", "fun", "null", false);

regresstypetrap("string1", 7, "string", false);
regresstypetrap("string2", null, "string", false);
regresstypetrap("string3", "fun", "string", true);

regresstypetrap("list1", 7, "list", false);
regresstypetrap("list2", null, "list", true);
regresstypetrap("list3", "fun", "list", false);
regresstypetrap("list4", '(1 2), "list", true);

regresstypetrap("function1", 7, "function", false);
regresstypetrap("function2", null, "function", false);
regresstypetrap("function3", "fun", "function", false);
regresstypetrap("function4", (fn () 1), "function", true);
regresstypetrap("function5", remove, "function", true);
regresstypetrap("function6", sequence, "function", true);
regresstypetrap("function7", string_length, "function", true);

