eval("f1 = fn (x, y) fn () x + y");
regress("closure1", f1(2,3)(), 5);

eval("f2 = fn (x) (fn () x = x + 1)");
f3 = f2(44);
regress("closure2a", f3(), 45);
regress("closure2b", f3(), 46);

regressqfail("wglobal", "lmap = null");

vqsort!(fn (s1, s2) string_cmp(s1, s2) < 0, vcopy('["bb" "aa" "cc"]));

regresseval("maxint1", "maxint", maxint);
regresseval("maxint2", "id(maxint)", maxint);
regresseval("minint1", "(fn (n) n == minint)(0)", 0);

eval("[ | y | x = fn () y; y = 5 ]");
regress("closurevars", x(), 5);


