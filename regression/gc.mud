eval("gcfn = fn (f, n) [ while (n = n - 1) f() ]");

eval("vfn = fn v v");
eval("gcfn((fn () vfn(10, 22)), 100000)");

eval("cfn = fn (x) fn () x");
eval("gcfn((fn () cfn(\"mad\")), 100000)");

eval("cvfn = fn (x) (fn (y) x = y)");
eval("gcfn((fn () cvfn('(1 2 3))), 100000)");

eval("consfn = fn (x) x . 1");
eval("gcfn((fn () consfn('(1 2 3))), 100000)");

eval("gcfn((fn () vector(7, 99, 544, 23, \"fun\", '(1 2 3))), 100000)");
cprim4 = vector;
eval("gcfn((fn () cprim4(7, 99, 544, 23, \"fun\", '(1 2 3))), 100000)");

eval("gcfn(fn () lappend('(1 2), '(3 4)), 100000)");

eval("gcfn(fn () make_string(10), 1000000)");
eval("gcfn(fn () ggg = make_string(10), 100000)");

gi = 999;
eval("[ catch_error((fn () [ garbage_collect(0); 1/0 ]), true); gi ]");
