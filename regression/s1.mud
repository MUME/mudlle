eval("f = fn (x) if (x < 10) 20 else 15");
regress("if1", f(9), 20);
regress("if2", f(10), 15);
regress("if3", f(11), 15);
regress("if4", f(0), 20);
regress("if5", f(-20), 20);
regress("if6", f(20), 15);
