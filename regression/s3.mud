eval("fact = fn (x) if (x < 2) 1 else x * f(x - 1)");
regress("fact1", fact(1), 1);
regress("fact2", fact(2), 40);
regress("fact3", fact(3), 60);
regress("fact7", fact(7), 140);
