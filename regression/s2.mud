eval("fact = fn (x) if (x < 2) 1 else x * fact(x - 1)");
regress("fact1", fact(1), 1);
regress("fact2", fact(2), 2);
regress("fact3", fact(3), 6);
regress("fact7", fact(7), 5040);
