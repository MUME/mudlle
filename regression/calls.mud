// calls: 0, 1, 2, 3 args
//   mcode to:
//     errors (int, null, badtype)
//     stack
//     closure
//     closure (varargs)
//     closure (varargs with indirection)
//     closure (badargs)
//     C primitive (unknown)
//     C primitive (badargs)
//     C primitive (known)
//     C primitive (leaf)
//     C primitive (leaf noalloc)
//     C secure (unknown)
//     C secure (badargs)
//     C secure (known)
//     C varargs (unknown)
//     C varargs (known)

//   C to:
//     closure
//     closure (varargs)
//     closure (badargs)

//   Stack to:
//     closure
//     closure (varargs)
//     closure (badargs)

// Errors
regressqfail("callnull", "undefined_fn()");
intfn = 7; regressqfail("callint", "intfn()");
strfn = "fun"; regressqfail("callstr", "strfn(23, 77)");
regressqfail("callclosure_badargs1", "id()");
regressqfail("callclosure_badargs2", "id(2,'(1))");
regressqfail("callprim_badargs1", "string_length()");
regressqfail("callprim_badargs2", "string_length(1,99,888,-1)");
regressqfail("callprim_badtype", "string_length(1)");
regressqfail("callsec_badargs", "remove(1,99,888,-1)");
regressqfail("callsec_badtype", "remove(1)");
regressfail("stkclosure_badargs", fn () id());
regressfail("cclosure_badargs", fn () handle_error(id, dummy));

// from mcode
stkfn = fn (a1, a2) a1 + a2;
regresseval("callstk", "stkfn(22, 99)", 121);
eval("cfn = fn (a1, a2) a1 + a2");
regresseval("callclosure", "cfn(22, 99)", 121);
eval("vfn = fn v v");
regresseval("callvarargs0", "vfn()", '[]);
regresseval("callvarargs1", "vfn(22)", '[22]);
regresseval("callvarargs2", "vfn(22, 99)", '[22 99]);
regresseval("callvarargs3", "vfn('(1), 22, 99)", '[(1) 22 99]);
eval("vfn2 = fn v [ fn (newv) [ | oldv | oldv = v; v = newv; oldv ] ]");
eval("vfn3 = vfn2(10, 99)");
regress("callvarargs4", vfn3(9), '[10 99]);
regress("callvarargs0", vfn3(99), 9);
cprim1 = string_length;
cprim2 = string_fill!;
s = make_string(3);
regresseval("callprim1", "cprim1(\"yuck\")", 4);
regresseval("callprim2", "[ cprim2(s, 97); s ]", "aaa");
regresseval("callleafnoalloc", "[ string_fill!(s, 98); s]", "bbb");
regresseval("callleaf", "string_length(make_string(10))", 10);
regresseval("callknown", "apply(id, '[22])", 22);
cprim3 = unlimited_execution;
regressqfail("callsec1", "cprim3()");
regressqpass("callsec2", "unlimited_execution()");
cprim4 = sequence;
regresseval("callvar1", "cprim4(22, 33)", '[22 33]);
regresseval("callvar2", "sequence(22, 33)", '[22 33]);

