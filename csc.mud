// Bootstrap via interpreter (load minimum for linker)
garbage_collect(700000);
load("lib.mud");
load("link.mud");
load("interface.mud");

load_library = fn (s) [ display(format("loading %s", s)); newline(); fload(s + ".obj") ];
// reload compiled modules
fload("dlist.obj");
fload("sequences.obj");
fload("misc.obj");
fload("vars.obj");
fload("compiler.obj");
fload("link.obj");

fload("noinf.obj");
fload("sparc.obj");
fload("compile.obj");
mc:verbose = 0;
