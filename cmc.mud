// Bootstrap via interpreter (load minimum for linker)
garbage_collect(300000);
load("lib.mud");
load("link.mud");
load("interface.mud");

load_library = fn (s) [ display(format("loading %s", s)); newline(); fload(s + ".obj") ];
// reload compiled modules
fload("compiler.obj");
fload("vars.obj");
fload("sequences.obj");
fload("dlist.obj");
fload("misc.obj");
fload("link.obj");

fload("68k.obj");
fload("compile.obj");
load("noinf.mud");
