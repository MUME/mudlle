eval = fn (s)
  [
    | prelinked |

    if (prelinked = mc:compile(mudlle_parse(s), false))
      mc:linkrun(prelinked, 1, true)
    else
      false
  ];

lcompile = fn (s, protect)
  [
    | objname, dot, prelinked |

    dot = string_index(s, ?.);
    if (dot < 0) objname = s + ".obj"
    else objname = substring(s, 0, dot) + ".obj";

    display(format("compiling %s", s)); newline();
    if (prelinked = mc:compile(mudlle_parse_file(s, s), protect))
      save_data(objname, prelinked)
  ];

fcompile = fn (s) lcompile(s, false);
pcompile = fn (s) lcompile(s, true);
fload = fn (s) mc:linkrun(load_data(s), 1, true);
test = fn (s) mc:compile(mudlle_parse(s), false);
ftest = fn (s) mc:compile(mudlle_parse_file(s, s), false);
