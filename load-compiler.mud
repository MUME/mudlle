[
  | fload |

  // Bootstrap via interpreter (load minimum for linker)
  basic_load("mudlle/compiler/compiler.mud", "compiler/compiler.mud", LVL_IMPLEMENTOR);
  basic_load("mudlle/compiler/misc.mud", "compiler/misc.mud", LVL_IMPLEMENTOR);
  basic_load("mudlle/compiler/sequences.mud", "compiler/sequences.mud", LVL_IMPLEMENTOR);
  basic_load("mudlle/compiler/link.mud", "compiler/link.mud", LVL_IMPLEMENTOR);

  fload = fn (s) catch_error(fn () mc:link(load_data(s), LVL_IMPLEMENTOR)());

  // Load compiled compiler
  fload("compiler.obj");
  fload("misc.obj");
  fload("sequences.obj");
  fload("link.obj");
  fload("dlist.obj");
  fload("graph.obj");
  fload("asparc.obj");
  fload("vars.obj");
  fload("flow.obj");
  fload("optimise.obj");
  fload("ins3.obj");
  fload("msparc.obj");
  fload("phase1.obj");
  fload("phase2.obj");
  fload("phase3.obj");
  fload("phase4.obj");
  fload("gensparc.obj");
  fload("sparc.obj");
];

