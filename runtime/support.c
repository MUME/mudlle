/* $Log: support.c,v $
 * Revision 1.15  1995/07/15  15:25:08  arda
 * Context cleanup.
 * Remove GCDEBUG.
 *
 * Revision 1.14  1995/06/04  14:24:41  arda
 * Rename/move some files, misc. junk
 *
 * Revision 1.13  1995/01/22  15:12:00  arda
 * Linux patches.
 *
 * Revision 1.12  1994/10/09  06:44:22  arda
 * Libraries
 * Type inference
 * Many minor improvements
 *
 * Revision 1.11  1994/09/06  07:50:47  arda
 * Constant support: detect_immutability, global_set!, string_{i}search.
 *
 * Revision 1.10  1994/08/31  16:57:36  arda
 * *** empty log message ***
 *
 * Revision 1.9  1994/08/29  15:44:54  arda
 * Mudlle stuff
 *
 * Revision 1.8  1994/08/29  13:19:50  arda
 * Contagious immutability.
 * Global array of values instead of variables.
 * Direct recursion.
 *
 * Revision 1.7  1994/08/26  04:36:05  arda
 * hidden objects
 *
 * Revision 1.6  1994/08/22  18:03:39  arda
 * Primitives for compiler.
 *
 * Revision 1.5  1994/08/22  11:19:06  arda
 * Changes for mudlle compiler in MUME.
 *
 * Revision 1.4  1994/08/17  16:30:11  arda
 * Seclevel fixes.
 *
 * Revision 1.3  1994/08/17  14:10:34  arda
 * Changed bcons to balloc_cons.
 *
 * Revision 1.2  1994/08/16  19:17:19  arda
 * Added flags to primitives for better calling sequences.
 *
*/

static char rcsid[] = "$Id: support.c,v 1.15 1995/07/15 15:25:08 arda Exp $";

#include <string.h>
#include <stddef.h>
#include <alloca.h>
#include "runtime/runtime.h"
#include "alloc.h"
#include "global.h"
#include "utils.h"
#include "mparser.h"
#include "lexer.h"
#include "calloc.h"
#include "builtins.h"
#include "module.h"

#define ALIGN(x, n) ((x) + ((n) - 1) & ~((n) - 1))

UNSAFEOP(mudlle_parse, "s -> v. Parses a mudlle expression and returns a parse tree",
	 1, (struct string *code),
	 OP_LEAF | OP_NOESCAPE)
{
  value parsed;

  TYPEIS(code, type_string);

  read_from_string(code->str);
  erred = FALSE;
  memory = new_block();
  if (yyparse() == 0 && !erred)
    parsed = mudlle_parse(parsed_code);
  else
    parsed = makebool(FALSE);

  free_block(memory);

  return parsed;
}

UNSAFEOP(mudlle_parse_file, "s1 s2 -> v. Parses a file s (nice name s2) and returns its parse tree",
	 2, (struct string *name, struct string *nicename),
	 OP_LEAF | OP_NOESCAPE)
{
  FILE *f;
  value parsed;
  char *fname;

  TYPEIS(name, type_string);
  TYPEIS(nicename, type_string);
  if (!(f = fopen(name->str, "r"))) return makebool(FALSE);

  fname = alloca(strlen(nicename->str) + 1);
  strcpy(fname, nicename->str);
  read_from_file(f, fname);

  erred = FALSE;
  memory = new_block();
  if (yyparse() == 0 && !erred)
    parsed = mudlle_parse(parsed_code);
  else
    parsed = makebool(FALSE);

  free_block(memory);
  fclose(f);

  return parsed;
}

/* The machine language builtin ops */

static struct builtin_table {
  const char *name;
  void (*address)(void);
} builtins[] = {
#ifdef AMIGA
  "bsubtract", bsubtract,
  "bor", bor,
  "band", band,
  "bleq", bleq,
  "blne", blne,
  "bllt", bllt,
  "blle", blle,
  "blgt", blgt,
  "blge", blge,
  "bbitor", bbitor,
  "bbitxor", bbitxor,
  "bbitand", bbitand,
  "bshift_left", bshift_left,
  "bshift_right", bshift_right,
  "badd", badd,
  "bmultiply", bmultiply,
  "bdivide", bdivide,
  "bremainder", bremainder,
  "bnegate", bnegate,
  "bnot", bnot,
  "bbitnot", bbitnot,
  "bref", bref,
  "bcar", bcar,
  "bcdr", bcdr,
  "bwglobal", bwglobal,
  "bbadargs", bbadargs,
  "bcompare", bcompare,
  "bcall", bcall,
  "balloc", balloc,
  "balloc_readonly", balloc_readonly,
  "bcons", bcons,
  "berror", berror,
  "bvarargs", bvarargs,
#endif
#ifdef sparc
  "bsubtract", bsubtract,
  "bor", bor,
  "band", band,
  "bleq", bleq,
  "blne", blne,
  "bllt", bllt,
  "blle", blle,
  "blgt", blgt,
  "blge", blge,
  "bbitor", bbitor,
  "bbitxor", bbitxor,
  "bbitand", bbitand,
  "bshift_left", bshift_left,
  "bshift_right", bshift_right,
  "badd", badd,
  "bmultiply", bmultiply,
  "bdivide", bdivide,
  "bremainder", bremainder,
  "bnegate", bnegate,
  "bnot", bnot,
  "bbitnot", bbitnot,
  "bref", bref,
  "bcar", bcar,
  "bcdr", bcdr,
  "bwglobal", bwglobal,
  "bcleargc", bcleargc,
  "bvarargs", bvarargs,
  "bcompare", bcompare,
  "bcall", bcall,
  "balloc_variable", balloc_variable,
  "balloc_cons", balloc_cons,
  "balloc_cons_l0", balloc_cons_l0,
  "balloc_closure", balloc_closure,
  "bcall_primitive", bcall_primitive,
  "bcall_primitive_leaf", bcall_primitive_leaf,
  "bcall_primitive_leaf_noalloc", bcall_primitive_leaf_noalloc,
#endif
#ifdef linux
  "bdummy", 0,
#endif
  };

static ulong builtin_find(struct string *name)
{
  char *n = name->str;
  int i = sizeof(builtins) / sizeof(struct builtin_table);

  while ((i = i - 1) >= 0)
    if (!strcmp(n, builtins[i].name)) return (ulong)builtins[i].address;

  return 0;
}

static ulong primitive_find(struct string *name)
{
  ulong n = mglobal_lookup(name);
  struct primitive *p = GVAR(n);

  if (!TYPE(p, type_primitive)) return 0;
  return (ulong)p->op->op;
}

static void set_cst(ulong *sethi, ulong val)
/* Effects: Sets value defined by a sethi/or pair to val.
     sethi points to the sethi instruction of the pair, the or can be found
     by a linear scan from there
   Modifies: sethi
*/
{
  ulong *or, ortemplate;

  /* Find the or that goes with the sethi */
  ortemplate = (2 << 30 | 2 << 19) | (*sethi & ~((1 << 25) - 1));
  or = sethi;
  while ((*++or & ~((1 << 19) - 1)) != ortemplate) ;

  *sethi = (*sethi & ~((1 << 22) - 1)) | (val >> 10);
  *or = (*or & ~((1 << 13) - 1)) | (val & ((1 << 10) - 1));
}
     
VAROP(link, "s1 n1 s2 s3 s4 n2 l1 l2 l3 l4 -> code. Builds a code object from:\n\
its machine code s1,\n\
security level n1, help string s2, varname s3, filename s4, lineno n2\n\
constants l1=list of constant/offset pairs\n\
builtins l2=list of name/offset pairs\n\
globals l3=list of name/offset pairs\n\
primitives l4=list of name/offset pairs",
      0)
{
  ulong clen, ncsts = 0, size;
  struct mcode *new;
  uword *cst_offsets;
  struct list *scan_csts, *scan_builtins, *scan_globals, *scan_primitives;
  struct gcpro gcpro1;
  struct string *mcode;
  value seclev, lineno;
  struct string *help, *varname, *filename;
  struct list *csts, *builtins, *globals, *primitives;

#ifdef MUME
  if (seclevel < LVL_IMPLEMENTOR) runtime_error(error_security_violation);
#endif

  if (nargs != 10) runtime_error(error_wrong_parameters);
  mcode = args->data[0];
  seclev = args->data[1];
  help = args->data[2];
  varname = args->data[3];
  filename = args->data[4];
  lineno = args->data[5];
  csts = args->data[6];
  builtins = args->data[7];
  globals = args->data[8];
  primitives = args->data[9];

  TYPEIS(mcode, type_string);
  ISINT(seclev);
  if (help) TYPEIS(help, type_string);
  if (varname) TYPEIS(varname, type_string);
  if (filename) TYPEIS(filename, type_string);
  ISINT(lineno);

  scan_csts = csts;
  while (scan_csts != NULL)
    {
      struct list *cst;

      TYPEIS(scan_csts, type_pair);
      cst = scan_csts->car;
      TYPEIS(cst, type_pair);
      ISINT(cst->cdr);
      scan_csts = scan_csts->cdr;

      ncsts++;
    }

  scan_builtins = builtins;
  while (scan_builtins != NULL)
    {
      struct list *builtin;

      TYPEIS(scan_builtins, type_pair);
      builtin = scan_builtins->car;
      TYPEIS(builtin, type_pair);
      TYPEIS(builtin->car, type_string);
      ISINT(builtin->cdr);
      scan_builtins = scan_builtins->cdr;
    }

  scan_globals = globals;
  while (scan_globals != NULL)
    {
      struct list *global;

      TYPEIS(scan_globals, type_pair);
      global = scan_globals->car;
      TYPEIS(global, type_pair);
      TYPEIS(global->car, type_string);
      ISINT(global->cdr);
      scan_globals = scan_globals->cdr;
    }

  scan_primitives = primitives;
  while (scan_primitives != NULL)
    {
      struct list *primitive;

      TYPEIS(scan_primitives, type_pair);
      primitive = scan_primitives->car;
      TYPEIS(primitive, type_pair);
      TYPEIS(primitive->car, type_string);
      ISINT(primitive->cdr);
      scan_primitives = scan_primitives->cdr;
    }

#ifdef AMIGA
  clen = string_len(mcode);
  GCPRO1(args);
  size = offsetof(struct mcode, mcode) + clen + ncsts * sizeof(uword);
  new = gc_allocate(size);
  UNGCPRO();
  mcode = args->data[0];
  help = args->data[2];
  varname = args->data[3];
  filename = args->data[4];
  csts = args->data[6];
  builtins = args->data[7];
  globals = args->data[8];

  new->o.size = size;
  new->o.garbage_type = garbage_mcode;
  new->o.type = type_mcode;
  new->o.flags = 0;
  new->nb_constants = ncsts;
  new->seclevel = intval(seclev);
  new->help = help;
  new->varname = varname;
  new->filename = filename;
  new->lineno = intval(lineno);
  new->code_length = clen;
  memcpy(new->magic, "\xff\xff\xff\xff\xff\xff\xff\xff", 8);

  memcpy(new->mcode, mcode->str, clen);

  /* Copy constants and their offsets */
  scan_csts = csts;
  cst_offsets = (uword *)((ubyte *)&new->mcode + ALIGN(clen, sizeof(uword)));
  while (scan_csts != NULL)
    {
      struct list *cst;
      uword offset;

      cst = scan_csts->car;
      offset = intval(cst->cdr);
      *(value *)(new->mcode + offset) = cst->car;
      *cst_offsets++ = offset;

      scan_csts = scan_csts->cdr;
    }

  /* Set builtin addresses */
  scan_builtins = builtins;
  while (scan_builtins != NULL)
    {
      struct list *builtin;
      uword offset;
      ulong baddress;

      builtin = scan_builtins->car;
      offset = intval(builtin->cdr);
      if (!(baddress = builtin_find(builtin->car))) runtime_error(error_bad_value);
      *(ulong *)(new->mcode + offset) = baddress;

      scan_builtins = scan_builtins->cdr;
    }

  /* Set global offsets */
  scan_globals = globals;
  while (scan_globals != NULL)
    {
      struct list *global;
      uword offset;
      ulong goffset;
      struct vector *genv;

      global = scan_globals->car;
      offset = intval(global->cdr);
      goffset = mglobal_lookup(global->car);
      genv = environment->values;
      *(uword *)(new->mcode + offset) =
	(ubyte *)&genv->data[goffset] - (ubyte *)genv;

      scan_globals = scan_globals->cdr;
    }

  /* Primitives not implemented on Amiga */

#endif
#ifdef sparc
  clen = string_len(mcode);
  GCPRO1(args);
  size = offsetof(struct mcode, mcode) + clen + ncsts * sizeof(uword);
  new = gc_allocate(size);
  UNGCPRO();
  mcode = args->data[0];
  help = args->data[2];
  varname = args->data[3];
  filename = args->data[4];
  csts = args->data[6];
  builtins = args->data[7];
  globals = args->data[8];
  primitives = args->data[9];

  new->o.size = size;
  new->o.garbage_type = garbage_mcode;
  new->o.type = type_mcode;
  new->o.flags = 0;
  new->nb_constants = ncsts;
  new->seclevel = intval(seclev);
  new->help = help;
  new->varname = varname;
  new->filename = filename;
  new->lineno = intval(lineno);
  new->code_length = clen >> 2;
  new->myself = (ubyte *)new;
  memcpy(new->magic, "\xff\xff\xff\xff\xff\xff\xff\xff", 8);

  memcpy(new->mcode, mcode->str, clen);

  /* Copy constants and their offsets */
  scan_csts = csts;
  cst_offsets = (uword *)((ubyte *)new->mcode + clen);
  while (scan_csts != NULL)
    {
      struct list *cst;
      uword offset;

      cst = scan_csts->car;
      offset = intval(cst->cdr);
      set_cst(new->mcode + offset, (ulong)cst->car);
      *cst_offsets++ = offset;

      scan_csts = scan_csts->cdr;
    }

  /* Set builtin addresses */
  scan_builtins = builtins;
  while (scan_builtins != NULL)
    {
      struct list *builtin;
      ulong baddress, *callins;

      builtin = scan_builtins->car;
      if (!(baddress = builtin_find(builtin->car))) runtime_error(error_bad_value);

      callins = new->mcode + intval(builtin->cdr);
      *callins = 1 << 30 | (baddress - (ulong)callins) >> 2;

      scan_builtins = scan_builtins->cdr;
    }

  /* Set global offsets */
  scan_globals = globals;
  while (scan_globals != NULL)
    {
      struct list *global;
      ulong goffset;
      struct vector *genv;

      global = scan_globals->car;
      goffset = mglobal_lookup(global->car);

      /* Compute byte offset from environment base */
      genv = environment->values;
      goffset = (ubyte *)&genv->data[goffset] - (ubyte *)genv;
      set_cst(new->mcode + intval(global->cdr), goffset);

      scan_globals = scan_globals->cdr;
    }

  /* Set primitive addresses */
  scan_primitives = primitives;
  while (scan_primitives != NULL)
    {
      struct list *primitive;
      ulong paddress;

      primitive = scan_primitives->car;
      if (!(paddress = primitive_find(primitive->car))) runtime_error(error_bad_value);

      set_cst(new->mcode + intval(primitive->cdr), paddress);

      scan_primitives = scan_primitives->cdr;
    }

#endif

#ifdef GCDEBUG
  gcstats.anb[type_mcode]++;
  gcstats.asizes[type_mcode] += size;
#endif

  new->o.flags |= OBJ_IMMUTABLE;

  return new;
}

UNSAFEOP(make_closure, "mcode -> fn. Makes a function with no closure vars from given mcode object",
	  1, (struct mcode *mcode),
	 OP_LEAF | OP_NOESCAPE)
{
  TYPEIS(mcode, type_mcode);

  return alloc_closure0((struct code *)mcode);
}

TYPEDOP(closurep, "x -> b. True if x is a closure",
	1, (value x),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(TYPE(x, type_closure));
}

TYPEDOP(primitivep, "x -> b. True if x is a primitive",
	1, (value x),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(TYPE(x, type_primitive));
}

TYPEDOP(primitive_nargs, "primitive -> b. Returns # of arguments of primitive",
	1, (struct primitive *p),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "f.n")
{
  TYPEIS(p, type_primitive);

  return makeint(p->op->nargs);
}

TYPEDOP(primitive_flags, "primitive -> n. Returns flags of primitive",
	1, (struct primitive *p),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "f.n")
{
  TYPEIS(p, type_primitive);

  return makeint(p->op->flags);
}

TYPEDOP(primitive_type, "primitive -> l. Returns type of primitive",
	1, (struct primitive *p),
	OP_LEAF | OP_NOESCAPE, "f.l")
{
  struct list *l = NULL;
  const char **typing;
  struct gcpro gcpro1;

  TYPEIS(p, type_primitive);

  typing = p->op->type;
  if (typing)
    {
      GCPRO1(l);
      while (*typing)
	{
	  struct string *sig = alloc_string(*typing++);

	  l = alloc_list(sig, l);
	}
      UNGCPRO();
    }
  return l;	
}

UNSAFEOP(global_table, " -> table. Returns global symbol table",
	 0, (void),
	 OP_LEAF | OP_NOALLOC | OP_NOESCAPE)
{
  return global;
}

TYPEDOP(global_lookup, "s -> n. Returns index of global variable s",
	1, (struct string *name),
	OP_LEAF | OP_NOESCAPE, "s.n")
{
  TYPEIS(name, type_string);

  return makeint(mglobal_lookup(name));
}

TYPEDOP(global_value, "n -> x. Returns value of global variable n",
	1, (value goffset),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "n.x")
{
  long n;

  ISINT(goffset); n = intval(goffset);
  if (n < 0 || n >= intval(environment->used)) runtime_error(error_bad_value);

  return GVAR(n);
}

UNSAFEOP(global_set, "n x -> . Sets global variable n to x. Fails if n is readonly",
	 2, (value goffset, value x),
	 OP_LEAF | OP_NOALLOC)
{
  long n;

  ISINT(goffset); n = intval(goffset);
  if (n < 0 || n >= intval(environment->used) || GCONSTANT(n))
    runtime_error(error_bad_value);

  GVAR(n) = x;
  undefined();
}

TYPEDOP(module_status, "s -> n. Returns status of module s",
	1, (struct string *name),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "s.n")
{
  TYPEIS(name, type_string);

  return makeint(module_status(name->str));
}

UNSAFEOP(module_set, "s n -> . Sets status of module s to n",
	 2, (struct string *name, value status),
	 OP_LEAF | OP_NOALLOC | OP_NOESCAPE)
{
  char *tname;

  TYPEIS(name, type_string);
  tname = alloca(strlen(name->str) + 1);
  strcpy(tname, name->str);
  ISINT(status);

  module_set(tname, intval(status));

  undefined();
}

UNSAFEOP(module_unload, "s -> b. Unload module s, false if protected",
	 1, (struct string *name),
	 OP_LEAF | OP_NOALLOC)
{
  TYPEIS(name, type_string);

  return makebool(module_unload(name->str));
}

TYPEDOP(module_require, "s -> n. Load module s if needed, return its new status",
	1, (struct string *name),
	0, "s.n")
{
  char *tname;

  TYPEIS(name, type_string);
  tname = alloca(strlen(name->str) + 1);
  strcpy(tname, name->str);

  return makeint(module_require(tname));
}

TYPEDOP(module_vstatus, "n -> s/n. Return status of global variable n",
	1, (value goffset),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "n.S")
{
  long n;
  struct string *mod;
  int status;

  ISINT(goffset); n = intval(goffset);
  if (n < 0 || n >= intval(environment->used)) runtime_error(error_bad_value);

  status = module_vstatus(n, &mod);
  if (status == var_module) return mod;
  else return makeint(status);
}

UNSAFEOP(module_vset, "n s/n -> . Sets status of global variable n",
	 2, (value goffset, value status),
	 OP_LEAF | OP_NOALLOC)
{
  long n;
  struct string *mod;

  ISINT(goffset); n = intval(goffset);
  if (n < 0 || n >= intval(environment->used)) runtime_error(error_bad_value);

  if (integerp(status))
    {
      if (status != makeint(var_normal) && status != makeint(var_write))
	runtime_error(error_bad_value);

      mod = NULL;
    }
  else
    {
      mod = status;
      TYPEIS(mod, type_string);
      status = makeint(var_module);
    }
  return makebool(module_vset(n, intval(status), mod));
}

UNSAFEOP(module_table, " -> table. Returns module status table",
	 0, (void),
	 OP_LEAF | OP_NOALLOC | OP_NOESCAPE)
{
  return modules;
}

void support_init(void)
{
  DEFINE("mudlle_parse", mudlle_parse);
  DEFINE("mudlle_parse_file", mudlle_parse_file);
  DEFINE("link", link);
  DEFINE("make_closure", make_closure);
  DEFINE("closure?", closurep);
  DEFINE("primitive?", primitivep);
  DEFINE("primitive_nargs", primitive_nargs);
  DEFINE("primitive_flags", primitive_flags);
  DEFINE("primitive_type", primitive_type);
  DEFINE("global_table", global_table);
  DEFINE("global_lookup", global_lookup);
  DEFINE("global_value", global_value);
  DEFINE("global_set!", global_set);

  /* The mudlle types */
  system_define("type_code", makeint(type_code));
  system_define("type_closure", makeint(type_closure));
  system_define("type_variable", makeint(type_variable));
  system_define("type_internal", makeint(type_internal));
  system_define("type_primitive", makeint(type_primitive));
  system_define("type_varargs", makeint(type_varargs));
  system_define("type_secure", makeint(type_secure));
  system_define("type_integer", makeint(type_integer));
  system_define("type_string", makeint(type_string));
  system_define("type_vector", makeint(type_vector));
  system_define("type_pair", makeint(type_pair));
  system_define("type_symbol", makeint(type_symbol));
  system_define("type_table", makeint(type_table));
  system_define("type_private", makeint(type_private));
  system_define("type_object", makeint(type_object));
  system_define("type_character", makeint(type_character));
  system_define("type_gone", makeint(type_gone));
  system_define("type_outputport", makeint(type_outputport));
  system_define("type_mcode", makeint(type_mcode));
  system_define("type_null", makeint(type_null));
  system_define("last_type", makeint(last_type));

  /* Synthetic types */
  system_define("stype_none", makeint(stype_none));
  system_define("stype_any", makeint(stype_any));
  system_define("stype_function", makeint(stype_function));
  system_define("stype_list", makeint(stype_list));
  system_define("last_synthetic_type", makeint(last_synthetic_type));

  /* Primitive flags */
  system_define("OP_LEAF", makeint(OP_LEAF));
  system_define("OP_NOALLOC", makeint(OP_NOALLOC));
  system_define("OP_CLEAN", makeint(OP_CLEAN));
  system_define("OP_NOESCAPE", makeint(OP_NOESCAPE));

  /* Mudlle object flags */
  system_define("MUDLLE_READONLY", makeint(OBJ_READONLY));
  system_define("MUDLLE_IMMUTABLE", makeint(OBJ_IMMUTABLE));

  /* Garbage types */
  system_define("garbage_string", makeint(garbage_string));
  system_define("garbage_record", makeint(garbage_record));
  system_define("garbage_code", makeint(garbage_code));
  system_define("garbage_forwarded", makeint(garbage_forwarded));
  system_define("garbage_permanent", makeint(garbage_permanent));
  system_define("garbage_temp", makeint(garbage_temp));
  system_define("garbage_mcode", makeint(garbage_mcode));

  /* Errors */
  system_define("error_bad_function", makeint(error_bad_function));
  system_define("error_stack_underflow", makeint(error_stack_underflow));
  system_define("error_bad_type", makeint(error_bad_type));
  system_define("error_divide_by_zero", makeint(error_divide_by_zero));
  system_define("error_bad_index", makeint(error_bad_index));
  system_define("error_bad_value", makeint(error_bad_value));
  system_define("error_variable_read_only", makeint(error_variable_read_only));
  system_define("error_loop", makeint(error_loop));
  system_define("error_recurse", makeint(error_recurse));
  system_define("error_wrong_parameters", makeint(error_wrong_parameters));
  system_define("error_security_violation", makeint(error_security_violation));
  system_define("error_value_read_only", makeint(error_value_read_only));
  system_define("error_user_interrupt", makeint(error_user_interrupt));

  /* Module support */
  DEFINE("module_status", module_status);
  DEFINE("module_set!", module_set);
  DEFINE("module_table", module_table);
  system_define("module_unloaded", makeint(module_unloaded));
  system_define("module_error", makeint(module_error));
  system_define("module_loading", makeint(module_loading));
  system_define("module_loaded", makeint(module_loaded));
  system_define("module_protected", makeint(module_protected));
  DEFINE("module_unload", module_unload);
  DEFINE("module_require", module_require);
  DEFINE("module_vstatus", module_vstatus);
  DEFINE("module_vset!", module_vset);
  system_define("var_normal", makeint(var_normal));
  system_define("var_write", makeint(var_write));

  /* C options information */
#ifdef GCDEBUG
  system_define("OPTION_GCDEBUG", makebool(TRUE));
#else
  system_define("OPTION_GCDEBUG", makebool(FALSE));
#endif
}
