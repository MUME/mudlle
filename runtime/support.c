/*
 * Copyright (c) 1993-2012 David Gay and Gustav Hållberg
 * All rights reserved.
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose, without fee, and without written agreement is hereby granted,
 * provided that the above copyright notice and the following two paragraphs
 * appear in all copies of this software.
 *
 * IN NO EVENT SHALL DAVID GAY OR GUSTAV HALLBERG BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF DAVID GAY OR
 * GUSTAV HALLBERG HAVE BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * DAVID GAY AND GUSTAV HALLBERG SPECIFICALLY DISCLAIM ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
 * FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS ON AN
 * "AS IS" BASIS, AND DAVID GAY AND GUSTAV HALLBERG HAVE NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "runtime.h"

#include "../alloc.h"
#include "../builtins.h"
#include "../context.h"
#include "../global.h"
#include "../lexer.h"
#include "../module.h"
#include "../mparser.h"
#include "../utils.h"

#if defined(i386) && !defined(NOCOMPILER)
static int cc_length = 0;	/* statistics */
#endif

TYPEDOP(mudlle_parse, 0, "`s -> `v. Parses mudlle expression `s and"
        " returns a parse tree, or false if unsuccessful",
        1, (struct string *code), OP_LEAF | OP_NOESCAPE,
        "s.[vz]")
{
  TYPEIS(code, type_string);

  ASSERT_NOALLOC_START();
  const char *const lines[] = { code->str, NULL };
  struct reader_state rstate;
  save_reader_state(&rstate);
  read_from_strings(lines, NULL, NULL);
  block_t memory = new_block();
  mfile f = parse(memory);
  ASSERT_NOALLOC_END();

  value parsed;
  if (f)
    parsed = mudlle_parse(memory, f);
  else
    parsed = makebool(false);

  restore_reader_state(&rstate);
  free_block(memory);

  return parsed;
}

UNSAFETOP(mudlle_parse_file, 0,
          "`s1 `s2 `s3 -> `v. Parse the file `s1, recording `s2 as its"
          " file name and `s3 as its nice name,"
          " and return its parse tree `v, or false if unsuccessful",
          3, (struct string *filename, struct string *name,
              struct string *nicename),
          OP_LEAF | OP_NOESCAPE, "sss.[vz]")
{
  TYPEIS(filename, type_string);
  TYPEIS(name, type_string);
  TYPEIS(nicename, type_string);
  FILE *f = fopen(filename->str, "r");
  if (f == NULL) return makebool(false);

  int c = fgetc(f);
  if (c == '*' || c == EOF) {
    /* empty or commented-out file */
    fclose(f);
    return makebool(false);
  }

  rewind(f);

  char *fname, *nname;
  LOCALSTR(nname, nicename);
  LOCALSTR(fname, name);
  struct reader_state rstate;
  save_reader_state(&rstate);
  read_from_file(f, fname, nname);

  block_t memory = new_block();
  mfile mf = parse(memory);
  value parsed = mf ? mudlle_parse(memory, mf) : makebool(false);

  restore_reader_state(&rstate);
  free_block(memory);
  fclose(f);

  return parsed;
}

/* The machine language builtin ops */

#ifndef NOCOMPILER
static const struct builtin_table {
  const char *name;
  void (*address)(void);
} builtins[] = {
#ifdef AMIGA
  {"bsubtract", bsubtract},
  {"bor", bor},
  {"band", band},
  {"bleq", bleq},
  {"blne", blne},
  {"bllt", bllt},
  {"blle", blle},
  {"blgt", blgt},
  {"blge", blge},
  {"bbitor", bbitor},
  {"bbitxor", bbitxor},
  {"bbitand", bbitand},
  {"bshift_left", bshift_left},
  {"bshift_right", bshift_right},
  {"badd", badd},
  {"bmultiply", bmultiply},
  {"bdivide", bdivide},
  {"bremainder", bremainder},
  {"bnegate", bnegate},
  {"bnot", bnot},
  {"bbitnot", bbitnot},
  {"bref", bref},
  {"bcar", bcar},
  {"bcdr", bcdr},
  {"bwglobal", bwglobal},
  {"bbadargs", bbadargs},
  {"bcompare", bcompare},
  {"bcall", bcall},
  {"balloc", balloc},
  {"balloc_readonly", balloc_readonly},
  {"bcons", bcons},
  {"berror", berror},
  {"bvarargs", bvarargs},
#endif /* AMIGA */
#ifdef sparc
  {"bsubtract", bsubtract},
  {"bor", bor},
  {"band", band},
  {"bleq", bleq},
  {"blne", blne},
  {"bllt", bllt},
  {"blle", blle},
  {"blgt", blgt},
  {"blge", blge},
  {"bbitor", bbitor},
  {"bbitxor", bbitxor},
  {"bbitand", bbitand},
  {"bshift_left", bshift_left},
  {"bshift_right", bshift_right},
  {"badd", badd},
  {"bmultiply", bmultiply},
  {"bdivide", bdivide},
  {"bremainder", bremainder},
  {"bnegate", bnegate},
  {"bnot", bnot},
  {"bbitnot", bbitnot},
  {"bref", bref},
  {"bcar", bcar},
  {"bcdr", bcdr},
  {"bwglobal", bwglobal},
  {"bcleargc", bcleargc},
  {"bvarargs", bvarargs},
  {"bcompare", bcompare},
  {"bcall", bcall},
  {"balloc_variable", balloc_variable},
  {"balloc_cons", balloc_cons},
  {"balloc_closure", balloc_closure},
  {"bcall_primitive", bcall_primitive},
  {"bcall_primitive_leaf", bcall_primitive_leaf},
  {"bcall_primitive_leaf_noalloc", bcall_primitive_leaf_noalloc},
  {"bcall_secure", bcall_secure},
  {"bcall_varargs", bcall_varargs}
#endif /* sparc */
#ifdef i386
  {"xcount", (void (*)())&xcount},
  {"maxseclevel", (void (*)())&maxseclevel},
  {"seclevel", (void (*)())&internal_seclevel}, /* currently unused */
  {"ccontext", (void (*)())&ccontext},
  {"env_values", (void (*)())&env_values},
  {"max_loop_count", (void (*)())makeint(MAX_FAST_CALLS)},

#define FIRST_RELBUILTIN 6
#define FIRST_RELBUILTIN_FN bshift_left

  {"bshift_left", bshift_left},
  {"bshift_right", bshift_right},
  {"badd", badd},
  {"bmultiply", bmultiply},
  {"bdivide", bdivide},
  {"bremainder", bremainder},
  {"bref", bref},
  {"bset", bset},
  {"btypeof", btypeof},

  {"brglobal", brglobal},
  {"bwglobal", bwglobal},

  {"bcleargc", bcleargc},
  {"bcleargc0", bcleargc0},
  {"bcleargc1", bcleargc1},
  {"bcleargc2", bcleargc2},
  {"bvarargs", bvarargs},

  {"bcall", bcall},
  {"bcall_secure", bcall_secure},
  {"bcall_varargs", bcall_varargs},
  {"bapply_varargs", bapply_varargs},

  {"balloc_variable", balloc_variable},
  {"balloc_cons", balloc_cons},
  {"balloc_vector", balloc_vector},
  {"balloc_closure", balloc_closure},

  {"berror_bad_function", berror_bad_function},
  {"berror_stack_underflow", berror_stack_underflow},
  {"berror_bad_type", berror_bad_type},
  {"berror_divide_by_zero", berror_divide_by_zero},
  {"berror_bad_index", berror_bad_index},
  {"berror_bad_value", berror_bad_value},
  {"berror_variable_read_only", berror_variable_read_only},
  {"berror_loop", berror_loop},
  {"berror_recurse", berror_recurse},
  {"berror_wrong_parameters", berror_wrong_parameters},
  {"bearly_error_wrong_parameters", bearly_error_wrong_parameters},
  {"berror_security_violation", berror_security_violation},
  {"berror_value_read_only", berror_value_read_only},
  {"berror_user_interrupt", berror_user_interrupt},
  {"berror_no_match", berror_no_match},
  {"berror_compile", berror_compile},
  {"berror_abort", berror_abort}
#endif /* i386 */
  };
#endif /* ! NOCOMPILER */

#ifndef NOCOMPILER
static ulong builtin_find(struct string *name, int *k)
{
  char *n = name->str;
  for (int i = sizeof builtins / sizeof builtins[0]; --i >= 0; )
    if (strcmp(n, builtins[i].name) == 0)
      {
	if (k) *k = i;
	return (ulong)builtins[i].address;
      }

  return 0;
}
#endif /* ! NOCOMPILER */

#ifndef NOCOMPILER
static ulong primitive_find(struct string *name)
{
  ulong n = mglobal_lookup(name);
  struct primitive *p = GVAR(n);

  if (!(TYPE(p, type_primitive)
        || TYPE(p, type_varargs)))
    return 0;

  return (ulong)p->op->op;
}
#endif /* ! NOCOMPILER */

#ifdef sparc
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
#endif
#ifdef i386
static inline void set_cst(ubyte *x, ulong n)
{
  memcpy(x, &n, sizeof n);
}
#endif

#ifndef NOCOMPILER
static int check_primitives(struct list *scan)
{
  int count = 0;

  while (scan != NULL)
    {
      ++count;

      TYPEIS(scan, type_pair);
      struct list *primitive = scan->car;
      TYPEIS(primitive, type_pair);
      TYPEIS(primitive->car, type_string);
      ISINT(primitive->cdr);
      /* while primitive_find() can allocate, that will always trigger
         an error */
      if (!primitive_find(primitive->car))
        runtime_error(error_bad_value);
      scan = scan->cdr;
    }

  return count;
}
#endif /* ! NOCOMPILER */

static inline value assert_immutable(value v)
{
  assert(immutablep(v));
  return v;
}

#define OPT_TYPEIS(v, t) do { if (v) TYPEIS(v, t); } while (0)

static char *add_uword(char *dst, long v)
{
  uword w = v;
  memcpy(dst, &w, sizeof w);
  return dst + sizeof w;
}

static char *add_long(char *dst, long v)
{
  memcpy(dst, &v, sizeof v);
  return dst + sizeof v;
}

VARTOP(link, 0,
       "`s1 `n1 `s2 `s3 `s4 `n2 `l1 `l2 `l3 `l4 `l5 `l6"
       " `n3 `n4 `s5 `s6 `n5 `s7 -> `code. Builds a code object from:\n"
       "its machine code `s1,\n"
       "security level `n1, help string `s2, varname `s3, filename `s4,"
       " pretty-printed file name `s7, lineno `n2, return typeset `n3,"
       " return itype `n4, line number info `s5, argument types `s6"
       " (null for varargs), closure flags `n5\n"
       "constants `l1=list of constant/offset pairs\n"
       "builtins `l2=list of name/offset pairs\n"
       "globals `l3=list of name/offset pairs\n"
       "absolute primitives `l4=list of name/offset pairs\n"
       "relative primitives `l5=list of name/offset pairs\n"
       "seclevs `l6=list of offsets",
       0, "snsssnllllllnnssns.o")
{
  enum {
    lv_mcode,
    lv_seclev,
    lv_help,
    lv_varname,
    lv_filename,
    lv_lineno,
    lv_consts,
    lv_builtins,
    lv_globals,
    lv_primitives,
    lv_rel_primitives,
    lv_seclevs,
    lv_return_typeset,
    lv_return_itype,
    lv_linenos,
    lv_arg_types,
    lv_flags,
    lv_nicename,
    lv_arguments
  };

  assert(strlen(op_link.type[0]) == lv_arguments + 2);

  assert(builtins[FIRST_RELBUILTIN].address
         == &FIRST_RELBUILTIN_FN);

#ifdef NOCOMPILER
  runtime_error(error_bad_value);
#else  /* !NOCOMPILER */


  if (nargs != lv_arguments) runtime_error(error_wrong_parameters);

  GCPRO1(args);

  TYPEIS(args->data[lv_mcode], type_string);
  ulong clen = string_len((struct string *)args->data[lv_mcode]);

  char *(*add_const)(char *dst, long v);
  int cst_size;
  if (clen > (uword)~0)
    {
      cst_size = sizeof (long);
      add_const = add_long;
    }
  else
    {
      cst_size = sizeof (uword);
      add_const = add_uword;
    }

  OPT_TYPEIS(args->data[lv_help],      type_string);
  OPT_TYPEIS(args->data[lv_varname],   type_string);
  OPT_TYPEIS(args->data[lv_filename],  type_string);
  OPT_TYPEIS(args->data[lv_nicename],  type_string);
  OPT_TYPEIS(args->data[lv_arg_types], type_vector);
  int seclev           = GETINT(args->data[lv_seclev]);
  int alineno          = GETINT(args->data[lv_lineno]);
  ulong return_typeset = GETINT(args->data[lv_return_typeset]);
  ulong return_itype   = GETINT(args->data[lv_return_itype]);
  ulong flags          = GETINT(args->data[lv_flags]);

  ulong ncsts = 0;
  {
    struct list *scan_csts = args->data[lv_consts];
    struct gcpro gcproc;
    GCPRO(gcproc, scan_csts);
    while (scan_csts != NULL)
      {
        TYPEIS(scan_csts, type_pair);

        struct list *cst = scan_csts->car;
        TYPEIS(cst, type_pair);

        long ofs = GETINT(cst->cdr);
        assert(ofs >= 0);
        scan_csts = scan_csts->cdr;

        ncsts++;
      }
    UNGCPRO1(gcproc);
  }

  for (struct list *scan_builtins = args->data[lv_builtins];
       scan_builtins;
       scan_builtins = scan_builtins->cdr)
    {
      TYPEIS(scan_builtins, type_pair);
      struct list *builtin = scan_builtins->car;
      TYPEIS(builtin, type_pair);
      TYPEIS(builtin->car, type_string);
      ISINT(builtin->cdr);
    }

  {
    struct list *scan_globals = args->data[lv_globals];
    struct gcpro gcprog;
    GCPRO(gcprog, scan_globals);
    while (scan_globals != NULL)
      {
        TYPEIS(scan_globals, type_pair);
        struct list *lglobal = scan_globals->car;
        TYPEIS(lglobal, type_pair);
        ISINT(lglobal->cdr);
        struct string *gname;
        if (TYPE(lglobal->car, type_pair))
          {
            struct list *gtype = lglobal->car;
            gname = gtype->car;
            long t = GETINT(gtype->cdr);
            /* 0 means global index, 1 means gidx * 2 + 1 */
            if (t < 0 || t > 1)
              runtime_error(error_bad_value);
          }
        else
          gname = lglobal->car;
        TYPEIS(gname, type_string);
        mglobal_lookup(gname); /* don't want GC later! */
        scan_globals = scan_globals->cdr;
      }
    UNGCPRO1(gcprog);
  }

  check_primitives(args->data[lv_primitives]);
  ulong nrel = check_primitives(args->data[lv_rel_primitives]);

#ifdef AMIGA
  size = offsetof(struct mcode, mcode) + clen + ncsts * sizeof (uword);
  newp = gc_allocate(size);
  UNGCPRO();
  mcode = args->data[lv_mcode];
  help = args->data[lv_help];
  varname = args->data[lv_varname];
  afilename = args->data[lv_filename];
  csts = args->data[lv_consts];
  abuiltins = args->data[lv_builtins];
  globals = args->data[lv_globals];
  anicename = args->data[lv_nicename];

  newp->o.size = size;
  newp->o.garbage_type = garbage_mcode;
  newp->o.type = type_mcode;
  newp->o.flags = 0;
  newp->nb_constants = ncsts;
  newp->seclevel = seclev;
  newp->help = help;
  newp->varname = varname;
  newp->filename = afilename;
  newp->lineno = alineno;
  newp->code_length = clen;
  memcpy(newp->magic, "\xff\xff\xff\xff\xff\xff\xff\xff", 8);

  memcpy(newp->mcode, mcode->str, clen);

  /* Copy constants and their offsets */
  scan_csts = csts;
  cst_offsets = (uword *)((ubyte *)&newp->mcode
                          + MUDLLE_ALIGN(clen, sizeof (uword)));
  while (scan_csts != NULL)
    {
      struct list *cst;
      uword offset;

      cst = scan_csts->car;
      offset = intval(cst->cdr);
      *(value *)(newp->mcode + offset) = cst->car;
      *cst_offsets++ = offset;

      scan_csts = scan_csts->cdr;
    }

  /* Set builtin addresses */
  scan_builtins = abuiltins;
  while (scan_builtins != NULL)
    {
      struct list *builtin;
      uword offset;
      ulong baddress;

      builtin = scan_builtins->car;
      offset = intval(builtin->cdr);
      if (!(baddress = builtin_find(builtin->car, NULL)))
	runtime_error(error_bad_value);
      *(ulong *)(newp->mcode + offset) = baddress;

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
      genv = env_values;
      *(uword *)(newp->mcode + offset) =
	(ubyte *)&genv->data[goffset] - (ubyte *)genv;

      scan_globals = scan_globals->cdr;
    }

  /* Primitives not implemented on Amiga */

#endif

#ifdef sparc
  size = offsetof(struct mcode, mcode) + clen + ncsts * sizeof (uword);
  /* allocate extra space to ensure that gen1 will have space for this object
     even with the aligned forwarding */
  newp = gc_allocate(size + CODE_ALIGNMENT);
  UNGCPRO();
  /* No more GC from here on !!! */

  mcode = args->data[lv_mcode];
  help = args->data[lv_help];
  varname = args->data[lv_varname];
  afilename = args->data[lv_filename];
  csts = args->data[lv_consts];
  abuiltins = args->data[lv_builtins];
  globals = args->data[lv_globals];
  primitives = args->data[lv_primitives];
  anicename = args->data[lv_nicename];

  newp->o.size = size;
  newp->o.garbage_type = garbage_mcode;
  newp->o.type = type_mcode;
  newp->o.flags = 0;
  newp->nb_constants = ncsts;
  newp->seclevel = seclev;
  newp->help = help;

  newp->filename = afilename;
  newp->lineno = alineno;
  newp->code_length = clen;
  newp->myself = (ubyte *)newp;
  memcpy(newp->magic, "\xff\xff\xff\xff\xff\xff\xff\xff", 8);

  memcpy(newp->mcode, mcode->str, clen);

  /* Copy constants and their offsets */
  scan_csts = csts;
  cst_offsets = (uword *)((ubyte *)newp->mcode + clen);
  while (scan_csts != NULL)
    {
      struct list *cst;
      uword offset;

      cst = scan_csts->car;
      offset = intval(cst->cdr);
      set_cst(newp->mcode + offset, (ulong)cst->car);
      *cst_offsets++ = offset;

      scan_csts = scan_csts->cdr;
    }

  /* Set builtin addresses */
  scan_builtins = abuiltins;
  while (scan_builtins != NULL)
    {
      struct list *builtin;
      ulong baddress, *callins;

      builtin = scan_builtins->car;
      if (!(baddress = builtin_find(builtin->car, NULL)))
	runtime_error(error_bad_value);

      callins = newp->mcode + intval(builtin->cdr);
      *callins = 1 << 30 | (baddress - (ulong)callins) >> 2;

      scan_builtins = scan_builtins->cdr;
    }

  /* Set primitive addresses */
  scan_primitives = primitives;
  while (scan_primitives != NULL)
    {
      struct list *primitive;
      ulong paddress, offset;

      primitive = scan_primitives->car;
      offset = intval(primitive->cdr);
      paddress = primitive_find(primitive->car);
      set_cst(newp->mcode + offset, paddress);

      scan_primitives = scan_primitives->cdr;
    }

  /* Set global offsets */
  scan_globals = globals;
  while (scan_globals != NULL)
    {
      struct list *globl;
      ulong goffset, offset;
      struct vector *genv;

      globl = scan_globals->car;
      offset = intval(globl->cdr);
      goffset = mglobal_lookup(globl->car);

      /* Compute byte offset from environment base */
      genv = env_values;
      goffset = (ubyte *)&genv->data[goffset] - (ubyte *)genv;
      set_cst(newp->mcode + offset, goffset);

      scan_globals = scan_globals->cdr;
    }

#endif

#if defined(i386)
  for (struct list *seclevs = args->data[lv_seclevs];
       seclevs;
       seclevs = seclevs->cdr)
    {
      TYPEIS(seclevs, type_pair);
      struct list *e = seclevs->car;
      TYPEIS(e, type_pair);
      TYPEIS(e->car, type_integer);
      TYPEIS(e->cdr, type_integer);
    }

  /* Count relocatable builtins */
  for (struct list *scan_builtins = args->data[lv_builtins];
       scan_builtins;
       scan_builtins = scan_builtins->cdr)
    {
      struct list *builtin = scan_builtins->car;

      int k;
      if (!builtin_find(builtin->car, &k))
	runtime_error(error_bad_value);

      if (k >= FIRST_RELBUILTIN)
	nrel++;
    }

  ulong size = (MUDLLE_ALIGN(clen, cst_size)
                + (ncsts + nrel) * cst_size);

  /* The following gave a net slowdown, at least on the compiler */
  /* Avoid sharing cache lines with other objects */
  /*  size = MUDLLE_ALIGN(size, 32);*/

  size += offsetof(struct mcode, mcode);
  cc_length += size;

  /* allocate extra space to ensure that gen1 will have space for
     this object even with the aligned forwarding */
  struct mcode *newp = gc_allocate(size + CODE_ALIGNMENT - 1);
#ifdef GCDEBUG
  ulong newp_generation = newp->code.o.generation;
#endif
  {
    ulong diff = ((CODE_ALIGNMENT - (ulong)(&newp->mcode))
                  & (CODE_ALIGNMENT - 1));
    struct mcode *oldp = newp;
    newp = (struct mcode *)((char *)oldp + diff);
    assert(((ulong)(&newp->mcode) & (CODE_ALIGNMENT - 1)) == 0);
    memset(oldp, 0, diff);
  }
  UNGCPRO();
  /* No more GC from here on !!! */

  *newp = (struct mcode){
    .code = {
      .o = {
        .size         = size,
        .garbage_type = garbage_mcode,
        .type         = type_mcode,
        .flags        = 0,
#ifdef GCDEBUG
        .generation   = newp_generation,
#endif
      },
      .varname        = assert_immutable(args->data[lv_varname]),
      .filename       = assert_immutable(args->data[lv_filename]),
      .nicename       = assert_immutable(args->data[lv_nicename]),
      .help           = assert_immutable(args->data[lv_help]),
      .arg_types      = assert_immutable(args->data[lv_arg_types]),
      .lineno         = alineno,
      .seclevel       = seclev,
      .return_typeset = return_typeset
    },
    .nb_constants  = ncsts,
    .nb_rel        = nrel,
    .linenos       = assert_immutable(args->data[lv_linenos]),
    .code_length   = clen,
    .myself        = newp,
    .closure_flags = flags,
    .return_itype  = return_itype,
    .magic         = "\xff\xff\xff\xff\xff\xff\xff\xff"
  };

  memcpy(newp->mcode, ((struct string *)args->data[lv_mcode])->str, clen);
  /* align with nop of appropriate length */
  switch ((ulong)-clen % cst_size)
    {
    case 0: break;
    case 1: newp->mcode[clen] = 0x90; break;
    case 2:
      newp->mcode[clen] = 0x66; /* 66 nop */
      newp->mcode[clen + 1] = 0x90;
      break;
    case 3:
      newp->mcode[clen] = 0x0f;
      newp->mcode[clen + 1] = 0x1f;
      newp->mcode[clen + 2] = 0x0ff;
      break;
    default: abort();
    }

  /* Copy constants and their offsets */
  for (struct list *seclevs = args->data[lv_seclevs];
       seclevs;
       seclevs = seclevs->cdr)
    {
      assert(TYPE(seclevs, type_pair));
      struct list *e = seclevs->car;
      assert(TYPE(e, type_pair));
      assert(integerp(e->car));
      assert(integerp(e->cdr));
      long ofs = intval(e->cdr);
      enum {
        sl_c = 0,
        sl_mudlle = 1,
        sl_maxlev = 2
      } type = intval(e->car);
      switch (type)
        {
        case sl_c:
          {
            uword l = seclev;
            memcpy(newp->mcode + ofs, &l, sizeof l);
            break;
          }
        case sl_mudlle:
          {
            value l = makeint(seclev);
            memcpy(newp->mcode + ofs, &l, sizeof l);
            break;
          }
        case sl_maxlev:
          {
            value l = seclevel_to_maxseclevel(seclev);
            memcpy(newp->mcode + ofs, &l, sizeof l);
            break;
          }
        default:
          abort();
        }
    }

  char *cst_offsets = ((char *)newp->mcode
                       + MUDLLE_ALIGN(clen, cst_size));
  for (struct list *scan_csts = args->data[lv_consts];
       scan_csts;
       scan_csts = scan_csts->cdr)
    {
      assert(TYPE(scan_csts, type_pair));
      struct list *cst = scan_csts->car;
      assert(TYPE(cst, type_pair));
      assert(immutablep(cst->car));
      long offset = intval(cst->cdr);
      value c = cst->car;
      set_cst(newp->mcode + offset, (ulong)c);
      cst_offsets = add_const(cst_offsets, offset);
    }

  /* Set builtin addresses */
  for (struct list *scan_builtins = args->data[lv_builtins];
       scan_builtins;
       scan_builtins = scan_builtins->cdr)
    {
      assert(TYPE(scan_builtins, type_pair));
      struct list *builtin = scan_builtins->car;
      int k;
      ulong baddress = builtin_find(builtin->car, &k);
      assert(baddress);
      long offset = intval(builtin->cdr);
      ubyte *callins = newp->mcode + offset;

      if (k < FIRST_RELBUILTIN)
	set_cst(callins, baddress);
      else
	{
	  set_cst(callins, baddress - (ulong)callins - sizeof (ulong));
	  /* need to remember offset (for relocation) */
          cst_offsets = add_const(cst_offsets, offset);
	}
    }

  /* Set primitive addresses */
  for (struct list *scan_primitives = args->data[lv_primitives];
       scan_primitives;
       scan_primitives = scan_primitives->cdr)
    {
      assert(TYPE(scan_primitives, type_pair));
      struct list *primitive = scan_primitives->car;
      ulong offset = intval(primitive->cdr);
      ulong paddress = primitive_find(primitive->car);
      set_cst(newp->mcode + offset, paddress);
    }

  /* Set primitive addresses */
  for (struct list *scan_primitives = args->data[lv_rel_primitives];
       scan_primitives;
       scan_primitives = scan_primitives->cdr)
    {
      assert(TYPE(scan_primitives, type_pair));
      struct list *primitive = scan_primitives->car;
      long offset = intval(primitive->cdr);
      ulong paddress = primitive_find(primitive->car);
      set_cst(newp->mcode + offset,
              paddress - (ulong)(newp->mcode + offset + 4));
      /* need to remember offset (for relocation) */
      cst_offsets = add_const(cst_offsets, offset);
    }

  assert(cst_offsets - ((char *)newp->mcode
                        + MUDLLE_ALIGN(clen, cst_size))
         == (nrel + ncsts) * cst_size);

  /* Set global offsets */
  for (struct list *scan_globals = args->data[lv_globals];
       scan_globals;
       scan_globals = scan_globals->cdr)
    {
      assert(TYPE(scan_globals, type_pair));
      struct list *lglobal = scan_globals->car;
      long offset = intval(lglobal->cdr);

      ulong goffset;
      if (TYPE(lglobal->car, type_pair))
        {
          struct list *p = lglobal->car;
          struct string *name = p->car;
          goffset = mglobal_lookup(name);
          if (p->cdr == makeint(1))
            goffset = (ulong)makeint(goffset);
        }
      else
        {
          struct string *name = lglobal->car;
          goffset = mglobal_lookup(name);
          /* Compute byte offset from environment base */
          struct vector *genv = env_values;
          goffset = (ubyte *)&genv->data[goffset] - (ubyte *)genv;
        }

      set_cst(newp->mcode + offset, goffset);
    }

#endif

#ifdef GCSTATS
  gcstats.anb[type_mcode]++;
  gcstats.asizes[type_mcode] += MUDLLE_ALIGN(size + CODE_ALIGNMENT,
                                             sizeof (value));
#endif

  newp->code.o.flags |= OBJ_IMMUTABLE;

  return newp;
#endif /* !NOCOMPILER */
}

UNSAFETOP(make_closure, 0, "`mcode -> `f. Makes a function with no closure"
          " vars from given `mcode object",
	  1, (struct mcode *mcode),
          OP_LEAF | OP_NOESCAPE, "o.f")
{
  TYPEIS(mcode, type_mcode);

  return alloc_closure0(&mcode->code);
}

TYPEDOP(closurep, "closure?", "`x -> `b. True if `x is a closure.",
	1, (value x),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(TYPE(x, type_closure));
}

TYPEDOP(securep, "secure?", "`x -> `b. True if `x is a secure primitive.",
	1, (value x),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(TYPE(x, type_secure));
}

TYPEDOP(primitivep, "primitive?", "`x -> `b. True if `x is a primitive."
        " See also `any_primitive?().",
	1, (value x),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(TYPE(x, type_primitive));
}

TYPEDOP(varargsp, "varargs?",
        "`x -> `b. True if `x is a variable argument primitive.",
	1, (value x),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(TYPE(x, type_varargs));
}

TYPEDOP(any_primitivep, "any_primitive?", "`x -> `b. True if `x is either"
        " primitive, secure, or varargs.",
	1, (value x),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(is_any_primitive(x));
}

TYPEDOP(primitive_nargs, 0,
        "`primitive -> `n. Returns # of arguments of primitive"
        " or secop; -1 for varargs",
	1, (struct primitive *p),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "f.n")
{
  if (!is_any_primitive(p))
    runtime_error(error_bad_value);

  return makeint(p->op->nargs);
}

TYPEDOP(primitive_flags, 0, "`primitive -> `n. Returns flags of primitive.",
	1, (struct primitive *p),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "f.n")
{
  if (!is_any_primitive(p))
    runtime_error(error_bad_value);

  return makeint(p->op->flags);
}

static void recurse_typing(char *start, size_t size,
                           const char *from, char *to, struct list **l)
{
  for (;;)
    {
      if (*from == '[')
        {
          const char *end = strchr(from, ']');
          assert(end != NULL);
          for (++from; from < end; ++from)
            {
              *to = *from;
              recurse_typing(start, size, end + 1, to + 1, l);
            }
          return;
        }

      *to++ = *from;
      if (*from == 0)
        {
          assert(to - start <= size);
          struct string *s = alloc_string(start);
          *l = alloc_list(s, *l);
          return;
        }
      ++from;
    }
}

TYPEDOP(primitive_type, 0, "`primitive -> `l. Returns a list of type"
        " signatures for `primitive.\n"
        "`null means no type information is available.",
	1, (struct primitive *p),
	OP_LEAF | OP_NOESCAPE, "f.l")
{
  if (!is_any_primitive(p))
    runtime_error(error_bad_type);

  const char *const *atyping = p->op->type;
  if (atyping == NULL)
    return NULL;

  struct list *l = NULL;
  GCPRO1(l);
  for (; *atyping; ++atyping)
    {
      size_t size = strlen(*atyping) + 1;
      char t[size];
      recurse_typing(t, size, *atyping, t, &l);
    }
  UNGCPRO();
  return l;
}

TYPEDOP(closure_arguments, 0, "`c -> `v. Returns a vector of type information"
        " for `c's arguments. Each entry is a bitfield of 1 << `type_xxx"
        " flags. Returns null for vararg closures.",
        1, (struct closure *c), OP_LEAF | OP_NOALLOC | OP_NOESCAPE,
        "f.[vu]")
{
  TYPEIS(c, type_closure);
  return c->code->arg_types;
}

TYPEDOP(closure_return_typeset, 0, "`c -> `n. Returns possible return types of"
        " closure `c, a bitfield of 1 << `type_xxx.",
        1, (struct closure *c), OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "f.n")
{
  TYPEIS(c, type_closure);
  return makeint(c->code->return_typeset);
}

TYPEDOP(closure_return_itype, 0, "`c -> `n. Returns possible return itypes of"
        " closure `c, a bitfield of `itype_xxx. Returns -1 for interpreted"
        " code.",
        1, (struct closure *c), OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "f.n")
{
  TYPEIS(c, type_closure);
  if (c->code->o.type != type_mcode)
    return makeint(-1);
#ifdef NOCOMPILER
  abort();
#else
  return makeint(((struct mcode *)c->code)->return_itype);
#endif
}

TYPEDOP(closure_flags, 0, "`c -> `n. Returns closure flags for `c (closure,"
        " code, or mcode), a bitset of `clf_xxx flags.",
        1, (value c), OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "[fo].n")
{
  if (TYPE(c, type_closure))
    c = ((struct closure *)c)->code;

  if (TYPE(c, type_code))
    return makeint(0);

  TYPEIS(c, type_mcode);
  return makeint(((struct mcode *)c)->closure_flags);
}

UNSAFETOP(closure_code, 0,
          "`f -> `c. Returns the code or mcode object of closure `f.",
          1, (struct closure *fn), OP_LEAF | OP_NOESCAPE, "f.o")
{
  TYPEIS(fn, type_closure);
  return fn->code;
}

UNSAFETOP(global_table, 0, " -> `t. Returns global symbol table",
          0, (void),
          OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".t")
{
  return global;
}

TYPEDOP(global_name, 0, "`n -> `s. Returns the name of global `n"
        " for 0 <= `n < `global_names()",
        1, (value midx),
        OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "n.s")
{
  long idx = GETINT(midx);
  if (idx < 0 || idx >= intval(environment->used))
    runtime_error(error_bad_value);
  return GNAME(idx);
}

SECTOP(global_lookup, 0, "`s -> `n. Returns index of global variable `s."
       " Creates it if it doesn't exist already.",
       1, (struct string *name), SECLEVEL_GLOBALS,
       OP_LEAF | OP_NOESCAPE, "s.n")
{
  TYPEIS(name, type_string);

  return makeint(mglobal_lookup(name));
}

SECTOP(global_value, 0, "`n -> `x. Returns value of global variable `n"
       " for 0 <= `n < `global_names()",
       1, (value midx), SECLEVEL_GLOBALS,
       OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "n.x")
{
  long idx = GETINT(midx);
  if (idx < 0 || idx >= intval(environment->used))
    runtime_error(error_bad_value);

  return GVAR(idx);
}

TYPEDOP(global_names, 0,
        " -> `n. Returns the number of globals used. See also `global_name()"
        " and `global_value()",
	0, (void), OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".n")
{
  return environment->used;
}

UNSAFETOP(global_set, "global_set!",
          "`n `x -> `x. Sets global variable `n to `x."
          " Fails if global `n is readonly.",
          2, (value midx, value x),
          OP_LEAF | OP_NOALLOC, "nx.2")
{
  long idx = GETINT(midx);
  if (idx < 0 || idx >= intval(environment->used) || GCONSTANT(idx))
    runtime_error(error_bad_value);

  GVAR(idx) = x;
  return x;
}

void global_runtime_error(runtime_errors error, bool is_write, ulong goffset)
{
  primitive_runtime_error(error,
                          is_write ? &op_global_set : &op_global_lookup,
                          1, GNAME(goffset));
}

TYPEDOP(module_status, 0, "`s -> `n. Returns status of module (library) `s,"
        " one of:\n"
        "  `module_unloaded   \thas not been loaded\n"
        "  `module_error      \tfailed to load\n"
        "  `module_loading    \tis currently loading\n"
        "  `module_loaded     \tloaded correctly\n"
        "  `module_protected  \tsystem module that cannot be unloaded",
	1, (struct string *name),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "s.n")
{
  TYPEIS(name, type_string);

  return makeint(module_status(name->str));
}

TYPEDOP(module_seclevel, 0, "`s -> `n. Returns seclevel of module `s",
	1, (struct string *name),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "s.n")
{
  TYPEIS(name, type_string);

  return makeint(module_seclevel(name->str));
}

UNSAFETOP(module_set, "module_set!",
          "`s `n1 `n2 -> . Sets status of module `s to `n1, seclevel `n2",
          3, (struct string *name, value status, value seclev),
          OP_LEAF | OP_NOESCAPE, "snn.")
{
  char *tname;

  TYPEIS(name, type_string);
  LOCALSTR(tname, name);
  ISINT(status);
  ISINT(seclev);

  module_set(tname, intval(status), intval(seclev));

  undefined();
}

UNSAFETOP(module_unload, 0, "`s -> `b. Unload module `s, false if protected",
          1, (struct string *name),
          OP_LEAF | OP_NOALLOC, "s.n")
{
  TYPEIS(name, type_string);

  return makebool(module_unload(name->str));
}

TYPEDOP(module_require, 0,
        "`s -> `n. Load module s if needed, return its new status",
	1, (struct string *name),
	0, "s.n")
{
  char *tname;

  TYPEIS(name, type_string);
  LOCALSTR(tname, name);

  return makeint(module_require(tname));
}

TYPEDOP(module_vstatus, 0,
        "`n0 -> `s/`n1. Return status of global variable `n0; either the"
        " name of the defining library, or one of:\n"
        "  `var_normal          \tnormal variable\n"
        "  `var_module          \tdefined by a module (library)\n"
        "  `var_write           \twritten by mudlle\n"
        "  `var_system_write    \tanyone can read, but mudlle cannot write\n"
        "  `var_system_mutable  \tanyone may read or write",
	1, (value goffset),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "n.S")
{
  long n = GETINT(goffset); n = intval(goffset);
  if (n < 0 || n >= intval(environment->used)) runtime_error(error_bad_value);

  struct string *mod;
  enum vstatus status = module_vstatus(n, &mod);
  if (status == var_module)
    {
      assert(TYPE(mod, type_string));
      return mod;
    }

  return makeint(status);
}

UNSAFETOP(module_vset, "module_vset!",
          "`n0 `s/`n1 -> b. Sets status of global variable `n0 to either the"
          " name `s of the owning module, `var_normal, or `var_write."
          " Returns true if successful.",
          2, (value goffset, value status),
          OP_LEAF | OP_NOALLOC, "n[ns].n")
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

UNSAFETOP(module_table, 0, " -> `t. Returns the module status table",
          0, (void),
          OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".t")
{
  return module_data;
}

void support_init(void)
{
  DEFINE(mudlle_parse);
  DEFINE(mudlle_parse_file);
  DEFINE(link);
  DEFINE(make_closure);
  DEFINE(closurep);
  DEFINE(securep);
  DEFINE(varargsp);
  DEFINE(primitivep);
  DEFINE(any_primitivep);
  DEFINE(primitive_nargs);
  DEFINE(primitive_flags);
  DEFINE(primitive_type);
  DEFINE(closure_arguments);
  DEFINE(closure_return_typeset);
  DEFINE(closure_return_itype);
  DEFINE(closure_flags);
  DEFINE(closure_code);
  DEFINE(global_table);
  DEFINE(global_name);
  DEFINE(global_names);
  DEFINE(global_lookup);
  DEFINE(global_value);
  DEFINE(global_set);

  /* Mudlle object flags */
  system_define("MUDLLE_READONLY",  makeint(OBJ_READONLY));
  system_define("MUDLLE_IMMUTABLE", makeint(OBJ_IMMUTABLE));

  /* Module support */
  DEFINE(module_status);
  DEFINE(module_seclevel);
  DEFINE(module_set);
  DEFINE(module_table);
  DEFINE(module_unload);
  DEFINE(module_require);
  DEFINE(module_vstatus);
  DEFINE(module_vset);

#define __CDEF(name) system_define("mc:c_" #name, makeint(c_ ## name));
  FOR_COMPONENT_CLASSES(__CDEF)
#undef __CDEF
#define __MDEF(name) system_define("mc:m_" #name, makeint(m_ ## name));
  FOR_PARSER_MODULE_FIELDS(__MDEF)
#undef __MDEF
  system_define("mc:m_plain",   makeint(f_plain));
  system_define("mc:m_module",  makeint(f_module));
  system_define("mc:m_library", makeint(f_library));

  /* C options information */
#ifdef GCDEBUG
  system_define("OPTION_GCDEBUG", makebool(true));
#else
  system_define("OPTION_GCDEBUG", makebool(false));
#endif

#ifdef i386
  system_define("x86:function_offset",
                makeint(offsetinobj(struct mcode, mcode)));
  system_define("x86:mcode_seclevel",
                makeint(offsetinobj(struct mcode, code.seclevel)));

  system_define("x86:object_offset", makeint(sizeof (struct obj)));
  system_define("x86:object_size",   makeint(offsetof(struct obj, size)));
  system_define("x86:object_info",   makeint(offsetof(struct obj, size)
                                             + sizeoffield(struct obj, size)));
  system_define("x86:object_type",   makeint(offsetof(struct obj, flags) - 1));
  system_define("x86:object_flags",  makeint(offsetof(struct obj, flags)));

#ifndef NOCOMPILER
  system_define("x86:cc_frame_end_sp",
                makeint(offsetof(struct ccontext, frame_end_sp)));
  system_define("x86:cc_frame_end_bp",
                makeint(offsetof(struct ccontext, frame_end_bp)));
  system_define("x86:cc_callee",
                makeint(offsetof(struct ccontext, callee)));
#endif /* ! NOCOMPILER */
#endif /* i386 */

#ifndef NOCOMPILER
  system_define("mc:max_fast_calls", makeint(MAX_FAST_CALLS));
#endif
}
