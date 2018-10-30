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

#include "../mudlle-config.h"

#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "check-types.h"
#include "prims.h"
#include "support.h"

#include "../alloc.h"
#include "../builtins.h"
#include "../calloc.h"
#include "../context.h"
#include "../dwarf.h"
#include "../global.h"
#include "../lexer.h"
#include "../module.h"
#include "../mparser.h"
#include "../tree.h"
#include "../utils.h"

TYPEDOP(mudlle_parse, 0, "`s0 `s1 -> `v. Parses mudlle expression `s0 and"
        " returns a parse tree, or false if unsuccessful.\n`s1 is the"
        " filename; can be null for \"<string>\".",
        2, (struct string *code, struct string *name),
        OP_LEAF | OP_NOESCAPE,
        "s[su].[vz]")
{
  CHECK_TYPES(code, string,
              name, CT_TYPES(null, string));

  ASSERT_NOALLOC_START();
  const char *const lines[] = { code->str, NULL };
  struct reader_state rstate;
  save_reader_state(&rstate);
  struct alloc_block *memory = new_block();
  const char *cname = name ? heap_allocate_string(memory, name->str) : NULL;
  read_from_strings(lines, cname, cname, false);
  struct mfile *f;
  ASSERT_NOALLOC();
  bool ok = parse(memory, &f);
  /* parse error prints to mudout which may cause allocation */
  if (ok) ASSERT_NOALLOC();

  value parsed = ok ? mudlle_parse(memory, f) : makebool(false);

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
  ASSERT_NOALLOC_START();

  CHECK_TYPES(filename, string,
              name,     string,
              nicename, string);

  FILE *f = fopen(filename->str, "r");
  if (f == NULL) return makebool(false);

  int c = fgetc(f);
  if (c == '*' || c == EOF)
    {
      /* empty or commented-out file */
      fclose(f);
      return makebool(false);
    }

  rewind(f);

  struct reader_state rstate;
  save_reader_state(&rstate);
  struct alloc_block *memory = new_block();
  read_from_file(f, heap_allocate_string(memory, name->str),
                 heap_allocate_string(memory, nicename->str));

  struct mfile *mf;
  ASSERT_NOALLOC();
  bool ok = parse(memory, &mf);
  /* parse error prints to mudout which may cause allocation */
  if (ok) ASSERT_NOALLOC();
  value parsed = ok ? mudlle_parse(memory, mf) : makebool(false);

  restore_reader_state(&rstate);
  free_block(memory);
  fclose(f);

  return parsed;
}

/* The machine language builtin ops */

#ifndef NOCOMPILER
static const struct builtin {
  const char *name;
  void *address;
  bool absolute;
} builtins[] = {
#if defined __i386__ || defined __x86_64__

#ifdef __i386__
  #define BUILTIN_ABSOLUTE false
#else
  #define BUILTIN_ABSOLUTE true
#endif

#define DECL_BUILTIN(func) {                    \
  .name     = #func,                            \
  .address  = (void *)func,                     \
  .absolute = BUILTIN_ABSOLUTE                  \
},
  /* must be sorted; relies on all of these starting with "b" */
  X86_BUILTINS_FOREACH(DECL_BUILTIN)
#undef DECL_BUILTIN
  {"env_values",                    &env_values, true},
  {"max_loop_count",                makeint(MAX_LOOP_COUNT), true},
  {"maxseclevel",                   &maxseclevel, true},
  {"xcount",                        &xcount, true },
#else  /* ! (__i386__ || __x86_64__) */
  #error Unsupported architecture
#endif
  };
#endif /* ! NOCOMPILER */

#ifndef NOCOMPILER
static int cmp_builtin(const void *k, const void *b)
{
  const char *key = k;
  const struct builtin *builtin = b;
  return strcmp(key, builtin->name);
}

static const struct builtin *builtin_find(struct string *name)
{
  return bsearch(name->str, builtins, VLENGTH(builtins), sizeof builtins[0],
                 cmp_builtin);
}
#endif /* ! NOCOMPILER */

#ifndef NOCOMPILER
static ulong primitive_find(struct string *name)
{
  ulong n = mglobal_lookup(name);
  struct primitive *p = GVAR(n);

  if (!(TYPE(p, primitive)
        || TYPE(p, varargs)))
    return 0;

  return (ulong)p->op->op;
}

static inline void set_cst(uint8_t *x, ulong n)
{
  memcpy(x, &n, sizeof n);
}

static int check_primitives(struct list *scan)
{
  int count = 0;

  while (scan != NULL)
    {
      ++count;

      TYPEIS(scan, pair);
      struct list *primitive = scan->car;
      TYPEIS(primitive, pair);
      TYPEIS(primitive->car, string);
      TYPEIS(primitive->cdr, integer);
      /* while primitive_find() can allocate, that will always trigger
         an error */
      if (!primitive_find(primitive->car))
        runtime_error(error_bad_value);
      scan = scan->cdr;
    }

  return count;
}

static inline value assert_immutable(value v)
{
  assert(immutablep(v));
  return v;
}

#endif /* ! NOCOMPILER */

UNSAFETOP(register_mcode_module, 0,
          "`l -> . Register mcode objects in `l as a code module.",
          1, (struct list *l),
          OP_LEAF | OP_NOESCAPE | OP_NOALLOC,
          "l.")
{
  CHECK_TYPES(l, CT_TYPES(null, pair));

  struct ary mcodes = ARY_NULL;
  for (; l; l = l->cdr)
    {
      TYPEIS(l, pair);
      struct mcode *mcode = l->car;
      TYPEIS(mcode, mcode);
      if (!mcode->dwarf_seen)
        ary_add(&mcodes, mcode);
    }
  register_dwarf_mcodes(0, &mcodes);
  ary_free(&mcodes);
  undefined();
}

TYPEDOP(dwarf_line_number_info, 0,
        "`v -> `s. Returns DWARF line number information for locations"
        " in `v, a vector of (`address . `line). The addresses must be sorted,"
        " lowest first.",
        1, (struct vector *v), OP_LEAF | OP_NOESCAPE, "v.s")
{
  CHECK_TYPES(v, vector);

  const long nstates = vector_len(v);
  uint32_t last = 0;
  for (long l = 0; l < nstates; ++l)
    {
      struct list *loc = v->data[l];
      TYPEIS(loc, pair);
      long adr  = GETRANGE(loc->car, last, UINT32_MAX);
      last = adr;
      GETRANGE(loc->car, 0, INT32_MAX);
    }

  struct lni_state *states = nstates ? malloc(sizeof *states * nstates) : NULL;
  for (long l = 0; l < nstates; ++l)
    {
      struct list *loc = v->data[l];
      states[l] = (struct lni_state){
        .addr = intval(loc->car),
        .line = intval(loc->cdr),
      };
    }

  struct string *lni = dwarf_line_number_info(states, nstates);
  free(states);
  return lni;
}

VARTOP(link, 0,
       "`s1 `n1 `s2 `s3 `s4 `p `l1 `l2 `l3 `l4 `l5 `l6"
       " `n2 `n3 `s5 `s6 `n4 `s7 -> `code. Builds a code object from:\n"
       "its machine code `s1,\n"
       "security level `n1, help string `s2, varname `s3, filename `s4,"
       " pretty-printed file name `s7, location `p, a cons(line, column),"
       " return typeset `n2, return itype `n3, line number info `s5,"
       " argument types `s6 (null for varargs), and closure flags `n4."
       "constants `l1=list of constant/offset pairs\n"
       "builtins `l2=list of name/offset pairs\n"
       "globals `l3=list of name/offset pairs\n"
       "absolute primitives `l4=list of name/offset pairs\n"
       "relative primitives `l5=list of name/offset pairs\n"
       "seclevs `l6=list of offsets",
       0, "snssskllllllnnssns.o")
{
#ifdef NOCOMPILER
  runtime_error(error_bad_value);
#else  /* !NOCOMPILER */

#if !(defined __i386__ || defined __x86_64__)
#error Unsupported architecture
#endif


  enum {
    lv_mcode,
    lv_seclev,
    lv_help,
    lv_varname,
    lv_filename,
    lv_location,
    lv_consts,
    lv_builtins,
    lv_globals,
    lv_primitives,
    lv_rel_primitives,
    lv_seclevs,
    lv_return_typeset,
    lv_return_itype,
    lv_linenos,
    lv_arguments,
    lv_flags,
    lv_nicename,
    lv_number_of_fields
  };

  assert(strlen(THIS_OP->type[0]) == lv_number_of_fields + 2);


  if (nargs != lv_number_of_fields) runtime_error(error_wrong_parameters);

  GCPRO(args);

  TYPEIS(args->data[lv_mcode], string);
  ulong clen = string_len((struct string *)args->data[lv_mcode]);
  TYPEIS(args->data[lv_linenos], string);

#define OPT_TYPEIS(v, t) do {                   \
    if (v != NULL && !TYPE(v, t))               \
      runtime_error(error_bad_type);            \
} while (0)
  OPT_TYPEIS(args->data[lv_help],      string);
  OPT_TYPEIS(args->data[lv_varname],   string);
  OPT_TYPEIS(args->data[lv_filename],  string);
  OPT_TYPEIS(args->data[lv_nicename],  string);

  if (!TYPE(args->data[lv_arguments], string))
    {
      struct vector *v = args->data[lv_arguments];
      TYPEIS(v, vector);
      for (long i = 0; i < vector_len(v); ++i)
        {
          struct list *e = v->data[i];
          TYPEIS(e, pair);
          if (!isfalse(e->car))
            TYPEIS(e->car, string);
          TYPEIS(e->cdr, integer);
        }
    }

#undef OPT_TYPEIS
  seclev_t seclev         = GETRANGE(args->data[lv_seclev], 0, MAX_SECLEVEL);
  struct list *mloc = args->data[lv_location];
  TYPEIS(mloc, pair);
  int      alineno        = GETINT(mloc->car);
  int      acolumn        = GETINT(mloc->cdr);
  unsigned return_typeset = GETRANGE(args->data[lv_return_typeset],
                                     0, TYPESET_ANY);
  ulong    return_itype   = GETINT(args->data[lv_return_itype]);
  ulong    flags          = GETINT(args->data[lv_flags]);

  ulong ncsts = 0;
  {
    struct list *scan_csts = args->data[lv_consts];
    struct gcpro gcproc;
    GCPROV(gcproc, scan_csts);
    while (scan_csts != NULL)
      {
        TYPEIS(scan_csts, pair);

        struct list *cst = scan_csts->car;
        TYPEIS(cst, pair);

        long ofs = GETINT(cst->cdr);
        assert(ofs >= 0);
        scan_csts = scan_csts->cdr;

        ncsts++;
      }
    UNGCPROV(gcproc);
  }

  for (struct list *scan_builtins = args->data[lv_builtins];
       scan_builtins;
       scan_builtins = scan_builtins->cdr)
    {
      TYPEIS(scan_builtins, pair);
      struct list *builtin = scan_builtins->car;
      TYPEIS(builtin, pair);
      TYPEIS(builtin->car, string);
      TYPEIS(builtin->cdr, integer);
    }

  {
    struct list *scan_globals = args->data[lv_globals];
    struct gcpro gcprog;
    GCPROV(gcprog, scan_globals);
    while (scan_globals != NULL)
      {
        TYPEIS(scan_globals, pair);
        struct list *lglobal = scan_globals->car;
        TYPEIS(lglobal, pair);
        TYPEIS(lglobal->cdr, integer);
        struct string *gname;
        if (TYPE(lglobal->car, pair))
          {
            struct list *gtype = lglobal->car;
            gname = gtype->car;
            /* 0 means global index, 1 means gidx * 2 + 1 */
            (void)GETRANGE(gtype->cdr, 0, 1);
          }
        else
          gname = lglobal->car;
        TYPEIS(gname, string);
        mglobal_lookup(gname); /* don't want GC later! */
        scan_globals = scan_globals->cdr;
      }
    UNGCPROV(gcprog);
  }

  check_primitives(args->data[lv_primitives]);

#ifdef __i386__
  const ulong npcrel = 0;
  ulong nrel = check_primitives(args->data[lv_rel_primitives]);

  for (struct list *seclevs = args->data[lv_seclevs];
       seclevs;
       seclevs = seclevs->cdr)
    {
      TYPEIS(seclevs, pair);
      struct list *e = seclevs->car;
      TYPEIS(e, pair);
      TYPEIS(e->car, integer);
      TYPEIS(e->cdr, integer);
    }
#else
  const ulong npcrel = GETRANGE(args->data[lv_rel_primitives], 0, ULONG_MAX);
  const ulong nrel = 0;
#endif

  /* Count relocatable builtins */
  for (struct list *scan_builtins = args->data[lv_builtins];
       scan_builtins;
       scan_builtins = scan_builtins->cdr)
    {
      struct list *builtin = scan_builtins->car;

      const struct builtin *b = builtin_find(builtin->car);
      if (b == NULL)
	runtime_error(error_bad_value);

#ifdef __i386__
      if (!b->absolute)
	nrel++;
#else
      assert(b->absolute);
#endif
    }

  struct mcode_fields mfields;
  mcode_fields_spec(&mfields, NULL, clen, npcrel, nrel, ncsts);

  /* The following gave a net slowdown, at least on the compiler */
  /* Avoid sharing cache lines with other objects */
  /*  size = MUDLLE_ALIGN(size, 32);*/

  ulong size = mfields.code_size + offsetof(struct mcode, mcode);

  /* allocate extra space to ensure that gen1 will have space for
     this object even with the aligned forwarding */
  struct mcode *newp = gc_allocate(size + CODE_ALIGNMENT - 1);
#ifdef GCDEBUG
  ulong generation = newp->code.o.generation;
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
        .generation   = generation,
#endif
      },
      .varname        = assert_immutable(args->data[lv_varname]),
      .filename       = assert_immutable(args->data[lv_filename]),
      .nicename       = assert_immutable(args->data[lv_nicename]),
      .help           = assert_immutable(args->data[lv_help]),
      .arguments.obj  = assert_immutable(args->data[lv_arguments]),
      .linenos        = args->data[lv_linenos],
      .lineno         = (alineno > 0 && alineno <= UINT16_MAX
                         ? alineno
                         : 1),
      .column         = (acolumn > 0 && acolumn < P(8)
                         ? acolumn
                         : 1),
      .seclevel       = seclev,
      .return_typeset = return_typeset
    },
    .nb_constants  = ncsts,
#ifdef __i386__
    .myself        = newp,
    .nb_rel        = nrel,
#elif defined __x86_64__
    .nb_pc_rel     = npcrel,
#endif
    .code_length   = clen,
    .closure_flags = flags,
    .return_itype  = return_itype,
    .magic         = "\xff\xff\xff\xff\xff\xff\xff\xff"
  };

  memcpy(newp->mcode, ((struct string *)args->data[lv_mcode])->str, clen);

  mcode_fields(&mfields, newp);

  static const char *const nops[] =
    {
      "",
      "\x90",
      "\x66\x90",
      "\x0f\x1f\x00",
      "\x0f\x1f\x40\x00",
      "\x0f\x1f\x44\x00\x00",
      "\x66\x0f\x1f\x44\x00\x00",
      "\x0f\x1f\x80\x00\x00\x00\x00",
    };
  assert(mfields.code_pad <= VLENGTH(nops));

  /* align with nop of appropriate length */
  memcpy(newp->mcode + clen, nops[mfields.code_pad], mfields.code_pad);

  /* Copy constants and their offsets */
  for (struct list *seclevs = args->data[lv_seclevs];
       seclevs;
       seclevs = seclevs->cdr)
    {
      assert(TYPE(seclevs, pair));
      struct list *e = seclevs->car;
      assert(TYPE(e, pair));
      assert(integerp(e->car));
      assert(integerp(e->cdr));
      long ofs = intval(e->cdr);
      enum {
        sl_c      = 0,
        sl_mudlle = 1,
        sl_maxlev = 2
      } type = intval(e->car);
      switch (type)
        {
        case sl_c:
          {
            uint16_t l = seclev;
            memcpy(newp->mcode + ofs, &l, sizeof l);
            break;
          }
        case sl_mudlle:
          {
            uint32_t l = (ulong)makeint(seclev);
            memcpy(newp->mcode + ofs, &l, sizeof l);
            break;
          }
        case sl_maxlev:
          {
            uint32_t l = (ulong)seclevel_to_maxseclevel(seclev);
            memcpy(newp->mcode + ofs, &l, sizeof l);
            break;
          }
        default:
          abort();
        }
    }

  for (struct list *scan_csts = args->data[lv_consts];
       scan_csts;
       scan_csts = scan_csts->cdr)
    {
      assert(TYPE(scan_csts, pair));
      struct list *cst = scan_csts->car;
      assert(TYPE(cst, pair));
      assert(immutablep(cst->car));
      long offset = intval(cst->cdr);
      value c = cst->car;
      set_cst(newp->mcode + offset, (ulong)c);
      mfields.cst_offsets = mfields.add_cst_ofs(mfields.cst_offsets, offset);
    }

  /* Set builtin addresses */
  for (struct list *scan_builtins = args->data[lv_builtins];
       scan_builtins;
       scan_builtins = scan_builtins->cdr)
    {
      assert(TYPE(scan_builtins, pair));
      struct list *builtin = scan_builtins->car;
      const struct builtin *b = builtin_find(builtin->car);
      long offset = intval(builtin->cdr);
      uint8_t *callins = newp->mcode + offset;

      if (b->absolute)
	set_cst(callins, (ulong)b->address);
      else
	{
	  set_cst(callins,
                  (ulong)b->address - (ulong)callins - sizeof (ulong));
	  /* need to remember offset (for relocation) */
          mfields.cst_offsets = mfields.add_cst_ofs(
            mfields.cst_offsets, offset);
	}
    }

  /* Set primitive addresses */
  for (struct list *scan_primitives = args->data[lv_primitives];
       scan_primitives;
       scan_primitives = scan_primitives->cdr)
    {
      assert(TYPE(scan_primitives, pair));
      struct list *primitive = scan_primitives->car;
      ulong offset = intval(primitive->cdr);
      ulong paddress = primitive_find(primitive->car);
      set_cst(newp->mcode + offset, paddress);
    }

#ifdef __i386__
  /* Set primitive addresses */
  for (struct list *scan_primitives = args->data[lv_rel_primitives];
       scan_primitives;
       scan_primitives = scan_primitives->cdr)
    {
      assert(TYPE(scan_primitives, pair));
      struct list *primitive = scan_primitives->car;
      long offset = intval(primitive->cdr);
      ulong paddress = primitive_find(primitive->car);
      set_cst(newp->mcode + offset,
              paddress - (ulong)(newp->mcode + offset + 4));
      /* need to remember offset (for relocation) */
      mfields.cst_offsets = mfields.add_cst_ofs(mfields.cst_offsets, offset);
    }
#endif

  /* Set global offsets */
  for (struct list *scan_globals = args->data[lv_globals];
       scan_globals;
       scan_globals = scan_globals->cdr)
    {
      assert(TYPE(scan_globals, pair));
      struct list *lglobal = scan_globals->car;
      long offset = intval(lglobal->cdr);

      ulong goffset;
      if (TYPE(lglobal->car, pair))
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
          goffset = (uint8_t *)&genv->data[goffset] - (uint8_t *)genv;
        }

      assert(goffset <= UINT32_MAX);
      uint32_t u = goffset;
      memcpy(newp->mcode + offset, &u, sizeof u);
    }

#ifdef GCSTATS
  gcstats_add_alloc(type_mcode, MUDLLE_ALIGN(size + CODE_ALIGNMENT,
                                             sizeof (value)));
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
  CHECK_TYPES(mcode, mcode);
  return alloc_closure0(&mcode->code);
}

TYPEDOP(closurep, "closure?", "`x -> `b. True if `x is a closure.",
	1, (value x),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(TYPE(x, closure));
}

TYPEDOP(securep, "secure?", "`x -> `b. True if `x is a secure primitive.",
	1, (value x),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(TYPE(x, secure));
}

TYPEDOP(primitivep, "primitive?", "`x -> `b. True if `x is a primitive."
        " See also `any_primitive?().",
	1, (value x),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(TYPE(x, primitive));
}

TYPEDOP(varargsp, "varargs?",
        "`x -> `b. True if `x is a variable argument primitive.",
	1, (value x),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "x.n")
{
  return makebool(TYPE(x, varargs));
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
  CHECK_TYPES(p, CT_TYPESET(TYPESET_PRIMITIVE));
  return makeint(p->op->nargs);
}

TYPEDOP(primitive_flags, 0, "`primitive -> `n. Returns flags of primitive.",
	1, (struct primitive *p),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "f.n")
{
  CHECK_TYPES(p, CT_TYPESET(TYPESET_PRIMITIVE));
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
  CHECK_TYPES(p, CT_TYPESET(TYPESET_PRIMITIVE));

  const char *const *atyping = p->op->type;
  if (atyping == NULL)
    return NULL;

  struct list *l = NULL;
  GCPRO(l);
  for (; *atyping; ++atyping)
    {
      size_t size = strlen(*atyping) + 1;
      char t[size];
      recurse_typing(t, size, *atyping, t, &l);
    }
  UNGCPRO();
  return l;
}

TYPEDOP(closure_arguments, 0, "`c -> `v. Returns a"
        " vector(`name|false . `typeset) for `c's arguments.\n"
        "`name is a string; `typeset is a bitfield of 1 << `type_xxx.\n"
        "Returns just `name for vararg closures.",
        1, (struct closure *c), OP_LEAF | OP_NOALLOC | OP_NOESCAPE,
        "f.[vs]")
{
  CHECK_TYPES(c, closure);
  return c->code->arguments.obj;
}

TYPEDOP(closure_return_typeset, 0, "`c -> `n. Returns possible return types of"
        " closure `c, a bitfield of 1 << `type_xxx.",
        1, (struct closure *c), OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "f.n")
{
  CHECK_TYPES(c, closure);
  return makeint(c->code->return_typeset);
}

TYPEDOP(closure_return_itype, 0, "`c -> `n. Returns possible return itypes of"
        " closure `c, a bitfield of `itype_xxx. Returns -1 for interpreted"
        " code.",
        1, (struct closure *c), OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "f.n")
{
  CHECK_TYPES(c, closure);
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
  CHECK_TYPES(c, CT_TYPES(closure, code, mcode));

  if (TYPE(c, closure))
    c = ((struct closure *)c)->code;

  if (TYPE(c, code))
    return makeint(0);

  if (TYPE(c, mcode))
    return makeint(((struct mcode *)c)->closure_flags);

  abort();
}

UNSAFETOP(closure_code, 0,
          "`f -> `c. Returns the code or mcode object of closure `f.",
          1, (struct closure *fn), OP_LEAF | OP_NOESCAPE, "f.o")
{
  CHECK_TYPES(fn, closure);
  return fn->code;
}

UNSAFETOP(global_table, 0, "-> `t. Returns global symbol table",
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
  long idx;
  CHECK_TYPES(midx, CT_RANGE(idx, 0, intval(environment->used) - 1));
  return GNAME(idx);
}

SECTOP(global_lookup, 0, "`s -> `n. Returns index of global variable `s."
       " Creates it if it doesn't exist already.",
       1, (struct string *name), SECLEVEL_GLOBALS,
       OP_LEAF | OP_NOESCAPE, "s.n")
{
  CHECK_TYPES(name, string);
  size_t nlen = string_len(name);
  if (nlen < 1 || nlen > MAX_VARIABLE_LENGTH)
    runtime_error(error_bad_value);
  return makeint(mglobal_lookup(name));
}

SECTOP(global_value, 0, "`n -> `x. Returns value of global variable `n"
       " for 0 <= `n < `global_names()",
       1, (value midx), SECLEVEL_GLOBALS,
       OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "n.x")
{
  long idx;
  CHECK_TYPES(midx, CT_RANGE(idx, 0, intval(environment->used) - 1));
  return GVAR(idx);
}

TYPEDOP(global_names, 0,
        "-> `n. Returns the number of globals used. See also `global_name()"
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
  long idx;
  CHECK_TYPES(midx, CT_RANGE(idx, 0, intval(environment->used) - 1),
              x,    any);
  if (GCONSTANT(idx))
    runtime_error(error_variable_read_only);
  GVAR(idx) = x;
  return x;
}

NOT_DEFINED(global_read);
TYPEDOP(global_read, 0, "", 1, (value midx), OP_OPERATOR, "n.x")
{
  /* only used to print error messages */
  undefined();
}

NOT_DEFINED(global_write);
TYPEDOP(global_write, 0, "", 2, (value midx, value x), OP_OPERATOR, "nx.2")
{
  /* only used to print error messages */
  undefined();
}

const struct prim_op *const global_read_ext = &op_global_read;
const struct prim_op *const global_write_ext = &op_global_write;

void global_runtime_error(enum runtime_error error, bool is_write,
                          ulong goffset, value val)
{
  primitive_runtime_error(error,
                          is_write ? &op_global_write : &op_global_read,
                          is_write ? 2 : 1, makeint(goffset), val);
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
  CHECK_TYPES(name, string);
  return makeint(module_status(name->str));
}

TYPEDOP(module_seclevel, 0, "`s -> `n. Returns seclevel of module `s",
	1, (struct string *name),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "s.n")
{
  CHECK_TYPES(name, string);
  return makeint(module_seclevel(name->str));
}

UNSAFETOP(module_set, "module_set!",
          "`s `n1 `n2 -> . Sets status of module `s to `n1, seclevel `n2",
          3, (struct string *name, value status, value seclev),
          OP_LEAF | OP_NOESCAPE, "snn.")
{
  CHECK_TYPES(name,   string,
              status, integer,
              seclev, integer);

  char *tname;
  ALLOCA_STRING(tname, name, MAX_MODULE_NAME_LENGHT);
  if (tname == NULL)
    runtime_error(error_bad_value);

  module_set(tname, intval(status), intval(seclev));

  undefined();
}

UNSAFETOP(module_unload, 0, "`s -> `b. Unload module `s, false if protected",
          1, (struct string *name),
          OP_LEAF | OP_NOALLOC, "s.n")
{
  CHECK_TYPES(name, string);
  return makebool(module_unload(name->str));
}

TYPEDOP(module_require, 0,
        "`s -> `n. Load module s if needed, return its new status",
	1, (struct string *name),
	0, "s.n")
{
  CHECK_TYPES(name, string);
  char *tname;
  ALLOCA_STRING(tname, name, MAX_MODULE_NAME_LENGHT);
  if (tname == NULL)
    runtime_error(error_bad_value);

  return makeint(module_require(tname));
}

TYPEDOP(module_vstatus, 0,
        "`n0 -> `s/`n1. Return status of global variable `n0; either the"
        " name of the defining library, or one of:\n"
        "  `var_normal          \tnormal variable\n"
        "  `var_write           \twritten by mudlle\n"
        "  `var_system_write    \tanyone can read, but mudlle cannot write\n"
        "  `var_system_mutable  \tanyone may read or write",
	1, (value goffset),
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "n.[sn]")
{
  long idx;
  CHECK_TYPES(goffset, CT_RANGE(idx, 0, intval(environment->used) - 1));

  struct string *mod;
  enum vstatus status = module_vstatus(idx, &mod);
  if (status == var_module)
    {
      assert(TYPE(mod, string));
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
  long idx;
  CHECK_TYPES(goffset, CT_RANGE(idx, 0, intval(environment->used) - 1),
              status,  CT_TYPES(string, integer));
  struct string *mod = NULL;
  if (!integerp(status))
    {
      mod = status;
      status = makeint(var_module);
    }
  else if (status != makeint(var_normal) && status != makeint(var_write))
    RUNTIME_ERROR(error_bad_value, NULL);
  return makebool(module_vset(idx, intval(status), mod));
}

UNSAFETOP(module_table, 0, "-> `t. Returns the table of module status for"
          " each named mudlle library. Each entry is a vector(`status,"
          " `seclev) where `status is as returned by `module_status().",
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
  DEFINE(dwarf_line_number_info);
  DEFINE(register_mcode_module);
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

  system_define("mc:max_local_vars", makeint(MAX_LOCAL_VARS));

#if defined __i386__ || defined __x86_64__
 #ifdef __i386__
  #define PREFIX "x86:"
  #define ARCH 0x86
 #else
  #define PREFIX "x64:"
  #define ARCH 0x64
 #endif

  system_define("mc:arch", makeint(ARCH));
  system_define("mc:mcode_version", makeint(MCODE_VERSION));

  system_define(PREFIX "mcode_code_offset",
                makeint(offsetof(struct mcode, mcode)));

  system_define(PREFIX "object_offset", makeint(sizeof (struct obj)));
  system_define(PREFIX "object_size",   makeint(offsetof(struct obj, size)));
  system_define(PREFIX "object_info",
                makeint(offsetof(struct obj, size)
                        + sizeoffield(struct obj, size)));
  system_define(PREFIX "object_type",
                makeint(offsetof(struct obj, flags) - 1));
  system_define(PREFIX "object_flags",  makeint(offsetof(struct obj, flags)));

#ifndef NOCOMPILER
  system_define(PREFIX "cc_frame_end_sp",
                makeint(offsetof(struct ccontext, frame_end_sp)));
  system_define(PREFIX "cc_frame_end_bp",
                makeint(offsetof(struct ccontext, frame_end_bp)));
  system_define(PREFIX "cc_callee",
                makeint(offsetof(struct ccontext, callee)));
#endif /* ! NOCOMPILER */
#endif /* __i386__ || __x86_64__*/
}
