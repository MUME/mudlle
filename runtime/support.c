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
#include "support.h"

#include "../alloc.h"
#include "../builtins.h"
#include "../calloc.h"
#include "../context.h"
#include "../global.h"
#include "../lexer.h"
#include "../module.h"
#include "../mparser.h"
#include "../tree.h"
#include "../utils.h"

#if defined(i386) && !defined(NOCOMPILER)
static int cc_length = 0;	/* statistics */
#endif

TYPEDOP(mudlle_parse, 0, "`s0 `s1 -> `v. Parses mudlle expression `s0 and"
        " returns a parse tree, or false if unsuccessful.\n`s1 is the"
        " filename; can be null for \"<string>\".",
        2, (struct string *code, struct string *name),
        OP_LEAF | OP_NOESCAPE,
        "s[su].[vz]")
{
  TYPEIS(code, type_string);
  if (name)
    TYPEIS(name, type_string);

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

  TYPEIS(filename, type_string);
  TYPEIS(name, type_string);
  TYPEIS(nicename, type_string);
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
#ifdef i386
  {"badd",                          badd},
  {"balloc_closure",                balloc_closure},
  {"balloc_cons",                   balloc_cons},
  {"balloc_variable",               balloc_variable},
  {"balloc_vector",                 balloc_vector},
  {"bapply_varargs",                bapply_varargs},
  {"bcall",                         bcall},
  {"bcall_secure",                  bcall_secure},
  {"bcall_varargs",                 bcall_varargs},
  {"bcleargc",                      bcleargc},
  {"bcleargc0",                     bcleargc0},
  {"bcleargc1",                     bcleargc1},
  {"bcleargc2",                     bcleargc2},
  {"bcleargc3",                     bcleargc3},
  {"bcleargc4",                     bcleargc4},
  {"bconcat",                       bconcat},
  {"bdivide",                       bdivide},
  {"bearly_error_wrong_parameters", bearly_error_wrong_parameters},
  {"berror_abort",                  berror_abort},
  {"berror_bad_function",           berror_bad_function},
  {"berror_bad_index",              berror_bad_index},
  {"berror_bad_type",               berror_bad_type},
  {"berror_bad_value",              berror_bad_value},
  {"berror_compile",                berror_compile},
  {"berror_divide_by_zero",         berror_divide_by_zero},
  {"berror_loop",                   berror_loop},
  {"berror_no_match",               berror_no_match},
  {"berror_recurse",                berror_recurse},
  {"berror_security_violation",     berror_security_violation},
  {"berror_stack_underflow",        berror_stack_underflow},
  {"berror_user_interrupt",         berror_user_interrupt},
  {"berror_value_read_only",        berror_value_read_only},
  {"berror_variable_read_only",     berror_variable_read_only},
  {"berror_wrong_parameters",       berror_wrong_parameters},
  {"bmultiply",                     bmultiply},
  {"bref",                          bref},
  {"bremainder",                    bremainder},
  {"brestore_caller",               brestore_caller},
  {"brglobal",                      brglobal},
  {"bsave_caller",                  bsave_caller},
  {"bsave_caller_noalloc",          bsave_caller_noalloc},
  {"bset",                          bset},
  {"bshift_left",                   bshift_left},
  {"bshift_right",                  bshift_right},
  {"btypeof",                       btypeof},
  {"bvarargs",                      bvarargs},
  {"bwglobal",                      bwglobal},
  {"env_values",                    &env_values, true},
  {"max_loop_count",                makeint(MAX_LOOP_COUNT), true},
  {"maxseclevel",                   &maxseclevel, true},
  {"xcount",                        &xcount, true },
#else  /* ! i386 */
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

  if (!(TYPE(p, type_primitive)
        || TYPE(p, type_varargs)))
    return 0;

  return (ulong)p->op->op;
}

#ifdef i386
static inline void set_cst(ubyte *x, ulong n)
{
  memcpy(x, &n, sizeof n);
}
#endif

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

static inline value assert_immutable(value v)
{
  assert(immutablep(v));
  return v;
}

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

#endif /* ! NOCOMPILER */

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
#ifdef NOCOMPILER
  runtime_error(error_bad_value);
#else  /* !NOCOMPILER */

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

#define OPT_TYPEIS(v, t) do { if (v) TYPEIS(v, t); } while (0)
  OPT_TYPEIS(args->data[lv_help],      type_string);
  OPT_TYPEIS(args->data[lv_varname],   type_string);
  OPT_TYPEIS(args->data[lv_filename],  type_string);
  OPT_TYPEIS(args->data[lv_nicename],  type_string);
  OPT_TYPEIS(args->data[lv_arg_types], type_vector);
#undef OPT_TYPEIS
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
            /* 0 means global index, 1 means gidx * 2 + 1 */
            (void)GETRANGE(gtype->cdr, 0, 1);
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

#ifdef i386
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

      const struct builtin *b = builtin_find(builtin->car);
      if (b == NULL)
	runtime_error(error_bad_value);

      if (!b->absolute)
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
    case 1:                         /* nop */
      newp->mcode[clen]     = 0x90;
      break;
    case 2:
      newp->mcode[clen]     = 0x66; /* xchg %ax,%ax */
      newp->mcode[clen + 1] = 0x90;
      break;
    case 3:
      newp->mcode[clen]     = 0x0f; /* nopl (%eax) */
      newp->mcode[clen + 1] = 0x1f;
      newp->mcode[clen + 2] = 0x00;
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
      const struct builtin *b = builtin_find(builtin->car);
      long offset = intval(builtin->cdr);
      ubyte *callins = newp->mcode + offset;

      if (b->absolute)
	set_cst(callins, (ulong)b->address);
      else
	{
	  set_cst(callins,
                  (ulong)b->address - (ulong)callins - sizeof (ulong));
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

#else  /* ! i386 */
  #error Unsupported architecture
#endif

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
  long idx = GETRANGE(midx, 0, intval(environment->used) - 1);
  return GNAME(idx);
}

SECTOP(global_lookup, 0, "`s -> `n. Returns index of global variable `s."
       " Creates it if it doesn't exist already.",
       1, (struct string *name), SECLEVEL_GLOBALS,
       OP_LEAF | OP_NOESCAPE, "s.n")
{
  TYPEIS(name, type_string);
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
  long idx = GETRANGE(midx, 0, intval(environment->used) - 1);
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
  long idx = GETRANGE(midx, 0, intval(environment->used) - 1);
  if (GCONSTANT(idx))
    runtime_error(error_variable_read_only);

  GVAR(idx) = x;
  return x;
}

TYPEDOP(global_read, 0, "", 1, (value midx), OP_OPERATOR, "n.x")
{
  /* only used to print error messages */
  undefined();
}

TYPEDOP(global_write, 0, "", 2, (value midx, value x), OP_OPERATOR, "nx.2")
{
  /* only used to print error messages */
  undefined();
}

const struct primitive_ext *const global_read_ext = &op_global_read;
const struct primitive_ext *const global_write_ext = &op_global_write;

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
  ALLOCA_STRING(tname, name, MAX_MODULE_NAME_LENGHT);
  if (tname == NULL)
    runtime_error(error_bad_value);

  module_set(tname, GETINT(status), GETINT(seclev));

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
  TYPEIS(name, type_string);
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
	OP_LEAF | OP_NOALLOC | OP_NOESCAPE, "n.S")
{
  long idx = GETRANGE(goffset, 0, intval(environment->used) - 1);

  struct string *mod;
  enum vstatus status = module_vstatus(idx, &mod);
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
  struct string *mod;

  long idx = GETRANGE(goffset, 0, intval(environment->used) - 1);

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
}
