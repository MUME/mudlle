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

/* needed for REG_xxx constants for signal contexts */
#define _GNU_SOURCE

#include <stdlib.h>
#include <signal.h>

#include "../context.h"
#include "../module.h"

#include "arith.h"
#include "basic.h"
#include "bigint.h"
#include "bitset.h"
#include "bool.h"
#include "debug.h"
#include "files.h"
#include "io.h"
#include "list.h"
#include "mudlle-float.h"
#include "pattern.h"
#include "runtime.h"
#include "stringops.h"
#include "support.h"
#include "symbol.h"
#include "vector.h"
#include "xml.h"

#ifdef AMIGA
  #include <dos.h>
#endif


#include <stdio.h>
#include <string.h>

static ulong op_count;
static FILE *ops, *binops;
static struct string *system_module;

#define MAXNAME 32

void system_define(const char *name, value val)
/* Modifies: environment
   Requires: name not already exist in environment.
   Effects: Adds name to environment, with value val for the variable,
     as a 'define' of the system module.
*/
{
  GCPRO1(val);
  ulong aindex = global_lookup(name);
  UNGCPRO();

  assert(!GCONSTANT(aindex));

  GVAR(aindex) = val;
  module_vset(aindex, var_module, system_module);
}

void system_write(const char *name, value val)
/* Modifies: environment
   Requires: name not already exist in environment.
   Effects: Adds name to environment, with value val for the variable,
     as a 'define' of the system module.
*/
{
  GCPRO1(val);
  ulong aindex = global_lookup(name);
  UNGCPRO();

  assert(!GCONSTANT(aindex));

  GVAR(aindex) = val;
  module_vset(aindex, var_system_write, NULL);
}

struct vector *define_string_vector(const char *name, const char *const *vec,
                                    int count)
{
  if (count < 0)
    for (count = 0; vec[count]; ++count)
      ;

  struct vector *v = alloc_vector(count);

  GCPRO1(v);

  for (int n = 0; n < count; ++n)
    v->data[n] = make_readonly(alloc_string(vec[n]));
  UNGCPRO();
  make_readonly(v);
  if (name != NULL)
    system_define(name, v);
  return v;
}

void define_int_vector(const char *name, const int *vec, int count)
{
  struct vector *v = alloc_vector(count);

  GCPRO1(v);

  for (int n = 0; n < count; ++n)
    v->data[n] = makeint(vec[n]);

  UNGCPRO();
  system_define(name, make_readonly(v));
}

#define tassert(what) do {                                      \
  if (!(what))                                                  \
    {                                                           \
      fprintf(stderr, "%s:%d: %s: Assertion `%s' failed.\n",    \
              op->filename, op->lineno, op->name, #what);       \
      abort();                                                  \
    }                                                           \
} while (0)

static void validate_typing(const struct primitive_ext *op)
{
  static const char allowed[] = "fnsvluktryxo123456789SdDbB";

  if (op->type == NULL)
    return;

  for (const char *const *type = op->type; *type; ++type)
    {
      const char *this = *type;
      int argc = 0;
      bool saw_period = false;
      for (const char *s = this; *s; ++s)
        {
          if (*s == '.')
            {
              saw_period = true;
              ++s;
              if (*s == 0)
                break;
            }
          if (*s == '[')
            {
              for (;;)
                {
                  ++s;
                  tassert(*s);
                  if (*s == ']')
                    break;
                  tassert(strchr(allowed, *s));
                }
            }
          else if (*s == '*')
            {
              tassert(s > this && s[1] == '.' && !saw_period);
              continue;
            }
          else
            tassert(strchr(allowed, *s));
          if (saw_period)
            {
              tassert(!s[1]);
              break;
            }
          ++argc;
        }

      tassert(saw_period);

      if (op->nargs >= 0)
        tassert(op->nargs == argc);
    }
}

#undef tassert

static struct {
  size_t size, used;
  const struct primitive_ext **ops;
  bool locked;
} primitives;

void runtime_define(const struct primitive_ext *op)
{
  assert(op->nargs <= MAX_PRIMITIVE_ARGS);
  assert(!primitives.locked);

  validate_typing(op);

  if (binops)
    {
      char bname[MAXNAME];
      bname[MAXNAME - 1] = '\0';
      strncpy(bname, op->name, MAXNAME - 1);
      fwrite(bname, MAXNAME, 1, binops);
    }

  struct primitive *prim;
  if (op->seclevel > 0)
    {
      prim = alloc_secure(op_count++, op);
      if (ops) fprintf(ops, "%-20s %s SECURITY %d\n", op->name, op->help,
                       op->seclevel);
    }
  else
    {
      prim = alloc_primitive(op_count++, op);

      if (op->nargs < 0)	/* Varargs */
        {
          prim->o.type = type_varargs;
          assert(~op->flags & OP_NOALLOC);
        }
      if (ops) fprintf(ops, "%-20s %s\n", op->name, op->help);
    }

  if (primitives.used == primitives.size)
    {
      primitives.size = primitives.size ? primitives.size * 2 : 16;
      primitives.ops = realloc(primitives.ops,
                               primitives.size * sizeof primitives.ops[0]);
    }
  primitives.ops[primitives.used++] = op;

  system_define(op->name, prim);
}

static int cmp_ops(const void *_a, const void *_b)
{
  ulong a = (ulong)(*(const struct primitive_ext *const *)_a)->op;
  ulong b = (ulong)(*(const struct primitive_ext *const *)_b)->op;
  return a < b ? -1 : a > b;
}

static void sort_primitives(void)
{
  assert(!primitives.locked);
  primitives.ops = realloc(primitives.ops,
                           primitives.used * sizeof primitives.ops[0]);
  primitives.size = primitives.used;
  qsort(primitives.ops, primitives.used, sizeof primitives.ops[0],
        cmp_ops);
  primitives.locked = true;
}

const struct primitive_ext *lookup_primitive(ulong adr)
{
  assert(primitives.locked);
  struct primitive_ext key;
  key.op = (value (*)())adr;
  void *keyptr = &key;
  const struct primitive_ext **op = bsearch(
    &keyptr, primitives.ops, primitives.used,
    sizeof primitives.ops[0], cmp_ops);
  return op ? (*op) : NULL;
}

#ifdef MUDLLE_INTERRUPT
static bool interrupted = false;

void check_interrupt(void)
/* Effects: Causes a user_interrupt runtime error if interrupted is true
     (user caused SIGINT or SIGQUIT)
*/
{
#ifdef AMIGA
  chkabort();
#endif
  if (interrupted)
    {
      interrupted = false;
      runtime_error(error_user_interrupt);
    }
}

static void catchint(int sig)
{
  xcount = 1;
  interrupted = true;
}
#endif  /* MUDLLE_INTERRUPT */

#if defined(i386) && !defined(NOCOMPILER)

#    undef USE_SYS_UCONTEXT
#    if defined(__GLIBC__) && (defined(REG_EIP) || __GLIBC_MINOR__ >= 3)
#        include <sys/ucontext.h>
#        define USE_SYS_UCONTEXT
#    elif defined(__MACH__)
#        include <sys/ucontext.h>
#    elif defined(SA_SIGINFO)
#        include <asm/ucontext.h>
#    elif !defined(__GLIBC__) || __GLIBC__ < 2 || \
          (__GLIBC__ == 2 && __GLIBC_MINOR__ <= 1)
#        define sigcontext sigcontext_struct
#        include <asm/sigcontext.h>
#    elif (__GLIBC__ != 2 && __GLIBC_MINOR__ != 2)
#        include <sigcontext.h>
#    endif

#ifdef __MACH__
    typedef mcontext_t reg_context_t;
    #define GETREG(ctx, reg, REG) (*(ctx))->__ss.__ ## reg
    #define UCONTEXT_T ucontext_t
#elif defined USE_SYS_UCONTEXT
    typedef mcontext_t reg_context_t;
    #define GETREG(ctx, reg, REG) (ctx)->gregs[REG_ ## REG]
    #define UCONTEXT_T struct ucontext
#else
    typedef struct sigcontext reg_context_t;
    #define GETREG(ctx, reg, REG) (ctx)->reg
    #define UCONTEXT_T struct ucontext
#endif

#include <stddef.h>
#include "../builtins.h"

#ifdef SA_SIGINFO
static struct sigaction oldsegact;
#ifdef __MACH__
static struct sigaction oldbusact;
#endif
#endif

void got_real_segv(int sig)
{
  /*
   * reinstall the default handler, this will cause
   * a real crash (instead of the annoying abort
   * broken stack frame)
   */
#ifdef SA_SIGINFO
  if (sig == SIGSEGV)
    sigaction(sig, &oldsegact, NULL);
#ifdef __MACH__
  if (sig == SIGBUS)
    sigaction(sig, &oldbusact, NULL);
#endif
  abort();
#else
  signal(sig, SIG_DFL);
#endif
}

static inline void check_segv(int sig, reg_context_t *scp)
{
  const ubyte *eip = (const ubyte *)GETREG(scp, eip, EIP);

  if ((eip[0] == 0x80           /* cmpb $imm,object_type(reg) */
       && (eip[1] & 0xf8) == 0x78
       && eip[2] == 5)          /* offsetof(struct obj, type) */
      || (eip[0] == 0x0f        /* movzbl object_type(%ecx),%eax */
          && eip[1] == 0xb6
          && (eip[2] & 0xc0) == 0x40
          && eip[3] == 5))      /* offsetof(struct obj, type) */
    {
      /* mudlle code */
      if (eip >= gcblock && eip < gcblock + gcblocksize)
	{
	  ccontext.frame_end_sp = (ulong *)GETREG(scp, esp, ESP);
	  ccontext.frame_end_bp = (ulong *)GETREG(scp, ebp, EBP);
          /* make stack trace printing start from the right place */
	  ccontext.frame_end_sp[-1] = (ulong)eip;
	  runtime_error(error_bad_type);
	}
      /* bref and bset */
      if (eip >= (ubyte *)bref && eip < (ubyte *)bwglobal)
	{
	  ccontext.frame_end_sp = (ulong *)GETREG(scp, esp, ESP) + 1;
	  ccontext.frame_end_bp = (ulong *)GETREG(scp, ebp, EBP);
          bool is_set = eip >= (ubyte *)bset;
          if (is_set)
            set_runtime_error(error_bad_type,
                              (value)GETREG(scp, eax, EAX),
                              (value)GETREG(scp, ecx, ECX),
                              (value)GETREG(scp, edx, EDX));
          else
            ref_runtime_error(error_bad_type,
                              (value)GETREG(scp, eax, EAX),
                              (value)GETREG(scp, ecx, ECX));
	}
      /* bcall */
      if (eip >= (ubyte *)bcall && eip < (ubyte *)bcall_secure)
	{
	  ccontext.frame_end_sp = (ulong *)GETREG(scp, esp, ESP) + 1;
	  ccontext.frame_end_bp = (ulong *)GETREG(scp, ebp, EBP);
	  runtime_error(error_bad_function);
	}
    }

  got_real_segv(sig);
}

#ifdef SA_SIGINFO
static void catchsegv(int sig, siginfo_t *siginfo, void *_sigcontext)
{
  UCONTEXT_T *sigcontext = _sigcontext;
  check_segv(sig, &sigcontext->uc_mcontext);
}
#else
static void catchsegv(int sig, struct sigcontext scp)
{
  check_segv(sig, &scp);
}
#endif

#endif  /* i386 && !NOCOMPILER */

#ifdef sparc
#include "builtins.h"
#ifdef __SVR4
#include <sys/ucontext.h>
#endif

#ifdef __SVR4
static void cause_error(int nerror, ucontext_t *uap)
{
  /* Set up frame info after trap, call error handling */
  ulong *frame = (ulong *)uap->uc_mcontext.gregs[REG_SP];
  ulong pc = (ulong)uap->uc_mcontext.gregs[REG_PC];
  ulong retpc = (ulong)uap->uc_mcontext.gregs[REG_O7];

  flush_windows();
#else
static void cause_error(int nerror, struct sigcontext *scp)
{
  /* Set up frame info after trap, call error handling */
  register ulong *fp asm("fp");
  ulong *frame;
#ifndef sparc
  ulong pc = (ulong)scp->sc_pc;
#else
  ulong pc;
#endif
  ulong retpc;

  flush_windows();
  /* Surely there must be a better way to find these ? */
  frame  = (ulong *)(((ulong *)fp[14])[14] + 0x8b0);
  retpc = fp[15];
#endif

  /* we stick an appropriate pc in l1 spot */
  if (pc > (ulong)gcblock && pc < (ulong)gcblock + gcblocksize)
    frame[1] = pc; /* fault in compiled code */
  else /* probably fault in builtin lib */
    if (retpc > (ulong)gcblock && retpc < (ulong)gcblock + gcblocksize)
      frame[1] = retpc;
    else /* hmm - who knows what's up ? */
      frame[1] = 0;

  ccontext.frame_end = frame;
  runtime_error((runtime_errors)nerror);
}

#ifdef __SVR4

struct sigaction illact;
struct sigaction segact;

/* Catch runtime errors */
void catchill(int sig, siginfo_t *sip, ucontext_t *uap)
{
  ulong trapins = *(ulong *)sip->si_addr;
  int nerror;

  /* Check if it was a trap for a runtime error
     (numbers 16 to 16 + last_runtime_error - 1) */
  if ((trapins & ~(255 | 15 << 25)) == (2 << 30 | 58 << 19 | 1 << 13) &&
      (nerror = (int)(trapins & 255) - 16) >= 0 &&
      nerror < last_runtime_error)
    cause_error(nerror, uap);

  abort(); /* Really an illegal instruction */
}

void catchsegv(int sig, siginfo_t *sip, ucontext_t *uap)
{
  ulong trapins = *(ulong *)uap->uc_mcontext.gregs[REG_PC];

  /* Check if it was a type check (ie: lduh [x+4],g3) */
  if ((trapins & ~(31 << 14)) == (3 << 30 | 3 << 25 | 2 << 19 | 1 << 13 | 4))
    cause_error(error_bad_type, uap);
  /* or a function check (ie: lduh [x+4],l0) */
  else if ((trapins & ~(31 << 14)) == (3 << 30 | 16 << 25 | 2 << 19
                                       | 1 << 13 | 4))
    cause_error(error_bad_function, uap);

  abort(); /* Really an illegal instruction */
}
#else
/* Catch runtime errors */
void catchill(int sig, int code, struct sigcontext *scp, char *addr)
{
  ulong trapins = *(ulong *)addr;
  int nerror;

  /* Check if it was a trap for a runtime error
     (numbers 16 to 16 + last_runtime_error - 1) */
  if ((trapins & ~(255 | 15 << 25)) == (2 << 30 | 58 << 19 | 1 << 13) &&
      (nerror = (int)(trapins & 255) - 16) >= 0 &&
      nerror < last_runtime_error)
    {
      /* Yes ... */
      /* reset handler */
      sigsetmask(0);
      signal(SIGILL, catchill);
      cause_error(nerror, scp);
    }
  abort(); /* Really an illegal instruction */
}

void catchsegv(int sig, int code, struct sigcontext *scp, char *addr)
{
#ifndef sparc
  ulong trapins = *(ulong *)scp->sc_pc;
#else
  ulong trapins;
#endif
  int nerror;

  /* Check if it was a type check (ie: lduh [x+4],g3) */
  if ((trapins & ~(31 << 14)) == (3 << 30 | 3 << 25 | 2 << 19 | 1 << 13 | 4))
    {
      /* Yes ... */
      /* reset handler */
      sigsetmask(0);
      signal(sig, catchsegv);
      cause_error(error_bad_type, scp);
    }
  /* or a function check (ie: lduh [x+4],l0) */
  else if ((trapins & ~(31 << 14)) == (3 << 30 | 16 << 25 | 2 << 19
                                       | 1 << 13 | 4))
    {
      /* Yes ... */
      /* reset handler */
      sigsetmask(0);
      signal(sig, catchsegv);
      cause_error(error_bad_function, scp);
    }
  abort(); /* Really an illegal instruction */
}
#endif
#endif

void flag_check_failed(const struct primitive_ext *op, const char *name)
{
  abort();
}

STATIC_STRING(static_obj_empty_string, "");
struct string *const static_empty_string
  = GET_STATIC_STRING(static_obj_empty_string);

void runtime_init(void)
{
  ops = fopen("mudlle-functions", "w+");
  binops = fopen("mudlle-primitives", "w+");
  op_count = 0;
  system_module = alloc_string("system");
  staticpro(&system_module);

#ifdef MUDLLE_INTERRUPT
#ifdef __SVR4
  {
    struct sigaction act;
    act.sa_sigaction = (void *)catchint;
    act.sa_flags = SA_SIGINFO | SA_NODEFER | SA_RESTART;
    sigemptyset(&act.sa_mask);
    sigaction(SIGINT, &act, NULL);
#ifdef SIGQUIT
    sigaction(SIGQUIT, &act, NULL);
#endif
  }
#else
  signal(SIGINT, catchint);
#ifdef SIGQUIT
  signal(SIGQUIT, catchint);
#endif
#endif
#endif

#ifdef sparc
#ifdef __SVR4
  illact.sa_sigaction = (void *)catchill;
  illact.sa_flags = SA_SIGINFO | SA_NODEFER | SA_RESTART;
  sigaction(SIGILL, &illact, NULL);
  segact.sa_sigaction = (void *)catchsegv;
  segact.sa_flags = SA_SIGINFO | SA_NODEFER | SA_RESTART;
  sigaction(SIGSEGV, &segact, NULL);
  sigaction(SIGBUS, &segact, NULL);
#else
  signal(SIGILL, catchill);
  signal(SIGSEGV, catchsegv);
  signal(SIGBUS, catchsegv);
#endif
#endif

#if defined(i386) && !defined(NOCOMPILER)
#ifdef SA_SIGINFO
  {
    static char my_signal_stack[16 * 1024];
    stack_t my_stack;
    struct sigaction sact;
    int noaltstack;

    my_stack.ss_sp = my_signal_stack; /* we should point to the beginning */
    my_stack.ss_flags = SS_ONSTACK;
    my_stack.ss_size = sizeof my_signal_stack;
    if ((noaltstack = sigaltstack(&my_stack, NULL) < 0))
      {
	;
      }

    sact.sa_sigaction = catchsegv;
    sigemptyset(&sact.sa_mask);
    sact.sa_flags = SA_SIGINFO | SA_RESTART | SA_NODEFER;
    if (!noaltstack)
      sact.sa_flags |= SA_ONSTACK;
    sigaction(SIGSEGV, &sact, &oldsegact);
#ifdef __MACH__
    sigaction(SIGBUS, &sact, &oldbusact);
#endif
  }
#else
  signal(SIGSEGV, (void (*)(int))catchsegv);
#endif
#endif

  basic_init();
  debug_init();
  arith_init();
  bool_init();
  io_init();
  symbol_init();
  string_init();
  list_init();
  vector_init();
  support_init();
  bitset_init();
  files_init();
  float_init();
  bigint_init();
  pattern_init();
  mudlle_consts_init();
  xml_init();
#if 0
#endif
  module_set("system", module_protected, 0);

  sort_primitives();

  if (ops) fclose(ops);
  if (binops) fclose(binops);
}
