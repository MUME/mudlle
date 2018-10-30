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

#ifdef __linux__
  /* needed for REG_xxx constants for signal contexts */
  #define _GNU_SOURCE
#endif

#include "../mudlle-config.h"

#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../alloc.h"
#include "../context.h"
#include "../global.h"
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
#include "mudlle-xml.h"
#include "pattern.h"
#include "runtime.h"
#include "mudlle-string.h"
#include "support.h"
#include "symbol.h"
#include "vector.h"


static ulong op_count;
static struct string *system_module;

static void system_set(ulong idx, value val)
{
  /* Technically this is not required, but you should think hard about whether
     adding a mutable or writable global is the right thing to do. */
  assert(readonlyp(val));
  if (!immutablep(val))
    {
      check_immutable(val);
      assert(immutablep(val));
    }

  assert(!GCONSTANT(idx));

  GVAR(idx) = val;
  module_vset(idx, var_module, system_module);
}

void system_string_define(struct string *name, value val)
{
  GCPRO(val);
  ulong idx = mglobal_lookup(name);
  UNGCPRO();
  system_set(idx, val);
}

void system_write(struct string *name, value val)
/* Modifies: environment
   Requires: name not already exist in environment.
   Effects: Adds name to environment, with value val for the variable,
     as a 'define' of the system module.
*/
{
  GCPRO(val);
  ulong aindex = mglobal_lookup(name);
  UNGCPRO();

  if (!module_vset(aindex, var_system_write, NULL))
    abort();
  GVAR(aindex) = val;
}

struct vector *define_mstring_vector(struct string *name,
                                     const char *const *vec,
                                     int count)
{
  if (name != NULL)
    assert(name->o.garbage_type == garbage_static_string);

  if (count < 0)
    for (count = 0; vec[count]; ++count)
      ;

  struct vector *v = alloc_vector(count);

  GCPRO(v);
  for (int n = 0; n < count; ++n)
    SET_VECTOR(v, n, make_readonly(alloc_string(vec[n])));
  UNGCPRO();
  make_readonly(v);
  if (!check_immutable(&v->o))
    abort();
  if (name != NULL)
    system_string_define(name, v);
  return v;
}

void define_int_vector(struct string *name, const int *vec, int count)
{
  assert(name->o.garbage_type == garbage_static_string);

  struct vector *v = alloc_vector(count);
  GCPRO(v);
  for (int n = 0; n < count; ++n)
    v->data[n] = makeint(vec[n]);
  UNGCPRO();
  if (!check_immutable(make_readonly(v)))
    abort();
  system_string_define(name, v);
}

#define tassert(what) do {                                      \
  if (!(what))                                                  \
    {                                                           \
      fprintf(stderr, "%s:%d: %s: Assertion `%s' failed.\n",    \
              op->filename, op->lineno, op->name, #what);       \
      abort();                                                  \
    }                                                           \
} while (0)

static void validate_typing(const struct prim_op *op)
{
  static const char allowed[] = "fnzZsvluktryxo123456789dDbB";

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
              tassert(s[-1] != ']'); /* not supported by recurse_typing */
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
  const struct prim_op **ops;
  bool locked;
} primitives;

void runtime_define(const struct prim_op *op)
{
  assert(op->nargs <= MAX_PRIMITIVE_ARGS);
  assert(!primitives.locked);

  ulong idx = global_lookup(op->name);

  validate_typing(op);

  struct primitive *prim = allocate_primitive(op);

  if (primitives.used == primitives.size)
    {
      primitives.size = primitives.size ? primitives.size * 2 : 16;
      primitives.ops = realloc(primitives.ops,
                               primitives.size * sizeof primitives.ops[0]);
    }
  primitives.ops[primitives.used++] = op;

  system_set(idx, prim);
}

static int cmp_ops(const void *_a, const void *_b)
{
  ulong a = (ulong)(*(const struct prim_op *const *)_a)->op;
  ulong b = (ulong)(*(const struct prim_op *const *)_b)->op;
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

const struct prim_op *lookup_primitive(ulong adr)
{
  assert(primitives.locked);
  struct prim_op key;
  key.op = (value (*)())adr;
  void *keyptr = &key;
  const struct prim_op **op = bsearch(
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

#if (defined __i386__ || defined __x86_64__) && !defined NOCOMPILER

#undef USE_SYS_UCONTEXT
#if __GLIBC__ == 2 && (defined REG_EIP || __GLIBC_MINOR__ >= 3)
#  include <sys/ucontext.h>
#  define USE_SYS_UCONTEXT
#elif defined __MACH__
#  include <sys/ucontext.h>
#elif defined SA_SIGINFO
#  include <asm/ucontext.h>
#elif __GLIBC__ < 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ <= 1)
#  define sigcontext sigcontext_struct
#  include <asm/sigcontext.h>
#elif __GLIBC__ != 2 && __GLIBC_MINOR__ != 2
#  include <sigcontext.h>
#endif

#ifdef __MACH__
  #define REG_CONTEXT_T mcontext_t
  #define GETREG(ctx, reg, REG) (*(ctx))->__ss.__ ## reg
  #define UCONTEXT_T ucontext_t
#elif defined USE_SYS_UCONTEXT
  #define REG_CONTEXT_T mcontext_t
  #define GETREG(ctx, reg, REG) (ctx)->gregs[REG_ ## REG]
  #if __GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 27)
    #define UCONTEXT_T struct ucontext_t
  #else
    #define UCONTEXT_T struct ucontext
  #endif
#else
  #define REG_CONTEXT_T struct sigcontext
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

/* export for gdb */
void got_real_segv(int sig);
void got_real_segv(int sig)
{
  /* reinstall the default handler; this will cause a real crash */
#ifdef SA_SIGINFO
  if (sig == SIGSEGV)
    {
      sigaction(sig, &oldsegact, NULL);
      return;
    }
#ifdef __MACH__
  if (sig == SIGBUS)
    {
      sigaction(sig, &oldbusact, NULL);
      return;
    }
#endif
  abort();
#else
  signal(sig, SIG_DFL);
#endif
}

#if ((defined __i386__ || __x86_64__) && !defined NOCOMPILER    \
     && defined SA_SIGINFO)
 #define USE_ALTSTACK 1
static char my_signal_stack[MINSIGSTKSZ + 8 * 1024];
#else
 #undef USE_ALTSTACK
#endif

static void check_segv(int sig, REG_CONTEXT_T *scp)
{
#ifdef USE_ALTSTACK
  unsigned long sp = get_stack_pointer();
  if (sp >= (unsigned long)&my_signal_stack
      && sp < (unsigned long)&my_signal_stack + sizeof my_signal_stack)
    {
      /* will be restored by safe_mcatch() */
      hard_mudlle_stack_limit = mudlle_stack_limit
        = (unsigned long)&my_signal_stack + 1024;
    }
#endif  /* USE_ALTSTACK */

#ifdef __i386__
  #define GET_PC(scp) GETREG(scp, eip, EIP)
  #define GET_SP(scp) GETREG(scp, esp, ESP)
  #define GET_BP(scp) GETREG(scp, ebp, EBP)
  #define GET_ARG0(scp) GETREG(scp, eax, EAX)
  #define GET_ARG1(scp) GETREG(scp, ecx, ECX)
  #define GET_ARG2(scp) GETREG(scp, edx, EDX)
#elif defined __x86_64__
  #define GET_PC(scp) GETREG(scp, rip, RIP)
  #define GET_SP(scp) GETREG(scp, rsp, RSP)
  #define GET_BP(scp) GETREG(scp, rbp, RBP)
  #define GET_ARG0(scp) GETREG(scp, rdi, RDI)
  #define GET_ARG1(scp) GETREG(scp, rsi, RSI)
  #define GET_ARG2(scp) GETREG(scp, rdx, RDX)
#else
  #error Unsupported architecture
#endif
  const uint8_t *pc = (const uint8_t *)GET_PC(scp);

  CASSERT_EXPR(offsetof (struct obj, size) == 0);

#ifdef __x86_64__
  /* skip any REX prefix */
  if ((pc[0] & 0xf0) == 0x40)
    ++pc;
#endif

  const int obj_type_ofs = sizeof (ulong) + 1;

  if ((pc[0] == 0x80           /* cmpb $imm,object_type(reg) */
       && (pc[1] & 0xf8) == 0x78
       && pc[2] == obj_type_ofs)
      || ((pc[0] == 0x81        /* cmpl $imm,object_size(reg) */
           || pc[0] == 0x83)    /* cmpl $imm8,object_size(reg) */
          && (pc[1] & 0xf8) == 0x38)
      || (pc[0] == 0x0f        /* movzbl object_type(%ecx),%eax */
          && pc[1] == 0xb6
          && (pc[2] & 0xc0) == 0x40
          && pc[3] == obj_type_ofs)
      || (pc[0] == 0x8b        /* mov (reg0),reg1 for size */
          && (pc[1] & 0xc0) == 0)
      || (pc[0] == 0x8b        /* mov car/cdr(reg0),reg1 */
          && (pc[1] & 0xc0) == 0x40
          && (pc[2] == offsetof(struct list, car)
              || pc[2] == offsetof(struct list, cdr))))
    {
      /* mudlle code */
      if (pc >= gcblock && pc < gcblock + gcblocksize)
	{
	  ccontext.frame_end_sp = (ulong *)GET_SP(scp);
	  ccontext.frame_end_bp = (ulong *)GET_BP(scp);
          /* Make stack trace printing start from the right place.
             1 is subtracted in error.c as other PCs are return addresses */
	  ccontext.frame_end_sp[-1] = (ulong)pc + 1;
	  runtime_error(error_bad_type);
	}
      /* bref and bset */
      if (pc >= (uint8_t *)bref && pc < (uint8_t *)bwglobal)
	{
	  ccontext.frame_end_sp = (ulong *)GET_SP(scp) + 1;
	  ccontext.frame_end_bp = (ulong *)GET_BP(scp);
          bool is_set = pc >= (uint8_t *)bset;
          if (is_set)
            set_bad_type_error((value)GET_ARG0(scp),
                               (value)GET_ARG1(scp),
                               (value)GET_ARG2(scp));
          else
            ref_bad_type_error((value)GET_ARG0(scp),
                               (value)GET_ARG1(scp));
	}
      /* bcall */
      if (pc >= (uint8_t *)bcall && pc < (uint8_t *)bcall_secure)
	{
	  ccontext.frame_end_sp = (ulong *)GET_SP(scp) + 1;
	  ccontext.frame_end_bp = (ulong *)GET_BP(scp);
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

#endif  /* (__i386__ || __x86_64__) && !NOCOMPILER */

void flag_check_failed(const struct prim_op *op, const char *name)
{
  abort();
}

void runtime_init(void)
{
  op_count = 0;
  system_module = alloc_string("system");
  staticpro(&system_module);

#ifdef MUDLLE_INTERRUPT
  signal(SIGINT, catchint);
#ifdef SIGQUIT
  signal(SIGQUIT, catchint);
#endif
#endif

#if (defined __i386__ || defined __x86_64__) && !defined NOCOMPILER
#ifdef SA_SIGINFO
  {
    stack_t my_stack = {
      .ss_sp    = my_signal_stack,
      .ss_flags = 0,
      .ss_size  = sizeof my_signal_stack,
    };
    bool noaltstack = sigaltstack(&my_stack, NULL) < 0;
    if (noaltstack)
      {
	perror("sigaltstack()");
      }

    struct sigaction sact = {
      .sa_sigaction = catchsegv,
      .sa_flags     = (SA_SIGINFO | SA_RESTART | SA_NODEFER
                       | (noaltstack ? 0 : SA_ONSTACK)),
    };
    sigemptyset(&sact.sa_mask);
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
  module_set("system", module_protected, 0);

  sort_primitives();
}
