/*
 * Copyright (c) 1993-2006 David Gay and Gustav Hållberg
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

#include <stdlib.h>
#include <signal.h>

#include "runtime/runtime.h"
#include "module.h"
#include "vector.h"
#include "basic.h"
#include "symbol.h"
#include "stringops.h"
#include "files.h"
#include "arith.h"
#include "mudlle-float.h"
#include "bigint.h"
#include "pattern.h"
#include "bool.h"
#include "io.h"
#include "list.h"
#include "support.h"
#include "bitset.h"
#include "debug.h"
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
  ulong aindex;
  struct gcpro gcpro1;

  GCPRO1(val);
  aindex = global_lookup(name); /* may allocate ... */
  UNGCPRO();

  assert(GVAR(aindex) == NULL /* cannot define same constant twice */);

  GVAR(aindex) = val;
  module_vset(aindex, var_module, system_module);
}

void define_string_vector(const char *name, const char *const *vec, int count)
{
  struct vector *v;
  struct gcpro gcpro1;
  int n;

  if (count < 0)
    for (count = 0; vec[count]; ++count)
      ;

  v = alloc_vector(count);
  GCPRO1(v);

  for (n = 0; n < count; ++n)
    {
      struct string *s = alloc_string(vec[n]);
      s->o.flags |= OBJ_READONLY;
      v->data[n] = s;
    }
  UNGCPRO();
  v->o.flags |= OBJ_READONLY;
  system_define(name, v);
}

void define_int_vector(const char *name, const int *vec, int count)
{
  struct vector *v;
  struct gcpro gcpro1;
  int n;

  v = alloc_vector(count);
  GCPRO1(v);

  for (n = 0; n < count; ++n)
    v->data[n] = makeint(vec[n]);

  UNGCPRO();
  v->o.flags |= OBJ_READONLY;
  system_define(name, v);
}

void runtime_define(const char *name, struct primitive_ext *op)
{
  const char *const *type;

  struct primitive *prim;
  char bname[MAXNAME];

  op->name = name;

  assert(op->nargs <= MAX_PRIMITIVE_ARGS);

  if (op->type == NULL)
    goto no_types;

  for (type = op->type; *type; ++type)
    {
      char *period = strchr(*type, '.');
      char *star = strchr(*type, '*');
      int args;
      /* all type specifications have to have a period in them, with 0
         or 1 characters after it! */
      assert(period);
      assert(period[1] == 0 || period[2] == 0);
      if (op->nargs >= 0)
        {
          args = period - *type;
          assert(op->nargs == args);
        }

      if (star)
        {
          /* Kleene closure only allowed with varargs; must be followed by . */
          assert(star > *type);
          assert(star + 1 == period);
          assert(op->nargs < 0);
        }
    }

 no_types:

  if (binops)
    {
      bname[MAXNAME - 1] = '\0';
      strncpy(bname, name, MAXNAME - 1);
      fwrite(bname, MAXNAME, 1, binops);
    }

  if (op->seclevel > 0)
    {
      prim = alloc_secure(op_count++, op);
      if (ops) fprintf(ops, "%-20s %s SECURITY %d\n", name, op->help, op->seclevel);
    }
  else
    {
      prim = alloc_primitive(op_count++, op);

      if (op->nargs < 0)	/* Varargs */
        {
          prim->o.type = type_varargs;
          assert(~op->flags & OP_NOALLOC);
        }
      if (ops) fprintf(ops, "%-20s %s\n", name, op->help);
    }
  system_define(name, prim);
}

#ifdef MUDLLE_INTERRUPT
static int interrupted = FALSE;

void check_interrupt(void)
/* Effects: Causes a user_interrupt runtime error if interrupted is TRUE
     (user caused SIGINT or SIGQUIT)
*/
{
#ifdef AMIGA
  chkabort();
#endif
  if (interrupted)
    {
      interrupted = FALSE;
      runtime_error(error_user_interrupt);
    }
}

void catchint(int sig)
{
  xcount = 1;
  interrupted = TRUE;
}
#endif

#if defined(i386) && !defined(NOCOMPILER)

#ifdef SA_SIGINFO
#include <asm/ucontext.h>
#else
#if !defined(__GLIBC__) || __GLIBC__ < 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ <= 1)
#    define sigcontext sigcontext_struct
#    include <asm/sigcontext.h>
#elif (__GLIBC__ != 2 && __GLIBC_MINOR__ != 2)
#    include <sigcontext.h>
#endif
#endif

#include <stddef.h>
#include "builtins.h"

#ifdef SA_SIGINFO
static struct sigaction oldsegact;
#endif

static struct primitive *get_ref_primitive(void)
{
  static int nenv;
  struct primitive *prim;

  if (nenv == 0)
    nenv = global_lookup("ref");

  prim = GVAR(nenv);
  assert(TYPE(prim, type_primitive));
  return prim;
}

static struct primitive *get_set_primitive(void)
{
  static int nenv;
  struct primitive *prim;

  if (nenv == 0)
    nenv = global_lookup("set!");

  prim = GVAR(nenv);
  assert(TYPE(prim, type_primitive));
  return prim;
}

void got_real_segv(int sig)
{
  /* 
   * reinstall the default handler, this will cause
   * a real crash (instead of the annoying abort
   * broken stack frame)
   */
#ifdef SA_SIGINFO
  sigaction(sig, &oldsegact, NULL);
#else
  signal(sig, SIG_DFL);
#endif
}

static INLINE void check_segv(int sig, struct sigcontext *scp)
{
  ubyte *eip = (ubyte *)scp->eip;

  /* Check if it was a cmpb $imm,object_type(reg) */
  if (eip[0] == 0x80 &&
      (eip[1] & 0xf8) == 0x78 &&
      eip[2] == 5)		/* offsetof(struct obj, type) */
    {
      /* mudlle code */
      if (eip  >= gcblock && eip < gcblock + gcblocksize)
	{
	  ccontext.frame_end_sp = (ulong *)scp->esp;
	  ccontext.frame_end_sp[-1] = 0; /* This sometimes contains a mudlle value */
	  ccontext.frame_end_bp = (ulong *)scp->ebp;
	  ccontext.retadr = (ulong)eip + 4; /* retadr points to next instr. */
	  runtime_error(error_bad_type);
	}
      /* bref and bset */
      if (eip >= (ubyte *)bref && eip < (ubyte *)bwglobal)
	{
          int is_set = eip >= (ubyte *)bset;
          struct call_stack me;
          me.type = call_c;
          me.u.c.prim = is_set ? get_set_primitive() : get_ref_primitive();
          me.u.c.nargs = is_set ? 3 : 2;
          me.u.c.arg1 = (value)scp->eax;
          me.u.c.arg2 = (value)scp->ecx;
          me.u.c.arg3 = (value)scp->edx;
          me.next = call_stack;
          call_stack = &me;

	  ccontext.frame_end_sp = (ulong *)scp->esp + 1;
	  ccontext.frame_end_bp = (ulong *)scp->ebp;
	  runtime_error(error_bad_type);
	}
      /* bcall */
      if (eip >= (ubyte *)bcall && eip < (ubyte *)bcall_secure)
	{
	  ccontext.frame_end_sp = (ulong *)scp->esp + 1;
	  ccontext.frame_end_bp = (ulong *)scp->ebp;
	  runtime_error(error_bad_function);
	}
    }

  got_real_segv(sig);
}

#ifdef SA_SIGINFO
static void catchsegv(int sig, siginfo_t *siginfo, void *_sigcontext)
{
  struct ucontext *sigcontext = (struct ucontext *)_sigcontext;
  check_segv(sig, &sigcontext->uc_mcontext);
}
#else
static void catchsegv(int sig, struct sigcontext scp)
{
  check_segv(sig, &scp);
}
#endif

#endif

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
  else if ((trapins & ~(31 << 14)) == (3 << 30 | 16 << 25 | 2 << 19 | 1 << 13 | 4))
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
  else if ((trapins & ~(31 << 14)) == (3 << 30 | 16 << 25 | 2 << 19 | 1 << 13 | 4))
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

void runtime_init(void)
{
  ops = fopen("mudlle-functions", "w+");
  binops = fopen("mudlle-primitives", "w+");
  op_count = 0;
  system_module = alloc_string("system");
  staticpro((value *)&system_module);

#ifdef MUDLLE_INTERRUPT
#ifdef __SVR4
  {
    struct sigaction act;
    act.sa_sigaction = (void *)catchint;
    act.sa_flags = SA_SIGINFO | SA_NODEFER | SA_RESTART;
    sigemptyset(&act.sa_mask);
    sigaction(SIGINT, &act, NULL);
    sigaction(SIGQUIT, &act, NULL);
  }    
#else
  signal(SIGINT, catchint);
  signal(SIGQUIT, catchint);
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
  if (ops) fclose(ops);
  if (binops) fclose(binops);
}
