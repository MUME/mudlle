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

/*
The mudlle context can be split into 4 parts:

  - global context (X)
  - session context (S): information related to a particular session (I/O,
    security, execution time limits, etc)
  - catch context (T): information related to a particular error handler
  - call context (M): information related to a particular function call
    (be the function implemented in C, byte code, or machine code)

All information accessed by mudlle belongs to one of the four contexts,
all the contexts (except global) can be nested indefinitely.

Restriction: if a new session context is created, a new catch context must be
created immediately (to simplify trap handling).

Information for a particular context comes in three varieties:
 - constant: keeps the same value over the context's lifetime.
 - local: each context gets a separate value.
 - preserved: only one value exists, but the value at context exit is the same
   as at context entry.

Information storage may be implemented in 2 ways:
 - stored in global variables. The context entry & exit functions make sure
   that the information is saved and restored somehow (~ shallow binding)
 - stored in a context structure (with the other context information), with
   a context pointer to this structure. The context entry & exit functions
   stack & unstack these structures.

Convention: context structures fields are called x if they contain the value
of x for that context, old_x if they are saving the value of x in the previous
context (in that case the current value is in global variable x).
*/

#ifndef CONTEXT_H
#define CONTEXT_H

#include <setjmp.h>

#include "alloc.h"
#include "error.h"
#include "mvalues.h"


enum call_trace_mode {
  call_trace_off,               /* call trace completely suppressed */
  call_trace_barrier,           /* call trace to muderr/observers up to here */
  call_trace_on,                /* complete call traces to everyone  */
  call_trace_no_err             /* no call trace to muderr; only to watchers */
};

/* Function context */
/* ---------------- */

#ifndef USE_CCONTEXT

struct ccontext
{
  int dummy;
};

#else  /* USE_CCONTEXT */

#define __DEF_VALUE(n, reg) value reg
struct ccontext {
  ulong *frame_start;
  /* Use ccontext_frame() to extract the real sp/bp for this frame */
  ulong *frame_end_sp;
  ulong *frame_end_bp;
  /* Space to save callee and caller saved registers */
  struct {
    FOR_CALLEE_SAVE(__DEF_VALUE, ;);
  } callee;
  struct {
    FOR_CALLER_SAVE(__DEF_VALUE, ;);
  } caller;
};
#undef __DEF_VALUE

static inline void ccontext_frame(const struct ccontext *cc,
                                  ulong **bpp, ulong **spp)
{
  ulong *sp = cc->frame_end_sp;
  ulong *bp = cc->frame_end_bp;
  if ((ulong)bp <= 1)
    {
      /* esp points to the stack position where the previous ebp
         was pushed by the push %ebp/mov %esp,%ebp preamble */
      ulong c_args = (ulong)bp + 2;
      bp = (ulong *)*sp;
      sp += c_args;
    }
  *bpp = bp;
  *spp = sp;
}

#endif /* USE_CCONTEXT */

extern struct ccontext ccontext;

/* Security level of a calling function when directly calling a secure
   or vararg. DEFAULT_SECLEVEL otherwise. */
extern seclev_t internal_seclevel;
/* Lowest seclevel seen in current call chain. A mudlle int so it
   cheaply can be pushed to the stack. */
extern value maxseclevel;
/* Set to true whenever a secure or vararg was the most recently
   called primitive. */
extern bool seclevel_valid;

static inline seclev_t get_seclevel(void)
{
  assert(seclevel_valid);
  return internal_seclevel;
}

static inline void set_seclevel(seclev_t seclevel)
{
  internal_seclevel = seclevel;
}

value seclevel_to_maxseclevel(seclev_t seclev);
seclev_t get_effective_seclevel(void);

/* Does not require a valid seclevel (ie. to be called from a secure primitive)
 * if seclev < LEGACY_SECLEVEL. Useful for OP_FASTSEC OPs. */
static inline bool effective_seclevel_below(seclev_t seclev)
{
  if (seclev >= LEGACY_SECLEVEL)
    return get_effective_seclevel() < seclev;
  return intval(maxseclevel) < seclev;
}


/* Used only by byte-coded functions and C primitives (not used for,
   nor updated by, compiled code) */
enum call_class {
  call_bytecode,                /* interpreted byte code closures */
  call_compiled,                /* compiled closures */
  call_c,                       /* primitives */
  call_primop,                  /* prim_op; for displaying stack traces only */
  call_string,                  /* a string description; for displaying
                                   stack traces only*/
  call_session,                 /* a new session */
  call_invalid,                 /* invalid interpreter call */
  call_invalid_argp             /* invalid call with pointer to args */
};


struct call_stack
{
  struct call_stack *next;
  enum call_class type;
};

struct call_stack_mudlle {      /* used for call_bytecode */
  struct call_stack s;
  struct closure *fn;           /* Actual function */
  struct icode *code;           /* The function's code */
  struct vector *locals;	/* Local vars */
  int nargs;                    /* -1 = don't know yet */
  int offset;                   /* Instr. offset called from */
};

struct call_stack_c_header {
  struct call_stack s;
  union {
    struct primitive *prim;         /* for call_c */
    const struct prim_op *op;       /* for call_primop */
    const char *name;               /* for call_string */
    value value;                    /* for call_invalid{,_argv} */
  } u;
  int nargs;
};

struct call_stack_c {
  struct call_stack_c_header c;
  value args[];
};

struct call_stack_c_argp {
  struct call_stack_c_header c;
  value *argp;
};

extern struct call_stack *call_stack;

#ifdef USE_CCONTEXT

static inline struct ccontext *next_ccontext(const struct ccontext *cc)
{
#ifdef __i386__
  /* ebx, esi, edi */
  const int frame_size = (3 * sizeof (value)
                          + sizeof (struct call_stack)
                          + sizeof *cc);
#elif defined __x86_64__
  /* ebx, r12-r15 */
  const int frame_size = MUDLLE_ALIGN(
    5 * sizeof (value)
    + sizeof (struct call_stack)
    + sizeof *cc,
    16);
#else
  #error Unsupported architecture
#endif
  /* See START_INVOKE: the first (invoke) stack frame contains callee-save
     registers, padding (to align to stack alignment), the non-union field of
     struct call_stack, parent ccontext, and then invoke arguments. */
  return (struct ccontext *)((char *)cc->frame_start - frame_size);
}

/* The invoke arguments are below here. */
static inline ulong *ccontext_argsend(const struct ccontext *cc)
{
  return (ulong *)next_ccontext(cc);
}

#endif  /* USE_CCONTEXT */

/* Catch context */
/* ------------- */

struct catch_context
{
  struct catch_context *parent;

  sigjmp_buf exception;         /* the return point */

  /* How should call traces be shown if errors occur in this context ? */
  enum call_trace_mode call_trace_mode;

  /* Save values of constant/local/preserved mudlle variables, to be able to
     restore them after a throw(). In an ideal world, everything would be in
     the call_stack. */
  struct call_stack *old_call_stack;
  int old_stack_depth;
  struct gcpro *old_gcpro;
  struct ccontext old_ccontext;

  struct mjmpbuf *mjmpbuf;
};

extern struct catch_context *catch_context;

struct mexception {
  enum mudlle_signal sig;       /* last exception or SIGNAL_NONE */
  enum runtime_error err;       /* set if .sig == SIGNAL_ERROR */
};
extern struct mexception mexception;

static inline bool has_pending_exception(void)
{
  return mexception.sig != SIGNAL_NONE;
}

bool mcatch(void (*fn)(void *x), void *x,
            enum call_trace_mode call_trace_mode);
/* Effects: Executes fn(x) with error protection in place.
   Returns: true if all went well, false otherwise.
     If false, information on the exeception that occurred is in
       exception_signal/exception_error.
     The execution environment is protected by this function, i.e.:
        stack
	GC protection lists
	activation stack
	seclevel
	exception handler
     Calls to mcatch may be nested with no problems.
*/

/* Session context */
/* --------------- */

typedef void *muser_t;

struct session_context
{
  struct call_stack s;
  struct session_context *parent;
  struct oport *_mudout, *_muderr;
  muser_t _muduser;
  seclev_t old_minlevel;
  seclev_t old_maxseclevel;
  ulong old_xcount;
#if defined __i386__ || defined __x86_64__
  ulong old_stack_limit;
#endif
  struct gcpro *old_gcpro;
};

extern struct session_context *session_context;

/* end mudlle const */
#define mudout  (session_context->_mudout)
#define muduser (session_context->_muduser)
#define muderr  (session_context->_muderr)

extern ulong xcount;			/* Loop detection */
extern uint16_t minlevel;               /* Minimum security level */

extern ulong hard_mudlle_stack_limit, mudlle_stack_limit;

struct session_info {
  uint16_t minlevel;
  /* See session_context.seclevel */
  seclev_t maxseclevel;
  muser_t muser;
  struct oport *mout, *merr;
};

extern const struct session_info cold_session;

void session_start(struct session_context *newp,
                   const struct session_info *info);
void cold_session_start(struct session_context *context,
                        seclev_t maxseclevel);
void session_end(void);

void unlimited_execution(void);
/* Effects: Cancels execution limits for current session
*/

/* Global context */
/* -------------- */

void reset_context(void);
/* Effects: Clears all contexts. Should be called with caution ...
*/

void context_init(void);
/* Effects: Initialises module */

void set_mudlle_stack_limit(unsigned long reserved);

extern struct list *mudcalltrace;

void remove_call_trace(value v);
void add_call_trace(value v, bool unhandled_only);

value mjmpbuf(value *result);
bool is_mjmpbuf(value buf);

#endif
