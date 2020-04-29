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

#ifndef CALL_H
#define CALL_H

#include "error.h"
#include "types.h"

#define NVARARGS -1

#define PRIMARGSNVARARGS value v0, ulong nargs
#define PRIMARGNAMESNVARARGS v0, nargs

#define __PRIMNAME(N) v ## N
#define __PRIMARG(N) value __PRIMNAME(N)
#define PRIMARGNAMES0
#define PRIMARGNAMES1 CONCATCOMMA(1, __PRIMNAME)
#define PRIMARGNAMES2 CONCATCOMMA(2, __PRIMNAME)
#define PRIMARGNAMES3 CONCATCOMMA(3, __PRIMNAME)
#define PRIMARGNAMES4 CONCATCOMMA(4, __PRIMNAME)
#define PRIMARGNAMES5 CONCATCOMMA(5, __PRIMNAME)
#define PRIMARGS0     void
#define PRIMARGS1     CONCATCOMMA(1, __PRIMARG)
#define PRIMARGS2     CONCATCOMMA(2, __PRIMARG)
#define PRIMARGS3     CONCATCOMMA(3, __PRIMARG)
#define PRIMARGS4     CONCATCOMMA(4, __PRIMARG)
#define PRIMARGS5     CONCATCOMMA(5, __PRIMARG)

extern const struct prim_op *forbid_mudlle_calls;

void fail_allow_mudlle_call(void);

static inline void check_allow_mudlle_call(void)
{
  if (forbid_mudlle_calls == NULL)
    return;
  fail_allow_mudlle_call();
}

/* Effects: Calls c with listed arguments
   Returns: c's result
   Requires: callable(c, N) does not fail.
*/
value call0(value c);
#define DECL_CALL(N) value call ## N(value c, PRIMARGS ## N)
DOPRIMARGS(DECL_CALL, SEP_SEMI);
#undef DECL_CALL

value call1plus(value c, value arg, struct vector *args);
/* Effects: Calls c with argument arg
   Returns: c's result
   Requires: callable(c, 1 + vector_len(args)) does not fail.
   Cheat: If c is a closure, it will do the argument count check, so
     the requirement is waved (otherwise cause_event/react_event
     become painful).
*/

value callv(value c, struct vector *args);
/* Effects: Calls c with arguments args
   Returns: c's result
   Requires: callable(c, vector_len(args)) does not fail.
*/

enum runtime_error function_callable(struct obj *f, const char **errmsg,
                                     int nargs);
/* Returns: error to raise if function f cannot be called with nargs
   arguments */

void callable(value c, int nargs);
/* Effects: Causes an error of c is not something that can be called with
     nargs arguments.
*/

bool callablep(value c, int nargs);
/* Returns: false if c is not something that can be called with
     nargs arguments.
*/

bool minlevel_violator(value c, seclev_t minlev);
/* Returns: true is calling c will cause a minlevel runtime error
*/

/* As above, but trap errors. An error was caused if
   has_pending_exception(); mexception.sig and .err are set appropriately.
*/
value internal_mcatch_call(int argc, const char *name, value c, ...);
value internal_mcatch_call0(const char *name, value c);

/* mcatch_call(name, function, args ...) */
#define mcatch_call(name, ...)                                  \
  IF_NO_COMMA(__VA_ARGS__)(                                     \
    internal_mcatch_call0((name), __VA_ARGS__),                 \
    internal_mcatch_call(DEC(VA_NARGS(__VA_ARGS__)), (name),    \
                         __VA_ARGS__))

value mcatch_call1plus(const char *name, value c, value arg,
                       struct vector *args);
value mcatch_callv(const char *name, value c, struct vector *args);

/* Machine language interface */

value invoke0(struct closure *c);
#if defined __x86_64__ && !defined NOCOMPILER
#define DECL_INVOKE(N)                                                  \
value x64_invoke ## N(PRIMARGS ## N, struct closure *c);                \
static inline value invoke ## N(struct closure *c, PRIMARGS ## N)       \
{                                                                       \
  return x64_invoke ## N(PRIMARGNAMES ## N, c);                         \
}                                                                       \
value x64_invoke ## N(PRIMARGS ## N, struct closure *c)
#else
#define DECL_INVOKE(N)                                                  \
value invoke ## N(struct closure *c, PRIMARGS ## N)
#endif
DOPRIMARGS(DECL_INVOKE, SEP_SEMI);
#undef DECL_INVOKE

/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, mcode);
   Effects: Executes c(arg1, ..., argN)
   Returns: c()'s result
*/

value invoke1plus(struct closure *c, value arg, struct vector *args);
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, mcode);
   Effects: Executes c(args)
   Returns: c(args)'s result
*/

value invokev(struct closure *c, struct vector *args);
/* Requires: c be a closure whose code is in machine code, i.e.
     TYPEIS(c->code, mcode);
   Effects: Executes c(args)
   Returns: c(args)'s result
*/

value msetjmp(value f);
noreturn void mlongjmp(struct mjmpbuf *buf, value x);

noreturn void mrethrow(void);
noreturn void mthrow(enum mudlle_signal sig, enum runtime_error err);

void maybe_mrethrow(void);

#endif
