/* $Log: context.h,v $
 * Revision 1.1  1995/07/15  15:49:28  arda
 * New files, missing from previous commit.
 *
 *
 * Purpose: an attempt at providing a centralised view of the mudlle context,
 *   and the operations to control it.
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

#include "mvalues.h"
#include "types.h"
#include "mudio.h"

/* Function context */
/* ---------------- */

extern uword seclevel;		/* Security level of the function */

/* Used only by byte-coded functions and C primitives (not used for,
   nor updated by, compiled code) */
struct call_stack
{
  struct call_stack *next;
  enum { call_bytecode, call_c, call_compiled } type;
  union {
    struct {
      struct closure *fn;		/* Actual function */
      struct code *code;		/* Code for this function */
      struct vector *locals;		/* Local vars */
      int nargs;			/* -1 = don't know yet */
    } mudlle;

    struct {
      struct primitive_ext *op;
      value arg1, arg2, arg3, arg4, arg5;
      int nargs;
    } c;
    /* No information is kept for compiled code, just a placemarker in the
       call_stack (nicer messages that way) */
  } u;
};

extern struct call_stack *call_stack;

/* Catch context */
/* ------------- */

struct catch_context
{
  int display_error;		/* Should error messages be shown if an error
				   occurs in this context ? */
#ifdef sparc
  ulong *old_frame_start, *old_frame_end;/*Used to restore the frame boundaries
					   to a valid value when landing in
					   runtime_error from an unknown
					   state ... (eg processor trap) */
#endif
};

extern struct catch_context *catch_context;

extern long exception_signal;	/* Last exception that occured, 0 for none */
extern value exception_value;

int catch(void (*fn)(void *x), void *x, int display_error);
/* Effects: Executes fn(x) with error protection in place.
   Returns: TRUE if all went well, FALSE otherwise.
     If FALSE, information on the exeception that occurred is in 
       exception_signal/exception_value.
     The execution environment is protected by this function, i.e.:
        stack
	GC protection lists
	activation stack
	seclevel
	exception handler
     Calls to catch may be nested with no problems.
*/

NORETURN void throw(long signal, value val);


/* Session context */
/* --------------- */

struct session_context
{
  struct session_context *parent;
  Mio _mudout, _muderr;
  Muser _muduser;
  uword old_minlevel;
  ulong old_xcount;
  ulong call_count;
};

extern struct session_context *session_context;

extern ulong xcount;			/* Loop detection */
extern uword minlevel;			/* Minimum security level */

void session_start(struct session_context *new,
		   uword new_minlevel,
		   Muser new_muduser,
		   Mio new_mudout,
		   Mio new_muderr);

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

#endif
