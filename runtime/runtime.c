/* $Log: runtime.c,v $
 * Revision 1.35  1995/07/15  15:25:04  arda
 * Context cleanup.
 * Remove GCDEBUG.
 *
 * Revision 1.34  1994/10/09  06:44:17  arda
 * Libraries
 * Type inference
 * Many minor improvements
 *
 * Revision 1.33  1994/09/03  13:37:52  arda
 * Shops mudlled.
 * Some macros changed to functions.
 *
 * Revision 1.32  1994/08/29  13:19:49  arda
 * Contagious immutability.
 * Global array of values instead of variables.
 * Direct recursion.
 *
 * Revision 1.31  1994/08/22  18:03:38  arda
 * Primitives for compiler.
 *
 * Revision 1.30  1994/08/22  11:19:03  arda
 * Changes for mudlle compiler in MUME.
 *
 * Revision 1.29  1994/08/16  19:17:15  arda
 * Added flags to primitives for better calling sequences.
 *
 * Revision 1.26  1994/04/12  20:12:15  arda
 * (MD) Alignments and fixes + unknown from others...
 *
 * Revision 1.25  1994/03/23  14:31:44  arda
 * *** empty log message ***
 *
 * Revision 1.24  1994/02/24  08:33:43  arda
 * Owl: New error messages.
 *
 * Revision 1.23  1994/02/12  17:25:51  arda
 * Owl: MUME IV (special procedures eliminated).
 *
 * Revision 1.22  1994/02/03  19:22:40  arda
 * nothing special(3)
 *
 * Revision 1.21  1994/01/07  15:07:26  dgay
 * Owl: Default mob behaviour is now defined in a global mudlle
 *      reaction.
 *
 * Revision 1.20  1994/01/02  15:50:31  arda
 * bug fix
 *
 * Revision 1.19  1993/12/31  10:16:26  dgay
 * Owl: make events, reactions & primitives immutable.
 *
 * Revision 1.18  1993/12/26  14:50:42  dgay
 * Owl: Mudlled guilds.
 *      New skills.
 *
 * Revision 1.17  1993/11/26  21:58:49  arda
 * Removed spec procs on objects & rooms (Owl)
 * Who knows what else ?
 *
 * Revision 1.16  1993/08/15  21:02:06  un_mec
 * Owl: Several extras functions.
 *      rent.
 *
 * Revision 1.15  1993/07/21  20:38:14  un_mec
 * Owl: Standalone version of mudlle (mkf, runtime/mkf, mudlle.c) added to CVS
 *      New builtin functions, new abbreviations (. = cons, ! = not).
 *
 * Revision 1.14  1993/05/08  09:18:38  un_mec
 * Owl: New gain_xx functions.
 *
 * Revision 1.13  1993/05/02  07:38:17  un_mec
 * Owl: New output (mudlle ports).
 *
 * Revision 1.12  1993/04/24  16:50:09  un_autre
 * Owl's
 *
 * Revision 1.10  1993/04/22  18:59:26  un_autre
 * (MD) & Owl. Bug fixes. /player fixes. EVER_WHINER flag. saving_spells adjusted.
 *
 * Revision 1.9  1993/04/17  10:03:49  un_autre
 * Various
 *
 * Revision 1.8  1993/03/29  09:25:51  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.4  1993/03/17  12:51:08  dgay
 * Added security features.
 *
 * Revision 1.3  1993/03/14  16:16:45  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.6  1993/02/14  21:09:18  un_mec
 * Owl
 *
 * Revision 1.5  1993/01/30  12:14:13  un_mec
 * Owl: Mudlle reactions installed, with loading and editing commands.
 * Also new: room commands, actions (only tell for now).
 *
 * Revision 1.4  1993/01/19  23:13:10  un_mec
 * Owl: tell
 * 	   timed commands added
 *
 * Revision 1.3  1993/01/08  23:57:47  un_mec
 * Owl: Add character and object types.
 *
 * Revision 1.2  1992/12/30  14:12:01  un_mec
 * Owl:
 * Several changes:
 * - Variables don't have separate value & function cells, instead their are
 *   now 2 types: type_function & type_variable.
 * 	-> new functions store, recall. Removed store-xx, recall-xx.
 * - New types: list (Lisp style pair), vector (array)
 *
 * Revision 1.1  1992/12/27  21:42:20  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

static char rcsid[] = "$Id: runtime.c,v 1.35 1995/07/15 15:25:04 arda Exp $";

#include <signal.h>
#ifdef AMIGA
#include <dos.h>
#endif
#include "runtime/runtime.h"
#include "module.h"
#include "vector.h"
#include "basic.h"
#include "symbol.h"
#include "stringops.h"
#include "files.h"
#include "arith.h"
#include "bool.h"
#include "io.h"
#include "list.h"
#include "support.h"
#include "bitset.h"
#include "debug.h"

#ifdef MUME
#include "actions.h"
#include "char.h"
#include "object.h"
#include "room.h"
#include "invoke.h"
#endif

#include <stdio.h>
#include <string.h>

static op_count;
static FILE *ops, *binops;
value undefined_value;
static struct string *system_module;
static int interrupted;

#define MAXNAME 32

void system_define(const char *name, value val)
/* Modifies: environment
   Requires: name not already exist in environment.
   Effects: Adds name to environment, with value val for the variable,
     as a 'define' of the system module.
*/
{
  ulong index = global_lookup(name);

  GVAR(index) = val;
  module_vset(index, var_module, system_module);
}

void runtime_define(const char *name, struct primitive_ext *op)
{
  struct primitive *prim;
  char bname[MAXNAME];

  op->name = name;

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
	prim->o.type = type_varargs;

      if (ops) fprintf(ops, "%-20s %s\n", name, op->help);
    }
  system_define(name, prim);
}

#ifdef INTERRUPT
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
  interrupted = TRUE;
}
#endif

#ifdef sparc
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
      runtime_error(nerror);
    }
  abort(); /* Really an illegal instruction */
}

void catchsegv(int sig, int code, struct sigcontext *scp, char *addr)
{
  ulong trapins = *(ulong *)scp->sc_pc;
  int nerror;

  /* Check if it was a type check (ie: lduh [x+4],g3) */
  if ((trapins & ~(31 << 14)) == (3 << 30 | 3 << 25 | 2 << 19 | 1 << 13 | 4))
    {
      /* Yes ... */
      /* reset handler */
      sigsetmask(0);
      signal(sig, catchsegv);
      runtime_error(error_bad_type);
    }
  /* or a function check (ie: lduh [x+4],l0) */
  else if ((trapins & ~(31 << 14)) == (3 << 30 | 16 << 25 | 2 << 19 | 1 << 13 | 4))
    {
      /* Yes ... */
      /* reset handler */
      sigsetmask(0);
      signal(sig, catchsegv);
      runtime_error(error_bad_function);
    }
  abort(); /* Really an illegal instruction */
}
#endif

void runtime_init(void)
{
  ops = fopen("mudlle-functions", "w+");
  binops = fopen("mudlle-primitives", "w+");
  op_count = 0;
  undefined_value = makeint(42);
  system_module = alloc_string("system");
  staticpro((value *)&system_module);

#ifdef INTERRUPT
  signal(SIGINT, catchint);
  signal(SIGQUIT, catchint);
#endif

#ifdef sparc
  signal(SIGILL, catchill);
  signal(SIGSEGV, catchsegv);
  signal(SIGBUS, catchsegv);
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
#ifdef MUME
  character_init();
  object_init();
  invoke_init();
  room_init();
  actions_init();
  mumecst_init();
  mume_init();
  rent_init();
  predefined_init();
#endif
  module_set("system", module_protected);
  if (ops) fclose(ops);
  if (binops) fclose(binops);
}
