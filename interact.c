/*
 * Mudlle interaction with the MUD system.
 *
 * $Log: interact.c,v $
 * Revision 1.58  1995/07/16  09:16:50  arda
 * Add GCSTATS option.
 * Misc bug fixes.
 *
 * Revision 1.57  1995/07/15  15:24:24  arda
 * Context cleanup.
 * Remove GCDEBUG.
 *
 * Revision 1.56  1995/06/04  14:24:27  arda
 * Rename/move some files, misc. junk
 *
 * Revision 1.55  1995/01/22  15:11:38  arda
 * Linux patches.
 *
 * Revision 1.54  1994/11/08  09:25:15  arda
 * ?
 *
 * Revision 1.53  1994/10/18  15:54:35  arda
 * Who knows what else ?
 *
 * Revision 1.52  1994/10/09  15:14:25  arda
 * mudlle libraries.
 *
 * Revision 1.51  1994/10/09  06:42:15  arda
 * Libraries
 * Type inference
 * Many minor improvements
 *
 * Revision 1.50  1994/09/28  13:44:45  arda
 * Undocumented changes, as usual :(
 *
 * Revision 1.49  1994/09/24  17:55:38  arda
 * Some undocumented changes
 *
 * Revision 1.48  1994/09/16  13:07:10  arda
 * Rename protect to catch.
 * New protect/unprotect functions (like dynpro/undynpro).
 *
 * Revision 1.47  1994/09/15  19:46:38  arda
 * Performance improvements:
 *   setjmp -> _setjmp (setjmp is horrendously slow)
 *   cold_protect
 * reset_limits split from reset_interpreter
 * fix division of negative numbers
 * Add ?\{n,r,t}
 * gc_size returns "mutable" size
 *
 * Revision 1.46  1994/09/12  10:30:26  arda
 * Move god command parsing to mudlle.
 * Removed half_chop, argument_interpreter comments.
 * Cleaned up parsing routines further.
 *
 * Revision 1.45  1994/08/31  13:03:24  arda
 * Bug fixes (argh, no, new version of characters structures! (MD))
 *
 * Revision 1.44  1994/08/26  08:51:41  arda
 * Keep free block list for string ports.
 *
 * Revision 1.43  1994/08/26  04:35:36  arda
 * Fix bug in builtins.
 * Check gen0 size by type.
 *
 * Revision 1.42  1994/08/22  11:18:29  arda
 * Moved code allocation to ins.c
 * Changes for mudlle compiler in MUME.
 *
 * Revision 1.41  1994/08/18  07:57:37  arda
 * load_dir doesn't use chdir anymore.
 * Fixed CCALL_LEAF_NOALLOC (must restore globals)
 *
 * Revision 1.40  1994/08/17  15:07:23  arda
 * ?
 *
 * Revision 1.39  1994/08/17  10:19:32  arda
 * Improved make depend.
 * builtins were missing.
 *
 * Revision 1.38  1994/08/16  19:16:00  arda
 * Mudlle compiler for sparc now fully functional (68k compiler now needs
 * updating for primitives).
 * Changes to allow Sparc trap's for runtime errors.
 * Also added flags to primitives for better calling sequences.
 *
 * Revision 1.34  1994/02/24  08:32:54  arda
 * Owl: New error messages.
 *
 * Revision 1.33  1994/02/11  09:58:50  dgay
 * Owl: -Wall
 *      new shared string handling
 *      configuration file
 *
 * Revision 1.32  1994/02/03  19:21:31  arda
 * nothing special(2)
 *
 * Revision 1.31  1994/01/29  19:50:26  dgay
 * Owl: add file & line information to functions.
 *
 * Revision 1.30  1994/01/27  17:08:03  arda
 * Hmm.
 *
 * Revision 1.29  1994/01/07  08:08:52  dgay
 * Owl: Global events.
 *
 * Revision 1.28  1993/12/13  19:26:18  arda
 * fixes+rivers
 *
 * Revision 1.27  1993/12/07  22:10:31  arda
 * align on zones
 *
 * Revision 1.26  1993/12/06  19:20:52  arda
 * divers CLI
 *
 * Revision 1.25  1993/12/04  22:40:58  arda
 * Owl: Changed room instance data to use a gdbm database.
 *      Removed number field on world.
 *      Added mudlle data on rooms.
 *      Included source for converters from update.
 *
 * Revision 1.24  1993/11/21  14:47:38  arda
 * Miscellaneous
 *
 * Revision 1.23  1993/10/03  14:07:13  dgay
 * Bumper disun8 update.
 *
 * Revision 1.22  1993/08/14  16:43:09  un_mec
 * Owl: Improved vpprintf
 *      New input system (with an input stack) => small changes to interact
 *      New identifier rules (lexer.l)
 *
 * Revision 1.21  1993/07/21  20:36:41  un_mec
 * Owl: Added &&, ||, optimised if.
 *      Added branches to the intermediate language.
 *      Separated destiniation language generation into ins module
 *      (with some peephole optimisation)
 *      Standalone version of mudlle (mkf, runtime/mkf, mudlle.c) added to CVS
 *
 * Revision 1.20  1993/05/29  13:25:25  un_autre
 * Bug fixes.
 *
 * Revision 1.19  1993/05/20  16:24:34  un_mec
 * divers
 *
 *
 * nouvelle version avec 107 niveaux
 *
 * Revision 1.18  1993/05/02  13:02:35  un_mec
 * Owl: ARGH! Bugs.
 *
 * Revision 1.17  1993/05/02  07:37:39  un_mec
 * Owl: New output (mudlle ports).
 *
 * Revision 1.16  1993/04/24  16:49:52  un_autre
 * Owl's
 *
 * Revision 1.14  1993/04/22  18:58:42  un_autre
 * (MD) & Owl. Bug fixes. /player fixes. EVER_WHINER flag. saving_spells adjusted.
 *
 * Revision 1.13  1993/04/17  11:11:25  un_mec
 * Owl: ?
 *
 * Revision 1.12  1993/04/17  10:01:16  un_autre
 * Various
 *
 * Revision 1.11  1993/04/10  09:16:58  un_mec
 * Owl: Debug mudlle.
 *
 * Revision 1.10  1993/03/29  09:23:53  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.9  1993/03/10  08:53:40  un_mec
 * Owl: Mudlle II, the revenge.
 *
 * Revision 1.8  1993/02/14  21:09:02  un_mec
 * Owl: Fix code limits.
 *
 * Revision 1.7  1993/02/14  00:39:32  un_mec
 * Owl: MUME III released:
 * - mudlle is now basically working. Lots of basic procedures still need
 * to be added.
 *
 * Revision 1.6  1993/01/30  23:08:53  un_mec
 * Owl: ?
 *
 * Revision 1.5  1993/01/30  12:13:32  un_mec
 * Owl: Mudlle reactions installed, with loading and editing commands.
 *
 * Revision 1.4  1993/01/26  09:48:50  un_mec
 * Owl:
 * - Limit mudlle execution time (prevent infinite loops).
 * - Add mudlle reaction procedures.
 *
 * Revision 1.3  1993/01/19  23:12:43  un_mec
 * Owl: Only load dump file if mudlle.bad doesn't exist.
 *
 * Revision 1.2  1992/12/30  14:10:43  un_mec
 * Owl:
 * Several changes:
 * - Variables don't have separate value & function cells, instead their are
 *   now 2 types: type_function & type_variable.
 * - print_value: New types (list, vector), printing rationalised.
 * - New type: list (Lisp style pair)
 * - lexer.l: Debug read_from_string
 * - debug_level & DEBUG macro provided to help debugging.
 *
 * Revision 1.1  1992/12/27  21:41:09  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

#include <unistd.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/signal.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <dirent.h>

#include "mudlle.h"
#include "interact.h"
#include "mparser.h"
#include "lexer.h"
#include "compile.h"
#include "mvalues.h"
#include "alloc.h"
#include "global.h"
#include "print.h"
#include "utils.h"
#include "runtime/runtime.h"
#include "runtime/invoke.h"
#include "error.h"
#include "call.h"
#include "module.h"
#include "mcompile.h"
#include "interpret.h"

#include "def.time.h"
#include "def.files.h"
#include "struct.time.h"
#include "struct.socket.h"
#include "frontend.h"
#include "struct.player.h"
#include "db.world.h"
#include "main.h"
#include "utils.files.h"
#include "parser.h"

block_t memory;
int debug_level = 0;

int load_file(char *name, char *nicename, int seclev, int reload)
{
  FILE *f;
  value result;
  int ok;

  if (!(f = fopen(name, "r"))) return FALSE;

  read_from_file(f, nicename);
  ok = interpret(&result, seclev, reload);
  fclose(f);

  return ok;
}

struct pload
{
  char *name;
  char *nicename;
  int seclev, reload;
  int result;
};

static void pload(void *_data)
{
  struct pload *data = _data;

  data->result = load_file(data->name, data->nicename, data->seclev, data->reload);
}

int catch_load_file(char *name, char *nicename, int seclev, int reload)
{
  struct pload data;

  data.name = name; data.nicename = nicename; data.seclev = seclev;
  data.reload = reload;
  if (catch(pload, &data, TRUE)) return data.result;
  else return FALSE;
}

void mudlle_load_all(void)
{
  struct session_context new;

  session_start(&new, 0, NULL, NULL, NULL);
  catch_load_file("mudlle/mume-loader.mud", "mume-loader.mud", LVL_IMPLEMENTOR, TRUE);
  session_end();
}

void mudlle_init(void)
{
  garbage_init();
  global_init();
  print_init();
  stack_init();
  module_init();
  runtime_init();
  parser_init();
  compile_init();
  mcompile_init();
  interpret_init();
  error_init();
  ports_init();
  context_init();
}

void mudlle_cleanup(void)
{
}

void mudlle_prompt(void *data, struct descriptor_data *d, char *prompt, int prompt_size)
{
  new_strcpy(prompt, prompt_size, "mudlle> ");
}

void mudlle_init_player(struct char_data *ch)
{
  cprintf(ch, "Welcome %s to the mudlled language system\n\r", PC_NAME(ch));
}

static void execute(char *line)
{
  value result;

  read_from_string(line);
  if (interpret(&result, PC_LEVEL(muduser), TRUE))
    {
      struct gcpro gcpro1;

      GCPRO1(result);
      mprintf(mudout, "Result: ");
      mprint(mudout, prt_print, result);
      mprintf(mudout, EOL);
      UNGCPRO();
    }
}

void mudlle_act(void *data, struct descriptor_data *d, char *command)
{
  struct char_data *ch = d->character;
  char *cmd, *s;

  if (*skip_spaces(command) == '\0') /* empty command, exit */
    pop_input(d);
  else
    {
      struct session_context new;

      memory = new_block();

      /* Setup input to be the string in command */
      DEBUG(1, fprintf(stderr, "Parsing %s\n", command));

      session_start(&new, 0, ch, ch, ch);
      execute(command);
      session_end();
    }
}

void do_mudlle(struct char_data *ch, char *argument)
{
  if (no_specials)
    {
      cputs("Specials are disabled.\n\r", ch);
      return;
    }

  if (ch->desc)
    {
      /* Check argument, if exists do immediately */
      while (*argument == ' ') argument++;

      if (*argument)
	mudlle_act(NULL, ch->desc, argument);
      else
	{
	  push_input(ch->desc, mudlle_act, mudlle_prompt, NULL, NULL);
	  mudlle_init_player(ch);
	}
    }
}

void do_react_mudlle(struct char_data *ch, char *argument)
{
  char arg1[MAX_INPUT_LENGTH];

  TAKE_ARGUMENT(argument,arg1);

  if (!arg1[0])
    ch->player->mudlled = !ch->player->mudlled;
  else if (is_abbrev(arg1, "on"))
    ch->player->mudlled = TRUE;
  else if (is_abbrev(arg1, "off"))
    ch->player->mudlled = FALSE;
  else 
    {
      cputs("/react [on | off]\n\r", ch);
      return;
    }

  if (ch->player->mudlled)
    send_to_char("You become very susceptible to mudlle.\n\r", ch);
  else
    send_to_char("Your brain clears.\n\r", ch);
}
