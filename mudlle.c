/* $Log: mudlle.c,v $
 * Revision 1.15  1995/07/15  15:24:34  arda
 * Context cleanup.
 * Remove GCDEBUG.
 *
 * Revision 1.14  1995/01/22  15:11:46  arda
 * Linux patches.
 *
 * Revision 1.13  1994/10/09  06:42:40  arda
 * Libraries
 * Type inference
 * Many minor improvements
 *
 * Revision 1.12  1994/09/16  13:07:14  arda
 * Rename protect to catch.
 * New protect/unprotect functions (like dynpro/undynpro).
 *
 * Revision 1.11  1994/09/15  19:46:43  arda
 * Performance improvements:
 *   setjmp -> _setjmp (setjmp is horrendously slow)
 *   cold_protect
 * reset_limits split from reset_interpreter
 * fix division of negative numbers
 * Add ?\{n,r,t}
 * gc_size returns "mutable" size
 *
 * Revision 1.10  1994/08/26  08:51:43  arda
 * Keep free block list for string ports.
 *
 * Revision 1.9  1994/08/22  18:03:01  arda
 * Minor fixes.
 *
 * Revision 1.8  1994/08/17  16:29:57  arda
 * Seclevel fixes.
 *
 * Revision 1.7  1994/08/16  19:16:06  arda
 * Mudlle compiler for sparc now fully functional (68k compiler now needs
 * updating for primitives).
 * Changes to allow Sparc trap's for runtime errors.
 * Also added flags to primitives for better calling sequences.
 *
 * Revision 1.4  1994/02/24  08:33:00  arda
 * Owl: New error messages.
 *
 * Revision 1.3  1994/01/29  19:50:34  dgay
 * Owl: add file & line information to functions.
 *
 * Revision 1.2  1993/11/27  11:29:02  arda
 * Owl: Major changes to affect.
 *      Save mudlle data with players & objects.
 *      Change skill format on disk.
 *      Other minor changes.
 *      Still needs full debugging.
 *
 * Revision 1.1  1993/07/21  20:36:53  un_mec
 * Owl: Added &&, ||, optimised if.
 *      Added branches to the intermediate language.
 *      Separated destiniation language generation into ins module
 *      (with some peephole optimisation)
 *      Standalone version of mudlle (mkf, runtime/mkf, mudlle.c) added to CVS
 *
 */

static char rcsid[] = "$Id: mudlle.c,v 1.15 1995/07/15 15:24:34 arda Exp $";

#include <stdio.h>
#include "mudlle.h"
#include "mparser.h"
#include "lexer.h"
#include "compile.h"
#include "mvalues.h"
#include "alloc.h"
#include "global.h"
#include "print.h"
#include "utils.h"
#include "module.h"
#include "mcompile.h"
#include "call.h"
#include "runtime/runtime.h"

block_t memory;

extern FILE *yyin;
int debug_level = 0;

int load_file(char *name)
{
  FILE *f;
  value result;
  int ok;

  if (!(f = fopen(name, "r")))
    runtime_error(error_bad_value);

  read_from_file(f, name);
  ok = interpret(&result, 1, TRUE);
  fclose(f);

  return ok;
}

struct pload
{
  char *name;
  int result;
};

static void pload(void *_data)
{
  struct pload *data = _data;

  data->result = load_file(data->name);
}

int catch_load_file(char *name)
{
  struct pload data;

  data.name = name;
  if (catch(pload, &data, TRUE)) return data.result;
  else return FALSE;
}

static void execute(char *line)
{
  value result;

  read_from_string(line);
  if (interpret(&result, 1, TRUE))
    {
      printf("Result: ");
      mprint(stdout, prt_print, result);
      printf("\n");
    }
}

main(int argc, char **argv)
{
  char line[512];
  struct session_context context;

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

  session_start(&context, 0, 0, stdout, stdout);
  do
    {
      printf("mudlle> ");
      if (!gets(line)) break;
      execute(line);
    }
  while (1);
  session_end();
}
