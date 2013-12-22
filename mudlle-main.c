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

#include <stdio.h>
#include <stdlib.h>

#ifdef USE_READLINE
#  include <readline/history.h>
#  include <readline/readline.h>

#  define HISTORY_FILE ".mudlle-history"
#endif

#include "compile.h"
#include "context.h"
#include "error.h"
#include "lexer.h"
#include "mudlle.h"
#include "print.h"
#include "types.h"

#include "runtime/runtime.h"

extern FILE *yyin;
int debug_level = 0;

int load_file(const char *fullname, const char *filename,
              const char *nicename, int seclev, bool reload)
{
  FILE *f = fopen(fullname, "r");
  if (f == NULL)
    runtime_error(error_bad_value);

  read_from_file(f, filename, nicename);
  value result;
  int ok = interpret(&result, seclev, reload);
  fclose(f);

  return ok;
}

static void execute(char *line)
{
  value result;

  read_from_string(line, NULL, NULL);
  if (interpret(&result, 1, true))
    {
      printf("Result: ");
      output_value(mudout, prt_print, false, result);
      printf("\n");
    }
}

#ifdef USE_READLINE
static void history_exit(void)
{
  write_history(HISTORY_FILE);
}
#endif


int main(int argc, char **argv)
{
  struct session_context context;
  struct oport *out;

#ifdef USE_READLINE
  using_history();
  read_history(HISTORY_FILE);
  rl_bind_key('\t', rl_insert);
#endif

  mudlle_init();

  define_string_vector("argv", (const char *const *)argv, argc);

#ifdef USE_READLINE
  if (atexit(history_exit))
    perror("atexit(history_exit)");
#endif

  out = make_file_oport(stdout);
  session_start(&context,
                &(const struct session_info){ .mout = out, .merr = out });
  for (;;)
    {
#ifdef USE_READLINE
      char *line = readline((char *)"mudlle> ");

      if (!line)
	break;
      if (*line)
        {
	  add_history(line);
	  execute(line);
        }
      free(line);
#else
      char line[512];

      fputs("mudlle> ", stdout);
      fflush(stdout);
      if (!fgets(line, sizeof line, stdin))
	break;
      if (*line)
        execute(line);
#endif
    }
  session_end();

  return 0;
}
