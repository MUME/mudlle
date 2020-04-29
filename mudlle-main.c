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

#include "mudlle-config.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#ifdef USE_READLINE
#  include <readline/history.h>
#  include <readline/readline.h>

#  define HISTORY_FILE ".mudlle-history"
#endif

#include "compile.h"
#include "context.h"
#include "error.h"
#include "lexer.h"
#include "print.h"
#include "tree.h"
#include "types.h"

#include "runtime/runtime.h"

static void execute(const char *line, bool show_result)
{
  const char *const lines[] = { line, NULL };
  struct reader_state rstate;
  save_reader_state(&rstate);
  static const struct filename fname = {
    .path = "<string>",
    .nice = "<string>"
  };
  read_from_strings(lines, &fname, false);

  value result;
  if (interpret(&result, 1, true) && show_result)
    {
      printf("Result: ");
      output_value(mudout, prt_write, result);
      printf("\n");
    }

  restore_reader_state(&rstate);
}

#ifdef USE_READLINE
static void history_exit(void)
{
  write_history(HISTORY_FILE);
}

static bool ends_in_newline(const char *s)
{
  size_t l = strlen(s);
  return l > 0 && s[l - 1] == '\n';
}
#endif  /* USE_READLINE */


int main(int argc, char **argv)
{
  mudlle_init();

  define_string_vector("argv", (const char *const *)argv, argc);

#ifdef USE_READLINE
  bool is_tty = isatty(0);
  if (is_tty)
    {
      using_history();
      read_history(HISTORY_FILE);
      rl_bind_key('\t', rl_insert);

      if (atexit(history_exit))
        perror("atexit(history_exit)");
    }
#endif

  struct oport *out = make_file_oport(stdout);
  struct session_context context;
  session_start(&context,
                &(const struct session_info){
                  .mout        = out,
                  .merr        = out,
                  .maxseclevel = MAX_SECLEVEL });
  for (;;)
    {
#ifdef USE_READLINE
      char *alloc = NULL;
      const char *line;
      if (is_tty)
	line = alloc = readline("mudlle> ");
      else
	{
	  static char buf[1024];
	  line = fgets(buf, sizeof buf, stdin);
          if (line && !ends_in_newline(line))
            {
              fputs("Input line too long.\n", stderr);
              exit(EXIT_FAILURE);
            }
	}

      if (!line)
	break;
      if (*line)
        {
	  if (is_tty)
	    add_history(line);
	  execute(line, is_tty);
        }
      free(alloc);
#else
      char line[512];

      fputs("mudlle> ", stdout);
      fflush(stdout);
      if (!fgets(line, sizeof line, stdin))
	break;
      if (*line)
        execute(line, true);
#endif
    }
  session_end();

  return 0;
}
