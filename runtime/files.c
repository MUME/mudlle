/* $Log: files.c,v $
 * Revision 1.32  1995/07/30  14:23:37  arda
 * Undocumented changes, as usual
 *
 * Revision 1.31  1995/07/15  15:24:57  arda
 * Context cleanup.
 * Remove GCDEBUG.
 *
 * Revision 1.30  1995/06/04  14:24:33  arda
 * Rename/move some files, misc. junk
 *
 * Revision 1.29  1994/10/09  06:44:06  arda
 * Libraries
 * Type inference
 * Many minor improvements
 *
 * Revision 1.28  1994/09/16  13:07:24  arda
 * Rename protect to catch.
 * New protect/unprotect functions (like dynpro/undynpro).
 *
 * Revision 1.27  1994/09/16  09:12:12  arda
 * start_process
 *
 * Revision 1.26  1994/09/01  13:05:03  arda
 * David
 *
 * Revision 1.25  1994/08/31  13:03:36  arda
 * Bug fixes (argh, no, new version of characters structures! (MD))
 *
 * Revision 1.24  1994/08/29  15:44:52  arda
 * Mudlle stuff
 *
 * Revision 1.23  1994/08/23  09:30:49  arda
 * Final(?) changes for mudlle compiler.
 * Changed code organisation in mudlle directory.
 *
 * Revision 1.22  1994/08/22  11:18:54  arda
 * Changes for mudlle compiler in MUME.
 *
 * Revision 1.21  1994/08/17  10:49:38  arda
 * Misc. fixes
 *
 * Revision 1.20  1994/08/17  10:19:54  arda
 * Improved make depend.
 * basic_load for compiler, select_reactor.
 *
 * Revision 1.19  1994/08/16  19:17:03  arda
 * Added flags to primitives for better calling sequences.
 *
 * Revision 1.15  1994/02/11  10:00:22  dgay
 * Owl: -Wall
 *      new shared string handling
 *      configuration file
 *
 * Revision 1.14  1994/02/03  19:22:33  arda
 * nothing special(3)
 *
 * Revision 1.13  1994/01/29  19:50:57  dgay
 * Owl: add file & line information to functions.
 *
 * Revision 1.12  1994/01/27  17:08:46  arda
 * Hmm.
 *
 * Revision 1.11  1994/01/07  08:09:10  dgay
 * Owl: Global events.
 *
 * Revision 1.10  1993/12/31  09:19:17  arda
 * Owl: New primitives, events to allow replacement of special procs.
 *
 * Revision 1.9  1993/12/29  10:50:38  arda
 * divers
 *
 * Revision 1.8  1993/12/04  22:41:17  arda
 * Owl: Changed room instance data to use a gdbm database.
 *      Removed number field on world.
 *      Added mudlle data on rooms.
 *      Included source for converters from update.
 *
 * Revision 1.7  1993/08/14  16:43:48  un_mec
 * Owl: New input system (with an input stack) => small interaction changes
 *
 * Revision 1.6  1993/07/25  10:59:01  un_mec
 * Owl: Move mudlle files, delete 0-length files, cancel timed ops.
 *
 * Revision 1.5  1993/06/25  15:38:08  un_autre
 * *** empty log message ***
 *
 * Revision 1.4  1993/05/29  13:25:31  un_autre
 * Bug fixes.
 *
 * Revision 1.3  1993/04/24  15:20:55  un_mec
 * Owl: Code cleanup.
 *
 * Revision 1.2  1993/04/22  18:59:16  un_autre
 * (MD) & Owl. Bug fixes. /player fixes. EVER_WHINER flag. saving_spells adjusted.
 *
 * Revision 1.1  1993/04/17  11:12:23  un_mec
 * Owl: A few new functions.
 *
 */

static char rcsid[] = "$Id: files.c,v 1.32 1995/07/30 14:23:37 arda Exp $";

#include <sys/time.h>
#include <alloca.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>

#include "runtime/runtime.h"
#include "print.h"
#include "utils.h"
#include "mparser.h"
#include "interpret.h"
#include "runtime/files.h"

#ifdef MUME
#include "interact.h"
#include "invoke.h"

#include "def.files.h"
#include "struct.time.h"
#include "struct.player.h"
#include "struct.socket.h"
#include "model_mobile.h"
#include "db.world.h"
#include "callback.h"
#include "editor.h"
#include "pager.h"
#include "parser.h"
#include "utils.files.h"
#include "utils.gods.h"

static void edit_ended(struct descriptor_data *d, char *fname, char *text)
{
  struct char_data *ch = d->character;

  if (text)
    if (!*text)		/* Empty text, delete file */
      {
	if (unlink(fname) == 0)
	  cputs("Empty file, deleted.\n\r", ch);
	else
	  cputs("Failed to delete an empty file - contact an implementor.\n\r", ch);
      }
}

static void view_ended(struct descriptor_data *d)
{
}

UNSAFEOP(mudlle_edit, "s n -> . User edits or views mudlle file s (owner n)",
	 2, (struct string *name, value _owner),
	 OP_LEAF)
{
  long owner;

  TYPEIS(name, type_string);
  ISINT(_owner); owner = intval(_owner);

  if (muduser && muduser->player && muduser->desc)
    {
      if (mudlle_write_access(muduser, owner))
	fileedit(muduser, name->str, "Edit mudlle file", edit_ended);
      else if (mudlle_read_access(muduser, owner))
	fileview(muduser->desc, name->str, view_ended);
      else
	cputs("You can't even look at that file.\n\r", muduser);
    }
  undefined();
}

static void mumedit_free(void *_data)
{
  struct dynpro *data = _data;

  undynpro(data);
  free(data);
}

static void mumedit_callback(void *_data, struct descriptor_data *d,
			     char *text, int len)
{
  struct string *mtext;
  struct gcpro gcpro1;
  struct character *who;
  value continuation;
  struct vector *args;
  struct session_context new;

  if (text)
    {
      char *ntext = split_lines(text, len, "\n\r", TRUE);

      mtext = alloc_string(ntext);
      new_free(ntext);
    }
  else mtext = NULL;
  GCPRO1(mtext);
  args = alloc_vector(2);
  UNGCPRO();
  args->data[1] = mtext;

  GCPRO1(args);
  who = char_mudlle(d->character);
  args->data[0] = who;
  UNGCPRO();

  continuation = ((struct dynpro *)_data)->obj;
  mumedit_free(_data);
  session_start(&new, 0, NULL, NULL, NULL);
  catch_call(continuation, args);
  session_end();
}

TYPEDOP(textedit, "p s1 s2 n fn -> b. Have p edit text s1 (descr s2) with key n.\n\
Call fn(p, text) when the edit is done (text=null for cancel).\n\
Returns true if the edit could proceed, false if there was a key conflict",
	5, (struct character *who, struct string *text, struct string *descr,
	    value key, value continuation),
	OP_LEAF | OP_NOESCAPE, "ossnf.n")
{
  struct dynpro *data;
  callback_t cb;

  TYPEIS(who, type_character);
  TYPEIS(text, type_string);
  TYPEIS(descr, type_string);
  ISINT(key);
  callable(continuation, 2);

  if (!CHAR_DESCRIPTOR(who->ch)) runtime_error(error_bad_value);

  data = xmalloc(sizeof *data);
  data->next = data->prev = NULL;
  dynpro(data, continuation);

  init_callback(cb, mumedit_callback, data, mumedit_free);
  if (edit(CHAR_DESCRIPTOR(who->ch), intval(key), text->str, string_len(text),
	   descr->str, &cb))
    return makebool(TRUE);
  else
    {
      mumedit_free(data);
      return makebool(FALSE);
    }
}

static void program_ended(void *data, struct descriptor_data *who)
{
  struct session_context new;

  session_start(&new, 0, NULL, NULL, NULL);
  catch_call0(unprotect(data));
  session_end();
}

static void program_free(void *data)
{
  unprotect(data);
}

VAROP(start_process, "p s v1 v2 b fn -> . Execute program s with argv v1, envp v2. Call fn when it terminates. If b is true, make connection interactive",
	 0)
{
  long i, argc, envc;
  char **argv_c, **envp_c;
  struct character *who;
  struct string *program;
  struct vector *argv, *envp;
  value interactive, cont;

  if (seclevel < LVL_IMPLEMENTOR) runtime_error(error_security_violation);
  if (nargs != 6) runtime_error(error_wrong_parameters);

  who = args->data[0];
  program = args->data[1];
  argv = args->data[2];
  envp = args->data[3];
  interactive = args->data[4];
  cont = args->data[5];

  TYPEIS(who, type_character);
  TYPEIS(program, type_string);
  TYPEIS(argv, type_vector);
  TYPEIS(envp, type_vector);
  callable(cont, 0);

  argc = vector_len(argv);
  if (argc < 1) runtime_error(error_bad_value);
  argv_c = alloca((argc + 1) * sizeof(char *));
  for (i = 0; i < argc; i++)
    {
      struct string *s = argv->data[i];

      TYPEIS(s, type_string);
      argv_c[i] = s->str;
    }
  argv_c[argc] = NULL;

  envc = vector_len(envp);
  envp_c = alloca((envc + 1) * sizeof(char *));
  for (i = 0; i < envc; i++)
    {
      struct string *s = envp->data[i];

      TYPEIS(s, type_string);
      envp_c[i] = s->str;
    }
  envp_c[envc] = NULL;

  if (who->ch->desc)
    {
      callback_t cb;

      init_callback(cb, program_ended, protect(cont), program_free);
      start_program(who->ch->desc, program->str, argv_c, envp_c,
		    istrue(interactive), &cb);
    }
  undefined();
}
	 

SECOP(basic_load, "s1 s2 n b1 -> b2. Loads file s1 (nice name s2) at security level n.n\
If b1 is false, libraries are not reloaded. True if successful",
      4, (struct string *name, struct string *nicename, value seclev, value reload),
      LVL_IMPLEMENTOR, 0)
{
  char *fname;
  long sl;

  TYPEIS(name, type_string);
  ISINT(seclev);
  sl = intval(seclev);
  if (sl < 0 || sl > LVL_IMPLEMENTOR) runtime_error(error_bad_value);

  fname = alloca(strlen(nicename->str) + 1);
  strcpy(fname, nicename->str);
  return makebool(load_file(name->str, fname, sl, istrue(reload)));
}

#else

OPERATION(load, "s -> . Loads file s", 1, (struct string *name), 0)
{
  TYPEIS(name, type_string);
  return makebool(load_file(name->str));
}

#endif

UNSAFEOP(mkdir, "s n1 -> n2. Make directory s (mode n1)",
	 2, (struct string *name, value mode),
	 OP_LEAF | OP_NOALLOC)
{
  TYPEIS(name, type_string);
  ISINT(mode);

  return makeint(mkdir(name->str, intval(mode)));
}

UNSAFEOP(directory_files, "s -> l. List all files of directory s (returns false if problems)",
	 1, (struct string *dir),
	 OP_LEAF)
{
  DIR *d;

  TYPEIS(dir, type_string);

  if (d = opendir(dir->str))
    {
      struct dirent *entry;
      struct list *files = NULL;
      struct gcpro gcpro1;

      GCPRO1(files);
      while (entry = readdir(d))
	{
	  struct string *fname = alloc_string(entry->d_name);

	  files = alloc_list(fname, files);
	}
      UNGCPRO();
      closedir(d);

      return files;
    }
  return makebool(FALSE);	  
}

UNSAFEOP(file_stat, "s -> v. Returns status of file s (returns false for failure)",
	 1, (struct string *fname),
	 OP_LEAF)
{
  struct stat sb;

  TYPEIS(fname, type_string);
  if (!stat(fname->str, &sb))
    {
      struct vector *info = alloc_vector(13);

      info->data[0] = makeint(sb.st_dev);
      info->data[1] = makeint(sb.st_ino);
      info->data[2] = makeint(sb.st_mode);
      info->data[3] = makeint(sb.st_nlink);
      info->data[4] = makeint(sb.st_uid);
      info->data[5] = makeint(sb.st_gid);
      info->data[6] = makeint(sb.st_rdev);
      info->data[7] = makeint(sb.st_size);
      info->data[8] = makeint(sb.st_atime);
      info->data[9] = makeint(sb.st_mtime);
      info->data[10] = makeint(sb.st_ctime);
      info->data[11] = makeint(sb.st_blksize);
      info->data[12] = makeint(sb.st_blocks);

      return info;
    }
  return makebool(FALSE);
}

UNSAFEOP(remove, "s -> b. Removes file s, returns TRUE if success",
	  1, (struct string *fname),
	  OP_LEAF)
{
  TYPEIS(fname, type_string);

  return makebool(unlink(fname->str) == 0);
}

void files_init(void)
{
#ifdef MUME
  DEFINE("textedit", textedit);
  DEFINE("mudlle_edit", mudlle_edit);
  DEFINE("start_process", start_process);
  DEFINE("basic_load", basic_load);
#else
  DEFINE("load", load);
#endif
  DEFINE("mkdir", mkdir);
  DEFINE("directory_files", directory_files);
  DEFINE("file_stat", file_stat);
  DEFINE("remove", remove);
}
