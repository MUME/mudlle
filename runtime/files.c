/*
 * Copyright (c) 1993-2006 David Gay and Gustav H�llberg
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

#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#ifdef WIN32
#  include <io.h>
#endif
#include <dirent.h>

#ifndef WIN32
#  include <grp.h>
#  include <pwd.h>
#  include <glob.h>
#endif

#include "call.h"
#include "context.h"
#include "interpret.h"
#include "mparser.h"
#include "utils.h"

#include "runtime/files.h"
#include "runtime/io.h"
#include "runtime/runtime.h"


TYPEDOP(load, 0, "`s -> `b. Loads file `s. Returns true if successful",
        1, (struct string *name), 0, "s.n")
{
  char *fname;

  TYPEIS(name, type_string);
  LOCALSTR(fname, name);
  return makebool(load_file(name->str, fname, 1, true));
}


UNSAFETOP(mkdir, 0, "`s `n1 -> `n2. Make directory `s (mode `n1)",
          2, (struct string *name, value mode),
          OP_LEAF | OP_NOALLOC, "sn.n")
{
  TYPEIS(name, type_string);
  ISINT(mode);

#ifdef WIN32
  return makeint(mkdir(name->str));
#else
  return makeint(mkdir(name->str, intval(mode)));
#endif
}

UNSAFETOP(directory_files, 0, "`s -> `l. List all files of directory `s"
          " (returns false if problems)",
          1, (struct string *dir),
          OP_LEAF, "s.[ln]")
{
  DIR *d;

  TYPEIS(dir, type_string);

  if ((d = opendir(dir->str)))
    {
      struct dirent *entry;
      struct list *files = NULL;
      struct gcpro gcpro1;

      GCPRO1(files);
      while ((entry = readdir(d)))
	{
	  struct string *fname = alloc_string(entry->d_name);

	  files = alloc_list(fname, files);
	}
      UNGCPRO();
      closedir(d);

      return files;
    }
  return makebool(false);	  
}

#ifndef WIN32
UNSAFETOP(glob_files, 0,
          "`s0 `s1 `n -> `l. Returns a list of all files matched by "
          "the glob pattern `s1, executed in directory `s0, using flags in `n "
          "(`GLOB_xxx). Returns FALSE on error", 
          3, (struct string *dir, struct string *pat, value n),
          OP_LEAF, "ssn.[ln]")
{
  TYPEIS(pat, type_string);
  TYPEIS(dir, type_string);

  long flags = GETINT(n);

  const int allowed_flags =
    0
#ifdef GLOB_TILDE
    | GLOB_TILDE
#endif
#ifdef GLOB_BRACE
    | GLOB_BRACE
#endif
#ifdef GLOB_MARK
    | GLOB_MARK
#endif
#ifdef GLOB_NOCHECK
    | GLOB_NOCHECK
#endif
#ifdef GLOB_NOESCAPE
    | GLOB_NOESCAPE
#endif
#ifdef GLOB_PERIOD
    | GLOB_PERIOD
#endif
#ifdef GLOB_NOMAGIC
    | GLOB_NOMAGIC
#endif
#ifdef GLOB_ONLYDIR
    | GLOB_ONLYDIR
#endif
    ;

  if (flags & ~allowed_flags)
    runtime_error(error_bad_value);

  int orig_wd;
  if ((orig_wd = open(".", 0)) < 0)
    runtime_error(error_bad_value);

  if (chdir(dir->str) < 0)
    {
      close(orig_wd);
      runtime_error(error_bad_value);
    }

  glob_t files;
  int res = glob(pat->str, flags, NULL, &files);
  if (fchdir(orig_wd) < 0)
    {
      close(orig_wd);
      globfree(&files);
      runtime_error(error_bad_value);
    }

  close(orig_wd);

  if (res)
    {
      globfree(&files);
      return makebool(false);
    }

  struct list *l = NULL;
  struct gcpro gcpro1;
  GCPRO1(l);
  for (char **s = files.gl_pathv; *s; ++s)
    {
      struct string *f = alloc_string(*s);
      struct gcpro gcpro2;
      GCPRO(gcpro2, f);
      l = alloc_list(f, l);
      UNGCPRO1(gcpro2);
    }
  UNGCPRO();
  globfree(&files);
  return l;
}
#endif /* ! WIN32 */

static value build_file_stat(struct stat *sb)
{
  struct vector *info = alloc_vector(FILE_STAT_FIELDS);
  
  info->data[FS_DEV]     = makeint((int)sb->st_dev);
  info->data[FS_INO]     = makeint((int)sb->st_ino);
  info->data[FS_MODE]    = makeint(sb->st_mode);
  info->data[FS_NLINK]   = makeint(sb->st_nlink);
  info->data[FS_UID]     = makeint(sb->st_uid);
  info->data[FS_GID]     = makeint(sb->st_gid);
  info->data[FS_RDEV]    = makeint((int)sb->st_rdev);
  info->data[FS_SIZE]    = makeint((int)sb->st_size);
  info->data[FS_ATIME]   = makeint(sb->st_atime);
  info->data[FS_MTIME]   = makeint(sb->st_mtime);
  info->data[FS_CTIME]   = makeint(sb->st_ctime);
#ifndef WIN32
  info->data[FS_BLKSIZE] = makeint(sb->st_blksize);
  info->data[FS_BLOCKS]  = makeint((int)sb->st_blocks);
#endif
  
  return info;
}

UNSAFETOP(file_stat, 0, "`s -> `v. Returns status of file `s, or false for"
          " failure. See the `FS_xxx constants and `file_lstat().",
          1, (struct string *fname),
          OP_LEAF, "s.[vn]")
{
  struct stat sb;

  TYPEIS(fname, type_string);
  if (!stat(fname->str, &sb))
    return build_file_stat(&sb);
  else
    return makebool(false);
}

#ifndef WIN32
UNSAFETOP(real_path, 0,
          "`s0 -> `s1. Resolves any symbolic links, follows references to"
          " /./ and /../, and returns the canonicalized path for `s0."
          " Returns `errno on error.",
          1, (struct string *path), OP_LEAF, "s.S")
{
  char buf[PATH_MAX], *res;
  TYPEIS(path, type_string);
  if ((res = realpath(path->str, buf)) == NULL)
    return makeint(errno);
  return alloc_string(res);
}

UNSAFETOP(file_lstat, 0,
          "`s -> `v. Returns status of file `s (not following links)."
          " Returns FALSE for failure. See the `FS_xxx constants and"
          " `file_stat()",
          1, (struct string *fname),
          OP_LEAF, "s.[vn]")
{
  struct stat sb;

  TYPEIS(fname, type_string);
  if (!lstat(fname->str, &sb))
    return build_file_stat(&sb);
  else
    return makebool(false);
}

UNSAFETOP(readlink, 0,
          "`s1 -> `s2. Returns the contents of symlink `s, or FALSE "
          "for failure", 1, (struct string *lname), OP_LEAF, "s.S")
{
  struct stat sb;
  struct gcpro gcpro1;

  TYPEIS(lname, type_string);
  if (lstat(lname->str, &sb) ||
      !S_ISLNK(sb.st_mode))
    return makebool(false);
  
  GCPRO1(lname);
  struct string *res = alloc_empty_string(sb.st_size);
  UNGCPRO();

  if (readlink(lname->str, res->str, sb.st_size) < 0)
    return makebool(false);

  return res;
}
#endif /* ! WIN32 */

UNSAFETOP(file_regularp, "file_regular?",
          "`s -> `b. Returns TRUE if `s is a regular file",
          1, (struct string *fname),
          OP_LEAF | OP_NOALLOC, "s.n")
{
  TYPEIS(fname, type_string);
  struct stat sb;
  return makebool(stat(fname->str, &sb) == 0
                  && S_ISREG(sb.st_mode));
}

UNSAFETOP(remove, 0, "`s -> `b. Removes file `s, returns TRUE if success",
	  1, (struct string *fname),
	  OP_LEAF, "s.n")
{
  TYPEIS(fname, type_string);

  return makebool(unlink(fname->str) == 0);
}

UNSAFETOP(rename, 0,
          "`s1 `s2 -> `n. Renames file `s1 to `s2. Returns the Unix error "
          "number or 0 for success",
	  2, (struct string *oldname, struct string *newname),
	  OP_LEAF, "ss.n")
{
  TYPEIS(oldname, type_string);
  TYPEIS(newname, type_string);

  return makeint(rename(oldname->str, newname->str) ? errno : 0);
}

#ifndef WIN32
UNSAFETOP(chown, 0, "`s1 `n0 `n1 -> `n2. Changed owner of file `s1 to uid `n0"
          " and gid `n1. Use -1 not to change that field."
          " Returns errno or 0 for success.",
          3, (struct string *fname, value uid, value gid),
          OP_LEAF, "snn.n")
{
  TYPEIS(fname, type_string);
  return makeint(chown(fname->str, GETINT(uid), GETINT(gid)) ? errno : 0);
		 
}

UNSAFETOP(chmod, 0, "`s1 `n0 -> `n1. Changed mode of file `s1 to `n0. "
          " Returns errno or 0 for success.",
          2, (struct string *fname, value mode),
          OP_LEAF, "sn.n")
{
  TYPEIS(fname, type_string);
  return makeint(chmod(fname->str, GETINT(mode)) ? errno : 0);
		 
}
#endif /* ! WIN32 */

TYPEDOP(strerror, 0, "`n -> `s. Returns an error string corresponding to"
        " errno `n, or `false if there is no such errno.",
        1, (value merrno), OP_LEAF, "n.S")
{
  const char *s = strerror(GETINT(merrno));
  return s ? alloc_string(s) : makebool(false);
}

UNSAFETOP(print_file_part, 0,
          "`oport `s1 `n0 `x -> `n1. Print `x bytes of contents of file"
          " `s1 starting from byte `n0, or the rest of the file if"
          " `x is null.\n"
          "Output it sent to port `oport.\n"
          "Returns zero if successful, a Unix errno value"
          " if there was file I/O error, or -1 if `oport became too full.\n"
          "It is not an error to start after the end of the file, nor to"
          " try to read beyond the end of the file.",
	  4, (struct oport *p, struct string *name, value mstart,
              value mbytes),
          OP_LEAF, "osn[nu].n")
{
  TYPEIS(name, type_string);

  long start = GETINT(mstart);
  if (start < 0)
    runtime_error(error_bad_value);
  long bytes;
  if (mbytes == NULL)
    bytes = -1;
  else
    {
      bytes = GETINT(mbytes);
      if (bytes < 0)
        runtime_error(error_bad_value);
    }

  FILE *f = fopen(name->str, "r");
  if (f == NULL)
    return makeint(errno);

  if (start != 0 && fseek(f, start, SEEK_SET) == -1)
    {
      int r = errno;
      fclose(f);
      return makeint(r);
    }

  struct gcpro gcpro1;
  GCPRO1(p);
  p = get_oport(p);

  int result = 0;
  char *buf = malloc(16 * 1024);
  while (bytes)
    {
      long toread = (bytes < 0 || bytes > sizeof buf
                     ? sizeof buf
                     : bytes);
      size_t res = fread(buf, 1, toread, f);
      if (res == 0)
        {
          result = ferror(f) ? EIO : 0;
          break;
        }
      struct oport_stat obuf;
      opstat(p, &obuf);
      if (obuf.size >= (obuf.type == oport_type_file
                        ? 10 * 1024 * 1024
                        : 128 * 1024))
        {
          result = -1;
          break;
        }
      opwrite(p, buf, res);
      if (bytes >= 0)
        bytes -= res;
    }
  free(buf);
  UNGCPRO();
  fclose(f);

  return makeint(result);
}

UNSAFETOP(print_file, 0,
          "`oport `s1 -> `n. Print the contents of file `s1 and to output"
          " port `oport. Returns zero if successful, a Unix errno value"
          " if there was file I/O error, or -1 if `oport became too full."
          " Cf. `print_file_part().",
	  2, (struct oport *p, struct string *name),
	  OP_LEAF, "os.n")
{
  return code_print_file_part(p, name, makeint(0), NULL);
}

UNSAFETOP(file_read, 0,
          "`s1 -> `s2. Reads file `s1 and returns its contents (or "
          "the Unix errno value)",
	  1, (struct string *name),
	  OP_LEAF, "s.S")
{
  int fd;

  TYPEIS(name, type_string);

  if ((fd = open(name->str, O_RDONLY)) >= 0)
    {
      off_t size = lseek(fd, 0, SEEK_END);

      if (size >= 0)
	{
	  struct string *s = alloc_empty_string(size);

	  if (lseek(fd, 0, SEEK_SET) == 0 && read(fd, s->str, size) == size)
	    {
	      close(fd);
	      return s;
	    }
	}
      close(fd);
    }
  return makeint(errno);
}

UNSAFETOP(file_write, 0,
          "`s1 `s2 -> `n. Writes `s2 to file `s1. Creates `s1 if it "
          "doesn't exist. Returns the Unix return code (0 for success)",
          2, (struct string *file, struct string *data),
          OP_LEAF, "ss.n")
{
  int fd;
  TYPEIS(file, type_string);
  TYPEIS(data, type_string);

  fd = open(file->str, O_WRONLY | O_CREAT | O_TRUNC, 0666);
  if (fd != -1)
    {
      size_t len = string_len(data);
      size_t res = write(fd, data->str, len);
      close(fd);
      if (res == len)
	return makeint(0);
    }      

  return makeint(errno);
}

UNSAFETOP(file_append, 0,
          "`s1 `s2 -> `n. Appends string `s2 to file `s1. Creates `s1 "
          "if nonexistent. Returns the Unix error number for failure, 0 for "
          "success.",
          2, (struct string *file, struct string *val), 
          OP_LEAF | OP_NOESCAPE, "ss.n")
{
  int fd;
  unsigned long size;

  TYPEIS(file, type_string);
  TYPEIS(val, type_string);

  fd = open(file->str, O_WRONLY | O_CREAT | O_APPEND, 0666);
  if (fd != -1)
    {
#ifndef WIN32
      fchmod(fd, 0666);   /* ### What's the point of this? - finnag */
#endif
      int len;
      size = string_len(val);
      len = write(fd, val->str, size);
      close(fd);
      if (len == size)
        return makeint(0);
    }
  return makeint(errno);
}

#ifndef WIN32
TYPEDOP(passwd_file_entries, 0,
        " -> `l. Returns a list of [ `pw_name `pw_uid "
        "`pw_gid `pw_gecos `pw_dir `pw_shell ] from the contents of "
        "/etc/passwd. Cf. `getpwent(3)",
        0, (void), OP_LEAF, ".l")
{
  struct list *res = NULL;
  struct gcpro gcpro1, gcpro2;
  struct passwd *pw;
  struct vector *v = NULL;

  GCPRO2(res, v);
  
  setpwent();
  for (;;) 
    {
      do
	{
	  errno = 0;
	  pw = getpwent();
	}
      while (pw == NULL && errno == EINTR);
    
      if (pw == NULL)
	{
	  int en = errno;
	  endpwent();
	  if (en)
	    runtime_error(error_bad_value);
	  break;
	}

      v = alloc_vector(6);
      SET_VECTOR(v, PW_NAME,  alloc_string(pw->pw_name));
      SET_VECTOR(v, PW_UID,   makeint(pw->pw_uid));
      SET_VECTOR(v, PW_GID,   makeint(pw->pw_gid));
      SET_VECTOR(v, PW_GECOS, alloc_string(pw->pw_gecos));
      SET_VECTOR(v, PW_DIR,   alloc_string(pw->pw_dir));
      SET_VECTOR(v, PW_SHELL, alloc_string(pw->pw_shell));

      res = alloc_list(v, res);
    }

  UNGCPRO();

  return res;
}

TYPEDOP(group_file_entries, 0, 
        " -> `l. Returns a list of [ `gr_name `gr_gid "
        "( `gr_mem ... ) ] from the contents of /etc/group."
        " Cf. `getgrent(3)",
        0, (void), OP_LEAF, ".l")
{
  struct list *res = NULL, *l = NULL;
  struct gcpro gcpro1, gcpro2, gcpro3;
  struct group *grp;
  struct vector *v = NULL;

  GCPRO3(res, v, l);
  
  setgrent();
  for (;;) 
    {
      do
	{
	  errno = 0;
	  grp = getgrent();
	}
      while (grp == NULL && errno == EINTR);
    
      if (grp == NULL)
	{
	  int en = errno;
	  endpwent();
	  if (en)
	    runtime_error(error_bad_value);
	  break;
	}

      v = alloc_vector(3);
      SET_VECTOR(v, GR_NAME, alloc_string(grp->gr_name));
      SET_VECTOR(v, GR_GID, makeint(grp->gr_gid));

      l = NULL;
      for (char **s = grp->gr_mem; *s; ++s)
	{
	  struct string *str = alloc_string(*s);
	  l = alloc_list(str, l);
	}

      SET_VECTOR(v, GR_MEM, l);

      res = alloc_list(v, res);
    }

  UNGCPRO();

  return res;
}
#endif /* ! WIN32 */

#define DEF(s) system_define(#s, makeint(s))

void files_init(void)
{
  DEFINE(load);
  DEFINE(file_read);
  DEFINE(file_write);
  DEFINE(file_append);
  DEFINE(print_file);
  DEFINE(print_file_part);

  DEFINE(mkdir);
  DEFINE(directory_files);
  DEFINE(file_stat);

#ifndef WIN32
  DEFINE(real_path);
  DEFINE(readlink);
  DEFINE(file_lstat);
  DEFINE(passwd_file_entries);
  DEFINE(group_file_entries);

  DEF(S_IFMT);
  DEF(S_IFSOCK);
  DEF(S_IFLNK);
  DEF(S_IFBLK);
  DEF(S_IFREG);
  DEF(S_IFDIR);
  DEF(S_IFCHR);
  DEF(S_IFIFO);
  DEF(S_ISUID);
  DEF(S_ISGID);
  DEF(S_ISVTX);
  DEF(S_IRWXU);
  DEF(S_IRUSR);
  DEF(S_IWUSR);
  DEF(S_IXUSR);
  DEF(S_IRWXG);
  DEF(S_IRGRP);
  DEF(S_IWGRP);
  DEF(S_IXGRP);
  DEF(S_IRWXO);
  DEF(S_IROTH);
  DEF(S_IWOTH);
  DEF(S_IXOTH);

  DEFINE(chown);
  DEFINE(chmod);

  DEFINE(file_regularp);
  DEFINE(remove);
  DEFINE(rename);

  DEFINE(strerror);

  DEFINE(glob_files);
#ifdef GLOB_MARK
  DEF(GLOB_MARK);
#endif
#ifdef GLOB_NOCHECK
  DEF(GLOB_NOCHECK);
#endif
#ifdef GLOB_NOESCAPE
  DEF(GLOB_NOESCAPE);
#endif
#ifdef GLOB_TILDE
  DEF(GLOB_TILDE);
#endif
#ifdef GLOB_BRACE
  DEF(GLOB_BRACE);
#endif
#ifdef GLOB_PERIOD
  DEF(GLOB_PERIOD);
#endif
#ifdef GLOB_NOMAGIC
  DEF(GLOB_NOMAGIC);
#endif
#ifdef GLOB_ONLYDIR
  DEF(GLOB_ONLYDIR);
#endif
#endif /* ! WIN32 */
}
