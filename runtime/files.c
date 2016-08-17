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

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <utime.h>

#ifdef WIN32
#  include <io.h>
#else
#  include <grp.h>
#  include <pwd.h>
#  include <glob.h>
#endif

#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/statvfs.h>

#include "../call.h"
#include "../compile.h"
#include "../interpret.h"
#include "../mparser.h"
#include "../ports.h"
#include "../utils.h"

#include "bigint.h"
#include "files.h"
#include "io.h"
#include "mudlle-float.h"
#include "runtime.h"


TYPEDOP(load, 0, "`s -> `b. Loads file `s. Returns true if successful",
        1, (struct string *name), OP_STR_READONLY | OP_TRACE, "s.n")
{
  TYPEIS(name, type_string);
  char *fname;
  ALLOCA_PATH(fname, name);
  return makebool(load_file(fname, fname, fname, 1, true));
}


UNSAFETOP(mkdir, 0, "`s `n1 -> `n2. Make directory `s (mode `n1)."
          " Returns Unix errno.",
          2, (struct string *name, value mode),
          OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_STR_READONLY, "sn.n")
{
  TYPEIS(name, type_string);
  ISINT(mode);

#ifdef WIN32
  return makeint(mkdir(name->str));
#else
  return makeint(mkdir(name->str, intval(mode)));
#endif
}

UNSAFETOP(rmdir, 0, "`s -> `n. Remove directory `s. Returns Unix errno.",
          1, (struct string *name),
          OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_STR_READONLY, "s.n")
{
  TYPEIS(name, type_string);
  return makeint(rmdir(name->str));
}

UNSAFETOP(directory_files, 0, "`s -> `l. List all files of directory `s"
          " (returns false if problems)",
          1, (struct string *dir),
          OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "s.[ln]")
{
  TYPEIS(dir, type_string);

  DIR *d = opendir(dir->str);
  if (d == NULL)
    return makebool(false);

  struct list *files = NULL;
  GCPRO1(files);
  for (struct dirent *entry; (entry = readdir(d)); )
    {
      struct string *fname = alloc_string(entry->d_name);
      files = alloc_list(fname, files);
    }
  UNGCPRO();
  closedir(d);

  return files;
}

#ifndef WIN32
UNSAFETOP(glob_files, 0,
          "`s0 `s1 `n -> `l. Returns a list of all files matched by "
          "the glob pattern `s1, executed in directory `s0, using flags in `n "
          "(`GLOB_xxx). Returns FALSE on error",
          3, (struct string *dir, struct string *pat, value n),
          OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "ssn.[ln]")
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
  GCPRO1(l);
  for (char **s = files.gl_pathv; *s; ++s)
    {
      struct string *f = alloc_string(*s);
      l = alloc_list(f, l);
    }
  UNGCPRO();
  globfree(&files);
  return l;
}
#endif /* ! WIN32 */

static value make_timespec(const struct timespec *ts)
{
  return makefloat(ts->tv_sec + ts->tv_nsec * 1e-9);
}

#ifdef __MACH__
#define st_atim st_atimespec
#define st_mtim st_mtimespec
#define st_ctim st_ctimespec
#endif

static value build_file_stat(struct stat *sb)
{
  struct vector *info = alloc_vector(FILE_STAT_FIELDS);
  GCPRO1(info);

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
  SET_VECTOR(info, FS_AFTIME, make_timespec(&sb->st_atim));
  SET_VECTOR(info, FS_MFTIME, make_timespec(&sb->st_mtim));
  SET_VECTOR(info, FS_CFTIME, make_timespec(&sb->st_ctim));

  UNGCPRO();

  return info;
}

UNSAFETOP(file_stat, 0, "`s -> `v. Returns status of file `s, or false for"
          " failure. See the `FS_xxx constants and `file_lstat().",
          1, (struct string *fname),
          OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "s.[vz]")
{
  struct stat sb;

  TYPEIS(fname, type_string);
  if (!stat(fname->str, &sb))
    return build_file_stat(&sb);
  else
    return makebool(false);
}

#ifdef USE_GMP
TYPEDOP(file_system_stat, 0, "`s -> `v. Returns file system statistics"
        " for the file system containing file `s, or Unix errno for error.\n"
        "Note that all entries may contain bigints if the result does"
        " not fit in an integer.\n"
        "See the `FSYS_xxx and `FILE_SYSTEM_STAT_FIELDS constants.",
        1, (struct string *fname),
        OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "s.[vn]")
{
  TYPEIS(fname, type_string);

  struct statvfs sb;
  if (statvfs(fname->str, &sb) < 0)
    return makeint(errno);

  struct vector *v = alloc_vector(FILE_SYSTEM_STAT_FIELDS);
  GCPRO1(v);

#define SETVFS(FIELD, field) \
  SET_VECTOR(v, FSYS_ ## FIELD, make_unsigned_int_or_bigint(sb.f_ ## field))

  SETVFS(BSIZE,   bsize);
  SETVFS(FRSIZE,  frsize);
  CASSERT_EXPR((fsblkcnt_t)-1 > 0 && (fsblkcnt_t)-1 <= ULONG_MAX);
  SETVFS(BLOCKS,  blocks);
  SETVFS(BFREE,   bfree);
  SETVFS(BAVAIL,  bavail);
  CASSERT_EXPR((fsfilcnt_t)-1 > 0 && (fsfilcnt_t)-1 <= ULONG_MAX);
  SETVFS(FILES,   files);
  SETVFS(FFREE,   ffree);
  SETVFS(FAVAIL,  favail);
  SETVFS(FSID,    fsid);
  SETVFS(FLAG,    flag);
  SETVFS(NAMEMAX, namemax);

#undef SETVFS

  UNGCPRO();
  return v;
}
#endif  /* USE_GMP */

#ifndef WIN32
UNSAFETOP(real_path, 0,
          "`s0 -> `s1. Resolves any symbolic links, follows references to"
          " /./ and /../, and returns the canonicalized path for `s0."
          " Returns `errno on error.",
          1, (struct string *path),
          OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "s.S")
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
          OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "s.[vz]")
{
  struct stat sb;

  TYPEIS(fname, type_string);
  if (!lstat(fname->str, &sb))
    return build_file_stat(&sb);
  else
    return makebool(false);
}

UNSAFETOP(file_utime, 0,
          "`s `x -> `n. Sets the access and modification times"
          " of file `s. If `x is `null, set both to the current time;"
          " if `x is a pair, set them to `car(`x) and `cdr(`x), respectively."
          " Returns zero on success or `errno on failure.",
          2, (struct string *fname, struct list *mtimes),
          OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "sl.n")
{
  TYPEIS(fname, type_string);

  struct utimbuf buf, *times;
  if (mtimes == NULL)
    times = NULL;
  else
    {
      TYPEIS(mtimes, type_pair);
      buf.actime = GETUINT(mtimes->car);
      buf.modtime = GETUINT(mtimes->cdr);
      times = &buf;
    }

  if (utime(fname->str, times) < 0)
    return makeint(errno);
  return makeint(0);
}

UNSAFETOP(readlink, 0,
          "`s1 -> `s2. Returns the contents of symlink `s, or FALSE "
          "for failure", 1, (struct string *lname),
          OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "s.[sz]")
{
  TYPEIS(lname, type_string);

  struct stat sb;
  if (lstat(lname->str, &sb) || !S_ISLNK(sb.st_mode))
    return makebool(false);

  if (sb.st_size > MAX_STRING_SIZE)
    return makebool(false);

  char buf[sb.st_size ? sb.st_size : PATH_MAX];

  ssize_t r = readlink(lname->str, buf, sizeof buf);
  if (r < 0)
    return makebool(false);

  struct string *res = alloc_empty_string(r);
  memcpy(res->str, buf, r);
  return res;
}
#endif /* ! WIN32 */

UNSAFETOP(file_regularp, "file_regular?",
          "`s -> `b. Returns TRUE if `s is a regular file.",
          1, (struct string *fname),
          OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_STR_READONLY, "s.n")
{
  TYPEIS(fname, type_string);
  struct stat sb;
  return makebool(stat(fname->str, &sb) == 0 && S_ISREG(sb.st_mode));
}

UNSAFETOP(directoryp, "directory?",
          "`s -> `b. Returns TRUE if `s is a directory.",
          1, (struct string *dname),
          OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_STR_READONLY, "s.n")
{
  TYPEIS(dname, type_string);
  struct stat sb;
  return makebool(stat(dname->str, &sb) == 0 && S_ISDIR(sb.st_mode));
}

UNSAFETOP(remove, 0, "`s -> `n. Removes file `s. Returns the Unix error"
          " number or 0 for success.",
	  1, (struct string *fname),
	  OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "s.n")
{
  TYPEIS(fname, type_string);
  return makeint(unlink(fname->str) ? errno : 0);
}

UNSAFETOP(rename, 0,
          "`s1 `s2 -> `n. Renames file `s1 to `s2. Returns the Unix error "
          "number or 0 for success",
	  2, (struct string *oldname, struct string *newname),
	  OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "ss.n")
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
          OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "snn.n")
{
  TYPEIS(fname, type_string);
  return makeint(chown(fname->str, GETINT(uid), GETINT(gid)) ? errno : 0);
}

UNSAFETOP(chmod, 0, "`s1 `n0 -> `n1. Changed mode of file `s1 to `n0. "
          " Returns errno or 0 for success.",
          2, (struct string *fname, value mode),
          OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "sn.n")
{
  TYPEIS(fname, type_string);
  return makeint(chmod(fname->str, GETINT(mode)) ? errno : 0);
}
#endif /* ! WIN32 */

TYPEDOP(strerror, 0, "`n -> `s. Returns an error string corresponding to"
        " errno `n, or `false if there is no such errno.",
        1, (value merrno), OP_LEAF | OP_NOESCAPE, "n.[sz]")
{
  const char *s = strerror(GETINT(merrno));
  return s ? alloc_string(s) : makebool(false);
}

TYPEDOP(getenv, 0, "`s0 -> `s1|false. Return the value of the environment"
        " variable `s0, or false if no match.",
        1, (struct string *s), OP_LEAF | OP_NOESCAPE | OP_STR_READONLY,
        "s.[sz]")
{
  TYPEIS(s, type_string);
  char *r = getenv(s->str);
  return r == NULL ? makebool(false) : alloc_string(r);
}

TYPEDOP(getcwd, 0, "-> `s. Returns the name of the current working directory,"
        " or a Unix errno value on error. Cf. `strerror",
        0, (void), OP_LEAF | OP_NOESCAPE, ".S")
{
  char buf[PATH_MAX];
  char *wd = getcwd(buf, sizeof buf);
  return wd == NULL ? makeint(errno) : alloc_string(wd);
}

UNSAFETOP(chdir, 0, "`s -> `n. Change current working directory to `s."
          " Returns Unix error number or 0 for success.",
          1, (struct string *path), OP_LEAF | OP_NOESCAPE | OP_STR_READONLY,
          "s.n")
{
  if (chdir(path->str) < 0)
    return makeint(errno);
  return makeint(0);
}

#define PROC_GETINT(name, desc, see)                                    \
TYPEDOP(name, 0, "-> `n. Return the " desc " of the process."           \
        " See also " see ".",                                           \
        0, (void), OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".n")            \
{                                                                       \
  return makeint(name());                                               \
}

PROC_GETINT(getuid, "real user ID", "`geteuid() and `getgid()")
PROC_GETINT(geteuid, "effective user ID", "`getuid() and `getegid()")
PROC_GETINT(getgid, "real group ID", "`getuid() and `getegid()")
PROC_GETINT(getegid, "effective group ID", "`geteuid() and `getgid()")

UNSAFETOP(print_file_part, 0,
          "`oport `s `n0 `x -> `n1. Print `x bytes from file"
          " `s starting from byte `n0, or the rest of the file if"
          " `x is null.\n"
          "Output it sent to port `oport.\n"
          "Returns zero if successful, a Unix errno value"
          " if there was file I/O error, or -1 if `oport became too full.\n"
          "It is not an error to start after the end of the file, nor to"
          " try to read beyond the end of the file.",
	  4, (struct oport *p, struct string *name, value mstart,
              value mbytes),
          OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "osn[nu].n")
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

  p = get_oport(p);
  if (p == NULL)
    runtime_error(error_bad_type);

  FILE *f = fopen(name->str, "r");
  if (f == NULL)
    return makeint(errno);

  if (start != 0 && fseek(f, start, SEEK_SET) == -1)
    {
      int r = errno;
      fclose(f);
      return makeint(r);
    }

  GCPRO1(p);

  int result = 0;
  const size_t bufsize = 16 * 1024;
  char *buf = malloc(bufsize);
  while (bytes)
    {
      long toread = (bytes < 0 || bytes > bufsize
                     ? bufsize
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
          "`oport `s -> `n. Print the contents of file `s to output"
          " port `oport. Returns zero if successful, a Unix errno value"
          " if there was file I/O error, or -1 if `oport became too full."
          " Cf. `print_file_part().",
	  2, (struct oport *p, struct string *name),
	  OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "os.n")
{
  return code_print_file_part(p, name, makeint(0), NULL);
}

UNSAFETOP(file_read, 0,
          "`s1 -> `s2|`n. Reads and returns at most `MAX_STRING_SIZE"
          " characters from file `s1. Returns a Unix errno value on error.",
	  1, (struct string *name),
	  OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "s.S")
{
  TYPEIS(name, type_string);

  int fd = open(name->str, O_RDONLY);
  if (fd < 0)
    return makeint(errno);

  off_t size = lseek(fd, 0, SEEK_END);
  if (size < 0)
    goto got_error;

  if (lseek(fd, 0, SEEK_SET) < 0)
    goto got_error;

  if (size > MAX_STRING_SIZE)
    size = MAX_STRING_SIZE;

  struct string *s = alloc_empty_string(size);
  ssize_t n = read(fd, s->str, size);
  if (n < 0)
    goto got_error;

  close(fd);

  if (n != size)
    {
      GCPRO1(s);
      struct string *ns = alloc_empty_string(n);
      memcpy(ns->str, s->str, n);
      UNGCPRO();
      s = ns;
    }

  return s;

 got_error: ;
  int r = errno;
  close(fd);
  return makeint(r);
}

static bool write_block(void *data, struct string *mstr, size_t len)
{
  const char *str = mstr->str;
  int fd = (long)data;
  while (len > 0)
    {
      ssize_t n = write(fd, str, len);
      if (n < 0)
        {
          if (errno == EINTR)
            continue;
          return false;
        }
      len -= n;
      str += n;
    }
  return true;
}

static value file_write(struct string *file, value data, bool do_append)
{
  TYPEIS(file, type_string);
  if (!TYPE(data, type_string))
    check_string_port(data);

  int m = O_WRONLY | O_CREAT | (do_append ? O_APPEND : O_TRUNC);
  int fd = open(file->str, m, 0666);
  if (fd < 0)
    return makeint(errno);

  if (!(TYPE(data, type_string)
        ? write_block((void *)(long)fd, data,
                      string_len((struct string *)data))
        : port_for_blocks(data, write_block, (void *)(long)fd)))
    goto got_error;

  close(fd);
  return makeint(0);

 got_error: ;
  int r = errno;
  close(fd);
  return makeint(r);
}

UNSAFETOP(file_write, 0,
          "`s1 `s2|`p -> `n. Writes string `s2 (or contents of string"
          " oport `p) to file `s1.\n"
          "Creates the file if does not exist.\n"
          "Returns a Unix error number for failure or 0 for success.\n"
          "On failure, partial data may have been written.",
          2, (struct string *file, value data),
          OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "s[so].n")
{
  return file_write(file, data, false);
}

UNSAFETOP(file_append, 0,
          "`s1 `s2|`p -> `n. Appends string `s2 (or contents of string"
          " oport `p) to file `s1.\n"
          "Creates the file if does not exist.\n"
          "Returns a Unix error number for failure or 0 for success.\n"
          "On failure, partial data may have been written.",
          2, (struct string *file, value data),
          OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "s[so].n")
{
  return file_write(file, data, true);
}

#ifndef WIN32
SECTOP(passwd_file_entries, 0,
       "-> `l. Returns a list of [ `pw_name `pw_uid "
       "`pw_gid `pw_gecos `pw_dir `pw_shell ] from the contents of "
       "/etc/passwd. Cf. `getpwent(3)",
       0, (void),
       1,
       OP_LEAF | OP_NOESCAPE, ".l")
{
  struct list *res = NULL;
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

SECTOP(group_file_entries, 0,
       "-> `l. Returns a list of [ `gr_name `gr_gid "
       "( `gr_mem ... ) ] from the contents of /etc/group."
       " Cf. `getgrent(3)",
       0, (void),
       1,
       OP_LEAF | OP_NOESCAPE, ".l")
{
  struct list *res = NULL, *l = NULL;
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
  DEFINE(rmdir);
  DEFINE(directory_files);
  DEFINE(file_stat);
  DEFINE(file_utime);

  DEFINE(chdir);

#ifndef WIN32
  DEFINE(real_path);
  DEFINE(readlink);
  DEFINE(file_lstat);
  DEFINE(passwd_file_entries);
  DEFINE(group_file_entries);

#ifdef USE_GMP
  DEFINE(file_system_stat);
#endif  /* USE_GMP */

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
  DEFINE(directoryp);

  DEFINE(remove);
  DEFINE(rename);

  DEFINE(strerror);

  DEFINE(getcwd);
  DEFINE(getenv);

  DEFINE(getuid);
  DEFINE(geteuid);
  DEFINE(getgid);
  DEFINE(getegid);

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
