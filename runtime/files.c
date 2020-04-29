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

#include "../mudlle-config.h"

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <glob.h>
#include <grp.h>
#include <limits.h>
#include <pwd.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <utime.h>

#ifdef __linux__
 #include <mntent.h>
#elif defined __MACH__
 #include <fstab.h>
#endif

#include <sys/stat.h>
#include <sys/statvfs.h>
#include <sys/time.h>
#include <sys/types.h>

#include "../call.h"
#include "../compile.h"
#include "../context.h"
#include "../interpret.h"
#include "../mparser.h"
#include "../mudlle-macro.h"
#include "../ports.h"
#include "../tree.h"
#include "../utils.h"

#include "bigint.h"
#include "check-types.h"
#include "files.h"
#include "io.h"
#include "mudlle-float.h"
#include "prims.h"

#ifdef ARG_MAX
#endif

TYPEDOP(load, , "`s -> `b. Loads file `s. Returns true if successful",
        (struct string *name), OP_STR_READONLY | OP_TRACE, "s.n")
{
  CHECK_TYPES(name, string);
  char *path;
  ALLOCA_PATH(path, name);
  struct filename fname = { .path = path, .nice = path };
  return makebool(load_file(path, &fname, 1, true));
}


UNSAFEOP(mkdir, , "`s `n1 -> `n2. Make directory `s (mode `n1)."
         " Returns Unix errno.",
         (struct string *name, value mode),
         OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_STR_READONLY, "sn.n")
{
  CHECK_TYPES(name, string,
              mode, integer);
  return makeint(mkdir(name->str, intval(mode)));
}

UNSAFEOP(rmdir, , "`s -> `n. Remove directory `s. Returns Unix errno.",
         (struct string *name),
         OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_STR_READONLY, "s.n")
{
  CHECK_TYPES(name, string);
  return makeint(rmdir(name->str));
}

UNSAFEOP(directory_files, , "`s -> `l. List all files of directory `s"
         " (returns false if problems)",
         (struct string *dir),
         OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "s.[ln]")
{
  CHECK_TYPES(dir, string);

  DIR *d = opendir(dir->str);
  if (d == NULL)
    return makebool(false);

  struct list *files = NULL;
  GCPRO(files);
  for (struct dirent *entry; (entry = readdir(d)); )
    {
      struct string *fname = alloc_string(entry->d_name);
      files = alloc_list(fname, files);
    }
  UNGCPRO();
  closedir(d);

  return files;
}

static const long allowed_glob_flags =
  0
#ifdef GLOB_TILDE
 #define GD_TILDE     "  `GLOB_TILDE    \tCarry out tilde expansion for"     \
                      " home directories.\n"
  | GLOB_TILDE
#else
 #define GD_TILDE ""
#endif
#ifdef GLOB_BRACE
 #define GD_BRACE     "  `GLOB_BRACE    \tExpand {a,b} style brace"          \
                      " expressions.\n"
  | GLOB_BRACE
#else
 #define GD_BRACE ""
#endif
#ifdef GLOB_MARK
 #define GD_MARK      "  `GLOB_MARK     \tAppend a slash to each path"       \
                      " corresponding to a directory.\n"
  | GLOB_MARK
#else
 #define GD_MARK ""
#endif
#ifdef GLOB_NOCHECK
 #define GD_NOCHECK   "  `GLOB_NOCHECK  \tIf no pattern matches, return the" \
                      " original pattern.\n"
  | GLOB_NOCHECK
#else
 #define GD_NOCHECK ""
#endif
#ifdef GLOB_NOESCAPE
 #define GD_NOESCAPE  "  `GLOB_NOESCAPE \tDo not allow backslash to be"      \
                      " used as an escape character.\n"
  | GLOB_NOESCAPE
#else
 #define GD_NOESCAPE ""
#endif
#ifdef GLOB_PERIOD
 #define GD_PERIOD    "  `GLOB_PERIOD   \tAllow a leading period to be"      \
                      " matched by metacharacters.\n"
  | GLOB_PERIOD
#else
 #define GD_PERIOD ""
#endif
#ifdef GLOB_NOMAGIC
 #define GD_NOMAGIC   "  `GLOB_NOMAGIC  \tIf the pattern contains no"        \
                      " metacharacters, return it as the only match,"        \
                      " even if there is no such file.\n"
  | GLOB_NOMAGIC
#else
 #define GD_NOMAGIC ""
#endif
#ifdef GLOB_ONLYDIR
 #define GD_ONLYDIR   "  `GLOB_ONLYDIR  \tHint only to return matching"      \
                      " directories. The implementation may ignore this"     \
                      " flag.\n"
  | GLOB_ONLYDIR
#else
 #define GD_ONLYDIR ""
#endif
  | 0;

UNSAFEOP(glob_files, ,
         "`s0 `s1 `n -> `l. Returns a list of all files matched by"
         " the glob pattern `s1, executed in directory `s0. Returns false on"
         " error. `n specifies flags to use:\n"
         GD_TILDE
         GD_BRACE
         GD_MARK
         GD_NOCHECK
         GD_NOESCAPE
         GD_PERIOD
         GD_NOMAGIC
         GD_ONLYDIR,
         (struct string *dir, struct string *pat, value n),
         OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "ssn.[ln]")
{
  long flags;
  CHECK_TYPES(pat, string,
              dir, string,
              n,   CT_INT(flags));
  if (flags & ~allowed_glob_flags)
    RUNTIME_ERROR(error_bad_value, "invalid flags");

  int orig_wd;
  if ((orig_wd = open(".", 0)) < 0)
    RUNTIME_ERROR(error_abort, "cannot find current working directory");

  if (chdir(dir->str) < 0)
    {
      close(orig_wd);
      RUNTIME_ERROR(error_bad_value, "failed to change working directory");
    }

  glob_t files;
  int res = glob(pat->str, flags, NULL, &files);
  if (fchdir(orig_wd) < 0)
    {
      close(orig_wd);
      globfree(&files);
      RUNTIME_ERROR(error_bad_value, "failed to restore working directory");
    }

  close(orig_wd);

  if (res)
    {
      globfree(&files);
      return makebool(false);
    }

  struct list *l = NULL;
  GCPRO(l);
  for (char **s = files.gl_pathv; *s; ++s)
    {
      struct string *f = alloc_string(*s);
      l = alloc_list(f, l);
    }
  UNGCPRO();
  globfree(&files);
  return l;
}

static value make_timespec(const struct timespec *ts)
{
  return alloc_float(ts->tv_sec + ts->tv_nsec * 1e-9);
}

#ifdef __MACH__
#define st_atim st_atimespec
#define st_mtim st_mtimespec
#define st_ctim st_ctimespec
#endif

static value build_file_stat(struct stat *sb)
{
  struct vector *info = alloc_vector(FILE_STAT_FIELDS);
  GCPRO(info);

#define SETVSTAT(FIELD, field) \
  SET_VECTOR(info, FS_ ## FIELD, make_int_or_bigint(sb->st_ ## field))

  SETVSTAT(DEV,   dev);
  SETVSTAT(INO,   ino);
  SETVSTAT(MODE,  mode);
  SETVSTAT(NLINK, nlink);
  SETVSTAT(UID,   uid);
  SETVSTAT(GID,   gid);
  SETVSTAT(RDEV,  rdev);
  SETVSTAT(SIZE,  size);

  /* treat time as the time() primitive does: no bigints */
  info->data[FS_ATIME] = makeint(sb->st_atime);
  info->data[FS_MTIME] = makeint(sb->st_mtime);
  info->data[FS_CTIME] = makeint(sb->st_ctime);

  SETVSTAT(BLKSIZE, blksize);
  SETVSTAT(BLOCKS,  blocks);

#undef SETVSTAT

  SET_VECTOR(info, FS_AFTIME, make_timespec(&sb->st_atim));
  SET_VECTOR(info, FS_MFTIME, make_timespec(&sb->st_mtim));
  SET_VECTOR(info, FS_CFTIME, make_timespec(&sb->st_ctim));

  UNGCPRO();

  return info;
}

UNSAFEOP(file_stat, , "`s -> `v. Returns status of file `s, or false for"
         " failure. See the `FS_xxx constants and `file_lstat().",
         (struct string *fname),
         OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "s.[vz]")
{
  CHECK_TYPES(fname, string);

  struct stat sb;
  if (!stat(fname->str, &sb))
    return build_file_stat(&sb);
  else
    return makebool(false);
}

#ifdef USE_GMP
TYPEDOP(file_system_stat, , "`s -> `v. Returns file system statistics"
        " for the file system containing file `s, or Unix errno for error.\n"
        "Note that all entries may contain bigints if the result does"
        " not fit in an integer.\n"
        "See the `FSYS_xxx and `FILE_SYSTEM_STAT_FIELDS constants.\n"
        "Cf. `mount_points().",
        (struct string *fname),
        OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "s.[vn]")
{
  CHECK_TYPES(fname, string);

  struct statvfs sb;
  if (statvfs(fname->str, &sb) < 0)
    return makeint(errno);

  struct vector *v = alloc_vector(FILE_SYSTEM_STAT_FIELDS);
  GCPRO(v);

#define SETVFS(FIELD, field) \
  SET_VECTOR(v, FSYS_ ## FIELD, make_int_or_bigint(sb.f_ ## field))

  SETVFS(BSIZE,   bsize);
  SETVFS(FRSIZE,  frsize);
  SETVFS(BLOCKS,  blocks);
  SETVFS(BFREE,   bfree);
  SETVFS(BAVAIL,  bavail);
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

UNSAFEOP(real_path, ,
         "`s0 -> `s1. Resolves any symbolic links, follows references to"
         " /./ and /../, and returns the canonicalized path for `s0."
         " Returns `errno on error.",
         (struct string *path),
         OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "s.[sn]")
{
  CHECK_TYPES(path, string);
  char buf[PATH_MAX], *res;
  if ((res = realpath(path->str, buf)) == NULL)
    return makeint(errno);
  return alloc_string(res);
}

UNSAFEOP(file_lstat, ,
         "`s -> `v. Returns status of file `s (not following links)."
         " Returns FALSE for failure. See the `FS_xxx constants and"
         " `file_stat()",
         (struct string *fname),
         OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "s.[vz]")
{
  CHECK_TYPES(fname, string);
  struct stat sb;
  if (!lstat(fname->str, &sb))
    return build_file_stat(&sb);
  else
    return makebool(false);
}

UNSAFEOP(file_utime, ,
         "`s `x -> `n. Sets the access and modification times"
         " of file `s. If `x is `null, set both to the current time;"
         " if `x is a pair, set them to `car(`x) and `cdr(`x), respectively."
         " Returns zero on success or `errno on failure.",
         (struct string *fname, struct list *mtimes),
         OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "sl.n")
{
  CHECK_TYPES(fname, string,
              mtimes, CT_TYPESET(TYPESET_LIST));

  struct utimbuf buf, *times;
  if (mtimes == NULL)
    times = NULL;
  else
    {
      assert(TYPE(mtimes, pair));
      buf.actime = GETUINT(mtimes->car);
      buf.modtime = GETUINT(mtimes->cdr);
      times = &buf;
    }

  if (utime(fname->str, times) < 0)
    return makeint(errno);
  return makeint(0);
}

UNSAFEOP(readlink, ,
         "`s1 -> `s2. Returns the contents of symlink `s, or FALSE"
         " for failure", (struct string *lname),
         OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "s.[sz]")
{
  CHECK_TYPES(lname, string);

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

UNSAFEOP(file_regularp, "file_regular?",
         "`s -> `b. Returns TRUE if `s is a regular file.",
         (struct string *fname),
         OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_STR_READONLY, "s.n")
{
  CHECK_TYPES(fname, string);
  struct stat sb;
  return makebool(stat(fname->str, &sb) == 0 && S_ISREG(sb.st_mode));
}

UNSAFEOP(directoryp, "directory?",
         "`s -> `b. Returns TRUE if `s is a directory.",
         (struct string *dname),
         OP_LEAF | OP_NOALLOC | OP_NOESCAPE | OP_STR_READONLY, "s.n")
{
  CHECK_TYPES(dname, string);
  struct stat sb;
  return makebool(stat(dname->str, &sb) == 0 && S_ISDIR(sb.st_mode));
}

UNSAFEOP(remove, , "`s -> `n. Removes file `s. Returns the Unix error"
         " number or 0 for success.",
         (struct string *fname),
         OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "s.n")
{
  CHECK_TYPES(fname, string);
  return makeint(unlink(fname->str) ? errno : 0);
}

UNSAFEOP(rename, ,
         "`s1 `s2 -> `n. Renames file `s1 to `s2. Returns the Unix error"
         " number or 0 for success",
         (struct string *oldname, struct string *newname),
         OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "ss.n")
{
  CHECK_TYPES(oldname, string,
              newname, string);
  return makeint(rename(oldname->str, newname->str) ? errno : 0);
}

UNSAFEOP(chown, , "`s1 `n0 `n1 -> `n2. Changed owner of file `s1 to uid `n0"
         " and gid `n1. Use -1 not to change that field."
         " Returns errno or 0 for success.",
         (struct string *fname, value uid, value gid),
         OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "snn.n")
{
  CHECK_TYPES(fname, string,
              uid,   integer,
              gid,   integer);
  return makeint(chown(fname->str, intval(uid), intval(gid)) ? errno : 0);
}

UNSAFEOP(chmod, , "`s1 `n0 -> `n1. Changed mode of file `s1 to `n0."
         "  Returns errno or 0 for success.",
         (struct string *fname, value mode),
         OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "sn.n")
{
  CHECK_TYPES(fname, string,
              mode,  integer);
  return makeint(chmod(fname->str, intval(mode)) ? errno : 0);
}

TYPEDOP(strerror, , "`n -> `s. Returns an error string corresponding to"
        " errno `n, or `false if there is no such errno.",
        (value merrno), OP_LEAF | OP_NOESCAPE, "n.[sz]")
{
  CHECK_TYPES(merrno, integer);
  const char *s = strerror(intval(merrno));
  return s ? alloc_string(s) : makebool(false);
}

TYPEDOP(getenv, , "`s0 -> `s1|false. Return the value of the environment"
        " variable `s0, or false if no match.",
        (struct string *s), OP_LEAF | OP_NOESCAPE | OP_STR_READONLY,
        "s.[sz]")
{
  CHECK_TYPES(s, string);
  char *r = getenv(s->str);
  return r == NULL ? makebool(false) : alloc_string(r);
}

TYPEDOP(getcwd, , "-> `s. Returns the name of the current working directory,"
        " or a Unix errno value on error. Cf. `strerror",
        (void), OP_LEAF | OP_NOESCAPE, ".[sn]")
{
  char buf[PATH_MAX];
  char *wd = getcwd(buf, sizeof buf);
  return wd == NULL ? makeint(errno) : alloc_string(wd);
}

UNSAFEOP(chdir, , "`s -> `n. Change current working directory to `s."
         " Returns Unix error number or 0 for success.",
         (struct string *path), OP_LEAF | OP_NOESCAPE | OP_STR_READONLY,
         "s.n")
{
  CHECK_TYPES(path, string);
  if (chdir(path->str) < 0)
    return makeint(errno);
  return makeint(0);
}

#define PROC_GETINT(name, desc, see)                            \
TYPEDOP(name, , "-> `n. Return the " desc " of the process."    \
        " See also " see ".",                                   \
        (void), OP_LEAF | OP_NOALLOC | OP_NOESCAPE, ".n")       \
{                                                               \
  return makeint(name());                                       \
}

PROC_GETINT(getuid, "real user ID", "`geteuid() and `getgid()")
PROC_GETINT(geteuid, "effective user ID", "`getuid() and `getegid()")
PROC_GETINT(getgid, "real group ID", "`getuid() and `getegid()")
PROC_GETINT(getegid, "effective group ID", "`geteuid() and `getgid()")

UNSAFEOP(print_file_part, ,
         "`oport `s `n0 `x -> `n1. Print `x bytes from file"
         " `s starting from byte `n0, or the rest of the file if"
         " `x is null.\n"
         "Output it sent to port `oport.\n"
         "Returns zero if successful, a Unix errno value"
         " if there was file I/O error, or -1 if `oport became too full.\n"
         "It is not an error to start after the end of the file, nor to"
         " try to read beyond the end of the file.",
         (struct oport *p, struct string *name, value mstart,
          value mbytes),
         OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "osn[nu].n")
{
  long start, bytes = -1;
  {
    GCPRO(name);                /* CT_OPORT may cause GC */
    CHECK_TYPES(p,      CT_OPORT,
                name,   string,
                mstart, CT_RANGE(start, 0, LONG_MAX),
                mbytes, OR(null, CT_RANGE(bytes, 0, LONG_MAX)));
    UNGCPRO();
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

  GCPRO(p);

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
      if (obuf.size >= (is_file_port(p)
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

UNSAFEOP(print_file, ,
         "`oport `s -> `n. Print the contents of file `s to output"
         " port `oport. Returns zero if successful, a Unix errno value"
         " if there was file I/O error, or -1 if `oport became too full."
         " Cf. `print_file_part().",
         (struct oport *p, struct string *name),
         OP_LEAF | OP_NOESCAPE | OP_STR_READONLY, "os.n")
{
  return code_print_file_part(p, name, makeint(0), NULL);
}

UNSAFEOP(file_read, ,
         "`s1 -> `s2|`n. Reads and returns at most `MAX_STRING_SIZE"
         " characters from file `s1. Returns a Unix errno value on error.",
         (struct string *name),
         OP_LEAF | OP_NOESCAPE | OP_STR_READONLY | OP_NUL_STR, "s.[sn]")
{
  CHECK_TYPES(name, string);

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
      GCPRO(s);
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
  int fd = ptr_int(data);
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

static value file_write(struct string *file, value data, bool do_append,
                        const struct prim_op *op)
{
  CHECK_TYPES_OP(op,
                 file, string,
                 data, OR(string, CT_STR_OPORT));

  int m = O_WRONLY | O_CREAT | (do_append ? O_APPEND : O_TRUNC);
  int fd = open(file->str, m, 0666);
  if (fd < 0)
    return makeint(errno);

  if (!(TYPE(data, string)
        ? write_block(int_ptr(fd), data, string_len((struct string *)data))
        : port_for_blocks(data, write_block, int_ptr(fd))))
    goto got_error;

  close(fd);
  return makeint(0);

 got_error: ;
  int r = errno;
  close(fd);
  return makeint(r);
}

UNSAFEOP(file_write, ,
         "`s1 `s2|`p -> `n. Writes string `s2 (or contents of string"
         " oport `p) to file `s1.\n"
         "Creates the file if does not exist.\n"
         "Returns a Unix error number for failure or 0 for success.\n"
         "On failure, partial data may have been written.",
         (struct string *file, value data),
         OP_LEAF | OP_NOESCAPE | OP_STR_READONLY | OP_NUL_STR, "s[so].n")
{
  return file_write(file, data, false, THIS_OP);
}

UNSAFEOP(file_append, ,
         "`s1 `s2|`p -> `n. Appends string `s2 (or contents of string"
         " oport `p) to file `s1.\n"
         "Creates the file if does not exist.\n"
         "Returns a Unix error number for failure or 0 for success.\n"
         "On failure, partial data may have been written.",
         (struct string *file, value data),
         OP_LEAF | OP_NOESCAPE | OP_STR_READONLY | OP_NUL_STR, "s[so].n")
{
  return file_write(file, data, true, THIS_OP);
}

SECOP(passwd_file_entries, ,
      "-> `l. Returns a list of [ `pw_name `pw_uid"
      " `pw_gid `pw_gecos `pw_dir `pw_shell ] from the contents of"
      " /etc/passwd. Cf. `getpwent(3)",
      (void), LVL_VALA, OP_LEAF | OP_NOESCAPE, ".l")
{
  struct list *res = NULL;
  struct vector *v = NULL;

  GCPRO(res, v);

  setpwent();
  for (;;)
    {
      errno = 0;
      struct passwd *pw = getpwent();
      if (pw == NULL)
        {
          if (errno == EINTR)
            continue;
          break;
	}

      v = alloc_vector(PASSWD_ENTRY_FIELDS);
      SET_VECTOR(v, PW_NAME,  alloc_string(pw->pw_name));
      SET_VECTOR(v, PW_UID,   makeint(pw->pw_uid));
      SET_VECTOR(v, PW_GID,   makeint(pw->pw_gid));
      SET_VECTOR(v, PW_GECOS, alloc_string(pw->pw_gecos));
      SET_VECTOR(v, PW_DIR,   alloc_string(pw->pw_dir));
      SET_VECTOR(v, PW_SHELL, alloc_string(pw->pw_shell));

      res = alloc_list(v, res);
    }

  int en = errno;
  endpwent();
  if (en)
    runtime_error(error_bad_value);

  UNGCPRO();

  return res;
}

SECOP(group_file_entries, ,
      "-> `l. Returns a list of [ `gr_name `gr_gid ( `gr_mem ... ) ] from"
      " the contents of /etc/group. Cf. `getgrent(3)",
      (void), LVL_VALA, OP_LEAF | OP_NOESCAPE, ".l")
{
  struct list *res = NULL, *l = NULL;
  struct group *grp;
  struct vector *v = NULL;

  GCPRO(res, v, l);

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

#if defined __linux__ || defined __MACH__
SECOP(mount_points, ,
      "-> `l. Returns a list of mount points, where each mount point"
      " is a vector indexed by `MNT_xxx:\n"
      "  `MNT_FSNAME   \tname fo the filesystem\n"
      "  `MNT_DIR      \tfilesystem path prefix\n"
      "  `MNT_TYPE     \tmount type\n"
      "  `MNT_OPTS     \tmount options\n"
      "  `MNT_FREQ     \tdump frequency in days\n"
      "  `MNT_PASSNO   \tpass number for parallel fsck\n"
      "Cf. `file_system_stat().",
      (void), LVL_VALA, OP_LEAF | OP_NOESCAPE, ".l")
{
#ifdef __linux__
  FILE *f = setmntent("/etc/mtab", "r");
  if (f == NULL)
    runtime_error_message(error_abort, "failed to open /etc/mtab");
 #define FSF(l, m) ent->mnt_ ## l
 #define FOR_FSENT() for (struct mntent *ent; (ent = getmntent(f));)
 #define END_FSENT()  endmntent(f)
#elif defined __MACH__
  if (!setfsent())
    runtime_error_message(error_abort, "setfsent() failed");
 #define FSF(l, m) ent->fs_ ## m
 #define FOR_FSENT() for (struct fstab *ent; (ent = getfsent());)
 #define END_FSENT() endfsent()
#else
  #error Unsupported platform
#endif
  struct list *res = NULL;
  struct vector *v = NULL;
  GCPRO(res, v);
  FOR_FSENT()
    {
      v = alloc_vector(MOUNT_ENTRY_FIELDS);
      SET_VECTOR(v, MNT_FSNAME, alloc_string(FSF(fsname, spec)));
      SET_VECTOR(v, MNT_DIR,    alloc_string(FSF(dir,    file)));
      SET_VECTOR(v, MNT_TYPE,   alloc_string(FSF(type,   type)));
      SET_VECTOR(v, MNT_OPTS,   alloc_string(FSF(opts,   mntops)));
#ifdef USE_GMP
      SET_VECTOR(v, MNT_FREQ,   make_int_or_bigint(FSF(freq,   freq)));
      SET_VECTOR(v, MNT_PASSNO, make_int_or_bigint(FSF(passno, passno)));
#endif
      res = alloc_list(v, res);
    }
  UNGCPRO();
  END_FSENT();
  return res;
}
#endif	/* __linux__ || defined __MACH__ */

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

  DEFINE(real_path);
  DEFINE(readlink);
  DEFINE(file_lstat);
  DEFINE(passwd_file_entries);
  DEFINE(group_file_entries);

#if defined __linux__ || defined __MACH__
  DEFINE(mount_points);
#endif
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
}
