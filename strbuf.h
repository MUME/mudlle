#ifndef STRBUF_H
#define STRBUF_H

#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#include "mudlle.h"
#include "mvalgrind.h"

struct strbuf {
  /* invariants: size == 0 => used == 0 && buf == ""
                 size > 0  => used < size && buf == malloc(size)
                 buf[used] == 0 */
  size_t size, used;
  char *buf;                                 /* nul-terminated */
};

#define SBNULL { .buf = (char *)"" }

/* return number of characters in strbuf (excluding nul) */
static inline size_t sb_len(const struct strbuf *sb) { return sb->used; }
/* return pointer to the string in strbuf */
static inline const char *sb_str(const struct strbuf *sb) { return sb->buf; }
/* return mutable pointer to the string in strbuf; the terminating nul
   must not be modified */
static inline char *sb_mutable_str(struct strbuf *sb) { return sb->buf; }

/* make strbuf have room for at least 'need' characters (including nul) */
void sb_setminsize(struct strbuf *sb, size_t need);
/* make strbuf have room for exactly 'need' characters (including nul) */
void sb_setsize(struct strbuf *sb, size_t size);

/* set number of characters in strbuf to 'len' */
static inline void sb_setlen(struct strbuf *sb, size_t len)
{
  /* check for special case */
  if (len == 0 && sb->size == 0)
    return;
  assert(len < sb->size);
  if (len > sb->used)
    VALGRIND_MAKE_MEM_UNDEFINED(sb->buf + sb->used + 1, len - sb->used);
  else
    VALGRIND_MAKE_MEM_NOACCESS(sb->buf + len + 1, sb->used - len);
  sb->used = len;
  sb->buf[sb->used] = 0;
}

/* create new strbuf from memory */
static inline struct strbuf sb_initmem(const void *data, size_t len)
{
  struct strbuf sb = SBNULL;
  if (len > 0)
    {
      sb_setsize(&sb, len + 1);
      sb_setlen(&sb, len);
      memcpy(sb.buf, data, len);
      sb.buf[len] = 0;
    }
  return sb;
}

/* create new strbuf from string */
static inline struct strbuf sb_initstr(const char *str)
{
  return sb_initmem(str, strlen(str));
}

/* create new strbuf from formatted string */
struct strbuf sb_initf(const char *fmt, ...)
  FMT_PRINTF(1, 2);

/* make room for at least 'len' additional characters */
static inline void sb_makeroom(struct strbuf *sb, size_t len)
{
  /* note we need room for trailing nul as well */
  size_t need = sb->used + len;
  if (need >= sb->size)
    sb_setminsize(sb, need);
  VALGRIND_MAKE_MEM_UNDEFINED(sb->buf + sb->used + 1, len);
}

/* add memory */
static inline void sb_addmem(struct strbuf *sb, const void *data, size_t len)
{
  sb_makeroom(sb, len);
  memcpy(sb->buf + sb->used, data, len);
  sb->used += len;
  sb->buf[sb->used] = 0;
}

/* add string */
static inline void sb_addstr(struct strbuf *sb, const char *str)
{
  sb_addmem(sb, str, strlen(str));
}

/* add characters */
static inline void sb_addnc(struct strbuf *sb, int c, size_t n)
{
  sb_makeroom(sb, n);
  memset(sb->buf + sb->used, c, n);
  sb->used += n;
  sb->buf[sb->used] = 0;
}

/* add character */
static inline void sb_addc(struct strbuf *sb, int c)
{
  sb_makeroom(sb, 1);
  sb->buf[sb->used++] = c;
  sb->buf[sb->used] = 0;
}

/* initialize strbuf */
static inline void sb_init(struct strbuf *sb)
{
  *sb = (struct strbuf)SBNULL;
}

/* free strbuf; leaves it in initialized (empty) state */
static inline void sb_free(struct strbuf *sb)
{
  if (sb->size)
    {
      free(sb->buf);
      sb_init(sb);
    }
}

/* empty strbuf */
static inline void sb_empty(struct strbuf *sb)
{
  sb_setlen(sb, 0);
}

/* return malloced contents of strbuf, which is left initialized (empty) */
static inline char *sb_detach(struct strbuf *sb)
{
  if (sb->size == 0)
    return strdup("");
  char *result = sb->buf;
  sb_init(sb);
  return result;
}

/* add formatted string */
int sb_printf(struct strbuf *sb, const char *fmt, ...)
  FMT_PRINTF(2, 3);
int sb_vprintf(struct strbuf *sb, const char *fmt, va_list va)
  FMT_PRINTF(2, 0);

struct oport;
struct oport *make_strbuf_oport(struct strbuf *sb);
void strbuf_init(void);

#endif  /* STRBUF_H */
