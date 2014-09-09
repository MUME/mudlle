#ifndef STRBUF_H
#define STRBUF_H

#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#include "mudlle.h"

typedef struct strbuf {
  /* invariants: size == 0 => used == 0 && buf == ""
                 size > 0  => used < size && buf == malloc(size)
                 buf[used] == 0 */
  size_t size, used;
  char *buf;                                 /* nul-terminated */
} strbuf_t;

#define SBNULL { .buf = (char *)"" }

/* return number of characters in strbuf (excluding nul) */
static inline size_t sb_len(const strbuf_t *sb) { return sb->used; }
/* return pointer to the string in strbuf */
static inline const char *sb_str(const strbuf_t *sb) { return sb->buf; }
/* return mutable pointer to the string in strbuf; the terminating nul
   must not be modified */
static inline char *sb_mutable_str(strbuf_t *sb) { return sb->buf; }

/* make strbuf have room for at least 'need' characters (including nul) */
void sb_setminsize(strbuf_t *sb, size_t need);
/* make strbuf have room for exactly 'need' characters (including nul) */
void sb_setsize(strbuf_t *sb, size_t size);

/* set number of characters in strbuf to 'len' */
static inline void sb_setlen(strbuf_t *sb, size_t len)
{
  /* check for special case */
  if (len == 0 && sb->size == 0)
    return;
  assert(len < sb->size);
  sb->used = len;
  sb->buf[sb->used] = 0;
}

/* create new strbuf from memory */
static inline strbuf_t sb_initmem(const void *data, size_t len)
{
  strbuf_t sb = SBNULL;
  if (len > 0) {
    sb_setsize(&sb, len + 1);
    sb_setlen(&sb, len);
    memcpy(sb.buf, data, len);
    sb.buf[len] = 0;
  }
  return sb;
}

/* create new strbuf from string */
static inline strbuf_t sb_initstr(const char *str)
{
  return sb_initmem(str, strlen(str));
}

/* create new strbuf from formatted string */
strbuf_t sb_initf(const char *fmt, ...)
  FMT_PRINTF(1, 2);

/* make room for at least 'len' additional characters */
static inline void sb_makeroom(strbuf_t *sb, size_t len)
{
  /* note we need room for trailing nul as well */
  size_t need = sb->used + len;
  if (need < sb->size)
    return;

  sb_setminsize(sb, need);
}

/* add memory */
static inline void sb_addmem(strbuf_t *sb, const void *data, size_t len)
{
  sb_makeroom(sb, len);
  memcpy(sb->buf + sb->used, data, len);
  sb->used += len;
  sb->buf[sb->used] = 0;
}

/* add string */
static inline void sb_addstr(strbuf_t *sb, const char *str)
{
  sb_addmem(sb, str, strlen(str));
}

/* add characters */
static inline void sb_addnc(strbuf_t *sb, int c, size_t n)
{
  sb_makeroom(sb, n);
  memset(sb->buf + sb->used, c, n);
  sb->used += n;
  sb->buf[sb->used] = 0;
}

/* add character */
static inline void sb_addc(strbuf_t *sb, int c)
{
  sb_makeroom(sb, 1);
  sb->buf[sb->used++] = c;
  sb->buf[sb->used] = 0;
}

/* initialize strbuf */
static inline void sb_init(strbuf_t *sb)
{
  *sb = (strbuf_t)SBNULL;
}

/* free strbuf; leaves it in initialized (empty) state */
static inline void sb_free(strbuf_t *sb)
{
  if (sb->size)
    {
      free(sb->buf);
      sb_init(sb);
    }
}

/* empty strbuf */
static inline void sb_empty(strbuf_t *sb)
{
  sb_setlen(sb, 0);
}

/* return malloced contents of strbuf, which is left initialized (empty) */
static inline char *sb_detach(strbuf_t *sb)
{
  char *result = sb->size ? sb->buf : strdup("");
  sb_init(sb);
  return result;
}

/* add formatted string */
int sb_printf(strbuf_t *sb, const char *fmt, ...)
  FMT_PRINTF(2, 3);
int sb_vprintf(strbuf_t *sb, const char *fmt, va_list va)
  FMT_PRINTF(2, 0);

struct oport;
struct oport *make_strbuf_oport(strbuf_t *sb);
void strbuf_init(void);

#endif  /* STRBUF_H */
