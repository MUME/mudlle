#include <stdio.h>

#include "alloc.h"
#include "ports.h"
#include "strbuf.h"

static void sb_setsize_nonzero(struct strbuf *sb, size_t size)
{
  if (size == sb->size)
    return;
  assert(size > 0);
  if (sb->used == 0)
    {
      /* avoid copying old string */
      if (sb->size > 0)
        free(sb->buf);
      sb->buf = malloc(size);
    }
  else if (sb->used >= size)
    {
      sb->used = size - 1;
      sb->buf = realloc(sb->buf, size);
    }
  else
    {
      char *nbuf = malloc(size);
      memcpy(nbuf, sb->buf, sb->used);
      if (sb->size > 0)
        free(sb->buf);
      sb->buf = nbuf;
    }
  sb->buf[sb->used] = 0;
  sb->size = size;
  VALGRIND_MAKE_MEM_NOACCESS(sb->buf + sb->used + 1, size - sb->used - 1);
}

void sb_setsize(struct strbuf *sb, size_t size)
{
  if (size == 0)
    sb_free(sb);
  else
    sb_setsize_nonzero(sb, size);
}

void sb_trim(struct strbuf *sb)
{
  size_t len = sb->used;
  if (len == 0)
    sb_free(sb);
  else
    sb_setsize_nonzero(sb, len + 1);
}

void sb_setminsize(struct strbuf *sb, size_t need)
{
  size_t nsize = sb->size ? sb->size : 32;
  while (need >= nsize)
    nsize *= 2;
  sb_setsize_nonzero(sb, nsize);
}

int sb_vprintf(struct strbuf *sb, const char *fmt, va_list va)
{
  bool is_self = fmt == sb->buf;
  if (is_self)
    {
      /* special-case using self as format buffer; must end in
         explicit null */
      size_t l = sb_len(sb);
      assert(l > 0 && sb->buf[l - 1] == 0);
    }

  va_list va2;
  va_copy(va2, va);
  int need = vsnprintf(NULL, 0, fmt, va2);
  va_end(va2);
  sb_makeroom(sb, need);
  if (is_self)
    fmt = sb->buf;
  int used = vsprintf(sb->buf + sb_len(sb), fmt, va);
  sb->used += used;
  assert(need == used);
  return used;
}

int sb_printf(struct strbuf *sb, const char *fmt, ...)
{
  va_list va;
  va_start(va, fmt);
  int used = sb_vprintf(sb, fmt, va);
  va_end(va);
  return used;
}

struct strbuf sb_initf(const char *fmt, ...)
{
  va_list va;
  va_start(va, fmt);
  int need = vsnprintf(NULL, 0, fmt, va);
  va_end(va);

  struct strbuf sb = SBNULL;
  sb_setsize_nonzero(&sb, need + 1);
  sb_setlen(&sb, need);

  va_start(va, fmt);
  int used = vsprintf(sb.buf, fmt, va);
  va_end(va);
  assert(need == used);

  return sb;
}

const char base64chars[] = ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdef"
                            "ghijklmnopqrstuvwxyz0123456789+/");

void sb_add_base64(struct strbuf *sb, const void *data, size_t len, bool pad)
{
  if (pad)
    sb_makeroom(sb, ((len + 2) / 3) * 4);
  else
    sb_makeroom(sb, (len * 4 + 2) / 3);

  const unsigned char *chars = data;
  enum { s0, s1, s2 } state = s0;
  unsigned char prev = 0;
  while (len > 0)
    {
      unsigned char c, this = *chars++;
      --len;
      switch (state)
        {
        case s0:                 /*   | _6_ | (2) */
          c = this >> 2;
          ++state;
          break;
        case s1:                 /* 2 | _4_ | (4) */
          c = ((prev & 3) << 4) | (this >> 4);
          ++state;
          break;
        case s2:                 /* 4 | _2_ | (6) */
          c = ((prev & 0xf) << 2) | (this >> 6);
          sb_addc(sb, base64chars[c]);
          c = this & 0x3f;
          state = s0;
          break;
        default:
          abort();
        }
      sb_addc(sb, base64chars[c]);
      prev = this;
    }
  unsigned char c, npad;
  switch (state)
    {
    case s0:                 /*   | _6_ | (2) */
      return;
    case s1:                 /* 2 | _4_ | (4) */
      c = ((prev & 3) << 4);
      npad = 2;
      break;
    case s2:                 /* 4 | _2_ | (6) */
      c = ((prev & 0xf) << 2);
      npad = 1;
      break;
    default:
      abort();
    }
  sb_addc(sb, base64chars[c]);
  if (pad)
    sb_addnc(sb, '=', npad);
}

struct strbuf_port {
  struct oport oport;
  struct tagged_ptr sb;
};

static struct strbuf *get_port_strbuf(struct oport *p)
{
  struct strbuf_port *sp = (struct strbuf_port *)p;
  return get_tagged_ptr(&sp->sb);
}

static void strbuf_port_close(struct oport *p)
{
  struct strbuf_port *sp = (struct strbuf_port *)p;
  set_tagged_ptr(&sp->sb, NULL);
}

static void strbuf_port_flush(struct oport *p)
{
}

static void strbuf_port_putnc(struct oport *p, int c, size_t n)
{
  struct strbuf *sb = get_port_strbuf(p);
  sb_addnc(sb, c, n);
}

static void strbuf_port_write(struct oport *p, const char *data, size_t nchars)
{
  struct strbuf *sb = get_port_strbuf(p);
  sb_addmem(sb, data, nchars);
}

static void strbuf_port_swrite(struct oport *p, struct string *s, size_t from,
                               size_t nchars)
{
  struct strbuf *sb = get_port_strbuf(p);
  sb_addmem(sb, s->str + from, nchars);
}

static void strbuf_port_stat(struct oport *p, struct oport_stat *buf)
{
  struct strbuf *sb = get_port_strbuf(p);
  *buf = (struct oport_stat){ .size = sb_len(sb) };
}

static const struct oport_methods strbuf_port_methods = {
  .name   = "strbuf",
  .close  = strbuf_port_close,
  .putnc  = strbuf_port_putnc,
  .write  = strbuf_port_write,
  .swrite = strbuf_port_swrite,
  .flush  = strbuf_port_flush,
  .stat   = strbuf_port_stat,
};

struct oport *make_strbuf_oport(struct strbuf *sb)
{
  struct strbuf_port *p = (struct strbuf_port *)alloc_oport(
    grecord_fields(*p), &strbuf_port_methods);
  set_tagged_ptr(&p->sb, sb);
  return &p->oport;
}
