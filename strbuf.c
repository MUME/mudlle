#include <stdio.h>

#include "alloc.h"
#include "ports.h"
#include "strbuf.h"

void sb_setsize(struct strbuf *sb, size_t size)
{
  if (size == sb->size)
    return;
  if (size == 0)
    {
      sb_free(sb);
      return;
    }
  if (sb->used == 0 && sb->size > 0)
    {
      /* avoid copying old string */
      free(sb->buf);
      sb->buf = malloc(size);
      goto done;
    }
  if (sb->used >= size)
    sb->used = size - 1;
  char *nbuf = malloc(size);
  memcpy(nbuf, sb->buf, sb->used);
  if (sb->size > 0)
    free(sb->buf);
  sb->buf = nbuf;
 done:
  sb->buf[sb->used] = 0;
  sb->size = size;
  VALGRIND_MAKE_MEM_NOACCESS(sb->buf + sb->used + 1, size - sb->used - 1);
}

void sb_setminsize(struct strbuf *sb, size_t need)
{
  size_t nsize = sb->size ? sb->size : 32;
  while (need >= nsize)
    nsize *= 2;
  sb_setsize(sb, nsize);
}

int sb_vprintf(struct strbuf *sb, const char *fmt, va_list va)
{
  va_list va2;
  va_copy(va2, va);
  int need = vsnprintf(NULL, 0, fmt, va2);
  va_end(va2);
  sb_makeroom(sb, need);
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
  sb_setsize(&sb, need + 1);
  sb_makeroom(&sb, need);	/* for valgrind annotations */
  va_start(va, fmt);
  int used = vsprintf(sb.buf, fmt, va);
  va_end(va);
  assert(need == used);
  sb.used += used;
  return sb;
}

struct strbuf_port {
  struct oport oport;
  value mstrbuf;                /* C pointer as mudlle integer */
};

static struct strbuf *get_port_strbuf(struct oport *p)
{
  return (void *)(intval(((struct strbuf_port *)p)->mstrbuf) << 1);
}

static void strbuf_port_close(struct oport *p)
{
  struct strbuf_port *sp = (struct strbuf_port *)p;
  sp->mstrbuf = makeint(0);
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
  *buf = (struct oport_stat){
    .type = oport_type_user,
    .size = sb_len(sb)
  };
}

static const struct oport_methods strbuf_port_methods = {
  .close  = strbuf_port_close,
  .putnc  = strbuf_port_putnc,
  .write  = strbuf_port_write,
  .swrite = strbuf_port_swrite,
  .flush  = strbuf_port_flush,
  .stat   = strbuf_port_stat,
};

static struct gtemp *mstrbuf_port_methods;

struct oport *make_strbuf_oport(struct strbuf *sb)
{
  struct strbuf_port *p = (struct strbuf_port *)allocate_record(
    type_oport, grecord_fields(*p));
  p->oport.methods = mstrbuf_port_methods;
  p->mstrbuf = makeint((long)sb >> 1);
  return &p->oport;
}

void strbuf_init(void)
{
  mstrbuf_port_methods = allocate_temp(type_internal,
                                       (void *)&strbuf_port_methods);
  staticpro(&mstrbuf_port_methods);
}
