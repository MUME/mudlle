#include <stdio.h>

#include "alloc.h"
#include "ports.h"
#include "strbuf.h"

void sb_setsize(strbuf_t *sb, size_t size)
{
  sb->buf = realloc(sb->size ? sb->buf : NULL, size);
  sb->size = size;
}

void sb_setminsize(strbuf_t *sb, size_t need)
{
  size_t nsize = sb->size ? sb->size : 32;
  while (need >= nsize)
    nsize *= 2;
  sb_setsize(sb, nsize);
}

int sb_vprintf(strbuf_t *sb, const char *fmt, va_list va)
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

int sb_printf(strbuf_t *sb, const char *fmt, ...)
{
  va_list va;
  va_start(va, fmt);
  int used = sb_vprintf(sb, fmt, va);
  va_end(va);
  return used;
}

strbuf_t sb_initf(const char *fmt, ...)
{
  strbuf_t sb = SBNULL;
  va_list va, va2;
  va_start(va, fmt);
  va_copy(va2, va);
  int need = vsnprintf(NULL, 0, fmt, va2);
  va_end(va2);
  sb_setsize(&sb, need + 1);
  int used = vsprintf(sb.buf, fmt, va);
  assert(need == used);
  sb.used += used;
  return sb;
}

struct strbuf_port {
  struct oport oport;
  value mstrbuf;                /* C pointer as mudlle integer */
};

static strbuf_t *get_port_strbuf(struct oport *p)
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

static void strbuf_port_putch(struct oport *p, int c, size_t n)
{
  strbuf_t *sb = get_port_strbuf(p);
  sb_addnc(sb, c, n);
}

static void strbuf_port_write(struct oport *p, const char *data, size_t nchars)
{
  strbuf_t *sb = get_port_strbuf(p);
  sb_addmem(sb, data, nchars);
}

static void strbuf_port_swrite(struct oport *p, struct string *s, size_t from,
                               size_t nchars)
{
  strbuf_t *sb = get_port_strbuf(p);
  sb_addmem(sb, s->str + from, nchars);
}

static void strbuf_port_stat(struct oport *p, struct oport_stat *buf)
{
  strbuf_t *sb = get_port_strbuf(p);
  *buf = (struct oport_stat){
    .type = oport_type_user,
    .size = sb_len(sb)
  };
}

static const struct oport_methods strbuf_port_methods = {
  .close  = strbuf_port_close,
  .putch  = strbuf_port_putch,
  .write  = strbuf_port_write,
  .swrite = strbuf_port_swrite,
  .flush  = strbuf_port_flush,
  .stat   = strbuf_port_stat,
};

static struct gtemp *mstrbuf_port_methods;

struct oport *make_strbuf_oport(strbuf_t *sb)
{
  struct strbuf_port *p
    = (struct strbuf_port *)allocate_record(type_outputport, 2);
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
