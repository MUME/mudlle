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

#include <ctype.h>
#include <errno.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

#include "alloc.h"
#ifdef ALLOC_STATS
#include "context.h"
#endif
#include "error.h"
#include "mvalues.h"
#include "types.h"


const char *const mudlle_type_names[] = {
  "code",   "closure", "variable", "internal",  "primitive", "varargs",
  "secure", "integer", "string",   "vector",    "pair",      "symbol",
  "table",  "private", "object",   "character", "gone",      "oport",
  "mcode",  "float",   "bigint",   "reference", "null",
  "none",   "any",     "function", "list"
};
CASSERT_VLEN(mudlle_type_names, last_synthetic_type);

#ifdef ALLOC_STATS

struct file_line
{
  struct file_line *next;
  char *file;
  int line;
  unsigned key;
  int count;
  int size;
};

struct file_line_stats
{
  int size, used;
  struct file_line **slots;
};

static struct file_line_stats type_alloc_stats[last_type];

static unsigned str_int_hash(const char *str, int line)
{
  unsigned key = line;
  while (*str)
    {
      key = (key * 31) ^ *(unsigned char *)str;
      ++str;
    }
  return key;
}

struct vector *get_alloc_stats(void)
{
  int entries = 0;
  for (enum mudlle_type t = 0; t < last_type; ++t)
    entries += type_alloc_stats[t].used;

  struct vector *res = alloc_vector(entries);
  struct vector *vec = NULL;

  int used = 0;

  GCPRO2(res, vec);
  for (enum mudlle_type t = 0; t < last_type; ++t)
    for (int i = 0; i < type_alloc_stats[t].size; ++i)
      for (struct file_line *fl = type_alloc_stats[t].slots[i];
           fl; fl = fl->next)
        {
          vec = alloc_vector(5);
          SET_VECTOR(vec, 0, alloc_string(fl->file));
          vec->data[1] = makeint(fl->line);
          vec->data[2] = makeint(t);
          vec->data[3] = makeint(fl->count);
          vec->data[4] = makeint(fl->size);
          fl->count = fl->size = 0;

          res->data[used++] = vec;
        }
  UNGCPRO();

  return res;
}

static struct file_line *file_line_stat_lookup(struct file_line_stats *stats,
					       const char *file,
					       int line)
{
  unsigned key = str_int_hash(file, line);
  struct file_line *fl;

  if (stats->size == 0)
    return NULL;

  for (fl = stats->slots[key % stats->size]; fl; fl = fl->next)
    if (fl->key == key && fl->line == line
	&& !strcmp(fl->file, file))
      return fl;

  return NULL;
}

static void inc_file_line(struct file_line_stats *stats, const char *file,
			  int line, int size)
{
  struct file_line *data = file_line_stat_lookup(stats, file, line);

  if (data != NULL)
    {
      ++data->count;
      data->size += size;
      return;
    }

  unsigned key = str_int_hash(file, line);

  if (stats->used * 3 >= stats->size * 2)
    {
      int osize = stats->size;
      stats->size = osize ? osize * 2 : 16;
      struct file_line **new = calloc(stats->size, sizeof *new);

      for (int i = 0; i < osize; ++i)
        {
          for (struct file_line *fl = stats->slots[i], *next;
               fl; fl = next)
            {
              next = fl->next;
              int slot = fl->key % stats->size;
              fl->next = new[slot];
              new[slot] = fl;
            }
        }

      free(stats->slots);
      stats->slots = new;
    }

  data = malloc(sizeof *data);
  data->key = key;
  data->file = strdup(file);
  data->line = line;
  data->count = 1;
  data->size = size;

  data->next = stats->slots[key % stats->size];
  stats->slots[key % stats->size] = data;

  ++stats->used;
}

void record_allocation(enum mudlle_type type, long size)
{
  const char *filename = "<nowhere>";
  int line = 0;;

  struct call_stack *cs = call_stack;
  if (cs == NULL)
    goto done;

  while (cs->next)
    {
      switch (cs->type) {
      case call_c:
      case call_compiled:
      case call_primop:
      case call_invalid:
        break;
      case call_string:
        goto done;
      case call_bytecode:
        {
          struct call_stack_mudlle *mcs = (struct call_stack_mudlle *)cs;
          filename = mcs->fn->code->filename->str;
          line = mcs->fn->code->lineno;
          goto done;
        }
      }
      cs = cs->next;
    }

 done:
  inc_file_line(&type_alloc_stats[type], filename, line, size);
}

#endif /* ALLOC_STATS */

struct closure *unsafe_alloc_closure(ulong nb_variables)
{
  ulong fields = nb_variables + grecord_fields(struct closure);
  return make_readonly(UNSAFE_ALLOCATE_RECORD(closure, fields));
}

struct closure *alloc_closure0(struct code *code)
{
  GCPRO1(code);

#ifdef ALLOC_STATS
  record_allocation(type_closure, sizeof(struct obj) + sizeof(value));
#endif

  struct closure *newp = (struct closure *)allocate_record(
    type_closure, grecord_fields(*newp));
  newp->code = code;
  assert(immutablep(code));
  newp->o.flags |= OBJ_READONLY | OBJ_IMMUTABLE;
  UNGCPRO();

  return newp;
}

/* allocate uninitialized string for 'len' characters */
struct string *alloc_empty_string(size_t len)
{
  struct string *result = (struct string *)allocate_string(type_string,
                                                           len + 1);
  result->str[len] = 0;
  return result;
}

struct string *alloc_string(const char *s)
{
  if (!s) s = "(null)";
  size_t l = strlen(s);
  struct string *newp = (struct string *)allocate_string(type_string, l + 1);
  memcpy(newp->str, s, l + 1);
  return newp;
}

struct string *alloc_string_length(const char *str, size_t len)
{
  if (str == NULL && len > 0)
    {
      abort();
    }

  struct string *newp = alloc_empty_string(len);
  memcpy(newp->str, str, len);

  return newp;
}

struct string *mudlle_string_copy(struct string *s)
{
  GCPRO1(s);
  size_t len = string_len(s);
  struct string *result = alloc_empty_string(len);
  memcpy(result->str, s->str, len);
  UNGCPRO();
  return result;
}

char *mudlle_string_dup(struct string *s)
{
  size_t size = string_len(s) + 1;
  char *r = malloc(size);
  memcpy(r, s->str, size);
  return r;
}

struct string *safe_alloc_string(const char *s)
{
  return alloc_string(s ? s : "");
}

struct variable *alloc_variable(value val)
{
  GCCHECK(val);
  GCPRO1(val);
  struct variable *newp = UNSAFE_ALLOCATE_RECORD(variable, 1);
  newp->vvalue = val;
  UNGCPRO();

  return newp;
}

struct mudlle_float *alloc_mudlle_float(double d)
{
  struct mudlle_float *newp;

  newp = (struct mudlle_float *)allocate_string(type_float, sizeof d);
  newp->d = d;
  newp->o.flags |= OBJ_READONLY | OBJ_IMMUTABLE;

  return newp;
}

/* always run this before doing any operations on bi, in case a GC has
   moved the data around */
void check_bigint(struct bigint *bi)
{
#ifdef USE_GMP
  bi->mpz[0]._mp_d = &bi->limbs[0];
#endif
}

struct bigint *alloc_bigint(mpz_t mpz)
{
  struct bigint *newp;

#ifdef USE_GMP
  newp = (struct bigint *)
    allocate_string(type_bigint,
		    sizeof(mpz_t) +
		    sizeof(mp_limb_t) * mpz[0]._mp_alloc);
  newp->mpz[0]._mp_alloc = mpz[0]._mp_alloc;
  newp->mpz[0]._mp_size = mpz[0]._mp_size;
  newp->mpz[0]._mp_d = (mp_limb_t *)0xdeadbeef;
  memcpy(&newp->limbs[0], mpz->_mp_d,
	 sizeof(mp_limb_t) * abs(mpz[0]._mp_size));
#else
  newp = (struct bigint *)allocate_string(type_bigint, sizeof *newp);
#endif
  newp->o.flags |= OBJ_READONLY | OBJ_IMMUTABLE;

  return newp;
}

struct symbol *alloc_symbol(struct string *name, value data)
{
  GCCHECK(name);
  GCCHECK(data);
  assert(obj_readonlyp(&name->o));
  GCPRO2(name, data);
  struct symbol *newp = UNSAFE_ALLOCATE_RECORD(symbol, 2);
  newp->name = name;
  newp->data = data;
  UNGCPRO();

  return newp;
}

struct vector *alloc_vector(ulong size)
{
  return (struct vector *)allocate_record(type_vector, size);
}

struct list *alloc_list(value car, value cdr)
{
  GCCHECK(car);
  GCCHECK(cdr);
  GCPRO2(car, cdr);
  struct list *newp = (struct list *)unsafe_allocate_record(
    type_pair, grecord_fields(*newp));
  newp->car = car;
  newp->cdr = cdr;
  UNGCPRO();

  return newp;
}

struct character *alloc_character(struct char_data *ch)
{
  return (struct character *)allocate_temp(type_character, ch);
}

struct object *alloc_object(struct obj_data *obj)
{
  return (struct object *)allocate_temp(type_object, obj);
}

struct mprivate *alloc_private(enum mprivate_type type, ulong size)
{
  struct mprivate *p = (struct mprivate *)allocate_record(
    type_private, grecord_fields(*p) + size);
  p->ptype = makeint(type);
  return p;
}

/*
 * Converts the string sp or length len into an int i and returns true.
 * On over/underflow or illegal characters, it returns false.
 */
bool mudlle_strtolong(const char *sp, size_t len, long *l, int base)
{
  const char *const end = sp + len;

  assert(base == 0 || (base >= 2 && base <= 36));

  while (sp < end && isspace(*(unsigned char *)sp))
    ++sp;

  if (sp == end)
    return false;

  int sign;
  if (*sp == '+')
    {
      sign = 1;
      ++sp;
    }
  else if (*sp == '-')
    {
      sign = -1;
      ++sp;
    }
  else
    sign = 0;

  if (sp == end)
    return false;

  if (*sp == '0')
    {
      unsigned char t = sp[1];
      t = tolower(t);
      if (t == 'x')
        {
          if (base != 0 && base != 16)
            return false;
          base = 16;
          sp += 2;
        }
      else if (t == 'b')
        {
          if (base != 0 && base != 2)
            return false;
          base = 2;
          sp += 2;
        }
      else
        {
          ++sp;
          if (sp == end)
            {
              /* optimize for common case */
              *l = 0;
              return true;
            }
          if (base == 0)
            base = 8;
          goto skip_empty_check;
        }
    }
  else if (base == 0)
    base = 10;

  if (sp == end)
    return false;

 skip_empty_check:;

  /* only allow the sign bit to be set if no + or - and base != 10 */
  long lim = (!sign && base != 10
              ? (MAX_TAGGED_INT << 1) + 1
              : (sign == -1
                 ? -MIN_TAGGED_INT
                 : MAX_TAGGED_INT));

  long limrad = lim / base;
  long n = 0;
  for (;;)
    {
      unsigned char c = *sp++;

      int d;
      if (isdigit(c))
	d = c - '0';
      else if (isalpha(c))
        d = toupper(c) - 'A' + 10;
      else
	return false;

      if (d >= base)
        return false;

      n = n * base + d;
      if (n > lim)
	return false;

      if (sp == end)
	{
	  if (!sign)
            n = (n << 1) >> 1;  /* extend sign bit */
	  *l = sign == -1 ? -n : n;
	  return true;
	}

      if (n > limrad)
	return false;
    }

}

/* returns true on success; sp[len] must be NUL */
bool mudlle_strtofloat(const char *sp, size_t len, double *d)
{
  assert(sp[len] == 0);         /* could make a copy to handle this case */
  char *endp;
  *d = strtod(sp, &endp);
  return *sp && endp == sp + len;
}
