/*
 * Copyright (c) 1993-2006 David Gay and Gustav Hållberg
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

#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <errno.h>
#include <math.h>
#include "alloc.h"

const char *const mtypenames[] = {
  "code",   "closure", "variable", "internal",  "primitive", "varargs",     
  "secure", "integer", "string",   "vector",    "list",      "symbol",      
  "table",  "private", "object",   "character", "gone",      "output-port", 
  "mcode",  "float",   "bigint",   "null",      
  "none",   "any",     "function", "list"
};
CASSERT_VLEN(mtypenames, last_synthetic_type);

#ifdef ALLOC_STATS

#define ALLOC_PAIR_STATS
#define ALLOC_VARIABLE_STATS

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

static struct file_line_stats closure_alloc_stats;
static struct file_line_stats pair_alloc_stats;
static struct file_line_stats variable_alloc_stats;

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

struct vector *file_line_stat_vector(struct file_line_stats *stats)
{
  struct vector *res = alloc_vector(stats->used);
  struct vector *vec = NULL;

  struct gcpro gcpro1, gcpro2;
  int used = 0, i;
  
  GCPRO2(res, vec);
  for (i = 0; i < stats->size; ++i) {
    struct file_line *fl;

    for (fl = stats->slots[i]; fl; fl = fl->next)
      {
	struct string *str;
	
	vec = alloc_vector(4);
	str = alloc_string(fl->file);
	vec->data[0] = str;
	vec->data[1] = makeint(fl->line);
	vec->data[2] = makeint(fl->count);
	vec->data[3] = makeint(fl->size);

	fl->count = fl->size = 0;

	res->data[used++] = vec;
      }
  }

  return res;
}

struct vector *get_closure_alloc_stats(void)
{
  return file_line_stat_vector(&closure_alloc_stats);
}

struct vector *get_pair_alloc_stats(void)
{
  return file_line_stat_vector(&pair_alloc_stats);
}

struct vector *get_variable_alloc_stats(void)
{
  return file_line_stat_vector(&variable_alloc_stats);
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

  if (data == NULL)
    {
      unsigned key = str_int_hash(file, line);

      if (stats->used * 3 >= stats->size * 2)
	{
	  struct file_line **new;
	  int i, osize = stats->size;

	  stats->size = osize ? osize * 2 : 16;
	  new = malloc(sizeof *new * stats->size);
	  memset(new, 0, sizeof *new * stats->size);
	  
	  for (i = 0; i < osize; ++i)
	    {
	      struct file_line *fl = stats->slots[i];
	      while (fl)
		{
		  struct file_line *next = fl->next;
		  
		  fl->next = new[fl->key % stats->size];
		  new[fl->key % stats->size] = fl;

		  fl = next;
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

      return;
    }

  ++data->count;
  data->size += size;
}

static void record_allocation(struct file_line_stats *stats, long size)
{
  struct call_stack *cs = call_stack;

  if (cs == NULL)
    {
      inc_file_line(stats,
		    "<nowhere>", 0,
		    size);
      return;
    }

  while (cs->next && cs->type == call_c)
    cs = cs->next;
  
  switch (cs->type) {
  case call_bytecode:
    inc_file_line(stats,
		  cs->u.mudlle.fn->code->filename->str,
		  cs->u.mudlle.fn->code->lineno,
		  size);
    break;
  case call_c:
    inc_file_line(stats,
		  cs->u.c.op->name,
		  -1,
		  size);
    break;
  case call_compiled:
    inc_file_line(stats,
		  "<compiled>",
		  0,
		  size);
    break;
  default:
    assert(0);
  }
}

#endif /* ALLOC_STATS */

struct closure *unsafe_alloc_closure(ulong nb_variables)
{
  struct closure *newp;

#ifdef ALLOC_CLOSURE_STATS
  record_allocation(&closure_alloc_stats, sizeof(struct obj) + (nb_variables + 1) * sizeof(value));
#endif

  newp = (struct closure *)unsafe_allocate_record(type_closure, nb_variables + 1);
  newp->o.flags |= OBJ_READONLY;

  return newp;
}

struct closure *alloc_closure0(struct code *code)
{
  struct closure *newp;
  struct gcpro gcpro1;

  GCPRO1(code);

#ifdef ALLOC_CLOSURE_STATS
  record_allocation(&closure_alloc_stats, sizeof(struct obj) + sizeof(value));
#endif

  newp = (struct closure *)allocate_record(type_closure, 1);
  newp->code = code;
  newp->o.flags |= OBJ_READONLY;
  UNGCPRO();

  return newp;
}

struct string *alloc_string(const char *s)
{
  struct string *newp;

  if (!s) s = "(null)";
  newp = (struct string *)allocate_string(type_string, strlen(s) + 1);

  strcpy(newp->str, s);

  return newp;
}

struct string *alloc_string_length(const char *str, size_t len)
{
  struct string *newp;

  if (!str)
    {
      str = "(null)";
      len = 6;
    }

  newp = (struct string *)allocate_string(type_string, len + 1);

  memcpy(newp->str, str, len);
  newp->str[len] = 0;

  return newp;
}

struct string *safe_alloc_string(const char *s)
{
  return alloc_string(s ? s : "");
}

struct variable *alloc_variable(value val)
{
  struct variable *newp;
  struct gcpro gcpro1;

  GCCHECK(val);
  GCPRO1(val);
#ifdef ALLOC_VARIABLE_STATS
  record_allocation(&variable_alloc_stats, sizeof(struct obj) + sizeof(value));
#endif
  newp = (struct variable *)unsafe_allocate_record(type_variable, 1);
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
  struct gcpro gcpro1, gcpro2;
  struct symbol *newp;

  GCCHECK(name);
  GCCHECK(data);
  GCPRO2(name, data);
  newp = (struct symbol *)unsafe_allocate_record(type_symbol, 2);
  newp->name = name;
  newp->data = data;
  UNGCPRO();

  return newp;
}

struct vector *alloc_vector(ulong size)
{
  struct vector *newp = (struct vector *)allocate_record(type_vector, size);

  return newp;
}

struct list *alloc_list(value car, value cdr)
{
  struct gcpro gcpro1, gcpro2;
  struct list *newp;

  GCCHECK(car);
  GCCHECK(cdr);
  GCPRO2(car, cdr);
#ifdef ALLOC_PAIR_STATS
  record_allocation(&pair_alloc_stats, sizeof(struct list));
#endif
  newp = (struct list *)unsafe_allocate_record(type_pair, 2);
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

struct primitive *alloc_primitive(ulong nb, const struct primitive_ext *op)
{
  return (struct primitive *)allocate_permanent(type_primitive, nb, (void *)op);
}

struct primitive *alloc_secure(ulong nb, const struct primitive_ext *op)
{
  return (struct primitive *)allocate_permanent(type_secure, nb, (void *)op);
}

struct grecord *alloc_private(int id, ulong size)
{
  struct grecord *p = allocate_record(type_private, size + 1);

  p->data[0] = makeint(id);
  return p;
}

/*
 * Converts the string sp into an int i and returns 1.
 * On over/underflow or illegal characters, it returns 0.
 */
int mudlle_strtoint(const char *sp, int *i)
{
  int n = 0;
  int lim, limrad;
  int sign, radix;

  while (isspace(*(unsigned char *)sp)) 
    ++sp;

  if (*sp == '+' || *sp == '-')
    sign = *(sp++) == '-' ? -1 : 1;
  else
    sign = 0;

  /* only allow the sign bit to be set if no + or - and radix != 10 */

  lim = (!sign ? (MAX_TAGGED_INT << 1) + 1 : 
	 sign == -1 ? -MIN_TAGGED_INT : MAX_TAGGED_INT);

  if (*sp == '0' && *(sp + 1) == 'x')
    {
      radix = 16;
      sp += 2;
    }
  else
    {
      radix = 10;
      if (!sign) 
	lim = MAX_TAGGED_INT;
    }

  if (!*sp) 
    return 0;

  limrad = lim / radix;

  for (;;)
    {
      char c = toupper(*(sp++));

      if (!c)
	{ 
	  if (!sign && n & (MAX_TAGGED_INT + 1))
	    n |= 0x80000000;          /* have to extend the sign bit here */
	  *i = sign == - 1 ? -n : n;
	  return 1;
	}

      if (n > limrad)
	return 0;

      n *= radix;
      if (c >= '0' && c <= '9')
	n += c - '0';
      else if (c >= 'A' && c < 'A' - 10 + radix)
	n += c - 'A' + 10;
      else
	return 0;

      if (n > lim)
	return 0;
    }
  
}

int mudlle_strtofloat(const char *sp, double *d)
{
  char *endp;

  if (*sp == '0' && *(sp + 1) == 'f')
    {
      int i;
      char buf[9];
      union {
	double d;
	long l[2];
      } u;

      sp += 2;
      for (i = 0; i < 16; ++i)
	if (!isxdigit((unsigned char)sp[++i]))
	  return 0;
      if (sp[16]) 
	return 0;
	
      u.l[0] = strtol(sp + 8, NULL, 16);
      memcpy(buf, sp, 8);
      buf[8] = 0;
      u.l[1] = strtol(buf, NULL, 16); 

      *d = u.d;
      return 1;
    }

  *d = strtod(sp, &endp);

  return *sp && !*endp;
}
