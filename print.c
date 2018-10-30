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

#include "mudlle-config.h"

#include <ctype.h>
#include <float.h>
#include <math.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "alloc.h"
#include "code.h"
#include "context.h"
#include "dwarf.h"
#include "global.h"
#include "ins.h"
#include "print.h"
#include "strbuf.h"
#include "table.h"
#include "utils.h"

#include "runtime/mudlle-string.h"


#define DEFAULT_PRINT_COUNT 512 /* max # objects for display() etc. */
#define MAX_PRINT_STRLEN    400 /* max string length for display() etc. */

struct print_config {
  struct oport *f;              /* GCPRO'ed */
  enum prt_level level;
  size_t maxlen;                /* not SIZE_MAX implies capped port */
  size_t count;
  bool replace_gone;
};

static unsigned char writable_chars[256 / 8];
#define set_writable(c, ok) do {			\
  unsigned char __c = (c);				\
  if (ok)						\
    writable_chars[__c >> 3] |= P(__c & 7);             \
  else							\
    writable_chars[__c >> 3] &= ~P(__c & 7);            \
} while (0)
#define writable(c) (writable_chars[(unsigned char)(c) >> 3]	\
		     & P(((unsigned char)(c) & 7)))

static bool print_value(struct print_config *config, value v);

void sb_write_string(struct strbuf *sb, const char *str, size_t len)
{
  sb_makeroom(sb, 2 + len);     /* conservative */
  sb_addc(sb, '"');

  const char *end = str + len;
  for (;;)
    {
      const char *s = str;
      while (s < end && writable(*s))
        ++s;
      sb_addmem(sb, str, s - str);
      str = s;

      if (str == end)
        break;

      unsigned char c = *str++;
      sb_addc(sb, '\\');
      switch (c)
        {
        case '\\': case '"': sb_addc(sb, c); break;
        case '\a': sb_addc(sb, 'a'); break;
        case '\b': sb_addc(sb, 'b'); break;
        case '\f': sb_addc(sb, 'f'); break;
        case '\n': sb_addc(sb, 'n'); break;
        case '\r': sb_addc(sb, 'r'); break;
        case '\t': sb_addc(sb, 't'); break;
        case '\v': sb_addc(sb, 'v'); break;
        default: sb_printf(sb, "%03o", c); break;
        }
    }

  sb_addc(sb, '"');
}

static bool output_string(struct print_config *config, struct string *print,
                          bool str_nul)
{
  ulong l = string_len(print);
  if (str_nul)
    {
      /* treat as nul-terminated string */
      size_t slen = strlen(print->str);
      if (slen < l)
        l = slen + 1;           /* print the terminating \0 */
    }

  if (config->level == prt_display)
    {
      pswrite_substring(config->f, print, 0, l);
      return true;
    }

  long idx = 0;
  const char *suffix = "\"";

  if (config->maxlen == SIZE_MAX && l > MAX_PRINT_STRLEN)
    {
      l = MAX_PRINT_STRLEN;
      suffix = " ...\"";
    }

  GCPRO(print);

  pputc('"', config->f);
  while (idx < l)
    {
      long pos = idx;
      while (pos < l && writable(print->str[pos]))
        ++pos;

      pswrite_substring(config->f, print, idx, pos - idx);

      if (pos == l)
        break;

      unsigned char c = print->str[pos];
      pputc('\\', config->f);
      switch (c)
        {
        case '\\': case '"': pputc(c, config->f); break;
        case '\a': pputc('a', config->f); break;
        case '\b': pputc('b', config->f); break;
        case '\f': pputc('f', config->f); break;
        case '\n': pputc('n', config->f); break;
        case '\r': pputc('r', config->f); break;
        case '\t': pputc('t', config->f); break;
        case '\v': pputc('v', config->f); break;
        default: pprintf(config->f, "%03o", c); break;
        }

      idx = pos + 1;
    }
  UNGCPRO();
  pputs(suffix, config->f);
  return true;
}

/* write a nul-terminated mudlle string (stop at any nul character) */
void write_nul_string(struct oport *op, struct string *s)
{
  struct print_config config = {
    .maxlen = SIZE_MAX,
    .level  = prt_write,
    .count  = 1,
    .f      = op
  };
  GCPRO(config.f);
  output_string(&config, s, true);
  UNGCPRO();
}

static bool write_string(struct print_config *config, value v)
{
  return output_string(config, v, false);
}

static uint32_t last_instr_line;

static const char *global_name(int idx)
{
  if (idx < 0 || idx >= vector_len(global_names))
    return "<unknown>";

  static struct strbuf sb = SBNULL;
  sb_empty(&sb);
  struct string *str = GNAME(idx);
  sb_addmem(&sb, str->str, string_len(str));
  return sb_str(&sb);
}

static void print_global_exec(struct oport *f, const char *type, int nargs,
			      uint16_t uw)
{
  pprintf(f, "execute[%s %u %s] %d\n", type, uw, global_name(uw), nargs);
}

static int write_instruction(struct oport *f, union instruction *i,
                             ulong ofs, uint32_t line)
{
  union instruction *old_i = i;
  static const char *const brname[] = { "", "(loop)", "(nz)", "(z)" };
  static const char *const builtin_names[] = {
    "eq", "neq", "gt", "lt", "le", "ge", "ref", "set",
    "add", "sub", "bitand", "bitor", "not" };
  CASSERT_VLEN(builtin_names, op_builtin_not - op_builtin_eq + 1);

#define insinstr()  (*i++)
#define insoper()   (insinstr().op)
#define insuint8()  (insinstr().u)
#define insint8()   (insinstr().s)
#define insuint16() (i += 2, (i[-2].u << 8) + i[-1].u)
#define insint16()  ((int16_t)insuint16())

  enum operator op = insoper();

  if (line != last_instr_line)
    pprintf(f, "%5lu: ", (unsigned long)line);
  else
    pputs("       ", f);
  last_instr_line = line;

  pprintf(f, "%5lu: ", ofs);

  static const char *const var_class_names[] = {
    [vclass_local]   = "local",
    [vclass_closure] = "closure",
    [vclass_global]  = "global"
  };
#define VAR_CLASSES VLENGTH(var_class_names)

  CASSERT_EXPR(op_recall + VAR_CLASSES == op_assign);
  CASSERT_EXPR(op_assign + VAR_CLASSES == op_vref);
  CASSERT_EXPR(op_vref   + VAR_CLASSES == op_closure_var);

  if (op >= op_recall && op < op_closure_var + VAR_CLASSES)
    {
      static const char *const opnames[] = {
        "recall", "assign", "vref", "closure var"
      };

      const char *opname = opnames[(op - op_recall) / VAR_CLASSES];
      enum variable_class vclass = (op - op_recall) % VAR_CLASSES;
      if (vclass == vclass_global)
	{
	  unsigned uw = insuint16();
	  pprintf(f, "%s[global] %u %s\n", opname, uw, global_name(uw));
	}
      else
	pprintf(f, "%s[%s] %u\n", opname, var_class_names[vclass],
                (unsigned)insuint8());
    }
  else if (op >= op_builtin_eq && op <= op_builtin_not)
    pprintf(f, "builtin_%s\n", builtin_names[op - op_builtin_eq]);
  else if (op == op_typeset_check)
    pprintf(f, "typeset_check %d\n", insuint8());
  else if (op >= op_typecheck && op < op_typecheck + last_synthetic_type)
    pprintf(f, "typecheck %s %d\n",
            mudlle_type_names[op - op_typecheck],
            insuint8());
  else switch (op)
    {
    case op_define: pputs("define\n", f); break;
    case op_return: pputs("return\n", f); break;
    case op_constant1: pprintf(f, "constant %u\n", insuint8()); break;
    case op_constant2: pprintf(f, "constant %u\n", insuint16()); break;
    case op_integer1: pprintf(f, "integer1 %d\n", insint8()); break;
    case op_integer2: pprintf(f, "integer2 %d\n", insint16()); break;
    case op_closure: pprintf(f, "closure %u\n", insuint8()); break;
    case op_closure_code1: pprintf(f, "closure code %u\n", insuint8()); break;
    case op_closure_code2: pprintf(f, "closure code %u\n", insuint16()); break;
    case op_execute: pprintf(f, "execute %u\n", insuint8()); break;
    case op_execute2: pprintf(f, "execute %u\n", insuint16()); break;
    case op_execute_primitive:
      pprintf(f, "execute_primitive %u\n", insuint8());
      break;
    case op_execute_primitive2:
      pprintf(f, "execute_primitive %u\n", insuint16());
      break;
    case op_execute_secure: pprintf(f, "execute_secure %u\n", insuint8());
      break;
    case op_execute_secure2:
      pprintf(f, "execute_secure %u\n", insuint16());
      break;
    case op_execute_varargs: pprintf(f, "execute_varargs %u\n", insuint8());
      break;
    case op_execute_varargs2:
      pprintf(f, "execute_varargs %u\n", insuint16());
      break;
    case op_execute_global_1arg:
      print_global_exec(f, "global", 1, insuint16());
      break;
    case op_execute_global_2arg:
      print_global_exec(f, "global", 2, insuint16());
      break;
    case op_execute_primitive_1arg:
      print_global_exec(f, "primitive", 1, insuint16()); break;
    case op_execute_primitive_2arg:
      print_global_exec(f, "primitive", 2, insuint16()); break;
    case op_argcheck: pprintf(f, "argcheck %u\n", insuint8()); break;
    case op_varargs: pputs("varargs\n", f); break;
    case op_discard: pputs("discard\n", f); break;
    case op_pop_n: pprintf(f, "pop %u\n", insuint8()); break;
    case op_exit_n: pprintf(f, "exit %u\n", insuint8()); break;
    case op_branch1: case op_branch_z1: case op_branch_nz1: case op_loop1:
      {
        int8_t sgnbyte = insint8();
        pprintf(f, "branch%s %d (to %lu)\n", brname[(op - op_branch1) / 2],
                sgnbyte, ofs + (i - old_i + sgnbyte));
        break;
      }
    case op_branch2: case op_branch_z2: case op_branch_nz2: case op_loop2:
      {
        int16_t word1 = insint16();
        pprintf(f, "wbranch%s %d (to %lu)\n", brname[(op - op_branch1) / 2],
                word1, ofs + (i - old_i + word1));
        break;
      }
    case op_clear_local:
      pprintf(f, "clear[local] %u\n", (unsigned)insuint8());
      break;
    default:
      pprintf(f, "Opcode %d\n", op); break;
    }
  return i - old_i;
}

static bool write_code(struct print_config *config, value v)
{
  struct icode *c = v;
  last_instr_line = UINT32_MAX;

  GCPRO(c);
  union instruction *const ins
    = (union instruction *)&c->constants[c->nb_constants];
  long nbins = (union instruction *)((char *)c + c->code.o.size) - ins;
  pprintf(config->f, "Code %ld bytes:\n", nbins);

  for (long i = 0; i < nbins; )
    {
      uint32_t line = dwarf_lookup_line_number(&c->code, i);
      i += write_instruction(config->f, ins + i, i, line);
    }

  pprintf(config->f, "\n%u locals, %u stack, seclevel %u, %u constants:\n",
	  c->nb_locals, c->stkdepth, c->code.seclevel, c->nb_constants);
  bool result = true;
  for (long i = 0; i < c->nb_constants; i++)
    {
      pprintf(config->f, "%lu: ", i);
      if (integerp(c->constants[i]))
        {
          long l = intval(c->constants[i]);
          struct strbuf sb = sb_initf("%ld (%#lx)", l, l);
          pputs(sb_str(&sb), config->f);
          sb_free(&sb);
        }
      else if (!print_value(config, c->constants[i]))
        {
          result = false;
          break;
        }
      pputc('\n', config->f);
    }
  UNGCPRO();
  return result;
}

static bool write_primitive(struct print_config *config, value v)
{
  struct primitive *prim = v;
  pputs(prim->op->name, config->f);
  pputs("()", config->f);
  return true;
}

static bool write_closure(struct print_config *config, value v)
{
  struct closure *c = v;
  if (config->level != prt_examine)
    {
      struct code *code = c->code;
      if (code->varname)
        pswrite(config->f, code->varname);
      else
        pputs("fn", config->f);
      pputs("()", config->f);
      return true;
    }

  ulong nbvar = ((c->o.size - offsetof(struct closure, variables))
                 / sizeof (value));
  GCPRO(c);
  pputs("Closure, code is\n", config->f);

  if (!print_value(config, c->code))
    goto fail;
  pprintf(config->f, "\nand %lu variables are\n", nbvar);

  for (ulong i = 0; i < nbvar; i++)
    {
      pprintf(config->f, "%lu: ", i);
      if (!print_value(config, c->variables[i]))
        goto fail;
      pputc('\n', config->f);
    }
  UNGCPRO();
  return true;

 fail:
  UNGCPRO();
  return false;
}

static bool write_vector(struct print_config *config, value v)
{
  struct vector *vec = v;
  GCPRO(vec);
  pputc('[', config->f);
  const char *prefix = "";
  for (ulong i = 0, len = vector_len(vec); i < len; i++)
    {
      pputs(prefix, config->f);
      prefix = " ";
      if (!print_value(config, vec->data[i]))
        {
          UNGCPRO();
          return false;
        }
    }
  UNGCPRO();
  pputc(']', config->f);
  return true;
}

static bool write_list(struct print_config *config, value v)
{
  struct list *l = v;
  GCPRO(l);
  pputc('(', config->f);
  for (;;)
    {
      if (!print_value(config, l->car))
        goto fail;
      if (!TYPE(l->cdr, pair))
        break;
      pputc(' ', config->f);
      l = l->cdr;
    }

  if (l->cdr)
    {
      pputs(" . ", config->f);
      if (!print_value(config, l->cdr))
        goto fail;
    }
  UNGCPRO();
  pputc(')', config->f);
  return true;

 fail:
  UNGCPRO();
  return false;
}

struct write_table_data {
  struct print_config *config;
  const char *prefix;
};

/* return true if unsuccessful */
static bool write_table_entry(struct symbol *s, void *_data)
{
  GCPRO(s);

  struct write_table_data *data = _data;
  pputs(data->prefix, data->config->f);
  data->prefix = " ";
  write_string(data->config, s->name);
  pputc('=', data->config->f);
  bool ok = print_value(data->config, s->data);

  UNGCPRO();
  return !ok;
}

static bool write_table(struct print_config *config, value v)
{
  struct table *t = v;

  switch (config->level)
    {
    case prt_constant:
    case prt_examine:
      break;
    case prt_write:
      if (config->maxlen != SIZE_MAX)
        break;
      /* fallthrough */
    default:
      if (table_entries(t) > 10)
        {
          pputs(is_ctable(t) ? "{ctable}" : "{table}", config->f);
          return true;
        }
    }

  struct write_table_data data = {
    .config = config,
    .prefix = ""
  };

  GCPRO(t);
  pputc('{', config->f);
  if (is_ctable(t))
    {
      pputc('c', config->f);
      data.prefix = " ";
    }
  UNGCPRO();
  if (table_exists(t, write_table_entry, &data))
    return false;
  pputc('}', config->f);
  return true;
}

static bool write_symbol(struct print_config *config, value v)
{
  struct symbol *sym = v;
  GCPRO(sym);
  pputc('<', config->f);
  write_string(config, sym->name);
  pputc('=', config->f);
  if (!print_value(config, sym->data))
    {
      UNGCPRO();
      return false;
    }
  UNGCPRO();
  pputc('>', config->f);
  return true;
}


static bool write_reference(struct print_config *config, value v)
{
  struct grecord *rec = v;
  GCPRO(rec);
  pputc('&', config->f);
  long items = grecord_len(rec);
  value r0 = rec->data[0];
  bool result = true;
  switch (items)
    {
    case 1:
      if (integerp(r0))
        {
          long idx = intval(r0);
          assert(idx >= 0 && idx < intval(environment->used));
          struct string *n = GNAME(idx);
          pswrite(config->f, n);
          break;
        }
      if (!pointerp(r0))
        abort();
      switch (((struct obj *)r0)->type)
        {
        case type_variable: ;
          struct variable *var = r0;
          if (!print_value(config, var->vvalue))
            result = false;
          break;
        case type_symbol:
          pputs("symbol_get(", config->f);
          if (!print_value(config, rec->data[0]))
            {
              result = false;
              break;
            }
          pputc(')', config->f);
          break;
        default:
          abort();
        }
      break;
    case 2:;
      value r1 = rec->data[1];
      if (TYPE(r0, pair) && integerp(r1))
        {
          pputs(intval(r1) ? "cdr(" : "car(", config->f);
          if (!print_value(config, r0))
            {
              result = false;
              break;
            }
          pputc(')', config->f);
          break;
        }
      if (TYPE(r1, vector))
        {
          struct vector *fns = (struct vector *)r1;
          assert(vector_len(fns) == 3);
          struct string *desc = fns->data[0];
          assert(TYPE(desc, string));
          pputs("<", config->f);
          pswrite(config->f, desc);
          pputc('>', config->f);
          break;
        }
      print_value(config, r0);
      pputc('[', config->f);
      if (!print_value(config, r1))
        {
          result = false;
          break;
        }
      pputc(']', config->f);
      break;
    }
  UNGCPRO();
  return result;
}

static bool write_character(struct print_config *config, value v)
{
  return true;
}

static bool write_object(struct print_config *config, value v)
{
  return true;
}

static bool write_float(struct print_config *config, value v)
{
  struct mudlle_float *f = v;
  double d = f->d;
  if (isnan(d))
    {
      /* mudlle doesn't support different types of NaNs; they're not
         portable anyway */
      pputs("nan", config->f);
      return true;
    }

  switch (isinf(d))
    {
    case 1:
      pputs("inf", config->f);
      return true;
    case -1:
      pputs("-inf", config->f);
      return true;
    case 0:
      break;
    default:
      abort();
    }

  assert(isfinite(d));

#ifndef DBL_DECIMAL_DIG
  /* clang 3.8 doesn't define this C11 macro */
#define DBL_DECIMAL_DIG __DBL_DECIMAL_DIG__
#endif

  char buf[DBL_DECIMAL_DIG + 16];
  int prec = config->level == prt_constant ? DBL_DECIMAL_DIG : 6;
  int len = snprintf(buf, sizeof buf, "%.*g", prec, d);
  assert(len < sizeof buf);
  pputs(buf, config->f);

  /* append ".0" if the result looks like an integer */
  const char *s = buf;
  if (*s == '-')
    ++s;
  for (; *s; ++s)
    if (!isdigit(*s))
      return true;
  pputs(".0", config->f);
  return true;
}

static bool write_bigint(struct print_config *config, value v)
{
#ifdef USE_GMP
  struct bigint *bi = v;
  check_bigint(bi);
  int size = mpz_sizeinbase(bi->mpz, 10);
  char buf[size + 2];
  mpz_get_str(buf, 10, bi->mpz);
  if (config->level != prt_display)
    pputs("#b", config->f);
  pputs(buf, config->f);
#else
  pputs("{bigint-unsupported}", config->f);
#endif
  return true;
}

static bool write_variable(struct print_config *config, value v)
{
  struct variable *var = v;
  GCPRO(var);
  pputs("variable = ", config->f);
  UNGCPRO();
  return print_value(config, var->vvalue);
}

static bool write_private(struct print_config *config, value v)
{
  struct mprivate *val = v;
  assert(integerp(val->ptype));
  switch (intval(val->ptype))
    {
    case PRIVATE_MJMPBUF:
      {
        struct mjmpbuf *buf = (struct mjmpbuf *)val;
        if (buf->context)
          pputs("{jmpbuf}", config->f);
        else
          pputs("{old jmpbuf}", config->f);
        break;
      }
    case PRIVATE_REGEXP:
      pputs("{regexp}", config->f);
      break;
    default:
      pputs("{private}", config->f);
    }
  return true;
}

static bool print_value(struct print_config *config, value v)
{
  static const bool hidden[][last_type] = {
    [prt_display] = {
      [type_code]     = true,
      [type_variable] = true,
      [type_internal] = true,
      [type_symbol]   = true,
      [type_table]    = true,
      [type_gone]     = true,
      [type_oport]    = true,
      [type_mcode]    = true,
    },
    [prt_write] = {
      [type_code]     = true,
      [type_internal] = true,
      [type_gone]     = true,
      [type_oport]    = true,
      [type_mcode]    = true,
    },
    [prt_examine] = {
      [type_internal] = true,
      [type_gone]     = true,
      [type_oport]    = true,
      [type_mcode]    = true,
    }
  };

  if (get_stack_pointer() < hard_mudlle_stack_limit)
    {
      return false;
  }

  if (config->count-- == 0
      || (config->maxlen != SIZE_MAX && capped_port_overflow(config->f)))
    return false;

  if (integerp(v))
    {
      static struct intstr buf; /* static to save stack space */
      pputs(longtostr(&buf, 10, intval(v)), config->f);
      return true;
    }

  if (v == NULL)
    {
      pputs("()", config->f);
      return true;
    }

  enum mudlle_type type = ((struct obj *)v)->type;

  if (config->level == prt_constant)
    switch (type)
      {
      case type_string:
      case type_vector:
      case type_pair:
      case type_symbol:
      case type_table:
      case type_float:
      case type_bigint:
        break;
      default:
        if (config->replace_gone)
          {
            pprintf(config->f, "(/*%s*/)", mudlle_type_names[type]);
            return true;
          }
        return false;
      }
  else if (hidden[config->level][type])
    {
      pprintf(config->f, "{%s}", mudlle_type_names[type]);
      return true;
    }

  static bool (*const funcs[])(struct print_config *, value) = {
    [type_bigint]    = write_bigint,
    [type_character] = write_character,
    [type_closure]   = write_closure,
    [type_code]      = write_code,
    [type_float]     = write_float,
    [type_gone]      = NULL,    /* always hidden */
    [type_integer]   = NULL,    /* handled above */
    [type_internal]  = NULL,    /* always hidden */
    [type_null]      = NULL,    /* handled above */
    [type_object]    = write_object,
    [type_oport]     = NULL,    /* always hidden */
    [type_pair]      = write_list,
    [type_primitive] = write_primitive,
    [type_private]   = write_private,
    [type_reference] = write_reference,
    [type_secure]    = write_primitive,
    [type_string]    = write_string,
    [type_symbol]    = write_symbol,
    [type_table]     = write_table,
    [type_varargs]   = write_primitive,
    [type_variable]  = write_variable,
    [type_vector]    = write_vector,
  };
  CASSERT_VLEN(funcs, last_type);
  return funcs[type](config, v);
}

static bool safe_output_value(value *v, struct print_config *config,
                              bool no_quote)
{
  if (!no_quote)
    switch (TYPEOF(*v))
      {
      case type_null:           /* null should never happen */
      case type_pair:
      case type_vector:
      case type_symbol:
      case type_table:
        pputc('\'', config->f);
        break;
      default:
        break;
      }

  return print_value(config, *v);
}

static bool simple_print(struct oport *f, const char *s, enum prt_level level,
                         size_t maxlen)
{
  size_t len = strlen(s);
  bool ok = len <= maxlen;
  if (!ok)
    {
      if (level == prt_constant)
        return false;
      len = maxlen;
    }
  opwrite(f, s, len);
  return ok;
}

/* return true if printed correctly */
static bool internal_output_value(struct oport *f, enum prt_level level,
                                  bool no_quote, value v, size_t maxlen,
                                  bool replace_gone)
{
  if (!f)
    return true;

  /* Optimise common cases (avoid complexity check overhead) */
  if (integerp(v))
    {
      struct intstr buf;
      return simple_print(f, longtostr(&buf, 10, intval(v)), level, maxlen);
    }

  if (v == NULL)
    {
      const char *s = (no_quote
                       ? "()"
                       : (level == prt_constant ? "'()" : "null"));
      return simple_print(f, s, level, maxlen);
    }

  size_t print_count = maxlen == SIZE_MAX ? DEFAULT_PRINT_COUNT : SIZE_MAX;
  struct print_config config = {
    .maxlen       = maxlen,
    .level        = level,
    .count        = print_count,
    .replace_gone = replace_gone
  };

  if (TYPE(v, string) && maxlen == SIZE_MAX)
    {
      GCPRO(config.f);
      config.f = f;
      if (!write_string(&config, v))
        abort();
      UNGCPRO();
      return true;
    }

  GCPRO(config.f, f, v);

  bool result;
  if (level == prt_constant)
    {
      /* dry run to make sure printing is successful */
      config.f = make_sink_oport();
      if (config.maxlen != SIZE_MAX)
        config.f = make_capped_oport(config.f, config.maxlen);
      if (!safe_output_value(&v, &config, no_quote)
          || (config.maxlen != SIZE_MAX && capped_port_overflow(config.f)))
        {
          result = false;
          goto done;
        }

      /* restore max count */
      config.count = print_count;
    }

  /* if maxlen is SIZE_MAX, we need temporary storage as we may need
     to fall back to "<complex ...>" */
  config.f = (config.maxlen == SIZE_MAX
              ? make_string_oport()
              : make_capped_oport(f, config.maxlen));

  if (!safe_output_value(&v, &config, no_quote))
    {
      /* some problem occurred */
      if (config.level == prt_constant)
        abort();         /* should never happen as we did a dry run */

      if (config.maxlen == SIZE_MAX)
        pprintf(f, "<complex %s>", mudlle_type_names[TYPEOF(v)]);
      result = false;
    }
  else if (config.maxlen == SIZE_MAX)
    {
      port_append(f, config.f);
      result = true;
    }
  else
    result = !capped_port_overflow(config.f);

 done:
  UNGCPRO();
  opclose(config.f);
  return result;
}

void output_value_cut(struct oport *f, enum prt_level level,
                      bool no_quote, value v, size_t maxlen)
{
  internal_output_value(f, level, no_quote, v, maxlen, false);
}

void output_value(struct oport *f, enum prt_level level, value v)
{
  internal_output_value(f, level, false, v, SIZE_MAX, false);
}

bool print_constant(struct oport *f, value v, size_t maxlen,
                    bool replace_gone)
{
  return internal_output_value(f, prt_constant, true, v, maxlen, replace_gone);
}

void describe_fn(struct strbuf *sb, value v)
{
  struct oport *op;
  {
    GCPRO(v);
    op = make_strbuf_oport(sb);
    UNGCPRO();
  }

  if (!pointerp(v))
    goto no_fn;

  struct obj *o = v;
  switch (o->type)
    {
    case type_primitive:
    case type_secure:
    case type_varargs:
      {
        struct primitive *prim = v;
        pprintf(op, "%s() [%s]", prim->op->name, mudlle_type_names[o->type]);
        return;
      }
    case type_closure:
      {
        struct closure *cl = v;
        struct code *code = cl->code;
        struct string *name = code->varname;
        struct string *filename = code->filename;
        long lineno = code->lineno;

        GCPRO(op, filename);
        if (name)
          pswrite(op, name);
        else
          pputs("<fn>", op);
        pputs("() [", op);
        pswrite(op, filename);
        pprintf(op, ":%ld]", lineno);
        UNGCPRO();
        return;
      }
    default:
      break;
    }

 no_fn:
  output_value(op, prt_display, v);
}

void print_init(void)
{
  for (unsigned char c = 32; c < 127; c++) set_writable(c, true);
  set_writable('"', false);
  set_writable('\\', false);
  for (unsigned char c = 161; c < 255; c++) set_writable(c, true);
}

void sb_add_seclevel(struct strbuf *sb, int lev)
{
  sb_printf(sb, "%d", lev);
}
