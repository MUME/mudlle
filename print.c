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

#include <setjmp.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "alloc.h"
#include "code.h"
#include "global.h"
#include "ins.h"
#include "print.h"
#include "strbuf.h"
#include "table.h"

#include "runtime/mudlle-string.h"


#define DEFAULT_PRINT_COUNT 512
#define MAX_PRINT_COUNT     (64 * 1024)
#define MAX_PRINT_STRLEN    400
#define FLOATSTRLEN         20

static jmp_buf print_complex;

struct print_config {
  enum prt_level level;
  size_t    maxlen;             /* not (size_t)-1 implies string oport */
  size_t    count;
};

static unsigned char writable_chars[256 / 8];
#define set_writable(c, ok) do {			\
  unsigned char __c = (c);				\
  if (ok)						\
    writable_chars[__c >> 3] |= 1 << (__c & 7);		\
  else							\
    writable_chars[__c >> 3] &= ~(1 << (__c & 7));	\
} while(0)
#define writable(c) (writable_chars[(unsigned char)(c) >> 3]	\
		     & (1 << ((unsigned char)(c) & 7)))

static void _print_value(struct oport *f, struct print_config *config, value v,
                         bool toplev);

static void check_cutoff(struct oport *f, struct print_config *config)
{
  if (config->count-- == 0)
    longjmp(print_complex, 1);
  if (config->maxlen != (size_t)-1
      && string_port_length(f) >= config->maxlen)
    longjmp(print_complex, 1);
}

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

static void write_string(struct oport *p, struct print_config *config,
                         struct string *print)
{
  ulong l = string_len(print);

  if (config->level == prt_display)
    {
      pswrite_substring(p, print, 0, l);
      return;
    }

  long idx = 0;
  const char *suffix = "\"";

  if (config->maxlen == (size_t)-1 && l > MAX_PRINT_STRLEN)
    {
      l = MAX_PRINT_STRLEN;
      suffix = " ...\"";
    }

  GCPRO2(p, print);

  pputc('"', p);
  while (idx < l)
    {
      long pos = idx;
      while (pos < l && writable(print->str[pos]))
        ++pos;

      opwrite(p, print->str + idx, pos - idx);

      if (pos == l)
        break;

      unsigned char c = print->str[pos];
      pputc('\\', p);
      switch (c)
        {
        case '\\': case '"': pputc(c, p); break;
        case '\a': pputc('a', p); break;
        case '\b': pputc('b', p); break;
        case '\f': pputc('f', p); break;
        case '\n': pputc('n', p); break;
        case '\r': pputc('r', p); break;
        case '\t': pputc('t', p); break;
        case '\v': pputc('v', p); break;
        default: pprintf(p, "%03o", c); break;
        }

      idx = pos + 1;
    }
  pputs(suffix, p);
  UNGCPRO();
}

static int last_instr_line;

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
			      uword uw)
{
  pprintf(f, "execute[%s %u %s] %d\n", type, uw, global_name(uw), nargs);
}

static int write_instruction(struct icode *code, struct oport *f,
			     union instruction *i, ulong ofs)
{
  union instruction *old_i = i;
  static const char *const brname[] = { "", "(loop)", "(nz)", "(z)" };
  static const char *const builtin_names[] = {
    "eq", "neq", "gt", "lt", "le", "ge", "ref", "set",
    "add", "sub", "bitand", "bitor", "not" };
  CASSERT_VLEN(builtin_names, op_builtin_not - op_builtin_eq + 1);

#define insinstr() (*i++)
#define insoper()  (insinstr().op)
#define insubyte() (insinstr().u)
#define insbyte()  (insinstr().s)
#define insuword() (i += 2, (i[-2].u << 8) + i[-1].u)
#define insword()  ((word)insuword())

  enum operator op = insoper();

  int line = get_code_line_number(code, ofs);
  if (line != last_instr_line)
    pprintf(f, "%5d: ", line);
  else
    pputs("       ", f);
  last_instr_line = line;

  pprintf(f, "%5lu: ", ofs);

  if (op >= op_recall && op <= op_closure_var + vclass_closure)
    {
      static const char *const opnames[] = {
        "recall", "assign", "vref", "closure var"
      };
      static const char *const classname[] = {
	[vclass_local]	 = "local",
	[vclass_closure] = "closure",
	[vclass_global]	 = "global"
      };

      const char *opname = opnames[(op - op_recall) / 3];
      enum variable_class vclass = (op - op_recall) % 3;
      if (vclass == vclass_global)
	{
	  unsigned uw = insuword();
	  pprintf(f, "%s[global] %u %s\n", opname, uw, global_name(uw));
	}
      else
	pprintf(f, "%s[%s] %u\n", opname, classname[vclass],
                (unsigned)insubyte());
    }
  else if (op >= op_builtin_eq && op <= op_builtin_not)
    pprintf(f, "builtin_%s\n", builtin_names[op - op_builtin_eq]);
  else if (op == op_typeset_check)
    pprintf(f, "typeset_check %d\n", insubyte());
  else if (op >= op_typecheck && op < op_typecheck + last_synthetic_type)
    pprintf(f, "typecheck %s %d\n",
            mudlle_type_names[op - op_typecheck],
            insubyte());
  else switch (op)
    {
    case op_define: pprintf(f, "define\n"); break;
    case op_return: pprintf(f, "return\n"); break;
    case op_constant1: pprintf(f, "constant %u\n", insubyte()); break;
    case op_constant2: pprintf(f, "constant %u\n", insuword()); break;
    case op_integer1: pprintf(f, "integer1 %d\n", insbyte()); break;
    case op_integer2: pprintf(f, "integer2 %d\n", insword()); break;
    case op_closure: pprintf(f, "closure %u\n", insubyte()); break;
    case op_closure_code1: pprintf(f, "closure code %u\n", insubyte()); break;
    case op_closure_code2: pprintf(f, "closure code %u\n", insuword()); break;
    case op_execute: pprintf(f, "execute %u\n", insubyte()); break;
    case op_execute_primitive:
      pprintf(f, "execute_primitive %u\n", insubyte());
      break;
    case op_execute_secure: pprintf(f, "execute_secure %u\n", insubyte());
      break;
    case op_execute_varargs: pprintf(f, "execute_varargs %u\n", insubyte());
      break;
    case op_execute_global1: print_global_exec(f, "global", 1, insuword());
      break;
    case op_execute_global2: print_global_exec(f, "global", 2, insuword());
      break;
    case op_execute_primitive1:
      print_global_exec(f, "primitive", 1, insuword()); break;
    case op_execute_primitive2:
      print_global_exec(f, "primitive", 2, insuword()); break;
    case op_argcheck: pprintf(f, "argcheck %u\n", insubyte()); break;
    case op_varargs: pprintf(f, "varargs\n"); break;
    case op_discard: pprintf(f, "discard\n"); break;
    case op_pop_n: pprintf(f, "pop %u\n", insubyte()); break;
    case op_exit_n: pprintf(f, "exit %u\n", insubyte()); break;
    case op_branch1: case op_branch_z1: case op_branch_nz1: case op_loop1:
      {
        sbyte sgnbyte = insbyte();
        pprintf(f, "branch%s %d (to %lu)\n", brname[(op - op_branch1) / 2],
                sgnbyte, ofs + (i - old_i + sgnbyte));
        break;
      }
    case op_branch2: case op_branch_z2: case op_branch_nz2: case op_loop2:
      {
        word word1 = insword();
        pprintf(f, "wbranch%s %d (to %lu)\n", brname[(op - op_branch1) / 2],
                word1, ofs + (i - old_i + word1));
        break;
      }
    case op_clear_local:
      pprintf(f, "clear[local] %u\n", (unsigned)insubyte());
      break;
    default: pprintf(f, "Opcode %d\n", op); break;
    }
  return i - old_i;
}

static void write_code(struct oport *f, struct print_config *config,
                       struct icode *c)
{
  last_instr_line = -1;

  GCPRO2(f, c);
  union instruction *const ins
    = (union instruction *)&c->constants[c->nb_constants];
  long nbins = (union instruction *)((char *)c + c->code.o.size) - ins;
  pprintf(f, "Code %ld bytes:\n", nbins);

  for (long i = 0; i < nbins; )
    i += write_instruction(c, f, ins + i, i);

  pprintf(f, "\n%u locals, %u stack, seclevel %u, %u constants:\n",
	  c->nb_locals, c->stkdepth, c->code.seclevel, c->nb_constants);
  for (long i = 0; i < c->nb_constants; i++)
    {
      pprintf(f, "%lu: ", i);
      if (integerp(c->constants[i]))
        {
          long l = intval(c->constants[i]);
          struct strbuf sb = sb_initf("%ld (%#lx)", l, l);
          pputs(sb_str(&sb), f);
          sb_free(&sb);
        }
      else
        _print_value(f, config, c->constants[i], false);
      pprintf(f, "\n");
    }
  UNGCPRO();
}

static void write_closure(struct oport *f, struct print_config *config,
                          struct closure *c)
{
  ulong nbvar = ((c->o.size - offsetof(struct closure, variables))
                 / sizeof(value));
  GCPRO2(f, c);
  pprintf(f, "Closure, code is\n");
  _print_value(f, config, c->code, false);
  pprintf(f, "\nand %lu variables are\n", nbvar);

  for (ulong i = 0; i < nbvar; i++)
    {
      pprintf(f, "%lu: ", i);
      _print_value(f, config, c->variables[i], false);
      pprintf(f, "\n");
    }
  UNGCPRO();
}

static void write_vector(struct oport *f, struct print_config *config,
                         struct vector *v, bool toplev)
{
  ulong len = vector_len(v), i;
  GCPRO2(f, v);
  if (config->level != prt_display && toplev) pputc('\'', f);
  pputc('[', f);
  const char *prefix = "";
  for (i = 0; i < len; i++)
    {
      pputs(prefix, f);
      prefix = " ";
      _print_value(f, config, v->data[i], false);
    }
  pputc(']', f);
  UNGCPRO();
}

static void write_list(struct oport *f, struct print_config *config,
                       struct list *v, bool toplev)
{
  GCPRO2(f, v);
  if (config->level != prt_display && toplev)
    pputc('\'', f);
  pputc('(', f);
  for (;;)
    {
      _print_value(f, config, v->car, false);
      if (!TYPE(v->cdr, type_pair))
        break;
      pputc(' ', f);
      v = v->cdr;
    }

  if (v->cdr)
    {
      pputs(" . ", f);
      _print_value(f, config, v->cdr, false);
    }
  pprintf(f, ")");
  UNGCPRO();
}

struct write_table_data {
  struct oport *oport;
  struct print_config *config;
  const char *prefix;
};

static void write_table_entry(struct symbol *s, void *_data)
{
  GCPRO1(s);

  struct write_table_data *data = _data;
  pputs(data->prefix, data->oport);
  data->prefix = " ";
  write_string(data->oport, data->config, s->name);
  pputc('=', data->oport);
  _print_value(data->oport, data->config, s->data, false);

  UNGCPRO();
}

static void write_table(struct oport *f, struct print_config *config,
                        struct table *t, bool toplev)
{
  if (table_entries(t) > 10)
    {
      switch (config->level)
        {
        case prt_examine: break;
        case prt_write:
          if (config->maxlen != (size_t)-1)
            break;
          /* fallthrough */
        default:
          pputs(is_ctable(t) ? "{ctable}" : "{table}", f);
          return;
        }
    }

  struct write_table_data data = {
    .oport  = f,
    .config = config,
    .prefix = ""
  };

  GCPRO1(data.oport);
  if (config->level != prt_display && toplev)
    pputc('\'', data.oport);
  pputc('{', data.oport);
  if (is_ctable(t))
    {
      pputc('c', data.oport);
      data.prefix = " ";
    }
  table_foreach(t, &data, write_table_entry);
  pputc('}', data.oport);
  UNGCPRO();
}

static void write_character(struct oport *f, struct print_config *config,
                            struct character *c)
{
}

static void write_object(struct oport *f, struct print_config *config,
                         struct object *c)
{
}

static bool write_integer(struct oport *f, long v, size_t maxlen)
{
  char buf[INTSTRSIZE];
  char *s = inttostr(buf, 10, (ulong)v, true);
  size_t slen = strlen(s);
  opwrite(f, s, maxlen < slen ? maxlen : slen);
  return maxlen >= slen;
}

static void write_float(struct oport *f, struct mudlle_float *v)
{
  char buf[FLOATSTRLEN];
  int len = snprintf(buf, FLOATSTRLEN, "%#g", v->d);
  if (len > 0 && buf[len - 1] == '.')
    snprintf(buf, FLOATSTRLEN, "%#.7g", v->d);

  pputs(buf, f);
}

static void write_bigint(struct oport *f, struct print_config *config,
                         struct bigint *bi)
{
#ifdef USE_GMP
  check_bigint(bi);
  int size = mpz_sizeinbase(bi->mpz, 10);
  char buf[size + 2];
  mpz_get_str(buf, 10, bi->mpz);
  if (config->level != prt_display)
    pputs("#b", f);
  pputs(buf, f);
#else
  pputs("{bigint-unsupported}", f);
#endif
}

static void write_private(struct oport *f, struct mprivate *val)
{
  assert(integerp(val->ptype));
  switch (intval(val->ptype))
    {
    case PRIVATE_MJMPBUF:
      {
        struct mjmpbuf *buf = (struct mjmpbuf *)val;
        if (buf->context)
          pputs("{jmpbuf}", f);
        else
          pputs("{old jmpbuf}", f);
        break;
      }
    case PRIVATE_REGEXP:
      pputs("{regexp}", f);
      break;
    default:
      pputs("{private}", f);
    }
}

static void _print_value(struct oport *f, struct print_config *config,
                         value v, bool toplev)
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
    },
    [prt_constant] = {
      /* these are inverted! */
      [type_integer]  = true,
      [type_string]   = true,
      [type_vector]   = true,
      [type_pair]     = true,
      [type_symbol]   = true,
      [type_table]    = true,
      [type_float]    = true,
      [type_bigint]   = true,
      [type_null]     = true,
    }
  };

  check_cutoff(f, config);

  if (integerp(v))
    {
      write_integer(f, intval(v), (size_t)-1);
      return;
    }
  if (v == NULL)
    {
      pputs(toplev ? "null" : "()", f);
      return;
    }

  struct obj *obj = v;

  assert(obj->type < last_type);

  if (config->level == prt_constant)
    {
      if (!hidden[config->level][obj->type])
	longjmp(print_complex, 1);
    }
  else if (hidden[config->level][obj->type])
    {
      pprintf(f, "{%s}", mudlle_type_names[obj->type]);
      return;
    }

  switch (obj->type)
      {
      default: abort();
      case type_string: write_string(f, config, v); break;
      case type_symbol:
        {
          struct symbol *sym = v;

          GCPRO2(f, sym);
          if (config->level != prt_display && toplev) pputc('\'', f);
          pprintf(f, "<");
          write_string(f, config, sym->name);
          pprintf(f, "=");
          _print_value(f, config, sym->data, false);
          pprintf(f, ">");
          UNGCPRO();
          break;
        }
      case type_reference:
        {
          struct grecord *rec = v;
          GCPRO2(f, rec);
          pputc('&', f);
          long items = grecord_len(rec);
          value r0 = rec->data[0];
          switch (items)
            {
            case 1:
              if (integerp(r0))
                {
                  long idx = intval(r0);
                  assert(idx >= 0 && idx < intval(environment->used));
                  struct string *n = GNAME(idx);
                  pswrite(f, n);
                  break;
                }
              if (!pointerp(r0))
                abort();
              switch (((struct obj *)r0)->type)
                {
                case type_variable: ;
                  struct variable *var = r0;
                  _print_value(f, config, var->vvalue, false);
                  break;
                case type_symbol:
                  pputs("symbol_ref(", f);
                  _print_value(f, config, rec->data[0], true);
                  pputc(')', f);
                  break;
                default:
                  abort();
                }
              break;
            case 2:
              if (TYPE(r0, type_pair) && integerp(rec->data[1]))
                {
                  assert(integerp(rec->data[1]));
                  pputs(intval(rec->data[1]) ? "cdr(" : "car(", f);
                  _print_value(f, config, rec->data[0], true);
                  pputc(')', f);
                  break;
                }
              if (TYPE(rec->data[1], type_vector))
                {
                  struct vector *fns = (struct vector *)rec->data[1];
                  assert(vector_len(fns) == 3);
                  struct string *desc = fns->data[0];
                  assert(TYPE(desc, type_string));
                  pputs("&<", f);
                  pswrite(f, desc);
                  pputc('>', f);
                  break;
                }
              _print_value(f, config, r0, false);
              pputc('[', f);
              _print_value(f, config, rec->data[1], false);
              pputc(']', f);
              break;
            }
          UNGCPRO();
          break;
        }
      case type_variable:
        {
          struct variable *var = v;
          pputs("variable = ", f);
          _print_value(f, config, var->vvalue, false);
          break;
        }
      case type_private: write_private(f, v); break;
      case type_code: write_code(f, config, v); break;
      case type_primitive: case type_secure: case type_varargs:
        {
          struct primitive *prim = v;
          pputs(prim->op->name, f);
          pputs("()", f);
          break;
        }
      case type_closure:
        {
          struct closure *cl = v;
          struct code *code = cl->code;
          if (config->level == prt_examine)
            write_closure(f, config, v);
          else
            {
              if (code->varname)
                pswrite(f, code->varname);
              else
                pputs("fn", f);
              pputs("()", f);
            }
          break;
        }
      case type_table: write_table(f, config, v, toplev); break;
      case type_pair: write_list(f, config, v, toplev); break;
      case type_vector: write_vector(f, config, v, toplev); break;
      case type_character: write_character(f, config, v); break;
      case type_object: write_object(f, config, v); break;
      case type_float: write_float(f, v); break;
      case type_bigint: write_bigint(f, config, v); break;
      }
}

/* return true if value was correctly printed */
bool output_value_cut(struct oport *f, enum prt_level level, bool no_quote,
                      value v, size_t maxlen)
{
  if (!f) return false;
  /* Optimise common cases (avoid complexity check overhead) */
  if (integerp(v))
    return write_integer(f, intval(v), maxlen);
  if (v == NULL)
    {
      const char *s = (no_quote || level == prt_constant) ? "()" : "null";
      size_t l = strlen(s);
      opwrite(f, s, maxlen < l ? maxlen : l);
      return maxlen >= l;
    }

  struct print_config config = {
    .maxlen = maxlen,
    .level  = level,
    .count  = (maxlen == (size_t)-1
               ? DEFAULT_PRINT_COUNT
               : (MAX_PRINT_COUNT < maxlen
                  ? MAX_PRINT_COUNT
                  : maxlen))
  };

  if (((struct obj *)v)->type == type_string && maxlen == (size_t)-1)
    {
      write_string(f, &config, v);
      return true;
    }

  struct oport *p = NULL;
  GCPRO3(f, v, p);
  p = make_string_oport();
  bool ok = false;
  if (setjmp(print_complex))	/* exit when problems */
    {
      if (level == prt_constant)
	;			/* make no effort to do anything nice */
      else if (string_port_length(p) >= maxlen)
	port_append_substring(f, p, 0, maxlen);
      else
	{
	  struct strbuf sb = sb_initf("<complex %s>",
				      mudlle_type_names[TYPEOF(v)]);
	  size_t len = sb_len(&sb);
	  opwrite(f, sb_str(&sb), maxlen < len ? maxlen : len);
	  sb_free(&sb);
	}
    }
  else
    {
      _print_value(p, &config, v, !no_quote);
      ok = string_port_length(p) <= maxlen;
      port_append_substring(f, p, 0, maxlen);
    }
  opclose(p);
  UNGCPRO();
  return ok;
}

void output_value(struct oport *f, enum prt_level level, bool no_quote,
                  value v)
{
  output_value_cut(f, level, no_quote, v, (size_t)-1);
}

void describe_fn(struct strbuf *sb, value v)
{
  struct oport *op;
  {
    GCPRO1(v);
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

        GCPRO2(op, filename);
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
  output_value(op, prt_display, false, v);
}

void print_init(void)
{
  for (unsigned char c = 32; c < 127; c++) set_writable(c, true);
  set_writable('"', false);
  set_writable('\\', false);
  for (unsigned char c = 161; c < 255; c++) set_writable(c, true);
}
