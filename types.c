/* $Log: types.c,v $
 * Revision 1.10  1994/10/09  06:43:08  arda
 * Libraries
 * Type inference
 * Many minor improvements
 *
 * Revision 1.9  1994/08/29  13:17:35  arda
 * Contagious immutability.
 * Global array of values instead of variables.
 * Direct recursion.
 *
 * Revision 1.8  1994/02/11  09:59:21  dgay
 * Owl: -Wall
 *      new shared string handling
 *      configuration file
 *
 * Revision 1.7  1993/07/25  10:58:38  un_mec
 * Owl: General schema for private types defined.
 *
 * Revision 1.6  1993/04/22  18:58:57  un_autre
 * (MD) & Owl. Bug fixes. /player fixes. EVER_WHINER flag. saving_spells adjusted.
 *
 * Revision 1.5  1993/03/29  09:24:47  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.4  1993/03/17  12:50:06  dgay
 * Fixed GC of help strings in code blocks.
 * Added security features.
 *
 * Revision 1.3  1993/03/14  16:15:11  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.3  1993/01/08  23:57:10  un_mec
 * Owl: Allow characters and objects to appear in mudlle.
 *
 * Revision 1.2  1992/12/30  14:10:58  un_mec
 * Owl:
 * Several changes:
 * - Variables don't have separate value & function cells, instead their are
 *   now 2 types: type_function & type_variable.
 * - print_value: New types (list, vector), printing rationalised.
 * - New type: list (Lisp style pair)
 * - lexer.l: Debug read_from_string
 * - debug_level & DEBUG macro provided to help debugging.
 *
 * Revision 1.1  1992/12/27  21:41:42  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

static char rcsid[] = "$Id: types.c,v 1.10 1994/10/09 06:43:08 arda Exp $";

#include <string.h>
#include "mudlle.h"
#include "types.h"
#include "alloc.h"

struct closure *unsafe_alloc_closure(ulong nb_variables)
{
  struct closure *new = (struct closure *)unsafe_allocate_record(type_closure, nb_variables + 1);

  new->o.flags |= OBJ_READONLY;

  return new;
}

struct closure *alloc_closure0(struct code *code)
{
  struct closure *new;
  struct gcpro gcpro1;

  GCCHECK(code);
  GCPRO1(code);
  new = (struct closure *)allocate_record(type_closure, 1);
  new->code = code;
  new->o.flags |= OBJ_READONLY;
  UNGCPRO();

  return new;
}

struct string *alloc_string(const char *s)
{
  struct string *new = (struct string *)allocate_string(type_string, strlen(s) + 1);

  strcpy(new->str, s);

  return new;
}

struct variable *alloc_variable(value val)
{
  struct variable *new;
  struct gcpro gcpro1;

  GCCHECK(val);
  GCPRO1(val);
  new = (struct variable *)unsafe_allocate_record(type_variable, 1);
  new->value = val;
  UNGCPRO();

  return new;
}

struct symbol *alloc_symbol(struct string *name, value data)
{
  struct gcpro gcpro1, gcpro2;
  struct symbol *new;

  GCCHECK(name);
  GCCHECK(data);
  GCPRO2(name, data);
  new = (struct symbol *)unsafe_allocate_record(type_symbol, 2);
  new->name = name;
  new->data = data;
  UNGCPRO();

  return new;
}

struct vector *alloc_vector(ulong size)
{
  struct vector *new = (struct vector *)allocate_record(type_vector, size);

  return new;
}

struct list *alloc_list(value car, value cdr)
{
  struct gcpro gcpro1, gcpro2;
  struct list *new;

  GCCHECK(car);
  GCCHECK(cdr);
  GCPRO2(car, cdr);
  new = (struct list *)unsafe_allocate_record(type_pair, 2);
  new->car = car;
  new->cdr = cdr;
  UNGCPRO();

  return new;
}

struct character *alloc_character(struct char_data *ch)
{
  return (struct character *)allocate_temp(type_character, ch);
}

struct object *alloc_object(struct obj_data *obj)
{
  return (struct object *)allocate_temp(type_object, obj);
}

struct primitive *alloc_primitive(ulong nb, struct primitive_ext *op)
{
  return (struct primitive *)allocate_permanent(type_primitive, nb, op);
}

struct primitive *alloc_secure(ulong nb, struct primitive_ext *op)
{
  return (struct primitive *)allocate_permanent(type_secure, nb, op);
}

struct grecord *alloc_private(int id, ulong size)
{
  struct grecord *p = allocate_record(type_private, size + 1);

  p->data[0] = makeint(id);
  return p;
}
