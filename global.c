/* $Log: global.c,v $
 * Revision 1.12  1994/10/09  06:42:06  arda
 * Libraries
 * Type inference
 * Many minor improvements
 *
 * Revision 1.11  1994/09/06  07:50:34  arda
 * Constant support: detect_immutability, global_set!, string_{i}search.
 *
 * Revision 1.10  1994/08/29  13:17:18  arda
 * Contagious immutability.
 * Global array of values instead of variables.
 * Direct recursion.
 *
 * Revision 1.9  1994/08/16  19:15:54  arda
 * Mudlle compiler for sparc now fully functional (68k compiler now needs
 * updating for primitives).
 * Changes to allow Sparc trap's for runtime errors.
 * Also added flags to primitives for better calling sequences.
 *
 * Revision 1.6  1994/02/12  17:24:44  arda
 * Owl: Better code generated.
 *
 * Revision 1.5  1993/04/22  18:58:35  un_autre
 * (MD) & Owl. Bug fixes. /player fixes. EVER_WHINER flag. saving_spells adjusted.
 *
 * Revision 1.4  1993/03/29  09:23:50  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.3  1993/03/14  16:14:13  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.2  1993/01/11  16:15:09  un_mec
 * Added read-only variables.
 *
 * Revision 1.1  1992/12/27  21:41:06  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

static char rcsid[] = "$Id: global.c,v 1.12 1994/10/09 06:42:06 arda Exp $";

#include <string.h>
#include "mudlle.h"
#include "global.h"
#include "objenv.h"
#include "table.h"
#include "alloc.h"
#include "types.h"
#include "error.h"
#include "module.h"

struct env *environment;
struct vector *env_values;
struct table *global;
struct vector *mvars;

void global_init(void)
/* Effects: Initialises the global environment before use.
*/
{
  environment = alloc_env(GLOBAL_SIZE);
  staticpro((value *)&environment);
  env_values = environment->values;
  staticpro((value *)&env_values);
  global = alloc_table(GLOBAL_SIZE);
  staticpro((value *)&global);
  mvars = alloc_vector(GLOBAL_SIZE);
  staticpro((value *)&mvars);
}

static ulong global_add(struct string *name, value val)
{
  struct symbol *pos;
  struct gcpro gcpro1;
  ulong index, old_size;

  GCCHECK(val);

  GCPRO1(name);
  old_size = intval(environment->size);
  index = env_add_entry(environment, val);
  if (intval(environment->size) != old_size) /* Increase mvars too */
    {
      struct vector *new_mvars = alloc_vector(intval(environment->size));

      memcpy(new_mvars->data, mvars->data, mvars->o.size - sizeof(struct obj));
      mvars = new_mvars;
    }
  env_values = environment->values;
  UNGCPRO();
  mvars->data[index] = makeint(var_normal);
  pos = table_add_fast(global, name, makeint(index));
  pos->o.flags |= OBJ_READONLY; /* index of global vars never changes */

  return index;
}

ulong global_lookup(const char *name)
/* Returns: the index for global variable name in environment.
     If name doesn't exist yet, it is created with a variable
     whose value is NULL.
   Modifies: environment
*/
{
  struct symbol *pos;

  if (table_lookup(global, name, &pos)) return (ulong)intval(pos->data);

  return global_add(alloc_string(name), NULL);
}

ulong mglobal_lookup(struct string *name)
/* Returns: the index for global variable name in environment.
     If name doesn't exist yet, it is created with a variable
     whose value is NULL.
   Modifies: environment
*/
{
  struct symbol *pos;
  struct string *tname;
  struct gcpro gcpro1;

  if (table_lookup(global, name->str, &pos)) return (ulong)intval(pos->data);

  GCPRO1(name);
  tname = (struct string *)allocate_string(type_string, string_len(name) + 1);
  strcpy(tname->str, name->str);
  UNGCPRO();

  return global_add(tname, NULL);
}

struct list *global_list(void)
/* Returns: List of symbols representing all the global variables.
     The value cell of each symbol contains the variables number
*/
{
  return table_list(global);
}
