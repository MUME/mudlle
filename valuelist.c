/* $Log: valuelist.c,v $
 * Revision 1.3  1993/04/22  18:58:59  un_autre
 * (MD) & Owl. Bug fixes. /player fixes. EVER_WHINER flag. saving_spells adjusted.
 *
 * Revision 1.2  1993/03/29  09:24:55  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.3  1993/03/14  16:15:24  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.1  1992/12/27  21:41:48  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

static char rcsid[] = "$Id: valuelist.c,v 1.3 1993/04/22 18:58:59 un_autre Exp $";

#include "mudlle.h"
#include "valuelist.h"
#include "alloc.h"

void addtail(valuelist *list, value value)
{
  struct local_value *new = allocate(memory, sizeof *new);

  GCCHECK(value);
  new->value = value;
  new->prev = list->last;
  new->next = NULL;
  if (!list->first) list->first = list->last = new;
  else
    {
      list->last->next = new;
      list->last = new;
    }
}
