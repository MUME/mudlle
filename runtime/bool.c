/* $Log: bool.c,v $
 * Revision 1.6  1994/10/09  06:44:04  arda
 * Libraries
 * Type inference
 * Many minor improvements
 *
 * Revision 1.5  1994/08/16  19:17:00  arda
 * Added flags to primitives for better calling sequences.
 *
 * Revision 1.4  1993/04/25  19:50:33  un_mec
 * Owl: Miscellaneous changes.
 *      I HATE fixing bugs twice.
 *
 * Revision 1.3  1993/03/29  09:25:31  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.3  1993/03/14  16:16:34  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.1  1992/12/27  21:42:14  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

static char rcsid[] = "$Id: bool.c,v 1.6 1994/10/09 06:44:04 arda Exp $";

#include <string.h>
#include "runtime/runtime.h"

OPERATION(not, "b1 -> b2. b2 = not b1", 1, (value v),
	  OP_LEAF | OP_NOALLOC | OP_NOESCAPE)
{
  return makebool(!istrue(v));
}

OPERATION(or, "b1 b2 -> b. b = b1 or b2", 2, (value v1, value v2),
	  OP_LEAF | OP_NOALLOC | OP_NOESCAPE)
{
  return (makebool(istrue(v1) || istrue(v2)));
}

OPERATION(and, "b1 b2 -> b. b = b1 and b2", 2, (value v1, value v2),
	  OP_LEAF | OP_NOALLOC | OP_NOESCAPE)
{
  return (makebool(istrue(v1) && istrue(v2)));
}

OPERATION(equal, "x1 x2 -> b. TRUE if x1 and x2 are the same object",
	  2, (value v1, value v2),
	  OP_LEAF | OP_NOALLOC | OP_NOESCAPE)
{
  return (makebool(v1 == v2));
}

OPERATION(not_equal, "x1 x2 -> b. TRUE if n1 != n2", 2, (value v1, value v2),
	  OP_LEAF | OP_NOALLOC | OP_NOESCAPE)
{
  return (makebool(v1 != v2));
}

void bool_init(void)
{
  DEFINE("not", not);
  DEFINE("or", or);
  DEFINE("and", and);
  DEFINE("==", equal);
  DEFINE("!=", not_equal);
  system_define("true", makebool(TRUE));
  system_define("false", makebool(FALSE));
}
