/*
 * Copyright (c) 1993-1999 David Gay and Gustav Hållberg
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
