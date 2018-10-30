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

#include "alloc.h"
#include "compile.h"
#include "context.h"
#include "error.h"
#include "global.h"
#include "interpret.h"
#include "mcompile.h"
#include "mparser.h"
#include "mudlle.h"
#include "print.h"
#include "stack.h"
#include "strbuf.h"
#include "table.h"

#include "runtime/runtime.h"

void mudlle_init(void)
{
  assert(table_good_size(MAX_TABLE_ENTRIES) <= MAX_VECTOR_SIZE);
  assert(table_good_size(MAX_TABLE_ENTRIES + 1) > MAX_VECTOR_SIZE);
  garbage_init();
  global_init();
  print_init();
  stack_init();
  module_init();
  runtime_init();
  compile_init();
  mcompile_init();
  ports_init();
  context_init();
}
