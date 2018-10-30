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

#ifndef RUNTIME_MUDLLE_FLOAT_H
#define RUNTIME_MUDLLE_FLOAT_H

#include "../error.h"

#include "bigint.h"

void float_init(void);

enum runtime_error get_floatval(double *d, value v);

static inline double floatval(value v)
{
  double d;
  enum runtime_error e = get_floatval(&d, v);
  if (e != error_none)
    runtime_error(e);
  return d;
}

static inline value makefloat(double d)
{
  return (value)alloc_mudlle_float(d);
}

#define DEF_FLOAT(var) system_define(#var, makefloat(var))

#ifdef USE_GMP
#define FLOAT_TYPESET (TSET(integer) | TSET(bigint) | TSET(float))
#else
#define FLOAT_TYPESET (TSET(integer) | TSET(float))
#endif

#define __CT_FLOAT_E(var, msg, dst) get_floatval(&(dst), var)
#define CT_FLOAT(dst) F(FLOAT_TYPESET, __CT_FLOAT_E, dst)

#endif /* !RUNTIME_MUDLLE_FLOAT_H */
