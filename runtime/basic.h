/*
 * Copyright (c) 1993-2012 David Gay and Gustav H�llberg
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

#ifndef RUNTIME_BASIC_H
#define RUNTIME_BASIC_H

#include "../error.h"
#include "../types.h"

extern const struct primitive_ext *const dereference_prim_ext;
extern const struct primitive_ext *const ref_prim_ext;
extern const struct primitive_ext *const set_refb_prim_ext;
extern const struct primitive_ext *const setb_prim_ext;
extern const struct primitive_ext *const warning_prim_ext;

void basic_init(void);
value code_ref(value x1, value x2);
value code_setb(value x1, value x2, value x3);

void ref_runtime_error(enum runtime_error error, value x1, value x2) NORETURN;
void set_runtime_error(enum runtime_error error, value x1, value x2, value x3)
  NORETURN;

#endif /* RUNTIME_BASIC_H */
