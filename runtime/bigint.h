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

#ifndef RUNTIME_BIGINT_H
#define RUNTIME_BIGINT_H

#include <limits.h>
#include <stdbool.h>

#include "../mudlle-config.h"
#include "../mvalues.h"

void bigint_init(void);
void free_mpz_temps(void);

#ifdef USE_GMP
struct bigint;
double bigint_to_double(struct bigint *bi);
#endif

value make_unsigned_int_or_bigint(unsigned long long u);
value make_signed_int_or_bigint(long long s);

#define __make_i_or_bi(x, un, TYPE)                             \
  (TYPE ## _MAX < MAX_TAGGED_INT				\
   ? makeint((long)(x))						\
   : make_ ## un ## signed_int_or_bigint(x))
#define make_int_or_bigint(x)                                   \
  _Generic(                                                     \
    (x),                                                        \
    bool               : makebool(x),                           \
    char               : makeint((long)(x)),                    \
    signed char        : __make_i_or_bi((x), , SCHAR),          \
    unsigned char      : __make_i_or_bi((x), un, UCHAR),	\
    short              : __make_i_or_bi((x), , SHRT),           \
    unsigned short     : __make_i_or_bi((x), un, USHRT),	\
    int                : __make_i_or_bi((x), , INT),            \
    unsigned           : __make_i_or_bi((x), un, UINT),         \
    long               : make_signed_int_or_bigint(x),          \
    unsigned long      : make_unsigned_int_or_bigint(x),        \
    long long          : make_signed_int_or_bigint(x),          \
    unsigned long long : make_unsigned_int_or_bigint(x))

#endif /* RUNTIME_BIGINT_H */
