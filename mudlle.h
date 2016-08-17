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

#ifndef MUDLLE_H
#define MUDLLE_H

/* Generally useful declarations for the mudlle (pronounced muddle)
   interpreter */

#include "options.h"

#  ifndef __GNUC__
#    define __attribute__(x)
#  endif

#ifndef NORETURN
#  define NORETURN __attribute__ ((noreturn))
#endif

#ifndef UNUSED
#  define UNUSED __attribute__((unused))
#endif

#ifndef FMT_PRINTF
#  define FMT_PRINTF(a, b) __attribute__((format(printf, a, b)))
#endif

#ifdef USE_GMP
#include <gmp.h>
#else
typedef unsigned long mpz_t;
typedef char mp_limb_t;
#define mpz_init_set_str(m, s, n) ((m) = 0)
#define mpz_cmp(b1, b2) 1
#endif

#  include <assert.h>
#  include <stdbool.h>

#ifdef HAVE_ALLOCA_H
#  include <alloca.h>
#endif


#define VLENGTH(name) (sizeof (name) / sizeof (name)[0])

#ifdef HAVE_STATIC_ASSERT

#define CASSERT(name, what)                                     \
  _Static_assert((what), "CASSERT(" #name ", " #what ")")
#define CASSERT_EXPR(what)                                      \
  ((void)sizeof (struct {                                       \
      _Static_assert((what), "CASSERT_EXPR(" #what ")");        \
      int i;                                                    \
  }))
#define CASSERT_VLEN(name, len)                                 \
  _Static_assert(VLENGTH(name) == (len),                        \
                 "CASSERT_VLEN(" #name ", " #len ")")

#else  /* ! HAVE_STATIC_ASSERT */

#define __CASSERT_SIZE(what)                            \
  (__builtin_constant_p(what) && (what) ? 1 : -1)
#define CASSERT_EXPR(what)                              \
  ((void)sizeof (int [__CASSERT_SIZE(what)]))
#define CASSERT(name, what)                             \
  typedef int __cassert_ ## name[__CASSERT_SIZE(what)]  \
  __attribute__((unused))
#define CASSERT_VLEN(name, len)                         \
  CASSERT(name ## _length, VLENGTH(name) == (len))

#endif  /* ! HAVE_STATIC_ASSERT */


#ifdef WIN32
static inline long __ntohl(long n)
{
  return (((n & 0xff) << 24)
          | ((n & 0xff00) << 8)
          | ((n & 0xff0000) >> 8)
          | ((n & 0xff000000) >> 24));
}

static inline short __ntohs(short n)
{
  return (((n & 0xff) << 8)
          | ((n & 0xff00) >> 8));
}
#endif /* WIN32 */

enum mudlle_data_version {
  MDATA_VER_LEGACY,
  MDATA_VER_NEW_HASH,       /* new hash algorithm for symbol tables */
  MDATA_VER_RO_SYM_NAMES,   /* symbols forced to have readonly names */

  MDATA_VERSIONS,
  MDATA_VER_CURRENT = MDATA_VERSIONS - 1
};

void mudlle_init(void);

#endif
