#ifndef MUDLLE_H
#define MUDLLE_H

/* Generally useful declarations for the mudlle (pronounced muddle)
   interpreter */

#include "mudlle-config.h"

#ifdef USE_GMP
#  include <gmp.h>
#else  /* ! USE_GMP */
typedef unsigned long mpz_t;
typedef char mp_limb_t;
#  define mpz_init_set_str(m, s, n) ((m) = 0)
#  define mpz_cmp(b1, b2) 1
#  define mpz_neg(b1, b2) ((void)0)
#endif /* ! USE_GMP */

#  include <assert.h>
#  include <stdbool.h>
#  include <stdint.h>
#  include <stdnoreturn.h>
#  define UNUSED __attribute__((__unused__))
#  define FMT_PRINTF(a, b) __attribute__((__format__(__printf__, a, b)))

enum mudlle_data_version {
  MDATA_VER_LEGACY,
  MDATA_VER_NEW_HASH,       /* new hash algorithm for symbol tables */
  MDATA_VER_RO_SYM_NAMES,   /* symbols forced to have readonly names */

  MDATA_VERSIONS,
  MDATA_VER_CURRENT = MDATA_VERSIONS - 1
};

void mudlle_init(void);

#endif
