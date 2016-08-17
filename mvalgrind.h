#ifndef MVALGRIND_H
#define MVALGRIND_H

#include "mudlle.h"

#ifdef HAVE_VALGRIND_MEMCHECK_H
  #include <valgrind/memcheck.h>

  #if __VALGRIND_MAJOR__ == 3 && __VALGRIND_MINOR__ == 6
    /* get rid of unused-variable warnings */
    #undef VALGRIND_DISCARD_TRANSLATIONS
    #define VALGRIND_DISCARD_TRANSLATIONS(_qzz_addr,_qzz_len)           \
      {unsigned int _qzz_res UNUSED;                                    \
      VALGRIND_DO_CLIENT_REQUEST(_qzz_res, 0,                           \
                                  VG_USERREQ__DISCARD_TRANSLATIONS,     \
                                  _qzz_addr, _qzz_len, 0, 0, 0);        \
      }
    #undef VALGRIND_CREATE_MEMPOOL
    #define VALGRIND_CREATE_MEMPOOL(pool, rzB, is_zeroed)               \
      {unsigned int _qzz_res UNUSED;                                    \
       VALGRIND_DO_CLIENT_REQUEST(_qzz_res, 0,                          \
                                  VG_USERREQ__CREATE_MEMPOOL,           \
                                  pool, rzB, is_zeroed, 0, 0);          \
      }
    #undef VALGRIND_DESTROY_MEMPOOL
    #define VALGRIND_DESTROY_MEMPOOL(pool)                              \
      {unsigned int _qzz_res UNUSED;                                    \
       VALGRIND_DO_CLIENT_REQUEST(_qzz_res, 0,                          \
                                  VG_USERREQ__DESTROY_MEMPOOL,          \
                                  pool, 0, 0, 0, 0);                    \
      }
    #undef VALGRIND_MEMPOOL_ALLOC
    #define VALGRIND_MEMPOOL_ALLOC(pool, addr, size)                    \
      {unsigned int _qzz_res UNUSED;                                    \
       VALGRIND_DO_CLIENT_REQUEST(_qzz_res, 0,                          \
                                  VG_USERREQ__MEMPOOL_ALLOC,            \
                                  pool, addr, size, 0, 0);              \
      }
    #undef VALGRIND_MEMPOOL_CHANGE
    #define VALGRIND_MEMPOOL_CHANGE(pool, addrA, addrB, size)           \
      {unsigned int _qzz_res UNUSED;                                    \
       VALGRIND_DO_CLIENT_REQUEST(_qzz_res, 0,                          \
                                  VG_USERREQ__MEMPOOL_CHANGE,           \
                                  pool, addrA, addrB, size, 0);         \
      }
    #undef VALGRIND_MEMPOOL_FREE
    #define VALGRIND_MEMPOOL_FREE(pool, addr)                           \
       {unsigned int _qzz_res UNUSED;                                   \
        VALGRIND_DO_CLIENT_REQUEST(_qzz_res, 0,                         \
                                   VG_USERREQ__MEMPOOL_FREE,            \
                                   pool, addr, 0, 0, 0);                \
       }
  #endif

#else
  #define VALGRIND_CREATE_MEMPOOL(pool, rzB, is_zeroed) ((void)0)
  #define VALGRIND_DESTROY_MEMPOOL(pool) ((void)0)
  #define VALGRIND_DISCARD_TRANSLATIONS(_qzz_addr,_qzz_len) ((void)0)
  #define VALGRIND_MAKE_MEM_DEFINED(_qzz_addr,_qzz_len) ((void)0)
  #define VALGRIND_MAKE_MEM_NOACCESS(_qzz_addr,_qzz_len) ((void)0)
  #define VALGRIND_MAKE_MEM_UNDEFINED(_qzz_addr,_qzz_len) ((void)0)
  #define VALGRIND_MEMPOOL_ALLOC(pool, addr, size) ((void)0)
  #define VALGRIND_MEMPOOL_CHANGE(pool, addrA, addrB, size) ((void)0)
  #define VALGRIND_MEMPOOL_FREE(pool, addr) ((void)0)
#endif

#endif  /* !MVALGRIND_H */
