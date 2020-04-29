#include "mudlle-config.h"

#  include <assert.h>
#include <limits.h>

#include "charset.h"
#include "hash.h"
#include "mudlle-macro.h"
#include "mvalues.h"

/* FNV-1a hash from http://tools.ietf.org/html/draft-eastlake-fnv-03 */
#define FNV_PRIME (sizeof (long) == 4           \
                   ? 0x01000193L                \
                   : 0x00000100000001b3L)
#define FNV_OFFSET (sizeof (long) == 4          \
                    ? 0x811c9dc5L               \
                    : 0xcbf29ce484222325L)

CASSERT(sizeof (long) == 4 || sizeof (long) == 8);

ulong fold_hash(ulong code, int bits)
{
  if (bits == 0)
    return 0;
  if (bits == CHAR_BIT * sizeof code)
    return code;
  assert(bits > 0 && bits < CHAR_BIT * sizeof code);
  code ^= code >> bits;
  ulong mask = ~0UL >> (CHAR_BIT * sizeof (ulong) - bits);
  return code & mask;
}

/* case- and accentuation-insensitive */
ulong symbol_7inhash(const char *name, size_t len, int bits)
{
  ulong code = FNV_OFFSET;
  while (len--)
    {
      unsigned char c = *name++;
      code ^= TO_7LOWER(c);
      code *= FNV_PRIME;
    }
  return fold_hash(code, bits);
}

/* case- and accentuation-sensitive */
ulong symbol_nhash(const char *name, size_t len, int bits)
{
  ulong code = FNV_OFFSET;
  while (len--)
    {
      unsigned char c = *name++;
      code ^= c;
      code *= FNV_PRIME;
    }
  return fold_hash(code, bits);
}

unsigned int string_nhash(const char *s, size_t len)
{
  return symbol_nhash(s, len, TAGGED_INT_BITS - 1);
}

unsigned int string_hash(const char *s)
{
  return string_nhash(s, strlen(s));
}

unsigned int string_7hash(const char *s)
{
  return symbol_7inhash(s, strlen(s), TAGGED_INT_BITS - 1);
}
