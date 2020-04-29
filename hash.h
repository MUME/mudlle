#ifndef HASH_H
#define HASH_H

#include <stdlib.h>

unsigned long fold_hash(unsigned long code, int bits);

unsigned long symbol_nhash(const char *name, size_t len, int bits);
unsigned long symbol_7inhash(const char *name, size_t len, int bits);

unsigned int string_nhash(const char *s, size_t len);
unsigned int string_hash(const char *s);
unsigned int string_7hash(const char *s);

#endif  /* HASH_H */
