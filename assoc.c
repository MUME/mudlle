#include "mudlle-config.h"

#  include <assert.h>
#include <stdlib.h>

#include "assoc.h"
#include "charset.h"
#include "hash.h"
#include "table.h"

struct assoc_array_node {
  struct assoc_array_node *next;
  unsigned hash;
  const void *key;
  void *data;
};

/* returns pointer to node, or NULL if not found */
static struct assoc_array_node *assoc_array_lookup_node(
  const struct assoc_array *assoc, const void *key)
{
  if (assoc->used == 0) return NULL;

  unsigned hash = assoc->type->hash(key);
  unsigned bucket = fold_hash(hash, assoc->size_bits);
  for (struct assoc_array_node *node = assoc->nodes[bucket];
       node;
       node = node->next)
    if (node->hash == hash && assoc->type->cmp(key, node->key) == 0)
      return node;
  return NULL;
}

static inline int bits_size(int bits)
{
  if (bits <= 0)                /* bits = 0 is special-case */
    return 0;
  return P(bits);
}

void *assoc_array_lookup(const struct assoc_array *assoc, const void *key)
{
  struct assoc_array_node *node = assoc_array_lookup_node(assoc, key);
  return node ? node->data : NULL;
}

void **assoc_array_lookup_ref(const struct assoc_array *assoc, const void *key)
{
  struct assoc_array_node *node = assoc_array_lookup_node(assoc, key);
  return node ? &node->data : NULL;
}

static void rehash_assoc_array(struct assoc_array *assoc, int size_bits)
{
  int size = bits_size(size_bits);
  assert(size > 0);
  struct assoc_array_node **nodes = calloc(size, sizeof *nodes);

  int old_size = bits_size(assoc->size_bits);
  for (int i = 0; i < old_size; ++i)
    for (struct assoc_array_node *node = assoc->nodes[i], *next;
         node;
         node = next)
      {
        next = node->next;
        unsigned hash = node->hash;
        unsigned bucket = fold_hash(hash, size_bits);
        node->next = nodes[bucket];
        nodes[bucket] = node;
      }

  free(assoc->nodes);
  assoc->nodes = nodes;
  assoc->size_bits = size_bits;
}

/* must know that "key" isn't in the table */
static void assoc_array_add(struct assoc_array *assoc,
                            const void *key, void *data)
{
  unsigned hash = assoc->type->hash(key);
  unsigned bucket = fold_hash(hash, assoc->size_bits);

  struct assoc_array_node *node = malloc(sizeof *node);
  *node = (struct assoc_array_node){
    .next = assoc->nodes[bucket],
    .hash = hash,
    .key  = key,
    .data = data
  };
  assoc->nodes[bucket] = node;

  ++assoc->used;
}

void assoc_array_set(struct assoc_array *assoc,
                     const void *key, void *data)
{
  struct assoc_array_node *node = assoc_array_lookup_node(assoc, key);
  if (node)
    {
      if (assoc->type->free_data)
        assoc->type->free_data(node->data);
      node->data = data;
      return;
    }

  if (assoc->used * 3 >= bits_size(assoc->size_bits) * 2)
    rehash_assoc_array(assoc, assoc->size_bits ? assoc->size_bits + 1 : 4);

  assoc_array_add(assoc, key, data);
}

/* Returns true if the key was found */
int assoc_array_remove(struct assoc_array *assoc, const void *key)
{
  if (assoc->used == 0) return false;

  unsigned hash = assoc->type->hash(key);
  unsigned bucket = fold_hash(hash, assoc->size_bits);

  for (struct assoc_array_node **nodep = &assoc->nodes[bucket];
       *nodep;
       nodep = &(*nodep)->next)
    {
      struct assoc_array_node *node = *nodep;
      if (node->hash != hash || assoc->type->cmp(key, node->key) != 0)
        continue;
      *nodep = node->next;

      if (assoc->type->free_key)
        assoc->type->free_key((void *)node->key);
      if (assoc->type->free_data)
        assoc->type->free_data(node->data);
      free(node);
      --assoc->used;
      return true;
    }
  return false;
}

bool assoc_array_exists(struct assoc_array *assoc,
			bool (*f)(const void *key, void *data, void *idata),
			void *idata)
{
  int size = bits_size(assoc->size_bits);
  for (int i = 0; i < size; ++i)
    for (struct assoc_array_node *node = assoc->nodes[i], *next;
         node;
         node = next)
      {
        /* save 'next' so it's safe to delete the _current_ entry
           while looping */
        next = node->next;
        if (f(node->key, node->data, idata))
	  return true;
      }
  return false;
}

void assoc_array_free(struct assoc_array *assoc)
{
  int size = bits_size(assoc->size_bits);
  for (int i = 0; i < size; ++i)
    for (struct assoc_array_node *node = assoc->nodes[i], *next;
         node;
         node = next)
      {
        next = node->next;
        if (assoc->type->free_key)
          assoc->type->free_key((void *)node->key);
        if (assoc->type->free_data)
          assoc->type->free_data(node->data);
        free(node);
      }
  free(assoc->nodes);
  assoc->used = assoc->size_bits = 0;
  assoc->nodes = NULL;
}

static int assoc_array_cmp_string(const void *a, const void *b)
{
  return strcmp(a, b);
}

static unsigned assoc_array_hash_string(const void *k)
{
  const char *s = k;
  size_t len = strlen(s);
  return symbol_nhash(s, len, TAGGED_INT_BITS - 1);
}

const struct assoc_array_type const_charp_to_voidp_assoc_array_type = {
  .cmp       = assoc_array_cmp_string,
  .hash      = assoc_array_hash_string
};

static void assoc_array_freep(void *p)
{
  free(p);
}

static int assoc_array_cmp_long(const void *_a, const void *_b)
{
  long a = (long)_a, b = (long)_b;
  return a < b ? -1 : a > b;
}

static unsigned assoc_array_hash_long(const void *_k)
{
  long l = (long)_k;
  return symbol_nhash((const char *)&l, sizeof l,
                      CHAR_BIT * sizeof (unsigned));
}

const struct assoc_array_type long_to_mallocp_assoc_array_type = {
  .cmp       = assoc_array_cmp_long,
  .hash      = assoc_array_hash_long,
  .free_data = assoc_array_freep
};

const struct assoc_array_type long_to_voidp_assoc_array_type = {
  .cmp       = assoc_array_cmp_long,
  .hash      = assoc_array_hash_long,
};

static unsigned assoc_array_hash_istring(const void *k)
{
  return string_7hash(k);
}

static int assoc_array_cmp_istring(const void *a, const void *b)
{
  return str7icmp(a, b);
}

const struct assoc_array_type const_icharp_to_voidp_assoc_array_type = {
  .cmp       = assoc_array_cmp_istring,
  .hash      = assoc_array_hash_istring
};
