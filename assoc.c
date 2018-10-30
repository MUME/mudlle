#include "mudlle-config.h"

#include <assert.h>
#include <stdlib.h>

#include "assoc.h"
#include "charset.h"
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
  unsigned bucket = hash & (assoc->size - 1);
  for (struct assoc_array_node *node = assoc->nodes[bucket];
       node;
       node = node->next)
    if (node->hash == hash && assoc->type->cmp(key, node->key) == 0)
      return node;
  return NULL;
}

void *assoc_array_lookup(const struct assoc_array *assoc, const void *key)
{
  struct assoc_array_node *node = assoc_array_lookup_node(assoc, key);
  return node ? node->data : NULL;
}

static void rehash_assoc_array(struct assoc_array *assoc, int size)
{
  assert(size && (size & (size - 1)) == 0);

  struct assoc_array_node **nodes = calloc(size, sizeof *nodes);

  for (int i = 0; i < assoc->size; ++i)
    for (struct assoc_array_node *node = assoc->nodes[i], *next;
         node;
         node = next)
      {
        next = node->next;
        unsigned hash = node->hash;
        unsigned bucket = hash & (size - 1);
        node->next = nodes[bucket];
        nodes[bucket] = node;
      }

  free(assoc->nodes);
  assoc->nodes = nodes;
  assoc->size = size;
}

/* must know that "key" isn't in the table */
static void assoc_array_add(struct assoc_array *assoc,
                            const void *key, void *data)
{
  unsigned hash = assoc->type->hash(key);
  unsigned bucket = hash & (assoc->size - 1);

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

  if (assoc->used * 3 >= assoc->size * 2)
    rehash_assoc_array(assoc, assoc->size ? assoc->size * 2 : 16);

  assoc_array_add(assoc, key, data);
}

/* Returns true if the key was found */
int assoc_array_remove(struct assoc_array *assoc, const void *key)
{
  if (assoc->used == 0) return false;

  unsigned hash = assoc->type->hash(key);
  unsigned bucket = hash & (assoc->size - 1);

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
  for (int i = 0; i < assoc->size; ++i)
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
  for (int i = 0; i < assoc->size; ++i)
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
  assoc->used = assoc->size = 0;
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
  return case_symbol_hash_len(s, len, MAX_TAGGED_INT + 1);
}

const struct assoc_array_type const_charp_to_voidp_assoc_array_type = {
  .cmp       = assoc_array_cmp_string,
  .hash      = assoc_array_hash_string
};
