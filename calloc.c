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

#include "mudlle-config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "alloc.h"
#include "calloc.h"
#include "mvalgrind.h"
#include "utils.h"

/* Module: calloc
   Date: 14 Feb 92
   Purpose: Fast & easy memory allocator for the mudlle compiler
   Based on the concept of blocks: you allocate memory from a block,
   and can free the whole block at one go.
   Individual deallocations are not possible.
*/

struct memblock
{
  struct memblock *previous;
  char *pos;			/* Where next to allocate from */
  char *end;			/* End of block */
  char data[];
};

struct alloc_block {
  struct memblock *memblock;
};

#define DFLT_BLOCK_SIZE (16384 - sizeof (struct memblock))

static struct memblock *make_block(size_t size)
{
  if (size < DFLT_BLOCK_SIZE)
    size = DFLT_BLOCK_SIZE;
  struct memblock *newp = malloc(sizeof *newp + size);
  if (newp == NULL)
    {
      fprintf(stderr, "No memory left\n");
      abort();
    }

  VALGRIND_CREATE_MEMPOOL(newp, 0, 0);
  VALGRIND_MAKE_MEM_NOACCESS(newp->data, size);

  newp->pos = newp->data;
  newp->end = newp->data + size;
  newp->previous = 0;

  return newp;
}

struct alloc_block *new_block(void)
/* Return: A new block from which to allocate some memory.
 */
{
  struct alloc_block *newp = xmalloc(sizeof *newp);
  newp->memblock = make_block(DFLT_BLOCK_SIZE);
  return newp;
}

void free_block(struct alloc_block *b)
/* Effect: Free all memory allocated in block b.
 */
{
  struct memblock *blk = b->memblock;

  while (blk)
    {
      struct memblock *prev = blk->previous;
      VALGRIND_DESTROY_MEMPOOL(blk);
      free(blk);
      blk = prev;
    }
  free(b);
}

void *allocate(struct alloc_block *b, unsigned long size)
/* Effects: Allocates size bytes from block b. The result is aligned
   correctly for all types.
   Returns: A pointer to the start of the block.
   Note: In this implementation, 12 + average(size)/2 bytes will be wasted
   for every BLOCK_SIZE bytes allocated.
*/
{
  if (size == 0)
    return NULL;

  struct memblock *blk = b->memblock;

  /* This could depend on the machine */
  unsigned long asize = MUDLLE_ALIGN(size, sizeof (long));

  void *result = blk->pos;
  blk->pos += asize;
  if (blk->pos > blk->end)
    {
      /* Block full, get new one */
      struct memblock *newp = make_block(asize);
      newp->previous = blk;
      b->memblock = newp;
      result = newp->pos;
      newp->pos += asize;
      blk = newp;
    }
  VALGRIND_MEMPOOL_ALLOC(blk, result, size);
  return result;
}

const char *heap_allocate_string(struct alloc_block *heap, const char *s)
{
  size_t n = strlen(s) + 1;
  char *r = allocate(heap, n);
  memcpy(r, s, n);
  return r;
}
