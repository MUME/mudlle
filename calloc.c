/*
 * Copyright (c) 1993-1999 David Gay and Gustav Hållberg
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

#include <stdlib.h>
#include <stdio.h>
#include "mudlle.h"
#include "calloc.h"
#include "utils.h"

/* Module: calloc
   Date: 14 Feb 92
   Purpose: Fast & easy memory allocator for the mudlle compiler
     Based on the concept of blocks: you allocate memory from a block,
       and can free the whole block at one go.
       Individual deallocations are not possible.
*/

#define BLOCK_SIZE 10000	/* Should probably be chosen w/ respect to
				   malloc implementation */

struct memblock
{
    char *pos;			/* Where next to allocate from */
    char *end;			/* End of block */
    struct memblock *previous;
    char data[BLOCK_SIZE];
};

static struct memblock *make_block(void)
{
    struct memblock *newp = malloc(sizeof(struct memblock));

    if (!newp)
    {
	fprintf(stderr, "No memory left\n");
	exit(1);
    }
    newp->pos = newp->data;
    newp->end = newp->data + BLOCK_SIZE;
    newp->previous = 0;

    return newp;
}

block_t new_block(void)
/* Return: A new block from which to allocate some memory.
*/
{
    block_t newp = xmalloc(sizeof *newp);

    *newp = make_block();
    return newp;
}

void free_block(block_t b)
/* Effect: Free all memory allocated in block b.
*/
{
#if 1
    struct memblock *blk = *b;

    while (blk)
    {
	struct memblock *prev = blk->previous;

	free(blk);
	blk = prev;
    }
    free(b); /* A good idea, 9 years late. */
#endif
}

void *allocate(block_t b, unsigned long size)
/* Effects: Allocates size bytes from block b. The result is aligned
     correctly for all types.
   Returns: A pointer to the start of the block.
   Note: In this implementation, 12 + average(size)/2 bytes will be wasted
     for every BLOCK_SIZE bytes allocated.
*/
{
    struct memblock *blk = *b;
    void *result;

    /* This could depend on the machine */
    size = ALIGN(size, sizeof(long));

    result = blk->pos;
    if ((blk->pos += size) >= blk->end)
    {
	/* Block full, get new one */
	struct memblock *newp = make_block();

	assert(size < BLOCK_SIZE);
	newp->previous = blk;
	*b = newp;
	result = newp->pos;
	newp->pos += size;
    }
    return result;
}
