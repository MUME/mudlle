/* $Log: calloc.c,v $
 * Revision 1.3  1993/05/02  07:37:37  un_mec
 * Owl: New output (mudlle ports).
 *
 * Revision 1.2  1993/03/29  09:23:37  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.3  1993/03/14  16:13:53  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.1  1992/12/27  21:40:55  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

static char rcsid[] = "$Id: calloc.c,v 1.3 1993/05/02 07:37:37 un_mec Exp $";

#include <stdlib.h>
#include <stdio.h>
#ifdef MUME
#include <malloc.h>
#endif
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

#define ALIGN(x, n) ((x) + ((n) - 1) & ~((n) - 1))

#define BLOCK_SIZE 10000	/* Should probably be chosen w/ respect to
				   malloc implementation */

struct block
{
    char *pos;			/* Where next to allocate from */
    char *end;			/* End of block */
    struct block *previous;
    char data[BLOCK_SIZE];
};

static struct block *make_block(void)
{
    struct block *new = malloc(sizeof(struct block));

    if (!new)
    {
	fprintf(stderr, "No memory left\n");
	exit(1);
    }
    new->pos = new->data;
    new->end = new->data + BLOCK_SIZE;
    new->previous = 0;

    return new;
}

block_t new_block(void)
/* Return: A new block from which to allocate some memory.
*/
{
    block_t new = xmalloc(sizeof *new);

    *new = make_block();
    return new;
}

void free_block(block_t b)
/* Effect: Free all memory allocated in block b.
*/
{
    struct block *blk = *b;

    while (blk)
    {
	struct block *prev = blk->previous;

	free(blk);
	blk = prev;
    }
}

void *allocate(block_t b, unsigned long size)
/* Effects: Allocates size bytes from block b. The result is aligned
     correctly for all types.
   Returns: A pointer to the start of the block.
   Note: In this implementation, 12 + average(size)/2 bytes will be wasted
     for every BLOCK_SIZE bytes allocated.
*/
{
    struct block *blk = *b;
    void *result;

    /* This could depend on the machine */
    size = ALIGN(size, sizeof(long));

    result = blk->pos;
    if ((blk->pos += size) >= blk->end)
    {
	/* Block full, get new one */
	struct block *new = make_block();

	assert(size < BLOCK_SIZE);
	new->previous = blk;
	*b = new;
	result = new->pos;
	new->pos += size;
    }
    return result;
}
