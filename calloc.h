/* $Log: calloc.h,v $
 * Revision 1.2  1993/03/29  09:23:39  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.3  1993/03/14  16:13:56  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.1  1992/12/27  21:40:57  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

/* $Log: calloc.h,v $
 * Revision 1.2  1993/03/29  09:23:39  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.3  1993/03/14  16:13:56  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.1  1992/12/27  21:40:57  un_mec
 * Mudlle source, without any Mume extensions.
 *
 * Revision 1.1  1992/02/20  17:52:45  gay_d
 * Initial revision
 * */

#ifndef CALLOC_H
#define CALLOC_H

/* Module: calloc
   Date: 14 Feb 92
   Purpose: Fast & easy memory allocator for the mudlle compiler
     Based on the concept of blocks: you allocate memory from a block,
       and can free the whole block at one go.
       Individual deallocations are not possible.
*/

typedef struct block **block_t;

block_t new_block(void);
/* Return: A new block from which to allocate some memory.
*/

void free_block(block_t b);
/* Effect: Free all memory allocated in block b.
*/

void *allocate(block_t b, unsigned long size);
/* Effects: Allocates size bytes from block b. This block is aligned
     correctly for all objects.
   Returns: A pointer to the start of the block.
   Note: In this implementation, 12 + average(size)/2 bytes will be wasted
     for every BLOCK_SIZE bytes allocated (see calloc.c).
     Also, size must be smaller than BLOCK_SIZE
*/

#endif
