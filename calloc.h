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

#ifndef CALLOC_H
#define CALLOC_H

/* Module: calloc
   Date: 14 Feb 92
   Purpose: Fast & easy memory allocator for the mudlle compiler
     Based on the concept of blocks: you allocate memory from a block,
       and can free the whole block at one go.
       Individual deallocations are not possible.
*/

struct alloc_block *new_block(void);
/* Return: A new block from which to allocate some memory.
*/

void free_block(struct alloc_block *b);
/* Effect: Free all memory allocated in block b.
*/

void *allocate(struct alloc_block *b, unsigned long size);
/* Effects: Allocates size bytes from block b. This block is aligned
     correctly for all objects.
   Returns: A pointer to the start of the block.
   Note: In this implementation, 12 + average(size)/2 bytes will be wasted
     for every BLOCK_SIZE bytes allocated (see calloc.c).
     Also, size must be smaller than BLOCK_SIZE
*/

const char *heap_allocate_string(struct alloc_block *heap, const char *s);

#endif
