/* $Log: valuelist.h,v $
 * Revision 1.3  1995/01/22  15:11:55  arda
 * Linux patches.
 *
 * Revision 1.2  1993/03/29  09:24:56  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.3  1993/03/14  16:15:27  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.1  1992/12/27  21:41:49  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

#ifndef VALUELIST_H
#define VALUELIST_H

#include "mvalues.h"

/* A list of constants */
typedef struct
{
  struct local_value *first, *last;
} valuelist;

struct local_value
{
  struct local_value *next, *prev;
  value value;
};

void addtail(valuelist *list, value value);

#define init_list(list) do { (list)->first = (list)->last = NULL; } while (0)

#endif
