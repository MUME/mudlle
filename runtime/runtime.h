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

#ifndef RUNTIME_RUNTIME_H
#define RUNTIME_RUNTIME_H

#include "../types.h"

void runtime_init(void);

void flag_check_failed(const struct prim_op *op, const char *flag);

#  define LVL_IMPLEMENTOR 1
#  define LVL_VALA 1
#  define CASSERT_SECLEVEL(seclevel)            \
  CASSERT_EXPR(seclevel == 0 || seclevel == 1)

void system_string_define(struct string *name, value val);
void system_write(struct string *name, value val);
/* Modifies: environment
   Requires: name not already exist in environment.
   Effects: Adds name to environment, with value val for the variable,
     as a 'define' of the system module or as a system-write variable.
*/

#define define_string_vector(name, vec, count) do {     \
  STATIC_STRING(define_name, name);                     \
  define_mstring_vector(GET_STATIC_STRING(define_name), \
                        (vec), (count));                \
} while (0)

struct vector *define_mstring_vector(struct string *name,
                                     const char *const *vec,
                                     int count);
void define_int_vector(struct string *name, const int *vec, int count);

void runtime_define(const struct prim_op *op);

#define TYPEIS(v, want) do {                    \
  if (!TYPE((v), want))                         \
    bad_type_error((v), type_ ## want);         \
} while (0)

/* get mudlle integer */
#define GETINT(v) (integerp(v)					\
		   ? intval(v)					\
		   : (bad_type_error(v, type_integer), 0L))

/* get mudlle integer; treat negative numbers as positive overflow */
#define GETUINT(v) (integerp(v)					\
		    ? uintval(v)				\
		   : (bad_type_error(v, type_integer), 0L))

/* get mudlle integer; throw error if v is not in [min, max] */
#define GETRANGE(v, min, max)                                   \
  (!integerp(v)                                                 \
   ? (bad_type_error(v, type_integer), 0L)                      \
   : ((intval(v) < (min) || intval(v) > (max))                  \
      ? (out_of_range_error(intval(v), (min), (max)), 0L)       \
      : intval(v)))

#ifdef MUDLLE_INTERRUPT
/* Effects: Causes a user_interrupt runtime error if user caused
   SIGINT or SIGQUIT */
void check_interrupt(void);
#endif

void mudlle_consts_init(void);

const struct prim_op *lookup_primitive(ulong adr);

#endif /* RUNTIME_RUNTIME_H */
