/*
 * Copyright (c) 1993-2004 David Gay and Gustav Hållberg
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

#ifndef MUDLLE_H
#define MUDLLE_H

/* Generally useful declarations for the mudlle (pronounced muddle)
   interpreter */


#include <assert.h>
#include "options.h"


#ifndef UNUSED
#ifdef __GNUC__
#define UNUSED __attribute__((unused))
#else
#define UNUSED
#endif
#endif

#ifdef USE_GMP
#include <gmp.h>
#else
typedef unsigned long mpz_t;
typedef char mp_limb_t;
#define mpz_init_set_str(m, s, n) 0
#define mpz_cmp(b1, b2) 1
#endif


#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif

#include "context.h"
#include "mudio.h"
#include "calloc.h"
#include "alloc.h"

extern int debug_level;

#define DEBUG(n, stmt) do { if (debug_level >= n) stmt; } while (0)

#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE 1
#endif

int load_file(const char *name, const char *nicename, int seclev, int reload);
int catch_load_file(const char *name, const char *nicename, int seclev, int reload);

#endif
