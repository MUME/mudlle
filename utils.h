/* $Log: utils.h,v $
 * Revision 1.7  1994/10/09  06:43:12  arda
 * Libraries
 * Type inference
 * Many minor improvements
 *
 * Revision 1.6  1994/02/11  09:59:35  dgay
 * Owl: -Wall
 *      new shared string handling
 *      configuration file
 *
 * Revision 1.5  1993/12/06  19:20:57  arda
 * divers CLI
 *
 * Revision 1.4  1993/08/15  21:00:35  un_mec
 * Owl: Overload [].
 *      Added xcalloc, xrealloc.
 *
 * Revision 1.3  1993/06/20  13:35:25  un_mec
 * Owl: edit protocol bug fix.
 *
 * Revision 1.2  1993/03/29  09:24:52  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.3  1993/03/14  16:15:21  dgay
 * Optimised stack & gc ops.
 *
 * Revision 1.1  1992/12/27  21:41:47  un_mec
 * Mudlle source, without any Mume extensions.
 *
 */

#ifndef UTILS_H
#define UTILS_H

extern int erred;

#ifdef DEBUG_MEMORY
void *debug_xmalloc(const char *file, int line, int size);
void *debug_xcalloc(const char *file, int line, int number, int size);
void *debug_xrealloc(const char *file, int line, void *old, int size);
char *debug_xstrdup(const char *file, int line, const char *s);
#else
void *xmalloc(int size);
void *xcalloc(int number, int size);
void *xrealloc(void *old, int size);
char *xstrdup(const char *s);
#endif

char *strlwr(char *s);
void error(const char *msg, ...);
void warning(const char *msg, ...);

#ifndef HAVE_MEMMOVE
void memmove(char *to, const char *from, int n);
#endif

/* The only standard tag ... */
#define TAG_END -1

#endif
