/* $Log: lexer.h,v $
 * Revision 1.3  1994/10/09  06:42:21  arda
 * Libraries
 * Type inference
 * Many minor improvements
 *
 * Revision 1.2  1994/01/29  19:50:29  dgay
 * Owl: add file & line information to functions.
 *
 * Revision 1.1  1993/03/29  09:24:07  un_mec
 * Owl: Changed descriptor I/O
 *      New interpreter / compiler structure.
 *
 * Revision 1.3  1993/03/14  16:14:26  dgay
 * Optimised stack & gc ops.
 * */

#ifndef LEXER_H
#define LEXER_H

extern YYSTYPE yylval;

extern int lineno;
extern const char *filename;

int yylex(void);

void read_from_string(const char *str);
void read_from_file(FILE *f, const char *name);

#endif
