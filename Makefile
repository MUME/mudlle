# Makefile for x86/linux
# Alternative (commented out lines) are for sparc/solaris

# Copyright (c) 1993-1999 David Gay and Gustav Hållberg
# All rights reserved.
# 
# Permission to use, copy, modify, and distribute this software for any
# purpose, without fee, and without written agreement is hereby granted,
# provided that the above copyright notice and the following two paragraphs
# appear in all copies of this software.
# 
# IN NO EVENT SHALL DAVID GAY OR GUSTAV HALLBERG BE LIABLE TO ANY PARTY FOR
# DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
# OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF DAVID GAY OR
# GUSTAV HALLBERG HAVE BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# 
# DAVID GAY AND GUSTAV HALLBERG SPECIFICALLY DISCLAIM ANY WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
# FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS ON AN
# "AS IS" BASIS, AND DAVID GAY AND GUSTAV HALLBERG HAVE NO OBLIGATION TO
# PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.

OBJS=  compile.o env.o interpret.o objenv.o print.o table.o tree.o \
       types.o stack.o utils.o valuelist.o mparser.o lexer.o alloc.o global.o \
       calloc.o mudlle.o ports.o ins.o error.o mcompile.o \
       module.o call.o context.o x86builtins.o utils.charset.o
#      module.o call.o context.o builtins.o utils.charset.o

CC=gcc 
CFLAGS= -Wall -Wshadow -Wwrite-strings -Wnested-externs -I. -g -DUSE_READLINE -DUSE_GMP
#CFLAGS= -Wall -Wshadow -Wnested-externs -Wno-char-subscripts -Wno-unused -I. -g -munaligned-doubles
MAKEDEPEND=gcc -MM

mudlle: lexer.c tokens.h $(OBJS) runlib
	$(CC) -o mudlle $(OBJS) runtime/librun.a -lm -lreadline -ltermcap -lgmp -lpcre
#	$(CC) -o mudlle $(OBJS) runtime/librun.a -lm -lreadline

runlib: 
		(cd runtime; $(MAKE) "CC=$(CC)")

clean:
	rm -f *.o *.obj libmudlle.a lexer.c tokens.h mparser.c parser.output \
mudlle-functions mudlle-primitives
	(cd runtime; $(MAKE) clean)

veryclean: clean
	rm -f *~ runtime/*~ mudlle .depend runtime/.depend

tar: veryclean
	cd ..;tar cvf - mudlle | gzip >mudlle.tar.gz

lexer.c: lexer.l
	flex -F -8 lexer.l
	mv lex.yy.c lexer.c

tokens.h mparser.c: parser.y
	bison -dtv parser.y
	mv parser.tab.h tokens.h
	mv parser.tab.c mparser.c

builtins.o: builtins.s
	as  -o builtins.o -P builtins.s

x86builtins.o: x86builtins.S
	$(CC) -c x86builtins.S

dep depend .depend: mparser.c lexer.c
	$(MAKEDEPEND) $(CFLAGS) -I.. *.c > .depend
	(cd runtime; $(MAKE) depend)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

-include .depend
