#                                                              -*- makefile -*-

# Copyright (c) 1993-2004 David Gay and Gustav Hållberg
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

ifeq ($(shell uname -m),sun4u)
BUILTINS=builtins.o
else
BUILTINS=x86builtins.o
endif

OBJS= compile.o env.o interpret.o objenv.o print.o table.o	\
	tree.o types.o stack.o utils.o valuelist.o parser.o	\
	lexer.o alloc.o global.o calloc.o mudlle.o ports.o	\
	ins.o error.o mcompile.o module.o call.o context.o	\
	$(BUILTINS) utils.charset.o

SRC = $(filter-out x86builtins.c, $(OBJS:%.o=%.c))

CC=gcc 
CFLAGS= -g -O0 -Wall -Wshadow -Wwrite-strings -Wnested-externs	\
	-Wunused -I. -DUSE_READLINE -DUSE_GMP
export CC
export CFLAGS
MAKEDEPEND=gcc -MM

mudlle: lexer.c tokens.h $(OBJS) runlib
	$(CC) -g -Wall -o mudlle $(OBJS) runtime/librun.a -lgmp -lpcre -lm -lreadline -lcurses

puremud: $(OBJS) runlib
	purify -cache-dir=/tmp $(CC) -o puremud $(OBJS) runtime/librun.a -lm

runlib: 
	$(MAKE) -C runtime -f Makefile

.PHONY: clean
clean:
	$(MAKE) -C runtime -f Makefile clean
	rm -f *.o lexer.c tokens.h parser.c .depend

mytar:
	tar cvf - *.[chylsa] *.mud smakefile.base Makefile.base Makefile SCOPTIONS runtime/*.[ch] runtime/smakefile runtime/SCOPTIONS runtime/Makefile.base runtime/Makefile doc/*.doc | gzip >mudlle.tar.gz

tar:
	tar cvf mudlle.tar Makefile *.[chylsa] *.mud smakefile.base SCOPTIONS \
runtime/Makefile runtime/arith.[ch] runtime/basic.[ch] runtime/bool.[ch] \
runtime/list.[ch] runtime/runtime.[ch] runtime/SCOPTIONS compile \
runtime/smakefile runtime/string.c runtime/stringops.h runtime/files.[ch] \
runtime/symbol.[ch] runtime/symbol.h runtime/vector.[ch] doc/mudlle-ref.doc \
doc/mudlle-intro.doc runtime/support.[ch] runtime/bitset.[ch] runtime/io.[ch] \
runtime/debug.[ch] runtime/float.[ch]


%.o: %.c
	$(CC) $(CFLAGS) -o $@ -c $<

lexer.c: lexer.l
	flex -F -8 lexer.l
	mv lex.yy.c lexer.c

tokens.h: parser.c
parser.c: parser.y
	bison -dtv parser.y
	mv parser.tab.h tokens.h
	mv parser.tab.c parser.c

builtins.o: builtins.S
	$(CC) -o builtins.o -c builtins.S

x86builtins.o: x86builtins.S
	$(CC) -c x86builtins.S -o x86builtins.o

.PHONY: dep depend
dep depend: .depend
	$(MAKE) -C runtime -f Makefile depend

.depend: $(SRC)
	$(MAKEDEPEND) $(CFLAGS) $(SRC) > .depend

compiler:
	/bin/sh install-compiler

-include .depend
