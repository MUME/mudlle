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

OBJS=  basic.o arith.o bool.o symbol.o runtime.o io.o string.o	 \
       list.o vector.o support.o bitset.o debug.o files.o float.o \
       bigint.o pattern.o mudllecst.o

SRC = $(OBJS:%.o=%.c)

CFLAGS +=-fno-optimize-sibling-calls -I.. -DHAVE_PCRE_PCRE_H

CC=gcc
MAKEDEPEND=gcc -MM

CSTFILES = files.h

all: librun.a

.PHONY: clean
clean:
	rm -f *.o librun.a mudllecst.c .depend

librun.a: $(OBJS)
	if [ -f librun.a ]; then rm -f librun.a; fi
	ar r librun.a $(OBJS)
	-ranlib librun.a

mudllecst.c: consts.pl $(CSTFILES)
	perl $^ > $@

%.o: %.c
	$(CC) $(CFLAGS) -o $@ -c $<


.PHONY: dep depend
dep depend: .depend

.depend: $(SRC)
	$(MAKEDEPEND) $(CFLAGS) $(SRC)> .depend

lsimple.c: simple.l
	flex -F -Psimple simple.l
	mv lex.simple.c lsimple.c

psimple.c: psimple.h
psimple.h: simple.y
	bison -b psimple -d -p simple -t -v simple.y
	mv psimple.tab.c psimple.c
	mv psimple.tab.h psimple.h

-include .depend
