#                                                              -*- makefile -*-

# Copyright (c) 1993-2012 David Gay and Gustav Hållberg
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

# disable built-in rules
.SUFFIXES:

USE_XML       := yes
USE_GMP       := yes
USE_READLINE  := yes
# USE_PCRE    	:= yes
# PCRE_HEADER 	:= PCRE_H	# for <pcre.h>
# PCRE_HEADER 	:= PCRE_PCRE_H	# for <pcre/pcre.h>
# USE_MINGW     := yes
# USE_LPSOLVE   := yes
# LPSOLVE_HEADER := LPKIT_LPKIT_H     # for <lpkit/lpkit.h>
# LPSOLVE_HEADER := LP_SOLVE_LPKIT_H  # for <lp_solve/lpkit.h>

export USE_XML USE_GMP USE_READLINE USE_PCRE USE_MINGW USE_LPSOLVE

PRIMITIVE_CFLAGS := -mstackrealign
export PRIMITIVE_CFLAGS

ifeq ($(shell uname -m),sun4u)
BUILTINS=builtins.o
else
BUILTINS=x86builtins.o
endif

OBJS= $(BUILTINS) alloc.o call.o calloc.o charset.o compile.o	\
	context.o env.o error.o global.o ins.o interpret.o	\
	lexer.o mcompile.o module.o mudlle-main.o mudlle.o		\
	objenv.o parser.o ports.o print.o stack.o strbuf.o	\
	table.o tree.o types.o utils.o valuelist.o

alloc.o error.o: CFLAGS+=$(PRIMITIVE_CFLAGS)

SRC = $(filter-out x86builtins.c, $(OBJS:%.o=%.c))

CC=gcc
CFLAGS := -g -std=gnu99 -O0 -Wall -Wshadow -Wwrite-strings	\
          -Wnested-externs -Wunused
CPPFLAGS := -m32
LDFLAGS := -m32 -fno-pie
LIBS := -lm
ifeq ($(shell uname -s),Darwin)
CPPFLAGS += -I/opt/local/include
LDFLAGS += -L/opt/local/lib
LIBS += -liconv
endif
MAKEDEPEND=gcc -MM

export CC CFLAGS CPPFLAGS LDFLAGS MAKEDEPEND

ifneq ($(USE_XML),)
CPPFLAGS  += -DUSE_XML
LIBS += -lxml2
endif

ifneq ($(USE_GMP),)
CPPFLAGS += -DUSE_GMP
LIBS += -lgmp
endif

ifneq ($(USE_READLINE),)
CPPFLAGS += -DUSE_READLINE
LIBS += -lreadline -lcurses
endif

ifneq ($(USE_PCRE),)
CPPFLAGS += -DUSE_PCRE -DHAVE_$(PCRE_HEADER)
LIBS += -lpcre
endif

ifneq ($(USE_LPSOLVE),)
CPPFLAGS += -DHAVE_LIB_LPK -DHAVE_$(LPSOLVE_HEADER)
LIBS += -llpk -lfl -lm
endif

ifneq ($(USE_MINGW),)
LIBS += -lwsock32
endif

LIBRUN := runtime/librun.a

all: mudlle

mudlle: $(OBJS) $(LIBRUN)
	$(CC) $(LDFLAGS) -o $@ $^ $(LIBS)

profiler: profiler.o
	$(CC) $(LDFLAGS) $(CPPFLAGS) -o $@ $<

puremud: $(OBJS) $(LIBRUN)
	purify -cache-dir=/tmp $(CC) -o puremud $^ -lm

.PHONY: $(LIBRUN)
$(LIBRUN):
	$(MAKE) -C $(dir $@) -f Makefile $(notdir $@)

.PHONY: clean depclean
depclean:
	$(MAKE) -C runtime -f Makefile $@
	rm -f .depend

clean:
	$(MAKE) -C runtime -f Makefile $@
	rm -f *.o lexer.c tokens.h parser.c .depend

%.o: %.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -o $@ -c $<

lexer.o: tokens.h

lexer.c: lexer.l
	flex -F -8 lexer.l
	mv lex.yy.c lexer.c

tokens.h: parser.c
parser.c: parser.y
	bison -dtv parser.y
	mv parser.tab.h tokens.h
	perl -pi -e 's!parser\.tab\.h!tokens\.h!g' tokens.h
	mv parser.tab.c parser.c
	perl -pi -e 's!parser\.tab\.c!parser\.c!g' parser.c

builtins.o: builtins.S
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

x86builtins.o: x86builtins.S x86consts.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

x86consts.h: genconst
	./genconst > $@

genconst: genconst.o Makefile
	$(CC) $(LDFLAGS) -o $@ $<

genconst.o: genconstdefs.h

CONSTH := types.h mvalues.h context.h error.h

genconstdefs.h: $(CONSTH) runtime/consts.pl Makefile
	perl runtime/consts.pl $(CONSTH) | grep '^ *\(/\*\|DEF\)' \
		| sed 's/,$$/;/g' > $@

.PHONY: dep depend
dep depend: .depend
	$(MAKE) -C runtime -f Makefile depend

.depend: $(SRC) genconst.c genconstdefs.h
	$(MAKEDEPEND) $(CPPFLAGS) $(CFLAGS) $(SRC) genconst.c \
		| sed 's/\.o *:/.o:/g' > .depend

compiler: build-compiler.sh mudlle
	/bin/sh $<

install: install-compiler.sh compiler
	/bin/sh $< $(IDIR)

depfile:=.depend

# include dependency files unless we are only running cleaning targets
ifneq (,$(MAKECMDGOALS))
ifeq ($(MAKECMDGOALS),$(filter clean depclean,$(MAKECMDGOALS)))
depfile:=
endif
endif

-include $(depfile)
