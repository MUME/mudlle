#                                                              -*- makefile -*-

# Copyright (c) 1993-2012 David Gay and Gustav H�llberg
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
# USE_VALGRIND  := yes

# USE_CPP_STEP := t

export USE_XML USE_GMP USE_READLINE USE_PCRE USE_MINGW USE_LPSOLVE USE_CPP_STEP

PRIMITIVE_CFLAGS := -mstackrealign
export PRIMITIVE_CFLAGS

ARCH := i386
ifeq ($(ARCH),x86_64)
override ARCH := amd64
endif

ifneq ($(filter i386 amd64,$(ARCH)),$(ARCH))
$(error Unsupported ARCH=$(ARCH); expected i386 or amd64)
endif

ifeq ($(ARCH),i386)
BUILTINS := x86builtins.o
BUILTINDEPS := x86consts.h
ARCHFLAG := -m32
INSTALLFLAGS := -y
INSTALLDEPS := compiler
else
ifeq ($(ARCH),amd64)
ARCHFLAG := -m64
INSTALLFLAGS := -y -c
INSTALLDEPS := 
endif
endif

OBJS= $(BUILTINS) alloc.o call.o calloc.o charset.o compile.o	\
	context.o env.o error.o global.o ins.o interpret.o	\
	lexer.o mcompile.o module.o mudlle-main.o mudlle.o		\
	objenv.o parser.tab.o ports.o print.o stack.o strbuf.o	\
	table.o tree.o types.o utils.o

alloc.o error.o: CFLAGS+=$(PRIMITIVE_CFLAGS)

SRC = $(filter-out x86builtins.c, $(OBJS:%.o=%.c))

CC=gcc
CFLAGS := -g3 -std=gnu99 -O2 -Wall -Wshadow -Wwrite-strings	\
          -Wnested-externs -Wunused-macros
CPPFLAGS := $(ARCHFLAG)
LDFLAGS := $(ARCHFLAG) -fno-pie
LIBS := -lm
MAKEDEPEND=$(CC) -MM
PERL:=perl

ifeq ($(shell uname -s),Darwin)
CPPFLAGS += -isystem /opt/local/include
LDFLAGS += -L/opt/local/lib
LIBS += -liconv
endif # Darwin

export CC CFLAGS CPPFLAGS LDFLAGS MAKEDEPEND PERL

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

ifneq ($(USE_VALGRIND),)
CPPFLAGS += -DHAVE_VALGRIND_MEMCHECK_H
endif

ifneq ($(USE_MINGW),)
LIBS += -lwsock32
endif

clang:=$(shell : | $(CC) $(CPPFLAGS) $(CFLAGS) -E -dM - \
	| grep '^\#define __clang__\b')
ifneq ($(clang),)
NO_UNUSED_MACROS:=x86builtins.o
charset.o: CFLAGS:=-Wno-invalid-source-encoding $(CFLAGS)
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
	rm -f *.o lexer.c tokens.h parser.c .depend genconst \
		genconstdefs.h mudlle parser.output x86consts.h

ifeq (,$(USE_CPP_STEP))
%.o: %.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -o $@ -c $<
else
%.o: %-mpp.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -o $@ -c $<

.PRECIOUS: %-mpp.c
%-mpp.c: %.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -E $< | grep -v '^#' | indent > $@
endif

.depend parser.tab.o lexer.o $(NO_UNUSED_MACROS): \
	CFLAGS:=$(filter-out -Wunused-macros,$(CFLAGS))

lexer.o: parser.tab.h

lexer.c: lexer.l
	flex -CFe -8 -o $@ $<

%.tab.c %.tab.h: %.y
	bison -dtv $<

x86builtins.o: x86builtins.S $(BUILTINDEPS)
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

x86consts.h: genconst Makefile
	./genconst > $@

genconst: genconst.o Makefile
	$(CC) $(LDFLAGS) -o $@ $<

genconst.o: genconstdefs.h

CONSTH := types.h mvalues.h context.h error.h

genconstdefs.h: runtime/consts.pl $(CONSTH) Makefile
	$(PERL) $< -d -o $@ -- $(CONSTH)

.PHONY: dep depend
dep depend: .depend
	$(MAKE) -C runtime -f Makefile depend

.depend: $(SRC) genconst.c genconstdefs.h $(BUILTINS:%.o=%.S) $(BUILTINDEPS)
	$(MAKEDEPEND) $(CPPFLAGS) $(CFLAGS) $(filter-out %.h,$^) \
		| sed 's/\.o *:/.o:/g' > .depend

# Currently 22 files, split into at most 8 groups for parallel builds
GROUPS=0 1 2 3 4 5 6 7

define BUILDER
.PHONY: comp_$(1)_$(3)
comp_$(1)_$(3): build-slice.sh $(4)
	@echo "Compiling $(2) compiler files $(3)/$(words $(GROUPS))"
	@/bin/sh $$< $(2) $(3) $(words $(GROUPS))
endef

define PASS
$$(foreach g,$$(GROUPS),$$(eval $$(call BUILDER,$(1),$(2),$$(g),$(3))))

.PHONY: comp_$(1)
comp_$(1): $$(foreach g,$$(GROUPS),comp_$(1)_$$(g))

endef

$(eval $(call PASS,xc,xc,mudlle))
$(eval $(call PASS,icxc,icxc,comp_xc))
$(eval $(call PASS,icxc2,icxc,comp_icxc))

.PHONY: compiler
compiler: comp_icxc2

.PHONY: install
install: install-compiler.sh $(INSTALLDEPS)
	/bin/sh $< $(INSTALLFLAGS) $(IDIR)

depfile:=.depend

# include dependency files unless we are only running cleaning targets
ifneq (,$(MAKECMDGOALS))
ifeq ($(MAKECMDGOALS),$(filter clean depclean,$(MAKECMDGOALS)))
depfile:=
endif
endif

-include $(depfile)
