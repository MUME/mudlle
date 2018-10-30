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
# USE_LPSOLVE   := yes
# LPSOLVE_HEADER := LPKIT_LPKIT_H     # for <lpkit/lpkit.h>
# LPSOLVE_HEADER := LP_SOLVE_LPKIT_H  # for <lp_solve/lpkit.h>
# USE_VALGRIND  := yes

# USE_CPP_STEP := t

ifeq ($(verbose),yes)
  Q :=
else
  Q := @
endif

ifeq ($(optimize),no)
  NO_OPT := t
endif

ARCH := i386
ifneq ($(filter x86 i386 i686,$(ARCH)),)
override ARCH := i386
endif

ifneq ($(filter x64-64 x86_64 x64,$(ARCH)),)
override ARCH := amd64
endif


ifeq ($(filter i386 amd64,$(ARCH)),)
$(error Unsupported ARCH=$(ARCH); expected i386 or amd64)
endif

ifeq ($(ARCH),i386)
PRIMITIVE_CFLAGS := -mstackrealign
BUILTINS := x86builtins.o
BUILTINDEPS := x86consts.h
ARCHFLAG := -m32
else
ifeq ($(ARCH),amd64)
BUILTINS := x64builtins.o
BUILTINDEPS := x64consts.h
ARCHFLAG := -m64
endif
endif

ARCHDEP:=.mudlle-arch

OLD_ARCH:=$(shell [ -f $(ARCHDEP) ] && cat $(ARCHDEP))
ifneq ($(OLD_ARCH),$(ARCH))
 ifneq ($(OLD_ARCH),)
  $(info Architecture changed. Recompiling.)
 endif
.PHONY: $(ARCHDEP)
endif

$(ARCHDEP):
	@echo "$(ARCH)" > $@


SRC := alloc.c assoc.c call.c calloc.c charset.c compile.c context.c	\
	dwarf.c elf.c env.c error.c global.c ins.c interpret.c lexer.c	\
	mcompile.c module.c mudlle-main.c mudlle.c objenv.c		\
	parser.tab.c ports.c print.c stack.c strbuf.c table.c tree.c	\
	types.c utils.c

OBJS := $(BUILTINS) $(SRC:%.c=%.o)

alloc.o error.o: CFLAGS+=$(PRIMITIVE_CFLAGS)

warnings := all missing-declarations missing-prototypes nested-externs	\
        shadow unused-macros write-strings

CC := gcc -std=gnu11
CFLAGS := -g3 $(if $(NO_OPT),-O0,-O2) $(addprefix -W,$(warnings))
CPPFLAGS := $(ARCHFLAG) -I.
LDFLAGS := $(ARCHFLAG) -fno-pie
LIBS := -lm
MAKEDEPEND = $(CC) -MM
PERL := perl

LIBCURSES := -lcurses

ifeq ($(shell uname -s),Darwin)
CPPFLAGS += -isystem /opt/local/include
LDFLAGS += -L/opt/local/lib
LIBS += -liconv
endif # Darwin

export CC CFLAGS CPPFLAGS LDFLAGS MAKEDEPEND PERL

ifneq ($(USE_XML),)
CPPFLAGS += -DUSE_XML
LIBS += -lxml2
endif

ifneq ($(USE_GMP),)
CPPFLAGS += -DUSE_GMP
LIBS += -lgmp
endif

ifneq ($(USE_READLINE),)
CPPFLAGS += -DUSE_READLINE
LIBS += -lreadline $(LIBCURSES)
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

clang:=$(shell : | $(CC) $(CPPFLAGS) $(CFLAGS) -E -dM - \
	| grep '^\#define __clang__\b')
NO_UNUSED_MACROS:=x86builtins.o x64builtins.o
charset.o: CFLAGS:=-Wno-invalid-source-encoding $(CFLAGS)

LIBRUN := runtime/librun.a

.PHONY: all
all: mudlle

mudlle: $(OBJS)
	@echo "Link $@"
	$(Q)$(CC) $(LDFLAGS) -o $@ $^ $(LIBS)

profiler: profiler.o
	@echo "Link $@"
	$(Q)$(CC) $(LDFLAGS) -o $@ $<

.PHONY: clean depclean
depclean:
	rm -f .depend

clean:
	rm -f *.o *.obj lexer.c tokens.h parser.tab.c parser.tab.h	\
		.depend genconst genconstdefs.h mudlle parser.output	\
		x86consts.h x64consts.h

ifeq (,$(USE_CPP_STEP))
%.o: %.c $(ARCHDEP)
	@echo "Compile $<"
	$(Q)$(CC) $(CPPFLAGS) $(CFLAGS) -o $@ -c $<
else
%.o: %-mpp.c
	@echo "Compile $<"
	$(Q)$(CC) $(CPPFLAGS) $(CFLAGS) -o $@ -c $<

.PRECIOUS: %-mpp.c
%-mpp.c: %.c $(ARCHDEP)
	@echo "Create $@"
	$(Q)$(CC) $(CPPFLAGS) $(CFLAGS) -E $< | grep -v '^#' | indent > $@
endif

.depend parser.tab.o lexer.o $(NO_UNUSED_MACROS): \
	CFLAGS:=$(filter-out -Wunused-macros,$(CFLAGS))

lexer.o: parser.tab.h

lexer.c: lexer.l
	@echo "Flex $<"
	$(Q)flex -CFe -8 -o $@ $<

%.tab.c %.tab.h: %.y
	@echo "Bison $<"
	$(Q)bison -dtv $<

$(BUILTINS): $(BUILTINS:%.o=%.S) $(BUILTINDEPS) $(ARCHDEP)
	@echo "Compile $<"
	$(Q)$(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

x64consts.h x86consts.h: genconst Makefile
	@echo "Create $<"
	$(Q)./genconst > $@

genconst: genconst.o Makefile
	@echo "Link $@"
	$(Q)$(CC) $(LDFLAGS) -o $@ $<

genconst.o: genconstdefs.h

CONSTH := types.h mvalues.h context.h error.h

genconstdefs.h: runtime/consts.pl $(CONSTH) Makefile
	@echo "Create $@"
	$(Q)LC_ALL=C $(PERL) $< -d -o $@ -- $(CONSTH)

.PHONY: dep depend
dep depend: .depend
	$(Q)$(MAKE) -C runtime -f Makefile depend

.depend: $(SRC) genconst.c genconstdefs.h $(BUILTINS:%.o=%.S) $(BUILTINDEPS) \
		$(ARCHDEP)
	@echo "Create $@"
	$(Q)$(MAKEDEPEND) $(CPPFLAGS) $(CFLAGS)		\
		$(filter-out %.h $(ARCHDEP),$^)		\
		> $@

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


.PHONY: help
help:
	@echo 'Available make targets:' ;				\
	echo '  mudlle    the mudlle binary (default)' ;		\
	echo '  compiler  the mudlle compiler' ;			\
	echo '  clean     remove build files' ;				\
	echo '  depclean  remove dependency files' ;			\
	echo ;								\
	echo 'Useful make variables:' ;					\
	echo '  ARCH      set to "i386" (default) or "amd64"' ;		\
	echo '  optimize  set to "no" for -O0 builds' ;			\
	echo '  verbose   set to "yes" for verbose builds'

depfile:=.depend

# include dependency files unless we are only running cleaning targets
NO_DEP_TGT:=clean depclean help

ifneq (,$(MAKECMDGOALS))
ifeq ($(MAKECMDGOALS),$(filter $(NO_DEP_TGT),$(MAKECMDGOALS)))
depfile:=
endif
endif

include runtime/Makefile

ifeq (,$(wildcard $(depfile)))
-include $(depfile)
else
include $(depfile)
endif
