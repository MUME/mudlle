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

OBJS:=arith.o basic.o bigint.o bitset.o bool.o debug.o files.o	\
        io.o list.o mudlle-float.o mudllecst.o pattern.o		\
        runtime.o string.o support.o symbol.o vector.o xml.o

$(OBJS): CFLAGS+=$(PRIMITIVE_CFLAGS)

SRC = $(OBJS:%.o=%.c)

CSTFILES = files.h io.h ../error.h ../module.h ../types.h

ifneq ($(USE_XML),)
CPPFLAGS += -isystem /usr/include/libxml2
CSTFILES += xml.h
endif

all: librun.a

.PHONY: clean depclean
depclean:
	rm -f .depend

clean:
	rm -f *.o librun.a mudllecst.c .depend

librun.a: $(OBJS)
	[ ! -f librun.a ] || rm -f librun.a
	ar r librun.a $(OBJS)
	-ranlib librun.a

mudllecst.c: consts.pl $(CSTFILES) Makefile
	perl $< $(CSTFILES) > $@

%.o: %.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -o $@ -c $<


.PHONY: dep depend
dep depend: .depend

.depend: $(SRC)
	$(MAKEDEPEND) $(CPPFLAGS) $(CFLAGS) $(SRC) \
		| sed 's/\.o *:/.o:/g' > $@

depfile:=.depend

# include dependency files unless we are only running cleaning targets
ifneq (,$(MAKECMDGOALS))
ifeq ($(MAKECMDGOALS),$(filter clean depclean,$(MAKECMDGOALS)))
depfile:=
endif
endif

-include $(depfile)
