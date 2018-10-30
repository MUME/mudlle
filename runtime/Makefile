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

.PHONY: wrong-file
wrong-file:
	$(error Use Makefile in the parent directory)

RTOBJS:=$(addprefix runtime/, arith.o basic.o bigint.o bitset.o	\
        bool.o debug.o files.o io.o list.o mudlle-float.o		\
        mudlle-string.o mudlle-xml.o mudllecst.o pattern.o		\
        runtime.o support.o symbol.o vector.o)

$(RTOBJS): CFLAGS+=$(PRIMITIVE_CFLAGS)

mudlle: $(RTOBJS)

RTSRC = $(RTOBJS:%.o=%.c)

CSTFILES := error.h module.h types.h			\
	runtime/basic.h runtime/files.h runtime/io.h

ifneq ($(USE_XML),)
CPPFLAGS += -isystem /usr/include/libxml2
CSTFILES += runtime/mudlle-xml.h
endif

RTDEPFILE:=runtime/.depend

.PHONY: clean_runtime

clean: clean_runtime
clean_runtime:
	rm -f runtime/*.o runtime/mudllecst.c $(RTDEPFILE)

runtime/mudllecst.c: runtime/consts.pl $(CSTFILES) runtime/Makefile
	@echo "Create $@"
	$(Q)LC_ALL=C $(PERL) $< -o $@ -- $(CSTFILES)

$(RTDEPFILE): $(RTSRC) $(ARCHDEP)
	@echo "Create $@"
	$(Q)$(MAKEDEPEND) $(CPPFLAGS) $(CFLAGS) $(RTSRC) \
		| sed 's!^\(.*\)\.o *:!runtime/\1.o:!g' > $@

ifneq (,$(depfile))
ifeq (,$(wildcard $(RTDEPFILE)))
-include $(RTDEPFILE)
else
include $(RTDEPFILE)
endif
endif
