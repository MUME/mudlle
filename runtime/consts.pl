#!/usr/bin/perl -w

#
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
#

undef $/;

print '
#include "runtime/runtime.h"
';

foreach $file (@ARGV) {
    print "#include \"$file\"\n";
}

print "\n";
print "void mudlle_consts_init(void)\n";
print "{\n";

$line = "";
foreach $file (@ARGV) {
    open(FILE, "<$file") or die "Failed opening $file: $!";
    print($line, "  /* $file */\n");
    $line = "\n";
    while (<FILE>) {
	s!end mudlle const.*?start mudlle const!!gs;
	s!/\*.*?\*/!!gs;
	while (/^#[ \t]*define[ \t]+(\w+)[ \t]+(\S+)/gm) {
	    printf("  system_define(\"%s\", makeint(%s));\n", $1, $1);
	}
	
	while (/enum\s+(\w+\s+)?{([^}]*)}/gm) {
            $enum = $2;
            while ($enum =~ /(\w+)\s*(=\s*[^,]*)?\s*(,|$)/g) {
	        printf("  system_define(\"%s\", makeint(%s));\n", $1, $1);
            }
        }
    }
}

print "}\n";
