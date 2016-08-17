# Copyright (c) 1993-2015 David Gay and Gustav Hållberg
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

use strict;
use warnings;

use Getopt::Std;

$Getopt::Std::STANDARD_HELP_VERSION = 1;

my %opts;
getopts('do:', \%opts) or exit(1);

sub HELP_MESSAGE { usage(); }
sub VERSION_MESSAGE { }

sub usage {
    print "Synopsis:\n";
    print "  $0 [options] <header-files>\n";
    print "Options:\n";
    print "  -d          Generate FOR_DEFS() output\n";
    print "  -o <file>   Send output to <file>\n";
}

$#ARGV >= 0 or die "No input files specified";

undef $/;

my ($ofile, $guarded);

if (defined($opts{'o'})) {
    open($ofile, '>', $opts{'o'})
        or die "Failed to open output file: $!";
    $guarded = $opts{'o'};
} else {
    open($ofile, '>-') or die;
}
print $ofile "/* automatically generated by ", $0, " */\n";

unless (defined $opts{'d'}) {
    print $ofile '
#  include "runtime.h"

';

    foreach my $file (@ARGV) {
        print $ofile "#include \"$file\"\n";
    }
}

print $ofile "\n";
print $ofile "#define FOR_DEFS(op) \\\n";
my $line = "";
foreach my $file (@ARGV) {
    open(FILE, "<$file") or die "Failed opening $file: $!";
    print $ofile ($line, "  /* $file */ \\\n");
    $line = "  \\\n";
    my $found = 0;
    while (<FILE>) {
	s!end mudlle const.*?start mudlle const!!gs;
	s!end mudlle const.*!!gs;
	s!/\*.*?\*/!!gs;
	while (/^\#[ \t]*define[ \t]+(\w+)[ \t]+(\S+)/gm) {
            next if $2 =~ m/^"/;
	    printf $ofile ("  op(%s) \\\n", $1);
            $found = 1;
	}

	while (/enum\s+(\w+\s+)?{([^}]*)}/gm) {
            my $enum = $2;
            while ($enum =~ /(\w+)\s*(=\s*[^,]*)?\s*(,|$)/g) {
                printf $ofile ("  op(%s) \\\n", $1);
                $found = 1;
            }
        }
    }
    print STDERR "${file}:1: warning: no constants found\n"
        unless $found;
}

print $ofile "\n";

unless (defined $opts{'d'}) {
    print $ofile '#define SDEF_OP(n) STATIC_STRING(sstr_ ## n, #n);

FOR_DEFS(SDEF_OP)

static const struct {
  struct string *name;
  value mval;
} mudlle_int_consts[] = {
#define DEF_OP(n) { GET_STATIC_STRING(sstr_ ## n), makeint(n) },
FOR_DEFS(DEF_OP)
};

void mudlle_consts_init(void)
{
  for (int i = 0; i < VLENGTH(mudlle_int_consts); ++i)
    system_string_define(mudlle_int_consts[i].name, mudlle_int_consts[i].mval);
}
';
}

undef $guarded;

END {
    unlink($guarded) if defined($guarded);
}
