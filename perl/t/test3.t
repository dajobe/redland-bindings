# -*- Mode: Perl -*-
#
# test3.t - Redland perl test 3 - error and warnings
#
# Copyright (C) 2000-2005 David Beckett - http://www.dajobe.org/
# Copyright (C) 2000-2005 University of Bristol - http://www.bristol.ac.uk/
# 
# This package is Free Software and part of Redland http://librdf.org/
# 
# It is licensed under the following three licenses as alternatives:
#   1. GNU Lesser General Public License (LGPL) V2.1 or any newer version
#   2. GNU General Public License (GPL) V2 or any newer version
#   3. Apache License, V2.0 or any newer version
# 
# You may not use this file except in compliance with at least one of
# the above three licenses.
# 
# See LICENSE.html or LICENSE.txt at the top of this package for the
# full license terms.
# 
# 
#

######################### We start with some black magic to print on failure.

BEGIN { $| = 1; print "1..3\n"; }
END {print "not ok 1\n" unless $loaded;}
use RDF::Redland::CORE;
$loaded = 1;
print "ok 1\n";

######################### End of black magic.

use strict;

my $test=2;

# Test using Redland module only

my $debug=defined $ENV{'TEST_VERBOSE'};

my $world=&RDF::Redland::CORE::librdf_new_world();
&RDF::Redland::CORE::librdf_world_open($world);
&RDF::Redland::CORE::librdf_perl_world_init($world);

package RDF::Redland::World;

sub message ($$) {
  my($code, $level, $facility, $message, $line, $column, $byte, $file, $uri)=@_;
  if($level > 3) {
    if(ref $RDF::Redland::Error_Sub) {
      return $RDF::Redland::Error_Sub->($message);
    } else {
      die "Redland error: $message\n";
    }
  } else {
    if(ref $RDF::Redland::Warning_Sub) {
      return $RDF::Redland::Warning_Sub->($message);
    } else {
      warn "Redland warning: $message\n";
    }
  }
  1;
}

package main;

# check 'die' works
my $result='not ok';
eval '&RDF::Redland::CORE::librdf_internal_test_error($world)';
$result='ok' if $@ =~ /test error message number 1/;
print "$result $test\n";
$test++;

# check 'warn' works
$::warn_worked='not ok';
$SIG{__WARN__}=sub { $::warn_worked='ok' if shift =~ /test warning message number 2/ };
&RDF::Redland::CORE::librdf_internal_test_warning($world);
print "$::warn_worked $test\n";

&RDF::Redland::CORE::librdf_perl_world_finish();
$world=undef;

exit 0;
