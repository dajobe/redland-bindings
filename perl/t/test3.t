# -*- Mode: Perl -*-
#
# test3.t - Redland perl test 3 - error and warnings
#
# $Id$
#
# Copyright (C) 2000-2002 David Beckett - http://purl.org/net/dajobe/
# Institute for Learning and Research Technology - http://www.ilrt.org/
# University of Bristol - http://www.bristol.ac.uk/
# 
# This package is Free Software or Open Source available under the
# following licenses (these are alternatives):
#   1. GNU Lesser General Public License (LGPL)
#   2. GNU General Public License (GPL)
#   3. Mozilla Public License (MPL)
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
  my($type,$message)=@_;
  if($type == 0) {
    if(ref $RDF::Redland::Error_Sub) {
      $RDF::Redland::Error_Sub->($message);
    } else {
      die "Redland error: $message\n";
    }
  } else {
    if(ref $RDF::Redland::Warning_Sub) {
      $RDF::Redland::Warning_Sub->($message);
    } else {
      warn "Redland warning: $message\n";
    }
  }
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
