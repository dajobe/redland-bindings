# -*- Mode: Perl -*-
#
# test2.t - Redland perl test 2 - general RDF::Redland coverage
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

BEGIN { $| = 1; print "1..5\n"; }
END {print "not ok 1\n" unless $loaded;}
use RDF::Redland;
$loaded = 1;
print "ok 1\n";

######################### End of black magic.

use strict;

my $test=2;

# Test the RDF::Redland module interfaces

$RDF::Redland::Debug=1 if $ENV{'TEST_VERBOSE'};

my $storage=new RDF::Redland::Storage("hashes", "test", "new='yes',hash-type='memory',dir='.'");
if(!$storage) {
  warn "new RDF::Redland::Storage failed\n";
  print "not ok $test\n";
  last;
}
print "ok $test\n";
$test++;

my $model=new RDF::Redland::Model($storage, "");
if(!$model) {
  warn "new RDF::Redland::Model failed\n";
  print "not ok $test\n";
  last;
}
print "ok $test\n";
$test++;

my $statement=RDF::Redland::Statement->new_from_nodes(RDF::Redland::Node->new_from_uri_string("http://purl.org/net/dajobe/"),
					     RDF::Redland::Node->new_from_uri_string("http://purl.org/dc/elements/1.1/creator"),
					     RDF::Redland::Node->new_from_literal("Dave Beckett", "", 0));
if(!$statement) {
  warn "new RDF::Redland::Statement->new_from_nodes failed\n";
  print "not ok $test\n";
  last;
}
print "ok $test\n";
$test++;

$model->add_statement($statement);
$statement=undef;

# Match against an empty statement - find everything
$statement=RDF::Redland::Statement->new_from_nodes(undef,undef,undef);
my $stream=$model->find_statements($statement);
my $failed=0;
while(!$stream->end) {
  my $s=$stream->current->as_string;
  if(!length $s) {
    warn "RDF::Redland::Statement->as_string failed\n";
    print "not ok $test\n";
    $failed=1;
    last;
  }
  warn "found statement: $s\n" if $RDF::Redland::Debug;
  $stream->next;
}
last if $failed;

my $source_node=RDF::Redland::Node->new_from_uri_string("http://purl.org/net/dajobe/");
my $target_node=RDF::Redland::Node->new_from_uri_string("http://purl.org/dc/elements/1.1/creator");

my $iterator=$model->targets_iterator($source_node,$target_node);
$failed=0;
while(!$iterator->end) {
  my $n=$iterator->current->as_string;
  if(!length $n) {
    warn "RDF::Redland::Node->as_string failed\n";
    print "not ok $test\n";
    $failed=1;
    last;
  }
  warn "found node: $n\n" if $RDF::Redland::Debug;
  $iterator->next;
}
$iterator=undef;
$source_node=undef;
$target_node=undef;
last if $failed;

# This happens automatically at the end of scope, but can be forced.
# However the stream must be closed  before the object that generated
# it is destroyed.
$stream=undef;

# These happen automatically but can be forced; model always has
# to be closed first, so it keeps a reference to storage around - sneaky!
$storage=undef;
$model=undef;

print "ok $test\n";
$test++;

exit 0;
