# -*- Mode: Perl -*-
#
# test1.t - Redland perl test 1 - RDF::Redland::CORE low level APIs
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

BEGIN { $| = 1; print "1..6\n"; }
END {print "not ok 1\n" unless $loaded;}
use RDF::Redland::CORE;
$loaded = 1;
print "ok 1\n";

######################### End of black magic.

use strict;

my $test=2;

# Test using RDF::Redland::CORE module only

my $debug=defined $ENV{'TEST_VERBOSE'};

my $world=&RDF::Redland::CORE::librdf_new_world();
&RDF::Redland::CORE::librdf_world_open($world);
&RDF::Redland::CORE::librdf_perl_world_init($world);

my $storage=&RDF::Redland::CORE::librdf_new_storage($world, "hashes", "test", "new='yes',hash-type='memory',dir='.'");
if(!$storage) {
  warn "Redland::librdf_new_storage failed\n";
  print "not ok $test\n";
  exit 1;
}
print "ok $test\n";
$test++;

my $model=&RDF::Redland::CORE::librdf_new_model($world, $storage, "");
if(!$model) {
  warn "Redland::librdf_new_model failed\n";
  print "not ok $test\n";
  exit 1;
}
print "ok $test\n";
$test++;

my $statement=&RDF::Redland::CORE::librdf_new_statement_from_nodes($world, 
							  &RDF::Redland::CORE::librdf_new_node_from_uri_string($world, "http://purl.org/net/dajobe/"),
							  &RDF::Redland::CORE::librdf_new_node_from_uri_string($world, "http://purl.org/dc/elements/1.1/creator"),
							  &RDF::Redland::CORE::librdf_new_node_from_literal($world, "Dave Beckett", "", 0)
							  );
if(!$statement) {
  warn "Redland::librdf_new_statement_from_nodes failed\n";
  print "not ok $test\n";
  exit 1;
}
print "ok $test\n";
$test++;

&RDF::Redland::CORE::librdf_model_add_statement($model, $statement);
&RDF::Redland::CORE::librdf_free_statement($statement);
$statement=undef;


$statement=&RDF::Redland::CORE::librdf_new_statement($world);
my $stream=&RDF::Redland::CORE::librdf_model_find_statements($model,$statement);
&RDF::Redland::CORE::librdf_free_statement($statement);
$statement=undef;

my $failed=0;
while(!&RDF::Redland::CORE::librdf_stream_end($stream)) {
  $statement=&RDF::Redland::CORE::librdf_stream_get_object($stream);
  my $s=&RDF::Redland::CORE::librdf_statement_to_string($statement);
  if(!length $s) {
    warn "Redland::librdf_new_statement_to_string failed\n";
    print "not ok $test\n";
    $failed=1;
    last;
  }
  warn "found statement: $s\n" if $debug;
  &RDF::Redland::CORE::librdf_stream_next($stream);
}
&RDF::Redland::CORE::librdf_free_stream($stream);
$stream=undef;
last if $failed;

print "ok $test\n";
$test++;

my $source_node=&RDF::Redland::CORE::librdf_new_node_from_uri_string($world, "http://purl.org/net/dajobe/");
my $target_node=&RDF::Redland::CORE::librdf_new_node_from_uri_string($world, "http://purl.org/dc/elements/1.1/creator");

my $iterator=&RDF::Redland::CORE::librdf_model_get_targets($model,$source_node,$target_node);
$failed=0;
while(!&RDF::Redland::CORE::librdf_iterator_end($iterator)) {
  my $node=&RDF::Redland::CORE::librdf_iterator_get_object($iterator);
  my $n=&RDF::Redland::CORE::librdf_node_to_string($node);
  if(!length $n) {
    warn "Redland::librdf_node_to_string failed\n";
    print "not ok $test\n";
    $failed=1;
    last;
  }
  warn "found node: $n\n" if $debug;
  &RDF::Redland::CORE::librdf_iterator_next($iterator);
}
&RDF::Redland::CORE::librdf_free_iterator($iterator);
$iterator=undef;
&RDF::Redland::CORE::librdf_free_node($source_node);
$source_node=undef;
&RDF::Redland::CORE::librdf_free_node($target_node);
$target_node=undef;

unless($failed)  {
  print "ok $test\n";
}

$test++;

&RDF::Redland::CORE::librdf_free_model($model);
$model=undef;
&RDF::Redland::CORE::librdf_free_storage($storage);
$storage=undef;

&RDF::Redland::CORE::librdf_perl_world_finish();
$world=undef;

exit 0;
