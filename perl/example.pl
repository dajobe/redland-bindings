#!/usr/bin/perl
#
# example.pl - Redland eaxmple Perl program
#
# $Id$
#
# Copyright (C) 2000-2001 David Beckett - http://purl.org/net/dajobe/
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

use RDF;

$test_file="dc.rdf";

warn "Creating storage\n";
my $storage=new RDF::Storage("hashes", "test", 
			     "new='yes',hash-type='bdb',dir='.'");
die "Failed to create RDF::Storage\n" unless $storage;
  
warn "\nCreating model\n";
my $model=new RDF::Model($storage, "");
die "Failed to create RDF::Model for storage\n" unless $model;


warn "\nCreating statement\n";
my $statement=RDF::Statement->new_from_nodes(RDF::Node->new_from_uri_string("http://purl.org/net/dajobe/"),
					     RDF::Node->new_from_uri_string("http://purl.org/dc/elements/1.1/creator"),
					     RDF::Node->new_from_literal("Dave Beckett", "", 0, 0));
die "Failed to create RDF::Statement\n" unless $statement;

warn "\nAdding statement to model\n";
$model->add_statement($statement);

warn "\nParsing URI (file) $test_file\n";
my $uri=new RDF::URI("file:$test_file");

my $parser=new RDF::Parser('repat');
die "Failed to find parser\n" if !$parser;
$parser->feature("http://www.w3.org/1999/02/22-rdf-syntax-ns#aboutEach", "yes");

$stream=$parser->parse_as_stream($uri,$uri);
my $count=0;
while(!$stream->end) {
  $model->add_statement($stream->next);
  $count++;
}
$stream=undef;
warn "Parsing added $count statements\n";

warn "\nPrinting all statements\n";
$stream=$model->serialise;
while(!$stream->end) {
  print "Statement: ",$stream->next->as_string,"\n";
}
$stream=undef;

warn "\nSearching model for statements matching predicate http://purl.org/dc/elements/1.1/creator\n";
$statement=RDF::Statement->new_from_nodes(undef, RDF::Node->new_from_uri_string("http://purl.org/dc/elements/1.1/creator"), undef);
my $stream=$model->find_statements($statement);
while(!$stream->end) {
  my $statement2=$stream->next;
  print "Matching Statement: ",$statement2->as_string,"\n";
  my $subject=$statement2->subject;
  print "  Subject: ",$subject->as_string,"\n";
  print "  Predicate: ",$statement2->predicate->as_string,"\n";
  print "  Object: ",$statement2->object->as_string,"\n";
}
$stream=undef;
