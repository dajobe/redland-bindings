#!/usr/bin/perl
#
# example.pl - Redland eaxmple Perl program
#
# Copyright (C) 2000-2005 David Beckett - http://www.dajobe.org/
# Copyright (C) 2000-2005 University of Bristol - http://www.bristol.ac.uk/
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

use RDF::Redland;

$test_file="../data/dc.rdf";

warn "Creating storage\n";
my $storage=new RDF::Redland::Storage("hashes", "test", 
				      "new='yes',hash-type='bdb',dir='.'");
die "Failed to create RDF::Redland::Storage\n" unless $storage;
  
warn "\nCreating model\n";
my $model=new RDF::Redland::Model($storage, "");
die "Failed to create RDF::Redland::Model for storage\n" unless $model;


my $creator_uri=new RDF::Redland::URI("http://purl.org/dc/elements/1.1/creator");

warn "\nCreating statement\n";
my $statement=new RDF::Redland::Statement(RDF::Redland::Node->new_from_uri("http://www.dajobe.org/"),
					  $creator_uri,
					  new RDF::Redland::Node("Dave Beckett"));
die "Failed to create RDF::Redland::Statement\n" unless $statement;

warn "\nAdding statement to model\n";
$model->add_statement($statement);
$statement=undef;

my $n=new RDF::Redland::URI('http://example.org/foo');
my $statementn=new RDF::Redland::Statement($n, $n, $n);

warn "\nAdding statement (new Statement(n, n, n)) to model\n";
$model->add_statement($statementn);
$statement=undef;

warn "\nAdding statement (n,n,n) to model\n";
$model->add_statement($n, $n, $n);
$statement=undef;

die "Data file $test_file not found - cannot parse it.\n  Try editing \$test_file variable to point to a file.\n"
  unless -r $test_file;

warn "\nParsing URI (file) $test_file\n";
my $uri=new RDF::Redland::URI("file:$test_file");

# Use any rdf/xml parser that is available
my $parser=new RDF::Redland::Parser("rdfxml", "application/rdf+xml");
die "Failed to find parser\n" if !$parser;

$stream=$parser->parse_as_stream($uri,$uri);
my $count=0;
while(!$stream->end) {
  $model->add_statement($stream->current);
  $count++;
  $stream->next;
}
$stream=undef;
warn "Parsing added $count statements\n";


warn "\nNamespace declarations seen during parsing:\n";
my(%namespaces)=$parser->namespaces_seen();
while(my($prefix,$uri)=each %namespaces) {
  warn "  prefix '$prefix' URI ".$uri->as_string."\n";
}

warn "\nPrinting all statements\n";
$stream=$model->as_stream;
while(!$stream->end) {
  print "Statement: ",$stream->current->as_string,"\n";
  $stream->next;
}
$stream=undef;

warn "\nSearching model for statements matching predicate http://purl.org/dc/elements/1.1/creator\n";
$statement=new RDF::Redland::Statement(undef, $creator_uri, undef);
my $stream=$model->find_statements($statement);
while(!$stream->end) {
  my $statement2=$stream->current;
  print "Matching Statement: ",$statement2->as_string,"\n";
  my $subject=$statement2->subject;
  print "  Subject: ",$subject->as_string,"\n";
  print "  Predicate: ",$statement2->predicate->as_string,"\n";
  print "  Object: ",$statement2->object->as_string,"\n";
  $stream->next;
}
$stream=undef;

$statement=undef;


my $home=RDF::Redland::Node->new_from_uri("http://www.dajobe.org/");
warn "\nSearching model for targets of subject ",$home->uri->as_string," predicate ", $creator_uri->as_string, "\n";
my(@nodes)=$model->targets($home, new RDF::Redland::Node($creator_uri));
die "Failed to find any targets matching\n"
  unless @nodes;
for my $node (@nodes) {
  print "Matching Node: ",$node->as_string,"\n";
}
$iterator=undef;


my $q = new RDF::Redland::Query("PREFIX dc: <http://purl.org/dc/elements/1.1/>\nSELECT ?a ?c WHERE { ?a dc:title ?c }", undef, undef, "sparql");
print "Querying for dc:titles:\n";
my $results=$model->query_execute($q);
my $count=1;
while(!$results->finished) {
  print "result $count: {\n";
  for(my $i=0; $i < $results->bindings_count; $i++) {
    my $val=$results->binding_value($i);
    next unless defined $val; # optionals
    print "  ",$results->binding_name($i),"=",$val->as_string,"\n";
  }
  print "}\n";
  $results->next_result;
  $count++;
}
$results=undef;
warn "Returned $count results\n";


print "\nExecuting query again\n";
my $results=$model->query_execute($q);
my $str=$results->to_string;
print "Query results serialized to an XML string size ".length($str)." bytes\n";

print "\nExecuting SPARQL construct query\n";
my $q2 = new RDF::Redland::Query("CONSTRUCT { ?a ?b ?c } WHERE { ?a ?b ?c }", undef, undef, "sparql");
my $results=$model->query_execute($q2);
$stream=$results->as_stream;
$count=0;
while(!$stream->end) {
  print "Statement: ",$stream->current->as_string,"\n";
  $stream->next;
  $count++;
}
$stream=undef;
warn "Returned $count triples\n";

warn "\nWriting model to test-out.rdf as rdf/xml\n";

# Use any rdf/xml parser that is available
my $serializer=new RDF::Redland::Serializer("rdfxml");
die "Failed to find serializer\n" if !$serializer;

$serializer->set_namespace("dc", new RDF::Redland::URI("http://purl.org/dc/elements/1.1/"));
$serializer->serialize_model_to_file("test-out.rdf", $uri, $model);

my $str1=$serializer->serialize_model_to_string($uri, $model);
warn "\nSerialized to RDF/XML as a string size ",length($str1)," bytes\n";

$serializer=undef;

my $str=$model->to_string(new RDF::Redland::URI("http://example.org/base#"), "ntriples");
warn "\nSerialized to ntriples as a string size ",length($str)," bytes\n";

warn "\nDone\n";

# Required in order to ensure storage is correctly flushed to disk
$storage=undef;
$model=undef;
