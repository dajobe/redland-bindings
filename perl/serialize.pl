#!/usr/bin/perl -w
#
# serialize.pl - Redland serialize test program
#
# $Id$
#
# Copyright (C) 2002 David Beckett - http://purl.org/net/dajobe/
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
# Status: incomplete
#
#
#

use strict;

use RDF::Redland;

die <<"EOT" if @ARGV < 1 || @ARGV > 2;
Usage: $0 <URI> [BASE URI>]

This program serializes the file URI to RDF/XML using optional
base URI BASE URI if given.

EOT

my $uri=$ARGV[0];

my $tmp_file;
my $source_uri;
if($uri !~ m%^file:%) {
  use URI::URL;
  use LWP::Simple;
  
  $tmp_file="/tmp/$0-$$.serialize";

  my $perl_uri;
  eval "\$perl_uri=new URI::URL('$uri')";
  if($@) {
    die "$0: URI $uri is not supported by Perl\n";
  }
  my $rc=getstore($perl_uri, $tmp_file);
  
  if(!is_success($rc)) {
    die "$0: Failed to fetch URI $uri - HTTP error $rc\n";
    unlink $tmp_file;
  }
  $source_uri="file:$tmp_file";
} else {
  $source_uri=$uri;
}
my $base_uri=$uri;
$base_uri=$ARGV[1] if @ARGV ==2;


my $storage=new RDF::Redland::Storage("hashes", "serialize", 
				      "new='yes',hash-type='bdb',dir='.'");
die "$0: Failed to create RDF::Redland::Storage\n" unless $storage;

my $model=new RDF::Redland::Model($storage, "");
die "$0: Failed to create RDF::Redland::Model for storage\n" unless $model;

my $parser=new RDF::Redland::Parser("raptor");
die "$0: Failed to create RDF::Redland::Parser 'raptor'\n" unless $parser;

my $redland_base_uri=new RDF::Redland::URI $uri;
my $redland_source_uri=new RDF::Redland::URI $source_uri;

my $stream=$parser->parse_as_stream($redland_source_uri, $redland_base_uri);
die "$0: Failed to create stream using parse_as_stream\n"
  if !$stream || $stream->end;
my $count=0;
while(!$stream->end) {
  my $statement=$stream->next;
  $model->add_statement($statement);
  $statement=undef;
  $count++;
}
$stream=undef;
print "URI \"$uri\" parsed as RDF/XML OK (created $count statements)\n";

unlink $tmp_file if $tmp_file;

## SERIALIZE test

my $RDF_NS="http://www.w3.org/1999/02/22-rdf-syntax-ns#";
my $XML_NS="http://www.w3.org/XML/1998/namespace";
my $RDF_type_predicate=RDF::Redland::Node->new_from_uri_string($RDF_NS."type");

my $DC_NS="http://purl.org/dc/elements/1.1";

my $state={
  inscope_namespaces => [
    ["xml" , $XML_NS, -1],
  ],
  favourite_prefixes => {
    $RDF_NS => "rdf",
    $DC_NS  => "dc",
  },
  feature_prefer_ID_to_about => "false", # prefer rdf:ID or rdf:about ?
  feature_indenting_size => 2,
  feature_content_encoding => "utf8",
  feature_prefer_default_ns_rdf => "false",
  depth => 0,                            # depth of output XML elements

  node_types => {},
  nodes => {},
  subject_key_to_node => {},
};


sub predicate_split($) {
  my $predicate=shift;
  my $uri=$predicate->uri->as_string;
  if($uri =~ m%^(.*)[#/](.*)$%) {
    return($1,$2);
  }
  return($uri,undef);
}


$stream=$model->serialise;
my $id=1;
while(!$stream->end) {
  my $statement=$stream->next;
  my $subject=$statement->subject;
  my $subject_key=$subject->as_string;

  my($max_uri,$name)=predicate_split($statement->predicate);
  if(!$name) {
    warn "$0: Statenent with predicate URI $max_uri cannot be serialized\n";
    next;
  }

  if($statement->predicate->equals($RDF_type_predicate)) {
    print "found type statement ",$statement->as_string,"\n";
    push(@{$state->{node_types}->{$subject_key}}, $statement->object);
  } else {
    push(@{$state->{nodes}->{$subject_key}}, $statement);
  }
  $state->{subject_key_to_node}->{$subject_key}=$subject;
}


sub make_xml_qname($$$;$) {
  my($state,$ns_uri,$name,$is_attr)=@_;
  for my $nspace (@{$state->{inscope_namespaces}}) {
    my($prefix,$nspace_uri)=@$nspace;
    next if $nspace_uri ne $ns_uri;
    next if ($is_attr && !length $prefix);

    return $prefix ? "$prefix:$name" : $name;
  }
  die "Namespace URI $ns_uri with a prefix is not declared in current scope\n"
    if $is_attr;
  die "Namespace URI $ns_uri is not declared in current scope\n";
}

sub declare_namespace($$;$) {
  my($state,$ns_uri,$prefix)=@_;
  if(!defined $prefix) {
    $prefix=$state->{favourite_prefixes}->{$ns_uri};
    die "No prefix given for Namespace URI $ns_uri and no favourite found\n"
      unless $prefix;
  }
  unshift(@{$state->{inscope_namespaces}},
	  [$prefix, $ns_uri, $state->{depth}]);
}

sub undeclare_namespaces($) {
  my $state=shift;
  my(@nn);
  for my $nspace (@{$state->{inscope_namespaces}}) {
    push(@nn, $nspace) if $nspace->[2] >= $state->{depth};
  }
  $state->{inscope_namespaces}=\@nn;
}


sub indent($) {
  my $state=shift;
  $state->{depth}++;
}

sub outdent($) {
  my $state=shift;
  $state->{depth}--;
}

sub emit($$) {
  my($state,$string)=@_;

  my $unit=$state->{feature_indenting_size};
  print ' ' x ($state->{depth} * $unit);
  print $string;
  print "\n";
}


# print <predicate>object </predicate>
# in various forms depending on type of object node
sub format_statement ($$$) {
  my($state,$predicate,$object)=@_;

  my($predicate_uri,$predicate_name)=predicate_split($predicate);
  
  ensure_declared_namespace($state, $predicate_uri);

  my $qname=make_xml_qname($state, $predicate_uri, $predicate_name);

  my $otype=$object->type;
  if($otype == $RDF::Redland::Node::Type_Resource) {
    my $attr=make_xml_qname($state, $RDF_NS, "resource", 1);
    my $object_value=$object->uri->as_string;
    emit($state, qq{<$qname $attr="$object_value"/>});
  } elsif($otype == $RDF::Redland::Node::Type_Literal) {
    my $literal=$object->literal_value; # FIXME or literal_value_as_latin1
    my $literal_lang=$object->literal_value_language;
    my $attrs='';
    if($literal_lang) {
      $attrs.=qq{ xml:lang="$literal_lang"};
    }
    if ($object->literal_value_is_wf_xml) {
      my $attr=make_xml_qname($state, $RDF_NS, "parseType", 1);
      $attrs.=" " if $attrs;
      $attrs.=qq{$attr="Literal"};
    }
    emit($state, qq{<$qname${attrs}>$literal</$qname>});
  } elsif($otype == $RDF::Redland::Node::Type_Blank) {
    my $element=make_xml_qname($state, $RDF_NS, "Description");
    emit($state, qq{<$qname><$element/></$qname>});
  } else {
    die "Unknown object type $otype\n";
  }
}


sub start_format_subject($$) {
  my($state,$subject_node)=@_;

  my $element=make_xml_qname($state, $RDF_NS, "Description");
  my $about='';
  if($subject_node->type eq $RDF::Redland::Node::Type_Resource) {
    my $attr=make_xml_qname($state, $RDF_NS, "about", 1);
    my $url=$subject_node->uri->as_string;
    $about=qq{ $attr="$url"};
  }
  emit($state, qq{<$element$about>});
}

sub end_format_subject($$) {
  my($state,$subject_node)=@_;

  my $element=make_xml_qname($state, $RDF_NS, "Description");
  my $about='';
  emit($state, qq{</$element>});
}



sub dump_nodes ($@) {
  my($state,@order)=@_;

  for my $subject_key (@order) {
    my $subject_node=$state->{subject_key_to_node}->{$subject_key};
    #print "key $subject_key - node ",$subject_node->as_string,"\n";
    
    start_format_subject($state, $subject_node);

    indent($state);
    my $count=1;
    for my $statement (@{$state->{nodes}->{$subject_key}}) {
      format_statement($state, $statement->predicate, $statement->object);
      $count++;
    }
    outdent($state);

    end_format_subject($state, $subject_node)
  }

}


# start

#use Data::Dumper;
#print Dumper($state),"\n";

my(@nodes_with_types)=keys %{$state->{node_types}};
my(@other_nodes)=grep(!exists $state->{node_types}->{$_}, 
		      keys %{$state->{nodes}});

# order:
#   rdf:ID things first
#   rdf:about things with types?
#   rest

my(@order)=(@nodes_with_types, @other_nodes);

print qq{<?xml version='1.0' encoding='}; #'
print $state->{feature_content_encoding}, "'?>\n"; #'

my $element=make_xml_qname($state, $RDF_NS, "RDF");

emit($state, qq{<$element>});

  indent($state);
  dump_nodes($state, @order);
  outdent($state);

emit($state, qq{</$element>});

