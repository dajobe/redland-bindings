# -*- Mode: Perl -*-
#
# QueryResults.pm - Redland Perl RDF Query Results module
#
# $Id$
#
# Copyright (C) 2004-2005 David Beckett - http://purl.org/net/dajobe/
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

package RDF::Redland::QueryResults;

use RDF::Redland::Stream;

use strict;

=pod

=head1 NAME

RDF::Redland::QueryResults - Redland RDF Syntax Query Results Class

=head1 SYNOPSIS

  use RDF::Redland;

  ...
  my $query=new RDF::Redland::Query("query string", undef, undef, "sparql");
  my $results=$model->query_execute($query);
  # or my $results=$query->execute($model);
  while(!$results->finished) {
    for (my $i=0; $i < $results->bindings_count(); $i++) {
      my $name=$results->binding_name($i);
      my $value=$results->binding_value($i);
      # ... do something with the results
    }
    $results->next_result;
  }

The $results in the example is an object of class RDF::Redland::QueryResults.

=head1 DESCRIPTION

This class represents queries of various syntaxes over an
RDF::Redland::Model returning a sequence of results that bind
variable names to RDF::Redland::Node values.

=cut

######################################################################

=pod

=head1 CONSTRUCTOR

There are no public constructors.

=cut

# CONSTRUCTOR

sub new ($$) {
  my($proto,$object)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};

  $self->{QUERYRESULTS}=$object;

  bless ($self, $class);
  return $self;
}

# DESTRUCTOR
sub DESTROY ($) {
  warn "RDF::Redland::QueryResults DESTROY\n" if $RDF::Redland::Debug;
  &RDF::Redland::CORE::librdf_free_query_results(shift->{QUERYRESULTS});
}


=head1 METHODS

=over

=item count

Return the number of current results from the query.

=cut

sub count ($) {
  my($self)=@_;
  return &RDF::Redland::CORE::librdf_query_results_get_count($self->{QUERYRESULTS});
}


=item finished

Return non-0 if the results have been exhausted.

=cut

sub finished ($) {
  my($self)=@_;
  return &RDF::Redland::CORE::librdf_query_results_finished($self->{QUERYRESULTS});
}


=item binding_name INDEX

Get the name of variable binding I<INDEX> in the array of variable names.

=cut

sub binding_name ($$) {
  my($self,$index)=@_;
  return &RDF::Redland::CORE::librdf_query_results_get_binding_name($self->{QUERYRESULTS},$index);
}


=item binding_names

Get the names all of the variable bindings as an array.

=cut

sub binding_names ($) {
  my($self)=shift;
  my(@names);
  my $count=&RDF::Redland::CORE::librdf_query_results_get_bindings_count($self->{QUERYRESULTS});
  for (my $i=0; $i < $count; $i++) {
    push(@names, $self->binding_name($i));
  }
  return @names;
}


=item binding_value INDEX

Get the value of the variable binding I<INDEX> in the current query result.

=cut

sub binding_value ($$) {
  my($self,$index)=@_;
  my $node=&RDF::Redland::CORE::librdf_query_results_get_binding_value($self->{QUERYRESULTS},$index);
  RDF::Redland::Node->_new_from_object($node);
}


=item binding_values

Get the values of all of the variable bindings in the current query result.

=cut

sub binding_values ($) {
  my($self)=shift;
  my(@values);
  my $count=&RDF::Redland::CORE::librdf_query_results_get_bindings_count($self->{QUERYRESULTS});
  for (my $i=0; $i < $count; $i++) {
    push(@values, $self->binding_value($i));
  }
  return @values;
}


=item binding_value_by_name NAME

Get the value of the variable binding I<NAME> in the current query result.

=cut

sub binding_value_by_name ($$) {
  my($self,$name)=@_;
  my $node=&RDF::Redland::CORE::librdf_query_results_get_binding_value_by_name($self->{QUERYRESULTS},$name);
  RDF::Redland::Node->_new_from_object($node);
}


=item bindings

Get the variable names and values of the current query result as a hash

=cut

sub bindings ($) {
  my($self)=shift;
  my(%results);
  my $count=&RDF::Redland::CORE::librdf_query_results_get_bindings_count($self->{QUERYRESULTS});
  for (my $i=0; $i < $count; $i++) {
    $results{$self->binding_name($i)}=$self->binding_value($i);
  }
  return %results;
}


=item bindings_count

Return the number of variable bindings.

=cut

sub bindings_count ($) {
  my($self)=@_;
  return &RDF::Redland::CORE::librdf_query_results_get_bindings_count($self->{QUERYRESULTS});
}


=item next_result

Move to the next query result.

=cut

sub next_result ($) {
  my($self)=@_;
  return &RDF::Redland::CORE::librdf_query_results_next($self->{QUERYRESULTS});
}


=item as_stream

Return a new RDF::Redland::Stream object representing the query results
as an RDF Graph.

=cut

sub as_stream($) {
  my($self)=@_;
  die "Query results is not in RDF graph format"
    unless $self->is_graph;

  my $stream=&RDF::Redland::CORE::librdf_query_results_as_stream($self->{QUERYRESULTS});
  return new RDF::Redland::Stream($stream,$self);
}


=item to_string [FORMAT-URI [BASE-URI]]

Serialize to a string syntax in format I<FORMAT-URI> using the optional
I<BASE-URI>.  The default format when none is given is determined by
librdf_query_results_to_string.

=cut

sub to_string($;$$) {
  my($self, $format_uri, $base_uri)=@_;
  $format_uri ||= undef;
  $base_uri ||= undef;

  if($self->is_graph) {
    my $tmpstorage=RDF::Redland::Storage->new("memory");
    my $tmpmodel=RDF::Redland::Model->new($tmpstorage, "");
    $tmpmodel->add_statements($self->as_stream);
    my $serializer=RDF::Redland::Serializer->new();
    return $serializer->serialize_model_to_string($base_uri, $tmpmodel);
  }

  if(!$self->is_boolean && !$self->is_bindings) {
    die "Unknown query result format cannot be written as a string";
  }
  
  if($format_uri && !ref($format_uri)) {
    $format_uri = RDF::Redland::URI->new($format_uri);
  }

  my $rformat_uri=$format_uri ? $format_uri->{URI} : undef;

  if($base_uri && !ref($base_uri)) {
    $base_uri = RDF::Redland::URI->new($base_uri);
  }
  my $rbase_uri=$base_uri ? $base_uri->{URI} : undef;

  warn "RDF::Redland::QueryResults format URI ".$format_uri->as_string." base URI ".($base_uri ? $base_uri->as_string : "undef")."\n"
    if $RDF::Redland::Debug;

  return &RDF::Redland::CORE::librdf_query_results_to_string($self->{QUERYRESULTS}, $rformat_uri, $rbase_uri);
}


=item is_bindings

Return non-0 if the query results format is variable bindings

=cut

sub is_bindings($) {
  my($self)=shift;
  return &RDF::Redland::CORE::librdf_query_results_is_bindings($self->{QUERYRESULTS});
}


=item is_boolean

Return non-0 if the query results format is a boolean

=cut

sub is_boolean($) {
  my($self)=shift;
  return &RDF::Redland::CORE::librdf_query_results_is_boolean($self->{QUERYRESULTS});
}


=item is_graph

Return non-0 if the query results format is an RDF graph

=cut

sub is_graph($) {
  my($self)=shift;
  return &RDF::Redland::CORE::librdf_query_results_is_graph($self->{QUERYRESULTS});
}


=item get_boolean

Get the boolean query result; non-0 is true.

=cut

sub get_boolean($) {
  my($self)=shift;
  die "Query result is not in boolean format"
    unless $self->is_boolean;

  return &RDF::Redland::CORE::librdf_query_results_get_boolean($self->{QUERYRESULTS});
}

=pod

=back

=head1 SEE ALSO

L<RDF::Redland::Query>

=head1 AUTHOR

Dave Beckett - http://purl.org/net/dajobe/

=cut

1;
