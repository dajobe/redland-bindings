# -*- Mode: Perl -*-
#
# QueryResults.pm - Redland Perl RDF Query Results module
#
# $Id$
#
# Copyright (C) 2004 David Beckett - http://purl.org/net/dajobe/
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
  my $query=new RDF::Redland::Query("rdql", undef, $query_string);
  my $results=$model->execute($query);
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


=pod

=back

=head1 SEE ALSO

L<RDF::Redland::Query>

=head1 AUTHOR

Dave Beckett - http://purl.org/net/dajobe/

=cut

1;
