# -*- Mode: Perl -*-
#
# Query.pm - Redland Perl RDF Query module
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

package RDF::Redland::Query;

use strict;

use RDF::Redland::Stream;

=pod

=head1 NAME

RDF::Redland::Query - Redland RDF Syntax Querys Class

=head1 SYNOPSIS

  use RDF::Redland;

  ...
  my $query=new RDF::Redland::Query("rdql", undef, $query_string);
  $model->query_as_bindings($query);
  while(!$query->finished) {
    for (my $i=0; $i < $query->bindings_count(); $i++) {
      my $name=$query->result_binding_name($i);
      my $value=$query->result_binding_value($i);
      # ... do something with the results
    }
    $query->next_result;
  }

=head1 DESCRIPTION

This class represents queries of various syntaxes over an
RDF::Redland::Model returning a sequence of results that bind
variable names to RDF::Redland::Node values.

=cut

######################################################################

=pod

=head1 CONSTRUCTORS

=over

=item new NAME URI QUERY-STRING

Create a new RDF::Redland::Query object for a query string I<QUERY-STRING>
IN QUERY language I<NAME> with base URI I<URI> (can be undef).
If I<NAME> is undef, the default query language is used.  If
I<URI> is omitted, the current directory is used as the base URI.

=cut

# CONSTRUCTOR
# (main)
sub new ($$$$) {
  my($proto,$name,$uri,$query_string)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};

  if(defined $uri) {
    $uri=$uri->{URI};
  }

  $self->{QUERY}=&RDF::Redland::CORE::librdf_new_query($RDF::Redland::World->{WORLD},$name,$name,$uri,$query_string);
  return undef if !$self->{QUERY};

  bless ($self, $class);
  return $self;
}

=pod

=back

=cut

sub DESTROY ($) {
  warn "RDF::Redland::Query DESTROY\n" if $RDF::Redland::Debug;
  &RDF::Redland::CORE::librdf_free_query(shift->{QUERY});
}

=head1 METHODS

=over

=item run_query_as_stream MODEL

Run the query against the given I<MODEL> returning a stream of statement
answers, rather than running it and returning variable bindings using
run_query_as_bindings.

Returns an RDF::Redland::Stream of RDF::Redland::Statement objects or
undef on failure.

=cut

sub run_query_as_stream ($) {
  my($self,$model)=@_;
  my $stream=&RDF::Redland::CORE::librdf_query_run_as_stream($self->{QUERY},$model->{MODEl});
  return undef if !$stream;
  return new RDF::Redland::Stream($stream,$self);
}

=item run_query_as_bindings MODEL

Run the query against model I<MODEL> returning the results as bindings
that can be accessed by other methodos.


=cut

sub run_query_as_bindings ($$) {
  my($self,$model)=@_;
  return &RDF::Redland::CORE::librdf_query_run_as_bindings($self->{QUERY},$model->{MODEL});
}

=item get_result_count

Return the current number of results from the query.

=cut

sub get_result_count ($) {
  my($self)=@_;
  return &RDF::Redland::CORE::librdf_query_get_result_count_feature($self->{QUERY});
}


=item finished

Return non-0 if the results have been exhausted.

=cut

sub finished ($) {
  my($self)=@_;
  return &RDF::Redland::CORE::librdf_query_query_results_finished($self->{QUERY});
}


=item result_binding_name INDEX

Get the name of the variable binding I<INDEX> in the sequence of
variable bindings.

=cut

sub result_binding_name ($$) {
  my($self,$index)=@_;
  return &RDF::Redland::CORE::librdf_query_get_result_binding_name($self->{QUERY},$index);
}


=item result_binding_names

Get the names of all of the variable bindings in the sequence of
variable bindings.

=cut

sub result_binding_names ($) {
  my($self)=shift;
  my(@names);
  my $count=&RDF::Redland::CORE::librdf_query_get_bindings_count($self->{QUERY});
  for (my $i=0; $i < $count; $i++) {
    push(@names, $self->result_binding_name($i));
  }
  return @names;
}


=item result_binding_value INDEX

Get the value of the variable binding I<INDEX> in the sequence of
variable bindings.

=cut

sub result_binding_value ($$) {
  my($self,$index)=@_;
  my $node=&RDF::Redland::CORE::librdf_query_get_result_binding_value($self->{QUERY},$index);
  RDF::Redland::Node->_new_from_object($node);
}


=item result_binding_values

Get the values of all of the variable bindings in the sequence of
variable bindings.

=cut

sub result_binding_values ($) {
  my($self)=shift;
  my(@values);
  my $count=&RDF::Redland::CORE::librdf_query_get_bindings_count($self->{QUERY});
  for (my $i=0; $i < $count; $i++) {
    push(@values, $self->result_binding_value($i));
  }
  return @values;
}


=item result_binding_value_by_name NAME

Get the value of the variable binding I<NAME> in the sequence of
variable bindings.

=cut

sub result_binding_value_by_name ($$) {
  my($self,$name)=@_;
  my $node=&RDF::Redland::CORE::librdf_query_get_result_binding_value_by_name($self->{QUERY},$name);
  RDF::Redland::Node->_new_from_object($node);
}


=item result_bindings

Get the names and values of all of the variable bindings in the
sequence of variable bindings as a hash

=cut

sub result_bindings ($) {
  my($self)=shift;
  my(%results);
  my $count=&RDF::Redland::CORE::librdf_query_get_bindings_count($self->{QUERY});
  for (my $i=0; $i < $count; $i++) {
    $results{$self->result_binding_name($i)}=$self->result_binding_value($i);
  }
  return %results;
}


=item bindings_count

Return the size of the sequence of variable bindings.

=cut

sub bindings_count ($) {
  my($self)=@_;
  return &RDF::Redland::CORE::librdf_query_get_bindings_count($self->{QUERY});
}


=item next_result

Move to the next result in the sequence of variable bindings.

=cut

sub next_result ($) {
  my($self)=@_;
  return &RDF::Redland::CORE::librdf_query_next_result($self->{QUERY});
}


=pod

=back

=head1 SEE ALSO

L<RDF::Redland::URI>

=head1 AUTHOR

Dave Beckett - http://purl.org/net/dajobe/

=cut

1;
