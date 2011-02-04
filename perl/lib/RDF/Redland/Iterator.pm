# -*- Mode: Perl -*-
#
# Iterator.pm - Redland Perl RDF Iterator module
#
# Copyright (C) 2000-2003 David Beckett - http://www.dajobe.org/
# Copyright (C) 2000-2003 University of Bristol - http://www.bristol.ac.uk/
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

package RDF::Redland::Iterator;

use strict;

=pod

=head1 NAME

RDF::Redland::Iterator - Redland RDF Iterator Class

=head1 SYNOPSIS

  use RDF::Redland;

  ...
  my $iterator=$model->targets_iterator($source_node, $arc_node);
  while($iterator && !$iterator->end) {
    my $node=$iterator->current;
    ...
    $iterator->next;
  }

=head1 DESCRIPTION

This class is used to return lists of RDF::Redland::Node objects from a method
that returns an RDF::Redland::Iterator - commonly one of the
get_sources_iterator, get_targets_iterator or get_arcs_iterator
methods of the RDF::Redland::Model class.

This allows efficient retrieval of long lists of RDF::Redland::Node objects
but isn't really very Perl-friendly.  The get_sources, get_targets or
get_arcs methods of RDF::Redland::Model class return Perl lists and the
get_source, get_target and get_arc methods return single arbitrary
results.

=cut

######################################################################

=pod

=head1 CONSTRUCTORS

No public constructors - are created and returned from various methods
of classes including RDF::Redland::Model

=cut

# CONSTRUCTOR
# (main)
sub new ($$@) {
  my($proto,$object,@creators)=@_;
  return undef if !$object;

  warn "RDF::Redland::Iterator->new($object,@creators)\n" if $RDF::Redland::Debug;

  my $class = ref($proto) || $proto;
  my $self  = {};
  $self->{ITERATOR}=$object;
  # Keep around a reference to the objects that we use
  # so that perl does not destroy them while we are using them
  $self->{CREATORS}=[@creators];

  bless ($self, $class);
  return $self;
}

# DESTRUCTOR
sub DESTROY ($) {
  my $self=shift;
  warn "RDF::Redland::Iterator DESTROY\n" if $RDF::Redland::Debug;
  &RDF::Redland::CORE::librdf_free_iterator($self->{ITERATOR});
  $self->{CREATORS}=undef;
}


=head1 METHODS

=over

=item end

Return non 0 if the iterator has finished

=cut

sub end ($) {
  &RDF::Redland::CORE::librdf_iterator_end(shift->{ITERATOR});
}


=item current

Returns the current RDF::Redland::Node object from the iteration
or undef if the iteration is finished.

=cut

sub current ($) {
  my $object=&RDF::Redland::CORE::librdf_iterator_get_object(shift->{ITERATOR});
  RDF::Redland::Node->_new_from_object($object);
}


=item next

Moves the iterator to the next item, returns undef if
the iteration is finished.

=cut

sub next ($) {
  return &RDF::Redland::CORE::librdf_iterator_next(shift->{ITERATOR});
}


=item context

Returns the context RDF::Redland::Node object from the iteration
or undef if the iteration is finished.

=cut

sub context ($) {
  my $object=&RDF::Redland::CORE::librdf_iterator_get_context(shift->{ITERATOR});
  RDF::Redland::Node->_new_from_object($object);
}

=pod

=back

=head1 SEE ALSO

L<RDF::Redland::Model> and L<RDF::Redland::Node>

=head1 AUTHOR

Dave Beckett - http://www.dajobe.org/

=cut

1;
