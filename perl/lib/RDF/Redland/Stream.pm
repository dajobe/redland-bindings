# -*- Mode: Perl -*-
#
# Stream.pm - Redland Perl RDF Stream module
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

package RDF::Redland::Stream;

use strict;

use RDF::Redland::Statement;

=pod

=head1 NAME

RDF::Redland::Stream - Redland RDF Stream of RDF::Redland::Statement objects Class

=head1 SYNOPSIS

  use RDF::Redland;

  ...
  my $stream=$model->serialise;
  while($stream && !$stream->end) {
    my $statement=$stream->next;
    ...
  }

=head1 DESCRIPTION

Represents a sequence of RDF::Redland::Statement objects passed between
various Redland objects.

=cut

######################################################################

=pod

=head1 CONSTRUCTORS

No public constructors - are created and returned from various methods
of classes including RDF::Redland::Model and RDF::Redland::Parser

=cut

sub new ($$$) {
  my($proto,$object,$creator)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};
  $self->{STREAM}=$object;

  # Keep around a reference to the object that created the stream so
  # that perl destroys us before it.
  $self->{CREATOR}=$creator;

  bless ($self, $class);
  return $self;
}

# DESTRUCTOR
sub DESTROY ($) {
  my $self=shift;
  warn "RDF::Redland::Stream DESTROY $self" if $RDF::Redland::Debug;
  &RDF::Redland::CORE::librdf_free_stream($self->{STREAM});
  $self->{CREATOR}=undef;
  warn "RDF::Redland::Stream DESTROY done\n" if $RDF::Redland::Debug;
}

=head1 METHODS

=over

=item end

Returns non 0 if the stream is finished.

=cut

sub end ($) {
  my $self=shift;
  return 1 if !$self->{STREAM};
  &RDF::Redland::CORE::librdf_stream_end($self->{STREAM});
}

=item next

Returns the next RDF::Redland::Statement object from the stream or undef if
the stream is finished.

=cut

sub next ($) {
  my $self=shift;
  return undef if !$self->{STREAM};
  
  my $statement=&RDF::Redland::CORE::librdf_stream_next($self->{STREAM});
  return undef if !$statement;

  # return a new statement created by the librdf stream object
  RDF::Redland::Statement->_new_from_object($statement, 1);
}

=pod

=back

=head1 SEE ALSO

L<RDF::Redland::Model> and L<RDF::Redland::Parser>

=head1 AUTHOR

Dave Beckett - http://purl.org/net/dajobe/

=cut

1;
