# -*- Mode: Perl -*-
#
# RDF.pm - Redland Perl RDF Stream module
#
# $Id$
#
# Copyright (C) 2000-2001 David Beckett - http://purl.org/net/dajobe/
# Institute for Learning and Research Technology - http://www.ilrt.org/
# University of Bristol - http://www.bristol.ac.uk/
# 
# This package is Free Software or Open Source available under the
# following licenses (these are alternatives):
#   1. GNU Lesser General Public License (LGPL) Version 2
#   2. GNU General Public License (GPL) Version 2
#   3. Mozilla Public License (MPL) Version 1.1
# and no other versions of those licenses.
# 
# See INSTALL.html or INSTALL.txt at the top of this package for the
# full license terms.
# 
#

package RDF::Stream;

use strict;

use RDF::Statement;

=pod

=head1 NAME

RDF::Stream - Redland RDF Stream of RDF::Statement objects Class

=head1 SYNOPSIS

  use RDF;

  ...
  my $stream=$model->serialise;
  while($stream && !$stream->end) {
    my $statement=$stream->next;
    ...
  }

=head1 DESCRIPTION

Represents a sequence of RDF::Statement objects passed between
various Redland objects.

=cut

######################################################################

=pod

=head1 CONSTRUCTORS

No public constructors - are created and returned from various methods
of classes including RDF::Model and RDF::Parser

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
  warn "RDF::Stream DESTROY $self" if $RDF::Debug;
  &Redland::librdf_free_stream($self->{STREAM});
  $self->{CREATOR}=undef;
  warn "RDF::Stream DESTROY done\n" if $RDF::Debug;
}

=head1 METHODS

=over

=item end

Returns non 0 if the stream is finished.

=cut

sub end ($) {
  my $self=shift;
  return 1 if !$self->{STREAM};
  &Redland::librdf_stream_end($self->{STREAM});
}

=item next

Returns the next RDF::Statement object from the stream or undef if
the stream is finished.

=cut

sub next ($) {
  my $self=shift;
  return undef if !$self->{STREAM};
  
  my $statement=&Redland::librdf_stream_next($self->{STREAM});
  return undef if !$statement;

  # return a new statement created by the librdf stream object
  RDF::Statement->_new_from_object($statement, 1);
}

=pod

=back

=head1 SEE ALSO

L<RDF::Model> and L<RDF::Parser>

=head1 AUTHOR

Dave Beckett - http://purl.org/net/dajobe/

=cut

1;
