# -*- Mode: Perl -*-
#
# Serializer.pm - Redland Perl RDF Serializer module
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
#

package RDF::Redland::Serializer;

use strict;

use RDF::Redland::Stream;

=pod

=head1 NAME

RDF::Redland::Serializer - Redland RDF Serializing to Syntax Class

=head1 SYNOPSIS

  use RDF::Redland;

  ...
  my $serializer=new RDF::Redland::Serializer("ntriples");

  $serializer->serialize_model(STDOUT, $base_uri, $model);

=head1 DESCRIPTION

This class represents serializers that turn RDF graphs into various syntaxes.
from an RDF::Redland::Model object.

=cut

######################################################################

=pod

=head1 CONSTRUCTORS

=over

=item new [NAME [MIME_TYPE [URI]]]

Create a new RDF::Redland::Serializer object for a syntax serializer
named I<NAME>, with MIME Type I<MIME_TYPE> and/or URI I<URI>.  Any
field can be undef or omitted; if all are omitted, a random serializer
will be requested.

=cut

# CONSTRUCTOR
# (main)
sub new ($;$$$) {
  my($proto,$name,$mime_type,$uri)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};

  if(defined $uri) {
    $uri=$uri->{URI};
  }

  $self->{SERIALIZER}=&RDF::Redland::CORE::librdf_new_serializer($RDF::Redland::World->{WORLD},$name,$mime_type,$uri);
  return undef if !$self->{SERIALIZER};

  bless ($self, $class);
  return $self;
}

=pod

=back

=cut

sub DESTROY ($) {
  warn "RDF::Redland::Serializer DESTROY\n" if $RDF::Redland::Debug;
  &RDF::Redland::CORE::librdf_free_serializer(shift->{SERIALIZER});
}

=head1 METHODS

=over

=item serialize_model HANDLE BASE_URI MODEL

Serialize the RDF Graph I<MODEL> as syntax with the
base RDF::Redland::URI I<BASE_URI> to perl handle I<HANDLE>.

=cut

sub serialize_model ($$$$) {
  my($self,$handle,$base_uri,$model)=@_;
  return &RDF::Redland::CORE::librdf_serializer_serialize_model($self->{SERIALIZER},$handle, $base_uri->{URI},$model->{MODEL});
}

=item feature URI [VALUE]

Get/set a serializer feature.  The feature is named via
RDF::Redland::URI I<URI> and the value is a string.  If I<VALUE> is
given, the feature is set to that value, otherwise the current value
is returned.

=cut

sub feature ($$;$) {
  my($self,$uri,$value)=@_;

  warn "RDF::Redland::Serializer->feature('$uri', '$value')\n" if $RDF::Redland::Debug;
  $uri=RDF::Redland::URI->new($uri)
    unless ref $uri;

  return &RDF::Redland::CORE::librdf_serializer_get_feature($self->{SERIALIZER},$uri->{URI})
    unless $value;

  return &RDF::Redland::CORE::librdf_serializer_set_feature($self->{SERIALIZER},$uri->{URI},$value);
}


=pod

=back

=head1 SEE ALSO

L<RDF::Redland::Parser>,
L<RDF::Redland::URI>

=head1 AUTHOR

Dave Beckett - http://purl.org/net/dajobe/

=cut

1;
