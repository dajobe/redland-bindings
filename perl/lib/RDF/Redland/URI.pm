# -*- Mode: Perl -*-
#
# RDF.pm - Redland Perl RDF URI module
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

package RDF::URI;

use strict;

=pod

=head1 NAME

RDF::URI - Redland RDF URI Class

=head1 SYNOPSIS

  use RDF;

  my $uri=new RDF::URI("http://example.com/");

  my $uri2=RDF::URI->new_from_uri($uri);

  print $uri2->as_string,"\n";

=head1 DESCRIPTION

Represents a URI as a mostly-opaque object for identifying things
in the RDF world.  The URIs are also used for identifying features
for the RDF::Parser class. 

=cut

######################################################################

=pod

=head1 CONSTRUCTORS

=over

=item new STRING

Create a new RDF::URI object from a URI string.

=cut

sub new ($$) {
  my($proto,$string)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};

  warn "RDF::URI->new('$string')\n" if $RDF::Debug;

  $self->{URI}=&Redland::librdf_new_uri($string);
  return undef if !$self->{URI};

  bless ($self, $class);
  return $self;
}

=item new_from_uri URI

Create a new RDF::URI object from RDF::URI I<URI> (copy constructor)

=cut

sub new_from_uri ($$) {
  my($proto,$uri)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};

  warn "RDF::URI->new_from_uri($uri)\n" if $RDF::Debug;

  # If the URI is a perl URI, use the above constructor
  if (UNIVERSAL::isa($uri, 'URI::http')) {
    return new($proto, $uri->as_string);
  }

  $self->{URI}=&Redland::librdf_new_uri_from_uri($uri->{URI});
  return undef if !$self->{URI};

  bless ($self, $class);
  return $self;
}


sub _new_from_object ($$) {
  my($proto,$object)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};

  warn "RDF::URI->_new_from_object from object $object\n" if $RDF::Debug;

  $self->{URI}=$object;
  $self->{DONT_FREE_ME}=1;

  bless ($self, $class);
  return $self;
}

=pod

=back

=cut

sub DESTROY ($) {
  my $self=shift;
  warn "RDF::URI DESTROY\n" if $RDF::Debug;
  if($self->{URI}) {
    if(!$self->{DONT_FREE_ME}) {
      &Redland::librdf_free_uri($self->{URI});
    }
  }
}

=head1 METHODS

=over

=item as_string

Return the statement formatted as a string (UTF-8 encoded).

=cut

sub as_string ($) {
  &Redland::librdf_uri_to_string(shift->{URI});
}

=pod

=back

=head1 SEE ALSO

L<RDF::Parser>

=head1 AUTHOR

Dave Beckett - http://purl.org/net/dajobe/

=cut

1;
