# -*- Mode: Perl -*-
#
# URI.pm - Redland Perl RDF URI module
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

package RDF::Redland::URI;

use strict;

=pod

=head1 NAME

RDF::Redland::URI - Redland RDF URI Class

=head1 SYNOPSIS

  use RDF::Redland;

  my $uri=new RDF::Redland::URI("http://example.com/");

  my $uri2=RDF::Redland::URI->new_from_uri($uri);

  print $uri2->as_string,"\n";

=head1 DESCRIPTION

Represents a URI as a mostly-opaque object for identifying things
in the RDF world.  The URIs are also used for identifying features
for the RDF::Redland::Parser class. 

=cut

######################################################################

=pod

=head1 CONSTRUCTORS

=over

=item new STRING

Create a new RDF::Redland::URI object from a URI string.

=cut

sub new ($$) {
  my($proto,$arg)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};

  if(my $arg_class=ref($arg)) {
    if(UNIVERSAL::isa($arg_class, 'RDF::Redland::URI')) {
      return $arg->clone;
    } elsif (UNIVERSAL::isa($arg_class, 'URI')) {
      $arg=$arg->as_string;
    } else {
      die "RDF::Redland::URI::new - Cannot make a URI from an object of class $arg_class\n";
    }
  }
  
  $self->{URI}=&RDF::Redland::CORE::librdf_new_uri($RDF::Redland::World->{WORLD},$arg);
  return undef if !$self->{URI};

  bless ($self, $class);
  return $self;
}

=item clone URI

Copy a RDF::Redland::URI

=cut

sub clone ($) {
  my($uri)=@_;
  my $class = ref($uri);
  my $self  = {};

  if(!$class || $class ne 'RDF::Redland::URI') {
    die "RDF::Redland::URI::clone - Cannot copy a URI object not of class RDF::Redland::URI\n";
  }

  $self->{URI}=&RDF::Redland::CORE::librdf_new_uri_from_uri($uri->{URI});
  return undef if !$self->{URI};

  bless ($self, $class);
  return $self;
}


sub new_from_uri ($$) {
  my($proto,$uri)=@_;
  return $uri->clone;
}


sub _new_from_object ($$) {
  my($proto,$object)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};

  warn "RDF::Redland::URI->_new_from_object from object $object\n" if $RDF::Redland::Debug;

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
  warn "RDF::Redland::URI DESTROY\n" if $RDF::Redland::Debug;
  if($self->{URI}) {
    if(!$self->{DONT_FREE_ME}) {
      &RDF::Redland::CORE::librdf_free_uri($self->{URI});
    }
  }
}

=head1 METHODS

=over

=item as_string

Return the statement formatted as a string (UTF-8 encoded).

=cut

sub as_string ($) {
  &RDF::Redland::CORE::librdf_uri_to_string(shift->{URI});
}

=item equals URI

Return non zero if this uri is equal to URI

=cut

sub equals ($$) {
  my($self,$uri)=@_;
  &RDF::Redland::CORE::librdf_uri_equals($self->{URI}, $uri->{URI});
}

=pod

=back

=head1 OLD METHODS

=over

=item new_from_uri URI

Create a new RDF::Redland::URI object from RDF::Redland::URI I<URI>
(copy constructor)

Use $u=$uri->clone

=back

=head1 SEE ALSO

L<RDF::Redland::Parser>

=head1 AUTHOR

Dave Beckett - http://purl.org/net/dajobe/

=cut

1;
