# -*- Mode: Perl -*-
#
# RDF.pm - Redland Perl RDF Storage module
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

package RDF::Storage;

use strict;

=pod

=head1 NAME

RDF::Storage - Redland RDF Storage Class

=head1 SYNOPSIS

  use RDF;
  my $storage=new RDF::Storage("hashes", "test", "new='yes',hash-type='memory'");
  ...

=head1 DESCRIPTION

Create objects for storing RDF::Model objects either persistentantly
or in memory.

=cut

######################################################################

=pod

=head1 CONSTRUCTORS

=over

=item new STORAGE_NAME NAME OPTIONS_STRING

Create a new RDF::Storage object for the storage factory named
I<STORAGE_NAME> with storage named I<NAME> and options
I<OPTIONS_STRING>.  The options are formatted in the form
key1='value1',key2='value2'.  The quotes are required.

=cut

sub new ($$$$) {
  my($proto,$storage_name,$name,$options_string)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};

  warn qq{RDF::Storage->new("$storage_name", "$name", "$options_string")\n} if $RDF::Debug;

  $self->{STORAGE}=&Redland::librdf_new_storage($RDF::World->{WORLD},$storage_name,$name,$options_string);
  return undef if !$self->{STORAGE};

  bless ($self, $class);
  return $self;
}

=item new_from_storage STORAGE

Create a new RDF::Storage object from RDF::Storage I<STORAGE> (copy
constructor).

=cut

sub new_from_storage ($$$) {
  my($proto,$storage)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};
  $self->{STORAGE}=&Redland::librdf_new_storage_from_storage($storage->{STORAGE});
  return undef if !$self->{STORAGE};

  bless ($self, $class);
  return $self;
}

=pod

=back

=cut

sub DESTROY ($) {
  my $self=shift;
  warn "RDF::Storage DESTROY $self" if $RDF::Debug;
  if(!$self->{STORAGE}) {
    warn "RDF::Storage DESTROY - librdf object gone - FIXME!\n" if $RDF::Debug;
  } else {
    &Redland::librdf_free_storage($self->{STORAGE});
  }
  warn "RDF::Storage DESTROY done\n" if $RDF::Debug;
}

=pod

=head1 SEE ALSO

L<RDF::Model>

=head1 AUTHOR

Dave Beckett - http://purl.org/net/dajobe/

=cut

1;
