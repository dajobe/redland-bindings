# -*- Mode: Perl -*-
#
# Storage.pm - Redland Perl RDF Storage module
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

package RDF::Redland::Storage;

use strict;

=pod

=head1 NAME

RDF::Redland::Storage - Redland RDF Storage Class

=head1 SYNOPSIS

  use RDF::Redland;
  my $storage=new RDF::Redland::Storage("hashes", "test", "new='yes',hash-type='memory'");
  ...

=head1 DESCRIPTION

Create objects for storing RDF::Redland::Model objects either persistently
or in memory.

=cut

######################################################################

=pod

=head1 CONSTRUCTORS

=over

=item new STORAGE_NAME [NAME [OPTIONS_STRING]]

Create a new RDF::Redland::Storage object for the storage factory named
I<STORAGE_NAME> with storage named I<NAME> and storage options
I<OPTIONS_STRING> which are specific to the storage factory type.

The storage options are formatted in the form
key1='value1',key2='value2' and the single quotes are required.

Currently defined storage options:

=over

=item new='yes'

Create a new storage erasing any existing one (default).

=item write='yes'

Provide write access to store (default)
otherwise is read only.

=item dir='DIR'          

Work in DIR directory when creating files.

=item mode='MODE'        

File creation mode, default is (octal) 0644
Takes decimal (123), hex (0x123) or octal (0123).

=item contexts='yes'     

Enable statement contexts.  Each statement can
be stored with an optional context Node and
the context retrieved after queries.

=item hash-type='TYPE' (I<hashes> storage only)

Use the TYPE hash-type for I<hashes> storage.  Current defined types
are 'memory' and 'bdb' but is dependent on the hash factories
available.

=item index-predicates='yes' (I<hashes> storage only)

Enable indexing from predicates to (subject,object) which can in
particular be useful for rdf:type relations.

=back

Example:

  $storage=new RDF::Redland::Storage("hashes", "test", 
                            "new='yes',hash-type='bdb',dir='.'");

Creates a new storage of the I<hashes> type (indexed hashes) named
I<test> (these will be file names or URIs if the storage is persistent)
and with options I<new='yes',hash-type='bdb',dir='.'> so a new storage
is created with BerkeleyDB (BDB) key:value hashes i.e. persistent and
in the current directory.

=cut

sub new ($$;$$) {
  my($proto,$storage_name,$name,$options_string)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};

  warn qq{RDF::Redland::Storage->new("$storage_name", "$name", "$options_string")\n} if $RDF::Redland::Debug;

  $self->{STORAGE}=&RDF::Redland::CORE::librdf_new_storage($RDF::Redland::World->{WORLD},$storage_name,$name,$options_string);
  return undef if !$self->{STORAGE};

  bless ($self, $class);
  return $self;
}

=item new_from_storage STORAGE

Create a new RDF::Redland::Storage object from RDF::Redland::Storage I<STORAGE> (copy
constructor).  The new storage may have a new name chosen by the
storage factory.

=cut

sub new_from_storage ($$$) {
  my($proto,$storage)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};
  $self->{STORAGE}=&RDF::Redland::CORE::librdf_new_storage_from_storage($storage->{STORAGE});
  return undef if !$self->{STORAGE};

  bless ($self, $class);
  return $self;
}

=pod

=back

=cut

sub DESTROY ($) {
  my $self=shift;
  warn "RDF::Redland::Storage DESTROY $self" if $RDF::Redland::Debug;
  if(!$self->{STORAGE}) {
    warn "RDF::Redland::Storage DESTROY - librdf storage object gone\n" if $RDF::Redland::Debug;
  } else {
    &RDF::Redland::CORE::librdf_free_storage($self->{STORAGE});
  }
  warn "RDF::Redland::Storage DESTROY done\n" if $RDF::Redland::Debug;
}

=pod

=head1 SEE ALSO

L<RDF::Redland::Model>

=head1 AUTHOR

Dave Beckett - http://purl.org/net/dajobe/

=cut

1;
