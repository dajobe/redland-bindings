# -*- Mode: Perl -*-
#
# Storage.pm - Redland Perl RDF Storage module
#
# Copyright (C) 2000-2005 David Beckett - http://www.dajobe.org/
# Copyright (C) 2000-2005 University of Bristol - http://www.bristol.ac.uk/
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

The storage options may be given either as a Perl hash or as a
string. The string form are formatted in the form
key1='value1',key2='value2' and the single quotes are required. The
Perl hash form follows normal Perl conventions, and the boolean
options use normal Perl concepts of truth.

Currently defined storage options:

=over

=item new='yes'

Create a new storage erasing any existing one (boolean, default).

=item write='yes'

Provide write access to store (boolean, default)
otherwise is read only.

=item dir='DIR'          

Work in DIR directory when creating files.

=item mode='MODE'        

File creation mode, default is (octal) 0644
Takes decimal (123), hex (0x123) or octal (0123).

=item contexts='yes'     

Enable statement contexts.  Each statement can
be stored with an optional context Node and
the context retrieved after queries. Boolean.

=item hash-type='TYPE' (I<hashes> storage only)

Use the TYPE hash-type for I<hashes> storage.  Current defined types
are 'memory' and 'bdb' but is dependent on the hash factories
available.

=item index-predicates='yes' (I<hashes> storage only)

Enable indexing from predicates to (subject,object) which can in
particular be useful for rdf:type relations. Boolean.

=item bulk='no' (I<mysql> storage only)

Whether model/storage method add_statements should be optimized, until
a model/storage sync operation. Boolean.

=item merge='no' (I<mysql> storage only)

Whether to maintain a table with merged models. Boolean.

=back

Example, string form:

  $storage=new RDF::Redland::Storage("hashes", "test", 
                            "new='yes',hash-type='bdb',dir='.'");

Example, Perl hash form:

  $storage=new RDF::Redland::Storage("hashes", "test", 
                            {new=>1,hash-type=>'bdb',dir=>'.'});

Creates a new storage of the I<hashes> type (indexed hashes) named
I<test> (these will be file names or URIs if the storage is persistent)
and with options I<new='yes',hash-type='bdb',dir='.'> so a new storage
is created with BerkeleyDB (BDB) key:value hashes i.e. persistent and
in the current directory.

Example, Perl hash form:

  $storage=new RDF::Redland::Storage("mysql", "test", {host=>'localhost',database=>'testdb',user=>'testuser',new=>0,password=>'',contexts=>1});

Uses an existing storage of the I<mysql> type, named I<test> on
localhost with database name I<testdb> using a user I<testuser> and no
password. Contexts are enabled.


=cut

sub new ($$;$$) {
  my($proto,$storage_name,$name,$options)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};
  my $options_string = $options; # Default to use the string as is.
  if (ref($options) eq 'HASH') { # Build the string from a key=>value hash
    my %booleans = (new=>1,bulk=>1,merge=>1,write=>1,contexts=>1,'index-predicates'=>1);
    $options_string = '';
    while (my ($key, $value) = each(%{$options})) {
      if (defined($value)) {
        if (length($options_string) > 0) {
          $options_string .= ',';
        }
        if ($booleans{$key}) {
          $options_string .= "$key='". (($value) ? 'yes':'no') ."'";
        } else {
          $options_string .= "$key='$value'";
        }
      }
    }
  }

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

Dave Beckett - http://www.dajobe.org/

=cut

1;
