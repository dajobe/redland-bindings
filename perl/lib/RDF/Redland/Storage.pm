# -*- Mode: Perl -*-
#
# RDF.pm - Redland Perl RDF Storage module
#
# $Id$
#
# Copyright (C) 2000 David Beckett - http://purl.org/net/dajobe/
# Institute for Learning and Research Technology, University of Bristol.
#
#    This package is Free Software available under either of two licenses
#    (see FAQS.html to see why):
# 
# 1. The GNU Lesser General Public License (LGPL)
# 
#    See http://www.gnu.org/copyleft/lesser.html or COPYING.LIB for the
#    full license text.
#      _________________________________________________________________
# 
#      Copyright (C) 2000 David Beckett, Institute for Learning and
#      Research Technology, University of Bristol. All Rights Reserved.
# 
#      This library is free software; you can redistribute it and/or
#      modify it under the terms of the GNU Lesser General Public License
#      as published by the Free Software Foundation; either version 2 of
#      the License, or (at your option) any later version.
# 
#      This library is distributed in the hope that it will be useful, but
#      WITHOUT ANY WARRANTY; without even the implied warranty of
#      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
#      Lesser General Public License for more details.
# 
#      You should have received a copy of the GNU Lesser General Public
#      License along with this library; if not, write to the Free Software
#      Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
#      USA
#      _________________________________________________________________
# 
#    NOTE - under Term 3 of the LGPL, you may choose to license the entire
#    library under the GPL. See COPYING for the full license text.
# 
# 2. The Mozilla Public License
# 
#    See http://www.mozilla.org/MPL/MPL-1.1.html or MPL.html for the full
#    license text.
# 
#    Under MPL section 13. I declare that all of the Covered Code is
#    Multiple Licensed:
#      _________________________________________________________________
# 
#      The contents of this file are subject to the Mozilla Public License
#      version 1.1 (the "License"); you may not use this file except in
#      compliance with the License. You may obtain a copy of the License
#      at http://www.mozilla.org/MPL/
# 
#      Software distributed under the License is distributed on an "AS IS"
#      basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
#      the License for the specific language governing rights and
#      limitations under the License.
# 
#      The Initial Developer of the Original Code is David Beckett.
#      Portions created by David Beckett are Copyright (C) 2000 David
#      Beckett, Institute for Learning and Research Technology, University
#      of Bristol. All Rights Reserved.
# 
#      Alternatively, the contents of this file may be used under the
#      terms of the GNU Lesser General Public License, in which case the
#      provisions of the LGPL License are applicable instead of those
#      above. If you wish to allow use of your version of this file only
#      under the terms of the LGPL License and not to allow others to use
#      your version of this file under the MPL, indicate your decision by
#      deleting the provisions above and replace them with the notice and
#      other provisions required by the LGPL License. If you do not delete
#      the provisions above, a recipient may use your version of this file
#      under either the MPL or the LGPL License.
#

package RDF::Storage;

use strict;

use Redland;

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

  $self->{STORAGE}=&Redland::librdf_new_storage($storage_name,$name,$options_string);
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
  warn "RDF::Storage DESTROY $self\n" if $RDF::Debug;
  &Redland::librdf_free_storage($self->{STORAGE}) if $self->{STORAGE};
  warn "RDF::Storage DESTROY done\n" if $RDF::Debug;
}

=pod

=head1 SEE ALSO

L<RDF::Model>

=head1 AUTHOR

Dave Beckett - http://purl.org/net/dajobe/

=cut

1;
