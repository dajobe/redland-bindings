# -*- Mode: Perl -*-
#
# RDF.pm - Redland Perl RDF Iterator module
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

package RDF::Iterator;

use Redland;

=pod

=head1 NAME

RDF::Iterator - Redland RDF Iterator Class

=head1 DESCRIPTION

This class is used to return lists of RDF::Node objects from a method
that returns an RDF::Iterator - commonly one of the
get_sources_iterator, get_targets_iterator or get_arcs_iterator
methods of the RDF::Model class.

This allows efficient retrieval of long lists of RDF::Node objects
but isn't really very Perl-friendly.  The get_sources, get_targets or
get_arcs methods of RDF::Model class return Perl lists and the
get_source, get_target and get_arc methods return single arbitrary
results.

=cut

######################################################################

=pod

=head1 CONSTRUCTORS

No public constructors - are created and returned from various methods
of classes including RDF::Model

=cut

# CONSTRUCTOR
# (main)
sub new ($$@) {
  my($proto,$object,@creators)=@_;
  return undef if !$object;

  my $class = ref($proto) || $proto;
  my $self  = {};
  $self->{ITERATOR}=$object;
  # Keep around a reference to the objects that created the iterator
  # so that perl does not destroy us before them.
  $self->{CREATORS}=[@creators];

  bless ($self, $class);
  return $self;
}

# DESTRUCTOR
sub DESTROY ($) {
  warn "RDF::Iterator DESTROY\n" if $RDF::Debug;
  &Redland::librdf_free_iterator($shift->{ITERATOR});
}


=head1 METHODS

=over

=item have_elements

Return non 0 if the iterator has more RDF::Node objects.

=cut

sub have_elements ($) {
  &Redland::librdf_iterator_have_elements(shift->{ITERATOR});
}

=item next

Returns the next RDF::Node object from iteration or undef if
the iteration is finished.

=cut

sub next ($) {
  # return a new (1) node (2)owned by the librdf iterator object
  # Reasons: (1) at the user API level the iterator only returns nodes
  #          (2) the node returned is shared with the iterator
  my $object=&Redland::librdf_iterator_get_next(shift->{ITERATOR});
  return undef if !$object;

  warn "RDF::Iterator::next object is $object\n" if $RDF::Debug;
  RDF::Node->_new_from_object($object,0);
}

=pod

=back

=head1 SEE ALSO

L<RDF::Model> and L<RDF::Node>

=head1 AUTHOR

Dave Beckett - http://purl.org/net/dajobe/

=cut

1;
