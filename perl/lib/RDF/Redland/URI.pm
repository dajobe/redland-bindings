# -*- Mode: Perl -*-
#
# RDF.pm - Redland Perl RDF URI module
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
