# -*- Mode: Perl -*-
#
# RDF.pm - Redland Perl RDF Parser module
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

package RDF::Parser;

use RDF::Stream;
use Redland;

=pod

=head1 NAME

RDF::Parser - Redland RDF Syntax Parsers Class

=head1 DESCRIPTION

This class represents parsers of various syntaxes that can deliver a
RDF model either as a RDF::Stream of RDF::Statement objects or
directly into an RDF::Model object.

=cut

######################################################################

=pod

=head1 CONSTRUCTORS

=over

=item new NAME MIME_TYPE URI

Create a new RDF::Parser object for a syntax parser named I<NAME>,
with MIME Type I<MIME_TYPE> and/or URI I<URI>.  I<NAME> is the only
required field.

=cut

# CONSTRUCTOR
# (main)
sub new ($$$$) {
  my($proto,$name,$mime_type,$uri)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};
  $mime_type ||="";

  if(defined $uri) {
    $uri=$uri->{URI};
  }

  $self->{PARSER}=&Redland::librdf_new_parser($name,$mime_type,$uri);
  return undef if !$self->{PARSER};

  bless ($self, $class);
  return $self;
}

=pod

=back

=cut

sub DESTROY ($) {
  warn "RDF::Parser DESTROY\n" if $RDF::Debug;
  &Redland::librdf_free_parser(shift->{PARSER});
}

=head1 METHODS

=over

=item parse_as_stream SOURCE_URI BASE_URI

Parse the syntax at the RDF::URI I<SOURCE_URI> with optional base
RDF::URI I<BASE_URI>.  If the base URI is given then the content is
parsed as if it was at the base URI rather than the source URI.

Returns an RDF::Stream of RDF::Statement objects or undef on failure.

=cut

sub parse_as_stream ($$$) {
  my($self,$uri,$base_uri)=@_;
  my $stream=&Redland::librdf_parser_parse_as_stream($self->{PARSER},$uri->{URI}, $base_uri->{URI});
  return new RDF::Stream($stream,$self,1);
}

=item parse_into_model SOURCE_URI BASE_URI MODEL

Parse the syntax at the RDF::URI I<SOURCE_URI> with optional base
RDF::URI I<BASE_URI> into RDF::Model I<MODEL>.  If the base URI is
given then the content is parsed as if it was at the base URI rather
than the source URI.

=cut

sub parse_into_model ($$$$) {
  my($self,$uri,$base_uri,$model)=@_;
  return &Redland::librdf_parser_parse_into_model($self->{PARSER},$uri->{URI},$base_uri->{URI},$model->{MODEL});
}

=item feature URI [VALUE]

Get/set a parser feature.  The feature is named via RDF::URI I<URI>
and the value is a string.  If I<VALUE> is given, the feature is set
to that value, otherwise the current value is returned.

=cut

sub feature ($$;$) {
  my($self,$uri,$value)=@_;

  warn "RDF::Parser->feature('$uri', '$value')\n" if $RDF::Debug;
  $uri=RDF::URI->new($uri)
    unless ref $uri;

  return &Redland::librdf_parser_get_feature($self->{PARSER},$uri->{URI})
    unless $value;

  return &Redland::librdf_parser_set_feature($self->{PARSER},$uri->{URI},$value);
}


=pod

=back

=head1 SEE ALSO

L<RDF::URi>

=head1 AUTHOR

Dave Beckett - http://purl.org/net/dajobe/

=cut

1;
