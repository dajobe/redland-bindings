# -*- Mode: Perl -*-
#
# RDF.pm - Redland Perl RDF Node module
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

package RDF::Node;

use vars qw($Type_Resource $Type_Property $Type_Literal
	    $Type_Statement $Type_Bag $Type_Seq $Type_Alt $Type_Li
	    $Type_Model);

# FIXME:Should be the same as values of librdf_node_type enum in rdf_node.h
# and mechanically kept in sync.
$Type_Resource  = 1;
$Type_Property  = $Type_Resource;
$Type_Literal   = 2;
$Type_Statement = 3;
# FIXME: Are these sensible?
#$Type_Bag       = 4;
#$Type_Seq       = 5;
#$Type_Alt       = 6;
$Type_Li        = 7;
$Type_Model     = 8;
# FIXME: Needs to also match documentation near method type


use Redland;


=pod

=head1 NAME

RDF::Node - Redland RDF Node (RDF Resource, Property, Literal) Class

=head1 DESCRIPTION

This class represents nodes and arcs in the RDF model graph.  RDF
model Nodes are RDF resources and literals and RDF model Arcs are
properties.   RDF::Statement is a subclass of RDF::Node.

=cut

######################################################################

=pod

=head1 CONSTRUCTORS

=over

=item new

Create a new RDF::Node object

=cut


# CONSTRUCTOR
# (main)
sub new ($) {
  my($proto)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};
  $self->{NODE}=&Redland::librdf_new_node;
  return undef if !$self->{NODE};

  bless ($self, $class);
  return $self;
}

=item new_from_uri_string URI_STRING

Create a new RDF::Node object for a resource with URI I<URI_STRING>.

=cut

sub new_from_uri_string ($$) {
  my($proto,$uri_string)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};
  die "RDF::Node::new_from_uri_string - Cannot create node from empty URI\n"
    unless $uri_string;
  $self->{NODE}=&Redland::librdf_new_node_from_uri_string($uri_string);
  return undef if !$self->{NODE};

  bless ($self, $class);
  return $self;
}

=item new_from_uri URI

Create a new RDF::Node object for a resource with RDF::URI object
I<URI>.

=cut

sub new_from_uri ($$) {
  my($proto,$uri)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};
  $self->{NODE}=&Redland::librdf_new_node_from_uri($uri->{URI});
  return undef if !$self->{NODE};

  bless ($self, $class);
  return $self;
}

=item new_from_literal STRING XML_LANGUAGE XML_SPACE IS_WF

Create a new RDF::Node object for a literal value I<STRING> with XML
language (xml:lang attribute) I<XML_LANGUAGE>, XML space (xml:space)
I<XML_SPACE> and if content is well formed XML, when I<IS_WF> is non
0.  I<XML_LANGUAGE> and I<XML_SPACE> are optional can can be set to
undef.

=cut

sub new_from_literal ($$$$$) {
  my($proto,$string,$xml_language,$xml_space,$is_wf_xml)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};
  $self->{NODE}=&Redland::librdf_new_node_from_literal($string,$xml_language,$xml_space,$is_wf_xml);
  return undef if !$self->{NODE};

  bless ($self, $class);
  return $self;
}

=item new_from_node NODE

Create a new RDF::Node object from existing RDF::Node I<NODE> (copy
constructor).

=cut

sub new_from_node ($$) {
  my($proto,$node)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};
  $self->{NODE}=&Redland::librdf_new_node_from_node($node->{NODE});
  return undef if !$self->{NODE};

  bless ($self, $class);
  return $self;
}

# internal constructor to build an object from a node created
# by librdf e.g. from the result of a iterator->next operation
# this may be shared in which case it should not be freed
sub _new_from_object ($$$) {
  my($proto,$object,$free_me)=@_;
  return undef if !$object;
  my $class = ref($proto) || $proto;
  my $self  = {};
  warn "RDF::Node::_new_from_object from object $object, free_me=$free_me\n" if $RDF::Debug;

  $self->{NODE}=$object;
  $self->{DONT_FREE_ME}=!$free_me;
  bless ($self, $class);
  return $self;
}

=pod

=back

=cut

# DESTRUCTOR
sub DESTROY ($) {
  my $self=shift;
  if($self->{NODE}) {
    if(!$self->{DONT_FREE_ME}) {
      warn "RDF::Node doing librdf_free_node on librdf node\n" if $RDF::Debug;
      &Redland::librdf_free_node($self->{NODE});
    }
  }
  warn "RDF::Node DESTROY done\n" if $RDF::Debug;
}


=head1 METHODS

=over

=item uri [URI]

Get/set the URI of the node.  With no arguments, returns the current
URI as an RDF::URI object otherwise sets it to RDF::URI object I<URI>.

=cut

sub uri ($;$) {
  my($self,$uri)=@_;

  return RDF::URI->_new_from_object(&Redland::librdf_node_get_uri(shift->{NODE}))
    unless $uri;

  return &Redland::librdf_node_set_uri($self->{NODE},$uri->{URI});
}

=item type [TYPE]

Get/set the node type.  With no arguments, returns the current
type, otherwise sets it to I<TYPE>.  The current list of types are:

  $RDF::Node::Type_Resource
  $RDF::Node::Type_Property 
  $RDF::Node::Type_Literal
  $RDF::Node::Type_Statement
  $RDF::Node::Type_li
  $RDF::Node::Type_Model

=cut

sub type ($;$) {
  my($self,$type)=@_;

  return &Redland::librdf_node_get_type(shift->{NODE})
    unless $type;

  return &Redland::librdf_node_set_type($self->{NODE},$type);
}

=item literal_value

Get the node literal value string (when the node is of type
$RDF::Node::Type_Literal)

=cut

sub literal_value ($) {
  &Redland::librdf_node_get_literal_value(shift->{NODE});
}

=item literal_value_language

Get the node literal XML language (when the node is of type
$RDF::Node::Type_Literal) or undef if not present.

=cut

sub literal_value_language ($) {
  &Redland::librdf_node_get_literal_value_language(shift->{NODE});
}

=item literal_value_xml_space

Get the node literal XML Space property (when the node is of type
$RDF::Node::Type_Literal) or 0 if not present.

=cut

sub literal_value_xml_space ($) {
  &Redland::librdf_node_get_literal_value_xml_space(shift->{NODE});
}

=item literal_value_is_wf_xml

Return non 0 if the literal string is well formed XML (when the node
is of type $RDF::Node::Type_Literal).

=cut

sub literal_value_is_wf_xml ($) {
  &Redland::librdf_node_get_literal_value_is_wf_xml(shift->{NODE});
}

=item set_literal_value STRING XML_LANGUAGE XML_SPACE IS_WF

Set the node literal value to STRING with XML language (xml:lang
attribute) XML_LANGUAGE, XML space (xml:space) XML_SPACE and if
content is well formed XML, when IS_WF is non 0.  XML_LANGUAGE and
XML_SPACE are optional can can be set to undef.

=cut

sub set_literal_value ($$$$$) {
  my($self,$value,$xml_language,$xml_space,$is_wf_xml)=@_;
  return &Redland::librdf_node_set_literal_value($self->{NODE},$value,$xml_language,$xml_space,$is_wf_xml);
}

=item as_string

Return the RDF::Node formatted as a string (UTF-8 encoded).

=cut

sub as_string ($) {
  &Redland::librdf_node_to_string(shift->{NODE});
}

=pod

=back

=head1 SEE ALSO

L<RDF::Statement>

=head1 AUTHOR

Dave Beckett - http://purl.org/net/dajobe/

=cut

1;
