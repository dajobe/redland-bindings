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

use Redland;

# CONSTRUCTOR
# (main)
sub new ($) {
  my($proto)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};
  $self->{NODE}=&Redland::librdf_new_node;
  bless ($self, $class);
  return $self;
}

sub new_from_uri_string ($$) {
  my($proto,$uri_string)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};
  $self->{NODE}=&Redland::librdf_new_node_from_uri_string($uri_string);
  bless ($self, $class);
  return $self;
}

sub new_from_uri ($$) {
  my($proto,$uri)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};
  $self->{NODE}=&Redland::librdf_new_node_from_uri($uri->{URI});
  bless ($self, $class);
  return $self;
}

sub new_from_literal ($$$$$) {
  my($proto,$string,$xml_language,$xml_space,$is_wf_xml)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};
  $self->{NODE}=&Redland::librdf_new_node_from_literal($string,$xml_language,$xml_space,$is_wf_xml);
  bless ($self, $class);
  return $self;
}

sub new_from_node ($$) {
  my($proto,$node)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};
  $self->{NODE}=&Redland::librdf_new_node_from_node($node->{NODE});
  bless ($self, $class);
  return $self;
}

# internal constructor to build an object from a node created
# by librdf e.g. from the result of a iterator->next operation
# this is always shared (at present) so should not be freed
sub _new_from_object ($$) {
  my($proto,$object)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};
  warn "RDF::Node::_new_from_object from object $object\n" if $RDF::Debug;
  $self->{NODE}=$object;
  $self->{DONT_FREE_ME}=1;
  bless ($self, $class);
  return $self;
}

# DESTRUCTOR
sub DESTROY ($) {
  warn "RDF::Node DESTROY\n" if $RDF::Debug;
  my $self=shift;
  if($self->{NODE}) {
    if(!$self->{DONT_FREE_ME}) {
      warn "RDF::Node doing librdf_free_node on librdf node\n" if $RDF::Debug;
      &Redland::librdf_free_node($self->{NODE});
    }
  }
  warn "RDF::Node DESTROY done\n" if $RDF::Debug;
}

sub uri ($;$) {
  my($self,$uri)=@_;

  return &Redland::librdf_node_get_uri(shift->{NODE})
    unless $uri;

  return &Redland::librdf_node_set_uri($self->{NODE},$uri->{URI});
}

sub type ($;$) {
  my($self,$type)=@_;

  return &Redland::librdf_node_get_type(shift->{NODE})
    unless $type;

  return &Redland::librdf_node_set_type($self->{NODE},$type);
}

sub literal_value ($) {
  &Redland::librdf_node_get_literal_value(shift->{NODE});
}

sub literal_value_language ($) {
  &Redland::librdf_node_get_literal_value_language(shift->{NODE});
}

sub literal_value_xml_space ($) {
  &Redland::librdf_node_get_literal_value_xml_space(shift->{NODE});
}

sub literal_value_is_wf_xml ($) {
  &Redland::librdf_node_get_literal_value_is_wf_xml(shift->{NODE});
}

sub set_literal_value ($$$$$) {
  my($self,$value,$xml_language,$xml_space,$is_wf_xml)=@_;
  return &Redland::librdf_node_set_literal_value($self->{NODE},$value,$xml_language,$xml_space,$is_wf_xml);
}

sub as_string ($) {
  &Redland::librdf_node_to_string(shift->{NODE});
}

1;
