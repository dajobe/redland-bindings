# -*- Mode: Perl -*-
#
# RDF.pm - Redland Perl RDF Model module
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


package RDF::Model;

use RDF::Iterator;
use RDF::Stream;

use Redland;

# CONSTRUCTOR
# (main)
sub new ($$$) {
  my($proto,$storage,$options_string)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};
  $self->{MODEL}=&Redland::librdf_new_model($storage->{STORAGE},$options_string);
  # keep a reference around so storage object is destroyed after this
  $self->{STORAGE}=$storage;
  bless ($self, $class);
  return $self;
}

sub new_with_options ($$$) {
  my($proto,$storage,$options)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};
  my $options_hash=RDF::Hash->new_from_perl_hash($options);
  $self->{MODEL}=&Redland::librdf_new_model_with_options($storage->{STORAGE},$options_hash->{HASH});
  # keep a reference around so storage object is destroyed after this
  $self->{STORAGE}=$storage;
  bless ($self, $class);
  return $self;
}

sub new_from_model ($$) {
  my($proto,$model)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};
  $self->{MODEL}=&Redland::librdf_new_model_from_model($storage->{STORAGE},$model->{MODEL});
  # keep a reference around so storage object is destroyed after this
  $self->{STORAGE}=$storage;
  bless ($self, $class);
  return $self;
}

# DESTRUCTOR
sub DESTROY ($) {
  &Redland::librdf_free_model(shift->{MODEL});
}

# Methods

sub size ($) {
  &Redland::librdf_model_size(shift->{MODEL});
}

sub add ($$$$) {
  my($self,$subject,$predicate,$object)=@_;
  return &Redland::librdf_model_add($self->{MODEL},$subject->{NODE},$predicate->{NODE},$object->{NODE});
}

sub add_string_literal_statement ($$$$$$$$) {
  my($self,$subject,$predicate,$string,$xml_language,$xml_space,$is_wf_xml)=@_;
  return &Redland::librdf_model_add_string_literal_statement($self->{MODEL},$subject->{NODE},$predicate->{NODE},$string,$xml_language,$xml_space,$is_wf_xml);
}

sub add_statement ($$) {
  my($self,$statement)=@_;
  &Redland::librdf_model_add_statement($self->{MODEL},$statement->{STATEMENT});

  # Tell node objects that they no longer own the node
  $self->{SUBJECT}->{NODE}=undef;
  $self->{PREDICATE}->{NODE}=undef;
  $self->{OBJECT}->{NODE}=undef;

  # Tell statement object that it no longer owns the statement
  $statement->{STATEMENT}=undef;
}

sub add_statements ($$) {
  my($self,$statement_stream)=@_;
  return &Redland::librdf_model_add_statements($self->{MODEL},$statement_stream->{STREAM});
}

sub remove_statement ($$) {
  my($self,$statement)=@_;
  return &Redland::librdf_model_remove_statement($self->{MODEL},$statement->{STATEMENT});
}

sub contains_statement ($$) {
  my($self,$statement)=@_;
  return &Redland::librdf_model_contains_statement($self->{MODEL},$statement->{STATEMENT});
}

sub serialise ($) {
  my $stream=&Redland::librdf_model_serialise(shift->{MODEL});
  return new RDF::Stream($stream);
}

sub find_statements ($$) {
  my($self,$statement)=@_;
  my $stream=&Redland::librdf_model_find_statements($self->{MODEL},$statement->{STATEMENT});
  return new RDF::Stream($stream);
}

sub get_sources ($$) {
  my($self,$arc,$target)=@_;
  my $iterator=&Redland::librdf_model_get_sources($self->{MODEL},$arc->{NODE},$target->{NODE});
  return new RDF::Iterator $iterator;
}

sub get_arcs ($$) {
  my($self,$source,$target)=@_;
  my $iterator=&Redland::librdf_model_get_arcs($self->{MODEL},$source->{NODE},$target->{NODE});
  return new RDF::Iterator $iterator;
}

sub get_targets ($$) {
  my($self,$source,$arc)=@_;
  my $iterator=&Redland::librdf_model_get_targets($self->{MODEL},$source->{NODE},$arc->{NODE});
  return new RDF::Iterator $iterator;
}

1;
