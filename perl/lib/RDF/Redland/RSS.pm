# -*- Mode: Perl -*-
#
# RSS.pm - Redland Perl RSS 1.0 module
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

use Redland;


package RDF::RSS;

use RDF::Model;

@ISA=qw(RDF::Model);

use vars qw($NS_URL);

$NS_URL="http://purl.org/rss/1.0/";

# CONSTRUCTOR
# Do it all here, create storage (in memory), model, parse it.
sub new ($$;$) {
  my($proto,$source_uri_string,$base_uri_string)=@_;
  my $class = ref($proto) || $proto;

  $base_uri_string ||= $source_uri_string;

  my $base_uri=new RDF::URI $base_uri_string;
  return undef if !$base_uri;

  my $source_uri=new RDF::URI $source_uri_string;
  return undef if !$source_uri;

  my $storage=new RDF::Storage("hashes", "rss", 
			       "new='yes',write='yes',hash-type='memory'");
  return undef if !$storage;
  
  my $self=new RDF::Model($storage, "");
  return undef if !$self;

  my $parser=new RDF::Parser('repat');
  return undef if !$parser;

  $parser->parse_into_model($source_uri, $base_uri, $self);

  bless ($self, $class); # reconsecrate as RDF::RSS
  return $self;
}


# CONSTRUCTOR
# Use an existing, open model to work on
sub new_from_model ($$) {
  my($proto,$model)=@_;
  my $class = ref($proto) || $proto;
  my $self  = $model;

  bless ($self, $class); # reconsecrate as RDF::RSS
  return $self;
}


# DESTRUCTOR
sub DESTROY ($) {
  my $self=shift;
  warn "RDF::RSS DESTROY\n" if $RDF::Debug;
}



sub _find_by_type ($$) {
  my($self,$type_value)=@_;

  my $rdf_type=RDF::Node->new_from_uri_string("http://www.w3.org/1999/02/22-rdf-syntax-ns#type");
  return () if !$rdf_type;

  my $object=RDF::Node->new_from_uri_string($type_value);
  return () if !$object;

  my(@results)=$self->get_sources($rdf_type, $object);

  # Turn the nodes into RDF::RSS:Node-s
  @results=map { RDF::RSS::Node->new($self,$_) } @results;

  return(@results);
}


sub channels ($) {
  shift->_find_by_type($NS_URL.'channel');
}

sub items ($) {
  shift->_find_by_type($NS_URL.'item');
}

sub image ($) {
  shift->_find_by_type($NS_URL.'image');
}

sub textinput ($) {
  shift->_find_by_type($NS_URL.'textinput');
}



package RDF::RSS::Node;

use RDF::Node;

@ISA=qw(RDF::Node);

# CONSTRUCTOR
sub new ($$$) {
  my($proto,$model,$node)=@_;
  my $class = ref($proto) || $proto;

  warn "RDF::RSS::Node::new in model $model with node $node\n" if $RDF::Debug;

  return undef if !$model || !$node;

  my $self  = RDF::Node->new_from_node($node);
  return undef if !$self;

  $self->{MODEL}=$model;

  bless ($self, $class); # reconsecrate as RDF::RSS::Node
  return $self;
}

sub DESTROY ($) {
  my $self=shift;
  warn "RDF::RSS::Node DESTROY\n" if $RDF::Debug;
}


sub _find_targets_by_predicate ($$) {
  my($self,$uri_string)=@_;
  warn "RDF::RSS::_find_targets_by_predicate from $self with predicate $uri_string\n" if $RDF::Debug;

  my $predicate=RDF::Node->new_from_uri_string($uri_string);
  return () if !$predicate;

  my(@targets)=$self->{MODEL}->get_targets($self,$predicate);
  warn "RDF::RSS::_find_targets_by_predicate returned ",scalar @targets, " results\n" if $RDF::Debug;

  # Convert list of RDF::Node-s into list of RDF::RSS:Node-s
  return map { RDF::RSS::Node->new($self->{MODEL}, $_) } @targets;
}

# Convienience accessors

# for all (channel, image, item, textinput) resources
sub title ($) {
  my(@r)=shift->_find_targets_by_predicate($RDF::RSS::NS_URL.'title');
  return wantarray ? @r : $r[0];
}

sub link ($) {
  my(@r)=shift->_find_targets_by_predicate($RDF::RSS::NS_URL.'link');
  return wantarray ? @r : $r[0];
}

# for channel, item, textinput resources
sub description ($) {
  my(@r)=shift->_find_targets_by_predicate($RDF::RSS::NS_URL.'description');
  return wantarray ? @r : $r[0];
}

# for image, item, textinput resources
sub inchannel ($) {
  my(@r)=shift->_find_targets_by_predicate($RDF::RSS::NS_URL.'inchannel');
  return wantarray ? @r : $r[0];
}

# for image resources
sub image_url ($) {
  my(@r)=shift->_find_targets_by_predicate($RDF::RSS::NS_URL.'url');
  return wantarray ? @r : $r[0];
}

# for textinput resources
sub name ($) {
  my(@r)=shift->_find_targets_by_predicate($RDF::RSS::NS_URL.'name');
  return wantarray ? @r : $r[0];
}


# -------------------------
# Methods for channels only

# always returns a list of nodes
sub items ($) {
  my $self=shift;

  my $items_predicate=RDF::Node->new_from_uri_string($RDF::RSS::NS_URL.'items');
  return () if !$items_predicate;

  # Get 1st resource inside <items> - i.e. the 1st rdf:Seq
  # This gets the entire list then just takes the first one
  my($seq_resource)=$self->{MODEL}->get_targets($self, $items_predicate);
  return () if !$seq_resource;

  $seq_resource=RDF::RSS::Node->new($self->{MODEL}, $seq_resource);

  # Find all rdf:_<n> properties from <rdf:Seq>
  my(@resources);
  for my $prop ($seq_resource->properties) {
    if($prop->uri->as_string =~ m%^http://www.w3.org/1999/02/22-rdf-syntax-ns#_(\d+)$%) {
       # Must want a list here otherwise get an RDF::Iterator object
       my($resource)=$seq_resource->{MODEL}->get_targets($seq_resource, $prop);
       push(@resources, [$1, $resource]);
     }
  }

  # In order - sort them by ordinal, convert to RDF::RSS::Node objects
  # and return
  return map {RDF::RSS::Node->new($self->{MODEL}, $_->[1])}
              sort {$a->[0] <=> $b->[0]} @resources;
}

# for channel (0 or 1 allowed)
sub image ($) {
  my $self=shift;

  my($image)=$self->_find_targets_by_predicate($RDF::RSS::NS_URL.'image');
  return undef if !$image;

  return RDF::RSS::Node->new($self->{MODEL}, $image);
}


# for channel (0 or 1 allowed)
sub textinput ($) {
  my $self=shift;

  my($textinput)=$self->_find_targets_by_predicate($RDF::RSS::NS_URL.'textinput');
  return undef if !$textinput;

  return RDF::RSS::Node->new($self->{MODEL}, $textinput);
}


# general property
sub property ($$) {
  my($self,$property)=@_;

  my(@targets)=$self->{MODEL}->get_targets($self,$property);

  # Convert list of RDF::Node-s into list of RDF::RSS:Node-s
  @targets=map { RDF::RSS::Node->new($self->{MODEL}, $_) } @targets;
  return wantarray ? @targets : $targets[0];
}

sub properties ($) {
  my($self)=@_;

  my $prop= RDF::Node->new_from_node($self);
  my $statement=RDF::Statement->new_from_nodes($prop, undef, undef);
  my(@arcs_out_statements)=$self->{MODEL}->find_statements($statement);

  # Convert list of RDF::Statement-s into list of RDF::RSS:Node-s
  # of predicates
  return map { RDF::RSS::Node->new($self->{MODEL}, $_->predicate) } @arcs_out_statements;
}

sub properties_with_ns_prefix ($$) {
  my($self,$ns_prefix)=@_;

  my(@arcs);
  my(%arcs_uris_seen);
  for my $arc ($self->properties) {
    my $uri=$arc->uri->as_string;
    next unless $uri =~ m%^$ns_prefix(.*)$% && !$arcs_uris_seen{$uri};
    push(@arcs, $arc);
    $arcs_uris_seen{$uri}=1;
  }
  @arcs;
}



1;
