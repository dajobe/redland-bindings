# -*- Mode: Perl -*-
#
# Model.pm - Redland Perl RDF Model module
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


package RDF::Redland::Model;

use strict;

use RDF::Redland::Iterator;
use RDF::Redland::Stream;

=pod

=head1 NAME

RDF::Redland::Model - Redland RDF Model Class

=head1 SYNOPSIS

  use RDF::Redland;
  my $storage=new RDF::Redland::Storage("hashes", "test", "new='yes',hash-type='memory'");
  my $model=new RDF::Redland::Model($storage, "");
  ...

  my(@sources)=$model->targets($predicate_node, $object_node);

  ...

=head1 DESCRIPTION

Manipulate the RDF model.

=cut

######################################################################

=pod

=head1 CONSTRUCTORS

=over

=item new STORAGE OPTIONS_STRING

=item new_with_options STORAGE OPTIONS_HASH 

Create a new RDF::Redland::Model object using RDF::Redland::Storage object I<STORAGE>
with a options.  The options can be given either as a string in
the first form as I<OPTIONS_STRING>.  The options take the form
key1='value1',key2='value2'.  The quotes are required.   In the
second case I<OPTIONS_HASH> is a reference to a Perl hash of options.

=cut

sub new ($$$) {
  my($proto,$storage,$options_string)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};

  warn qq{RDF::Redland::Model->new(storage, "$options_string")\n} if $RDF::Redland::Debug;
  
  $self->{MODEL}=&RDF::Redland::CORE::librdf_new_model($RDF::Redland::World->{WORLD},$storage->{STORAGE},$options_string);
  return undef if !$self->{MODEL};

  # keep a reference to storage so model is always destroyed before storage
  $self->{STORAGE}=$storage;
  bless ($self, $class);
  return $self;
}


sub new_with_options ($$$) {
  my($proto,$storage,$options)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};
  my $options_hash=RDF::Redland::Hash->new_from_perl_hash($options);
  $self->{MODEL}=&RDF::Redland::CORE::librdf_new_model_with_options($storage->{STORAGE},$options_hash->{HASH});
  return undef if !$self->{MODEL};

  # keep a reference to storage so model is always destroyed before storage
  $self->{STORAGE}=$storage;
  bless ($self, $class);
  return $self;
}


=item new_from_model MODEL

Create a new model from an existing RDF::Redland::Model I<MODEL> (copy constructor).

=cut

sub new_from_model ($$) {
  my($proto,$model)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};
  $self->{MODEL}=&RDF::Redland::CORE::librdf_new_model_from_model($model->{MODEL});
  return undef if !$self->{MODEL};

  bless ($self, $class);
  return $self;
}

=pod

=back

=cut

# DESTRUCTOR
sub DESTROY ($) {
  my $self=shift;
  warn "RDF::Redland::Model DESTROY $self" if $RDF::Redland::Debug;
  if(!$self->{MODEL}) {
    warn "RDF::Redland::Model DESTROY - librdf object gone - FIXME!\n" if $RDF::Redland::Debug;
  } else {
    &RDF::Redland::CORE::librdf_free_model($self->{MODEL});
  }
  warn "RDF::Redland::Model DESTROY done\n" if $RDF::Redland::Debug;
}


=head1 METHODS

=over

=item size

Return the size of the model (number of statements).

=cut

sub size ($) {
  &RDF::Redland::CORE::librdf_model_size(shift->{MODEL});
}


=item add SUBJECT PREDICATE OBJECT

Add a new statement to the model with RDF::Redland::Node I<SUBJECT>,
I<PREDICATE> and I<OBJECT>.

=cut

sub add ($$$$) {
  my($self,$subject,$predicate,$object)=@_;
  return &RDF::Redland::CORE::librdf_model_add($self->{MODEL},$subject->{NODE},$predicate->{NODE},$object->{NODE});
}

=item add_string_literal_statement SUBJECT PREDICATE STRING XML_LANGUAGE IS_WF

Add a new statement to the model containing a literal string object
I<STRING> with XML language (xml:lang attribute) I<XML_LANGUAGE>
and if content is well formed XML, when I<IS_WF> is non 0.  I<XML_LANGUAGE>
is optional can can be set to undef.

=cut

sub add_string_literal_statement ($$$$$$$) {
  my($self,$subject,$predicate,$string,$xml_language,$is_wf_xml)=@_;
  return &RDF::Redland::CORE::librdf_model_add_string_literal_statement($self->{MODEL},$subject->{NODE},$predicate->{NODE},$string,$xml_language,$is_wf_xml);
}

=item add_statement STATEMENT

Add RDF::Redland::Statement I<STATEMENT> to the model.

=cut

sub add_statement ($$) {
  my($self,$statement)=@_;
  &RDF::Redland::CORE::librdf_model_add_statement($self->{MODEL},$statement->{STATEMENT});
}

=item add_statements STREAM

Add the statements from the RDF::Redland::Stream I<STREAM> to the model.

=cut

sub add_statements ($$) {
  my($self,$statement_stream)=@_;
  return &RDF::Redland::CORE::librdf_model_add_statements($self->{MODEL},$statement_stream->{STREAM});
}

=item remove_statement STATEMENT

Remove RDF::Redland::Statement I<STATEMENT> from the model.

=cut

sub remove_statement ($$) {
  my($self,$statement)=@_;
  return &RDF::Redland::CORE::librdf_model_remove_statement($self->{MODEL},$statement->{STATEMENT});
}

=item contains_statement STATEMENT

Return non 0 if the model contains RDF::Redland::Statement I<STATEMENT>.

=cut

sub contains_statement ($$) {
  my($self,$statement)=@_;
  return &RDF::Redland::CORE::librdf_model_contains_statement($self->{MODEL},$statement->{STATEMENT});
}

=item serialise

Return a new RDF::Redland::Stream object seralising the model as RDF::Redland::Statement
objects.

=cut

sub serialise ($) {
  my $self=shift;
  my $stream=&RDF::Redland::CORE::librdf_model_serialise($self->{MODEL});
  return new RDF::Redland::Stream($stream,$self);
}

# for alternate spelling
sub serialize ($) {
  shift->serialise;
}

=item find_statements STATEMENT

Find all matching statements in the model matching partial RDF::Redland::Statement
I<STATEMENT> (any of the subject, predicate, object RDF::Redland::Node can be undef).

In an array context, returns an array of the matching RDF::Redland::Statement
objects.  In a scalar context, returns the RDF::Redland::Stream object
representing the results.

=cut

sub find_statements ($$) {
  my($self,$statement)=@_;
  my $stream=&RDF::Redland::CORE::librdf_model_find_statements($self->{MODEL},$statement->{STATEMENT});
  my $user_stream=new RDF::Redland::Stream($stream,$self);
  return $user_stream if !wantarray;
  
  my(@results)=();
  while(!$user_stream->end) {
    push(@results, $user_stream->next);
  }
  $user_stream=undef;

  @results;
}

=item sources ARC TARGET

Get all source RDF::Redland::Node objects for a given arc I<ARC>, target I<TARGET>>
RDF::Redland::Node objects as a list of RDF::Redland::Node objects.

=cut

sub sources ($$$) {
  my($self,$arc,$target)=@_;
  my $iterator=&RDF::Redland::CORE::librdf_model_get_sources($self->{MODEL},$arc->{NODE},$target->{NODE});
  return () if !$iterator;
  my $user_iterator=new RDF::Redland::Iterator($iterator,$self,$arc,$target);
  return () if !$user_iterator;

  my(@results)=();
  while(!$user_iterator->end) {
    push(@results, $user_iterator->next);
  }
  $user_iterator=undef;

  @results;
}

=item arcs SOURCE TARGET

Get all arc RDF::Redland::Node objects for a given source I<SOURCE>, target I<TARGET>
RDF::Redland::Node objects as a list of RDF::Redland::Node objects.

=cut

sub arcs ($$$) {
  my($self,$source,$target)=@_;
  my $iterator=&RDF::Redland::CORE::librdf_model_get_arcs($self->{MODEL},$source->{NODE},$target->{NODE});
  return () if !$iterator;
  my $user_iterator=new RDF::Redland::Iterator($iterator,$self,$source,$target);
  return () if !$user_iterator;
  
  my(@results)=();
  while(!$user_iterator->end) {
    push(@results, $user_iterator->next);
  }
  $user_iterator=undef;

  @results;
}

=item targets SOURCE ARC

Get all target RDF::Redland::Node objects for a given source I<SOURCE>, arc I<ARC>
RDF::Redland::Node objects as a list of RDF::Redland::Node objects.

=cut

sub targets ($$$) {
  my($self,$source,$arc)=@_;
  my $iterator=&RDF::Redland::CORE::librdf_model_get_targets($self->{MODEL},$source->{NODE},$arc->{NODE});
  return () if !$iterator;
  my $user_iterator=new RDF::Redland::Iterator($iterator,$self,$source,$arc);
  return () if !$user_iterator;
  
  my(@results)=();
  while(!$user_iterator->end) {
    push(@results, $user_iterator->next);
  }
  $user_iterator=undef;

  @results;
}

=item sources_iterator ARC TARGET

Get all source RDF::Redland::Node objects for a given arc I<ARC>, target
I<TARGET> RDF::Redland::Node objects as an RDF::Redland::Iterator or undef on failure.

=cut

sub sources_iterator ($$$) {
  my($self,$arc,$target)=@_;
  my $iterator=&RDF::Redland::CORE::librdf_model_get_sources($self->{MODEL},$arc->{NODE},$target->{NODE});
  my $user_iterator=new RDF::Redland::Iterator($iterator,$self,$arc,$target);

  $user_iterator;
}

=item arcs_iterator SOURCE TARGET

Get all arc RDF::Redland::Node objects for a given source I<SOURCE>, target
I<TARGET> RDF::Redland::Node objects as an RDF::Redland::Iterator or undef on failure.

=cut

sub arcs_iterator ($$$) {
  my($self,$source,$target)=@_;
  my $iterator=&RDF::Redland::CORE::librdf_model_get_arcs($self->{MODEL},$source->{NODE},$target->{NODE});
  return new RDF::Redland::Iterator($iterator,$self,$source,$target);
}

=item targets_iterator SOURCE ARC

Get all target RDF::Redland::Node objects for a given source I<SOURCE>, arc I<ARC>
RDF::Redland::Node objects as an RDF::Redland::Iterator or undef on failure.

=cut

sub targets_iterator ($$$) {
  my($self,$source,$arc)=@_;
  my $iterator=&RDF::Redland::CORE::librdf_model_get_targets($self->{MODEL},$source->{NODE},$arc->{NODE});
  return new RDF::Redland::Iterator($iterator,$self,$source,$arc);
}

=item source ARC TARGET

Get one source RDF::Redland::Node object that matches a given arc I<ARC>,
target I<TARGET> RDF::Redland::Node objects or undef if there is no match.

=cut

sub source ($$$) {
  my($self,$arc,$target)=@_;
  my $node=&RDF::Redland::CORE::librdf_model_get_source($self->{MODEL},$arc->{NODE},$target->{NODE});
  return $node ? RDF::Redland::Node->_new_from_object($node,1) : undef;
}

=item arc SOURCE TARGET

Get one arc RDF::Redland::Node object that matches a given source I<SOURCE>,
target I<TARGET> RDF::Redland::Node objects or undef if there is no match.

=cut

sub arc ($$$) {
  my($self,$source,$target)=@_;
  my $node=&RDF::Redland::CORE::librdf_model_get_arc($self->{MODEL},$source->{NODE},$target->{NODE});
  return $node ? RDF::Redland::Node->_new_from_object($node,1) : undef;
}

=item target SOURCE ARC

Get one target RDF::Redland::Node object that matches a given source
I<SOURCE>, arc I<ARC> RDF::Redland::Node objects or undef if there is no
match.

=cut

sub target ($$$) {
  my($self,$source,$arc)=@_;
  my $node=&RDF::Redland::CORE::librdf_model_get_target($self->{MODEL},$source->{NODE},$arc->{NODE});
  return $node ? RDF::Redland::Node->_new_from_object($node,1) : undef;
}

=pod

=back

=head1 SEE ALSO

L<RDF::Redland::Storage>, L<RDF::Redland::Node> and L<RDF::Redland::Statement>

=head1 AUTHOR

Dave Beckett - http://purl.org/net/dajobe/

=cut

1;
