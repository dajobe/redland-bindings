# -*- Mode: Perl -*-
#
# RDF.pm - Redland Perl RDF Model module
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


package RDF::Model;

use strict;

use RDF::Iterator;
use RDF::Stream;

=pod

=head1 NAME

RDF::Model - Redland RDF Model Class

=head1 SYNOPSIS

  use RDF;
  my $storage=new RDF::Storage("hashes", "test", "new='yes',hash-type='memory'");
  my $model=new RDF::Model($storage, "");
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

Create a new RDF::Model object using RDF::Storage object I<STORAGE>
with a options.  The options can be given either as a string in
the first form as I<OPTIONS_STRING>.  The options take the form
key1='value1',key2='value2'.  The quotes are required.   In the
second case I<OPTIONS_HASH> is a reference to a Perl hash of options.

=cut

sub new ($$$) {
  my($proto,$storage,$options_string)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};

  warn qq{RDF::Model->new(storage, "$options_string")\n} if $RDF::Debug;
  
  $self->{MODEL}=&Redland::librdf_new_model($RDF::World->{WORLD},$storage->{STORAGE},$options_string);
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
  my $options_hash=RDF::Hash->new_from_perl_hash($options);
  $self->{MODEL}=&Redland::librdf_new_model_with_options($storage->{STORAGE},$options_hash->{HASH});
  return undef if !$self->{MODEL};

  # keep a reference to storage so model is always destroyed before storage
  $self->{STORAGE}=$storage;
  bless ($self, $class);
  return $self;
}


=item new_from_model MODEL

Create a new model from an existing RDF::Model I<MODEL> (copy constructor).

=cut

sub new_from_model ($$) {
  my($proto,$model)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};
  $self->{MODEL}=&Redland::librdf_new_model_from_model($model->{MODEL});
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
  warn "RDF::Model DESTROY $self" if $RDF::Debug;
  if(!$self->{MODEL}) {
    warn "RDF::Model DESTROY - librdf object gone - FIXME!\n" if $RDF::Debug;
  } else {
    &Redland::librdf_free_model($self->{MODEL});
  }
  warn "RDF::Model DESTROY done\n" if $RDF::Debug;
}


=head1 METHODS

=over

=item size

Return the size of the model (number of statements).

=cut

sub size ($) {
  &Redland::librdf_model_size(shift->{MODEL});
}


=item add SUBJECT PREDICATE OBJECT

Add a new statement to the model with RDF::Node I<SUBJECT>,
I<PREDICATE> and I<OBJECT>.

=cut

sub add ($$$$) {
  my($self,$subject,$predicate,$object)=@_;
  return &Redland::librdf_model_add($self->{MODEL},$subject->{NODE},$predicate->{NODE},$object->{NODE});
}

=item add_string_literal_statement SUBJECT PREDICATE STRING XML_LANGUAGE XML_SPACE IS_WF

Add a new statement to the model containing a literal string object
I<STRING> with XML language (xml:lang attribute) I<XML_LANGUAGE>, XML
space (xml:space) I<XML_SPACE> and if content is well formed XML,
when I<IS_WF> is non 0.  I<XML_LANGUAGE> and I<XML_SPACE> are
optional can can be set to undef.

=cut

sub add_string_literal_statement ($$$$$$$$) {
  my($self,$subject,$predicate,$string,$xml_language,$xml_space,$is_wf_xml)=@_;
  return &Redland::librdf_model_add_string_literal_statement($self->{MODEL},$subject->{NODE},$predicate->{NODE},$string,$xml_language,$xml_space,$is_wf_xml);
}

=item add_statement STATEMENT

Add RDF::Statement I<STATEMENT> to the model.

=cut

sub add_statement ($$) {
  my($self,$statement)=@_;
  &Redland::librdf_model_add_statement($self->{MODEL},$statement->{STATEMENT});
}

=item add_statements STREAM

Add the statements from the RDF::Stream I<STREAM> to the model.

=cut

sub add_statements ($$) {
  my($self,$statement_stream)=@_;
  return &Redland::librdf_model_add_statements($self->{MODEL},$statement_stream->{STREAM});
}

=item remove_statement STATEMENT

Remove RDF::Statement I<STATEMENT> from the model.

=cut

sub remove_statement ($$) {
  my($self,$statement)=@_;
  return &Redland::librdf_model_remove_statement($self->{MODEL},$statement->{STATEMENT});
}

=item contains_statement STATEMENT

Return non 0 if the model contains RDF::Statement I<STATEMENT>.

=cut

sub contains_statement ($$) {
  my($self,$statement)=@_;
  return &Redland::librdf_model_contains_statement($self->{MODEL},$statement->{STATEMENT});
}

=item serialise

Return a new RDF::Stream object seralising the model as RDF::Statement
objects.

=cut

sub serialise ($) {
  my $self=shift;
  my $stream=&Redland::librdf_model_serialise($self->{MODEL});
  return new RDF::Stream($stream,$self,1);
}

=item find_statements STATEMENT

Find all matching statements in the model matching partial RDF::Statement
I<STATEMENT> (any of the subject, predicate, object RDF::Node can be undef).

In an array context, returns an array of the matching RDF::Statement
objects.  In a scalar context, returns the RDF::Stream object
representing the results.

=cut

sub find_statements ($$) {
  my($self,$statement)=@_;
  my $stream=&Redland::librdf_model_find_statements($self->{MODEL},$statement->{STATEMENT});
  my $user_stream=new RDF::Stream($stream,$self);
  return $user_stream if !wantarray;
  
  my(@results)=();
  while(!$user_stream->end) {
    push(@results, $user_stream->next);
  }
  $user_stream=undef;

  @results;
}

=item sources ARC TARGET

Get all source RDF::Node objects for a given arc I<ARC>, target I<TARGET>>
RDF::Node objects as a list of RDF::Node objects.

=cut

sub sources ($$) {
  my($self,$arc,$target)=@_;
  my $iterator=&Redland::librdf_model_get_sources($self->{MODEL},$arc->{NODE},$target->{NODE});
  return () if !$iterator;
  my $user_iterator=new RDF::Iterator($iterator,$self,$arc,$target);
  return () if !$user_iterator;

  my(@results)=();
  while($user_iterator->have_elements) {
    push(@results, $user_iterator->next);
  }
  $user_iterator=undef;

  @results;
}

=item arcs SOURCE TARGET

Get all arc RDF::Node objects for a given source I<SOURCE>, target I<TARGET>
RDF::Node objects as a list of RDF::Node objects.

=cut

sub arcs ($$) {
  my($self,$source,$target)=@_;
  my $iterator=&Redland::librdf_model_get_arcs($self->{MODEL},$source->{NODE},$target->{NODE});
  return () if !$iterator;
  my $user_iterator=new RDF::Iterator($iterator,$self,$source,$target);
  return () if !$user_iterator;
  
  my(@results)=();
  while($user_iterator->have_elements) {
    push(@results, $user_iterator->next);
  }
  $user_iterator=undef;

  @results;
}

=item targets SOURCE ARC

Get all target RDF::Node objects for a given source I<SOURCE>, arc I<ARC>
RDF::Node objects as a list of RDF::Node objects.

=cut

sub targets ($$$) {
  my($self,$source,$arc)=@_;
  my $iterator=&Redland::librdf_model_get_targets($self->{MODEL},$source->{NODE},$arc->{NODE});
  return () if !$iterator;
  my $user_iterator=new RDF::Iterator($iterator,$self,$source,$arc);
  return () if !$user_iterator;
  
  my(@results)=();
  while($user_iterator->have_elements) {
    push(@results, $user_iterator->next);
  }
  $user_iterator=undef;

  @results;
}

=item sources_iterator ARC TARGET

Get all source RDF::Node objects for a given arc I<ARC>, target
I<TARGET> RDF::Node objects as an RDF::Iterator or undef on failure.

=cut

sub sources_iterator ($$$) {
  my($self,$arc,$target)=@_;
  my $iterator=&Redland::librdf_model_get_sources($self->{MODEL},$arc->{NODE},$target->{NODE});
  my $user_iterator=new RDF::Iterator($iterator,$self,$arc,$target);

  $user_iterator;
}

=item arcs_iterator SOURCE TARGET

Get all arc RDF::Node objects for a given source I<SOURCE>, target
I<TARGET> RDF::Node objects as an RDF::Iterator or undef on failure.

=cut

sub arcs_iterator ($$$) {
  my($self,$source,$target)=@_;
  my $iterator=&Redland::librdf_model_get_arcs($self->{MODEL},$source->{NODE},$target->{NODE});
  return new RDF::Iterator($iterator,$self,$source,$target);
}

=item targets_iterator SOURCE ARC

Get all target RDF::Node objects for a given source I<SOURCE>, arc I<ARC>
RDF::Node objects as an RDF::Iterator or undef on failure.

=cut

sub targets_iterator ($$$) {
  my($self,$source,$arc)=@_;
  my $iterator=&Redland::librdf_model_get_targets($self->{MODEL},$source->{NODE},$arc->{NODE});
  return new RDF::Iterator($iterator,$self,$source,$arc);
}

=item source ARC TARGET

Get one source RDF::Node object that matches a given arc I<ARC>,
target I<TARGET> RDF::Node objects or undef if there is no match.

=cut

sub source ($$$) {
  my($self,$arc,$target)=@_;
  my $node=&Redland::librdf_model_get_source($self->{MODEL},$arc->{NODE},$target->{NODE});
  return $node ? RDF::Node->_new_from_object($node,1) : undef;
}

=item arc SOURCE TARGET

Get one arc RDF::Node object that matches a given source I<SOURCE>,
target I<TARGET> RDF::Node objects or undef if there is no match.

=cut

sub arc ($$$) {
  my($self,$source,$target)=@_;
  my $node=&Redland::librdf_model_get_arc($self->{MODEL},$source->{NODE},$target->{NODE});
  return $node ? RDF::Node->_new_from_object($node,1) : undef;
}

=item target SOURCE ARC

Get one target RDF::Node object that matches a given source
I<SOURCE>, arc I<ARC> RDF::Node objects or undef if there is no
match.

=cut

sub target ($$$) {
  my($self,$source,$arc)=@_;
  my $node=&Redland::librdf_model_get_target($self->{MODEL},$source->{NODE},$arc->{NODE});
  return $node ? RDF::Node->_new_from_object($node,1) : undef;
}


# FIXME - remove these functions in future release
use vars qw($get_sources_warning $get_arcs_warning $get_targets_warning);
sub get_sources ($$$) {
  my($self,$arc,$target)=@_;
  warn "RDF::Model::get_sources is deprecated, please use RDF::Model::sources_iterator\n" 
    if $get_sources_warning++ == 0;
  $self->sources_iterator($arc,$target);
}

sub get_arcs ($$$) {
  my($self,$source,$target)=@_;
  warn "RDF::Model::get_arcs is deprecated, please use RDF::Model::arcs_iterator\n" 
    if $get_arcs_warning++ == 0;
  $self->arcs_iterator($source,$target);
}

sub get_targets ($$$) {
  my($self,$source,$arc)=@_;
  warn "RDF::Model::get_targets is deprecated, please use RDF::Model::targets_iterator\n" 
    if $get_targets_warning++ == 0;
  $self->targets_iterator($source,$arc);
}


=pod

=back

=head1 SEE ALSO

L<RDF::Storage>, L<RDF::Node> and L<RDF::Statement>

=head1 AUTHOR

Dave Beckett - http://purl.org/net/dajobe/

=cut

1;
