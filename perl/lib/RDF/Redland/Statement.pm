# -*- Mode: Perl -*-
#
# Statement.pm - Redland Perl RDF Statement module
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

package RDF::Redland::Statement;

use strict;

use RDF::Redland::Node;

=pod

=head1 NAME

RDF::Redland::Statement - Redland RDF Statement Class

=head1 SYNOPSIS

  use RDF::Redland;
  my $statement=new RDF::Redland::Statement();
  my $statement2=RDF::Redland::Statement->new_from_statement($statement);
  my $statement3=RDF::Redland::Statement->new_from_nodes($subject, $predicate, $object);
  ...

=head1 DESCRIPTION

Manipulate RDF statements which comprise three RDF::Redland::Node objects.
Also used for I<partial> statements which can have empty parts and
are used for matching statements in statement queries of the
model - see the L<RDF::Redland::Model>.

=cut

######################################################################

=pod

=head1 CONSTRUCTORS

=over

=item new

Create a new empty RDF::Redland::Statement object.

=cut

sub new ($) {
  my($proto)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};
  $self->{STATEMENT}=&RDF::Redland::CORE::librdf_new_statement($RDF::Redland::World->{WORLD});
  return undef if !$self->{STATEMENT};

  bless ($self, $class);
  return $self;
}

=item new_from_statement STATEMENT

Create a new RDF::Redland::Statement object from RDF::Redland::Statement I<STATEMENT>
(copy constructor).

=cut

sub new_from_statement ($$) {
  my($proto,$statement)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};
  $self->{STATEMENT}=&RDF::Redland::CORE::librdf_new_statement_from_statement($statement->{STATEMENT});
  return undef if !$self->{STATEMENT};

  bless ($self, $class);
  return $self;
}

=item new_from_nodes SUBJECT PREDICATE OBJECT

Create a new RDF::Redland::Statement with the given RDF::Redland::Node objects as parts
(or undef when empty for a I<partial> statement).

NOTE: After construction, the RDF::Redland::Node objects become owned by the
new RDF::Redland::Statement object and I<must not> be used elsewhere.
Existing nodes can be copied to use in this method with the RDF::Redland::Node
copy constructor new_from_node like this:

  $new_node = RDF::Redland::Node->new_from_node($node);

=cut

sub new_from_nodes ($$$$) {
  my($proto,$subject,$predicate,$object)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};

  # Get redland objects if Nodes given, or undef otherwise
  my $s=($subject ? $subject->{NODE} : undef);
  my $p=($predicate ? $predicate->{NODE} : undef);
  my $o=($object ? $object->{NODE} : undef);

  $self->{STATEMENT}=&RDF::Redland::CORE::librdf_new_statement_from_nodes($RDF::Redland::World->{WORLD}, $s, $p, $o);

  # Zap the incoming librdf node objects since they are now owned by the
  # librdf statement object $self->{STATEMENT}
  $subject->{NODE}=undef if $subject;
  $predicate->{NODE}=undef if $predicate;
  $object->{NODE}=undef if $object;

  return undef if !$self->{STATEMENT};

  bless ($self, $class);
  return $self;
}

# internal constructor to build an object from a statement created
# by librdf e.g. from the result of a stream->next operation
sub _new_from_object ($$$) {
  my($proto,$object,$free_me)=@_;
  return undef if !$object;
  my $class = ref($proto) || $proto;
  my $self  = {};
  warn "RDF::Redland::Statement::_new_from_object from object $object free_me=$free_me\n" if $RDF::Redland::Debug;
  $self->{STATEMENT}=$object;
  $self->{DONT_FREE_ME}=!$free_me;
  bless ($self, $class);
  return $self;
}

=pod

=back

=cut

sub DESTROY ($) {
  my $self=shift;
  warn "RDF::Redland::Statement DESTROY $self\n" if $RDF::Redland::Debug;
  if($self->{STATEMENT}) {
    if($self->{DONT_FREE_ME}) {
      warn "RDF::Redland::Statement DESTROY NOT doing librdf_free_statement on librdf statement\n" if $RDF::Redland::Debug;
    } else {
      warn "RDF::Redland::Statement DESTROY doing librdf_free_statement on librdf statement\n" if $RDF::Redland::Debug;
      &RDF::Redland::CORE::librdf_free_statement($self->{STATEMENT});
    }
  }
  warn "RDF::Redland::Statement DESTROY done\n" if $RDF::Redland::Debug;
}

=head1 METHODS

=over

=item subject [SUBJECT]

Get/set the statement subject.  When RDF::Redland::Node I<SUBJECT> is given, sets
the subject of the statement, otherwise returns a reference to the
statement RDF::Redland::Node subject which must be copied if used elsewhere.

=cut

sub subject ($;$) {
  my($self,$subject)=@_;

  return RDF::Redland::Node->_new_from_object(&RDF::Redland::CORE::librdf_statement_get_subject(shift->{STATEMENT}), 0)
    unless $subject;

  my $rc=&RDF::Redland::CORE::librdf_statement_set_subject($self->{STATEMENT},$subject->{NODE});
  # Zap the incoming librdf node object since it is now owned by the
  # librdf statement object $self->{STATEMENT}
  $subject->{NODE}=undef;

  $rc;
}

=item predicate [PREDICATE]

Get/set the statement predicate.  When RDF::Redland::Node I<PREDICATE> is given, sets
the predicate of the statement, otherwise returns a reference to the
statement RDF::Redland::Node predicate which must be copied if used elsewhere.

=cut

sub predicate ($;$) {
  my($self,$predicate)=@_;
  
  return RDF::Redland::Node->_new_from_object(&RDF::Redland::CORE::librdf_statement_get_predicate(shift->{STATEMENT}), 0)
    unless $predicate;

  my $rc=&RDF::Redland::CORE::librdf_statement_set_predicate($self->{STATEMENT},$predicate->{NODE});
  # Zap the incoming librdf node object since it is now owned by the
  # librdf statement object $self->{STATEMENT}
  $predicate->{NODE}=undef;

  $rc;
}

=item object [OBJECT]

Get/set the statement object.  When RDF::Redland::Node I<OBJECT> is given, sets
the object of the statement, otherwise returns a reference to the
statement RDF::Redland::Node object which must be copied if used elsewhere.

=cut

sub object ($;$) {
  my($self,$object)=@_;

  return RDF::Redland::Node->_new_from_object(&RDF::Redland::CORE::librdf_statement_get_object(shift->{STATEMENT}), 0)
    unless $object;

  my $rc=&RDF::Redland::CORE::librdf_statement_set_object($self->{STATEMENT},$object->{NODE});

  # Zap the incoming librdf node object since it is now owned by the
  # librdf statement object $self->{STATEMENT}
  $object->{NODE}=undef;

  $rc;
}

=item as_string

Return the statement formatted as a string (UTF-8 encoded).

=cut

sub as_string ($) {
  &RDF::Redland::CORE::librdf_statement_to_string(shift->{STATEMENT});
}

=pod

=back

=head1 SEE ALSO

L<RDF::Redland::Node>

=head1 AUTHOR

Dave Beckett - http://purl.org/net/dajobe/

=cut

1;
