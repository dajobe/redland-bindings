# -*- Mode: Perl -*-
#
# RDF.pm - Redland Perl RDF Statement module
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

package RDF::Statement;

use RDF::Node;

use Redland;

=pod

=head1 NAME

RDF::Statement - Redland RDF Statement Class

=head1 DESCRIPTION

Manipulate RDF statements which comprise three RDF::Node objects.
Also used for I<partial> statements which can have empty parts and
are used for matching statements in statement queries of the
model - see the L<RDF::Model>.

=cut

######################################################################

=pod

=head1 CONSTRUCTORS

=over

=item new

Create a new empty RDF::Statement object.

=cut

sub new ($) {
  my($proto)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};
  $self->{STATEMENT}=&Redland::librdf_new_statement();
  return undef if !$self->{STATEMENT};

  bless ($self, $class);
  return $self;
}

=item new_from_statement STATEMENT

Create a new RDF::Statement object from RDF::Statement I<STATEMENT>
(copy constructor).

=cut

sub new_from_statement ($$) {
  my($proto,$statement)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};
  $self->{STATEMENT}=&Redland::librdf_new_statement_from_statement($statement->{STATEMENT});
  return undef if !$self->{STATEMENT};

  bless ($self, $class);
  return $self;
}

=item new_from_nodes SUBJECT PREDICATE OBJECT

Create a new RDF::Statement with the given RDF::Node objects as parts
(or undef when empty for a I<partial> statement).

=cut

sub new_from_nodes ($$$$) {
  my($proto,$subject,$predicate,$object)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};

  # Get redland objects if Nodes given, or undef otherwise
  my $s=($subject ? $subject->{NODE} : undef);
  my $p=($predicate ? $predicate->{NODE} : undef);
  my $o=($object ? $object->{NODE} : undef);

  $self->{STATEMENT}=&Redland::librdf_new_statement_from_nodes($s, $p, $o);

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
  warn "RDF::Statement::_new_from_object from object $object\n" if $RDF::Debug;
  $self->{STATEMENT}=$object;
  $self->{DONT_FREE_ME}=1 if !$free_me;
  bless ($self, $class);
  return $self;
}

=pod

=back

=cut

sub DESTROY ($) {
  warn "RDF::Statement DESTROY\n" if $RDF::Debug;
  my $self=shift;
  if($self->{STATEMENT}) {
    if($self->{DONT_FREE_ME}) {
      warn "RDF::Statement NOT doing librdf_free_statement on librdf statement\n" if $RDF::Debug;
    } else {
      warn "RDF::Statement doing librdf_free_statement on librdf statement\n" if $RDF::Debug;
      &Redland::librdf_free_statement($self->{STATEMENT});
    }
  }
  warn "RDF::Statement DESTROY done\n" if $RDF::Debug;
}

=head1 METHODS

=over

=item subject [SUBJECT]

Get/set the statement subject.  When RDF::Node I<SUBJECT> is given, sets
the subject of the statement, otherwise returns a reference to the
statement RDF::Node subject which must be copied if used elsewhere.

=cut

sub subject ($;$) {
  my($self,$subject)=@_;

  return RDF::Node->_new_from_object(&Redland::librdf_statement_get_subject(shift->{STATEMENT}), 0)
    unless $subject;

  my $rc=&Redland::librdf_statement_set_subject($self->{STATEMENT},$subject->{NODE});
  # Zap the incoming librdf node object since it is now owned by the
  # librdf statement object $self->{STATEMENT}
  $subject->{NODE}=undef;

  $rc;
}

=item predicate [PREDICATE]

Get/set the statement predicate.  When RDF::Node I<PREDICATE> is given, sets
the predicate of the statement, otherwise returns a reference to the
statement RDF::Node predicate which must be copied if used elsewhere.

=cut

sub predicate ($;$) {
  my($self,$predicate)=@_;
  
  return RDF::Node->_new_from_object(&Redland::librdf_statement_get_predicate(shift->{STATEMENT}), 0)
    unless $predicate;

  my $rc=&Redland::librdf_statement_set_predicate($self->{STATEMENT},$predicate->{NODE});
  # Zap the incoming librdf node object since it is now owned by the
  # librdf statement object $self->{STATEMENT}
  $predicate->{NODE}=undef;

  $rc;
}

=item object [OBJECT]

Get/set the statement object.  When RDF::Node I<OBJECT> is given, sets
the object of the statement, otherwise returns a reference to the
statement RDF::Node object which must be copied if used elsewhere.

=cut

sub object ($;$) {
  my($self,$object)=@_;

  return RDF::Node->_new_from_object(&Redland::librdf_statement_get_object(shift->{STATEMENT}), 0)
    unless $object;

  my $rc=&Redland::librdf_statement_set_object($self->{STATEMENT},$object->{NODE});

  # Zap the incoming librdf node object since it is now owned by the
  # librdf statement object $self->{STATEMENT}
  $object->{NODE}=undef;

  $rc;
}

=item as_string

Return the statement formatted as a string (UTF-8 encoded).

=cut

sub as_string ($) {
  &Redland::librdf_statement_to_string(shift->{STATEMENT});
}

=pod

=back

=head1 SEE ALSO

L<RDF::Node>

=head1 AUTHOR

Dave Beckett - http://purl.org/net/dajobe/

=cut

1;
