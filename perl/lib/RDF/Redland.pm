# -*- Mode: Perl -*-
#
# Redland.pm - Redland top level Perl module
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

use RDF::Redland::Iterator;
use RDF::Redland::Model;
use RDF::Redland::Node;
use RDF::Redland::Parser;
use RDF::Redland::Serializer;
use RDF::Redland::Statement;
use RDF::Redland::Storage;
use RDF::Redland::Stream;
use RDF::Redland::URI;

use RDF::Redland::CORE;

package RDF::Redland::World;

sub new ($) {
  my($proto)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};

  $self->{WORLD}=&RDF::Redland::CORE::librdf_new_world();
  &RDF::Redland::CORE::librdf_world_open($self->{WORLD});

  bless ($self, $class);
  $self->{ME}=$self;
  return $self;
}

sub DESTROY ($) {
  warn "RDF::World DESTROY" if $RDF::Debug;
  &RDF::Redland::CORE::librdf_free_world($self->{WORLD}) if $self->{WORLD};
}


package RDF::Redland;

use vars qw($Debug $World);

$Debug=0;

$World=new RDF::Redland::World;


=pod

=head1 NAME

RDF::Redland - Redland RDF Class

=head1 SYNOPSIS

  use RDF::Redland;
  my $storage=new RDF::Redland::Storage("hashes", "test", "new='yes',hash-type='memory'");
  my $model=new RDF::Redland::Model($storage, "");

  ...

=head1 DESCRIPTION

This class initialises the Redland RDF classes.

=head1 SEE ALSO

L<RDF::Redland::Node>, L<RDF::Redland::Statement>, L<RDF::Redland::Model>,
L<RDF::Redland::Storage>, L<RDF::Redland::Parser>, L<RDF::Redland::Iterator>,
L<RDF::Redland::Stream>, L<RDF::Redland::URI> and L<RDF::Redland::RSS>.

=head1 AUTHOR

Dave Beckett - http://purl.org/net/dajobe/

=cut

1;
