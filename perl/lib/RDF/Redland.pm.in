# -*- Mode: Perl -*-
#
# RDF.pm - Redland top level Perl module
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

use RDF::Iterator;
use RDF::Model;
use RDF::Node;
use RDF::Parser;
use RDF::Statement;
use RDF::Storage;
use RDF::Stream;
use RDF::URI;

use Redland;

package RDF::World;

sub new ($) {
  my($proto)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};

  &Redland::librdf_init_world("", undef);

  bless ($self, $class);
  $self->{ME}=$self;
  return $self;
}

sub DESTROY ($) {
  warn "RDF::World DESTROY" if $RDF::Debug;
  &Redland::librdf_destroy_world();
}


package RDF;

use vars qw($Debug $World);

$Debug=0;

$World=new RDF::World;


=pod

=head1 NAME

RDF - Redland RDF Class

=head1 SYNOPSIS

  use RDF;
  my $storage=new RDF::Storage("hashes", "test", "new='yes',hash-type='memory'");
  my $model=new RDF::Model($storage, "");

  ...

=head1 DESCRIPTION

This class initialises the Redland RDF classes.

=head1 SEE ALSO

L<RDF::Node>, L<RDF::Statement>, L<RDF::Model>, L<RDF::Storage>,
L<RDF::Parser>, L<RDF::Iterator>, L<RDF::Stream>, L<RDF::URI>
and L<RDF::RSS>.

=head1 AUTHOR

Dave Beckett - http://purl.org/net/dajobe/

=cut

1;
