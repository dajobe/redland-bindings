# -*- Mode: Perl -*-
#
# Redland.pm - Redland top level Perl module
#
# $Id$
#
# Copyright (C) 2000-2004 David Beckett - http://purl.org/net/dajobe/
# Institute for Learning and Research Technology - http://www.ilrt.bris.ac.uk/
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
use RDF::Redland::Query;
use RDF::Redland::QueryResults;
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

  &RDF::Redland::CORE::librdf_perl_world_init($self->{WORLD});

  bless ($self, $class);
  $self->{ME}=$self;
  return $self;
}

sub DESTROY ($) {
  warn "RDF::World DESTROY\n" if $RDF::Debug;
  &RDF::Redland::CORE::librdf_perl_world_finish();
}

sub message ($$$$$$$$$) {
  if(ref $RDF::Redland::Log_Sub) {
    return $RDF::Redland::Log_Sub->(@_);
  }

  my($code, $level, $facility, $message, $line, $column, $byte, $file, $uri)=@_;
  if($level > 3) {
    if(ref $RDF::Redland::Error_Sub) {
      return $RDF::Redland::Error_Sub->($message);
    } else {
      die "Redland error: $message\n";
    }
  } else {
    if(ref $RDF::Redland::Warning_Sub) {
      return $RDF::Redland::Warning_Sub->($message);
    } else {
      warn "Redland warning: $message\n";
    }
  }
  1;
}

package RDF::Redland;

use vars qw($Debug $World $Error_Sub $Warning_Sub);

$Debug=0;

$World=new RDF::Redland::World;

$Error_Sub=undef;

$Warning_Sub=undef;

$Log_Sub=undef;

sub set_error_handler ($) {
  $Error_Sub=shift;
}

sub set_warning_handler ($) {
  $Warning_Sub=shift;
}

sub set_log_handler ($) {
  $Log_Sub=shift;
}


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

=head1 STATIC METHODS

=over

=item set_log_handler SUB

Set I<SUB> as the subroutine to be called on a Redland log message with
the following as the arguments.  For example:

  RDF::Redland::set_log_handler(sub {
    my($code, $level, $facility, $message, $line, $column, $byte, $file, $uri)=@_;

    # Do something with the log info
  });

The default if this is not set, is to run the error or warning handlers.

=item set_error_handler SUB

Set I<SUB> as the subroutine to be called on a Redland error with
the error message as the single argument.  For example:

  RDF::Redland::set_error_handler(sub {
    my $msg=shift;
    # Do something with $msg
  });

The default if this is not set, is to run die $msg

=item set_warning_handler SUB

Set I<SUB> as the subroutine to be called on a Redland warning with
the warning message as the single argument.  For example:

  RDF::Redland::set_warning_handler(sub {
    my $msg=shift;
    # Do something with $msg
  });

The default if this is not set, is to run warn $msg

=back

=head1 SEE ALSO

L<RDF::Redland::Node>, L<RDF::Redland::URI>,
L<RDF::Redland::Statement>, L<RDF::Redland::Model>,
L<RDF::Redland::Storage>, L<RDF::Redland::Parser>,
L<RDF::Redland::Query>, L<RDF::Redland::QueryResults>,
L<RDF::Redland::Iterator>, L<RDF::Redland::Stream>
and L<RDF::Redland::RSS>.

=head1 AUTHOR

Dave Beckett - http://purl.org/net/dajobe/

=cut

1;
