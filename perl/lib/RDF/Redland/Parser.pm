# -*- Mode: Perl -*-
#
# Parser.pm - Redland Perl RDF Parser module
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

package RDF::Redland::Parser;

use strict;

use RDF::Redland::Stream;

=pod

=head1 NAME

RDF::Redland::Parser - Redland RDF Syntax Parsers Class

=head1 SYNOPSIS

  use RDF::Redland;

  ...
  my $parser=new RDF::Redland::Parser("repat");

  # Return as an RDF::Redland::Stream
  my $stream=$parser->parse_as_stream($source_uri, $base_uri);
  
  # Store in an RDF::Redland::Model
  $parser->parse_into_model($source_uri, $base_uri, $model);

=head1 DESCRIPTION

This class represents parsers of various syntaxes that can deliver a
RDF model either as a RDF::Redland::Stream of RDF::Redland::Statement objects or
directly into an RDF::Redland::Model object.

=cut

######################################################################

=pod

=head1 CONSTRUCTORS

=over

=item new [NAME [MIME_TYPE [URI]]]

Create a new RDF::Redland::Parser object for a syntax parser named I<NAME>,
with MIME Type I<MIME_TYPE> and/or URI I<URI>.  Any field can be undef
or omitted; if all are omitted, a parser that provides MIME Type 
application/rdf+xml will be requested.

=cut

# CONSTRUCTOR
# (main)
sub new ($;$$$) {
  my($proto,$name,$mime_type,$uri)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};

  if(defined $uri) {
    $uri=$uri->{URI};
  }

  $self->{PARSER}=&RDF::Redland::CORE::librdf_new_parser($RDF::Redland::World->{WORLD},$name,$mime_type,$uri);
  return undef if !$self->{PARSER};

  bless ($self, $class);
  return $self;
}

=pod

=back

=cut

sub DESTROY ($) {
  warn "RDF::Redland::Parser DESTROY\n" if $RDF::Redland::Debug;
  &RDF::Redland::CORE::librdf_free_parser(shift->{PARSER});
}

=head1 METHODS

=over

=item parse_as_stream SOURCE_URI BASE_URI

Parse the syntax at the RDF::Redland::URI I<SOURCE_URI> with optional base
RDF::Redland::URI I<BASE_URI>.  If the base URI is given then the content is
parsed as if it was at the base URI rather than the source URI.

Returns an RDF::Redland::Stream of RDF::Redland::Statement objects or undef on failure.

=cut

sub parse_as_stream ($$$) {
  my($self,$uri,$base_uri)=@_;
  my $stream=&RDF::Redland::CORE::librdf_parser_parse_as_stream($self->{PARSER},$uri->{URI}, $base_uri->{URI});
  return new RDF::Redland::Stream($stream,$self);
}

=item parse_into_model SOURCE_URI BASE_URI MODEL

Parse the syntax at the RDF::Redland::URI I<SOURCE_URI> with optional base
RDF::Redland::URI I<BASE_URI> into RDF::Redland::Model I<MODEL>.  If the base URI is
given then the content is parsed as if it was at the base URI rather
than the source URI.

=cut

sub parse_into_model ($$$$) {
  my($self,$uri,$base_uri,$model)=@_;
  return &RDF::Redland::CORE::librdf_parser_parse_into_model($self->{PARSER},$uri->{URI},$base_uri->{URI},$model->{MODEL});
}

=item feature URI [VALUE]

Get/set a parser feature.  The feature is named via RDF::Redland::URI I<URI>
and the value is a string.  If I<VALUE> is given, the feature is set
to that value, otherwise the current value is returned.

=cut

sub feature ($$;$) {
  my($self,$uri,$value)=@_;

  warn "RDF::Redland::Parser->feature('$uri', '$value')\n" if $RDF::Redland::Debug;
  $uri=RDF::Redland::URI->new($uri)
    unless ref $uri;

  return &RDF::Redland::CORE::librdf_parser_get_feature($self->{PARSER},$uri->{URI})
    unless $value;

  return &RDF::Redland::CORE::librdf_parser_set_feature($self->{PARSER},$uri->{URI},$value);
}


=pod

=back

=head1 SEE ALSO

L<RDF::Redland::URI>

=head1 AUTHOR

Dave Beckett - http://purl.org/net/dajobe/

=cut

1;
