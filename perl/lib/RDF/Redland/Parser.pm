# -*- Mode: Perl -*-
#
# RDF.pm - Redland Perl RDF Parser module
#
# $Id$
#
# Copyright (C) 2000-2001 David Beckett - http://purl.org/net/dajobe/
# Institute for Learning and Research Technology - http://www.ilrt.org/
# University of Bristol - http://www.bristol.ac.uk/
# 
# This package is Free Software or Open Source available under the
# following licenses (these are alternatives):
#   1. GNU Lesser General Public License (LGPL) Version 2
#   2. GNU General Public License (GPL) Version 2
#   3. Mozilla Public License (MPL) Version 1.1
# and no other versions of those licenses.
# 
# See INSTALL.html or INSTALL.txt at the top of this package for the
# full license terms.
# 
#

package RDF::Parser;

use strict;

use RDF::Stream;

=pod

=head1 NAME

RDF::Parser - Redland RDF Syntax Parsers Class

=head1 SYNOPSIS

  use RDF;

  ...
  my $parser=new RDF::Parser("repat");

  # Return as an RDF::Stream
  my $stream=$parser->parse_as_stream($source_uri, $base_uri);
  
  # Store in an RDF::Model
  $parser->parse_into_model($source_uri, $base_uri, $model);

=head1 DESCRIPTION

This class represents parsers of various syntaxes that can deliver a
RDF model either as a RDF::Stream of RDF::Statement objects or
directly into an RDF::Model object.

=cut

######################################################################

=pod

=head1 CONSTRUCTORS

=over

=item new NAME MIME_TYPE URI

Create a new RDF::Parser object for a syntax parser named I<NAME>,
with MIME Type I<MIME_TYPE> and/or URI I<URI>.  I<NAME> is the only
required field.

=cut

# CONSTRUCTOR
# (main)
sub new ($$$$) {
  my($proto,$name,$mime_type,$uri)=@_;
  my $class = ref($proto) || $proto;
  my $self  = {};
  $mime_type ||="";

  if(defined $uri) {
    $uri=$uri->{URI};
  }

  $self->{PARSER}=&Redland::librdf_new_parser($name,$mime_type,$uri);
  return undef if !$self->{PARSER};

  bless ($self, $class);
  return $self;
}

=pod

=back

=cut

sub DESTROY ($) {
  warn "RDF::Parser DESTROY\n" if $RDF::Debug;
  &Redland::librdf_free_parser(shift->{PARSER});
}

=head1 METHODS

=over

=item parse_as_stream SOURCE_URI BASE_URI

Parse the syntax at the RDF::URI I<SOURCE_URI> with optional base
RDF::URI I<BASE_URI>.  If the base URI is given then the content is
parsed as if it was at the base URI rather than the source URI.

Returns an RDF::Stream of RDF::Statement objects or undef on failure.

=cut

sub parse_as_stream ($$$) {
  my($self,$uri,$base_uri)=@_;
  my $stream=&Redland::librdf_parser_parse_as_stream($self->{PARSER},$uri->{URI}, $base_uri->{URI});
  return new RDF::Stream($stream,$self);
}

=item parse_into_model SOURCE_URI BASE_URI MODEL

Parse the syntax at the RDF::URI I<SOURCE_URI> with optional base
RDF::URI I<BASE_URI> into RDF::Model I<MODEL>.  If the base URI is
given then the content is parsed as if it was at the base URI rather
than the source URI.

=cut

sub parse_into_model ($$$$) {
  my($self,$uri,$base_uri,$model)=@_;
  return &Redland::librdf_parser_parse_into_model($self->{PARSER},$uri->{URI},$base_uri->{URI},$model->{MODEL});
}

=item feature URI [VALUE]

Get/set a parser feature.  The feature is named via RDF::URI I<URI>
and the value is a string.  If I<VALUE> is given, the feature is set
to that value, otherwise the current value is returned.

=cut

sub feature ($$;$) {
  my($self,$uri,$value)=@_;

  warn "RDF::Parser->feature('$uri', '$value')\n" if $RDF::Debug;
  $uri=RDF::URI->new($uri)
    unless ref $uri;

  return &Redland::librdf_parser_get_feature($self->{PARSER},$uri->{URI})
    unless $value;

  return &Redland::librdf_parser_set_feature($self->{PARSER},$uri->{URI},$value);
}


=pod

=back

=head1 SEE ALSO

L<RDF::URI>

=head1 AUTHOR

Dave Beckett - http://purl.org/net/dajobe/

=cut

1;
