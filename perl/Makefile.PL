#!/usr/local/bin/perl
#
# Makefile.PL - Makefile for Perl 5 interface to Redland
#
# Copyright (C) 2000-2008 David Beckett - http://www.dajobe.org/
# Copyright (C) 2000-2005 University of Bristol - http://www.bristol.ac.uk/
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

use ExtUtils::MakeMaker;

$PKG_CONFIG=$ENV{PKG_CONFIG} || 'pkg-config';

if(`$PKG_CONFIG redland --exists 2>/dev/null`) {
  $REDLAND_CONFIG="$PKG_CONFIG redland";
} else {
  $REDLAND_CONFIG=$ENV{REDLAND_CONFIG} || 'redland-config';
  undef $PKG_CONFIG;
}

$CORE_PACKAGE="CORE";

my $version=$ENV{VERSION};
if(!defined $version) {
  my $varg=$PKG_CONFIG ? "--modversion" : "--version";
  $version=`$REDLAND_CONFIG $varg`;
  chomp $version;
  die "Cannot find Redland version via $REDLAND_CONFIG\n"
    unless $version;
}

my $redland_cflags=$ENV{CFLAGS};
if(!defined $redland_cflags) {
  $redland_cflags = `$REDLAND_CONFIG --cflags`;
  chomp $redland_cflags;
}

my $redland_ldflags=$ENV{LDFLAGS};
if(!defined $redland_ldflags) {
  $redland_ldflags = `$REDLAND_CONFIG --libs`;
  chomp $redland_ldflags;
}

my $swig=$ENV{SWIG};
if(!defined $swig) {
  my $swig_arg=$PKG_CONFIG ? "--variable=swig" : "--swig";
  $swig=`$REDLAND_CONFIG $swig_arg`;
  chomp $swig;
  die "cannot find redland SWIG file via $REDLAND_CONFIG\n"
    unless $swig;
}


my $objects="${CORE_PACKAGE}_wrap.o $redland_libs";


sub MY::postamble {
  return <<"EOT";
${CORE_PACKAGE}_wrap.c: $swig
	\$(MAKE) ${CORE_PACKAGE}_wrap.c
EOT
}

# I'm forced to do this since BASEEXT is defined in a non-overridable method.
push(@ExtUtils::MakeMaker::Overridable, "init_main");

# Ensure that the dynamic module gets compiled as CORE not Redland
sub MY::init_main {
  my($self)=@_;
  package MY; # so that "SUPER" works right
  $self->SUPER::init_main();

  # Perl 5.8 just made this harder
  if ($] < 5.008) {
    # OSX too
    if ($^O eq 'darwin') {
      $self->{FULLEXT}.="/".$::CORE_PACKAGE;
    }
  } else {
    $self->{FULLEXT}.="/".$::CORE_PACKAGE;
  }
  $self->{BASEEXT}=$::CORE_PACKAGE;

  # Stop perl linking with anything in the destination dir, it might
  # be entirely the wrong library such as a previous version.
  $self->{LDFLAGS} =~ s/-L\S+//;
  $self->{LDDLFLAGS} =~ s/-L\S+//;
}


WriteMakefile(
    'DISTNAME'	=> "Redland",
    'NAME'	=> "RDF::Redland",
    'AUTHOR'    => 'Dave Beckett <dave@dajobe.org>',
    'ABSTRACT'  => "Redland RDF library",
    'VERSION'   => $version,

    'DEFINE'    => "-DHAVE_CONFIG_H $redland_cflags",
    'OBJECT'    => "$objects",
    'dynamic_lib' => {'OTHERLDFLAGS' => "$redland_ldflags"},

    'PM' => { map { ("lib/RDF/$_", "\$(INST_LIBDIR)/$_") }
 qw(Redland.pm
Redland/Model.pm
Redland/RSS.pm
Redland/URI.pm
Redland/Iterator.pm
Redland/Storage.pm
Redland/Statement.pm
Redland/Stream.pm
Redland/Parser.pm
Redland/Query.pm
Redland/QueryResults.pm
Redland/Serializer.pm
Redland/CORE.pm
Redland/Node.pm
Redland/BlankNode.pm
Redland/URINode.pm
Redland/LiteralNode.pm
Redland/XMLLiteralNode.pm
)
      },


    'MAKEFILE'  => 'Makefile.perl',
    'clean'     => { FILES => "test*.db" },
    'realclean' => { FILES => "${CORE_PACKAGE}_wrap.c ${CORE_PACKAGE}_wrap.o lib/RDF/Redland/CORE.pm" },
);
