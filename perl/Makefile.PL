#!/usr/local/bin/perl
#
# Makefile.PL - Makefile for Perl 5 interface to Redland
#
# $Id$
#
# Copyright (C) 2000-2003 David Beckett - http://purl.org/net/dajobe/
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

use ExtUtils::MakeMaker;

$TOP_SRCDIR=$ENV{TOP_SRCDIR} || '..';
$CFLAGS=$ENV{CFLAGS} || "";
$MEM_LIBS=$ENV{MEM_LIBS} || "";

$CORE_PACKAGE="CORE";

my $objects="${CORE_PACKAGE}_wrap.o ";

if(-r '../redland-src-config') {
  my $redland_libs=`../redland-src-config --libs`;
  chomp $redland_libs;
  $objects .= $redland_libs;
}

my $includes="-I$TOP_SRCDIR/librdf";

my $version=$ENV{VERSION};

if(!$version) {
  $version=`../redland-src-config --version`;
  chomp $version;
  die "Cannot find Redland version via ../redland-src-config\n"
    unless $version;
}

sub MY::postamble {
  return <<"EOT";
${CORE_PACKAGE}_wrap.c: $TOP_SRCDIR/Redland.i
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
    'AUTHOR'    => 'Dave Beckett <Dave.Beckett@bristol.ac.uk>',
    'ABSTRACT'  => "Redland RDF library",
    'VERSION'   => $version,

    'DEFINE'    => "-DHAVE_CONFIG_H $CFLAGS",
    'INC'       => "$includes",
    'OBJECT'    => "$objects",
    'dynamic_lib' => {'OTHERLDFLAGS' => "$MEM_LIBS $CFLAGS"},

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
Redland/Node.pm)
      },


    'MAKEFILE'  => 'Makefile.perl',
    'clean'     => { FILES => "test*.db" },
    'realclean' => { FILES => "${CORE_PACKAGE}_wrap.c ${CORE_PACKAGE}_wrap.o lib/RDF/Redland/CORE.pm" },
);
