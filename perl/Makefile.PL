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
$PACKAGE=$ENV{PACKAGE} || "RDF::Redland";
$CFLAGS=$ENV{CFLAGS} || "";
$MEM_LIBS=$ENV{MEM_LIBS} || "";
$CORE_PACKAGE=$ENV{CORE_PACKAGE} || "CORE";

my $objects="${CORE_PACKAGE}_wrap.o ";

if(-r '../redland-src-config') {
  my $redland_libs=`../redland-src-config --libs`;
  chomp $redland_libs;
  $objects .= $redland_libs;
}

my $includes="-I$TOP_SRCDIR/librdf -I../librdf -I$TOP_SRCDIR/librdf/perl";

my $version=$ENV{VERSION};

if(!$version) {
  $version=`../redland-src-config --version`;
  chomp $version;
  die "Cannot find Redland version via redland-src-config\n"
    unless $version;
}

sub MY::postamble {
  return <<"EOT";
${CORE_PACKAGE}_wrap.c: $TOP_SRCDIR/Redland.i
	cp $TOP_SRCDIR/perl/${CORE_PACKAGE}_wrap.c .
EOT
}

# Ensure only .pm files are installed (since lib tree has CVS and
# automake Makefile.am files too)
sub MY::libscan {
  my($self,$name)=@_;
  return $name if $name =~ /\.pm$/;
  undef;
}

# I'm forced to do this since BASEEXT is defined in a non-overridable method.
push(@ExtUtils::MakeMaker::Overridable, "init_main");

# Ensure that the dynamic module gets compiled as CORE not Redland
sub MY::init_main {
  my($self)=@_;
  package MY; # so that "SUPER" works right
  $self->SUPER::init_main();
  $self->{FULLEXT}.="/".$::CORE_PACKAGE;
  $self->{BASEEXT}=$::CORE_PACKAGE;
}


WriteMakefile(
    'DISTNAME'	=> "Redland",
    'NAME'	=> $PACKAGE,
    'AUTHOR'    => 'Dave Beckett <Dave.Beckett@bristol.ac.uk>',
    'ABSTRACT'  => "Redland RDF library",
    'VERSION'   => $version,

    'DEFINE'    => "-DHAVE_CONFIG_H $CFLAGS",
    'INC'       => "$includes",
    'OBJECT'    => "$objects $MEM_LIBS",
    'dynamic_lib' => {'OTHERLDFLAGS' => "$MEM_LIBS"},

    'MAKEFILE'  => 'Makefile.perl',
    'clean'     => { FILES => "test*.db" },
    'realclean' => { FILES => "${CORE_PACKAGE}_wrap.c ${CORE_PACKAGE}_wrap.o lib/RDF/Redland/CORE.pm" },
);
