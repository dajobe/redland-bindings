#!/usr/local/bin/perl
#
# Makefile.PL - Makefile for Perl 5 interface to Redland
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

use ExtUtils::MakeMaker;

$TOP_SRCDIR=$ENV{TOP_SRCDIR} || '..';
$PACKAGE=$ENV{PACKAGE} || "Redland";
$MEM_LIBS=$ENV{MEM_LIBS} || "";

my $objects="Redland_wrap.o $TOP_SRCDIR/librdf.a ";

my $redland_libs=`$TOP_SRCDIR/redland-config --libs`;
chomp $redland_libs;
$redland_libs =~ s/^-L\S+ -lrdf\s*//;
$objects .= $redland_libs;

my $version=$ENV{VERSION};

if(!$version) {
  my $conf_in_file="$TOP_SRCDIR/configure.in";
  open(CONF, $conf_in_file) or die "Cannot open $conf_in_file for version - $!\n";
  while(<CONF>) {
    if(/AM_INIT_AUTOMAKE\(redland,\s+(\d+\.\d+\.\d+)\)/) {
      $version=$1;
      last;
    }
  }
  close(CONF);
  die "Cannot find AM_INIT_AUTOMAKE in $conf_in_file for version\n"
    unless $version;
}

sub MY::postamble {
  return <<"EOT";
Redland_wrap.c: $TOP_SRCDIR/Redland.i
	swig -v -perl5 -package $PACKAGE -o \$\@ \$\?
EOT
}

# Ensure only .pm files are installed (since lib tree has CVS and
# automake Makefile.am files too)
sub MY::libscan {
  my($self,$name)=@_;
  return $name if $name =~ /\.pm$/;
  undef;
}

WriteMakefile(
    'NAME'	=> $PACKAGE,
    'AUTHOR'    => 'Dave Beckett <Dave.Beckett@bristol.ac.uk>',
    'ABSTRACT'  => "Redland RDF library",
    'VERSION'   => $version,
    'DEFINE'    => '-Dbool=char -DHAVE_CONFIG_H',
    'CCFLAGS'   => "-I$TOP_SRCDIR",
    'OBJECT'    => "$objects $MEM_LIBS",
    'MAKEFILE'  => 'Makefile.perl',
    'clean'     => { FILES => "test*.db" },
    'realclean' => { FILES => "Redland_wrap.c Redland_wrap.o Redland.pm" },
);
