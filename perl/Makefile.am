# -*- Mode: Makefile -*-
#
# Makefile.am - automake file for Perl 5 interface to Redland
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

PERL_PACKAGE=Redland

PERL_MAKEFILE=Makefile.perl

SWIG_OUTPUTS=$(PERL_PACKAGE).pm $(PERL_PACKAGE)_wrap.c
PERL_CRUFT=blib pm_to_blib $(PERL_PACKAGE).bs $(PERL_MAKEFILE) $(PERL_MAKEFILE).old

EXTRA_DIST=MANIFEST MANIFEST.SKIP Makefile.PL test.pl example.pl dc.rdf \
$(SWIG_OUTPUTS)

SUBDIRS=lib

CLEANFILES=test*db $(PERL_PACKAGE)_wrap.o $(PERL_MAKEFILE) \
$(PERL_MAKEFILE).old
MAINTAINERCLEANFILES=$(SWIG_OUTPUTS) $(SWIG_CRUFT)

MEM_LIBS=@MEM_LIBS@
#MEM_LIBS=-ldmalloc

$(PERL_PACKAGE)_wrap.c: $(srcdir)/../Redland.i
	swig -v -perl5 -module $(PERL_PACKAGE) -o $@ $?

$(PERL_MAKEFILE): Makefile.PL
	PACKAGE=$(PERL_PACKAGE) VERSION=$(VERSION) TOP_SRCDIR=$(top_srcdir) MEM_LIBS=$(MEM_LIBS) perl $? PREFIX=$(prefix)

-perl test-perl install-perl clean-perl realclean-perl: $(PERL_MAKEFILE)
	@target=`echo $@ | sed -e 's/-perl//'`; \
	echo $(MAKE) -f $(PERL_MAKEFILE) $$target PREFIX=$(prefix); \
	$(MAKE) -f $(PERL_MAKEFILE) $$target PREFIX=$(prefix)

build-perl: -perl
install-perl: build-perl
check-perl: build-perl test-perl

# Perl make clean leaves the old makefile around
clean-local: clean-perl
	rm -f $(PERL_MAKEFILE).old
