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

PERL=@PERL@

PERL_REDLAND_PACKAGE=RDF::Redland
PERL_CORE_PACKAGE=CORE
PERL_FULL_CORE_PACKAGE="$(PERL_REDLAND_PACKAGE)::$(PERL_CORE_PACKAGE)"

PERL_MAKEFILE=Makefile.perl

SWIG_OUTPUTS=$(PERL_CORE_PACKAGE)_wrap.c \
	lib/RDF/Redland/$(PERL_CORE_PACKAGE).pm
PERL_CRUFT=pm_to_blib \
$(PERL_CORE_PACKAGE).bs $(PERL_CORE_PACKAGE)_wrap.o \
$(PERL_MAKEFILE).old


fakedir=$(prefix)
fake_DATA=build-perl-stamp

EXTRA_DIST=README.txt \
MANIFEST MANIFEST.SKIP Makefile.PL example.pl \
dc.rdf rss-dump.pl update-perl-api.pl \
$(SWIG_OUTPUTS)

SUBDIRS=lib t

CLEANFILES=dmalloc.log *.db test-out.rdf core* $(PERL_CRUFT)
MAINTAINERCLEANFILES=$(SWIG_OUTPUTS)

MEM_LIBS=@MEM_LIBS@
#MEM_LIBS=-ldmalloc

$(perlshareRDFdir)/Redland.pm: build-perl


$(PERL_CORE_PACKAGE)_wrap.c lib/RDF/Redland/$(PERL_CORE_PACKAGE).pm : $(srcdir)/../Redland.i
	swig -v -perl5 -module $(PERL_FULL_CORE_PACKAGE) -o $(PERL_CORE_PACKAGE)_wrap.c $< && mv $(PERL_CORE_PACKAGE).pm lib/RDF/Redland/$(PERL_CORE_PACKAGE).pm

$(PERL_MAKEFILE): $(srcdir)/Makefile.PL lib/RDF/Redland/$(PERL_CORE_PACKAGE).pm
	PACKAGE=$(PERL_REDLAND_PACKAGE) VERSION=$(VERSION) TOP_SRCDIR=$(top_srcdir) MEM_LIBS=$(MEM_LIBS) MEM=$(MEM) $(PERL) $(srcdir)/Makefile.PL

-perl test-perl install-perl: $(PERL_MAKEFILE)
	@target=`echo $@ | sed -e 's/-perl//'`; \
	echo $(MAKE) -f $(PERL_MAKEFILE) $$target PREFIX=$(prefix) SITEPREFIX=$(prefix); \
	$(MAKE) -f $(PERL_MAKEFILE) $$target PREFIX=$(prefix) SITEPREFIX=$(prefix)

clean-perl realclean-perl:
	@target=`echo $@ | sed -e 's/-perl//'`; \
	if test -r $(PERL_MAKEFILE); then \
	  echo $(MAKE) -f $(PERL_MAKEFILE) $$target PREFIX=$(prefix) SITEPREFIX=$(prefix); \
	  $(MAKE) -f $(PERL_MAKEFILE) $$target PREFIX=$(prefix) SITEPREFIX=$(prefix); \
	fi

build-perl: -perl

build-perl-stamp: $(srcdir)/../Redland.i
	$(MAKE) build-perl && touch build-perl-stamp

check-local: build-perl-stamp test-perl

# Perl make clean leaves the old makefile around
clean-local: 
	rm -rf blib $(PERL_MAKEFILE) $(PERL_MAKEFILE).old build-perl-stamp

# See this is the fake bit
install-fakeDATA: install-perl
