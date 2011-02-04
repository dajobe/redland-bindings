# -*- Mode: Makefile -*-
#
# Makefile.am - automake file for Perl 5 interface to Redland
#
# Copyright (C) 2000-2005 David Beckett - http://www.dajobe.org/
# Copyright (C) 2000-2005 University of Bristol - http://www.bristol.ac.uk/
# 
# This package is Free Software and part of Redland http://librdf.org/
# 
# It is licensed under the following three licenses as alternatives:
#   1. GNU Lesser General Public License (LGPL) V2.1 or any newer version
#   2. GNU General Public License (GPL) V2 or any newer version
#   3. Apache License, V2.0 or any newer version
# 
# You may not use this file except in compliance with at least one of
# the above three licenses.
# 
# See LICENSE.html or LICENSE.txt at the top of this package for the
# full license terms.
# 
# 

AM_CPPFLAGS=@CPPFLAGS@ @LIBRDF_CPPFLAGS@
AM_CFLAGS=@CFLAGS@ @LIBRDF_CPPFLAGS@ $(MEM)
AM_STANDARD_CFLAGS=@STANDARD_CFLAGS@ @LIBRDF_CPPFLAGS@ $(MEM)

AM_LDFLAGS=@LIBRDF_LDFLAGS@ $(MEM_LIBS)

REDLAND_SWIG=@REDLAND_SWIG@

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

EXTRA_DIST= \
MANIFEST MANIFEST.SKIP Makefile.PL example.pl \
rss-dump.pl \
redland-pre.i redland-post.i redland-decl.i $(SWIG_OUTPUTS)

SUBDIRS=lib t

CLEANFILES=*.db test-out.rdf core* $(PERL_CRUFT)
MAINTAINERCLEANFILES=$(SWIG_OUTPUTS)

MEM_LIBS=@MEM_LIBS@

$(perlshareRDFdir)/Redland.pm: build-perl


RUN=@REDLAND_RUN@

SWIG_OPTS=-I$(srcdir) -DREDLAND_PRE_I -DREDLAND_POST_I -DREDLAND_DECL_I

$(PERL_CORE_PACKAGE)_wrap.c lib/RDF/Redland/$(PERL_CORE_PACKAGE).pm : @REDLAND_SWIG@ redland-pre.i redland-post.i redland-decl.i
	swig -v -perl5 $(SWIG_OPTS) -module $(PERL_FULL_CORE_PACKAGE) -o $(PERL_CORE_PACKAGE)_wrap.c $(REDLAND_SWIG) && mv $(PERL_CORE_PACKAGE).pm lib/RDF/Redland/$(PERL_CORE_PACKAGE).pm

$(PERL_MAKEFILE): $(srcdir)/Makefile.PL lib/RDF/Redland/$(PERL_CORE_PACKAGE).pm
	VERSION=$(VERSION) LDFLAGS="$(AM_LDFLAGS)" CFLAGS="$(AM_CPPFLAGS) $(SWIG_OPTS) $(CPPFLAGS) $(AM_STANDARD_CFLAGS)" SWIG="$(REDLAND_SWIG)" $(PERL) $(srcdir)/Makefile.PL $(MAKE_PL_OPTS)

-perl install-perl: $(PERL_MAKEFILE)
	@target=`echo $@ | sed -e 's/-perl//'`; \
	echo $(MAKE) -f $(PERL_MAKEFILE) $$target; \
	$(MAKE) -f $(PERL_MAKEFILE) $$target

test-perl: $(PERL_MAKEFILE)
	$(RUN)$(MAKE) -f $(PERL_MAKEFILE) test

clean-perl realclean-perl:
	@target=`echo $@ | sed -e 's/-perl//'`; \
	if test -r $(PERL_MAKEFILE); then \
	  echo $(MAKE) -f $(PERL_MAKEFILE) $$target; \
	  $(MAKE) -f $(PERL_MAKEFILE) $$target; \
	fi

build-perl: -perl

build-perl-stamp: @REDLAND_SWIG@
	$(MAKE) build-perl && touch build-perl-stamp

check-local: build-perl-stamp test-perl

# Perl make clean leaves the old makefile around
clean-local:
	rm -rf blib $(PERL_MAKEFILE) $(PERL_MAKEFILE).old build-perl-stamp

# See this is the fake bit
install-fakeDATA: install-perl
