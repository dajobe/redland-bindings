# -*- Mode: Makefile -*-
#
# Makefile.am - docs automake file for Redland Bindings
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

EXTRA_DIST=README.html README \
csharp.html perl.html python.html tcl.html java.html ruby.html php.html \
fix-pod-xhtml fix-python-xhtml \
pod pydoc

POD_MODULES=RDF/Redland RDF/Redland/Iterator RDF/Redland/Model RDF/Redland/Node RDF/Redland/Parser RDF/Redland/Query RDF/Redland/QueryResults RDF/Redland/RSS RDF/Redland/Serializer RDF/Redland/Statement RDF/Redland/Storage RDF/Redland/Stream RDF/Redland/URI

DOC_C_FILES=$(DOC_CLASSES:%=$(srcdir)/../librdf/rdf_%.c)

PERL_HTML_POD_FILES=$(POD_MODULES:%=pod/%.html)
PYTHON_HTML_PYDOC_FILES=pydoc/RDF.html

CLEANFILES=README
MAINTAINERCLEANFILES=pod pydoc

PERL=perl

# Create some text files from HTML sources
LYNX=lynx
LYNXARGS=-dump -nolist

SUFFIXES = .html .txt .sgml .ps .pdf .pm .pod .3

pod: $(PERL_HTML_POD_FILES)

pydoc: $(PYTHON_HTML_PYDOC_FILES)

.html.txt:
	$(LYNX) $(LYNXARGS) $< > $@

@MAINT@$(PERL_HTML_POD_FILES): pod/%.html: $(srcdir)/../perl/lib/%.pm $(srcdir)/fix-pod-xhtml
@MAINT@	mkdir -p pod/RDF/Redland
@MAINT@	-pod2html --noindex --htmlroot /docs/pod --infile=$< | tidy -asxml -wrap 1000 2>/dev/null | perl $(srcdir)/fix-pod-xhtml $@
@MAINT@	rm -f pod2htm*
@MAINT@
@MAINT@$(srcdir)/../python/RDF.html: $(srcdir)/../python/RDF.py
@MAINT@	cd $(srcdir)/../python && make RDF.html
@MAINT@$(PYTHON_HTML_PYDOC_FILES): pydoc/%.html: $(srcdir)/../python/RDF.html $(srcdir)/fix-python-xhtml
@MAINT@	mkdir -p pydoc
@MAINT@	-tidy -asxml -wrap 1000 $< 2>/dev/null | perl $(srcdir)/fix-python-xhtml $@
@MAINT@.pod.html: $(srcdir)/fix-pod-xhtml
@MAINT@	-pod2html --noindex --htmlroot /docs/pod --infile=$< | tidy -asxml -wrap 1000 2>/dev/null | perl $(srcdir)/fix-pod-xhtml $@
@MAINT@	rm -f pod2htm*


clean-local:
	rm -rf pod2htm*

README: README.html
	$(LYNX) $(LYNXARGS) $< > $@

@SET_MAKE@
