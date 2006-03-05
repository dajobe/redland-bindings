# -*- Mode: Makefile -*-
#
# Makefile.am - docs automake file for Redland Bindings
#
# $Id$
#
# Copyright (C) 2000-2005, David Beckett http://purl.org/net/dajobe/
# Copyright (C) 2000-2005, University of Bristol, UK http://www.bristol.ac.uk/
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
# complete terms and further detail along with the license texts for
# the licenses in COPYING.LIB, COPYING and LICENSE-2.0.txt respectively.
# 
# 

EXTRA_DIST=README.html README \
csharp.html perl.html python.html tcl.html java.html ruby.html php.html \
fix-pod-xhtml fix-python-xhtml fix-rdoc-xhtml \
pod pydoc rdoc

POD_MODULES=RDF/Redland RDF/Redland/Iterator RDF/Redland/Model RDF/Redland/Node RDF/Redland/Parser RDF/Redland/Query RDF/Redland/QueryResults RDF/Redland/RSS RDF/Redland/Serializer RDF/Redland/Statement RDF/Redland/Storage RDF/Redland/Stream RDF/Redland/URI RDF/Redland/BlankNode RDF/Redland/URINode RDF/Redland/LiteralNode RDF/Redland/XMLLiteralNode

DOC_C_FILES=$(DOC_CLASSES:%=$(srcdir)/../librdf/rdf_%.c)

PERL_HTML_POD_FILES=$(POD_MODULES:%=pod/%.html)
PYTHON_HTML_PYDOC_FILES=pydoc/RDF.html

RUBY_FILES= model node parser query resource serializer statement	\
store stream uri schemas/foaf schemas/owl schemas/rdfs dc util		\
queryresults

RUBY_CLASSES= FOAF DC Redland Redland/OWL Redland/Util Redland/RDFS	\
Redland/Serializer Redland/NodeTypeError Redland/Uri			\
Redland/HashStore Redland/Query Redland/HashOpen Redland/Parser		\
Redland/Node Redland/Model Redland/ContextParser Redland/Stream		\
Redland/MemoryStore Redland/Statement Redland/Literal			\
Redland/RedlandError Redland/NodeIterator Redland/FileStore		\
Redland/Namespace Redland/World Redland/TripleStore Redland/Resource	\
Redland/MergedModel Redland/BNode Redland/QueryResults

RUBY_HTML_SOURCE_FILES=$(RUBY_FILES:%=rdoc/files/rdf/redland/%_rb.html)
RUBY_HTML_CLASS_FILES=$(RUBY_CLASSES:%=rdoc/classes/%.html)

RUBY_HTML_RDOC_FILES= \
$(RUBY_HTML_SOURCE_FILES) \
rdoc/files/rdf/redland_rb.html \
rdoc/fr_file_index.html \
$(RUBY_HTML_CLASS_FILES) \
rdoc/fr_class_index.html \
rdoc/fr_method_index.html \
rdoc/index.html
RUBY_EXTRA_RDOC_FILES=rdoc/rdoc-style.css

CLEANFILES=README
MAINTAINERCLEANFILES=pod pydoc rdoc-dirs-stamp

PERL=perl

# Create some text files from HTML sources
LYNX=lynx
LYNXARGS=-dump -nolist

SUFFIXES = .html .txt .sgml .ps .pdf .pm .pod .3

pod: $(PERL_HTML_POD_FILES)

pydoc: $(PYTHON_HTML_PYDOC_FILES)

rdoc: $(RUBY_HTML_RDOC_FILES) $(RUBY_EXTRA_RDOC_FILES)

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
@MAINT@
@MAINT@$(srcdir)/../ruby/rdoc-stamp:
@MAINT@	cd $(srcdir)/../ruby && make docs
@MAINT@rdoc-dirs-stamp:
@MAINT@	mkdir -p rdoc rdoc/classes rdoc/classes/Redland rdoc/files rdoc/files/rdf rdoc/files/rdf/redland rdoc/files/rdf/redland/schemas && touch $@
@MAINT@$(RUBY_HTML_RDOC_FILES): rdoc/%.html: $(srcdir)/../ruby/rdoc-stamp $(srcdir)/fix-rdoc-xhtml rdoc-dirs-stamp
@MAINT@	-tidy -asxml -wrap 1000 $(srcdir)/../ruby/$@ 2>/dev/null | perl $(srcdir)/fix-rdoc-xhtml $@
@MAINT@$(RUBY_EXTRA_RDOC_FILES): $(srcdir)/../ruby/rdoc-stamp rdoc-dirs-stamp
@MAINT@	$(INSTALL_DATA) $(srcdir)/../ruby/$@ $@


clean-local:
	rm -rf pod2htm*

README: README.html
	$(LYNX) $(LYNXARGS) $< > $@

@SET_MAKE@
