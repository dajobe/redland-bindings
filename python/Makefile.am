# -*- Mode: Makefile -*-
#
# Makefile.am - automake file for Python interface to Redland
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

RELEASE=@VERSION_RELEASE@

AM_CPPFLAGS=@CPPFLAGS@ @LIBRDF_CPPFLAGS@ @PYTHON_INCLUDES@
AM_CFLAGS=@CFLAGS@ @LIBRDF_CPPFLAGS@ $(MEM)

AM_LDFLAGS=@LIBRDF_LDFLAGS@ $(MEM_LIBS)

PYTHON=@PYTHON@

PYTHON_PACKAGE=Redland

PYTHON_LDFLAGS=@PYTHON_LDFLAGS@

SWIG_OUTPUTS=$(PYTHON_PACKAGE)_wrap.c

pythondir=@PYTHON_LIB@
python_DATA=RDF.py Redland.so

EXTRA_DIST=README RDF.py \
example.py redlandtest.py \
redland-post.i redland-decl.i redland-typemap.i $(SWIG_OUTPUTS)

SUBDIRS=test

CLEANFILES=dmalloc.log *.db *.pyc test-out.rdf \
$(PYTHON_PACKAGE)_wrap.so $(PYTHON_PACKAGE)_wrap.o $(PYTHON_PACKAGE)-stamp \
$(PYTHON_PACKAGE).so $(PYTHON_PACKAGE).bundle
MAINTAINERCLEANFILES=$(SWIG_OUTPUTS) $(SWIG_CRUFT)

RUN=@REDLAND_RUN@

SWIG_OPTS=-I$(srcdir) -DREDLAND_POST_I -DREDLAND_DECL_I -DREDLAND_TYPEMAP_I

$(PYTHON_PACKAGE)_wrap.c: @REDLAND_SWIG@ redland-post.i redland-decl.i redland-typemap.i
	swig -v -python $(SWIG_OPTS) $(SWIG_PYTHON_ARGS) -module $(PYTHON_PACKAGE) -o $@ @REDLAND_SWIG@

$(PYTHON_PACKAGE)_wrap.so: $(srcdir)/$(PYTHON_PACKAGE)_wrap.c
	$(CC) $(DEFS) $(SWIG_OPTS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS) $(STANDARD_CFLAGS) -fPIC -DPIC $(srcdir)/$(PYTHON_PACKAGE)_wrap.c -c -o $@

$(PYTHON_PACKAGE).so: $(PYTHON_PACKAGE)-stamp
$(PYTHON_PACKAGE)-stamp: $(PYTHON_PACKAGE)_wrap.so
	$(CC) $(AM_CFLAGS) $(CFLAGS) $(AM_LDFLAGS) $(LDFLAGS) $(PYTHON_LDFLAGS) $(PYTHON_PACKAGE)_wrap.so `@REDLAND_CONFIG@ --libs` -o $(PYTHON_PACKAGE).so
	touch $(PYTHON_PACKAGE)-stamp

install-pythonDATA: $(python_DATA)
	$(mkinstalldirs) $(DESTDIR)$(pythondir)
	$(INSTALL_PROGRAM) $(PYTHON_PACKAGE).so $(DESTDIR)$(pythondir)/$(PYTHON_PACKAGE).so
	$(INSTALL_DATA) RDF.py $(DESTDIR)$(pythondir)/RDF.py

uninstall-pythonDATA: $(python_DATA)
	rm -f $(DESTDIR)$(pythondir)/$(PYTHON_PACKAGE).so $(DESTDIR)$(pythondir)/RDF.py

check-local: unittest-python test-python

test-python: $(PYTHON_PACKAGE).so
	PYTHONPATH=. \
	  $(RUN)$(PYTHON) $(srcdir)/test/test.py

unittest-python: $(PYTHON_PACKAGE).so
	PYTHONPATH=. \
	  $(RUN)$(PYTHON) $(srcdir)/redlandtest.py

RDF.html: RDF.py $(PYTHON_PACKAGE).so
	PYTHONPATH=. \
	  $(RUN)$(PYTHON) -c 'import pydoc; pydoc.cli()' -w RDF
