# -*- Mode: Makefile -*-
#
# Makefile.am - automake file for Python interface to Redland
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

RELEASE=@VERSION_RELEASE@

AM_CPPFLAGS=-I$(top_srcdir)

PYTHON=@PYTHON@
PYTHON_INCLUDES=@PYTHON_INCLUDES@

PYTHON_PACKAGE=Redland

SWIG_OUTPUTS=$(PYTHON_PACKAGE)_wrap.c

pythondir=@PYTHON_LIB@
python_DATA=RDF.py Redland.so

EXTRA_DIST=MANIFEST.in README RDF.py \
example.py redlandtest.py \
$(SWIG_OUTPUTS)

SUBDIRS=test

CLEANFILES=dmalloc.log *.db *.pyc test-out.rdf \
$(PYTHON_PACKAGE)_wrap.so $(PYTHON_PACKAGE)_wrap.o $(PYTHON_PACKAGE)-stamp \
$(PYTHON_PACKAGE).so $(PYTHON_PACKAGE).bundle
MAINTAINERCLEANFILES=$(SWIG_OUTPUTS) $(SWIG_CRUFT)

$(PYTHON_PACKAGE)_wrap.c: $(srcdir)/../Redland.i
	swig -v -python $(SWIG_PYTHON_ARGS) -module $(PYTHON_PACKAGE) -o $@ $<

$(PYTHON_PACKAGE)_wrap.so: $(srcdir)/$(PYTHON_PACKAGE)_wrap.c
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS) $(STANDARD_CFLAGS) -fPIC -DPIC -I$(top_builddir)/librdf -I$(top_srcdir)/librdf/ @LIBRDF_EXTERNAL_CPPFLAGS@ $(PYTHON_INCLUDES) $(srcdir)/$(PYTHON_PACKAGE)_wrap.c -c -o $@

$(PYTHON_PACKAGE).so: $(PYTHON_PACKAGE)-stamp
$(PYTHON_PACKAGE)-stamp: $(PYTHON_PACKAGE)_wrap.so
	@if test `uname` = Darwin; then \
	  args="  -Wl,-F. -Wl,-F. -bundle -framework Python"; \
	else \
	  args=" -shared"; \
	fi; \
	echo $(CC) $(AM_CFLAGS) $(CFLAGS) $(AM_LDFLAGS) $(LDFLAGS) $$args $(PYTHON_PACKAGE)_wrap.so `$(top_builddir)/redland-src-config --libs` -o $(PYTHON_PACKAGE).so; \
	$(CC) $(AM_CFLAGS) $(CFLAGS) $(AM_LDFLAGS) $(LDFLAGS) $$args $(PYTHON_PACKAGE)_wrap.so `$(top_builddir)/redland-src-config --libs` -o $(PYTHON_PACKAGE).so
	touch $(PYTHON_PACKAGE)-stamp

install-pythonDATA: $(python_DATA)
	$(mkdir_p) $(DESTDIR)$(pythondir)
	$(INSTALL_PROGRAM) $(PYTHON_PACKAGE).so $(DESTDIR)$(pythondir)/$(PYTHON_PACKAGE).so
	$(INSTALL_DATA) RDF.py $(DESTDIR)$(pythondir)/RDF.py

uninstall-pythonDATA: $(python_DATA)
	rm -f $(DESTDIR)$(pythondir)/$(PYTHON_PACKAGE).so $(DESTDIR)$(pythondir)/RDF.py

check-local: unittest-python test-python

test-python: $(PYTHON_PACKAGE).so
	PYTHONPATH=. $(top_builddir)/redland-src-config --run \
	  $(PYTHON) $(srcdir)/test/test.py

unittest-python: $(PYTHON_PACKAGE).so
	PYTHONPATH=. $(top_builddir)/redland-src-config --run \
	  $(PYTHON) $(srcdir)/redlandtest.py

RDF.html: RDF.py $(PYTHON_PACKAGE).so
	PYTHONPATH=. $(top_builddir)/redland-src-config --run \
	  $(PYTHON) -c 'import pydoc; pydoc.cli()' -w RDF
