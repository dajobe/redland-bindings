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

INCLUDES=-I$(top_srcdir)

PYTHON=@PYTHON@
PYTHON_INCLUDES=@PYTHON_INCLUDES@

PYTHON_PACKAGE=Redland

SWIG_OUTPUTS=$(PYTHON_PACKAGE)_wrap.c
SWIG_CRUFT=$(PYTHON_PACKAGE).bs blib pm_to_blib


EXTRA_DIST=MANIFEST.in README setup.cfg setup.py RDF.py \
example.py redlandtest.py \
$(SWIG_OUTPUTS)

SUBDIRS=test

CLEANFILES=test*db core* test-out.rdf $(PYTHON_PACKAGE)_wrap.o
MAINTAINERCLEANFILES=$(SWIG_OUTPUTS) $(SWIG_CRUFT)

PYTHON_SETUP=PACKAGE=$(PYTHON_PACKAGE) VERSION=$(VERSION) TOP_SRCDIR=$(top_srcdir) MEM_LIBS=$(MEM_LIBS) MEM=$(MEM) $(PYTHON) $(srcdir)/setup.py

$(PYTHON_PACKAGE)_wrap.c: $(srcdir)/../Redland.i
	swig -v -python $(SWIG_PYTHON_ARGS) -module $(PYTHON_PACKAGE) -o $@ $<

build-python: $(PYTHON_PACKAGE)_wrap.c
	$(PYTHON_SETUP) build

check-python: test-python
test-python: build-python
	@bdir=`ls -1d build/lib.*`; \
	echo Using python build dir $$bdir; \
	echo PYTHONPATH=$$bdir $(PYTHON) test/test.py; \
	PYTHONPATH=$$bdir $(PYTHON) test/test.py

unittest-python: build-python
	@bdir=`ls -1d build/lib.*`; \
	echo Using python build dir $$bdir; \
	echo PYTHONPATH=$$bdir $(PYTHON) redlandtest.py; \
	PYTHONPATH=$$bdir $(PYTHON) redlandtest.py

clean-python:
	rm -rf build
	$(PYTHON_SETUP) clean

realclean-python:
	$(PYTHON_SETUP) clean --all

install-python: $(srcdir)/setup.py build-python
	$(PYTHON_SETUP) install

dist-python: sdist-python

# Built distribution (may be binary) for module
bdist-python: $(srcdir)/setup.py
	$(PYTHON_SETUP) bdist

# Source distributionn for module
sdist-python: $(srcdir)/setup.py
	$(PYTHON_SETUP) sdist

# RPM distribution for module
rpmdist-python: $(srcdir)/setup.py
	$(PYTHON_SETUP) bdist_rpm --release=$(RELEASE)

clean-local: clean-python
	rm -rf build dist

RDF.html: RDF.py build-python
	@bdir=`ls -1d build/lib.*`; \
	echo Using python build dir $$bdir; \
	echo PYTHONPATH=$$bdir $(PYTHON); \
	PYTHONPATH=$$bdir $(PYTHON) -c 'import pydoc; pydoc.cli()' -w RDF
