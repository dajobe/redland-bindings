# -*- Mode: Makefile -*-
#
# Makefile.am - automake file for Python interface to Redland
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

RELEASE=@VERSION_RELEASE@

# SWIG 1.3.27+ Needs SWIG_PYTHON_SILENT_MEMLEAK: I know better than SWIG
swig_flags= -DSWIG_PYTHON_SILENT_MEMLEAK

PYTHON_INCLUDES=@PYTHON_INCLUDES@

AM_CPPFLAGS=@CPPFLAGS@ @LIBRDF_CPPFLAGS@ $(PYTHON_INCLUDES) $(swig_flags)
AM_CFLAGS=@CFLAGS@ @LIBRDF_CPPFLAGS@ $(MEM) $(swig_flags)

# This module needs Rasqal and Raptor symbols
AM_LDFLAGS=@LIBRDF_LDFLAGS@ $(MEM_LIBS) @RASQAL_LDFLAGS@

PYTHON=@PYTHON@

PYTHON_PACKAGE=Redland

PYTHON_LDFLAGS=@PYTHON_LDFLAGS@

PYTHON_LIBEXT=@PYTHON_LIBEXT@

# Redland.py wraps and loads _Redland.so/dylib
PYTHON_FULL_DYLIB=_$(PYTHON_PACKAGE)$(PYTHON_LIBEXT)

SWIG_OUTPUTS=$(PYTHON_PACKAGE)_wrap.c Redland.py

pythondir=@PYTHON_LIB@
python_DATA=RDF.py $(PYTHON_FULL_DYLIB) Redland.py

EXTRA_DIST=README RDF.py \
example.py redlandtest.py \
redland-pre.i redland-post.i redland-decl.i redland-typemap.i $(SWIG_OUTPUTS)

SUBDIRS=test

CLEANFILES=*.db *.pyc test-out.rdf \
$(PYTHON_PACKAGE)_wrap.so $(PYTHON_PACKAGE)_wrap.o $(PYTHON_PACKAGE)-stamp \
$(PYTHON_FULL_DYLIB) $(PYTHON_PACKAGE).bundle
MAINTAINERCLEANFILES=$(SWIG_OUTPUTS) $(SWIG_CRUFT)

RUN=@REDLAND_RUN@

SWIG_OPTS=-I$(srcdir) -I$(srcdir)/.. -DREDLAND_PRE_I -DREDLAND_POST_I -DREDLAND_DECL_I -DREDLAND_TYPEMAP_I

$(PYTHON_PACKAGE)_wrap.c: @REDLAND_SWIG@ redland-post.i redland-decl.i redland-typemap.i
	swig -v -python $(SWIG_OPTS) $(SWIG_PYTHON_ARGS) -module $(PYTHON_PACKAGE) -o $@ @REDLAND_SWIG@

$(PYTHON_PACKAGE)_wrap.so: $(srcdir)/$(PYTHON_PACKAGE)_wrap.c
	$(CC) $(DEFS) $(SWIG_OPTS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS) $(STANDARD_CFLAGS) -fPIC -DPIC $(srcdir)/$(PYTHON_PACKAGE)_wrap.c -c -o $@

$(PYTHON_FULL_DYLIB): $(PYTHON_PACKAGE)-stamp
$(PYTHON_PACKAGE)-stamp: $(PYTHON_PACKAGE)_wrap.so
	$(CC) $(AM_CFLAGS) $(CFLAGS) $(PYTHON_PACKAGE)_wrap.so $(PYTHON_LDFLAGS) $(AM_LDFLAGS) $(LDFLAGS) -o $(PYTHON_FULL_DYLIB)
	touch $(PYTHON_PACKAGE)-stamp

install-pythonDATA: $(python_DATA)
	$(mkinstalldirs) $(DESTDIR)$(pythondir)
	$(INSTALL_PROGRAM) $(PYTHON_FULL_DYLIB) $(DESTDIR)$(pythondir)/$(PYTHON_FULL_DYLIB)
	$(INSTALL_DATA) RDF.py $(DESTDIR)$(pythondir)/RDF.py
	$(INSTALL_DATA) Redland.py $(DESTDIR)$(pythondir)/Redland.py

uninstall-pythonDATA: $(python_DATA)
	rm -f $(DESTDIR)$(pythondir)/$(PYTHON_FULL_DYLIB) $(DESTDIR)$(pythondir)/RDF.{py,pyc} $(DESTDIR)$(pythondir)/Redland.{py,pyc}

clean-local:
	rm -rf __pycache__

check-local: unittest-python test-python

test-python: $(PYTHON_FULL_DYLIB)
	PYTHONPATH=. \
	  $(RUN)$(PYTHON) $(srcdir)/test/test.py

unittest-python: $(PYTHON_FULL_DYLIB)
	PYTHONPATH=. \
	  $(RUN)$(PYTHON) $(srcdir)/redlandtest.py

RDF.html: RDF.py $(PYTHON_FULL_DYLIB)
	PYTHONPATH=. \
	  $(RUN)$(PYTHON) -c 'import pydoc; pydoc.cli()' -w RDF
