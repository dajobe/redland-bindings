# -*- Mode: Makefile -*-
#
# Makefile.am - automake file for Tcl interface to Redland
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

TCL=tcl

TCL_PACKAGE=Redland

SWIG_OUTPUTS=$(TCL_PACKAGE)_wrap.c
SWIG_CRUFT=

tcldir=$(shell echo 'puts $$tcl_library' | $(TCL))
tcl_DATA=Redland.so

EXTRA_DIST=README test.tcl example.tcl \
$(SWIG_OUTPUTS)

CLEANFILES=test*db test-out.rdf core* $(TCL_PACKAGE)_wrap.o pkgIndex.tcl
MAINTAINERCLEANFILES=$(SWIG_OUTPUTS) $(SWIG_CRUFT)

$(TCL_PACKAGE)_wrap.c: $(srcdir)/../Redland.i
	swig -v -tcl8 -module $(TCL_PACKAGE) -o $@ $<

$(TCL_PACKAGE)_wrap.o: $(TCL_PACKAGE)_wrap.c
	$(LIBTOOL) --mode=compile $(CC) $(CFLAGS) -I$(top_srcdir)/librdf -c -fpic -I$(top_srcdir)  $< -o $@

$(TCL_PACKAGE).so: $(TCL_PACKAGE)_wrap.o $(top_builddir)/librdf/librdf.la
	$(LIBTOOL) --mode=link $(CC) $(TCL_PACKAGE)_wrap.o $(top_builddir)/librdf/librdf.la -o $@

pkgIndex.tcl: $(TCL_PACKAGE).so
	echo "pkg_mkIndex . $(TCL_PACKAGE).so" | $(TCL)

check-local: $(TCL_PACKAGE).so
	$(TCL) test.tcl
