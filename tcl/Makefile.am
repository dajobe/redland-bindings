# -*- Mode: Makefile -*-
#
# Makefile.am - automake file for Tcl interface to Redland
#
# $Id$
#
# Copyright (C) 2000-2003 David Beckett - http://purl.org/net/dajobe/
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

TCL=@TCL@

TCL_PACKAGE=Redland

SWIG_OUTPUTS=$(TCL_PACKAGE)_wrap.c
SWIG_CRUFT=

tclversion=$(shell echo 'puts [info tclversion]' | $(TCL))
tclincdir=/usr/include/tcl$(tclversion)
tcllib=-ltcl$(tclversion)

tcldir=$(shell echo 'puts $$tcl_library' | $(TCL))
tcl_DATA=Redland.so

EXTRA_DIST=README test.tcl example.tcl \
$(SWIG_OUTPUTS)

CLEANFILES=*.db test-out.rdf core* $(TCL_PACKAGE)_wrap.o Redland.so \
pkgIndex.tcl
MAINTAINERCLEANFILES=$(SWIG_OUTPUTS) $(SWIG_CRUFT)

$(TCL_PACKAGE)_wrap.c: $(srcdir)/../Redland.i
	swig -v -tcl8 -module $(TCL_PACKAGE) -o $@ $<

$(TCL_PACKAGE)_wrap.o: $(TCL_PACKAGE)_wrap.c
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS) $(STANDARD_CFLAGS) -fPIC -DPIC -I$(top_srcdir)/librdf -I$(tclincdir) $(TCL_PACKAGE)_wrap.c -c -o $@

$(TCL_PACKAGE).so: $(TCL_PACKAGE)_wrap.o $(top_builddir)/librdf/librdf.la
	$(CC) $(AM_CFLAGS) $(CFLAGS) $(AM_LDFLAGS) $(LDFLAGS) -shared $(TCL_PACKAGE)_wrap.o $(tcllib) `$(top_srcdir)/redland-src-config --libs` -o $@ 

# Convince automake to install this as a program
tclDATA_INSTALL = $(INSTALL_PROGRAM)

pkgIndex.tcl: $(TCL_PACKAGE).so
	echo "pkg_mkIndex . $(TCL_PACKAGE).so" | $(TCL)

check-local: test-tcl

test-tcl: $(TCL_PACKAGE).so pkgIndex.tcl
	$(TCL) test.tcl
