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


EXTRA_DIST=README test.tcl example.tcl \
$(SWIG_OUTPUTS)

CLEANFILES=test*db $(TCL_PACKAGE)_wrap.o
MAINTAINERCLEANFILES=$(SWIG_OUTPUTS) $(SWIG_CRUFT)

$(TCL_PACKAGE)_wrap.c: $(srcdir)/../Redland.i
	swig -v -tcl8 -module $(TCL_PACKAGE) -o $@ $?

$(TCL_PACKAGE)_wrap.o: $(TCL_PACKAGE)_wrap.c
	$(CC) $(CFLAGS) -c -fpic -I$(top_srcdir)  $? -o $@

$(TCL_PACKAGE).so: $(TCL_PACKAGE)_wrap.o
	$(CC) -shared $? -o $@ `$(top_srcdir)/redland-config --libs` -L$(top_srcdir) -lrdf

pkgIndex.tcl: $(TCL_PACKAGE).so
	$(TCL) -c "pkg_mkIndex . $(TCL_PACKAGE).so"

build-tcl: $(TCL_PACKAGE).so pkgIndex.tcl

check-tcl: test-tcl
test-tcl: build-tcl
	$(TCL) test.tcl

clean-tcl realclean-tcl:
	rm -f $(TCL_PACKAGE).so

install-tcl: $(TCL_PACKAGE).so pkgIndex.tcl build-tcl
	tcldir=`tcl -c 'puts [lindex $$auto_path 0]'`; \
	mkdir $$tcldir/redland; \
	cp -p $(TCL_PACKAGE).so pkgIndex.tcl $$tcldir/redland

dist-tcl:
	@echo make tcl dist somehow

clean-local: clean-tcl
	rm -rf build dist
