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

RELEASE=@VERSION_RELEASE@

INCLUDES=-I$(top_srcdir)

TCL=@TCL@

TCL_PACKAGE=Redland

SWIG_OUTPUTS=$(TCL_PACKAGE)_wrap.c
SWIG_CRUFT=

tclversion=@TCL_VERSION@
tclincdir=@TCL_INCLUDE@
tcllib=-ltcl$(tclversion)

tcldir=$(shell echo 'puts $$tcl_library' | $(TCL))
tcl_DATA=Redland.so

EXTRA_DIST=test.tcl example.tcl \
$(SWIG_OUTPUTS)

CLEANFILES=*.db test-out.rdf core* \
$(TCL_PACKAGE)_wrap.lo $(TCL_PACKAGE)_wrap.o $(TCL_PACKAGE)-stamp \
$(TCL_PACKAGE).so $(TCL_PACKAGE).dylib \
pkgIndex.tcl
MAINTAINERCLEANFILES=$(SWIG_OUTPUTS) $(SWIG_CRUFT)

$(TCL_PACKAGE)_wrap.c: $(srcdir)/../Redland.i
	swig -v -tcl8 -module $(TCL_PACKAGE) -o $@ $<

$(TCL_PACKAGE)_wrap.so: $(TCL_PACKAGE)_wrap.c
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS) $(STANDARD_CFLAGS) -fPIC -DPIC -I$(top_srcdir)/librdf -I$(tclincdir) $(TCL_PACKAGE)_wrap.c -c -o $@


$(TCL_PACKAGE).so: $(TCL_PACKAGE)-stamp
$(TCL_PACKAGE)-stamp: $(TCL_PACKAGE)_wrap.so
	@if test `uname` = Darwin; then \
	  args=" -dynamiclib -flat_namespace -undefined suppress -compatibility_version 1 -current_version 1.0"; \
	else \
	  args=" -shared"; \
	fi; \
	echo $(CC) $(AM_CFLAGS) $(CFLAGS) $(AM_LDFLAGS) $(LDFLAGS) $$args $(TCL_PACKAGE)_wrap.so $(tcllib) `$(top_srcdir)/redland-src-config --libs` -o $(TCL_PACKAGE).so; \
	$(CC) $(AM_CFLAGS) $(CFLAGS) $(AM_LDFLAGS) $(LDFLAGS) $$args $(TCL_PACKAGE)_wrap.so $(tcllib) `$(top_srcdir)/redland-src-config --libs` -o $(TCL_PACKAGE).so
	if test `uname` = Darwin; then \
	  $(INSTALL_PROGRAM) $(TCL_PACKAGE).so $(TCL_PACKAGE).dylib; \
	fi
	touch $(TCL_PACKAGE)-stamp

install-tclDATA: $(tcl_DATA)
	if test -r $(TCL_PACKAGE).dylib; then \
	  $(INSTALL_PROGRAM) $(TCL_PACKAGE).dylib $(DESTDIR)$(tcldir)/$(TCL_PACKAGE).dylib; \
	else \
	  $(INSTALL_PROGRAM) $(TCL_PACKAGE).so $(DESTDIR)$(tcldir)/$(TCL_PACKAGE).so; \
	fi

uninstall-tclDATA: $(tcl_DATA)
	if test -r $(TCL_PACKAGE).dylib; then \
	  rm -f $(DESTDIR)$(tcldir)/$(TCL_PACKAGE).dylib; \
	else \
	  rm -f $(DESTDIR)$(tcldir)/$(TCL_PACKAGE).so; \
	fi

pkgIndex.tcl: $(TCL_PACKAGE).so
	if test -r $(TCL_PACKAGE).dylib; then \
	  echo "pkg_mkIndex -verbose . $(TCL_PACKAGE).dylib" | $(TCL); \
	else \
	  echo "pkg_mkIndex -verbose . $(TCL_PACKAGE).so" | $(TCL); \
	fi

check-local: test-tcl

test-tcl: $(TCL_PACKAGE).so pkgIndex.tcl $(srcdir)/test.tcl
	$(TCL) $(srcdir)/test.tcl
