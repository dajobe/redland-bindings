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
# There is a lot of fakery going on here in order that libtool deals
# with building the shared object and dynamically loaded library.
# libtool is fooled into building a dynamically loaded library
# (-rpath PWD in the link line) called libfake.la which is installed
# into the working directory.  [Note: Unlike ruby that needs -module]
#
# This libfake.dylib/.so is then suitable for loading dynamically.  On
# Darwin/OSX dynamic libraries are Redland.dylib so it is copied
# to that name.  Everything else gets Redland.so.
#
# To stop recompiles when either of the above are built, a
# stamp file $(TCL_PACKAGE)-stamp is used to control this.
#
# On install/unstall custom targets (install-tclDATA, uninstall-tclDATA)
# copy the appropriate file above.
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

CLEANFILES=*.db test-out.rdf core* \
$(TCL_PACKAGE)_wrap.lo $(TCL_PACKAGE)_wrap.o $(TCL_PACKAGE)-stamp \
Redland.so Redland.dylib \
libfake* \
pkgIndex.tcl
MAINTAINERCLEANFILES=$(SWIG_OUTPUTS) $(SWIG_CRUFT)

$(TCL_PACKAGE)_wrap.c: $(srcdir)/../Redland.i
	swig -v -tcl8 -module $(TCL_PACKAGE) -o $@ $<

# OLD action:
#	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS) $(STANDARD_CFLAGS) -fPIC -DPIC -I$(top_srcdir)/librdf -I$(tclincdir) $(TCL_PACKAGE)_wrap.c -c -o $@
$(TCL_PACKAGE)_wrap.lo: $(TCL_PACKAGE)_wrap.c
	$(LIBTOOL) --mode=compile $(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS) $(STANDARD_CFLAGS) -prefer-pic -I$(top_srcdir)/librdf -I$(top_builddir)/librdf -I$(tclincdir) $(srcdir)/$(TCL_PACKAGE)_wrap.c -c -o $@


# OLD action (for $(TCL_PACKAGE).so )
#	$(CC) $(AM_CFLAGS) $(CFLAGS) $(AM_LDFLAGS) $(LDFLAGS) -shared $(TCL_PACKAGE)_wrap.o $(tcllib) `$(top_srcdir)/redland-src-config --libs` -o $@ 
$(TCL_PACKAGE).so: $(TCL_PACKAGE)-stamp
$(TCL_PACKAGE)-stamp: $(TCL_PACKAGE)_wrap.lo
	destdir=`pwd`; \
	$(LIBTOOL) --mode=link $(CC) $(AM_CFLAGS) $(CFLAGS) $(AM_LDFLAGS) $(LDFLAGS) -o libfake.la -module -rpath $$destdir $(TCL_PACKAGE)_wrap.lo $(top_builddir)/librdf/librdf.la $(tcllib); \
	$(LIBTOOL) --mode=install install libfake.la $(DESTDIR)$$destdir/libfake.la
	if test -r libfake.dylib; then \
	  $(INSTALL_PROGRAM) libfake.dylib $(TCL_PACKAGE).dylib; \
	else \
	  $(INSTALL_PROGRAM) libfake.so $(TCL_PACKAGE).so; \
	fi
	rm -f libfake*
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
	  echo "pkg_mkIndex . $(TCL_PACKAGE).dylib" | $(TCL); \
	else \
	  echo "pkg_mkIndex . $(TCL_PACKAGE).so" | $(TCL); \
	fi

check-local: test-tcl

test-tcl: $(TCL_PACKAGE).so pkgIndex.tcl $(srcdir)/test.tcl
	$(TCL) $(srcdir)/test.tcl
