# -*- Mode: Makefile -*-
#
# Makefile.am - automake file for ECMA CLI (csharp) interface to Redland
#
# $Id$
#
# Copyright (C) 2003-2004 David Beckett - http://purl.org/net/dajobe/
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

AM_CPPFLAGS=@CPPFLAGS@ -I$(top_srcdir)/librdf -I$(top_builddir)/librdf @LIBRDF_CPPFLAGS@ @LIBRDF_INTERNAL_CPPFLAGS@ @LIBRDF_EXTERNAL_CPPFLAGS@
AM_CFLAGS=@CFLAGS@ @LIBRDF_CPPFLAGS@ @LIBRDF_INTERNAL_CPPFLAGS@ @LIBRDF_EXTERNAL_CPPFLAGS@ $(MEM)

CSHARPC=@CSHARPC@
CLIRUN=@CLIRUN@

CSHARP_C_SHLIB=redland-csharp
CSHARP_PACKAGE=Redland

CSHARP_SHARED_LIB=lib$(CSHARP_C_SHLIB).so

SWIG_OUTPUTS=$(CSHARP_PACKAGE)_wrap.c $(CSHARP_PACKAGE).cs $(CSHARP_PACKAGE)PINVOKE.cs
CSHARP_CRUFT=$(CSHARP_PACKAGE)_wrap.o $(CSHARP_PACKAGE).dll \
$(CSHARP_SHARED_LIB) $(CSHARP_PACKAGE).so

fakedir=$(prefix)
fake_DATA=build-csharp-stamp

EXTRA_DIST=README test.cs \
$(SWIG_OUTPUTS)

CLEANFILES=test*db *.exe *.dll test-out.rdf $(CSHARP_CRUFT)
MAINTAINERCLEANFILES=$(SWIG_OUTPUTS)

# These work for Mono and dotGNU
CSHARP_COMPILE=$(CSHARPC) $(CSHARPCFLAGS) $< -target:library -out:$@
CSHARP_LINK=$(CSHARPC) $(CSHARPLDFLAGS) $< -target:exe -out:$@

SUFFIXES=.cs .dll .exe
.cs.dll:
	$(CSHARP_COMPILE) 
.cs.exe:
	$(CSHARP_LINK) 

$(CSHARP_PACKAGE)_wrap.c: $(srcdir)/../Redland.i
	swig -v -csharp -package $(CSHARP_PACKAGE) -o $@ $<
	rm -f SWIGTYPE_*

$(CSHARP_PACKAGE)_wrap.o: $(CSHARP_PACKAGE)_wrap.c
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS) $(STANDARD_CFLAGS) -fPIC -DPIC $< -c -o $@

$(CSHARP_SHARED_LIB): $(CSHARP_PACKAGE)_wrap.o
	$(CC) $(AM_CFLAGS) $(CFLAGS) $(AM_LDFLAGS) $(LDFLAGS) -shared $< `$(top_builddir)/redland-src-config --libs` -o $@

build-csharp: build-csharp-stamp

build-csharp-stamp: $(CSHARP_SHARED_LIB) $(CSHARP_PACKAGE)PINVOKE.dll
	touch build-csharp-stamp

check-local: build-csharp-stamp test-csharp

test-csharp: build-csharp-stamp test.exe
	$(top_builddir)/redland-src-config --run $(CLIRUN) test.exe

install-csharp: $(CSHARP_SHARED_LIB) build-csharp
	$(INSTALL_PROGRAM) $(CSHARP_SHARED_LIB) @libdir@
	$(INSTALL_PROGRAM) $(CSHARP_PACKAGE)PINVOKE.dll @libdir@

clean-local:
	rm -f build-csharp-stamp

# See this is the fake bit
install-fakeDATA: install-csharp
