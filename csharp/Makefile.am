# -*- Mode: Makefile -*-
#
# Makefile.am - automake file for ECMA CLI (csharp) interface to Redland
#
# $Id$
#
# Copyright (C) 2003 David Beckett - http://purl.org/net/dajobe/
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

CSHARPCC=@CSHARPCC@
CLRRUN=@CLRRUN@

CSHARP_C_SHLIB=redland-csharp
CSHARP_PACKAGE=Redland

CSHARP_SHARED_LIB=lib$(CSHARP_C_SHLIB).so

SWIG_OUTPUTS=$(CSHARP_PACKAGE)_wrap.c $(CSHARP_PACKAGE).cs $(CSHARP_PACKAGE)PINVOKE.cs
CSHARP_CRUFT=$(CSHARP_PACKAGE)_wrap.o

EXTRA_DIST=README test.cs example.cs \
$(SWIG_OUTPUTS)

CLEANFILES=test*db test-out.rdf $(CSHARP_CRUFT)
MAINTAINERCLEANFILES=$(SWIG_OUTPUTS)

$(CSHARP_PACKAGE)_wrap.c: $(srcdir)/../Redland.i
	swig -v -csharp -package $(CSHARP_PACKAGE) -o $@ $<
	rm -f SWIGTYPE_*

$(CSHARP_PACKAGE)_wrap.o: $(CSHARP_PACKAGE)_wrap.c
	$(CC) $(CFLAGS) `$(top_srcdir)/redland-src-config --cflags` -c -fpic -I$(top_srcdir)  $< -o $@

$(CSHARP_PACKAGE).so: $(CSHARP_PACKAGE)_wrap.o
	$(CC) -shared $< -o $@ `$(top_srcdir)/redland-src-config --libs`

build-csharp: $(CSHARP_PACKAGE).so

check-csharp: test-csharp
test-csharp: build-csharp
	$(CSHARP) test.cs

clean-csharp realclean-csharp:
	rm -f $(CSHARP_PACKAGE).so

install-csharp: $(CSHARP_PACKAGE).so build-csharp
	csharpdir=@prefix@/lib/csharp; \
	mkdir $$csharpdir/redland; \
	cp -p $(CSHARP_PACKAGE).so $$csharpdir/redland

dist-csharp:
	@echo make csharp dist somehow

clean-local: clean-csharp
