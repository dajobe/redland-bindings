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
CSHARPCFLAGS=@CSHARPCFLAGS@
CLIRUN=@CLIRUN@
CLI_PREFIX=@CLI_PREFIX@

CSHARP_C_SHLIB=redland-csharp
CSHARP_PACKAGE=Redland

CSHARP_DLL=$(CSHARP_PACKAGE).dll

CSHARP_DLL_SRCS=IWrapper.cs World.cs Uri.cs Storage.cs Model.cs		\
Parser.cs Node.cs Statement.cs Stream.cs Iterator.cs Serializer.cs	\
Redland.cs MemoryStorage.cs HashStorage.cs util.cs

CSHARP_CRUFT=$(CSHARP_PACKAGE).dll

fakedir=$(prefix)
fake_DATA=build-csharp-stamp

SUBDIRS=examples

EXTRA_DIST=README test.cs $(CSHARP_DLL_SRCS)

CLEANFILES=test*db *.exe *.dll test-out.rdf $(CSHARP_CRUFT)
#MAINTAINERCLEANFILES=

# These should work for Mono and pnet
CSHARPLDFLAGS= $(CSHARPCFLAGS)
CSHARP_COMPILE=$(CSHARPC) $(CSHARPCFLAGS)
CSHARP_LINK=$(CSHARPC) $(CSHARPLDFLAGS)

SUFFIXES=.cs .dll .exe
.cs.dll:
	$(CSHARP_COMPILE) 
.cs.exe:
	$(CSHARP_LINK) 

$(CSHARP_DLL): $(CSHARP_DLL_SRCS)
	$(CSHARP_COMPILE) -target:library $(CSHARP_DLL_SRCS) -out:$@

build-csharp: build-csharp-stamp

build-csharp-stamp: $(CSHARP_DLL)
	touch build-csharp-stamp

check-local: build-csharp-stamp test-csharp

test-csharp: build-csharp-stamp test.exe
	$(top_builddir)/redland-src-config --run $(CLIRUN) test.exe

test.exe: test.cs Redland.dll
	$(CSHARP_LINK) test.cs -target:exe -r:Redland.dll -out:$@

install-csharp: build-csharp-stamp
	$(INSTALL_PROGRAM) $(CSHARP_DLL) @libdir@

clean-local:
	rm -f build-csharp-stamp $(CSHARP_DLL)

# See this is the fake bit
install-fakeDATA: install-csharp


#MONO_GACUTIL=$(CLIRUN) $(CLI_PREFIX)/gacutil.exe
MONO_GACUTIL=gacutil
PNET_GACUTL=ilgac

install-mono: Redland.dll
	$(MONO_GACUTIL) -i Redland.dll -f -package 1.0 -gacdir $(CLI_PREFIX)/lib -root $(CLI_PREFIX)/lib

install-pnet: Redland.dll
	$(PNET_GACUTIL) --install --default Redland.dll 1.0
