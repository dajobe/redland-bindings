# -*- Mode: Makefile -*-
#
# Makefile.am - automake file for ECMA CLI (csharp) examples to Redland
#
# $Id$
#
# Copyright (C) 2004 David Beckett - http://purl.org/net/dajobe/
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


CSHARPC=@CSHARPC@
CSHARPCFLAGS=@CSHARPCFLAGS@
CLIRUN=@CLIRUN@

RUN=@REDLAND_RUN@

EXTRA_DIST= GetsTest.cs ModelGeneralTest.cs \
ModelTest.cs SimpleGetsTest.cs StreamTest.cs TemplateTest.cs	\
example1.cs example2.cs example3.cs example4.cs \
nunit-console.exe AssemblyInfo.cs ConsoleUi.cs EncodingTest.cs

NUNIT_TESTS = ModelTest.dll SimpleGetsTest.dll \
GetsTest.dll StreamTest.dll ModelGeneralTest.dll EncodingTest.dll

CSHARP_TESTS=example1.exe example2.exe example3.exe example4.exe

CLEANFILES=test*.db *.mdb \
example1.xml example2.out example3.out example4.xml \
$(CSHARP_TESTS) $(NUNIT_TESTS) TestResult.xml

# These should work for Mono and pnet
CSHARPLDFLAGS= $(CSHARPCFLAGS)
CSHARP_COMPILE=$(CSHARPC) $(CSHARPCFLAGS)
CSHARP_LINK=$(CSHARPC) $(CSHARPLDFLAGS)

SUFFIXES=.cs .dll .exe
.cs.dll:
	$(CSHARP_COMPILE) 
.cs.exe:
	$(CSHARP_LINK) 

NUNIT_FLAGS = $(shell pkg-config nunit-framework --libs) -r:../Redland.dll


check-local: $(CSHARP_TESTS) 
	@MONO_PATH=..; export MONO_PATH; \
	for prog in $(CSHARP_TESTS); do \
	  $(ECHO) Running $$prog; \
	  $(RUN) $(CLIRUN) $$prog; \
	done

example4.exe: example4.cs ../Redland.dll
	$(CSHARP_LINK) example4.cs -r:../Redland.dll -out:$@

example3.exe: example3.cs ../Redland.dll
	$(CSHARP_LINK) example3.cs -r:../Redland.dll -out:$@

example2.exe: example2.cs ../Redland.dll
	$(CSHARP_LINK) example2.cs -r:../Redland.dll -out:$@

example1.exe: example1.cs ../Redland.dll
	$(CSHARP_LINK) example1.cs -r:../Redland.dll -out:$@


unittests: $(NUNIT_TESTS) ../Redland.dll
	@MONO_PATH=..; export MONO_PATH; \
	for test in  $(NUNIT_TESTS); do \
	  $(ECHO) Running nunit-console $$test; \
	  $(RUN) nunit-console $$test; \
	done

ModelGeneralTest.dll: ModelGeneralTest.cs
	$(CSHARP_COMPILE) -target:library $(NUNIT_FLAGS) ModelGeneralTest.cs -out:$@

StreamTest.dll: StreamTest.cs
	$(CSHARP_COMPILE) -target:library $(NUNIT_FLAGS) StreamTest.cs -out:$@

GetsTest.dll: GetsTest.cs
	$(CSHARP_COMPILE) -target:library $(NUNIT_FLAGS) GetsTest.cs -out:$@

SimpleGetsTest.dll: SimpleGetsTest.cs
	$(CSHARP_COMPILE) -target:library $(NUNIT_FLAGS) SimpleGetsTest.cs -out:$@

ModelTest.dll: ModelTest.cs
	$(CSHARP_COMPILE) -target:library $(NUNIT_FLAGS) ModelTest.cs -out:$@

EncodingTest.dll: EncodingTest.cs
	$(CSHARP_COMPILE) -target:library $(NUNIT_FLAGS) EncodingTest.cs -out:$@


clean-local:
	rm -rf $(CSHARP_TESTS) *~ *.xml *.db

../Redland.dll:
	@cd .. && $(MAKE) Redland.dll

