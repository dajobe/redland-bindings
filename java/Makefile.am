# -*- Mode: Makefile -*-
#
# Makefile.am - automake file for Java interface to Redland
#
# $Id$
#
# Copyright (C) 2000-2001 David Beckett - http://purl.org/net/dajobe/
# Institute for Learning and Research Technology - http://www.ilrt.org/
# University of Bristol - http://www.bristol.ac.uk/
# 
# This package is Free Software or Open Source available under the
# following licenses (these are alternatives):
#   1. GNU Lesser General Public License (LGPL) Version 2
#   2. GNU General Public License (GPL) Version 2
#   3. Mozilla Public License (MPL) Version 1.1
# and no other versions of those licenses.
# 
# See INSTALL.html or INSTALL.txt at the top of this package for the
# full license terms.
# 

JAVA_PACKAGE=redland
JAVA_PACKAGE=UK.ac.bristol.ilrt.redland

JAVA_PATH=UK/ac/bristol/ilrt/redland

SWIG_OUTPUTS=$(JAVA_PACKAGE).java $(JAVA_PACKAGE)_wrap.c
JAVA_CRUFT=$(JAVA_PATH)/*.class \
UK_ac_bristol_ilrt_redland_redland.h

EXTRA_DIST=test.java $(SWIG_OUTPUTS)

CLEANFILES=test*db $(JAVA_PACKAGE)_wrap.o
MAINTAINERCLEANFILES=$(SWIG_OUTPUTS) $(SWIG_CRUFT)

$(JAVA_PACKAGE)_wrap.c: $(srcdir)/../Redland.i
	swig -v -java -module $(JAVA_PACKAGE) -o $@ $?

build-java: $(JAVA_PACKAGE).class
$(JAVA_PACKAGE).java: $(JAVA_PACKAGE).class
	javac $(JAVA_PACKAGE).java

install-java: build-java
check-java: build-java

clean-java:
	rm -f *.class

$(JAVA_PATH)/redland.class: $(JAVA_PATH)/redland.java
	javac $(JAVA_PATH)/*.java

UK_ac_bristol_ilrt_redland_redland.h: $(JAVA_PATH)/redland.class
	javah -jni $(JAVA_PACKAGE).redland

libredland-java.o: redland_java.c UK_ac_bristol_ilrt_redland_redland.h
	gcc -I/usr/local/include/linux -shared -o libredland-java.so redland-java.c -L.. -lrdf
# or maybe:
#gcc -o libredland-java.so -shared -Wl,-soname,libredland-java.so -I/usr/local/include/linux redland-java.c -static -L.. -lrdf -lc
#gcc -o libredland-java.so -shared -Wl,-soname,libredland-java.so -I/usr/local/include/linux redland-java.c -static `../redland-config --libs`
