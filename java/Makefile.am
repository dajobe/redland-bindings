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

JAVA=@JAVA@
JAVAC=@JAVAC@
SWIG=@SWIG@

JAVA_MODULE=redland
JAVA_PACKAGE=org.librdf
JAVA_PATH=org/librdf/

JAVA_SHARED_LIB=lib$(JAVA_MODULE)-java.so

SWIG_OUTPUTS=$(JAVA_PATH)$(JAVA_MODULE).java $(JAVA_MODULE)_wrap.c
JAVA_CRUFT=$(JAVA_PATH)$(JAVA_MODULE).class $(JAVA_MODULE)_wrap.o \
$(JAVA_SHARED_LIB) skeleton.class

SUBDIRS=org

# The SWIG outputs are included in the distribution so SWIG is not
# required to use the Java interface
EXTRA_DIST=redland-fragment.java test.java example.java skeleton.java \
$(SWIG_OUTPUTS)

CLEANFILES=test*db $(JAVA_CRUFT)
MAINTAINERCLEANFILES=$(SWIG_OUTPUTS)


$(JAVA_PATH)$(JAVA_MODULE).java.in: $(srcdir)/../Redland.i
	$(SWIG) -v -java -jnic -module $(JAVA_MODULE) -package $(JAVA_PACKAGE) -o $(JAVA_MODULE)_wrap.c $? && cp $(JAVA_MODULE).java $(JAVA_PATH)$(JAVA_MODULE).java.in


# FIXME There must be a better way to do this with SWIG
$(JAVA_PATH)$(JAVA_MODULE).java: $(JAVA_PATH)$(JAVA_MODULE).java.in redland-fragment.java
	sed -e 's/}//' $(JAVA_PATH)$(JAVA_MODULE).java.in > $(JAVA_PATH)$(JAVA_MODULE).java.tmp
	cat $(JAVA_PATH)$(JAVA_MODULE).java.tmp redland-fragment.java > $@
	echo "}" >> $@
	rm -f $(JAVA_PATH)$(JAVA_MODULE).java.tmp


build-java: $(JAVA_MODULE)_wrap.c $(JAVA_SHARED_LIB) skeleton.class
install-java: build-java
check-java: build-java test-java

clean-local: clean-java

clean-java:
	target=`echo $@ | sed -e 's/-java//'`; \
	cd org && $(MAKE) $$target
	rm -f *.class

# A fake target to get all the class files compiled down the tree
compiled-java-classes: skeleton.class

skeleton.class: skeleton.java $(JAVA_MODULE)_wrap.c $(JAVA_PATH)$(JAVA_MODULE).java $(JAVA_SHARED_LIB)
	$(JAVAC) -classpath . skeleton.java

test-java: test.class compiled-java-classes
	$(JAVA) -classpath . test

test.class: test.java compiled-java-classes
	$(JAVAC) -classpath . test.java

example: example.class compiled-java-classes
	$(JAVA) -classpath . example

example.class: example.java compiled-java-classes
	$(JAVAC) -classpath . example.java


$(JAVA_SHARED_LIB): $(JAVA_MODULE)_wrap.c
	gcc -shared -Wl,-soname,$(JAVA_SHARED_LIB) -I.. -I/usr/local/include/linux $? -static -L.. `../redland-config --libs` -o $@
