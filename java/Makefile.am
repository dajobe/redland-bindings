# -*- Mode: Makefile -*-
#
# Makefile.am - automake file for Java interface to Redland
#
# $Id$
#
# Copyright (C) 2000-2004 David Beckett - http://purl.org/net/dajobe/
# Institute for Learning and Research Technology - http://www.ilrt.bris.ac.uk/
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

AM_CPPFLAGS=@CPPFLAGS@ -I$(top_srcdir)/librdf -I$(top_builddir)/librdf @LIBRDF_CPPFLAGS@ @JDK_INCLUDES@
AM_CFLAGS=@CFLAGS@ @LIBRDF_CPPFLAGS@ $(MEM)

noinst_SCRIPTS=run-java

JAVA_MODULE=core
JAVA_PACKAGE=org.librdf.redland
JAVA_PATH=org/librdf/redland/
CLASSPATH=.

javalibdir=$(prefix)/lib/java
javasharedir=$(prefix)/share/java

javalib_LTLIBRARIES=librdf-java.la

librdf_java_la_SOURCES=core_wrap.c
librdf_java_la_LDFLAGS=--rpath $(javalibdir) @LIBRDF_LIBTOOLLIBS@

javashare_DATA=librdf-java.jar

JAVA_JAR=librdf-java.jar

SWIG_OUTPUTS=$(JAVA_PATH)$(JAVA_MODULE).java $(JAVA_MODULE)_wrap.c
JAVA_CRUFT=$(JAVA_PATH)$(JAVA_MODULE).class $(JAVA_MODULE)_wrap.o \
$(JAVA_JAR) skeleton.class \
$(JAVA_PATH)$(JAVA_MODULE)JNI.java

JAVA_SOURCE_FILES=$(JAVA_PATH)/Iterator.java $(JAVA_PATH)Hash.java	\
$(JAVA_PATH)Parser.java $(JAVA_PATH)URI.java $(JAVA_PATH)Node.java	\
$(JAVA_PATH)World.java $(JAVA_PATH)Serializer.java			\
$(JAVA_PATH)Statement.java $(JAVA_PATH)Model.java			\
$(JAVA_PATH)Storage.java $(JAVA_PATH)core.java				\
$(JAVA_PATH)Stream.java

JAVA_CLASS_FILES=$(JAVA_PATH)/Iterator.class $(JAVA_PATH)Hash.class	\
$(JAVA_PATH)Parser.class $(JAVA_PATH)URI.class $(JAVA_PATH)Node.class	\
$(JAVA_PATH)World.class $(JAVA_PATH)Serializer.class			\
$(JAVA_PATH)Statement.class $(JAVA_PATH)Model.class			\
$(JAVA_PATH)Storage.class $(JAVA_PATH)core.class			\
$(JAVA_PATH)Stream.class


SUBDIRS=org

# The SWIG outputs are included in the distribution so SWIG is not
# required to use the Java interface
EXTRA_DIST=redland-fragment.java test1.java test2.java \
example.java skeleton.java \
$(SWIG_OUTPUTS)

CLEANFILES=test*db $(JAVA_CRUFT)
MAINTAINERCLEANFILES=$(SWIG_OUTPUTS)


$(JAVA_MODULE)_wrap.c $(JAVA_PATH)$(JAVA_MODULE).java.in: @REDLAND_SWIG@
	here=`pwd`; \
	cd $(JAVA_PATH); \
	$(SWIG) -v -java -noproxy -module $(JAVA_MODULE) -package $(JAVA_PACKAGE) -o $(JAVA_MODULE)_wrap.c $<; \
	if test -r $(JAVA_MODULE)JNI.java; then \
	  echo "Fixing SWIG 1.3.15+ broken Java API"; \
	  rm -f $(JAVA_MODULE).java SWIGTYPE_*; \
	  sed -e 's/class $(JAVA_MODULE)JNI/public class $(JAVA_MODULE)/' $(JAVA_MODULE)JNI.java > $(JAVA_MODULE).java; \
	  sed -e 's/$(JAVA_MODULE)JNI/$(JAVA_MODULE)/g' $(JAVA_MODULE)_wrap.c > $(JAVA_MODULE)_wrap.c.new; \
	  rm -f $(JAVA_MODULE)_wrap.c $(JAVA_MODULE)JNI.java; \
	  mv $(JAVA_MODULE)_wrap.c.new $(JAVA_MODULE)_wrap.c; \
	fi; \
	mv $(JAVA_MODULE)_wrap.c $$here; \
	mv $(JAVA_MODULE).java $(JAVA_MODULE).java.in

# FIXME There must be a better way to do this with SWIG
$(JAVA_PATH)$(JAVA_MODULE).java: $(JAVA_PATH)$(JAVA_MODULE).java.in redland-fragment.java
	sed -e 's/^}//' $(JAVA_PATH)$(JAVA_MODULE).java.in > $(JAVA_PATH)$(JAVA_MODULE).java.tmp
	cat $(JAVA_PATH)$(JAVA_MODULE).java.tmp redland-fragment.java > $@
	echo "}" >> $@
	rm -f $(JAVA_PATH)$(JAVA_MODULE).java.tmp


clean-local:
	rm -f *.class

skeleton.class: skeleton.java $(JAVA_MODULE)_wrap.c $(JAVA_PATH)$(JAVA_MODULE).java librdf-java.la
	$(JAVAC) skeleton.java

$(JAVA_CLASS_FILES): skeleton.class
$(JAVA_JAR): $(JAVA_CLASS_FILES)
	rm -f $@
	jar cf $@ $(JAVA_CLASS_FILES)


check-local: test-java

test-java: test1 test2

test1: test1.class $(JAVA_JAR)
	$(srcdir)/run-java $(JAVA) test1

test1.class: test1.java $(JAVA_JAR)
	$(JAVAC) -classpath $(CLASSPATH) test1.java

test2: test2.class $(JAVA_JAR)
	$(srcdir)/run-java $(JAVA) -classpath $(CLASSPATH):$(JAVA_JAR) test2

test2.class: test2.java $(JAVA_JAR)
	$(JAVAC) -classpath $(CLASSPATH) test2.java

example: example.class $(JAVA_JAR)
	$(JAVA) -classpath $(CLASSPATH):$(JAVA_JAR) example

example.class: example.java $(JAVA_JAR)
	$(JAVAC) -classpath $(CLASSPATH):$(JAVA_JAR) example.java

install-exec-local:
	if test `uname` = Darwin; then \
	  (cd $(DESTDIR)$(javalibdir) && rm -f librdf-java.jnilib && ln -s librdf-java.dylib librdf-java.jnilib); \
	fi

all-local:
	if test `uname` = Darwin; then \
	  (cd .libs && rm -f librdf-java.jnilib && ln -s librdf-java.dylib librdf-java.jnilib); \
	fi
