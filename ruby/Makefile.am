# -*- Mode: Makefile -*-
#
# Makefile.am - automake file for Ruby interface to Redland
# hacked from Tcl one by danbri@w3.org
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

INCLUDES=-I$(top_srcdir) @RUBY_INCLUDES@

RUBY=@RUBY@

RUBY_PACKAGE=Redland

SWIG_OUTPUTS=$(RUBY_PACKAGE)_wrap.c
SWIG_CRUFT=

rubylib=$(shell $(RUBY) -rrbconfig -e "puts Config::CONFIG['LIBRUBYARG']")

rubydir=$(shell $(RUBY) -rrbconfig -e "puts Config::CONFIG['archdir']")
ruby_DATA=Redland.so

EXTRA_DIST=test.rb example.rb \
$(SWIG_OUTPUTS)

CLEANFILES=*.db test-out.rdf core* $(RUBY_PACKAGE)_wrap.o Redland.so
MAINTAINERCLEANFILES=$(SWIG_OUTPUTS) $(SWIG_CRUFT)

$(RUBY_PACKAGE)_wrap.c: $(srcdir)/../Redland.i
	swig -v -ruby -module $(RUBY_PACKAGE) -o $@ $<

$(RUBY_PACKAGE)_wrap.o: $(RUBY_PACKAGE)_wrap.c
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS) $(STANDARD_CFLAGS) -fPIC -DPIC -I$(top_srcdir)/librdf -I$(rubydir) $(RUBY_PACKAGE)_wrap.c -c -o $@

$(RUBY_PACKAGE).so: $(RUBY_PACKAGE)_wrap.o $(top_builddir)/librdf/librdf.la
	$(CC) $(AM_CFLAGS) $(CFLAGS) $(AM_LDFLAGS) $(LDFLAGS) -shared $(RUBY_PACKAGE)_wrap.o $(rubylib) `$(top_srcdir)/redland-src-config --libs` -o $@ 


# Convince automake to install this as a program
rubyDATA_INSTALL = $(INSTALL_PROGRAM)

check-local: test-ruby

test-ruby: $(RUBY_PACKAGE).so
	$(RUBY) -I. example.rb file:../perl/dc.rdf raptor

clean-local:
	rm -f $(RUBY_PACKAGE).so
