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

RUBY=ruby

RUBY_PACKAGE=Redland

SWIG_OUTPUTS=$(RUBY_PACKAGE)_wrap.c
SWIG_CRUFT=


EXTRA_DIST=test.rb example.rb \
$(SWIG_OUTPUTS)

CLEANFILES=test*db $(RUBY_PACKAGE)_wrap.o 
MAINTAINERCLEANFILES=$(SWIG_OUTPUTS) $(SWIG_CRUFT)

$(RUBY_PACKAGE)_wrap.c: $(srcdir)/../Redland.i
	swig -v -ruby -module $(RUBY_PACKAGE) -o $@ $?

$(RUBY_PACKAGE)_wrap.o: $(RUBY_PACKAGE)_wrap.c
	$(CC) $(CFLAGS) -c -fpic $(INCLUDES)  $? -o $@

$(RUBY_PACKAGE).so: $(RUBY_PACKAGE)_wrap.o
	$(CC) -shared $? -o $@ `$(top_srcdir)/redland-config --libs` -L$(top_srcdir) -lrdf

build-ruby: $(RUBY_PACKAGE).so 

check-ruby: test-ruby
test-ruby: build-ruby
	$(RUBY) example.rb

clean-ruby realclean-ruby:
	rm -f $(RUBY_PACKAGE).so

# todo
install-ruby: $(RUBY_PACKAGE).so build-ruby
	tcldir=`tcl -c 'puts [lindex $$auto_path 0]'`; \
	mkdir $$tcldir/redland; \
	cp -p $(RUBY_PACKAGE).so pkgIndex.tcl $$tcldir/redland

dist-ruby:
	@echo make ruby dist somehow

clean-local: clean-ruby



