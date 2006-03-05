# -*- Mode: Makefile -*-
#
# Makefile.am - automake file for Ruby interface to Redland
#
# $Id$
#
# Copyright (C) 2000-2005 David Beckett - http://purl.org/net/dajobe/
# Copyright (C) 2000-2005 University of Bristol - http://www.bristol.ac.uk/
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

RELEASE=@VERSION_RELEASE@

AM_CPPFLAGS=@CPPFLAGS@ @LIBRDF_CPPFLAGS@  @RUBY_INCLUDES@
AM_CFLAGS=@CFLAGS@ @LIBRDF_CPPFLAGS@ $(MEM)

RUBY=@RUBY@

RUBY_PACKAGE=redland

SWIG_OUTPUTS=$(RUBY_PACKAGE)_wrap.c
SWIG_CRUFT=

RDOC_DIR=rdoc
RDOC_STAMP=rdoc-stamp

rubylib=@RUBY_LIB@

rubydir=/usr/lib/$(rubylib)
rubyarchdir=@RUBY_DIR@

rubyarch_DATA=redland.so

EXTRA_DIST=example.rb test.rb \
redland-pre.i redland-types.i $(SWIG_OUTPUTS)

SUBDIRS=lib test

CLEANFILES=*.db test-out.rdf core* \
$(RUBY_PACKAGE)_wrap.so $(RUBY_PACKAGE)_wrap.o ruby-redland-stamp \
$(RUBY_PACKAGE).so $(RUBY_PACKAGE).bundle
MAINTAINERCLEANFILES=$(SWIG_OUTPUTS) $(SWIG_CRUFT) $(RDOC_DIR) $(RDOC_STAMP)

RUN=@REDLAND_RUN@

SWIG_OPTS=-I$(srcdir) -DREDLAND_PRE_I -DREDLAND_TYPES_I

$(srcdir)/$(RUBY_PACKAGE)_wrap.c: @REDLAND_SWIG@ redland-pre.i redland-types.i
	swig -v -ruby $(SWIG_OPTS) -module $(RUBY_PACKAGE) -o $@ @REDLAND_SWIG@

$(RUBY_PACKAGE)_wrap.so: $(srcdir)/$(RUBY_PACKAGE)_wrap.c
	$(CC) $(DEFS)  $(SWIG_OPTS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS) $(STANDARD_CFLAGS) -fPIC -DPIC -I$(rubyarchdir) $(RUBY_PACKAGE)_wrap.c -c -o $@


$(RUBY_PACKAGE).so: ruby-redland-stamp
ruby-redland-stamp: $(RUBY_PACKAGE)_wrap.so
	@if test `uname` = Darwin; then \
	  args=" -bundle -flat_namespace -undefined suppress"; \
	else \
	  args=" -shared"; \
	fi; \
	echo $(CC) $(AM_CFLAGS) $(CFLAGS) $(AM_LDFLAGS) $(LDFLAGS) $$args $(RUBY_PACKAGE)_wrap.so -l$(rubylib) `@REDLAND_CONFIG@ --libs` -o $(RUBY_PACKAGE).so; \
	$(CC) $(AM_CFLAGS) $(CFLAGS) $(AM_LDFLAGS) $(LDFLAGS) $$args $(RUBY_PACKAGE)_wrap.so -l$(rubylib) `@REDLAND_CONFIG@ --libs` -o $(RUBY_PACKAGE).so
	if test `uname` = Darwin; then \
	  $(INSTALL_PROGRAM) $(RUBY_PACKAGE).so $(RUBY_PACKAGE).bundle; \
	fi
	touch ruby-redland-stamp

install-rubyarchDATA: $(rubyarch_DATA)
	$(mkinstalldirs) $(DESTDIR)$(rubyarchdir)
	if test -r $(RUBY_PACKAGE).bundle; then \
	  $(INSTALL_PROGRAM) $(RUBY_PACKAGE).bundle $(DESTDIR)$(rubyarchdir)/$(RUBY_PACKAGE).bundle; \
	else \
	  $(INSTALL_PROGRAM) $(RUBY_PACKAGE).so $(DESTDIR)$(rubyarchdir)/$(RUBY_PACKAGE).so; \
	fi

uninstall-rubyarchDATA: $(rubyarch_DATA)
	if test -r $(RUBY_PACKAGE).bundle; then \
	  rm -f $(DESTDIR)$(rubyarchdir)/$(RUBY_PACKAGE).bundle; \
	else \
	  rm -f $(DESTDIR)$(rubyarchdir)/$(RUBY_PACKAGE).so; \
	fi

check-local: test-ruby

test-ruby: test-simple test-example

test-simple: $(RUBY_PACKAGE).so $(srcdir)/example.rb
	$(RUN)$(RUBY) -I. -Ilib \
	  $(srcdir)/test.rb file:$(srcdir)/../data/dc.rdf raptor

test-example: $(RUBY_PACKAGE).so $(srcdir)/example.rb
	$(RUN)$(RUBY) -I. -Ilib \
	  $(srcdir)/example.rb file:$(srcdir)/../data/dc.rdf raptor

docs: $(RDOC_STAMP)
$(RDOC_STAMP):
	mkdir -p $(RDOC_DIR); \
	outdir=`cd $(RDOC_DIR); pwd`; \
	rm -rf $$outdir; \
	here=`pwd`; \
	cd $(srcdir)/lib; \
	rdoc --quiet --fmt html --charset 'utf-8' --inline-source --image-format png --op $$outdir --title "Redland Ruby API RDoc" --exclude convert_owl.rb --exclude constants.rb --exclude convertfoaf.rb && touch $$here/$@

clean-local:
	rm -rf $(RDOC_DIR) $(RDOC_STAMP)
