# -*- Mode: Makefile -*-
#
# Makefile.am - automake file for Ruby interface to Redland
#
# $Id$
#
# Copyright (C) 2000-2004 David Beckett - http://purl.org/net/dajobe/
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

RELEASE=@VERSION_RELEASE@

AM_CPPFLAGS=@CPPFLAGS@ -I$(top_srcdir)/librdf -I$(top_builddir)/librdf @LIBRDF_CPPFLAGS@ @LIBRDF_INTERNAL_CPPFLAGS@ @LIBRDF_EXTERNAL_CPPFLAGS@  @RUBY_INCLUDES@
AM_CFLAGS=@CFLAGS@ @LIBRDF_CPPFLAGS@ @LIBRDF_INTERNAL_CPPFLAGS@ @LIBRDF_EXTERNAL_CPPFLAGS@ $(MEM)

RUBY=@RUBY@

RUBY_PACKAGE=redland

SWIG_OUTPUTS=$(RUBY_PACKAGE)_wrap.c
SWIG_CRUFT=

rubylib=`$(RUBY) -rrbconfig -e "puts Config::CONFIG['ruby_install_name']"`

rubydir=`$(RUBY) -rrbconfig -e "puts Config::CONFIG['archdir']"`
ruby_DATA=redland.so

EXTRA_DIST=test.rb example.rb \
$(SWIG_OUTPUTS)

CLEANFILES=*.db test-out.rdf core* \
$(RUBY_PACKAGE)_wrap.so $(RUBY_PACKAGE)_wrap.o $(RUBY_PACKAGE)-stamp \
$(RUBY_PACKAGE).so $(RUBY_PACKAGE).bundle
MAINTAINERCLEANFILES=$(SWIG_OUTPUTS) $(SWIG_CRUFT)

$(srcdir)/$(RUBY_PACKAGE)_wrap.c: $(srcdir)/../Redland.i
	swig -v -ruby -module $(RUBY_PACKAGE) -o $@ $<


$(RUBY_PACKAGE)_wrap.so: $(srcdir)/$(RUBY_PACKAGE)_wrap.c
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS) $(STANDARD_CFLAGS) -fPIC -DPIC -I$(rubydir) $(RUBY_PACKAGE)_wrap.c -c -o $@


$(RUBY_PACKAGE).so: $(RUBY_PACKAGE)-stamp
$(RUBY_PACKAGE)-stamp: $(RUBY_PACKAGE)_wrap.so
	@if test `uname` = Darwin; then \
	  args=" -bundle -flat_namespace -undefined suppress"; \
	else \
	  args=" -shared"; \
	fi; \
	echo $(CC) $(AM_CFLAGS) $(CFLAGS) $(AM_LDFLAGS) $(LDFLAGS) $$args $(RUBY_PACKAGE)_wrap.so -l$(rubylib) `$(top_srcdir)/redland-src-config --libs` -o $(RUBY_PACKAGE).so; \
	$(CC) $(AM_CFLAGS) $(CFLAGS) $(AM_LDFLAGS) $(LDFLAGS) $$args $(RUBY_PACKAGE)_wrap.so -l$(rubylib) `$(top_srcdir)/redland-src-config --libs` -o $(RUBY_PACKAGE).so
	if test `uname` = Darwin; then \
	  $(INSTALL_PROGRAM) $(RUBY_PACKAGE).so $(RUBY_PACKAGE).bundle; \
	fi
	touch $(RUBY_PACKAGE)-stamp

install-rubyDATA: $(ruby_DATA)
	if test -r $(RUBY_PACKAGE).bundle; then \
	  $(INSTALL_PROGRAM) $(RUBY_PACKAGE).bundle $(DESTDIR)$(rubydir)/$(RUBY_PACKAGE).bundle; \
	else \
	  $(INSTALL_PROGRAM) $(RUBY_PACKAGE).so $(DESTDIR)$(rubydir)/$(RUBY_PACKAGE).so; \
	fi

uninstall-rubyDATA: $(ruby_DATA)
	if test -r $(RUBY_PACKAGE).bundle; then \
	  rm -f $(DESTDIR)$(rubydir)/$(RUBY_PACKAGE).bundle; \
	else \
	  rm -f $(DESTDIR)$(rubydir)/$(RUBY_PACKAGE).so; \
	fi

check-local: test-ruby

test-ruby: $(RUBY_PACKAGE).so $(srcdir)/example.rb
	$(top_builddir)/redland-src-config --run \
	  $(RUBY) -I. $(srcdir)/example.rb file:$(srcdir)/../perl/dc.rdf raptor

