# -*- Mode: Makefile -*-
#
# Makefile.am - automake file for Ruby interface to Redland
#
# $Id$
#
# Copyright (C) 2000-2003 David Beckett - http://purl.org/net/dajobe/
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
# There is a lot of fakery going on here in order that libtool deals
# with building the shared object and dynamically loaded module.  libtool
# is fooled into building a module (-module -rpath PWD in the link
# line) called libfake.la which is installed into the working directory.
# [Note: unlike Tcl that uses dynamically loaded libraries, no -module needed]
#
# This libfake.so is then suitable for loading dynamically.  On
# Darwin/OSX this needs to be called Redland.bundle so it is copied
# to that name.  Everything else gets Redland.so.
#
# To stop recompiles when either of the above are built, a
# stamp file $(RUBY_PACKAGE)-stamp is used to control this.
#
# On install/unstall custom targets (install-rubyDATA, uninstall-rubyDATA)
# copy the appropriate file above.
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

CLEANFILES=*.db test-out.rdf core* \
$(RUBY_PACKAGE)_wrap.lo $(RUBY_PACKAGE)_wrap.o $(RUBY_PACKAGE)-stamp \
Redland.so Redland.bundle \
libfake* 
MAINTAINERCLEANFILES=$(SWIG_OUTPUTS) $(SWIG_CRUFT)

$(srcdir)/$(RUBY_PACKAGE)_wrap.c: $(srcdir)/../Redland.i
	swig -v -ruby -module $(RUBY_PACKAGE) -o $@ $<


# OLD action:
#	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS) $(STANDARD_CFLAGS) -fPIC -DPIC -I$(top_srcdir)/librdf -I$(rubydir) $(RUBY_PACKAGE)_wrap.c -c -o $@

$(RUBY_PACKAGE)_wrap.lo: $(srcdir)/$(RUBY_PACKAGE)_wrap.c $(top_builddir)/librdf/librdf.la
	$(LIBTOOL) --mode=compile $(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS) $(CFLAGS) -prefer-pic -I$(top_srcdir)/librdf -I$(top_builddir)/librdf -I$(rubydir) $(srcdir)/$(RUBY_PACKAGE)_wrap.c -c -o $@


# OLD action (for $(RUBY_PACKAGE).so )
#	$(CC) $(AM_CFLAGS) $(CFLAGS) $(AM_LDFLAGS) $(LDFLAGS) -shared $(RUBY_PACKAGE)_wrap.o $(rubylib) `$(top_srcdir)/redland-src-config --libs` -o $@ 

$(RUBY_PACKAGE).so: $(RUBY_PACKAGE)-stamp
$(RUBY_PACKAGE)-stamp: $(RUBY_PACKAGE)_wrap.lo
	destdir=`pwd`; \
	$(LIBTOOL) --mode=link $(CC) $(AM_CFLAGS) $(CFLAGS) $(AM_LDFLAGS) $(LDFLAGS) -o libfake.la -module -rpath $$destdir $(RUBY_PACKAGE)_wrap.lo $(top_builddir)/librdf/librdf.la $(rubylib); \
	$(LIBTOOL) --mode=install install libfake.la $(DESTDIR)$$destdir/libfake.la
	if test `uname` = Darwin; then \
	  $(INSTALL_PROGRAM) libfake.so $(RUBY_PACKAGE).bundle; \
	else \
	  $(INSTALL_PROGRAM) libfake.so $(RUBY_PACKAGE).so; \
	fi
	rm -f libfake*
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
	$(RUBY) -I. $(srcdir)/example.rb file:$(srcdir)/../perl/dc.rdf raptor

