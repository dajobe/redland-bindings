# -*- Mode: Makefile -*-
#
# Makefile.am - automake file for PHP interface to Redland
#
# $Id$
#
# Copyright (C) 2002-2003 David Beckett - http://purl.org/net/dajobe/
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

RELEASE=@VERSION_RELEASE@

INCLUDES=-I$(top_srcdir) @PHP_INCLUDES@

PHP=php

PHP_PACKAGE=redland

SWIG_OUTPUTS=$(PHP_PACKAGE)_wrap.c php_$(PHP_PACKAGE).h
SWIG_CRUFT=$(PHP_PACKAGE).php

phpdir= $(shell php-config --extension-dir)
php_DATA=$(PHP_PACKAGE).so

EXTRA_DIST=test.php example.php php.ini \
$(SWIG_OUTPUTS)

CLEANFILES=dmalloc.log *.db test-out.rdf \
$(PHP_PACKAGE)_wrap.so $(PHP_PACKAGE)_wrap.o $(PHP_PACKAGE)-stamp \
$(PHP_PACKAGE).so $(PHP_PACKAGE).bundle $(PHP_PACKAGE)_wrap.c.orig 
MAINTAINERCLEANFILES=$(SWIG_OUTPUTS) $(SWIG_CRUFT)

$(PHP_PACKAGE)_wrap.c: $(srcdir)/../Redland.i
	swig -v -php4 -noproxy -module $(PHP_PACKAGE) -o $@ $<
	cp $(PHP_PACKAGE)_wrap.c $(PHP_PACKAGE)_wrap.c.tmp
	patch $(PHP_PACKAGE)_wrap.c < redland_wrap.c.patch
	mv $(PHP_PACKAGE)_wrap.c.tmp $(PHP_PACKAGE)_wrap.c.orig
	rm -f $(PHP_PACKAGE).php

if MAINTAINER_MODE
$(PHP_PACKAGE)_wrap.c.patch: $(PHP_PACKAGE)_wrap.c $(PHP_PACKAGE)_wrap.c.orig 
	-diff -u $(PHP_PACKAGE)_wrap.c.orig $(PHP_PACKAGE)_wrap.c > $@
endif

# -UHAVE_CONFIG_H to stop PHP trying to include config.h which
# quite often just doesn't exist
$(PHP_PACKAGE)_wrap.so: $(PHP_PACKAGE)_wrap.c
	$(CC) $(DEFS) -UHAVE_CONFIG_H $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS) $(STANDARD_CFLAGS) -fPIC -DPIC -I$(top_srcdir)/librdf `php-config --includes` $(PHP_PACKAGE)_wrap.c -c -o $@

$(PHP_PACKAGE).so: $(PHP_PACKAGE)-stamp
$(PHP_PACKAGE)-stamp: $(PHP_PACKAGE)_wrap.so
	@if test `uname` = Darwin; then \
	  args=" -bundle -flat_namespace -undefined suppress"; \
	else \
	  args=" -shared"; \
	fi; \
	cmd="$(CC) $(AM_CFLAGS) $(CFLAGS) `$(top_srcdir)/redland-src-config --libs` $(AM_LDFLAGS) $(LDFLAGS) $$args $(PHP_PACKAGE)_wrap.so -o $(PHP_PACKAGE).so"; \
	echo $$cmd; \
	$$cmd
	if test `uname` = Darwin; then \
	  $(INSTALL_PROGRAM) $(PHP_PACKAGE).so $(PHP_PACKAGE).bundle; \
	fi
	touch $(PHP_PACKAGE)-stamp

check-local: test-php

test-php: $(PHP_PACKAGE).so $(srcdir)/test.php
# I do not know why this is needed. PHP seems to ignore the global
# libraries when not installed
	-$(top_builddir)/redland-src-config --run \
	  $(PHP) -c $(srcdir)/php.ini $(srcdir)/test.php
