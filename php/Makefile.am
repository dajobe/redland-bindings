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

CLEANFILES=*.db core* \
$(PHP_PACKAGE)_wrap.so $(PHP_PACKAGE)_wrap.o $(PHP_PACKAGE)-stamp \
$(PHP_PACKAGE).so $(PHP_PACKAGE).bundle
MAINTAINERCLEANFILES=$(SWIG_OUTPUTS) $(SWIG_CRUFT)

$(PHP_PACKAGE)_wrap.c: $(srcdir)/../Redland.i
	swig -v -php4 -noproxy -module $(PHP_PACKAGE) -o $@ $<
	rm -f $(PHP_PACKAGE).php

$(PHP_PACKAGE)_wrap.so: $(PHP_PACKAGE)_wrap.c
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS) $(STANDARD_CFLAGS) -fPIC -DPIC -I$(top_srcdir)/librdf `php-config --includes` $(PHP_PACKAGE)_wrap.c -c -o $@

$(PHP_PACKAGE).so: $(PHP_PACKAGE)-stamp
$(PHP_PACKAGE)-stamp: $(PHP_PACKAGE)_wrap.so
	@if test `uname` = Darwin; then \
	  args=" -bundle -flat_namespace -undefined suppress"; \
	else \
	  args=" -shared"; \
	fi; \
	echo $(CC) $(AM_CFLAGS) $(CFLAGS) $(AM_LDFLAGS) $(LDFLAGS) $$args $(PHP_PACKAGE)_wrap.so `$(top_srcdir)/redland-src-config --libs` -o $(PHP_PACKAGE).so; \
	$(CC) $(AM_CFLAGS) $(CFLAGS) $(AM_LDFLAGS) $(LDFLAGS) $$args $(PHP_PACKAGE)_wrap.so `$(top_srcdir)/redland-src-config --libs` -o $(PHP_PACKAGE).so
	if test `uname` = Darwin; then \
	  $(INSTALL_PROGRAM) $(PHP_PACKAGE).so $(PHP_PACKAGE).bundle; \
	fi
	touch $(PHP_PACKAGE)-stamp

check-local: test-php

test-php: $(PHP_PACKAGE).so $(srcdir)/test.php
# I do not know why this is needed. PHP seems to ignore the global
# libraries when not installed
	-$(PHP) $(srcdir)/test.php
