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
#
# There is a lot of fakery going on here in order that libtool deals
# with building the shared object and dynamically loaded library.
# libtool is fooled into building a dynamically loaded library called
# libfake.la which is installed into the working directory.  [Note:
# Unlike ruby that needs -module]
#
# This libfake.so is then suitable for loading dynamically and
# is copied to redland.so to match the PHP naming scheme.  (Note
# I do not know what Darwin/OSX requires so there is no special
# actions here like for Tcl and Ruby)
#
# To stop recompiles when either of the above are built, a
# stamp file $(PHP_PACKAGE)-stamp is used to control this.
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
$(PHP_PACKAGE)_wrap.lo $(PHP_PACKAGE)_wrap.o $(PHP_PACKAGE)-stamp \
redland.so \
libfake*
MAINTAINERCLEANFILES=$(SWIG_OUTPUTS) $(SWIG_CRUFT)

$(PHP_PACKAGE)_wrap.c: $(srcdir)/../Redland.i
	swig -v -php4 -noproxy -module $(PHP_PACKAGE) -o $@ $<
	rm -f $(PHP_PACKAGE).php

$(PHP_PACKAGE)_wrap.lo: $(PHP_PACKAGE)_wrap.c
	$(LIBTOOL) --mode=compile $(CC) $(CFLAGS) -I$(top_srcdir)/librdf `php-config --includes` -prefer-pic -c $(INCLUDES) $< -o $@

#	$(LIBTOOL) --mode=link $(CC) $(PHP_PACKAGE)_wrap.lo $(top_builddir)/librdf/librdf.la -o $@
$(PHP_PACKAGE).so: $(PHP_PACKAGE)-stamp
$(PHP_PACKAGE)-stamp: $(PHP_PACKAGE)_wrap.lo $(top_builddir)/librdf/librdf.la
	destdir=`pwd`; \
	$(LIBTOOL) --mode=link $(CC) $(AM_CFLAGS) $(CFLAGS) $(AM_LDFLAGS) $(LDFLAGS) -o libfake.la -module -rpath $$destdir $(PHP_PACKAGE)_wrap.lo $(top_builddir)/librdf/librdf.la; \
	$(LIBTOOL) --mode=install install libfake.la $(DESTDIR)$$destdir/libfake.la
	$(INSTALL_PROGRAM) libfake.so $(PHP_PACKAGE).so
	rm -rf .libs libfake*
	touch $(PHP_PACKAGE)-stamp

check-local: test-php

test-php: $(PHP_PACKAGE).so
# I do not know why this is needed. PHP seems to ignore the global
# libraries when not installed
	l=.libs; here=`pwd`; \
	rm -rf $$l && mkdir $$l; \
	ln -s $(phpdir)/* $$here/$(PHP_PACKAGE).so $$l
	$(PHP) test.php

clean-local realclean-local:
	rm -f $(PHP_PACKAGE).so
