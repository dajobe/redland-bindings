# -*- Mode: Makefile -*-
#
# Makefile.am - automake file for PHP interface to Redland
#
# $Id$
#
# Copyright (C) 2002 David Beckett - http://purl.org/net/dajobe/
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

INCLUDES=-I$(top_srcdir) @PHP_INCLUDES@

PHP=php

PHP_PACKAGE=redland

SWIG_OUTPUTS=$(PHP_PACKAGE)_wrap.c php_$(PHP_PACKAGE).h
SWIG_CRUFT=$(PHP_PACKAGE).php

phpdir= $(shell php-config --extension-dir)
php_DATA=$(PHP_PACKAGE).so

EXTRA_DIST=test.php example.php php.ini \
$(SWIG_OUTPUTS)

CLEANFILES=test*db core* $(PHP_PACKAGE)_wrap.o 
MAINTAINERCLEANFILES=$(SWIG_OUTPUTS) $(SWIG_CRUFT)

$(PHP_PACKAGE)_wrap.c: $(srcdir)/../Redland.i
	swig -v -php -module $(PHP_PACKAGE) -o $@ $<
	rm -f $(PHP_PACKAGE).php

$(PHP_PACKAGE)_wrap.lo: $(PHP_PACKAGE)_wrap.c
	$(LIBTOOL) --mode=compile $(CC) $(CFLAGS) -I$(top_srcdir)/librdf `php-config --includes` -prefer-pic -c $(INCLUDES)  $<

$(PHP_PACKAGE).so: $(PHP_PACKAGE)_wrap.lo $(top_builddir)/librdf/librdf.la
	$(LIBTOOL) --mode=link $(CC) $(PHP_PACKAGE)_wrap.lo `php-config --ldflags` `php-config --libs` $(top_builddir)/librdf/librdf.la -o $@

check-local: test-php

test-php: $(PHP_PACKAGE).so
	$(PHP) -c . test.php

clean-local realclean-local:
	rm -f $(PHP_PACKAGE).so
