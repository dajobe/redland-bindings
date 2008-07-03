# -*- Mode: Makefile -*-
#
# Makefile.am - automake file for PHP interface to Redland
#
# Copyright (C) 2002-2006 David Beckett - http://www.dajobe.org/
# Copyright (C) 2002-2005 University of Bristol - http://www.bristol.ac.uk/
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

AM_CPPFLAGS=@CPPFLAGS@ @LIBRDF_CPPFLAGS@ @PHP_INCLUDES@ -DREDLAND_BINDINGS_VERSION=\"@VERSION@\"
AM_CFLAGS=@STANDARD_CFLAGS@ @LIBRDF_CPPFLAGS@ $(MEM)

PHP=php

PHP_PACKAGE=redland

PHP_CFLAGS=@PHP_CFLAGS@
PHP_LDFLAGS=@PHP_LDFLAGS@
PHP_SO_SUFFIX=@PHP_SO_SUFFIX@
PHP_SWIG=@PHP_SWIG@

SWIG_OUTPUTS=$(PHP_PACKAGE)_wrap.c php_$(PHP_PACKAGE).h
SWIG_CRUFT=$(PHP_PACKAGE).php

phpdir=@PHP_EXTENSION_DIR@
php_DATA=$(PHP_PACKAGE).$(PHP_SO_SUFFIX)

EXTRA_DIST=test.php example.php php.ini \
redland-decl.i redland-init.i redland-post.i redland-typemap.i \
$(SWIG_OUTPUTS)

CLEANFILES=dmalloc.log *.db test-out.rdf \
$(PHP_PACKAGE)_wrap.o $(PHP_PACKAGE)-stamp \
$(PHP_PACKAGE).$(PHP_SO_SUFFIX) $(PHP_PACKAGE).bundle
MAINTAINERCLEANFILES=$(SWIG_OUTPUTS) $(SWIG_CRUFT) $(PHP_PACKAGE)_wrap.c.orig

RUN=@REDLAND_RUN@

SWIG_OPTS=-I$(srcdir) -DREDLAND_POST_I -DREDLAND_INIT_I -DREDLAND_DECL_I -DREDLAND_TYPEMAP_I

$(PHP_PACKAGE)_wrap.c: @REDLAND_SWIG@ redland-decl.i redland-init.i redland-post.i redland-typemap.i
	swig -v $(PHP_SWIG) $(SWIG_OPTS) -noproxy -module $(PHP_PACKAGE) -o $@ @REDLAND_SWIG@
	rm -f $(PHP_PACKAGE).php

# -UHAVE_CONFIG_H to stop PHP trying to include config.h which
# quite often just doesn't exist
$(PHP_PACKAGE)_wrap.o: $(PHP_PACKAGE)_wrap.c
	$(CC) $(DEFS) $(PHP_CFLAGS) $(SWIG_OPTS) -UHAVE_CONFIG_H $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS) $(STANDARD_CFLAGS) -fPIC -DPIC $(PHP_PACKAGE)_wrap.c -c -o $@

$(PHP_PACKAGE).$(PHP_SO_SUFFIX): $(PHP_PACKAGE)-stamp
$(PHP_PACKAGE)-stamp: $(PHP_PACKAGE)_wrap.o
	$(CC) $(STANDARD_CFLAGS) `@REDLAND_CONFIG@ --libs` $(AM_LDFLAGS) $(LDFLAGS) $(PHP_LDFLAGS) $(PHP_PACKAGE)_wrap.o -o $(PHP_PACKAGE).$(PHP_SO_SUFFIX)
	touch $(PHP_PACKAGE)-stamp

check-local: test-php

# Make PHP run using the build directory (.) as the PHP extension dir
# to find redland.so (see php.ini for the configuration there)
# -c $(srcdir) is the only portable way to make it read php.ini
# from here rather than (also) reading some global php.ini.
# The alternative to use php -n is only available in PHP 4.3.0 or newer.
#
# See the manual for the php command:
#  http://www.php.net/manual/en/features.commandline.php
test-php: $(PHP_PACKAGE).$(PHP_SO_SUFFIX) $(srcdir)/test.php
	$(RUN)$(PHP) -c $(srcdir) $(srcdir)/test.php
