# -*- Mode: Makefile -*-
#
# Makefile.am - automake file for Ada2012 interface to Redland
#
# Copyright (C) 2014 Victor Porton - http://freesoft.portonvictor.org/
#
# This package is Free Software and part of Redland http://librdf.org/
#
# It is licensed under the following three licenses as alternatives:
#   1. GNU Lesser General Public License (LGPL) V2.1 or any newer version
#   2. GNU General Public License (GPL) V2 or any newer version
#   3. Apache License, V2.0 or any newer version
#
# You may not use this file except in compliance with at least one of
# the above three licenses.
#
# See LICENSE.html or LICENSE.txt at the top of this package for the
# full license terms.
#
#

soversion = 2.0.14
soname = librdf-ada.so.$(soversion)

ADAINCLUDE = $(includedir)/librdf
ADALIB = $(libdir)/librdf

LIB_DIR = $(builddir)/lib

EXTRA_DIST = librdf.gpr test.gpr src/*.ad[bs] src/test/*.ad[bs]

# $(LIB_DIR)/librdf-ada.so: my-dynamic-lib

if DEBUG
DEBUG_MODE=debug
else
DEBUG_MODE=release
endif

my-dynamic-lib:
	gnatmake -p -Plibrdf.gpr \
                 -XLIBRARY_KIND=dynamic -XOBJ_DIR=$(builddir)/obj-dynamic -Xsoversion=$(soname) -XMODE=Install -XDEBUG_MODE=$(DEBUG_MODE)

# $(LIB_DIR)/librdf-ada.a: my-static-lib

my-static-lib:
	gnatmake -p -Plibrdf.gpr \
                 -XLIBRARY_KIND=static -XOBJ_DIR=$(builddir)/obj-static -Xsoversion=$(soname) -XMODE=Install -XDEBUG_MODE=$(DEBUG_MODE)

check-syntax:
	gnatmake -p -Plibrdf.gpr \
                 -XLIBRARY_KIND=static -XOBJ_DIR=$(builddir)/obj-static -Xsoversion=$(soname) -XMODE=Install -XDEBUG_MODE=check

all-local: my-dynamic-lib my-static-lib

check-local: $(builddir)/obj-static/test/run_all_tests
	$(builddir)/obj-static/test/run_all_tests

$(builddir)/obj-static/test/run_all_tests: my-static-lib
	gnatmake -p -Ptest.gpr \
                 -XOBJ_DIR=$(builddir)/obj-static -XMODE=Install -XDEBUG_MODE=debug

clean-local:
	gnat clean -r -Plibrdf.gpr -XLIBRARY_KIND=dynamic -XOBJ_DIR=$(builddir)/obj-dynamic -Xsoversion=$(soname) -XMODE=Install
	gnat clean -r -Plibrdf.gpr -XLIBRARY_KIND=static -XOBJ_DIR=$(builddir)/obj-static -Xsoversion=$(soname) -XMODE=Install
	gnat clean -r -Ptest.gpr -XLIBRARY_KIND=static -XOBJ_DIR=$(builddir)/obj-static/test -Xsoversion=$(soname) -XMODE=Install
	rm -f $(builddir)/lib/$(soname)

install-data-local:
	mkdir -p $(ADAINCLUDE)
	mkdir -p $(ADALIB)
	mkdir -p $(libdir)
	cd $(srcdir)/src && $(INSTALL_DATA) *.ads *.adb $(ADAINCLUDE)
	cd $(builddir)/ali-static && $(INSTALL_DATA) *.ali $(ADALIB)/static
	cd $(builddir)/ali-dynamic && $(INSTALL_DATA) *.ali $(ADALIB)/dynamic
	$(INSTALL_DATA) lib/librdf-ada.so lib/librdf-ada.a $(soname) $(libdir)


uninstall-local:
	rm -rf $(ADAINCLUDE)
	rm -rf $(ADALIB)
	cd $(builddir)/lib && rm -f librdf-ada.so librdf-ada.a $(soname)

.PHONY: all-local clean-local install-local uninstall-local
.PHONY: my-dynamic-lib my-static-lib check-syntax
