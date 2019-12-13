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

soversion = 2.0.15
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

GPRBUILD = @GPRBUILD@ --relocate-build-tree -p -s -we
GPRCLEAN = @GPRCLEAN@
GPRINSTALL = @GPRINSTALL@ --prefix=$(prefix) --relocate-build-tree

my-dynamic-lib:
	$(GPRBUILD) librdf.gpr \
                 -XLIBRARY_KIND=dynamic -XOBJ_DIR=$(builddir)/obj-dynamic -Xsoversion=$(soname) -XMODE=Install -XDEBUG_MODE=$(DEBUG_MODE)

# $(LIB_DIR)/librdf-ada.a: my-static-lib

my-static-lib:
	$(GPRBUILD) librdf.gpr \
                 -XLIBRARY_KIND=static -XOBJ_DIR=$(builddir)/obj-static -Xsoversion=$(soname) -XMODE=Install -XDEBUG_MODE=$(DEBUG_MODE)

check-syntax:
	$(GPRBUILD) librdf.gpr -c \
                 -XLIBRARY_KIND=static -XOBJ_DIR=$(builddir)/obj-static -Xsoversion=$(soname) -XMODE=Install -XDEBUG_MODE=check

all-local: my-dynamic-lib my-static-lib

check-local: $(builddir)/obj-static/test/run_all_tests
	$(builddir)/obj-static/test/run_all_tests $(srcdir)

$(builddir)/obj-static/test/run_all_tests: my-static-lib
	$(GPRBUILD) test.gpr \
                 -XLIBRARY_KIND=static -XOBJ_DIR=$(builddir)/obj-static -XMODE=Install -XDEBUG_MODE=debug

clean-local:
	$(GPRCLEAN) -r librdf.gpr -XLIBRARY_KIND=dynamic -XOBJ_DIR=$(builddir)/obj-dynamic -Xsoversion=$(soname) -XMODE=Install
	$(GPRCLEAN) -r librdf.gpr -XLIBRARY_KIND=static -XOBJ_DIR=$(builddir)/obj-static -Xsoversion=$(soname) -XMODE=Install
	$(GPRCLEAN) -r test.gpr -XLIBRARY_KIND=static -XOBJ_DIR=$(builddir)/obj-static/test -Xsoversion=$(soname) -XMODE=Install
# 	rm -f $(builddir)/lib/$(soname)

install-data-local:
	$(GPRINSTALL) -f -p librdf.gpr -XLIBRARY_KIND=dynamic -XOBJ_DIR=$(builddir)/obj-dynamic -Xsoversion=$(soname) -XMODE=Install --build-name=dynamic
	$(GPRINSTALL) -f -p librdf.gpr -XLIBRARY_KIND=static -XOBJ_DIR=$(builddir)/obj-static -Xsoversion=$(soname) -XMODE=Install --build-name=static

uninstall-local:
	$(GPRINSTALL) --uninstall -f -p librdf.gpr -XLIBRARY_KIND=dynamic -XOBJ_DIR=$(builddir)/obj-dynamic -Xsoversion=$(soname) -XMODE=Install
	$(GPRINSTALL) --uninstall -f -p librdf.gpr -XLIBRARY_KIND=static -XOBJ_DIR=$(builddir)/obj-static -Xsoversion=$(soname) -XMODE=Install

.PHONY: my-dynamic-lib my-static-lib check-syntax
