# -*- Mode: Makefile -*-
#
# Makefile.am - automake file for LUA interface to Redland
#
# Copyright (C) 2010 David Beckett - http://www.dajobe.org/
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

AM_CPPFLAGS=@CPPFLAGS@ @LIBRDF_CPPFLAGS@ -DREDLAND_BINDINGS_VERSION=\"@VERSION@\"
AM_CFLAGS=@STANDARD_CFLAGS@ @LIBRDF_CPPFLAGS@

AM_LDFLAGS=@LIBRDF_LDFLAGS@

LUA=lua

LUA_PACKAGE=redland

LUA_CFLAGS=@LUA_CFLAGS@
LUA_LDFLAGS=@LUA_LDFLAGS@
LUA_SO_SUFFIX=so

SWIG_OUTPUTS=$(LUA_PACKAGE)_wrap.c
SWIG_CRUFT=

luadir=@LUA_EXTENSION_DIR@
lua_DATA=$(LUA_PACKAGE).$(LUA_SO_SUFFIX)

EXTRA_DIST= test.lua \
$(SWIG_OUTPUTS)

CLEANFILES=*.db test-out.rdf \
$(LUA_PACKAGE)_wrap.o $(LUA_PACKAGE)-stamp \
$(LUA_PACKAGE).$(LUA_SO_SUFFIX) $(LUA_PACKAGE).bundle
MAINTAINERCLEANFILES=$(SWIG_OUTPUTS) $(SWIG_CRUFT) $(LUA_PACKAGE)_wrap.c.orig

RUN=@REDLAND_RUN@

SWIG_OPTS=-I$(srcdir)

$(LUA_PACKAGE)_wrap.c: @REDLAND_SWIG@
	swig -lua -v $(SWIG_OPTS) -module $(LUA_PACKAGE) -o $@ @REDLAND_SWIG@

$(LUA_PACKAGE)_wrap.o: $(LUA_PACKAGE)_wrap.c
	$(CC) $(DEFS) $(LUA_CFLAGS) $(SWIG_OPTS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS) $(STANDARD_CFLAGS) -fPIC -DPIC $(LUA_PACKAGE)_wrap.c -c -o $@

$(LUA_PACKAGE).$(LUA_SO_SUFFIX): $(LUA_PACKAGE)-stamp
$(LUA_PACKAGE)-stamp: $(LUA_PACKAGE)_wrap.o
	$(CC) $(STANDARD_CFLAGS) $(LUA_PACKAGE)_wrap.o $(AM_LDFLAGS) -fPIC -shared $(LDFLAGS) $(LUA_LDFLAGS) -o $(LUA_PACKAGE).$(LUA_SO_SUFFIX)
	touch $(LUA_PACKAGE)-stamp

test-lua:
	$(LUA) test.lua

check-local: test-lua
