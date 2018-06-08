# -*- Mode: Makefile -*-
#
# Makefile.am - top level automake file for Redland Bindings
#
# Copyright (C) 2000-2007, David Beckett http://www.dajobe.org/
# Copyright (C) 2000-2005  University of Bristol, UK http://www.bristol.ac.uk/
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
# complete terms and further detail along with the license texts for
# the licenses in COPYING.LIB, COPYING and LICENSE-2.0.txt respectively.
# 
# 

# Subdirectories to build/install etc.
SUBDIRS=@SD@ data
# Subdirectories to use for distribution
DIST_SUBDIRS=data docs \
lua perl php python ruby ada

EXTRA_DIST=\
ChangeLog.1 ChangeLog.2 ChangeLog.3 ChangeLog.4 ChangeLog.5 \
ChangeLog.6 ChangeLog.7 ChangeLog.8 ChangeLog.9 ChangeLog.10 \
ChangeLog.11 ChangeLog.12 ChangeLog.13 \
README                LICENSE.txt  TODO \
README.html NEWS.html LICENSE.html TODO.html \
RELEASE.html INSTALL.html \
LICENSE-2.0.txt NOTICE \
bindings.rdf.in \
autogen.sh \
Makefile.PL \
redland-bindings.spec redland-bindings.spec.in


if MAINTAINER_MODE
# Create some text files from HTML sources
LYNX=lynx

HTML_TO_TEXT=TERM=vt100 $(LYNX) -dump $< | sed -e 's,file://localhost.*bindings/,,' > $@

SUFFIXES = .html .txt

.html.txt:
	$(HTML_TO_TEXT)

README: README.html
	$(HTML_TO_TEXT)

NEWS: NEWS.html
	$(HTML_TO_TEXT)

TODO: TODO.html
	$(HTML_TO_TEXT)
endif

@SET_MAKE@
