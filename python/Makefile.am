# -*- Mode: Makefile -*-
#
# Makefile.am - automake file for Python interface to Redland
#
# $Id$
#
# Copyright (C) 2000 David Beckett - http://purl.org/net/dajobe/
# Institute for Learning and Research Technology, University of Bristol.
#
#    This package is Free Software available under either of two licenses
#    (see FAQS.html to see why):
# 
# 1. The GNU Lesser General Public License (LGPL)
# 
#    See http://www.gnu.org/copyleft/lesser.html or COPYING.LIB for the
#    full license text.
#      _________________________________________________________________
# 
#      Copyright (C) 2000 David Beckett, Institute for Learning and
#      Research Technology, University of Bristol. All Rights Reserved.
# 
#      This library is free software; you can redistribute it and/or
#      modify it under the terms of the GNU Lesser General Public License
#      as published by the Free Software Foundation; either version 2 of
#      the License, or (at your option) any later version.
# 
#      This library is distributed in the hope that it will be useful, but
#      WITHOUT ANY WARRANTY; without even the implied warranty of
#      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
#      Lesser General Public License for more details.
# 
#      You should have received a copy of the GNU Lesser General Public
#      License along with this library; if not, write to the Free Software
#      Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
#      USA
#      _________________________________________________________________
# 
#    NOTE - under Term 3 of the LGPL, you may choose to license the entire
#    library under the GPL. See COPYING for the full license text.
# 
# 2. The Mozilla Public License
# 
#    See http://www.mozilla.org/MPL/MPL-1.1.html or MPL.html for the full
#    license text.
# 
#    Under MPL section 13. I declare that all of the Covered Code is
#    Multiple Licensed:
#      _________________________________________________________________
# 
#      The contents of this file are subject to the Mozilla Public License
#      version 1.1 (the "License"); you may not use this file except in
#      compliance with the License. You may obtain a copy of the License
#      at http://www.mozilla.org/MPL/
# 
#      Software distributed under the License is distributed on an "AS IS"
#      basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
#      the License for the specific language governing rights and
#      limitations under the License.
# 
#      The Initial Developer of the Original Code is David Beckett.
#      Portions created by David Beckett are Copyright (C) 2000 David
#      Beckett, Institute for Learning and Research Technology, University
#      of Bristol. All Rights Reserved.
# 
#      Alternatively, the contents of this file may be used under the
#      terms of the GNU Lesser General Public License, in which case the
#      provisions of the LGPL License are applicable instead of those
#      above. If you wish to allow use of your version of this file only
#      under the terms of the LGPL License and not to allow others to use
#      your version of this file under the MPL, indicate your decision by
#      deleting the provisions above and replace them with the notice and
#      other provisions required by the LGPL License. If you do not delete
#      the provisions above, a recipient may use your version of this file
#      under either the MPL or the LGPL License.

RELEASE=@VERSION_RELEASE@

INCLUDES=-I$(top_srcdir)

PYTHON=@PYTHON@
PYTHON_INCLUDES=@PYTHON_INCLUDES@

PYTHON_PACKAGE=Redland

SWIG_OUTPUTS=$(PYTHON_PACKAGE)_wrap.c $(PYTHON_PACKAGE)_wrap.doc
SWIG_CRUFT=$(PYTHON_PACKAGE).bs blib pm_to_blib


EXTRA_DIST=MANIFEST.in README Redland.i setup.cfg setup.py RDF.py \
$(SWIG_OUTPUTS)

SUBDIRS=test

CLEANFILES=test*db $(PYTHON_PACKAGE)_wrap.o
MAINTAINERCLEANFILES=$(SWIG_OUTPUTS) $(SWIG_CRUFT)

PYTHON_SETUP=PACKAGE=$(PYTHON_PACKAGE) VERSION=$(VERSION) TOP_SRCDIR=$(top_srcdir) $(PYTHON) setup.py

$(PYTHON_PACKAGE)_wrap.c: Redland.i
	swig -v -python -module $(PYTHON_PACKAGE) $?

build-python:
	$(PYTHON_SETUP) build

check-python: test-python
test-python: build-python
	@bdir=`ls -1d build/lib.*`; \
	echo Using python build dir $$bdir; \
	echo PYTHONPATH=$$bdir $(PYTHON) test/test.py; \
	PYTHONPATH=$$bdir $(PYTHON) test/test.py

clean-python:
	$(PYTHON_SETUP) clean

realclean-python:
	$(PYTHON_SETUP) clean --all

install-python: setup.py build-python
	$(PYTHON_SETUP) install

dist-python: sdist-python

# Built distribution (may be binary) for module
bdist-python: setup.py
	$(PYTHON_SETUP) bdist

# Source distributionn for module
sdist-python: setup.py
	$(PYTHON_SETUP) sdist

# RPM distribution for module
rpmdist-python: setup.py
	$(PYTHON_SETUP) bdist_rpm --release=$(RELEASE)

Redland_wrap.o: Redland_wrap.c
	$(COMPILE) -c $(PYTHON_INCLUDES) $<

clean-local: clean-python
	rm -rf build dist
