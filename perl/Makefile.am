# -*- Mode: Makefile -*-
#
# Makefile.am - automake file for Perl 5 interface to Redland
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

PERL_PACKAGE=Redland

PERL_MAKEFILE=Makefile.perl

SWIG_OUTPUTS=$(PERL_PACKAGE).pm $(PERL_PACKAGE)_wrap.c $(PERL_PACKAGE)_wrap.doc
PERL_CRUFT=blib pm_to_blib $(PERL_PACKAGE).bs $(PERL_MAKEFILE) $(PERL_MAKEFILE).old

EXTRA_DIST=MANIFEST MANIFEST.SKIP Makefile.PL Redland.i test.pl \
$(SWIG_OUTPUTS)

SUBDIRS=lib

CLEANFILES=test*db $(PERL_PACKAGE)_wrap.o $(PERL_MAKEFILE) \
$(PERL_MAKEFILE).old
MAINTAINERCLEANFILES=$(SWIG_OUTPUTS) $(SWIG_CRUFT)

$(PERL_PACKAGE)_wrap.c: Redland.i
	swig -v -perl5 -package $(PERL_PACKAGE) $?

$(PERL_MAKEFILE): Makefile.PL
	PACKAGE=$(PERL_PACKAGE) VERSION=$(VERSION) TOP_SRCDIR=$(top_srcdir) perl $? PREFIX=$(prefix)

-perl test-perl install-perl clean-perl realclean-perl: $(PERL_MAKEFILE)
	@target=`echo $@ | sed -e 's/-perl//'`; \
	echo $(MAKE) -f $(PERL_MAKEFILE) $$target PREFIX=$(prefix); \
	$(MAKE) -f $(PERL_MAKEFILE) $$target PREFIX=$(prefix)

build-perl: -perl
install-perl: build-perl
check-perl: build-perl test-perl

# Perl make clean leaves the old makefile around
clean-local: clean-perl
	rm -f $(PERL_MAKEFILE).old
