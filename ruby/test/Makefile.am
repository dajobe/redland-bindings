# -*- Mode: Makefile -*-
#
# Makefile.am - automake file for Ruby tests for Redland
#
# $Id$
#
# Copyright (C) 2004 David Beckett - http://purl.org/net/dajobe/
# Institute for Learning and Research Technology - http://www.ilrt.bris.ac.uk/
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


RUBY_TEST_FILES=test_model.rb test_node.rb test_parser.rb test_rdf.rb \
test_rdfs_resource.rb test_resource.rb test_serializer.rb \
test_statement.rb test_store.rb test_uri.rb

EXTRA_DIST=$(RUBY_TEST_FILES) ical.rdf triples.rdf


CLEANFILES=*.db out_namespace.rdf

check-local: check-ruby

check-ruby: ../ruby-redland-stamp $(RUBY_TEST_FILES)
	@result=0; fails=; \
	$(ECHO) "Testing Ruby"; \
	for test in $(RUBY_TEST_FILES); do \
	  $(ECHO) $(ECHO_N) "Checking $$test $(ECHO_C)"; \
	  $(top_builddir)/redland-src-config --run $(RUBY) -I .. -I ../lib $(srcdir)/$$test >/dev/null 2>&1; \
	  status=$$?; \
	  if test $$status != 0 -a $$status != 2 ; then \
	    $(ECHO) FAILED returned status $$status; result=1; \
	    echo $(top_builddir)/redland-src-config --run $(RUBY) -I .. -I ../lib $(srcdir)/$$test; \
	    fails="$$fails $$test"; \
	  else \
	    $(ECHO) "ok"; \
	  fi; \
	done; \
	if test $$result = 1 ; then \
	  echo "FAILED tests:$$fails"; \
	fi; \
	exit $$result

../ruby-redland-stamp:
	cd .. && $(MAKE) ruby-redland-stamp
