# -*- Mode: Makefile -*-
#
# Makefile.am - automake file for Ruby tests for Redland
#
# Copyright (C) 2004-2005 David Beckett - http://www.dajobe.org/
# Copyright (C) 2004-2005 University of Bristol - http://www.bristol.ac.uk/
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


RUBY_TEST_FILES=test_model.rb test_node.rb test_parser.rb test_rdf.rb \
test_rdfs_resource.rb test_resource.rb test_serializer.rb \
test_statement.rb test_store.rb test_uri.rb test_query.rb

EXTRA_DIST=$(RUBY_TEST_FILES) README ical.rdf triples.rdf one.nt


CLEANFILES=*.db out*.rdf

check-local: check-ruby

check-ruby: ../ruby-redland-stamp $(RUBY_TEST_FILES)
	@result=0; fails=; \
	$(ECHO) "Testing Ruby"; \
	for test in $(RUBY_TEST_FILES); do \
	  $(ECHO) $(ECHO_N) "Checking $$test $(ECHO_C)"; \
	  arg="-I.. -I../lib"; \
	  $(RUBY) $$arg $(srcdir)/$$test; \
	  status=$$?; \
	  if test $$status != 0 -a $$status != 2 ; then \
	    $(ECHO) FAILED returned status $$status; result=1; \
	    echo $(RUBY) $$arg $(srcdir)/$$test; \
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
