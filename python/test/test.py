#
# test.py - Redland Python 2.0 test code
#
# $Id$
#
# Copyright (C) 2000-2001 David Beckett - http://purl.org/net/dajobe/
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
#

import RDF

world=RDF.world()

storage=RDF.storage({'storage_name' : "hashes", 'name' : "test", "options_string" : "new='yes',hash-type='bdb',dir='.'"})
if not storage:
  raise "new RDF.storage failed"

#world.debug(1)

model=RDF.model(storage)
if not model:
  raise "new RDF.model failed"

statement=RDF.statement({'subject' : RDF.node({"uri_string" : "http://purl.org/net/dajobe/"}),
                        'predicate' : RDF.node({"uri_string" : "http://purl.org/dc/elements/1.1/creator"}),
                        'object' : RDF.node({"literal" : "Dave Beckett"})})
if not statement:
  raise "new RDF.statement failed"

model.add_statement(statement)

# Match against an empty statement - find everything
statement=RDF.statement({"subject" : None, "predicate" : None, "object": None});
# after this statement should not be touched since find_statements is using it
stream=model.find_statements(statement);

while not stream.end():
  statement2=stream.next();
  print "found statement:",statement2
del statement2

del statement

del stream

del model
del storage

# this must be done last
world.close()

