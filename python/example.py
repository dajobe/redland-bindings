#
# example.py - Redland Python 2.0 example code
#
# $Id$
#
# Copyright (C) 2000-2001 David Beckett - http://purl.org/net/dajobe/
# Institute for Learning and Research Technology - http://www.ilrt.org/
# University of Bristol - http://www.bristol.ac.uk/
# 
# This package is Free Software or Open Source available under the
# following licenses (these are alternatives):
#   1. GNU Lesser General Public License (LGPL) Version 2
#   2. GNU General Public License (GPL) Version 2
#   3. Mozilla Public License (MPL) Version 1.1
# and no other versions of those licenses.
# 
# See INSTALL.html or INSTALL.txt at the top of this package for the
# full license terms.
# 
#

import RDF

world=RDF.world()

storage=RDF.storage(storage_name="hashes", name="test", options_string="new='yes',hash-type='memory',dir='.'")
if not storage:
  raise "new RDF.storage failed"

#world.debug(1)

model=RDF.model(storage)
if not model:
  raise "new RDF.model failed"

statement=RDF.statement(subject=RDF.node(uri_string="http://purl.org/net/dajobe/"),
                        predicate=RDF.node(uri_string="http://purl.org/dc/elements/1.1/creator"),
                        object=RDF.node(literal="Dave Beckett"))
if not statement:
  raise "new RDF.statement failed"

# after this statement is owned by model and should not be used
model.add_statement(statement)
del statement

# Match against an empty statement - find everything
statement=RDF.statement(subject=None, predicate=None, object=None);
# after this statement should not be touched since find_statements is using it
stream=model.find_statements(statement);

while not stream.end():
  statement2=stream.next();
  print "found statement:",statement2
del statement2

del statement

del stream

test_file='../perl/dc.rdf'

print "Parsing URI (file)", test_file
uri=RDF.uri(string="file:"+test_file)

parser=RDF.parser('raptor')
if not parser:
  raise "Failed to create RDF.parser raptor"
parser.feature(RDF.uri(string="http://www.w3.org/1999/02/22-rdf-syntax-ns#aboutEach"), "yes")

stream=parser.parse_as_stream(uri,uri)
count=0
while not stream.end() :
  model.add_statement(stream.next())
  count=count+1

# Not needed since assignment below does this
#stream=None
#del stream

parser=None
del parser

del uri

print "Parsing added",count,"statements"

print "Printing all statements"
stream=model.serialise()
while not stream.end():
  print "Statement:",stream.next()
stream=None

del stream

del model

del storage

# this must be done last
world.close()

