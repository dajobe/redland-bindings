#
# example.py - Redland Python 2.0 example code
#
# $Id$
#
# Copyright (C) 2000-2002 David Beckett - http://purl.org/net/dajobe/
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

storage=RDF.Storage(storage_name="hashes",
                    name="test",
                    options_string="new='yes',hash-type='memory',dir='.'")
if not storage:
  raise "new RDF.Storage failed"

#RDF.debug(1)

model=RDF.Model(storage)
if not model:
  raise "new RDF.model failed"

statement=RDF.Statement(subject=RDF.Node(uri_string="http://purl.org/net/dajobe/"),
                        predicate=RDF.Node(uri_string="http://purl.org/dc/elements/1.1/creator"),
                        object=RDF.Node(literal="Dave Beckett"))
if not statement:
  raise "new RDF.Statement failed"

# after this statement is owned by model and should not be used
model.add_statement(statement)

# Match against an empty statement - find everything
statement=RDF.Statement(subject=None, predicate=None, object=None);
# after this statement should not be touched since find_statements is using it
stream=model.find_statements(statement);

while not stream.end():
  print "found statement:",stream.current()
  stream.next();

test_file='../perl/dc.rdf'

print "Parsing URI (file)", test_file
uri=RDF.Uri(string="file:"+test_file)

parser=RDF.Parser('raptor')
if not parser:
  raise "Failed to create RDF.Parser raptor"

stream=parser.parse_as_stream(uri,uri)
count=0
while not stream.end() :
  model.add_statement(stream.current())
  count=count+1
  stream.next();

print "Parsing added",count,"statements"

print "Printing all statements"
stream=model.serialise()
while not stream.end():
  print "Statement:",stream.current()
  stream.next()
