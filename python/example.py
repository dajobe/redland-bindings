#!/usr/bin/python
#
# example.py - Redland Python 2.0 example code
#
# $Id$
#
# Copyright (C) 2000-2003 David Beckett - http://purl.org/net/dajobe/
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
if storage is None:
  raise "new RDF.Storage failed"

#RDF.debug(1)

model=RDF.Model(storage)
if model is None:
  raise "new RDF.model failed"

statement=RDF.Statement(RDF.Uri("http://purl.org/net/dajobe/"),
                        RDF.Uri("http://purl.org/dc/elements/1.1/creator"),
                        RDF.Node("Dave Beckett"))
if statement is None:
  raise "new RDF.Statement failed"

model.add_statement(statement)

# Match against an empty statement - find everything
for s in model.find_statements(RDF.Statement()):
  print "found statement:",s

test_file='../data/dc.rdf'

print "Parsing URI (file)", test_file
uri=RDF.Uri(string="file:"+test_file)

parser=RDF.Parser('raptor')
if parser is None:
  raise "Failed to create RDF.Parser raptor"

count=0
for s in parser.parse_as_stream(uri,uri):
  model.add_statement(s)
  count=count+1

print "Parsing added",count,"statements"

print "Printing all statements"
for s in model.as_stream():
  print "Statement:",s

q = RDF.Query("SELECT ?a ?c WHERE (?a dc:title ?c) USING dc FOR <http://purl.org/dc/elements/1.1/>")
print "Querying for dc:titles:"
for result in q.run_as_bindings(model):
  print "{"
  for k in result:
    print "  "+k+" = "+str(result[k])
  print "}"

print "Writing model to test-out.rdf as rdf/xml"

# Use any rdf/xml parser that is available
serializer=RDF.Serializer()
serializer.serialize_model_to_file("test-out.rdf", model)

print "Done"
