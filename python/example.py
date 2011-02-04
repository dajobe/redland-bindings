#!/usr/bin/python
#
# example.py - Redland Python 2.0 example code
#
# Copyright (C) 2000-2005 David Beckett - http://www.dajobe.org/
# Copyright (C) 2000-2005 University of Bristol - http://www.bristol.ac.uk/
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
#

import RDF

storage=RDF.Storage(storage_name="hashes",
                    name="test",
                    options_string="new='yes',hash-type='memory',dir='.'")
if storage is None:
  raise Exception("new RDF.Storage failed")

#RDF.debug(1)

model=RDF.Model(storage)
if model is None:
  raise Exception("new RDF.model failed")

statement=RDF.Statement(RDF.Uri("http://www.dajobe.org/"),
                        RDF.Uri("http://purl.org/dc/elements/1.1/creator"),
                        RDF.Node("Dave Beckett"))
if statement is None:
  raise Exception("new RDF.Statement failed")

model.add_statement(statement)

# Match against an empty statement - find everything
for s in model.find_statements(RDF.Statement()):
  print "found statement:",s

test_file='../data/dc.rdf'

print "Parsing URI (file)", test_file
uri=RDF.Uri(string="file:"+test_file)

parser=RDF.Parser('raptor')
if parser is None:
  raise Exception("Failed to create RDF.Parser raptor")

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
for result in q.execute(model):
  print "{"
  for k in result:
    print "  "+k+" = "+str(result[k])
  print "}"

print "Writing model to test-out.rdf as rdf/xml"

# Use any rdf/xml parser that is available
serializer=RDF.Serializer()
serializer.set_namespace("dc", RDF.Uri("http://purl.org/dc/elements/1.1/"))
serializer.serialize_model_to_file("test-out.rdf", model)

print "Serialized to ntriples as a string size",len(model.to_string(name="ntriples", base_uri="http://example.org/base#")),"bytes"

print "Done"
