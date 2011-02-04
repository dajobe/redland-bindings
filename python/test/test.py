#
# test.py - Redland Python 2.0 test code
#
# Copyright (C) 2000-2004 David Beckett - http://www.dajobe.org/
# Copyright (C) 2000-2004 University of Bristol - http://www.bristol.ac.uk/
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
#

import RDF

storage=RDF.Storage(storage_name="memory",
                    name="test",
                    options_string="")
#                    options_string="new='yes',hash-type='bdb',dir='.'")
if storage is None:
  raise "new RDF.storage failed"

model=RDF.Model(storage)
if model is None:
  raise "new RDF.model failed"

statement=RDF.Statement(RDF.Uri("http://www.dajobe.org/"),
                        RDF.Uri("http://purl.org/dc/elements/1.1/creator"),
                        RDF.Node("Dave Beckett"))
if statement is None:
  raise "new RDF.statement failed"

model.add_statement(statement)

print "printing all model statements"
# Match against an empty statement - find everything
statement=RDF.Statement(subject=None, predicate=None, object=None);

for s in model.find_statements(statement):
  print "  found statement:",s

# Use any rdf/xml parser that is available
parser=RDF.Parser(name="rdfxml",mime_type="application/rdf+xml")
if parser is None:
  raise "Could not find any rdf/xml parser"

uri=RDF.Uri(string="file:../data/dc.rdf")
print "made uri", uri

for s in parser.parse_as_stream(uri, uri):
  print "found parsed statement:",s
  model.add_statement(s)


rdfxml_string="""<?xml version='1.0'?>
<rdf:RDF xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#'
     xmlns:dc='http://purl.org/dc/elements/1.1/'>
  <rdf:Description rdf:about='http://www.dajobe.org/'
               dc:title='Home Page of David Beckett' />
</rdf:RDF>"""

for s in parser.parse_string_as_stream(rdfxml_string, uri):
  print "found parsed statement from string:",s
  model.add_statement(s)

print parser.namespaces_seen()

# add it again just to get some more statements
print "adding statements again with model.load"
model.load(uri)



print "printing model"
for s in model.as_stream():
  print "found statement:",s

print "searching model by statement"

for s in model.find_statements(RDF.Statement(None, RDF.Uri("http://purl.org/dc/elements/1.1/title"), None)):
  print "  found statement:",s


print "searching model for node targets"
n1=RDF.Uri("http://www.dajobe.org/")
n2=RDF.Uri("http://purl.org/dc/elements/1.1/title")
for node in model.targets(n1,n2):
  print "  found node:",node

print "matching statements"
if not RDF.Statement(None,None,None).matches(RDF.Statement(n1,n2,"Title")):
  print "Failed"

print "Testing for None"
print RDF.Statement(None,None,None).subject

print "Adding datatyped literal statement to model"
model.add_typed_literal_statement(subject=RDF.Node(uri_string="http://example.org/subject"),
                                  predicate=RDF.Node(uri_string="http://example.org/predicate"),
                                  string="Literal content",
                                  xml_language="en-GB",
                                  datatype=RDF.Uri(string="http://example.org/datatype"))

print "writing model as RDF/XML to test-out.rdf"
ser=RDF.RDFXMLSerializer()
ser.serialize_model_to_file("test-out.rdf", model)

print "done"
