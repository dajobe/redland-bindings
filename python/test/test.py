#
# test.py - Redland Python 2.0 test code
#
# $Id$
#
# Copyright (C) 2000-2002 David Beckett - http://purl.org/net/dajobe/
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

storage=RDF.Storage(storage_name="memory",
                    name="test",
                    options_string="")
if not storage:
  raise "new RDF.storage failed"

model=RDF.Model(storage)
if not model:
  raise "new RDF.model failed"

statement=RDF.Statement(subject=RDF.Node(uri_string="http://purl.org/net/dajobe/"),
                        predicate=RDF.Node(uri_string="http://purl.org/dc/elements/1.1/creator"),
                        object=RDF.Node(literal="Dave Beckett"))
if not statement:
  raise "new RDF.statement failed"

model.add_statement(statement)

print "printing all model statements"
# Match against an empty statement - find everything
statement=RDF.Statement(subject=None, predicate=None, object=None);
# after this statement should not be touched since find_statements is using it
stream=model.find_statements(statement);

while not stream.end():
  statement2=stream.next();
  print "  found statement:",statement2


# Use any rdf/xml parser that is available
parser=RDF.Parser("raptor", "application/rdf+xml")
if parser is None:
  raise "Could not find any rdf/xml parser"

uri=RDF.Uri(string="file:../perl/dc.rdf")
print "made uri", uri

stream=parser.parse_as_stream(uri, uri)

while not stream.end():
  statement2=stream.next();
  print "found parsed statement:",statement2
  model.add_statement(statement2)

# add it again just to get some more statements
parser.parse_into_model(model, uri, uri)



print "printing model"
stream=model.serialise()

while not stream.end():
  statement2=stream.next();
  print "found statement:",statement2

print "searching model by statement"
search_statement=RDF.Statement(subject=None, predicate=RDF.Node(uri_string="http://purl.org/dc/elements/1.1/title"), object=None)


stream=model.find_statements(statement=search_statement)
while not stream.end():
  statement2=stream.next();
  print "  found statement:",statement2

print "searching model for node targets"
n1=RDF.Node(uri_string="http://purl.org/net/dajobe/")
n2=RDF.Node(uri_string="http://purl.org/dc/elements/1.1/title")
for node in model.targets(n1,n2):
  print "  found node:",node
