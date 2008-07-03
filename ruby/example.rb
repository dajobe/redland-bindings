#!/usr/bin/env ruby
#
# example.rb - Redland example Ruby program
#
# Copyright (C) 2002-2004 David Beckett - http://www.dajobe.org/
# Copyright (C) 2002-2004 University of Bristol - http://www.bristol.ac.uk/
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
# USAGE: ruby example.rb file:../data/dc.rdf raptor
# 
#

require 'rdf/redland'

uri_string=ARGV[0]
parser_name=ARGV[1]

storage=Redland::TripleStore.new("hashes", "test", "new='yes',hash-type='bdb',dir='.'")
raise "Failed to create RDF storage" if !storage


model=Redland::Model.new(storage)
if !model then
  raise "Failed to create RDF model"
end

parser=Redland::Parser.new(parser_name, "", nil)
if !parser then
  raise "Failed to create RDF parser"
end

uri=Redland::Uri.new(uri_string)
stream=parser.parse_as_stream(uri, uri)

count=0
while !stream.end?()
  statement=stream.current()
  model.add_statement(statement)
  puts "found statement: #{statement}"
  count=count+1
  stream.next()
end

puts "Parsing added #{count} statements"


puts "Printing all statements"
stream=model.as_stream()
while !stream.end?()
  statement=stream.current()
  puts "Statement: #{statement}"
  stream.next()
end

q = Redland::Query.new(" PREFIX dc: <http://purl.org/dc/elements/1.1/> SELECT ?a ?c WHERE { ?a dc:title ?c } ")
puts "Querying for dc:titles:"
results=q.execute(model)
while !results.finished?()
  puts "{"
  for k in 0..results.bindings_count()-1
    puts "  #{k}= #{results.binding_value(k)}"
  end
  puts "}"
  results.next()
end

results=q.execute(model)
size=results.to_string(Redland::Uri.new("http://www.w3.org/2001/sw/DataAccess/json-sparql/")).length()
puts "Serialized query results to JSON as a string size #{size} bytes"

puts "Writing model to test-out.rdf as rdf/xml"
# Use any rdf/xml parser that is available
serializer=Redland::Serializer.new()
serializer.set_namespace("dc", Redland::Uri.new("http://purl.org/dc/elements/1.1/"))
serializer.set_namespace("rdf", Redland::Uri.new("http://www.w3.org/1999/02/22-rdf-syntax-ns#"))
serializer.to_file("test-out.rdf", model)

size=model.to_string(name="ntriples", base_uri=Redland::Uri.new("http://example.org/base#")).length()
puts "Serialized to ntriples as a string size #{size} bytes"

puts "Done"
