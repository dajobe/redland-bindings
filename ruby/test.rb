#!/usr/bin/env ruby
#
# example.rb - Redland example Ruby program
#
# $Id$
#
# Copyright (C) 2002 David Beckett - http://purl.org/net/dajobe/
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
