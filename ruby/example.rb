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
# USAGE: ruby example.rb file:../perl/dc.rdf raptor
# 
#

require 'Redland'

uri_string=ARGV[0]
parser_name=ARGV[1]


world=Redland::librdf_new_world
Redland::librdf_world_open world

storage=Redland::librdf_new_storage world, "hashes", "test", "new='yes',hash-type='bdb',dir='.'"
raise "Failed to create RDF storage" if !storage


model=Redland::librdf_new_model world, storage, ""
if !model then
  Redland::librdf_free_storage storage
  raise "Failed to create RDF model"
end

parser=Redland::librdf_new_parser world, parser_name, "", nil
if !parser then
  Redland::librdf_free_model model
  Redland::librdf_free_storage storage
  raise "Failed to create RDF parser"
end

uri=Redland::librdf_new_uri world, uri_string

stream=Redland::librdf_parser_parse_as_stream parser, uri, uri

count=0
while Redland::librdf_stream_end(stream) == 0
  statement=Redland::librdf_stream_get_object stream
  Redland::librdf_model_add_statement model, statement
  puts "found statement: #{Redland::librdf_statement_to_string statement}"
  count=count+1
  Redland::librdf_stream_next stream
end

Redland::librdf_free_stream stream

puts "Parsing added #{count} statements"

Redland::librdf_free_parser parser


puts "Printing all statements"
stream=Redland::librdf_model_serialise model
while Redland::librdf_stream_end(stream) == 0
  statement=Redland::librdf_stream_get_object stream
  puts "Statement: #{Redland::librdf_statement_to_string statement}"
  Redland::librdf_stream_next stream
end

Redland::librdf_free_stream stream


Redland::librdf_free_model model
Redland::librdf_free_storage storage

Redland::librdf_free_world world

