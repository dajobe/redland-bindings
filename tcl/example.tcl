#
# Example code for Redland Tcl interface
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


lappend auto_path .

package require Redland


set uri_string [lindex $argv 0]

set parser [lindex $argv 1]



set world [librdf_new_world]
librdf_world_open $world

set storage [librdf_new_storage $world "hashes" "test" {new='yes',hash-type='bdb',dir='.'}]
if {"$storage" == "NULL"} then {
  error "Failed to create RDF storage"
}

set model [librdf_new_model $world $storage ""]
if {"$model" == "NULL"} then {
  librdf_free_storage $storage
  error "Failed to create RDF model"
}

set parser [librdf_new_parser $world $parser "" NULL]
if {"$parser" == "NULL"} then {
  librdf_free_model $model
  librdf_free_storage $storage
  error "Failed to create RDF parser"
}


set uri [librdf_new_uri $world $uri_string]

set stream [librdf_parser_parse_as_stream $parser $uri $uri]

set count 0
while {! [librdf_stream_end $stream]} {
  set statement [librdf_stream_get_object $stream]
  librdf_model_add_statement $model $statement
  puts [concat "found statement:" [librdf_statement_to_string $statement]]
  incr count
  librdf_stream_next $stream
}
librdf_free_stream $stream
librdf_free_uri $uri

puts "Parsing added $count statements"

librdf_free_parser $parser


puts "Printing all statements"
set stream [librdf_model_serialise $model]
while {! [librdf_stream_end $stream]} {
  set statement [librdf_stream_get_object $stream]
  puts [concat "Statement:" [librdf_statement_to_string $statement]]
  librdf_stream_next $stream
}
librdf_free_stream $stream


librdf_free_model $model
librdf_free_storage $storage

librdf_free_world $world
