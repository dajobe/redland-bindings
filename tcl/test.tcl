lappend auto_path .

package require redland

librdf_init_world $"" NULL

set storage [librdf_new_storage "hashes" "test" {new='yes',hash-type='bdb',dir='.'}]
if {[expr {"$storage" == "NULL"}]} then {
  puts "Failed to create RDF storage"
  exit 1
}

set model [librdf_new_model $storage ""]
if {[expr {"$model" == "NULL"}]} then {
  librdf_free_storage $storage
  puts "Failed to create RDF model"
  exit 1
}


set statement [librdf_new_statement_from_nodes [librdf_new_node_from_uri_string "http://purl.org/net/dajobe/"] [librdf_new_node_from_uri_string "http://purl.org/dc/elements/1.1/creator"] [librdf_new_node_from_literal "Dave Beckett" "" 0 0]]
if {[expr {"$statement" == "NULL"}]} then {
  librdf_free_model $model
  librdf_free_storage $storage
  puts "failed to create RDF statement"
  exit 1
}

librdf_model_add_statement $model $statement


puts "program hasn't crashed yet"


librdf_free_model $model
librdf_free_storage $storage

librdf_destroy_world
