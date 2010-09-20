--[[
 * test.lua - Redland Lua Interface test program
 *
 * Joe Presbrey - http://presbrey.mit.edu/
 * 2010-09-20
 *
 * This package is Free Software or Open Source available under the
 * following licenses (these are alternatives):
 *   1. GNU Lesser General Public License (LGPL)
 *   2. GNU General Public License (GPL)
 *   3. Mozilla Public License (MPL)
 *
 * See LICENSE.html or LICENSE.txt at the top of this package for the
 * full license terms.
 *
]]--

print("Testing Redland...");
require("redland");

world = redland.librdf_new_world();
print("Redland world opened");

storage = redland.librdf_new_storage(world,'hashes','dummy',"new=yes,hash-type='memory'");
print("Redland storage created");

model = redland.librdf_new_model(world,storage,'');
print("Redland model created");

parser = redland.librdf_new_parser(world,'rdfxml','application/rdf+xml',null);
print("Redland parser created");

uri = redland.librdf_new_uri(world,'file:../data/dc.rdf');

print("Parsing...");
redland.librdf_parser_parse_into_model(parser,uri,uri,model);
print("Done...");

redland.librdf_free_uri(uri);

redland.librdf_free_parser(parser);

query = redland.librdf_new_query(world, 'sparql', null, "PREFIX dc: <http://purl.org/dc/elements/1.1/> SELECT ?a ?c ?d WHERE { ?a dc:title ?c . OPTIONAL { ?a dc:related ?d } }", null);
print("Querying for dc:titles:");
results = redland.librdf_model_query_execute(model, query);
count=0;
while not(results == nil) and redland.librdf_query_results_finished(results) == 0 do
    print("result "..count..": {");
    for i = 0, redland.librdf_query_results_get_bindings_count(results) - 1, 1 do
        val = redland.librdf_query_results_get_binding_value(results, i);
        if val then
            nval = redland.librdf_node_to_string(val);
        else
            nval = "(unbound)";
        end
        print("  "..redland.librdf_query_results_get_binding_name(results, i).."="..(nval or "(nil)"));
    end
    print("}");
    redland.librdf_query_results_next(results);
    count = count + 1;
end
if not(results == nil) then
    print("Returned "..count.." results");
end

results = nil;

print "\nExecuting query again";
results = redland.librdf_model_query_execute(model, query);
if not(results == nil) then
  str = redland.librdf_query_results_to_string(results, nil, nil);
  print("Query results serialized to an XML string size "..string.len(str).." bytes");
else
  print("Query results couldn't be serialized to an XML string");
end

serializer = redland.librdf_new_serializer(world, "rdfxml", "", nil);
print("Redland serializer created");

base = redland.librdf_new_uri(world, "http://example.org/base.rdf");

print("Serializing...");
redland.librdf_serializer_serialize_model_to_file(serializer,'./test-out.rdf',base,model);
print("Done...");

redland.librdf_free_serializer(serializer);

redland.librdf_free_uri(base);

redland.librdf_free_model(model);

redland.librdf_free_storage(storage);

print("Done");