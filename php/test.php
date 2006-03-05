<?php
/* -*- Mode: php; c-basic-offset: 2 -*-
 *
 * test.php - Redland PHP Interface test program
 *
 * $Id$
 *
 * Copyright (C) 2003 Morten Frederiksen - http://purl.org/net/morten/
 *
 * Copyright (C) 2002-2004 David Beckett - http://purl.org/net/dajobe/
 * Copyright (C) 2002-2004 University of Bristol - http://www.bristol.ac.uk/
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
 *
 */

/* ------------------------------------------------------------------------ */

print "Testing Redland...\n";

global $REDLAND_LOADED__;
if ($REDLAND_LOADED__) return;
if (!extension_loaded("redland")) {

  /* PHP 4.3 provides PHP_SHLIB_PREFIX and PHP_SHLIB_SUFFIX */
  if (!defined('PHP_SHLIB_SUFFIX')) {
    define('PHP_SHLIB_SUFFIX', strtoupper(substr(PHP_OS, 0,3)) == 'WIN' ? 'dll' : 'so');
  }
  if (!defined('PHP_SHLIB_PREFIX')) {
    define('PHP_SHLIB_PREFIX',PHP_SHLIB_SUFFIX == 'dll' ? 'php_' : '');
  }

  if (!dl(PHP_SHLIB_PREFIX . "redland" . "." . PHP_SHLIB_SUFFIX )){
    die('no redland?');
    exit;
  }
}
$REDLAND_LOADED__ = true;

$world=librdf_php_get_world();

print "Redland world opened\n";

$storage=librdf_new_storage($world,'hashes','dummy',"new=yes,hash-type='memory'");
print "Redland storage created\n";

$model=librdf_new_model($world,$storage,'');
print "Redland model created\n";

$parser=librdf_new_parser($world,'raptor','application/rdf+xml',librdf_new_uri($world,'-'));
print "Redland parser created\n";

$uri=librdf_new_uri($world,'file:../data/dc.rdf');

print "Parsing...\n";
librdf_parser_parse_into_model($parser,$uri,$uri,$model);
print "Done...\n";

librdf_free_uri($uri);

librdf_free_parser($parser);


$nulluri = librdf_new_uri ($world, '-');

$query = librdf_new_query($world, 'sparql', $nulluri, "PREFIX dc: <http://purl.org/dc/elements/1.1/> SELECT ?a ?c ?d WHERE { ?a dc:title ?c . OPTIONAL { ?a dc:related ?d } }", $nulluri);
print "Querying for dc:titles:\n";
$results=librdf_model_query_execute($model, $query);
$count=1;
while($results && !librdf_query_results_finished($results)) {
  print "result $count: {\n";
  for ($i=0; $i < librdf_query_results_get_bindings_count($results); $i++)
  {
    $val=librdf_query_results_get_binding_value($results, $i);
    if ($val)
      $nval=librdf_node_to_string($val);
    else
      $nval='(unbound)';
    print "  ".librdf_query_results_get_binding_name($results, $i)."=".$nval."\n";
  }
  print "}\n";
  librdf_query_results_next($results);
  $count++;
}
if ($results)
  print "Returned $count results\n";
$results=null;

print "\nExecuting query again\n";
$results=librdf_model_query_execute($model, $query);
if ($results) {
  $format_uri=librdf_new_uri($world, "http://www.w3.org/TR/2004/WD-rdf-sparql-XMLres-20041221/");
  $str=librdf_query_results_to_string($results, $format_uri, $nulluri);
  print "Query results serialized to an XML string size ".strlen($str)." bytes\n";
} else
  print "Query results couldn't be serialized to an XML string\n";


$serializer=librdf_new_serializer($world,'rdfxml','application/rdf+xml',librdf_new_uri($world,'-'));
print "Redland serializer created\n";

$base=librdf_new_uri($world,'http://example.org/base.rdf');

print "Serializing...\n";
librdf_serializer_serialize_model_to_file($serializer,'./test-out.rdf',$base,$model);
print "Done...\n";

librdf_free_serializer($serializer);

librdf_free_uri($base);

librdf_free_model($model);

librdf_free_storage($storage);


print "Done\n";

?>
