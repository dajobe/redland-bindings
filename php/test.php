<?php
/* -*- Mode: php; c-basic-offset: 2 -*-
 *
 * test.php - Redland PHP Interface test program
 *
 * Copyright (C) 2003 Morten Frederiksen - http://purl.org/net/morten/
 *
 * Copyright (C) 2002-2004 David Beckett - http://purl.org/net/dajobe/
 * Institute for Learning and Research Technology - http://www.ilrt.org/
 * University of Bristol - http://www.bristol.ac.uk/
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

$parser=librdf_new_parser($world,'raptor','application/rdf+xml',librdf_new_uri($world,''));
print "Redland parser created\n";

$uri=librdf_new_uri($world,'file:../data/dc.rdf');

print "Parsing...\n";
librdf_parser_parse_into_model($parser,$uri,$uri,$model);
print "Done...\n";

librdf_free_uri($uri);

librdf_free_parser($parser);

$serializer=librdf_new_serializer($world,'rdfxml','application/rdf+xml',librdf_new_uri($world,''));
print "Redland serializer created\n";

$base=librdf_new_uri($world,'http://exampe.org/base.rdf');

print "Serializing...\n";
librdf_serializer_serialize_model_to_file($serializer,'./test-out.rdf',$base,$model);
print "Done...\n";

librdf_free_serializer($serializer);

librdf_free_uri($base);

librdf_free_model($model);

librdf_free_storage($storage);


print "Done\n";

?>
