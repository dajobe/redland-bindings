// -*- Mode: java; c-basic-offset: 2 -*-
//
// test.c - Redland Java interface test code
//
// $Id$
//
// Copyright (C) 2001 David Beckett - http://purl.org/net/dajobe/
// Institute for Learning and Research Technology - http://www.ilrt.org/
// University of Bristol - http://www.bristol.ac.uk/
// 
// This package is Free Software or Open Source available under the
// following licenses (these are alternatives):
//   1. GNU Lesser General Public License (LGPL)
//   2. GNU General Public License (GPL)
//   3. Mozilla Public License (MPL)
// 
// See LICENSE.html or LICENSE.txt at the top of this package for the
// full license terms.
// 
// 
//

import org.librdf.redland.core;

class test1 {
  
  public static void main(String[] args) {

    // LOW LEVEL TESTING - using the C objects directly, no Java objects

    System.out.println("Test starting");

    long world=core.librdf_new_world();
    core.librdf_world_open(world);

    long storage=core.librdf_new_storage(world, "hashes", "test",
                                         "hash-type='bdb',dir='.',new='yes'");
    if(storage == 0) {
      System.out.println("Failed to create RDF storage");
      System.exit(1);
    }

    long model=core.librdf_new_model(world, storage, null);
    if(model == 0) {
      System.out.println("Failed to create RDF model");
      System.exit(1);
    }
    
    long parser=core.librdf_new_parser(world, "raptor", "", 0);
    if(parser == 0) {
      System.out.println("Failed to create RDF parser");
      System.exit(1);
    }
    
    long uri=core.librdf_new_uri(world, "file:../perl/dc.rdf");
    if(core.librdf_parser_parse_into_model(parser, uri, uri, model) != 0) {
      System.out.println("Failed to parse ../perl/dc.rdf into model");
      System.exit(1);
    }
    core.librdf_free_uri(uri);

    long node=core.librdf_new_node_from_uri_string(world, "http://example.org/");

    core.librdf_free_node(node);

    core.librdf_free_parser(parser);

    core.librdf_free_model(model);

    core.librdf_free_storage(storage);

    core.librdf_free_world(world);
    
    System.out.println("Test finished");
  }
}
