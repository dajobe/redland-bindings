// -*- Mode: java; c-basic-offset: 2 -*-
//
// skeleton.java - class to invoke compilation of all Redland Java classes
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

import org.librdf.redland;
import org.librdf.world;
import org.librdf.uri;
import org.librdf.node;
import org.librdf.statement;
import org.librdf.iterator;
import org.librdf.stream;
import org.librdf.storage;
import org.librdf.hash;
import org.librdf.parser;
import org.librdf.model;

class skeleton {
  
  public static void main(String[] args) {
    // THIS CODE SHOULD NEVER ACTUALLY BE RUN
    // It is just used to get all the classes compiled.
    System.exit(1);
    
    org.librdf.world world=new org.librdf.world();
    org.librdf.uri uri=new org.librdf.uri(world, "test");
    org.librdf.node node=new org.librdf.node(world, uri);
    org.librdf.statement statement=new org.librdf.statement(world, node, node, node);
    org.librdf.iterator iterator=new org.librdf.iterator(world, 0L, statement, statement, statement);
    org.librdf.stream stream=new org.librdf.stream(world, 0L, statement);
    org.librdf.storage storage=new org.librdf.storage(world, "memory", "name", "");
    org.librdf.model model=new org.librdf.model(world, storage, "");
    org.librdf.parser parser=new org.librdf.parser(world, "raptor", "", uri);
  }
  
}
