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

import org.librdf.redland.core;
import org.librdf.redland.World;
import org.librdf.redland.URI;
import org.librdf.redland.Node;
import org.librdf.redland.Statement;
import org.librdf.redland.Iterator;
import org.librdf.redland.Stream;
import org.librdf.redland.Storage;
import org.librdf.redland.Hash;
import org.librdf.redland.Parser;
import org.librdf.redland.Model;

class skeleton {
  
  public static void main(String[] args) {
    // THIS CODE SHOULD NEVER ACTUALLY BE RUN
    // It is just used to get all the classes compiled.
    System.exit(1);
    
    org.librdf.redland.World world=new org.librdf.redland.World();
    org.librdf.redland.URI uri=new org.librdf.redland.URI(world, "test");
    org.librdf.redland.Node node=new org.librdf.redland.Node(world, uri);
    org.librdf.redland.Statement statement=new org.librdf.redland.Statement(world, node, node, node);
    org.librdf.redland.Iterator iterator=new org.librdf.redland.Iterator(world, 0L, statement, statement, statement);
    org.librdf.redland.Stream stream=new org.librdf.redland.Stream(world, 0L, statement);
    org.librdf.redland.Storage storage=new org.librdf.redland.Storage(world, "memory", "name", "");
    org.librdf.redland.Model model=new org.librdf.redland.Model(world, storage, "");
    org.librdf.redland.Parser parser=new org.librdf.redland.Parser(world, "raptor", "", uri);
  }
  
}
