// -*- Mode: java; c-basic-offset: 2 -*-
//
// example.c - Redland example code for Java
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

import org.librdf.redland.*;

class example {
  
  public static void main(String[] args) {

    org.librdf.redland.World world=new org.librdf.redland.World();
    world.open();

    System.out.println("Redland version: " + world.versionString());
    System.out.println("Redland Copyright: " + world.copyrightString());

  }
}
