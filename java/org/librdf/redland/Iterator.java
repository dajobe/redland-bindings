// -*- Mode: java; c-basic-offset: 2 -*-
//
// iterator.java - Redland Java Iterator class
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

package org.librdf.redland;

import org.librdf.redland.core;
import org.librdf.redland.World;
import org.librdf.redland.Node;



public class Iterator implements java.util.Iterator
{
  private long object;
  private World world;
  private Object creator1;
  private Object creator2;
  private Object creator3;
  
  public Iterator (World world, long object,
                   Object creator1, Object creator2, Object creator3)  
    {
      this.world=world;
      this.object=object;
      this.creator1=creator1;
      this.creator2=creator2;
      this.creator3=creator3;
    }

  protected void finalize()
    {
      core.librdf_free_iterator(this.object);
      this.object=0;
      this.creator1=null;
      this.creator2=null;
      this.creator3=null;
    }


  // java.util.Iterator methods
  public boolean hasNext() 
    {
      int is_end_int=core.librdf_iterator_end(this.object);
      return (is_end_int == 0);
    }


  public Object next() {
    long node_object=core.librdf_iterator_get_next(this.object);
    
    if(node_object == 0)
      throw new java.util.NoSuchElementException();

    return new Node(this.world, node_object, false);
  }


  public void remove() 
    {
      throw new UnsupportedOperationException();
    }

}
