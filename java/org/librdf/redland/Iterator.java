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

package org.librdf;

import org.librdf.world;
import org.librdf.redland;
import org.librdf.node;

public class iterator
{
  private long object;
  private world world;
  private Object creator1;
  private Object creator2;
  private Object creator3;
  
  public iterator (world world, long object,
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
      redland.librdf_free_iterator(this.object);
      this.object=0;
      this.creator1=null;
      this.creator2=null;
      this.creator3=null;
    }

  public boolean have_elements() 
    {
      int have_elements_int=redland.librdf_iterator_have_elements(this.object);
      return (have_elements_int != 0);
    }

  public boolean is_end() 
    {
      int is_end_int=redland.librdf_iterator_end(this.object);
      return (is_end_int != 0);
    }

  public node next() {
    long node_object=redland.librdf_iterator_get_next(this.object);

    return new node(this.world, node_object, false);
  }

}
