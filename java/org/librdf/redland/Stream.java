// -*- Mode: java; c-basic-offset: 2 -*-
//
// stream.java - Redland Java Stream class
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


public class Stream implements java.util.Iterator
{
  private long object;
  private World world;
  private Object creator;

  public Stream(World world, long object, Object creator) 
    {
      this.world=world;
      this.object=object;
      this.creator=creator;
    }


  protected void finalize() throws Throwable
    {
      core.librdf_free_stream(this.object);
      this.object=0;
      this.creator=null;

      super.finalize();
    }


  // java.util.Iterator methods

  public boolean hasNext() 
    {
      int is_end_int=core.librdf_stream_end(this.object);
      return (is_end_int == 0);
    }


  public Object next()
    {
      long statement_object=core.librdf_stream_next(this.object);

      return new Statement(this.world, statement_object, true);
    }

  public void remove() 
    {
      throw new UnsupportedOperationException();
    }



  protected long __get_object() 
    {
      return this.object;
    }

}
