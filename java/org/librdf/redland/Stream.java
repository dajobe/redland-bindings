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

package org.librdf;

import org.librdf.world;
import org.librdf.redland;

public class stream
{
  private long object;
  private world world;
  private Object creator;

  public stream(world world, long object, Object creator) 
    {
      this.world=world;
      this.object=object;
      this.creator=creator;
    }


  protected void finalize() 
    {
      redland.librdf_free_stream(this.object);
      this.object=0;
      this.creator=null;
    }


  public boolean is_end() 
    {
      int is_end_int=redland.librdf_stream_end(this.object);
      return (is_end_int != 0);
    }


  public statement next()
    {
      long statement_object=redland.librdf_stream_next(this.object);

      return new statement(this.world, statement_object, true);
    }

  protected long __get_object() 
    {
      return this.object;
    }

}
