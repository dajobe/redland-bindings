// -*- Mode: java; c-basic-offset: 2 -*-
//
// uri.java - Redland Java URI class
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

public class URI
{
  private long object;
  private World world;
  private boolean dont_free_me=false;
  
  public URI(World world, String uri_string) 
    {
      this.world=world;
      this.object=core.librdf_new_uri(world.__get_object(), uri_string);
    }
  
  public URI(URI old_uri)  
    {
      this.world=old_uri.world;
      this.object=core.librdf_new_uri_from_uri(old_uri.object);
    }

  protected URI(World world, long object)  
    {
      this.world=world;
      this.object=object;
      this.dont_free_me=true;
    }

  protected void finalize() throws Throwable
    {
      if(!this.dont_free_me)
        core.librdf_free_uri(this.object);
      this.object=0;

      super.finalize();
    }
  
  public String toString () {
    return core.librdf_uri_to_string(this.object);
  }
  
  public boolean equals(URI first_uri, URI second_uri) 
    {
      int equals_int=core.librdf_uri_equals(first_uri.object, second_uri.object);
      return (equals_int != 0);
    }


  protected long __get_object() 
    {
      return this.object;
    }

}
