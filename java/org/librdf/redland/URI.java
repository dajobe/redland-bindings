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

package org.librdf;

import org.librdf.world;
import org.librdf.redland;

public class uri
{
  private long object;
  private world world;
  private boolean dont_free_me=false;
  
  public uri(world world, String uri_string) 
    {
      this.world=world;
      this.object=redland.librdf_new_uri(world.__get_object(), uri_string);
    }
  
  public uri(uri old_uri)  
    {
      this.world=old_uri.world;
      this.object=redland.librdf_new_uri_from_uri(old_uri.object);
    }

  protected uri(world world, long object)  
    {
      this.world=world;
      this.object=object;
      this.dont_free_me=true;
    }

  protected void finalize() 
    {
      if(!this.dont_free_me)
        redland.librdf_free_uri(this.object);
      this.object=0;
    }
  
  public String to_string () {
    return redland.librdf_uri_to_string(this.object);
  }
  
  public boolean equals(uri first_uri, uri second_uri) 
    {
      int equals_int=redland.librdf_uri_equals(first_uri.object, second_uri.object);
      return (equals_int != 0);
    }

  protected long __get_object() 
    {
      return this.object;
    }

}
