// -*- Mode: java; c-basic-offset: 2 -*-
//
// storage.java - Redland Java Storage class
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

public class storage
{
  private long object;

  public storage(org.librdf.world world, String storage_name, String name, String options_string) 
    {
      redland.librdf_new_storage(world.__get_object(), 
                                 storage_name, name, options_string);
    }
  
  public storage(org.librdf.storage old_storage) 
    {
      object=redland.librdf_new_storage_from_storage(old_storage.object);
    }

  protected void finalize() 
    {
      org.librdf.redland.librdf_free_storage(this.object);
      this.object=0;
    }

  protected long __get_object() 
  {
    return this.object;
  }

}

