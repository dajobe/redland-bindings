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

package org.librdf.redland;

import org.librdf.redland.core;
import org.librdf.redland.World;

public class Storage
{
  private long object;

  public Storage(World world, String storage_name, String name, 
                 String options_string) 
    {
      this.object=core.librdf_new_storage(world.__get_object(), 
                                          storage_name, name, options_string);
    }
  
  public Storage(Storage old_storage) 
    {
      this.object=core.librdf_new_storage_from_storage(old_storage.object);
    }

  public void finished()
    {
      if(this.object != 0) {
        core.librdf_free_storage(this.object);
        this.object=0;
      }
    }


  protected long __get_object() 
  {
    return this.object;
  }

}

