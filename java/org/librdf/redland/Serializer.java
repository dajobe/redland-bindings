// -*- Mode: java; c-basic-offset: 2 -*-
//
// Serializer.java - Redland Java Serializer class
//
// $Id:
//
// Copyright (C) 2002 David Beckett - http://purl.org/net/dajobe/
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
import org.librdf.redland.URI;

public class Serializer
{
  private SWIGTYPE_p_librdf_serializer_s object;
  private World world;

  public Serializer (World world, String name, String mime_type, URI type_uri) 
    {
      this.world=world;
      SWIGTYPE_p_librdf_uri_s uri_object=(type_uri == null) ? null : type_uri.__get_object();
      this.object=core.librdf_new_serializer(world.__get_object(), name, mime_type, uri_object);
    }
  
  protected void finalize() throws Throwable
    {
      core.librdf_free_serializer(this.object);

      super.finalize();
    }


  public boolean serialize_to_file(const String filename, URI base_uri, Model model) 
    {
      int result=core.librdf_serializer_serialize_to_file(this.object, filename, base_uri.__get_object(), model.__get_object());
      return (result != 0);
    }

  
  public String getFeature(URI feature) 
    {
      return core.librdf_serializer_get_feature(this.object, feature.__get_object());
    }

  
  public int setFeature(URI feature, String value) 
    {
      return core.librdf_serializer_set_feature(this.object, feature.__get_object(), value);
    }
  
}
