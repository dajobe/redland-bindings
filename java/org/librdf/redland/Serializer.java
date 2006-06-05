// -*- Mode: java; c-basic-offset: 2 -*-
//
// Serializer.java - Redland Java Serializer class
//
// $Id$
//
// Copyright (C) 2002-2004 David Beckett - http://purl.org/net/dajobe/
// Copyright (C) 2002-2004 University of Bristol - http://www.bristol.ac.uk/
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
  private long object;
  private World world;

  public Serializer (World world, String name, String mime_type, URI type_uri) 
    {
      this.world=world;
      long uri_object=(type_uri == null) ? 0 : type_uri.__get_object();
      this.object=core.librdf_new_serializer(world.__get_object(), name, mime_type, uri_object);
    }
  
  public void finished()
    {
      if(this.object != 0) {
        core.librdf_free_serializer(this.object);
        this.object=0;
        this.world=null;
      }
    }


  public boolean serialize_to_file(String filename, URI base_uri, Model model) 
    {
      int result=core.librdf_serializer_serialize_model_to_file(this.object, filename, base_uri.__get_object(), model.__get_object());
      return (result != 0);
    }

  public String serialize_to_string(URI base_uri, Model model)
    {
      return core.librdf_serializer_serialize_model_to_string(this.object, base_uri.__get_object(), model.__get_object());
    }
  
  public Node getFeature(URI feature) 
    {
      return new Node(this.world, core.librdf_serializer_get_feature(this.object, feature.__get_object()), true);
    }

  
  public int setFeature(URI feature, Node value) 
    {
      return core.librdf_serializer_set_feature(this.object, feature.__get_object(), value.__get_object());
    }
  
  public int setNamespace(String prefix, URI namespace) 
    {
      return core.librdf_serializer_set_namespace(this.object, namespace.__get_object(), prefix);
    }
  
}
