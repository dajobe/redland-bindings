// -*- Mode: java; c-basic-offset: 2 -*-
//
// parser.java - Redland Java Parser class
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
import org.librdf.uri;

public class parser
{
  private long object;
  private world world;

  public parser (world world, String name, String mime_type, uri type_uri) 
    {
      this.world=world;
      this.object=redland.librdf_new_parser(world.__get_object(), name, mime_type, type_uri.__get_object());
    }
  
  protected void finalize() 
    {
      redland.librdf_free_parser(this.object);
    }
  

  public stream parse_as_stream(uri uri, uri base_uri) 
    {
      long stream_object=redland.librdf_parser_parse_as_stream(this.object, uri.__get_object(), base_uri.__get_object());
      return new stream(this.world, stream_object, this);
    }

  
  public int parse_into_model(uri uri, uri base_uri, model model) 
    {
      return redland.librdf_parser_parse_into_model(this.object, uri.__get_object(), base_uri.__get_object(), model.__get_object());
    }

  
  public String get_feature(uri feature) 
    {
      return redland.librdf_parser_get_feature(this.object, feature.__get_object());
    }

  
  public int set_feature(uri feature, String value) 
    {
      return redland.librdf_parser_set_feature(this.object, feature.__get_object(), value);
    }
  
}
