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

package org.librdf.redland;

import org.librdf.redland.core;
import org.librdf.redland.World;
import org.librdf.redland.URI;

public class Parser
{
  private long object;
  private World world;

  public Parser (World world, String name, String mime_type, URI type_uri) 
    {
      this.world=world;
      long uri_object=(type_uri == null) ? 0 : type_uri.__get_object();
      this.object=core.librdf_new_parser(world.__get_object(), name, mime_type, uri_object);
    }
  
  public void finished()
    {
      if(this.object != 0) {
        core.librdf_free_parser(this.object);
        this.object=0;
      }
    }


  public Stream parse(URI uri, URI base_uri) 
    {
      long stream_object=core.librdf_parser_parse_as_stream(this.object, uri.__get_object(), base_uri.__get_object());
      return new Stream(this.world, stream_object, this);
    }

  
  public boolean parse(URI uri, URI base_uri, Model model) 
    {
      int result=core.librdf_parser_parse_into_model(this.object, uri.__get_object(), base_uri.__get_object(), model.__get_object());
      return (result != 0);
    }

  
  public Stream parse(String s, URI base_uri) 
    {
      long stream_object=core.librdf_parser_parse_string_as_stream(this.object, s, base_uri.__get_object());
      return new Stream(this.world, stream_object, this);
    }

  
  public boolean parse(String s, URI base_uri, Model model) 
    {
      int result=core.librdf_parser_parse_string_into_model(this.object, s, base_uri.__get_object(), model.__get_object());
      return (result != 0);
    }

  
  public Node getFeature(URI feature) 
    {
      return new Node(this.world, core.librdf_parser_get_feature(this.object, feature.__get_object()), true);
    }

  
  public int setFeature(URI feature, Node value) 
    {
      return core.librdf_parser_set_feature(this.object, feature.__get_object(), value.__get_object());
    }
  
}
