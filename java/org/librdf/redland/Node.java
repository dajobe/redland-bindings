// -*- Mode: java; c-basic-offset: 2 -*-
//
// node.java - Redland Java Node class
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

public class Node
{
  private long object;
  private World world;
  private boolean dont_free_me=false;
  
  public Node(World world) 
    {
      this.world=world;
      this.object=core.librdf_new_node(world.__get_object());
    }

  public Node(World world, String uri_string) 
    {
      this.world=world;
      this.object=core.librdf_new_node_from_uri_string(world.__get_object(), uri_string);
    }

  public Node(World world, URI uri)
    {
      this.world=world;
      this.object=core.librdf_new_node_from_uri(world.__get_object(), uri.__get_object());
    }

  public Node(World world, String literal_string, String xml_language,
       int xml_space, boolean is_wf_xml)
    {
      int is_wf_xml_int=is_wf_xml ? 1 : 0;
      this.world=world;
      this.object=core.librdf_new_node_from_literal(world.__get_object(), literal_string, xml_language, xml_space, is_wf_xml_int);
    }

  public Node(Node old_node)
    {
      this.world=old_node.world;
      this.object=core.librdf_new_node_from_node(old_node.object);
    }

  // internal constructor to build an object from a node created
  // by librdf e.g. from the result of a iterator.next() operation
  // this may be shared in which case it should not be freed
  protected Node(World world, long object, boolean free_me)
    {
      this.world=world;
      this.object=object;
      this.dont_free_me=!free_me;
    }


  protected void finalize()  throws Throwable
    {
      if(!dont_free_me)
        core.librdf_free_node(this.object);
      this.object=0;

      super.finalize();
    }

  
  public URI getURI() 
    {
      long uri_object=core.librdf_node_get_uri(this.object);
      return new URI(this.world, uri_object);
    }

  public int setURI(URI uri) 
    {
      return core.librdf_node_set_uri(this.object, uri.__get_object());
    }
  

  public int getType() 
    {
      return core.librdf_node_get_type(this.object);
    }

  public void setType(int type) 
    {
      core.librdf_node_set_type(this.object, type);
    }
  

  public String getLiteralValue() 
    {
      return core.librdf_node_get_literal_value(this.object);
    }
  
  public String getLiteralValueAsLatin1() 
    {
      return core.librdf_node_get_literal_value_as_latin1(this.object);
    }
  
  public String getLiteralValueLanguage() 
    {
      return core.librdf_node_get_literal_value_language(this.object);
    }
  
  public int getLiteralValueXMLSpace() 
    {
      return core.librdf_node_get_literal_value_xml_space(this.object);
    }
  
  public boolean getLiteralValueIsWfXML() 
    {
      int is_wf_xml_int=core.librdf_node_get_literal_value_is_wf_xml(this.object);
      return (is_wf_xml_int >0);
    }
  

  public int setLiteralValue(String value, String xml_language, int xml_space, boolean is_wf_xml) 
    {
      int is_wf_xml_int=is_wf_xml ? 1 : 0;
      return core.librdf_node_set_literal_value(this.object, value, xml_language, xml_space, is_wf_xml_int);
    }

  public String toString() 
    {
      return core.librdf_node_to_string(this.object);
    }
  

  public boolean equals(Node first_node, Node second_node) 
    {
      int equals=core.librdf_node_equals(first_node.object, second_node.object);
      return (equals != 0);
    }


  protected long __get_object() 
    {
      return this.object;
    }

  protected void __zap_object() 
    {
      this.dont_free_me=true;
    }

}
