// -*- Mode: java; c-basic-offset: 2 -*-
//
// node.java - Redland Java Node class
//
// $Id$
//
// Copyright (C) 2001-2003 David Beckett - http://purl.org/net/dajobe/
// Copyright (C) 2001-2003 University of Bristol - http://www.bristol.ac.uk/
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

/** Node is an interface to the Redland librdf_node class
 *
 * <p>An <code>Node</code> is a class that represents nodes (resource,
 * literal, blank node) and arcs (properties, predicates) in the graph.</p>
 *
 * @version Revision='$Revision$' Date='$Date$'
 */

public class Node
{
  private long object;
  private World world;
  
  /**
   * Construct a new resource node
   *
   * @param world       Redland world object
   */
  public Node(World world)
    {
      this.world=world;
      this.object=core.librdf_new_node(world.__get_object());
    }

  /**
   * Construct a new resource node from a URI string or blank node identifier
   *
   * @param world       Redland world object
   * @param identifier  The URI string / identifier or null
   * @param is_blank    boolean to distinguish the two cases
   */
  public Node(World world, String identifier, boolean is_blank) 
    {
      this.world=world;
      if(is_blank)
        this.object=core.librdf_new_node_from_blank_identifier(world.__get_object(), identifier);
      else
        this.object=core.librdf_new_node_from_uri_string(world.__get_object(), identifier);
    }

  /**
   * Construct a new resource node from a URI
   *
   * @param world       Redland world object
   * @param uri         The Redland URI object or null
   */
  public Node(World world, URI uri)
    {
      this.world=world;
      this.object=core.librdf_new_node_from_uri(world.__get_object(), uri.__get_object());
    }

  /**
   * Construct a new literal node, optionally from a URI
   *
   * @param world       Redland world object
   * @param uri         The Redland URI object or null
   */
  public Node(World world, String literal_string, String xml_language,
              boolean is_wf_xml)
    {
      int is_wf_xml_int=is_wf_xml ? 1 : 0;
      this.world=world;
      this.object=core.librdf_new_node_from_literal(world.__get_object(), literal_string, xml_language, is_wf_xml_int);
    }

  /**
   * Construct a new node from an existing node (copy constructor)
   *
   * @param world       Redland world object
   * @param old_node    The Redland Node object
   */
  public Node(Node old_node)
    {
      this.world=old_node.world;
      this.object=core.librdf_new_node_from_node(old_node.object);
    }

  // internal constructor to build an object from a node created
  // by librdf e.g. from the result of a iterator.next() operation
  protected Node(World world, long object, boolean do_not_copy)
    {
      this.world=world;
      this.object=object;
    }


  protected Node(World world, long object)
    {
      this.world=world;
      this.object=core.librdf_new_node_from_node(object);
    }


  public void finished()
    {
      if(this.object != 0) {
        core.librdf_free_node(this.object);
        this.object=0;
        this.world=null;
      }
    }

  
  public URI getURI() 
    {
      long uri_object=core.librdf_node_get_uri(this.object);
      return new URI(this.world, uri_object);
    }

  public int getType() 
    {
      return core.librdf_node_get_type(this.object);
    }

  public boolean isResource() 
    {
      return (core.librdf_node_is_resource(this.object) != 0);
    }

  public boolean isLiteral() 
    {
      return (core.librdf_node_is_literal(this.object) != 0);
    }

  public boolean isBlank() 
    {
      return (core.librdf_node_is_blank(this.object) != 0);
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
  
  public boolean getLiteralValueIsWfXML() 
    {
      int is_wf_xml_int=core.librdf_node_get_literal_value_is_wf_xml(this.object);
      return (is_wf_xml_int >0);
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

}
