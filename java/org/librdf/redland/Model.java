// -*- Mode: java; c-basic-offset: 2 -*-
//
// model.java - Redland Java Model class
//
// $Id$
//
// Copyright (C) 2001-2004 David Beckett - http://purl.org/net/dajobe/
// Copyright (C) 2001-2004 University of Bristol - http://www.bristol.ac.uk/
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

import org.librdf.redland.Storage;
import org.librdf.redland.Node;
import org.librdf.redland.Statement;
import org.librdf.redland.Stream;
import org.librdf.redland.Iterator;
import org.librdf.redland.Hash;


public class Model
{
  private World world;
  private long object;
  private Storage storage;

  public Model(World world, Storage storage, String options_string) 
  {
    this.world=world;
    this.object=core.librdf_new_model(world.__get_object(), storage.__get_object(), options_string);
    this.storage=storage;
  }

  public Model(World world, Storage storage, Hash options) 
  {
    this.world=world;
    this.object=core.librdf_new_model_with_options(world.__get_object(), storage.__get_object(), options.__get_object());
    this.storage=storage;
  }

  public Model(Model old_model) 
  {
    this.world=old_model.world;
    this.object=core.librdf_new_model_from_model(old_model.__get_object());
  }


  public void finished()
  {
    if(this.object != 0) {
      core.librdf_free_model(this.object);
      this.object=0;
      world=null;
      storage=null;
    }
  }


  public int size() 
  {
    return core.librdf_model_size(this.object);
  }

  public void sync() 
  {
    core.librdf_model_sync(this.object);
  }

  public int add(Node subject, Node predicate, Node object) 
  {
    long subject_copy=core.librdf_new_node_from_node(subject.__get_object());
    long predicate_copy=core.librdf_new_node_from_node(predicate.__get_object());
    long object_copy=core.librdf_new_node_from_node(predicate.__get_object());
    return core.librdf_model_add(this.object, subject_copy, predicate_copy, object_copy);
  }

  public int add(Node subject, Node predicate, String string, String xml_language, URI uri)
  {
    long u=(uri != null) ? uri.__get_object() : 0;
    long subject_copy=core.librdf_new_node_from_node(subject.__get_object());
    long predicate_copy=core.librdf_new_node_from_node(predicate.__get_object());
    return core.librdf_model_add_typed_literal_statement(this.object, subject_copy, predicate_copy, string, xml_language, u);
  }

  public int add(Statement statement) 
  {
    return core.librdf_model_add_statement(this.object, statement.__get_object());
  }

  public int add(Stream statement_stream) 
  {
    return core.librdf_model_add_statements(this.object, statement_stream.__get_object());
  }

  public int remove(Statement statement) 
  {
    return core.librdf_model_remove_statement(this.object, statement.__get_object());
  }

  public int add(Statement statement, Node context) 
  {
    return core.librdf_model_context_add_statement(this.object, context.__get_object(), statement.__get_object());
  }

  public int add(Stream statement_stream, Node context) 
  {
    return core.librdf_model_context_add_statements(this.object, context.__get_object(), statement_stream.__get_object());
  }

  public int remove(Statement statement, Node context) 
  {
    return core.librdf_model_context_remove_statement(this.object, context.__get_object(), statement.__get_object());
  }

  public int context_remove(Node context) 
  {
    return core.librdf_model_context_remove_statements(this.object, context.__get_object());
  }

  public boolean contains(Statement statement) 
  {
    int contains_int=core.librdf_model_contains_statement(this.object, statement.__get_object());
    return (contains_int != 0);
  }

  public Stream as_stream() 
  {
    return new Stream(this.world, core.librdf_model_as_stream(this.object), this);
  }

  public Stream as_stream(Node context) 
  {
    return new Stream(this.world, core.librdf_model_context_as_stream(this.object, context.__get_object()), this);
  }

  public Stream findStatements(Statement statement) 
  {
    return new Stream(this.world, core.librdf_model_find_statements(this.object, statement.__get_object()), this);
  }

  public Stream findStatements(Statement statement, Node context_node) 
  {
    return new Stream(this.world, core.librdf_model_find_statements_in_context(this.object, statement.__get_object(), context_node.__get_object()), this);
  }

  public Iterator getSources(Node arc, Node target) 
  {
    return new Iterator(this.world, core.librdf_model_get_sources(this.object, arc.__get_object(), target.__get_object()), this, arc, target);
  }

  public Iterator getArcs(Node source, Node target) 
  {
    return new Iterator(this.world, core.librdf_model_get_arcs(this.object, source.__get_object(), target.__get_object()), this, source, target);
  }

  public Iterator getTargets(Node source, Node arc) 
  {
    return new Iterator(this.world, core.librdf_model_get_targets(this.object, source.__get_object(), arc.__get_object()), this, source, arc);
  }

  public Node getSource(Node arc, Node target) 
  {
    long node = core.librdf_model_get_source(this.object, arc.__get_object(), target.__get_object());
    if (node == 0)
      return null;
    else
      return new Node(this.world, node);
  }

  public Node getArc(Node source, Node target) 
  {
    long node = core.librdf_model_get_arc(this.object, source.__get_object(), target.__get_object());
    if (node == 0)
      return null;
    else
      return new Node(this.world, node);
  }

  public Node getTarget(Node source, Node arc) 
  {
    long node = core.librdf_model_get_target(this.object, source.__get_object(), arc.__get_object());
    if (node == 0)
      return null;
    else
      return new Node(this.world, node);
  }

  public Node getFeature(URI feature) 
    {
      return new Node(this.world, core.librdf_parser_get_feature(this.object, feature.__get_object()), true);
    }

  
  public int setFeature(URI feature, Node value) 
    {
      return core.librdf_parser_set_feature(this.object, feature.__get_object(), value.__get_object());
    }
  
  public Iterator getContexts() 
  {
    return new Iterator(this.world, core.librdf_model_get_contexts(this.object), this, null, null);
  }

  public String toString(URI base_uri, String name, String mime_type, URI type_uri) 
  {
    long rbase_uri=0L;
    if(base_uri !=null)
      rbase_uri=base_uri.__get_object();

    long rtype_uri=0L;
    if(type_uri !=null)
      rtype_uri=type_uri.__get_object();
    
    return core.librdf_model_to_string(this.object, rbase_uri, name, mime_type, rtype_uri);
  }

  public String toString()
  {
    return this.toString(null, "", "", null);
  }

  public QueryResults queryExecute(Query q)
  {
    long result=core.librdf_model_query_execute (this.object, q.__get_object());
    
    if (result == 0)
      return null;
    
    QueryResults qr = new QueryResults (result);
    return (qr);
  }
  

  protected long __get_object() 
  {
    return this.object;
  }
}
