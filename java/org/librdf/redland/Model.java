// -*- Mode: java; c-basic-offset: 2 -*-
//
// model.java - Redland Java Model class
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

import org.librdf.storage;
import org.librdf.node;
import org.librdf.statement;
import org.librdf.stream;
import org.librdf.iterator;
import org.librdf.hash;


public class model
{
  private world world;
  private long object;
  private storage storage;

  public model(world world, storage storage, String options_string) 
  {
    this.world=world;
    this.object=redland.librdf_new_model(world.__get_object(), storage.__get_object(), options_string);
    this.storage=storage;
  }

  public model(world world, storage storage, hash options) 
  {
    this.world=world;
    this.object=redland.librdf_new_model_with_options(world.__get_object(), storage.__get_object(), options.__get_object());
    this.storage=storage;
  }

  public model(model old_model) 
  {
    this.world=old_model.world;
    this.object=redland.librdf_new_model_from_model(old_model.__get_object());
  }


  protected void finalize()
  {
    redland.librdf_free_model(this.object);
    this.object=0;
  }


  public int size() 
  {
    return redland.librdf_model_size(this.object);
  }

  public int add(node subject, node predicate, node object) 
  {
    return redland.librdf_model_add(this.object, subject.__get_object(), predicate.__get_object(), object.__get_object());
  }

  public int add_string_literal_statement(node subject, node predicate, String string, String xml_language, int xml_space, boolean is_wf_xml) 
  {
    int is_wf_xml_int=is_wf_xml ? 1 : 0;
    return redland.librdf_model_add_string_literal_statement(this.object, subject.__get_object(), predicate.__get_object(), string, xml_language, xml_space, is_wf_xml_int);
  }

  public int add_statement(statement statement) 
  {
    return redland.librdf_model_add_statement(this.object, statement.__get_object());
  }

  public int add_statements(stream statement_stream) 
  {
    return redland.librdf_model_add_statements(this.object, statement_stream.__get_object());
  }

  public int remove_statement(statement statement) 
  {
    return redland.librdf_model_remove_statement(this.object, statement.__get_object());
  }

  public boolean contains_statement(statement statement) 
  {
    int contains_int=redland.librdf_model_contains_statement(this.object, statement.__get_object());
    return (contains_int != 0);
  }

  public stream serialise() 
  {
    return new stream(this.world, redland.librdf_model_serialise(this.object), this);
  }

  public stream find_statements(statement statement) 
  {
    return new stream(this.world, redland.librdf_model_find_statements(this.object, statement.__get_object()), this);
  }

  public iterator get_sources(node arc, node target) 
  {
    return new iterator(this.world, redland.librdf_model_get_sources(this.object, arc.__get_object(), target.__get_object()), this, arc, target);
  }

  public iterator get_arcs(node source, node target) 
  {
    return new iterator(this.world, redland.librdf_model_get_arcs(this.object, source.__get_object(), target.__get_object()), this, source, target);
  }

  public iterator get_targets(node source, node arc) 
  {
    return new iterator(this.world, redland.librdf_model_get_targets(this.object, source.__get_object(), arc.__get_object()), this, source, arc);
  }

  public node get_source(node arc, node target) 
  {
    return new node(this.world, redland.librdf_model_get_source(this.object, arc.__get_object(), target.__get_object()), true);
  }

  public node get_arc(node source, node target) 
  {
    return new node(this.world, redland.librdf_model_get_arc(this.object, source.__get_object(), target.__get_object()), true);
  }

  public node get_target(node source, node arc) 
  {
    return new node(this.world, redland.librdf_model_get_target(this.object, source.__get_object(), arc.__get_object()), true);
  }


  protected long __get_object() 
  {
    return this.object;
  }
}
