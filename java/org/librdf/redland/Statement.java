// -*- Mode: java; c-basic-offset: 2 -*-
//
// statement.java - Redland Java Statement class
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

public class statement
{
  private long object;
  private world world;
  private boolean dont_free_me=false;

  public statement(world world) 
    {
      this.world=world;
      this.object=redland.librdf_new_statement(world.__get_object());
    }
  
  public statement(statement old_statement) 
    {
      this.world=old_statement.world;
      object=redland.librdf_new_statement_from_statement(old_statement.object);
    }

  public statement(world world,
            node subject, node predicate, node object) 
    {
      long s=(subject != null) ? subject.__get_object() : 0;
      long p=(predicate != null) ? predicate.__get_object() : 0;
      long o=(object != null) ? object.__get_object() : 0;

      this.world=world;
      this.object=redland.librdf_new_statement_from_nodes(world.__get_object(), s, p, o);

      if(subject !=null)
        subject.__zap_object();
      if(predicate !=null)
        predicate.__zap_object();
      if(object !=null)
        object.__zap_object();
    }

  // internal constructor to build an object from a statement created
  // by librdf e.g. from the result of a stream.next() operation
  protected statement(world world, long object, boolean free_me) 
    {
      this.world=world;
      this.object=object;
      this.dont_free_me=!free_me;
    }
  

  protected void finalize() 
    {
      if(!this.dont_free_me)
        redland.librdf_free_statement(this.object);
      this.object=0;
    }
  

  public node get_subject() 
    {
      long node_object=redland.librdf_statement_get_subject(this.object);
      return new node(this.world, node_object, false);
    }

  public void set_subject(node node) 
    {
      redland.librdf_statement_set_subject(this.object, node.__get_object());
      node.__zap_object();
    }


  public node get_predicate()
    {
      long node_object=redland.librdf_statement_get_predicate(this.object);
      return new node(this.world, node_object, false);
    }

  public void set_predicate(node node)
    {
      redland.librdf_statement_set_predicate(this.object, node.__get_object());
      node.__zap_object();
    }


  public node get_object() 
    {
      long node_object=redland.librdf_statement_get_object(this.object);
      return new node(this.world, node_object, false);
    }

  public void set_object(node node) 
    {
      redland.librdf_statement_set_object(this.object, node.__get_object());
      node.__zap_object();
    }


  public String to_string() 
    {
      return redland.librdf_statement_to_string(this.object);
    }
  

  protected long __get_object() 
    {
      return this.object;
    }

}
