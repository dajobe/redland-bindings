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

package org.librdf.redland;

import org.librdf.redland.core;
import org.librdf.redland.World;

public class Statement
{
  private long object;
  private World world;
  private boolean dont_free_me=false;

  public Statement(World world) 
    {
      this.world=world;
      this.object=core.librdf_new_statement(world.__get_object());
    }
  
  public Statement(Statement old_statement) 
    {
      this.world=old_statement.world;
      object=core.librdf_new_statement_from_statement(old_statement.object);
    }

  public Statement(World world,
                   Node subject, Node predicate, Node object) 
    {
      long s=(subject != null) ? subject.__get_object() : 0;
      long p=(predicate != null) ? predicate.__get_object() : 0;
      long o=(object != null) ? object.__get_object() : 0;

      this.world=world;
      this.object=core.librdf_new_statement_from_nodes(world.__get_object(), s, p, o);

      if(subject !=null)
        subject.__zap_object();
      if(predicate !=null)
        predicate.__zap_object();
      if(object !=null)
        object.__zap_object();
    }

  // internal constructor to build an object from a statement created
  // by librdf e.g. from the result of a stream.next() operation
  protected Statement(World world, long object, boolean free_me) 
    {
      this.world=world;
      this.object=object;
      this.dont_free_me=!free_me;
    }
  

  public void finished()
    {
      if(this.object != 0) {
        if(!this.dont_free_me)
          core.librdf_free_statement(this.object);
        this.object=0;
        this.world=null;
      }
    }
  

  public Node getSubject() 
    {
      long node_object=core.librdf_statement_get_subject(this.object);
      return new Node(this.world, node_object, false);
    }

  public void setSubject(Node node) 
    {
      core.librdf_statement_set_subject(this.object, node.__get_object());
      node.__zap_object();
    }


  public Node getPredicate()
    {
      long node_object=core.librdf_statement_get_predicate(this.object);
      return new Node(this.world, node_object, false);
    }

  public void setPredicate(Node node)
    {
      core.librdf_statement_set_predicate(this.object, node.__get_object());
      node.__zap_object();
    }


  public Node getObject() 
    {
      long node_object=core.librdf_statement_get_object(this.object);
      return new Node(this.world, node_object, false);
    }

  public void setObject(Node node) 
    {
      core.librdf_statement_set_object(this.object, node.__get_object());
      node.__zap_object();
    }


  public String toString() 
    {
      return core.librdf_statement_to_string(this.object);
    }
  


  protected long __get_object() 
    {
      return this.object;
    }

}
