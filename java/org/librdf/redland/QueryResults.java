// -*- Mode: java; c-basic-offset: 2 -*-
//
// QueryResults.java - Redland Java QueryResults class
//
// $Id:$
//
// Copyright (C) 2006 David Beckett - http://purl.org/net/dajobe/
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

import java.util.Map;
import java.util.HashMap;
import org.librdf.redland.core;
import org.librdf.redland.World;
import org.librdf.redland.Node;

public class QueryResults implements java.util.Iterator
{
  private long object;
  private World world;
  
  public QueryResults(long object)
    {
      this.object=object;
    }

  public void finished()
  {
    if(this.object != 0) {
      core.librdf_free_query_results(this.object);
      this.object=0;
      this.world=null;
    }
  }


  private Map MakeResultsHash ()
    {
      Map h = new HashMap ();
      int c = core.librdf_query_results_get_bindings_count (this.object);
      for (int i = 0; i < c; i++) {
        String name = core.librdf_query_results_get_binding_name (this.object, i);
        long value = core.librdf_query_results_get_binding_value (this.object, i);
        if (value != 0) {
          h.put (name, new Node (world, value));
        } else {
          h.put (name, null);
        }
      }
      
      return h;
    }

  public int bindingsCount ()
    {
      return core.librdf_query_results_get_bindings_count (this.object);
    }

  public int resultsCount ()
    {
      return core.librdf_query_results_get_count (this.object);
    }

  public String bindingName (int offset)
    {
      return core.librdf_query_results_get_binding_name (this.object, offset);
    }

  public Node bindingValue (int offset)
    {
      long v = core.librdf_query_results_get_binding_value (this.object, offset);

      if (v != 0)
        return new Node (this.world, v);
      else
        return null;
    }

  public Node bindingValueByName (String name)
    {
      long v = core.librdf_query_results_get_binding_value_by_name (this.object, name);

      if (v != 0)
        return new Node (this.world, v);
      else
        return null;
    }

  public Object next()
    {
      if(core.librdf_query_results_next (this.object) == 0)
        return MakeResultsHash ();
      else
        return null;
    }

  public boolean hasNext() 
    {
      int is_end_int=core.librdf_query_results_finished(this.object);
      
      if (is_end_int == 0)
      	return true;
      else
      	return false;
    }

  public Stream asStream ()
    {
      long raw_ret = core.librdf_query_results_as_stream (this.object);
      // FIXME: throw exception if zero?
      if(raw_ret != 0)
        return new Stream (world, raw_ret, null);
      else 
        return null;
    }

  public void remove() 
    {
      throw new UnsupportedOperationException();
    }

}
