// -*- Mode: java; c-basic-offset: 2 -*-
//
// Query.java - Redland Java Query class
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

import org.librdf.redland.core;
import org.librdf.redland.World;
import org.librdf.redland.URI;

public class Query
{
  private long object;
  private World world;

  public static String RDQL = "rdql";

  public Query (World world, String s, URI base_uri, String query_language, URI query_uri)
    {
      this.world=world;

      long ibase_uri=(base_uri != null) ? base_uri.__get_object() : 0;
      long iquery_uri=(query_uri != null) ? query_uri.__get_object() : 0;

      this.object=core.librdf_new_query (world.__get_object(), query_language, iquery_uri, s, ibase_uri);	
    }

  public QueryResults execute(Model model)
    {
      long result=core.librdf_query_execute (this.object, model.__get_object());

      if (result == 0)
        return null;

      QueryResults qr = new QueryResults (result);
      return (qr);
    }

  public void finished()
    {
      if(this.object != 0) {
        core.librdf_free_query(this.object);
        this.object=0;
      }
    }

  protected long __get_object() 
    {
      return this.object;
    }

}
