// -*- Mode: java; c-basic-offset: 2 -*-
//
// world.java - Redland Java World class
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

public class World 
{
  private long object=0;
  
  public World() {
  }

  public void finished()
    {
      if(this.object != 0) {
        core.librdf_free_world(this.object);
        this.object=0;
      }
    }

  public void open() 
    {
      this.object=core.librdf_new_world();
      core.librdf_world_open(this.object);
    }

  public String versionString() 
    {
      return core.get_librdf_version_string();
    }

  public String copyrightString() 
    {
      return core.get_librdf_copyright_string();
    }

/*
  // Initialised by functions below using Java's assign "at most once"
  public static final String copyright;
  public static final String version;
  public static final int version_major;
  public static final int version_minor;
  public static final int version_release;

  static
    {
      // Load a library whose "core" name is 'rdf-java'
      // Operating system specific stuff will be added to make from this an
      // actual filename: Under Unix this will become librdf-java.so
      // while under Windows it will likely become something like
      // rdf-java.dll
      System.loadLibrary("rdf-java");

      // Initialise statics
      copyright=new String(get_librdf_copyright_string());
      version=new String(get_librdf_version_string());
      version_major=get_librdf_version_major();
      version_minor=get_librdf_version_minor();
      version_release=get_librdf_version_release();

    }
*/

  protected long __get_object() 
    {
      return this.object;
    }
  
}
