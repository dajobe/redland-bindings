/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * redland-post.i - Perl SWIG helper routines
 *
 * $Id$
 *
 * Copyright (C) 2000-2005, David Beckett http://purl.org/net/dajobe/
 * Institute for Learning and Research Technology http://www.ilrt.bristol.ac.uk/
 * University of Bristol, UK http://www.bristol.ac.uk/
 * 
 * This package is Free Software and part of Redland http://librdf.org/
 * 
 * It is licensed under the following three licenses as alternatives:
 *   1. GNU Lesser General Public License (LGPL) V2.1 or any newer version
 *   2. GNU General Public License (GPL) V2 or any newer version
 *   3. Apache License, V2.0 or any newer version
 * 
 * You may not use this file except in compliance with at least one of
 * the above three licenses.
 * 
 * See LICENSE.html or LICENSE.txt at the top of this package for the
 * complete terms and further detail along with the license texts for
 * the licenses in COPYING.LIB, COPYING and LICENSE-2.0.txt respectively.
 * 
 * 
 */

void librdf_perl_world_init(librdf_world *world);
void librdf_perl_world_finish(void);


/*
 * calls a perl subroutine defined as:
 *   RDF::Redland::World::message($$)
 * where first argument is an integer, second is a (scalar) string
 */
static int
librdf_call_perl_message(int code, int level, int facility,
                         const char *message,
                         int line, int column, int byte,
                         const char *file, const char *uri)
{
  dSP;
  int count;
  int rc=0;
  
  SAVETMPS;

  PUSHMARK(SP) ;
  XPUSHs(sv_2mortal(newSViv(code)));
  XPUSHs(sv_2mortal(newSViv(level)));
  XPUSHs(sv_2mortal(newSViv(facility)));
  if(message != NULL)
    XPUSHs(sv_2mortal(newSVpv(message, 0)));
  else
    XPUSHs((void*)0);
  XPUSHs(sv_2mortal(newSViv(line)));
  XPUSHs(sv_2mortal(newSViv(column)));
  XPUSHs(sv_2mortal(newSViv(byte)));
  if(file != NULL)
    XPUSHs(sv_2mortal(newSVpv(file, 0)));
  else
    XPUSHs((void*)0);
  if(uri != NULL)
    XPUSHs(sv_2mortal(newSVpv(uri, 0)));
  else
    XPUSHs((void*)0);
  PUTBACK;
  
  count=call_pv("RDF::Redland::World::message", G_SCALAR);

  SPAGAIN;
  if(count == 1)
    rc=POPi;
  PUTBACK;
    
  FREETMPS;
  
  return rc;
}

static int
librdf_perl_logger_handler(void *user_data, librdf_log_message *log)
{
  raptor_locator* locator = log->locator;
  int line= -1;
  int column= -1;
  int byte= -1;
  const char *uri=NULL;
  const char *file=NULL;
    
  if(locator) {
    line=raptor_locator_line(locator);
    column=raptor_locator_column(locator);
    byte=raptor_locator_byte(locator);
    file=raptor_locator_file(locator);
    uri=raptor_locator_uri(locator);
  }
  
  return librdf_call_perl_message(log->code, log->level, log->facility,
                                  log->message,
                                  line, column, byte, file, uri);
}


static librdf_world* librdf_perl_world=NULL;

void
librdf_perl_world_init(librdf_world *world)
{
  librdf_world_set_logger(world, NULL, librdf_perl_logger_handler);

  librdf_perl_world=world;
}

void
librdf_perl_world_finish(void)
{
  librdf_free_world(librdf_perl_world);
}
