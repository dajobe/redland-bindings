#!/usr/bin/python
#
# example-retrieve-from-saved.py - Redland Python 2.0 example code
#
# Copyright (C) 2000-2005 David Beckett - http://www.dajobe.org/
# Copyright (C) 2000-2005 University of Bristol - http://www.bristol.ac.uk/
# 
# This package is Free Software and part of Redland http://librdf.org/
# 
# It is licensed under the following three licenses as alternatives:
#   1. GNU Lesser General Public License (LGPL) V2.1 or any newer version
#   2. GNU General Public License (GPL) V2 or any newer version
#   3. Apache License, V2.0 or any newer version
# 
# You may not use this file except in compliance with at least one of
# the above three licenses.
# 
# See LICENSE.html or LICENSE.txt at the top of this package for the
# full license terms.
# 
#

# adapted from http://blog.literarymachine.net/?p=5

import RDF
 
# Create a new MySQL storage. The second parameter is NOT the
# name of the MySQL database to use, but the name of the
# triplestore. This makes it possible to create several
# triplestores within one database. The third parameter is
# a string containing the options for the actual MySQL database.
# They should speak for themselves, except for "new='yes'". If
# this option is given, the necessary table structure is created and
# any existing triples are dropped. You probably only want to use
# it in some kind of setup or installation procedure.

storage = RDF.Storage(storage_name="mysql", name="www.dajobe.org",
                      options_string="new='no',host='localhost',database='tests',user='librdf',password='whatever'");
if storage is None:
  raise Exception("new RDF.Storage failed")

#RDF.debug(1)

model=RDF.Model(storage)
if model is None:
  raise Exception("new RDF.model failed")

print "Printing all statements"
for s in model.as_stream():
  print "Statement:",s

q = RDF.Query("PREFIX dc: <http://purl.org/dc/elements/1.1/> SELECT ?a ?c WHERE {?a dc:title ?c}")
print "Querying for dc:titles:"
for result in q.execute(model):
  print "{"
  for k in result:
    print "  "+k+" = "+str(result[k])
  print "}"

print "Done"
