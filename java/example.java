// -*- Mode: java; c-basic-offset: 2 -*-
//
// example.c - Redland example code for Java
//
// $Id$
//
// Copyright (C) 2001-2006 David Beckett - http://purl.org/net/dajobe/
// Copyright (C) 2001-2002 University of Bristol - http://www.bristol.ac.uk/
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

import org.librdf.redland.*;

class example {
  
  public static void main(String[] args) {

    World world=new World();
    world.open();

    System.out.println("Redland version: " + world.versionString());
    System.out.println("Redland Copyright: " + world.copyrightString());

    Storage storage=new Storage(world, "hashes", "test", 
                                "new='yes',hash-type='memory',dir='.'");
    if(storage == null) {
      System.out.println("Failed to create RDF storage");
      System.exit(1);
    }

    Model model=new Model(world, storage, "");
    if(model == null) {
      System.out.println("Failed to create RDF model");
      System.exit(1);
    }

    URI creator_uri=new URI(world, "http://purl.org/dc/elements/1.1/creator");
    
    Statement statement=new Statement(world,
                                      new Node(world, new URI(world, "http://purl.org/net/dajobe/")), 
                                      new Node(world, creator_uri),
                                      new Node(world, "Dave Beckett", "", false));
    if(statement == null) {
      System.out.println("Failed to create RDF statement");
      System.exit(1);
    }

    model.add(statement);
    statement.finished();
    statement=null;

    Node n=new Node(world, new URI(world, "http://example.org/foo"));
    Statement statementn=new Statement(world, n, n, n);

    System.out.println("Adding statement (new Statement(n, n, n)) to model\n");
    model.add(statementn);
    statementn.finished();
    statementn=null;

    System.out.println("Adding statement (n,n,n) to model\n");
    model.add(n, n, n);

    System.out.println("Parsing URI (file) test_file\n");
    URI uri=new URI(world, "file:../data/dc.rdf");

    // Use any rdf/xml parser that is available
    Parser parser=new Parser(world, "rdfxml", "application/rdf+xml", null);
    if(parser == null) {
      System.out.println("Failed to create RDF/XML parser");
      System.exit(1);
    }

    Stream stream=parser.parse(uri, uri);
    int count=0;
    while(stream.hasNext()) {
      model.add(stream.current());
      count++;
      stream.next();
    }
    stream.finished();
    stream=null;

    System.out.println("Parsing added "+count+" statements");

    System.out.println("Printing all statements");
    stream=model.as_stream();
    while(stream.hasNext()) {
      System.out.println("Statement: "+stream.current());
      stream.next();
    }
    stream.finished();
    stream=null;

    System.out.println("Searching model for statements matching predicate http://purl.org/dc/elements/1.1/creator");
    statement=new Statement(world, null, new Node(world, creator_uri), null);
    stream=model.findStatements(statement);
    while(stream.hasNext()) {
      Statement statement2=stream.current();
      System.out.println("Matching Statement: "+statement2);
      Node subject=statement2.getSubject();
      System.out.println("  Subject: "+subject);
      System.out.println("  Predicate: "+statement2.getPredicate());
      System.out.println("  Object: "+statement2.getObject());
      stream.next();
    }
    stream.finished();
    stream=null;

    statement.finished();
    statement=null;

    Node home=new Node(world, new URI(world, "http://purl.org/net/dajobe/"));
    System.out.println("Searching model for targets of subject "+home.getURI()+" predicate " + creator_uri);

    org.librdf.redland.Iterator it=model.getTargets(home, new Node(world, creator_uri));

    if(it == null) {
      System.out.println("Failed to find any targets matching");
      System.exit(1);
    }

    while(it.hasNext()) {
      Node node=(Node)it.next();
      System.out.println("Matching Node: "+node);
    }
    it.finished();
    it=null;

    Query q = new Query(world, "PREFIX dc: <http://purl.org/dc/elements/1.1/>\nSELECT ?a ?c WHERE { ?a dc:title ?c }", null, "sparql", null);
    System.out.println("Querying for dc:titles:");
    QueryResults results=model.queryExecute(q);
    count=0;
    while(results.hasNext()) {
      System.out.println("result count: {");
      for(int i=0; i < results.bindingsCount(); i++) {
        Node val=results.bindingValue(i);
        // optionals
        if(val != null) {
          System.out.println("  "+results.bindingName(i)+"="+val);
        }
      }
      System.out.println("}");
      results.next();
      count++;
    }
    results.finished();
    results=null;
    System.out.println("Returned "+count+" results");

    System.out.println("\nExecuting query again");
    results=model.queryExecute(q);
    String str=results.toString();
    System.out.println("Query results serialized to an XML string size "+str.length()+" bytes");

    System.out.println("\nExecuting SPARQL construct query");
    Query q2 = new Query(world, "CONSTRUCT * WHERE { ?a ?b ?c }", null, "sparql", null);
    results=model.queryExecute(q2);
    stream=results.asStream();
    count=0;
    while(stream.hasNext()) {
      System.out.println("Statement: "+stream.current());
      stream.next();
      count++;
    }
    stream.finished();
    stream=null;

    results.finished();
    results=null;
    System.out.println("Returned "+count+" triples");

    System.out.println("Writing model to test-out.rdf as rdf/xml");

    /* Use any rdf/xml parser that is available */
    Serializer serializer=new Serializer(world, "rdfxml", null, null);
    if(serializer == null) {
      System.out.println("Failed to create RDF/XML serializer");
      System.exit(1);
    }

    serializer.setNamespace("dc", new URI(world, "http://purl.org/dc/elements/1.1/"));
    serializer.toFile("test-out.rdf", uri, model);

    String str1=serializer.toString(uri, model);
    System.out.println("Serialized to RDF/XML as a string size "+str1.length()+" bytes");
    serializer=null;

    str=model.toString(new URI(world, "http://example.org/base#"), "ntriples", null, null);
    System.out.println("Serialized to ntriples as a string size "+str.length()+" bytes");

    model.finished();
    model=null;

    storage.finished();
    storage=null;
    
    world.finished();
    world=null;
  }
}
