import std.typecons;
import std.stdio;
import rdf.raptor.iostream;

void countTest(string dir) {
    static import rdf.raptor.uri;
    import rdf.rasqal.data_graph;
    import rdf.rasqal.world;
    import rdf.rasqal.query;
    import rdf.rasqal.query_results;
    import rdf.rasqal.literal;

    string rdfFile = dir ~ "../data/dc.nt";
    string sparql = "SELECT (count(*) as ?count) WHERE { ?s ?p ?o . }";
    RasqalWorld world = RasqalWorld.createAndOpen();
    IOStream graphStream = IOStream.fromFilename(world.raptor, rdfFile);
    DataGraph graph = DataGraph.fromIOStream(world,
                                             graphStream,
                                             rdf.raptor.uri.URI.fromString(world.raptor, "http://example.org")
                                          //                                                 Name_URI => URI_Type'(From_Handle(null)),
                                          //                                                 Flags => Background,
                                          //                                                 Format_Type => Empty_Holder,
                                          //                                                 Format_Name => Empty_Holder,
                                          //                                                 Format_URI => URI_Type'(From_Handle(null))
                                         );
    Query query = Query.create(world, Nullable!string(), Nullable!string());
    query.prepare(sparql);
    query.addDataGraph(graph);
    QueryResults results = query.execute();
    string[] rows;
    assert(results.isBindings, "Result is bindings");
    foreach(i; QueryResultsRange(results)) {
        LiteralWithoutFinalize L = i.getBindingValueByName("count");
        rows ~= L.toString;
    }
    assert(rows.length == 1, "count() returns one row");
    assert(rows[0] == "3", "count() == 3");
}

void countTest2(string dir) {
    import rdf.redland.world;
    import rdf.redland.uri;
    import rdf.redland.node;
    import rdf.redland.model;
    import rdf.redland.storage;
    import rdf.redland.query;
    import rdf.redland.query_results;

    RedlandWorld world = RedlandWorld.createAndOpen();
    Storage storage = Storage.create(world, "memory", "test");
    Model model = Model.create(world, storage);
    string rdfFile = dir ~ "../data/dc.nt";
    string sparql = "SELECT (count(*) as ?count) WHERE { ?s ?p ?o . }";
//        Query: Query_Type := Copy(Create(World, "sparql", SPARQL)); -- FIXME: Copy causes an unhandled signal?
    Query query = Query.create(world, "sparql", sparql);
    QueryResults results = model.queryExecute(query);
    model.load(URI.fromFilename(world, rdfFile));
    string[] rows;
    foreach(i; QueryResultsRange(results)) {
        Node L = i.getBindingValueByName("count");
        rows ~= L.toString;
    }
    assert(rows.length == 1, "count() returns one row");
    assert(rows[0] == "3", "count() == 3");
}

void main(string[] args) {
    string dir = args.length > 1 ? args[1] ~ '/' : "";
    countTest(dir);
    countTest2(dir);
    writeln("Tests passed.");
}
