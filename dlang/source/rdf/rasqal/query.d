module rdf.rasqal.query;

import std.string;
import rdf.auxiliary.handled_record;
import rdf.raptor.uri;
import rdf.rasqal.data_graph;
import rdf.rasqal.query_results;

// This API is partial, as I consider many C functions as internals.

struct QueryHandle;

private extern extern(C) {
    DataGraphHandle* rasqal_new_data_graph_from_data_graph(DataGraphHandle* dg);

    void rasqal_free_query(QueryHandle* query);
    int rasqal_query_add_data_graph(QueryHandle* query, DataGraphHandle* data_graph);
    QueryResultsHandle* rasqal_query_execute(QueryHandle* query);
    int rasqal_query_prepare(QueryHandle* query, const char *query_string, URIHandle* base_uri);
    int rasqal_query_set_store_results(QueryHandle* query, int store_results);
}

struct QueryWithoutFinalize {
    mixin WithoutFinalize!(QueryHandle,
                           QueryWithoutFinalize,
                           Query);
    void addDataGraph(DataGraphWithoutFinalize graph) {
        int res = rasqal_query_add_data_graph(handle,
                                              rasqal_new_data_graph_from_data_graph(graph.handle));
        if(res != 0) throw new RDFException();
    }
    // TODO:
//    void addDataGraphDetach(DataGraphWithoutFinalize graph) {
//        addDataGraph(detach(Graph));
//    }
    void addDataGraphs(DataGraphWithoutFinalize[] graphs) {
        foreach(DataGraphWithoutFinalize dg; graphs) addDataGraph(dg);
    }
    // TODO:
//    void addDataGraphsDetach(DataGraphWithoutFinalize[] graphs) {
//        foreach(DataGraphWithoutFinalize dg; graphs) addDataGraphDetach(dg);
//    }
    // FIXME: Check return type ("without finalize") and the struct after `return` of execute() (here and in Ada):
    QueryResultsWithoutFinalize execute() {
        return QueryResultsWithoutFinalize.fromNonnullHandle(rasqal_query_execute(handle));
    }
    void prepare(string queryString, URIWithoutFinalize baseURI = URIWithoutFinalize.fromHandle(null)) {
        if(rasqal_query_prepare(handle, queryString.toStringz, baseURI.handle) != 0)
            throw new RDFException();
    }
    void setStoreResults(bool store) {
        if(rasqal_query_set_store_results(handle, store) != 0)
            throw new RDFException();
    }
}

struct Query {
    mixin WithFinalize!(QueryHandle,
                        QueryWithoutFinalize,
                        Query,
                        rasqal_free_query);
}

// TODO: Stopped at Set_Wildcard

