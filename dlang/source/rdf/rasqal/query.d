module rdf.rasqal.query;

import rdf.auxiliary.handled_record;
import rdf.rasqal.data_graph;

// This API is partial, as I consider many C functions as internals.

struct QueryHandle;

private extern extern(C) {
    DataGraphHandle* rasqal_new_data_graph_from_data_graph(DataGraphHandle* dg);

    void rasqal_free_query(QueryHandle* query);
    int rasqal_query_add_data_graph(QueryHandle* query, DataGraphHandle* data_graph);
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
//        foreach(auto dg; graphs) addDataGraphDetach(dg);
//    }
}

struct Query {
    mixin WithFinalize!(QueryHandle,
                        QueryWithoutFinalize,
                        Query,
                        rasqal_free_query);
}

// TODO: Stopped at Execute

