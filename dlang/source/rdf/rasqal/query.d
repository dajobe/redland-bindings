module rdf.rasqal.query;

import std.typecons;
import std.string;
import rdf.auxiliary.handled_record;
import rdf.auxiliary.nullable_string;
import rdf.raptor.uri;
import rdf.raptor.iostream;
import rdf.rasqal.memory;
import rdf.rasqal.world;
import rdf.rasqal.features;
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
    void rasqal_query_set_wildcard(QueryHandle* query, int wildcard);
    int rasqal_query_write(IOStreamHandle* iostr,
                           QueryHandle* query,
                           URIHandle* format_uri,
                           URIHandle* base_uri);
    int rasqal_query_iostream_write_escaped_counted_string(QueryHandle* query,
                                                           IOStreamHandle* iostr,
                                                           const char *string,
                                                           size_t len);
    char* rasqal_query_escape_counted_string(QueryHandle* query,
                                             const char *string,
                                             size_t len,
                                             size_t *output_len_p);
    int rasqal_query_set_feature(QueryHandle* query, FeatureType feature, int value);
    int rasqal_query_set_feature_string(QueryHandle* query, FeatureType feature, const char *value);
    int rasqal_query_get_feature(QueryHandle* query, FeatureType feature);
    const(char*) rasqal_query_get_feature_string(QueryHandle* query, FeatureType feature);
    QueryResultsType rasqal_query_get_result_type(QueryHandle* query);
    QueryHandle* rasqal_new_query(RasqalWorldHandle* world, const char *name, const char *uri);
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
        foreach(dg; graphs) addDataGraph(dg);
    }
    // TODO:
//    void addDataGraphsDetach(DataGraphWithoutFinalize[] graphs) {
//        foreach(dg; graphs) addDataGraphDetach(dg);
//    }
    QueryResults execute() {
        return QueryResults.fromNonnullHandle(rasqal_query_execute(handle));
    }
    void prepare(string queryString, URIWithoutFinalize baseURI = URIWithoutFinalize.fromHandle(null)) {
        if(rasqal_query_prepare(handle, queryString.toStringz, baseURI.handle) != 0)
            throw new RDFException();
    }
    void setStoreResults(bool store) {
        if(rasqal_query_set_store_results(handle, store) != 0)
            throw new RDFException();
    }
    void setWilcard(bool wilcard) {
        rasqal_query_set_store_results(handle, wilcard);
    }
    void writeQuery(IOStreamWithoutFinalize stream,
                    URIWithoutFinalize formatURI,
                    URIWithoutFinalize baseURI)
    {
        if(rasqal_query_write(stream.handle, handle, formatURI.handle, baseURI.handle) != 0)
            throw new RDFException();
    }
    // Is it really useful? Maybe remove from public API?
    void writeEscapedString(IOStreamWithoutFinalize stream, string str) {
        int res = rasqal_query_iostream_write_escaped_counted_string(handle,
                                                                     stream.handle,
                                                                     str.ptr,
                                                                     str.length);
        if(res != 0) throw new RDFException();
    }
    string escapeString(string str) {
        size_t outLen;
        char* result = rasqal_query_escape_counted_string(handle, str.ptr, str.length, &outLen);
        if(!result) throw new RDFException();
        scope(exit) rasqal_free_memory(result);
        return result[0..outLen].idup;
    }
    void setFeature(FeatureType feature, uint value) {
        if(rasqal_query_set_feature(handle, feature, value) != 0)
            throw new RDFException();
    }
    void setFeature(FeatureType feature, string value) {
        if(rasqal_query_set_feature_string(handle, feature, value.toStringz) != 0)
            throw new RDFException();
    }
    uint getFeatureInt(FeatureType feature) {
        int result = rasqal_query_get_feature(handle, feature);
        if(result < 0) throw new RDFException();
        return result;
    }
    string getFeatureString(FeatureType feature) {
        const char* result = rasqal_query_get_feature_string(handle, feature);
        if(!result) throw new RDFException();
        scope(exit) rasqal_free_memory(cast(char*)result);
        return result.fromStringz.idup;
    }
    QueryResultsType getResultType() {
        QueryResultsType result = rasqal_query_get_result_type(handle);
        if(result == QueryResultsType.unknown) throw new RDFException();
        return result;
    }
    // Deliberately not implemented
    // function getUpdateOperation
    // function getUpdateOperationsSequence
}

struct Query {
    mixin WithFinalize!(QueryHandle,
                        QueryWithoutFinalize,
                        Query,
                        rasqal_free_query);
    static Query create(RasqalWorldWithoutFinalize world, Nullable!string name, Nullable!string uri) {
        return fromNonnullHandle(rasqal_new_query(world.handle, name.myToStringz, uri.myToStringz));
    }
}

unittest {
    // see main.d instead
}

