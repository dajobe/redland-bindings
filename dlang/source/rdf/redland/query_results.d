module rdf.redland.query_results;

import rdf.auxiliary.handled_record;
import rdf.redland.stream;

struct QueryResultsHandle;

private extern extern(C) {
    void librdf_free_query_results(QueryResultsHandle* query_results);
    StreamHandle* librdf_query_results_as_stream(QueryResultsHandle* query_results);
    int librdf_query_results_get_count(QueryResultsHandle* query_results);
}

struct QueryResultsWithoutFinalize {
    mixin WithoutFinalize!(QueryResultsHandle,
                           QueryResultsWithoutFinalize,
                           QueryResults);
    Stream asStream() {
        return Stream.fromNonnullHandle(librdf_query_results_as_stream(handle));
    }
    @property uint currentCount() {
        return librdf_query_results_get_count(handle);
    }
}

struct QueryResults {
    mixin WithFinalize!(QueryResultsHandle,
                        QueryResultsWithoutFinalize,
                        QueryResults,
                        librdf_free_query_results);
}

// TODO: Stopped at Next
