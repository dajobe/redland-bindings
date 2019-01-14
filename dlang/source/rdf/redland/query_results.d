module rdf.redland.query_results;

import rdf.auxiliary.handled_record;

struct QueryResultsHandle;

private extern extern(C) {
    void librdf_free_query_results(QueryResultsHandle* query_results);
}

struct QueryResultsWithoutFinalize {
    mixin WithoutFinalize!(QueryResultsHandle,
                           QueryResultsWithoutFinalize,
                           QueryResults);
}

struct QueryResults {
    mixin WithFinalize!(QueryResultsHandle,
                        QueryResultsWithoutFinalize,
                        QueryResults,
                        librdf_free_query_results);
}

// TODO: Stopped at As_Stream
