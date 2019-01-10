module rdf.rasqal.query_results;

import rdf.auxiliary.handled_record;

struct QueryResultsHandle;

enum QueryResultsType { Bindings,
                        Boolean,
                        Graph,
                        Syntax,
                        Unknown }

private extern extern(C) {
    void rasqal_free_query_results(QueryResultsHandle* query_results);
    QueryResultsType rasqal_query_results_get_type(QueryResultsHandle* query_results);
    int rasqal_query_results_is_bindings(QueryResultsHandle* query_results);
    int rasqal_query_results_is_boolean(QueryResultsHandle* query_results);
    int rasqal_query_results_is_graph(QueryResultsHandle* query_results);
    int rasqal_query_results_is_syntax(QueryResultsHandle* query_results);
    int rasqal_query_results_finished(QueryResultsHandle* query_results);
}

struct QueryResultsWithoutFinalize {
    mixin WithoutFinalize!(QueryResultsHandle,
                           QueryResultsWithoutFinalize,
                           QueryResults);
    @property QueryResultsType type() {
        return rasqal_query_results_get_type(handle);
    }
    @property bool isBindings() {
        return rasqal_query_results_is_bindings(handle) != 0;
    }
    @property bool isBoolean() {
        return rasqal_query_results_is_boolean(handle) != 0;
    }
    @property bool isGraph() {
        return rasqal_query_results_is_graph(handle) != 0;
    }
    @property bool isSyntax() {
        return rasqal_query_results_is_syntax(handle) != 0;
    }
    // function addRow is deliberately not implemented
    bool finished()
        in(isBindings() || isGraph())
    {
        return rasqal_query_results_finished(handle) != 0;
    }
}

struct QueryResults {
    mixin WithFinalize!(QueryResultsHandle,
                        QueryResultsWithoutFinalize,
                        QueryResults,
                        rasqal_free_query_results);
}

// TODO: Stopped at Get_Binding_Name

