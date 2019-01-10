module rdf.rasqal.query_results;

import std.string;
import rdf.auxiliary.handled_record;
import rdf.rasqal.literal;

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
    const(char*) rasqal_query_results_get_binding_name(QueryResultsHandle* query_results,
                                                       int offset);
    LiteralHandle* rasqal_query_results_get_binding_value(QueryResultsHandle* query_results,
                                                          int offset);
    LiteralHandle* rasqal_query_results_get_binding_value_by_name(QueryResultsHandle* query_results,
                                                                  const char *name);
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
    string getBindingName(uint offset) {
        const char* ptr = rasqal_query_results_get_binding_name(handle, int(offset));
        if(!ptr) throw new RDFException();
        return ptr.fromStringz.idup;
    }
    LiteralWithoutFinalize getBindingValue(uint offset) {
        return LiteralWithoutFinalize.fromNonnullHandle(
            rasqal_query_results_get_binding_value(handle, int(offset)));
    }
    LiteralWithoutFinalize getBindingValueByName(string name) {
        return LiteralWithoutFinalize.fromNonnullHandle(
            rasqal_query_results_get_binding_value_by_name(handle, name.toStringz));
    }
    // rasqal_query_results_get_bindings() deliberately not implemented.
    // Use iterators instead.
}

struct QueryResults {
    mixin WithFinalize!(QueryResultsHandle,
                        QueryResultsWithoutFinalize,
                        QueryResults,
                        rasqal_free_query_results);
}

// TODO: Stopped at Get_Bindings_Count

