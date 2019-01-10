module rdf.rasqal.query_results;

import std.string;
import rdf.auxiliary.handled_record;
import rdf.raptor.statement;
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
    int rasqal_query_results_get_bindings_count(QueryResultsHandle* query_results);
    int rasqal_query_results_get_boolean(QueryResultsHandle* query_results);
    int rasqal_query_results_get_count(QueryResultsHandle* query_results);
    StatementHandle* rasqal_query_results_get_triple(QueryResultsHandle* query_results);
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
    @property uint bindingsCount() {
        int count = rasqal_query_results_get_bindings_count(handle);
        if(count < 0) throw new RDFException();
        return count;
    }
    @property bool boolean() {
        int value = rasqal_query_results_get_boolean(handle);
        if(value < 0) throw new RDFException();
        return value != 0;
    }
    @property uint currentCount() {
        int value = rasqal_query_results_get_count(handle);
        if(value < 0) throw new RDFException();
        return value;
    }
    // TODO
    //@property QueryWithoutFinalize query()
    @property StatementWithoutFinalize triple() {
        return StatementWithoutFinalize.fromNonnullHandle(rasqal_query_results_get_triple(handle));
    }
    // Deliberately not implemented:
    // getRowByOffset(uint offset)
}

struct QueryResults {
    mixin WithFinalize!(QueryResultsHandle,
                        QueryResultsWithoutFinalize,
                        QueryResults,
                        rasqal_free_query_results);
}

// TODO: Stopped at Next

