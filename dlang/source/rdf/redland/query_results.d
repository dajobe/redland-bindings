module rdf.redland.query_results;

import std.string;
import std.stdio: File, FILE;
import rdf.auxiliary.handled_record;
import rdf.redland.memory;
import rdf.redland.uri;
import rdf.redland.node;
import rdf.redland.stream;

struct QueryResultsHandle;

private extern extern(C) {
    void librdf_free_query_results(QueryResultsHandle* query_results);
    StreamHandle* librdf_query_results_as_stream(QueryResultsHandle* query_results);
    int librdf_query_results_get_count(QueryResultsHandle* query_results);

    int librdf_query_results_next(QueryResultsHandle* query_results);
    int librdf_query_results_finished(QueryResultsHandle *query_results);
    NodeHandle* librdf_query_results_get_binding_value(QueryResultsHandle* query_results,
                                                       int offset);
    const(char*) librdf_query_results_get_binding_name(QueryResultsHandle* query_results,
                                                       int offset);
    NodeHandle* librdf_query_results_get_binding_value_by_name(QueryResultsHandle* query_results,
                                                               const char *name);
    int librdf_query_results_get_bindings_count(QueryResultsHandle* query_results);
    char* librdf_query_results_to_counted_string2(QueryResultsHandle* query_results,
                                                  const char *name,
                                                  const char *mime_type,
                                                  URIHandle* format_uri,
                                                  URIHandle* base_uri,
                                                  size_t *length_p);
    int librdf_query_results_to_file_handle2(QueryResultsHandle* query_results,
                                             FILE *handle,
                                             const char *name,
                                             const char *mime_type,
                                             URIHandle* format_uri,
                                             URIHandle* base_uri);
    int librdf_query_results_is_bindings(QueryResultsHandle* query_results);
    int librdf_query_results_is_boolean(QueryResultsHandle* query_results);
    int librdf_query_results_is_graph(QueryResultsHandle* query_results);
    int librdf_query_results_is_syntax(QueryResultsHandle* query_results);
    int librdf_query_results_get_boolean(QueryResultsHandle* query_results);
    // I was lazy to implement query_results_formatter and related functions
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
    void next()
         in(isBindings || isGraph)
    {
        int res = librdf_query_results_next(handle);
        // Check is done by Finished procedure, not here
        //if(handle != 0) throw new RDFException();
    }
    bool finished()
         in(isBindings || isGraph)
    {
        return librdf_query_results_finished(handle) != 0;
    }
    @property uint bindingsCount() {
        int result = librdf_query_results_get_bindings_count(handle);
        if(result < 0) throw new RDFException();
        return result;
    }
    Node getBindingValue(uint index) {
        return Node.fromNonnullHandle(librdf_query_results_get_binding_value(handle, index));
    }
    string getBindingName(uint index) {
        const char* ptr = librdf_query_results_get_binding_name(handle, index);
        if(!ptr) throw new RDFException();
        return ptr.fromStringz.idup;
    }
    Node getBindingValueByName(string name) {
        NodeHandle* handle =
            librdf_query_results_get_binding_value_by_name(handle, name.toStringz);
        return Node.fromNonnullHandle(handle);
    }
    string toString(string name = "",
                    string mimeType = "",
                    URIWithoutFinalize formatURI = URIWithoutFinalize.fromHandle(null),
                    URIWithoutFinalize baseURI = URIWithoutFinalize.fromHandle(null))
    {
        size_t length;
        char* ptr =
            librdf_query_results_to_counted_string2(handle,
                                                    (name == "" ? null : name.ptr),
                                                    (mimeType == "" ? null : mimeType.ptr),
                                                    formatURI.handle,
                                                    baseURI.handle,
                                                    &length);
        if(!ptr) throw new RDFException();
        scope(exit) librdf_free_memory(ptr);
        return ptr[0..length].idup;
    }
    void toFileHandle (File file,
                       string name = "",
                       string mimeType = "",
                       URIWithoutFinalize formatURI = URIWithoutFinalize.fromHandle(null),
                       URIWithoutFinalize baseURI = URIWithoutFinalize.fromHandle(null))
    {
        int result =
            librdf_query_results_to_file_handle2(handle,
                                                 file.getFP,
                                                 (name == "" ? null : name.ptr),
                                                 (mimeType == "" ? null : mimeType.ptr),
                                                 formatURI.handle,
                                                 baseURI.handle);
        if(result != 0) throw new RDFException();
    }
    // librdf_query_results_to_file2() is wrong: http://bugs.librdf.org/mantis/view.php?id=639
    @property bool isBindings() { return librdf_query_results_is_bindings(handle) != 0; }
    @property bool isBoolean() { return librdf_query_results_is_boolean(handle) != 0; }
    @property bool isGraph() { return librdf_query_results_is_graph(handle) != 0; }
    @property bool isSyntax() { return librdf_query_results_is_syntax(handle) != 0; }
    @property bool boolean() {
        int result = librdf_query_results_get_boolean(handle);
        if(result < 0) throw new RDFException();
        return result != 0;
    }
}

struct QueryResults {
    mixin WithFinalize!(QueryResultsHandle,
                        QueryResultsWithoutFinalize,
                        QueryResults,
                        librdf_free_query_results);
}

/// Do not create more than one iterator for the same query results object.
struct BindingsIterator {
private:
    QueryResultsWithoutFinalize _results;
public:
    this(QueryResultsWithoutFinalize results) {
        _results = results;
    }
    @property ref BindingsIterator front() { return this; }
    @property bool empty() {
        return !_results.finished;
    }
    void popFront()
        in { assert(!empty); }
        do { _results.next(); }
    string getBindingName(uint index) {
        return _results.getBindingName(index);
    }
    Node getBindingValueByName(string name) {
        return _results.getBindingValueByName(name);
    }
}

struct VariablesIterator {
private:
    QueryResultsWithoutFinalize _results;
    uint index = 0;
public:
    this(QueryResultsWithoutFinalize results) {
        _results = results;
    }
    @property string front() { return _results.getBindingName(index); }
    @property bool empty() {
        return index < _results.bindingsCount;
    }
    void popFront()
        in { assert(!empty); }
        do { ++index; }
}

