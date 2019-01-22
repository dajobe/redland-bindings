module rdf.rasqal.query_results;

import std.string;
import rdf.config;
import rdf.auxiliary.versions;
import rdf.auxiliary.handled_record;
import rdf.raptor.uri;
import rdf.raptor.statement;
import rdf.raptor.iostream;
import rdf.rasqal.world;
import rdf.rasqal.literal;
import rdf.rasqal.query;

struct QueryResultsHandle;

private struct MyDummy;

enum QueryResultsType { bindings,
                        boolean,
                        graph,
                        syntax,
                        unknown }

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
    int rasqal_query_results_next(QueryResultsHandle* query_results);
    int rasqal_query_results_next_triple(QueryResultsHandle* query_results);
    int rasqal_query_results_read(IOStreamHandle* iostr,
                                  QueryResultsHandle* results,
                                  const char *name,
                                  const char *mime_type,
                                  URIHandle* format_uri,
                                  URIHandle* base_uri);
    int rasqal_query_results_write(IOStreamHandle* iostr,
                                   QueryResultsHandle* results,
                                   const char *name,
                                   const char *mime_type,
                                   URIHandle* format_uri,
                                   URIHandle* base_uri);
    const(char*) rasqal_query_results_type_label(QueryResultsType type);
    int rasqal_query_results_rewind(QueryResultsHandle* results);
    //QueryResultsHandle* rasqal_new_query_results(RasqalWorldHandle* world,
    //                                             QueryHandle* query,
    //                                             QueryResultsType type,
    //                                             MyDummy* vars_table);
    QueryResultsHandle* rasqal_new_query_results2(RasqalWorldHandle* world,
                                                  QueryHandle* query,
                                                  QueryResultsType type);
    QueryHandle* rasqal_query_results_get_query(QueryResultsHandle* query_results);
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
    string getBindingName(uint offset)
        in(isBindings)
    {
        const char* ptr = rasqal_query_results_get_binding_name(handle, int(offset));
        if(!ptr) throw new RDFException();
        return ptr.fromStringz.idup;
    }
    LiteralWithoutFinalize getBindingValue(uint offset)
        in(isBindings)
    {
        return LiteralWithoutFinalize.fromNonnullHandle(
            rasqal_query_results_get_binding_value(handle, int(offset)));
    }
    LiteralWithoutFinalize getBindingValueByName(string name)
        in(isBindings)
    {
        return LiteralWithoutFinalize.fromNonnullHandle(
            rasqal_query_results_get_binding_value_by_name(handle, name.toStringz));
    }
    // rasqal_query_results_get_bindings() deliberately not implemented.
    // Use iterators instead.
    @property uint bindingsCount()
        in(isBindings)
    {
        int count = rasqal_query_results_get_bindings_count(handle);
        if(count < 0) throw new RDFException();
        return count;
    }
    @property bool boolean()
        in(isBoolean)
    {
        int value = rasqal_query_results_get_boolean(handle);
        if(value < 0) throw new RDFException();
        return value != 0;
    }
    @property uint currentCount() {
        int value = rasqal_query_results_get_count(handle);
        if(value < 0) throw new RDFException();
        return value;
    }
    @property QueryWithoutFinalize query() {
        return QueryWithoutFinalize.fromNonnullHandle(rasqal_query_results_get_query(handle));
    }
    @property StatementWithoutFinalize triple()
        in(isGraph)
    {
        return StatementWithoutFinalize.fromNonnullHandle(rasqal_query_results_get_triple(handle));
    }
    // Deliberately not implemented:
    // getRowByOffset(uint offset)
    void next() {
        int res = rasqal_query_results_next(handle);
        //if(res != 0) throw new RDFException(); // Check is done by Finished procedure, not here
    }
    void nextTriple() {
        int res = rasqal_query_results_next_triple(handle);
        //if(res != 0) throw new RDFException(); // Check is done by Finished procedure, not here
    }
    void read(IOStreamWithoutFinalize stream,
              string formatName,
              string mimeType,
              URIWithoutFinalize formatURI,
              URIWithoutFinalize baseURI)
    {
        int res = rasqal_query_results_read(stream.handle,
                                            handle,
                                            formatName == "" ? null : formatName.toStringz,
                                            mimeType == "" ? null : mimeType.toStringz,
                                            formatURI.handle,
                                            baseURI.handle);
        if(res != 0) throw new RDFException();
    }
    void write(IOStreamWithoutFinalize stream,
               string formatName,
               string mimeType,
               URIWithoutFinalize formatURI,
               URIWithoutFinalize baseURI)
    {
        int res = rasqal_query_results_write(stream.handle,
                                             handle,
                                             formatName == "" ? null : formatName.toStringz,
                                             mimeType == "" ? null : mimeType.toStringz,
                                             formatURI.handle,
                                             baseURI.handle);
        if(res != 0) throw new RDFException();
    }
    void rewind() {
        if(rasqal_query_results_rewind(handle) != 0)
            throw new RDFException();
    }
}

struct QueryResults {
    mixin WithFinalize!(QueryResultsHandle,
                        QueryResultsWithoutFinalize,
                        QueryResults,
                        rasqal_free_query_results);
    static QueryResults create(RasqalWorldWithoutFinalize world,
                               QueryWithoutFinalize query,
                               QueryResultsType type)
    {
        return fromNonnullHandle(rasqal_new_query_results2(world.handle, query.handle, type));
    }
    static if(Version(rasqalVersionFeatures) >= Version("0.9.33")) {
        private extern extern(C)
        QueryResultsHandle* rasqal_new_query_results_from_string(RasqalWorldHandle* world,
                                                                 QueryResultsType type,
                                                                 URIHandle* base_uri,
                                                                 const char* string,
                                                                 size_t string_len);
        static create(RasqalWorldWithoutFinalize world,
                      QueryResultsType type,
                      URITypeWithoutFinalize baseURI,
                      string value)
        {
            return QueryResults.fromNonnullHandle(
                rasqal_new_query_results_from_string(world.handle,
                                                     type,
                                                     baseURI.handle,
                                                     value.ptr, value.length));
        }
    }
}

string typeLabel(QueryResultsType type) {
    const char* ptr = rasqal_query_results_type_label(type);
    if(!ptr) throw new RDFException();
    return ptr.fromStringz.idup;
}

struct QueryResultsRange {
private:
    QueryResultsWithoutFinalize obj;
public:
    this(QueryResultsWithoutFinalize obj) {
        this.obj = obj;
    }
    @property bool empty() { return obj.finished(); }
    @property QueryResultsRange front() { return this; } // return itself
    void popFront() { obj.next(); }
    string getBindingName(uint offset) {
        return obj.getBindingName(offset);
    }
    LiteralWithoutFinalize getBindingValue(uint offset) {
        return obj.getBindingValue(offset);
    }
    LiteralWithoutFinalize getBindingValueByName(string name) {
        return obj.getBindingValueByName(name);
    }
    void rewind() { obj.rewind(); }
}

struct QueryResultsTriplesRange {
private:
    QueryResultsWithoutFinalize obj;
public:
    this(QueryResultsWithoutFinalize obj) {
        this.obj = obj;
    }
    @property bool empty() { return obj.finished(); }
    @property StatementWithoutFinalize front() { return triple(); }
    void popFront() { obj.nextTriple(); }
    StatementWithoutFinalize triple() { return obj.triple(); }
    void rewind() { obj.rewind(); }
}

struct VariablesRange {
private:
    QueryResultsWithoutFinalize obj;
    uint count = 0;
public:
    this(QueryResultsWithoutFinalize obj) {
        this.obj = obj;
    }
    @property bool empty() { return count == obj.bindingsCount; }
    @property string front() { return name; }
    void popFront() { ++count; }
    @property string name() { return obj.getBindingName(count); }
}

