module rdf.raptor.parser;

import std.string;
import std.stdio : FILE, File;
import rdf.auxiliary.handled_record;
import rdf.auxiliary.user;
import rdf.raptor.uri;
import rdf.raptor.statement;
import rdf.raptor.namespace;
import rdf.raptor.log;
import rdf.raptor.iostream;

struct ParserHandle;

private extern extern(C) {
    void raptor_free_parser(ParserHandle* parser);
    alias raptor_graph_mark_handler = void function(void* user_data,
                                                    URIHandle* graph,
                                                    int flags);
    alias raptor_namespace_handler = void function(void* user_data, NamespaceHandle* nspace);
    alias raptor_statement_handler = void function(void* user_data, StatementHandle *statement);
    alias raptor_uri_filter_func = int function(void* user_data, URIHandle* uri);
    void raptor_parser_set_statement_handler(ParserHandle* parser,
                                             void* user_data,
                                             raptor_statement_handler handler);
    void raptor_parser_set_graph_mark_handler(ParserHandle* parser,
                                              void* user_data,
                                              raptor_graph_mark_handler handler);
    void raptor_parser_set_namespace_handler(ParserHandle* parser,
                                             void* user_data,
                                             raptor_namespace_handler handler);
    void raptor_parser_set_uri_filter(ParserHandle* parser,
                                      raptor_uri_filter_func filter,
                                      void* user_data);
    LocatorHandle* raptor_parser_get_locator(ParserHandle* rdf_parser);
    void raptor_parser_parse_abort(ParserHandle* rdf_parser);
    int raptor_parser_parse_chunk(ParserHandle* rdf_parser,
                                  const char *buffer,
                                  size_t len,
                                  int is_end);
    int raptor_parser_parse_file(ParserHandle* rdf_parser, URIHandle* uri, URIHandle* base_uri);
    int raptor_parser_parse_file_stream(ParserHandle* rdf_parser,
                                        FILE *stream,
                                        const char *filename,
                                        URIHandle* base_uri);
    int raptor_parser_parse_iostream(ParserHandle* rdf_parser,
                                     IOStreamHandle* iostr,
                                     URIHandle* base_uri);
    int raptor_parser_parse_start(ParserHandle* rdf_parser, URIHandle* uri);
    int raptor_parser_parse_uri(ParserHandle* rdf_parser, URIHandle* uri, URIHandle* base_uri);
    int raptor_parser_parse_uri_with_connection(ParserHandle* rdf_parser,
                                                URIHandle* uri,
                                                URIHandle* base_uri,
                                                void *connection);
}

enum GraphMarkFlags { Graph_Mark_Start = 1, Graph_Mark_Declared = 2 }

struct ParserWithoutFinalize {
    mixin WithoutFinalize!(ParserHandle,
                           ParserWithoutFinalize,
                           Parser);
    @property LocatorWithoutFinalize locator() {
        return LocatorWithoutFinalize.fromHandle(raptor_parser_get_locator(handle));
    }
    void parseAbort() {
        raptor_parser_parse_abort(handle);
    }
    void parseChunk(string buffer, bool isEnd) {
        if(raptor_parser_parse_chunk(handle, buffer.ptr, buffer.length, isEnd) != 0)
            throw new RDFException();
    }
    void parseFile(URIWithoutFinalize uri, URIWithoutFinalize baseURI = URIWithoutFinalize.fromHandle(null)) {
        if(raptor_parser_parse_file(handle, uri.handle, baseURI.handle) != 0)
            throw new RDFException();
    }
    void parseStdin(URIWithoutFinalize baseURI = URIWithoutFinalize.fromHandle(null)) {
        parseFile(URIWithoutFinalize.fromHandle(null), baseURI);
    }
    void parseFileStream(File stream, string filename, URIWithoutFinalize baseURI) {
        immutable int res = raptor_parser_parse_file_stream(handle,
                                                            stream.getFP,
                                                            filename.toStringz,
                                                            baseURI.handle);
        if(res != 0) throw new RDFException();
    }
    void parseIOStream(IOStreamWithoutFinalize stream, URIWithoutFinalize baseURI) {
        if(raptor_parser_parse_iostream(handle, stream.handle, baseURI.handle) != 0)
            throw new RDFException();
    }
    /// TODO: More detailed exit status handling (also in Ada)
    void parseStart(URIWithoutFinalize uri) {
        if(raptor_parser_parse_start(handle, uri.handle) != 0)
            throw new RDFException();
    }
    void parseURI(URIWithoutFinalize uri, URIWithoutFinalize baseURI = URIWithoutFinalize.fromHandle(null)) {
        if(raptor_parser_parse_uri(handle, uri.handle, baseURI.handle) != 0)
            throw new RDFException();
    }
    void parseURI(URIWithoutFinalize uri,
                  URIWithoutFinalize baseURI = URIWithoutFinalize.fromHandle(null),
                  void* connection = null)
    {
        if(raptor_parser_parse_uri_with_connection(handle, uri.handle, baseURI.handle, connection) != 0)
            throw new RDFException();
    }
}

struct Parser {
    mixin WithFinalize!(ParserHandle,
                        ParserWithoutFinalize,
                        Parser,
                        raptor_free_parser);
}

class UserParser : UserObject!Parser {
    void initializeAllCallbacks() {
        initializeGraphMarkHandler();
        initializeStatementHandler();
        initializeNamespaceHandler();
        initializeURIFilter();
    }
    void initializeGraphMarkHandler() {
        raptor_parser_set_graph_mark_handler(record.handle,
                                             context,
                                             &raptor_graph_mark_handler_impl);
    }

    void initializeStatementHandler() {
        raptor_parser_set_statement_handler(record.handle,
                                            context,
                                            &raptor_statement_handler_impl);
    }

    void initializeNamespaceHandler() {
        raptor_parser_set_namespace_handler(record.handle,
                                            context,
                                            &raptor_namespace_handler_impl);
    }

    void initializeURIFilter() {
        raptor_parser_set_uri_filter(record.handle, &raptor_uri_filter_impl, context);
    }

    void statementHandler(StatementWithoutFinalize statement) { }
    void graphMarkHandler(URIWithoutFinalize uri, GraphMarkFlags flags) { }
    void namespaceHandler(NamespaceWithoutFinalize namespace) { }
    bool uriFilter(URIWithoutFinalize uri) {
        return true;
    }
    private static extern(C) {
        void raptor_statement_handler_impl(void* data, StatementHandle* statement) {
            (cast(UserParser*)data).statementHandler(StatementWithoutFinalize.fromNonnullHandle(statement));
        }
        void raptor_graph_mark_handler_impl(void* data, URIHandle* uri, int Flags) {
            (cast(UserParser*)data).graphMarkHandler(URIWithoutFinalize.fromNonnullHandle(uri), cast(GraphMarkFlags)Flags);
        }
        void raptor_namespace_handler_impl(void* data, NamespaceHandle* NS) {
            (cast(UserParser*)data).namespaceHandler(NamespaceWithoutFinalize.fromNonnullHandle(NS));
        }
        int raptor_uri_filter_impl(void* data, URIHandle* uri) {
            return (cast(UserParser*)data).uriFilter(URIWithoutFinalize.fromNonnullHandle(uri));
        }
    }
}

// TODO: Stopped at Get_Graph
