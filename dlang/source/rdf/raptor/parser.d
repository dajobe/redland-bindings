module rdf.raptor.parser;

// import std.string;
import rdf.auxiliary.handled_record;
import rdf.auxiliary.user;
import rdf.raptor.uri;
import rdf.raptor.statement;
import rdf.raptor.namespace;

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
}

enum GraphMarkFlags { Graph_Mark_Start = 1, Graph_Mark_Declared = 2 }

struct ParserWithoutFinalize {
    mixin WithoutFinalize!(ParserHandle,
                           ParserWithoutFinalize,
                           Parser);
}

struct Parser {
    mixin WithFinalize!(ParserHandle,
                        ParserWithoutFinalize,
                        Parser,
                        raptor_free_parser);
}

class UserParser : UserObject!Parser {
    void Initialize_All_Callbacks() {
        Initialize_Graph_Mark_Handler();
        Initialize_Statement_Handler();
        Initialize_Namespace_Handler();
        Initialize_URI_Filter();
    }
    void Initialize_Graph_Mark_Handler() {
        raptor_parser_set_graph_mark_handler(record.handle,
                                             context,
                                             &raptor_graph_mark_handler_impl);
    }

    void Initialize_Statement_Handler() {
        raptor_parser_set_statement_handler(record.handle,
                                            context,
                                            &raptor_statement_handler_impl);
    }

    void Initialize_Namespace_Handler() {
        raptor_parser_set_namespace_handler(record.handle,
                                            context,
                                            &raptor_namespace_handler_impl);
    }

    void Initialize_URI_Filter() {
        raptor_parser_set_uri_filter(record.handle, &raptor_uri_filter_impl, context);
    }

    void Graph_Mark_Handler(URIWithoutFinalize uri, GraphMarkFlags flags) { }
    void Statement_Handler(StatementWithoutFinalize statement) { }
    void Namespace_Handler(NamespaceWithoutFinalize namespace) { }
    bool URI_Filter(URIWithoutFinalize uri) {
        return true;
    }
    private static extern(C) {
        void raptor_statement_handler_impl(void* data, StatementHandle* statement) {
            (cast(UserParser*)data).Statement_Handler(StatementWithoutFinalize.fromNonnullHandle(statement));
        }
        void raptor_graph_mark_handler_impl(void* data, URIHandle* uri, int Flags) {
            (cast(UserParser*)data).Graph_Mark_Handler(URIWithoutFinalize.fromNonnullHandle(uri), cast(GraphMarkFlags)Flags);
        }
        void raptor_namespace_handler_impl(void* data, NamespaceHandle* NS) {
            (cast(UserParser*)data).Namespace_Handler(NamespaceWithoutFinalize.fromNonnullHandle(NS));
        }
        int raptor_uri_filter_impl(void* data, URIHandle* uri) {
            return (cast(UserParser*)data).URI_Filter(URIWithoutFinalize.fromNonnullHandle(uri));
        }
    }
}

// TODO: Stopped at Graph_Mark_Flags
