module rdf.redland.parser;

import std.typecons;
import std.string;
import std.stdio : File, FILE;
import rdf.auxiliary.handled_record;
import rdf.auxiliary.nullable_string;
import rdf.raptor.iostream;
import rdf.raptor.syntax;
import rdf.redland.memory;
import rdf.redland.world;
import rdf.redland.uri;
import rdf.redland.node;
import rdf.redland.stream;
import rdf.redland.model;

struct ParserHandle;

enum { featureErrorCount   = "http://feature.librdf.org/parser-error-count",
       featureWarningCount = "http://feature.librdf.org/parser-warning-count" }

private extern extern(C) {
    void librdf_free_parser(ParserHandle* parser);
    int librdf_parser_check_name(RedlandWorldHandle* world, const char *name);
    const(char*) librdf_parser_guess_name2(RedlandWorldHandle* world,
                                           const char *mime_type,
                                           const char *buffer,
                                           const char *identifier);
    const(SyntaxDescription*) librdf_parser_get_description(RedlandWorldHandle* world,
                                                            uint counter);
    StreamHandle* librdf_parser_parse_as_stream(ParserHandle* parser,
                                                URIHandle* uri,
                                                URIHandle* base_uri);
    int librdf_parser_parse_into_model(ParserHandle* parser,
                                       URIHandle* uri,
                                       URIHandle* base_uri,
                                       ModelHandle* model);
    StreamHandle* librdf_parser_parse_file_handle_as_stream(ParserHandle* parser,
                                                            FILE *fh,
                                                            int close_fh,
                                                            URIHandle* base_uri);
    int librdf_parser_parse_file_handle_into_model(ParserHandle* parser,
                                                   FILE *fh,
                                                   int close_fh,
                                                   URIHandle *base_uri,
                                                   ModelHandle *model);
    StreamHandle* librdf_parser_parse_counted_string_as_stream(ParserHandle* parser,
                                                               const char *string,
                                                               size_t length,
                                                               URIHandle* base_uri);
    int librdf_parser_parse_counted_string_into_model(ParserHandle* parser,
                                                      const char* string,
                                                      size_t length,
                                                      URIHandle* base_uri,
                                                      ModelHandle* model);
    StreamHandle* librdf_parser_parse_iostream_as_stream(ParserHandle* parser,
                                                         IOStreamHandle* iostream,
                                                         URIHandle* base_uri);
    int librdf_parser_parse_iostream_into_model(ParserHandle* parser,
                                                IOStreamHandle* iostream,
                                                URIHandle* base_uri,
                                                ModelHandle* model);
    NodeHandle* librdf_parser_get_feature(ParserHandle* parser, URIHandle* feature);
    int librdf_parser_set_feature(ParserHandle* parser, URIHandle* feature, NodeHandle* value);
    char* librdf_parser_get_accept_header(ParserHandle* parser);
    ParserHandle* librdf_new_parser(RedlandWorldHandle* world,
                                    const char *name,
                                    const char *mime_type,
                                    URIHandle *type_uri);
}

struct ParserWithoutFinalize {
    mixin WithoutFinalize!(ParserHandle,
                           ParserWithoutFinalize,
                           Parser);
    Stream asStream(URIWithoutFinalize uri, URIWithoutFinalize baseURI) const {
        StreamHandle* h = librdf_parser_parse_as_stream(handle, uri.handle, baseURI.handle);
        return Stream.fromNonnullHandle(h);
    }
    void parseIntoModel(ModelWithoutFinalize model,
                        URIWithoutFinalize uri,
                        URIWithoutFinalize baseURI = URIWithoutFinalize.fromHandle(null))
    {
        if(librdf_parser_parse_into_model(handle, uri.handle, baseURI.handle, model.handle) != 0)
            throw new RDFException();
    }
    Stream parseFileHandleAsStream(File file,
                                   bool close,
                                   URIWithoutFinalize baseURI = URIWithoutFinalize.fromHandle(null))
    {
        StreamHandle* h = librdf_parser_parse_file_handle_as_stream(handle,
                                                                    file.getFP,
                                                                    close,
                                                                    baseURI.handle);
        return Stream.fromNonnullHandle(h);
    }
    /// order of arguments differs of C function
    void parseFileHandleIntoModel(File file,
                                  bool close,
                                  ModelWithoutFinalize model,
                                  URIWithoutFinalize baseURI = URIWithoutFinalize.fromHandle(null))
    {
        int res = librdf_parser_parse_file_handle_into_model(handle,
                                                             file.getFP,
                                                             close,
                                                             baseURI.handle,
                                                             model.handle);
        if(res != 0) throw new RDFException();
    }
    Stream parseStringAsStream(string text,
                               URIWithoutFinalize baseURI = URIWithoutFinalize.fromHandle(null))
        in(!text.empty)
    {
        StreamHandle* h = librdf_parser_parse_counted_string_as_stream(handle,
                                                                       text.ptr,
                                                                       text.length,
                                                                       baseURI.handle);
        return Stream.fromNonnullHandle(h);
    }
    /// order of arguments differs of C function
    void parseStringIntoModel(ModelWithoutFinalize model,
                              string text,
                              URIWithoutFinalize baseURI = URIWithoutFinalize.fromHandle(null))
    {
        int res = librdf_parser_parse_counted_string_into_model(handle,
                                                                text.ptr,
                                                                text.length,
                                                                baseURI.handle,
                                                                model.handle);
        if(res != 0) throw new RDFException();
    }
    Stream parseIOStreamAsStream(IOStream iostream,
                                 URIWithoutFinalize baseURI = URIWithoutFinalize.fromHandle(null))
    {
        StreamHandle* h =
            librdf_parser_parse_iostream_as_stream(handle, iostream.handle, baseURI.handle);
      return Stream.fromNonnullHandle(h);
    }
    /// order of arguments differs of C function
    void parseIOStreamIntoModel(ModelWithoutFinalize model,
                                IOStreamWithoutFinalize iostream,
                                URIWithoutFinalize baseURI = URIWithoutFinalize.fromHandle(null))
    {
        int res = librdf_parser_parse_iostream_into_model(handle,
                                                          iostream.handle,
                                                          baseURI.handle,
                                                          model.handle);
        if(res != 0) throw new RDFException();
    }
    Node getFeature(URIWithoutFinalize feature) const {
        return Node.fromHandle(librdf_parser_get_feature(handle, feature.handle));
    }
    void setFeature(URIWithoutFinalize feature, NodeWithoutFinalize value)
    {
        if(librdf_parser_set_feature(handle, feature.handle, value.handle) != 0)
            throw new RDFException();
    }
    string getAcceptHeader() const {
        char* ptr = librdf_parser_get_accept_header(handle);
        if(!ptr) throw new RDFException();
        scope(exit) librdf_free_memory(ptr);
        return ptr.fromStringz.idup;
    }
    // librdf_parser_get_namespaces_seen_count(),
    // librdf_parser_get_namespaces_seen_prefix(),
    // librdf_parser_get_namespaces_seen_uri()
    // not bound as internals

    // librdf_parser_get_uri_filter() and librdf_parser_set_uri_filter()
}

struct Parser {
    mixin WithFinalize!(ParserHandle,
                        ParserWithoutFinalize,
                        Parser,
                        librdf_free_parser);
    static Parser create(RedlandWorldWithoutFinalize world,
                         string name = "",
                         string mimeType = "",
                         URIWithoutFinalize typeURI = URIWithoutFinalize.fromHandle(null))
    {
        ParserHandle* h = librdf_new_parser(world.handle, name.toStringz,
                                            mimeType.empty ? null : mimeType.ptr,
                                            typeURI.handle);
        return Parser.fromHandle(h);
    }
}

bool parserCheckName(RedlandWorldWithoutFinalize world, string name) {
    return librdf_parser_check_name(world.handle, name.toStringz) != 0;
}

// Order of arguments not the same as in C
string parserGuessName(RedlandWorldWithoutFinalize world,
                       string mimeType,
                       string identifier,
                       Nullable!string buffer = Nullable!string())
{
    const char* result = librdf_parser_guess_name2(world.handle,
                                                   mimeType.empty ? null : mimeType.ptr,
                                                   buffer.myToStringz,
                                                   identifier.empty ? null : identifier.ptr);
    return result ? result.fromStringz.idup : "";
}

ref const(SyntaxDescription) getParserDescription (const RedlandWorldWithoutFinalize world, uint counter)
{
    return *librdf_parser_get_description(world.handle, counter);
}

struct ParsersEnumerate {
private:
    RedlandWorldWithoutFinalize _world;
    uint counter = 0;
public:
    this(RedlandWorldWithoutFinalize world) {
        _world = world;
    }
    @property uint position() const { return counter; }
    @property bool empty() const {
        return !librdf_parser_get_description(_world.handle, counter);
    }
    @property ref const(SyntaxDescription) front() const
        in(!empty)
    {
        return getParserDescription(_world, counter);
    }
    void popFront()
        in(!empty)
    {
        ++counter;
    }
}

