module rdf.raptor.serializer;

import std.string;
import std.stdio : FILE, File;
import rdf.auxiliary.handled_record;
import rdf.raptor.world;
import rdf.raptor.uri;
import rdf.raptor.statement;
import rdf.raptor.namespace;
import rdf.raptor.iostream;
import rdf.raptor.syntax;
import rdf.raptor.log;
import rdf.raptor.options;

struct SerializerHandle;

private extern extern(C) {
    void raptor_free_serializer(SerializerHandle* rdf_serializer);
    int raptor_serializer_start_to_iostream(SerializerHandle* rdf_serializer,
                                            URIHandle* uri,
                                            IOStreamHandle* iostream);
    int raptor_serializer_start_to_filename(SerializerHandle* rdf_serializer,
                                            const char *filename);
    int raptor_serializer_start_to_file_handle(SerializerHandle* rdf_serializer,
                                               URIHandle* uri,
                                               FILE *fh);
    int raptor_serializer_set_namespace(SerializerHandle* rdf_serializer,
                                        URIHandle* uri,
                                        const char *prefix);
    int raptor_serializer_set_namespace_from_namespace(SerializerHandle* rdf_serializer,
                                                       NamespaceHandle* nspace);
    int raptor_serializer_serialize_statement(SerializerHandle* rdf_serializer,
                                              StatementHandle* statement);
    int raptor_serializer_serialize_end(SerializerHandle* rdf_serializer);
    int raptor_serializer_flush(SerializerHandle* rdf_serializer);
    const(SyntaxDescription*) raptor_serializer_get_description(SerializerHandle* rdf_serializer);
    IOStreamHandle* raptor_serializer_get_iostream(SerializerHandle* serializer);
    LocatorHandle* raptor_serializer_get_locator(SerializerHandle* serializer);
    int raptor_serializer_set_option(SerializerHandle* serializer,
                                     RaptorOption option,
                                     const char *string,
                                     int integer);
    int raptor_serializer_get_option(SerializerHandle* serializer,
                                     RaptorOption option,
                                     char **string_p,
                                     int *integer_p);
    RaptorWorldHandle* raptor_serializer_get_world(SerializerHandle* serializer);
    SerializerHandle* raptor_new_serializer(RaptorWorldHandle* world, const char *name);
}

struct SerializerWithoutFinalize {
    mixin WithoutFinalize!(SerializerHandle,
                           SerializerWithoutFinalize,
                           Serializer);
    // WARNING: Other order of arguments than in C
    void startToIOStream(IOStreamWithoutFinalize stream,
                         URIWithoutFinalize uri = URIWithoutFinalize.fromHandle(null))
    {
        if(raptor_serializer_start_to_iostream(handle, uri.handle, stream.handle) != 0)
            throw new RDFException();
    }
    void startToFilename(string filename) {
        if(raptor_serializer_start_to_filename(handle, filename.toStringz) != 0)
            throw new RDFException();
    }
    // raptor_serializer_start_to_string() is deliberately not used.
    // Use StreamToString instead.
    void startToFilehandle(URIWithoutFinalize uri, File file) {
        if(raptor_serializer_start_to_file_handle(handle, uri.handle, file.getFP) != 0)
            throw new RDFException();
    }
    // WARNING: Other order of arguments than in C
    void setNamespace(string prefix,
                      URIWithoutFinalize uri = URIWithoutFinalize.fromHandle(null))
    {
        if(raptor_serializer_set_namespace(handle, uri.handle, prefix.toStringz) != 0)
            throw new RDFException();
    }
    void setNamespaceWithoutPrefix(URIWithoutFinalize uri = URIWithoutFinalize.fromHandle(null)) {
        if(raptor_serializer_set_namespace(handle, uri.handle, null) != 0)
            throw new RDFException();
    }
    void setNamespace(NamespaceWithoutFinalize namespace) {
        if(raptor_serializer_set_namespace_from_namespace(handle, namespace.handle) != 0)
            throw new RDFException();
    }
    void serializeStatement(StatementWithoutFinalize statement) {
        if(raptor_serializer_serialize_statement(handle, statement.handle) != 0)
            throw new RDFException();
    }
    void serializeEnd() {
        if(raptor_serializer_serialize_end(handle) != 0)
            throw new RDFException();
    }
    void serializeFlush() {
        if(raptor_serializer_flush(handle) != 0)
            throw new RDFException();
    }
    @property ref const(SyntaxDescription) description() {
        return *raptor_serializer_get_description(handle);
    }
    @property IOStreamWithoutFinalize iostream() {
        return IOStreamWithoutFinalize.fromHandle(raptor_serializer_get_iostream(handle));
    }
    @property LocatorWithoutFinalize locator() {
        return LocatorWithoutFinalize.fromHandle(raptor_serializer_get_locator(handle));
    }
    void setOption(RaptorOption option, string value) {
        if(raptor_serializer_set_option(handle, option, value.toStringz, 0) != 0)
            throw new RDFException();
    }
    void setOption(RaptorOption option, int value) {
        if(raptor_serializer_set_option(handle, option, null, value) != 0)
            throw new RDFException();
    }
    uint getNumericOption(RaptorOption option) {
        int V;
        if(raptor_serializer_get_option(handle, option, null, &V) < 0)
            throw new RDFException();
        return V;
    }
    string getStringOption (RaptorOption option) {
        char *V;
        if(raptor_serializer_get_option(handle, option, &V, null) < 0)
            throw new RDFException();
        return V.fromStringz.idup; // do NOT free V
    }
    @property RaptorWorldWithoutFinalize world() {
        return RaptorWorldWithoutFinalize.fromHandle(raptor_serializer_get_world(handle));
    }
}

struct Serializer {
    mixin WithFinalize!(SerializerHandle,
                        SerializerWithoutFinalize,
                        Serializer,
                        raptor_free_serializer);
    Serializer create (RaptorWorldWithoutFinalize world) {
        return Serializer.fromNonnullHandle(raptor_new_serializer(world.handle, null));
    }
    Serializer create (RaptorWorldWithoutFinalize world, string syntaxName) {
        return Serializer.fromNonnullHandle(raptor_new_serializer(world.handle, syntaxName.toStringz));
    }
}
