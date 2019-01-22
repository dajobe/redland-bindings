module rdf.redland.serializer;

import std.string;
import std.stdio : File, FILE;
import rdf.auxiliary.handled_record;
import rdf.raptor.iostream;
import rdf.raptor.syntax;
import rdf.redland.memory;
import rdf.redland.world;
import rdf.redland.uri;
import rdf.redland.stream;
import rdf.redland.model;

struct SerializerHandle;

private extern extern(C) {
    void librdf_free_serializer(SerializerHandle* serializer);
    const(SyntaxDescription*) librdf_serializer_get_description(RedlandWorldHandle* world,
                                                                uint counter);
    int librdf_serializer_check_name(RedlandWorldHandle* world, const char *name);
    int librdf_serializer_serialize_model_to_file_handle(SerializerHandle* serializer,
                                                         FILE* handle,
                                                         URIHandle* base_uri,
                                                         ModelHandle* model);
    int librdf_serializer_serialize_model_to_file(SerializerHandle* serializer,
                                                  const char *name,
                                                  URIHandle* base_uri,
                                                  ModelHandle* model);
    char* librdf_serializer_serialize_model_to_counted_string(SerializerHandle* serializer,
                                                              URIHandle* base_uri,
                                                              ModelHandle* model,
                                                              size_t *length_p);
    int librdf_serializer_serialize_model_to_iostream(SerializerHandle* serializer,
                                                      URIHandle* base_uri,
                                                      ModelHandle* model,
                                                      IOStreamHandle* iostr);

    char* librdf_serializer_serialize_stream_to_counted_string(SerializerHandle* serializer,
                                                               URIHandle* base_uri,
                                                               StreamHandle* stream,
                                                               size_t *length_p);
    int librdf_serializer_serialize_stream_to_file(SerializerHandle* serializer,
                                                   const char *name,
                                                   URIHandle* base_uri,
                                                   StreamHandle* stream);
    int librdf_serializer_serialize_stream_to_file_handle(SerializerHandle* serializer,
                                                          FILE *handle,
                                                          URIHandle* base_uri,
                                                          StreamHandle* stream);
    int librdf_serializer_serialize_stream_to_iostream(SerializerHandle* serializer,
                                                       URIHandle* base_uri,
                                                       StreamHandle* stream,
                                                       IOStreamHandle *iostr);
    SerializerHandle* librdf_new_serializer(RedlandWorldHandle* world,
                                            const char *name,
                                            const char *mime_type,
                                            URIHandle* type_uri);
}

struct SerializerWithoutFinalize {
    mixin WithoutFinalize!(SerializerHandle,
                           SerializerWithoutFinalize,
                           Serializer);
    /// Order of arguments not the same as in C
    void serializeToFileHandle(File file,
                               ModelWithoutFinalize model,
                               URIWithoutFinalize baseURI = URIWithoutFinalize.fromHandle(null))
    {
        int res = librdf_serializer_serialize_model_to_file_handle(handle,
                                                                   file.getFP,
                                                                   baseURI.handle,
                                                                   model.handle);
        if(res != 0) throw new RDFException();
    }
    /// Order of arguments not the same as in C
    void serializeToFile(string filename,
                         ModelWithoutFinalize model,
                         URIWithoutFinalize baseURI = URIWithoutFinalize.fromHandle(null))
    {
        int res = librdf_serializer_serialize_model_to_file(handle,
                                                            filename.toStringz,
                                                            baseURI.handle,
                                                            model.handle);
        if(res != 0) throw new RDFException();
    }
    /// Order of arguments not the same as in C
    string serializeToString(ModelWithoutFinalize model,
                             URIWithoutFinalize baseURI = URIWithoutFinalize.fromHandle(null))
    {
        size_t length;
        char* ptr = librdf_serializer_serialize_model_to_counted_string(handle,
                                                                        baseURI.handle,
                                                                        model.handle,
                                                                        &length);
        if(!ptr) throw new RDFException();
        scope(exit) librdf_free_memory(ptr);
        return ptr[0..length].idup;
    }
    /// Order of arguments not the same as in C
    void serializeToIOStream(ModelWithoutFinalize model,
                             IOStreamWithoutFinalize iostream,
                             URIWithoutFinalize baseURI = URIWithoutFinalize.fromHandle(null))
    {
        int res = librdf_serializer_serialize_model_to_iostream(handle,
                                                                baseURI.handle,
                                                                model.handle,
                                                                iostream.handle);
        if(res != 0) throw new RDFException();
    }
    /// Order of arguments not the same as in C
    string serializeToString(StreamWithoutFinalize stream,
                             URIWithoutFinalize baseURI = URIWithoutFinalize.fromHandle(null))
    {
        size_t length;
        char* ptr = librdf_serializer_serialize_stream_to_counted_string(handle,
                                                                         baseURI.handle,
                                                                         stream.handle,
                                                                         &length);
        if(!ptr) throw new RDFException();
        scope(exit) librdf_free_memory(ptr);
        return ptr[0..length].idup;
    }
    /// Order of arguments not the same as in C
    void serializeToFile(string filename,
                         StreamWithoutFinalize stream,
                         URIWithoutFinalize baseURI = URIWithoutFinalize.fromHandle(null))
    {
        int res = librdf_serializer_serialize_stream_to_file(handle,
                                                             filename.toStringz,
                                                             baseURI.handle,
                                                             stream.handle);
        if(res != 0) throw new RDFException();
    }
    void serializeToFileHandle(File file,
                               StreamWithoutFinalize stream,
                               URIWithoutFinalize baseURI = URIWithoutFinalize.fromHandle(null))
    {
        int res = librdf_serializer_serialize_stream_to_file_handle(handle,
                                                                    file.getFP,
                                                                    baseURI.handle,
                                                                    stream.handle);
        if(res != 0) throw new RDFException();
    }
    void serializeToIOStream(IOStreamWithoutFinalize file,
                             StreamWithoutFinalize stream,
                             URIWithoutFinalize baseURI = URIWithoutFinalize.fromHandle(null))
    {
        int res = librdf_serializer_serialize_stream_to_iostream(handle,
                                                                 baseURI.handle,
                                                                 stream.handle,
                                                                 file.handle);
        if(res != 0) throw new RDFException();
    }
   // http://bugs.librdf.org/mantis/view.php?id=641
//     not overriding function Get_Feature (Serializer: Serializer_Type_Without_Finalize;
//                                          Feature: URI_Type_Without_Finalize'Class);
//                                          return Node_Type;
//     not overriding function Set_Feature (Serializer: Serializer_Type_Without_Finalize;
//                                          Feature: URI_Type_Without_Finalize'Class;
//                                          Value: Node_Iterator_Type_Without_Finalize'Class);
}

struct Serializer {
    mixin WithFinalize!(SerializerHandle,
                        SerializerWithoutFinalize,
                        Serializer,
                        librdf_free_serializer);
    Serializer create(RedlandWorldWithoutFinalize world,
                      string name = "",
                      string mimeType = "",
                      URIWithoutFinalize typeURI = URIWithoutFinalize.fromHandle(null))
    {
        SerializerHandle* h = librdf_new_serializer(world.handle,
                                                    name.empty ? null : name.ptr,
                                                    mimeType.empty ? null : mimeType.ptr,
                                                    typeURI.handle);
        return Serializer.fromNonnullHandle(h);
    }
}

ref const(SyntaxDescription) getSerializerDescription(RedlandWorldWithoutFinalize world,
                                                      uint index)
{
    return *librdf_serializer_get_description(world.handle, index);
}

bool serializerCheckName(RedlandWorldWithoutFinalize world, string name) {
    return librdf_serializer_check_name(world.handle, name.toStringz) != 0;
}

struct SerializersEnumerate {
private:
    RedlandWorldWithoutFinalize _world;
    uint counter = 0;
public:
    this(RedlandWorldWithoutFinalize world) {
        _world = world;
    }
    @property uint position() { return counter; }
    @property bool empty() {
        return !librdf_serializer_get_description(_world.handle, counter);
    }
    @property ref const(SyntaxDescription) front()
        in(!empty)
    {
        return getSerializerDescription(_world, counter);
    }
    void popFront()
        in(!empty)
    {
        ++counter;
    }
}

