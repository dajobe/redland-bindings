module rdf.redland.serializer;

import std.string;
import std.stdio : File, FILE;
import rdf.auxiliary.handled_record;
import rdf.raptor.syntax;
import rdf.redland.world;
import rdf.redland.uri;
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
}

struct Serializer {
    mixin WithFinalize!(SerializerHandle,
                        SerializerWithoutFinalize,
                        Serializer,
                        librdf_free_serializer);
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

// TODO: Stopped at Serialize_To_String

