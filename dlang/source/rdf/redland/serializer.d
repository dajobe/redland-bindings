module rdf.redland.serializer;

import std.string;
import rdf.auxiliary.handled_record;
import rdf.raptor.syntax;
import rdf.redland.world;

struct SerializerHandle;

private extern extern(C) {
    void librdf_free_serializer(SerializerHandle* serializer);
    const(SyntaxDescription*) librdf_serializer_get_description(RedlandWorldHandle* world,
                                                                uint counter);
    int librdf_serializer_check_name(RedlandWorldHandle* world, const char *name);
}

struct SerializerWithoutFinalize {
    mixin WithoutFinalize!(SerializerHandle,
                           SerializerWithoutFinalize,
                           Serializer);
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

// TODO: Stopped at Serialize_To_File_Handle

