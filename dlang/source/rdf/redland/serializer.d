module rdf.redland.serializer;

import rdf.auxiliary.handled_record;
import rdf.raptor.syntax;
import rdf.redland.world;

struct SerializerHandle;

private extern extern(C) {
    void librdf_free_serializer(SerializerHandle* serializer);
    const(SyntaxDescription*) librdf_serializer_get_description(RedlandWorldHandle* world,
                                                                uint counter);
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

// TODO: Stopped at Serializer_Check_Name

