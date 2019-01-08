module rdf.raptor.serializer;

import std.string;
import std.stdio : FILE, File;
import rdf.auxiliary.handled_record;

struct SerializerHandle;

private extern extern(C) {
    void raptor_free_serializer(SerializerHandle* rdf_serializer);
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
                        raptor_free_serializer);
}

// TODO: Stopped at Start_To_IOStream
