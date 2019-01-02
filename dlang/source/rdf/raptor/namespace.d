module rdf.raptor.namespace;

import std.string;
// import std.typecons;
import rdf.auxiliary.handled_record;
// import rdf.auxiliary.nullable_string;
// import rdf.raptor.memory;
import rdf.raptor.world;
import rdf.raptor.uri;

struct NamespaceHandle;

private extern extern(C) {
    void raptor_free_namespace(NamespaceHandle* ns);
}

struct NamespaceWithoutFinalize {
    mixin WithoutFinalize!(NamespaceHandle,
                           NamespaceWithoutFinalize,
                           Namespace);
}

struct Namespace {
    mixin WithFinalize!(NamespaceHandle,
                        NamespaceWithoutFinalize,
                        Namespace,
                        raptor_free_namespace);
}
