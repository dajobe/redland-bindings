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
    URIHandle* raptor_namespace_get_uri(const NamespaceHandle* ns);
    const(char*) raptor_namespace_get_prefix(const NamespaceHandle* ns);
}

struct NamespaceWithoutFinalize {
    mixin WithoutFinalize!(NamespaceHandle,
                           NamespaceWithoutFinalize,
                           Namespace);
    @property URIWithoutFinalize uri() {
        // raptor_namespace_get_uri() may return NULL (for xmlns="")
        return URIWithoutFinalize.fromHandle(raptor_namespace_get_uri(handle));
    }
    @property string prefix() {
        return raptor_namespace_get_prefix(handle).fromStringz.idup;
    }
}

struct Namespace {
    mixin WithFinalize!(NamespaceHandle,
                        NamespaceWithoutFinalize,
                        Namespace,
                        raptor_free_namespace);
}

// TOOD: Stopped at Write
