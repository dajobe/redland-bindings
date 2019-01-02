module rdf.raptor.namespace_stack;

import std.string;
import rdf.auxiliary.handled_record;
import rdf.raptor.memory;
import rdf.raptor.world;

struct NamespaceStackHandle;

private extern extern(C) {
    void raptor_free_namespaces(NamespaceStackHandle* nstack);
}

struct NamespaceStackWithoutFinalize {
    mixin WithoutFinalize!(NamespaceStackHandle,
                           NamespaceStackWithoutFinalize,
                           NamespaceStack);
}

struct NamespaceStack {
    mixin WithFinalize!(NamespaceStackHandle,
                        NamespaceStackWithoutFinalize,
                        NamespaceStack,
                        raptor_free_namespaces);
}

// TODO
