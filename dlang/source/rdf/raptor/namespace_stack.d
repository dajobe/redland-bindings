module rdf.raptor.namespace_stack;

import std.string;
import std.typecons;
import rdf.auxiliary.handled_record;
import rdf.raptor.memory;
import rdf.raptor.world;
import rdf.raptor.namespace;

struct NamespaceStackHandle;

private extern extern(C) {
    void raptor_free_namespaces(NamespaceStackHandle* nstack);
    void raptor_namespaces_clear(NamespaceStackHandle* nstack);
    void raptor_namespaces_start_namespace(NamespaceStackHandle* nstack, NamespaceHandle* nspace);
    int raptor_namespace_stack_start_namespace(NamespaceStackHandle* nstack,
                                               NamespaceHandle* ns,
                                               int new_depth);
    int raptor_namespaces_start_namespace_full(NamespaceStackHandle* nstack,
                                               const char *prefix,
                                               const char *ns_uri_string,
                                               int depth);
    void raptor_namespaces_end_for_depth(NamespaceStackHandle* nstack, int depth);
}

struct NamespaceStackWithoutFinalize {
    mixin WithoutFinalize!(NamespaceStackHandle,
                           NamespaceStackWithoutFinalize,
                           NamespaceStack);
    void clear() {
        raptor_namespaces_clear(handle);
    }
    void startNamespace(NamespaceWithoutFinalize ns) {
        raptor_namespaces_start_namespace(handle, ns.handle);
    }
    void startNamespace (NamespaceWithoutFinalize ns, uint newDepth) {
        if(raptor_namespace_stack_start_namespace(handle, ns.handle, newDepth) != 0)
            throw new RDFException();
    }
    void startNamespace(NamespaceWithoutFinalize ns,
                        Nullable!string prefix,
                        Nullable!string nsUri,
                        uint newDepth)
    {
        immutable int res = raptor_namespaces_start_namespace_full(handle,
                                                                   prefix.toStringz,
                                                                   nsUri.toStringz,
                                                                   newDepth);
        if(res != 0)
            throw new RDFException();
    }
    void endForDepth(uint depth) {
        raptor_namespaces_end_for_depth(handle, depth);
    }
}

struct NamespaceStack {
    mixin WithFinalize!(NamespaceStackHandle,
                        NamespaceStackWithoutFinalize,
                        NamespaceStack,
                        raptor_free_namespaces);
}

// TODO: Stopped on Get_Default_Namespace
