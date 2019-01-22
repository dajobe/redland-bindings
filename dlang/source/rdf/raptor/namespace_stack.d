module rdf.raptor.namespace_stack;

import std.string;
import std.typecons;
import rdf.auxiliary.handled_record;
import rdf.raptor.memory;
import rdf.raptor.world;
import rdf.raptor.uri;
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
    NamespaceHandle* raptor_namespaces_get_default_namespace(NamespaceStackHandle* nstack);
    NamespaceHandle* raptor_namespaces_find_namespace(NamespaceStackHandle* nstack,
                                                      const char *prefix,
                                                      int prefix_length);
    NamespaceHandle* raptor_namespaces_find_namespace_by_uri(NamespaceStackHandle* nstack,
                                                             URIHandle* ns_uri);
    int raptor_namespaces_namespace_in_scope(NamespaceStackHandle* nstack,
                                             const NamespaceHandle* nspace);
    NamespaceStackHandle* raptor_new_namespaces(RaptorWorldHandle *world, int defaults);
}

enum NamespaceOptions { noneType = 0, xmlType = 1, rdfFType = 2, undefinedType = 3 };

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
    void startNamespace(NamespaceWithoutFinalize ns, uint newDepth) {
        if(raptor_namespace_stack_start_namespace(handle, ns.handle, newDepth) != 0)
            throw new RDFException();
    }
    void startNamespace(NamespaceWithoutFinalize ns,
                        Nullable!string prefix,
                        Nullable!string nsURI,
                        uint newDepth)
    {
        immutable int res = raptor_namespaces_start_namespace_full(handle,
                                                                   prefix.toStringz,
                                                                   nsURI.toStringz,
                                                                   newDepth);
        if(res != 0)
            throw new RDFException();
    }
    void endForDepth(uint depth) {
        raptor_namespaces_end_for_depth(handle, depth);
    }
    @property NamespaceWithoutFinalize defaultNamespace() {
        return NamespaceWithoutFinalize.fromNonnullHandle(raptor_namespaces_get_default_namespace(handle));
    }
    NamespaceWithoutFinalize findNamespace(string prefix) {
        NamespaceHandle* handle =
            raptor_namespaces_find_namespace(handle, prefix.ptr, cast(int)prefix.length);
        return NamespaceWithoutFinalize.fromNonnullHandle(handle);
    }
    NamespaceWithoutFinalize findDefaultNamespace(string prefix) {
        return NamespaceWithoutFinalize.fromNonnullHandle(raptor_namespaces_find_namespace(handle, null, 0));
    }
    NamespaceWithoutFinalize findNamespaceByURI(URIWithoutFinalize uri) {
        NamespaceHandle* res = raptor_namespaces_find_namespace_by_uri(handle, uri.handle);
        return NamespaceWithoutFinalize.fromNonnullHandle(res) ;
    }
    bool inScope(NamespaceWithoutFinalize ns) {
        return raptor_namespaces_namespace_in_scope(handle, ns.handle) != 0;
    }
}

struct NamespaceStack {
    mixin WithFinalize!(NamespaceStackHandle,
                        NamespaceStackWithoutFinalize,
                        NamespaceStack,
                        raptor_free_namespaces);
    static NamespaceStack createStack(RaptorWorldWithoutFinalize world, NamespaceOptions defaults) {
        return fromNonnullHandle(raptor_new_namespaces(world.handle, defaults) );
    }
}

// raptor_namespaces_init() not bound (it seems that function is internal for Raptor implementation).

unittest {
    RaptorWorld world = RaptorWorld.createAndOpen();
    NamespaceStack stack = NamespaceStack.createStack(world, NamespaceOptions.xmlType);
    URI uri1obj = URI.fromString(world, "http://www.w3.org/1999/xhtml/");
    Namespace ns1 = Namespace.create(stack, "xhtml", "http://www.w3.org/1999/xhtml/", 1);
    Namespace ns2 = Namespace.fromURI(stack, "xhtml", uri1obj, 1);
    assert(ns1.uri.toString == "http://www.w3.org/1999/xhtml/", "Check namespace URI");
    assert(ns2.uri.toString == "http://www.w3.org/1999/xhtml/", "Check namespace URI");
    assert(ns1.prefix == "xhtml", "Check namespace prefix");
    assert(ns2.prefix == "xhtml", "Check namespace prefix");
}

