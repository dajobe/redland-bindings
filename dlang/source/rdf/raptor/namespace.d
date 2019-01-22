module rdf.raptor.namespace;

import std.string;
import rdf.auxiliary.handled_record;
import rdf.raptor.memory;
import rdf.raptor.world;
import rdf.raptor.uri;
import rdf.raptor.iostream;
import rdf.raptor.namespace_stack;

struct NamespaceHandle;

private extern extern(C) {
    void raptor_free_namespace(NamespaceHandle* ns);
    URIHandle* raptor_namespace_get_uri(const NamespaceHandle* ns);
    const(char*) raptor_namespace_get_prefix(const NamespaceHandle* ns);
    int raptor_namespace_write(NamespaceHandle* ns, IOStreamHandle* iostr);
    char* raptor_namespace_format_as_xml(const NamespaceHandle* ns, size_t* length_p);
    int raptor_xml_namespace_string_parse(const char *string, char **prefix, char **uri_string);

    NamespaceHandle* raptor_new_namespace(NamespaceStackHandle* nstack,
                                          const char* prefix,
                                          const char* ns_uri_string,
                                          int depth);
    NamespaceHandle* raptor_new_namespace_from_uri(NamespaceStackHandle* nstack,
                                                   const char* prefix,
                                                   URIHandle* ns_uri,
                                                   int depth);
}

struct PrefixAndURI {
    string prefix;
    string uri;
}

struct NamespaceWithoutFinalize {
    mixin WithoutFinalize!(NamespaceHandle,
                           NamespaceWithoutFinalize,
                           Namespace);
    @property URIWithoutFinalize uri() const {
        // raptor_namespace_get_uri() may return NULL (for xmlns="")
        return URIWithoutFinalize.fromHandle(raptor_namespace_get_uri(handle));
    }
    @property string prefix() const {
        return raptor_namespace_get_prefix(handle).fromStringz.idup;
    }
    void write(IOStreamWithoutFinalize stream) const {
        if(raptor_namespace_write(handle, stream.handle) != 0)
          throw new IOStreamException();
    }
    string formatAsXML() const {
      char* str = raptor_namespace_format_as_xml(handle, null);
      if(!str) throw new RDFException();
      scope(exit) raptor_free_memory(str);
      return str.fromStringz.idup;
    }
}

struct Namespace {
    mixin WithFinalize!(NamespaceHandle,
                        NamespaceWithoutFinalize,
                        Namespace,
                        raptor_free_namespace);
    static Namespace create(NamespaceStackWithoutFinalize stack, string prefix, string ns, uint depth) {
        NamespaceHandle* h =
            raptor_new_namespace(stack.handle, prefix.toStringz, ns.toStringz, depth);
        return fromNonnullHandle(h);
    }
    static Namespace fromURI(NamespaceStackWithoutFinalize stack,
                             string prefix,
                             URIWithoutFinalize uri,
                             int depth)
    {
        NamespaceHandle* h =
            raptor_new_namespace_from_uri(stack.handle, prefix.toStringz, uri.handle, depth);
        return fromNonnullHandle(h);
    }
}

// See also extractPrefix and extractURI
PrefixAndURI stringParse(string ns) {
    char* prefix, uri;
    if(raptor_xml_namespace_string_parse(ns.toStringz, &prefix, &uri) != 0)
        throw new RDFException();
    scope(exit) {
        raptor_free_memory(prefix);
        raptor_free_memory(uri);
    }
    string prefix2 = prefix.fromStringz.idup;
    string uri2 = uri.fromStringz.idup;
    return PrefixAndURI(prefix2, uri2);
}

string extractPrefix(string ns) {
    char* prefix;
    if(raptor_xml_namespace_string_parse(ns.toStringz, &prefix, null) != 0)
        throw new RDFException();
    scope(exit) raptor_free_memory(prefix);
    string result = prefix.fromStringz.idup;
    return result;
}

string extractURI(string ns) {
    char* uri;
    if(raptor_xml_namespace_string_parse(ns.toStringz, null, &uri) != 0)
        throw new RDFException();
    scope(exit) raptor_free_memory(uri);
    return uri.fromStringz.idup;
}

unittest {
    // see namespace_stack.d
}

