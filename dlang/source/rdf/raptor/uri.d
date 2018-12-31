module rdf.raptor.uri;

import std.string;
import rdf.auxiliary.handled_record;
import rdf.raptor.memory;

private extern extern(C) {
    void raptor_free_uri(Dummy* uri);
    Dummy* raptor_uri_copy(Dummy* uri);
    int raptor_uri_compare(Dummy* uri1, Dummy* uri2);
    int raptor_uri_equals(Dummy* uri1, Dummy* uri2);
    char* raptor_uri_as_string(Dummy* uri);
    char* raptor_uri_to_relative_uri_string(Dummy* base_uri, Dummy* reference_uri);
    size_t raptor_uri_resolve_uri_reference(const char *base_uri,
                                            const char *reference_uri,
                                            char *buffer,
                                            size_t length);
}

/// Only absolute URIs!
struct URIWithoutFinalize {
    mixin WithoutFinalize!(URIWithoutFinalize,
                           URI,
                           raptor_uri_copy);
    mixin CompareHandles!(raptor_uri_equals, raptor_uri_compare);
    string toString() {
        return fromStringz(raptor_uri_as_string(handle)).idup;
    }
    static string toRelativeURIString(URIWithoutFinalize baseURI, URIWithoutFinalize referenceURI) {
      char* str =
            raptor_uri_to_relative_uri_string(baseURI.handle, referenceURI.handle);
      if(!str) throw new NullRDFException();
      scope(exit) raptor_free_memory(str);
      return fromStringz(str).idup;
    }
    static string resolveURIReference(string baseURI, string referenceURI) {
        immutable size_t bufferLength = baseURI.length + referenceURI.length + 1;
        char[] buffer = new char[bufferLength];
        immutable res = raptor_uri_resolve_uri_reference(toStringz(baseURI),
                                                         toStringz(referenceURI),
                                                         buffer.ptr,
                                                         bufferLength);
        if(!res) throw new RDFException();
        buffer.length = bufferLength;
        return cast(string)buffer;
    }
}

struct URI {
    mixin WithFinalize!(URIWithoutFinalize,
                        URI,
                        raptor_free_uri);
    mixin CompareHandles!(raptor_uri_equals, raptor_uri_compare);
}

// TODO
