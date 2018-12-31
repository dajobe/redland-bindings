module rdf.raptor.uri;

import std.string;
import rdf.auxiliary.handled_record;

private extern extern(C) {
    void raptor_free_uri(Dummy* uri);
    Dummy* raptor_uri_copy(Dummy* uri);
    int raptor_uri_compare(Dummy* uri1, Dummy* uri2);
    int raptor_uri_equals(Dummy* uri1, Dummy* uri2);
    char* raptor_uri_as_string(Dummy* uri);
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
}

struct URI {
    mixin WithFinalize!(URIWithoutFinalize,
                        URI,
                        raptor_free_uri);
    mixin CompareHandles!(raptor_uri_equals, raptor_uri_compare);
}

// TODO
