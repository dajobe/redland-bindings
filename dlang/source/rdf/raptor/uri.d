module rdf.raptor.uri;

import rdf.auxiliary.handled_record;

private extern extern(C) {
    void raptor_free_uri(Dummy* uri);
    Dummy* raptor_uri_copy(Dummy* uri);
}

struct URIWithoutFinalize {
    mixin WithoutFinalize!(URIWithoutFinalize,
                           URI,
                           raptor_uri_copy);
}

struct URI {
    mixin WithFinalize!(URIWithoutFinalize,
                        URI,
                        raptor_free_uri);
}

// TODO
