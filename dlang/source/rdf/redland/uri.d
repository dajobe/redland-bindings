module rdf.rasqal.uri;

import rdf.auxiliary.handled_record;
static import rdf.raptor.uri;

struct URIHandle;

private extern extern(C) {
    void librdf_free_uri(URIHandle* uri);
}

struct URIWithoutFinalize {
    mixin WithoutFinalize!(URIHandle,
                           URIWithoutFinalize,
                           URI);
}

struct URI {
    mixin WithFinalize!(URIHandle,
                        URIWithoutFinalize,
                        URI,
                        librdf_free_uri);
}
