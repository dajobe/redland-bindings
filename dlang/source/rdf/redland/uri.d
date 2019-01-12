module rdf.rasqal.uri;

import rdf.auxiliary.handled_record;
static import rdf.raptor.uri;

struct URIHandle;

private extern extern(C) {
    void librdf_free_uri(URIHandle* uri);
    URIHandle* librdf_new_uri_from_uri(URIHandle* old_uri);
}

struct URIWithoutFinalize {
    mixin WithoutFinalize!(URIHandle,
                           URIWithoutFinalize,
                           URI,
                           librdf_new_uri_from_uri);
}

struct URI {
    mixin WithFinalize!(URIHandle,
                        URIWithoutFinalize,
                        URI,
                        librdf_free_uri);
}

// TODO: Stopped at To_Raptor

