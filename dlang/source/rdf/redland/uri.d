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
    @property rdf.raptor.uri.URI toRaptor() {
      return rdf.raptor.uri.URI.fromHandle(cast(rdf.raptor.uri.URIHandle*)handle);
    }
}

struct URI {
    mixin WithFinalize!(URIHandle,
                        URIWithoutFinalize,
                        URI,
                        librdf_free_uri);
    URI fromRaptor(rdf.raptor.uri.URI uri) {
      return URI.fromHandle(cast(URIHandle*)handle);
    }
}

// TODO: Stopped at As_String

