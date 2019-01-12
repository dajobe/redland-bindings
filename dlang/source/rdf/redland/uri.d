module rdf.rasqal.uri;

import rdf.auxiliary.handled_record;
static import rdf.raptor.uri;

struct URIHandle;

private extern extern(C) {
    void librdf_free_uri(URIHandle* uri);
    URIHandle* librdf_new_uri_from_uri(URIHandle* old_uri);
    char* librdf_uri_as_counted_string(URIHandle* uri, size_t *len_p);
}

struct URIWithoutFinalize {
    mixin WithoutFinalize!(URIHandle,
                           URIWithoutFinalize,
                           URI,
                           librdf_new_uri_from_uri);
    @property rdf.raptor.uri.URIWithoutFinalize toRaptor() {
      return rdf.raptor.uri.URI.fromHandle(cast(rdf.raptor.uri.URIHandle*)handle);
    }
    string toString() {
      size_t length;
      char* str = librdf_uri_as_counted_string(handle, &length);
      return str[0..length].idup;
    }
}

struct URI {
    mixin WithFinalize!(URIHandle,
                        URIWithoutFinalize,
                        URI,
                        librdf_free_uri);
    URIWithoutFinalize fromRaptor(rdf.raptor.uri.URIWithoutFinalize uri) {
      return URI.fromHandle(cast(URIHandle*)handle);
    }
}

// TODO: Stopped at Print

