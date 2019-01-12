module rdf.rasqal.uri;

import std.stdio : File, FILE;
import rdf.auxiliary.handled_record;
static import rdf.raptor.uri;

struct URIHandle;

private extern extern(C) {
    void librdf_free_uri(URIHandle* uri);
    URIHandle* librdf_new_uri_from_uri(URIHandle* old_uri);
    char* librdf_uri_as_counted_string(URIHandle* uri, size_t *len_p);
    void librdf_uri_print(URIHandle* uri, FILE *fh);
    int librdf_uri_equals(URIHandle* first_uri, URIHandle* second_uri);
    int librdf_uri_compare(URIHandle* first_uri, URIHandle* second_uri);
}

struct URIWithoutFinalize {
    mixin WithoutFinalize!(URIHandle,
                           URIWithoutFinalize,
                           URI,
                           librdf_new_uri_from_uri);
    mixin CompareHandles!(librdf_uri_equals, librdf_uri_compare);
    @property rdf.raptor.uri.URIWithoutFinalize toRaptor() {
      return rdf.raptor.uri.URI.fromHandle(cast(rdf.raptor.uri.URIHandle*)handle);
    }
    string toString() {
      size_t length;
      char* str = librdf_uri_as_counted_string(handle, &length);
      return str[0..length].idup;
    }
    void print(File file) {
        librdf_uri_print(handle, file.getFP);
    }
}

struct URI {
    mixin WithFinalize!(URIHandle,
                        URIWithoutFinalize,
                        URI,
                        librdf_free_uri);
    mixin CompareHandles!(librdf_uri_equals, librdf_uri_compare);
    URIWithoutFinalize fromRaptor(rdf.raptor.uri.URIWithoutFinalize uri) { // FIXME: WithoutFinalize?
      return URI.fromHandle(cast(URIHandle*)handle);
    }
}

// TODO: Stopped at Is_File_URI

