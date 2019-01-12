module rdf.redland.uri;

import std.string;
import std.stdio : File, FILE;
import rdf.auxiliary.handled_record;
static import rdf.raptor.uri;
import rdf.redland.memory;
import rdf.redland.world;

struct URIHandle;

private extern extern(C) {
    void librdf_free_uri(URIHandle* uri);
    URIHandle* librdf_new_uri_from_uri(URIHandle* old_uri);
    char* librdf_uri_as_counted_string(URIHandle* uri, size_t *len_p);
    void librdf_uri_print(URIHandle* uri, FILE *fh);
    int librdf_uri_equals(URIHandle* first_uri, URIHandle* second_uri);
    int librdf_uri_compare(URIHandle* first_uri, URIHandle* second_uri);
    int librdf_uri_is_file_uri(URIHandle* uri);
    const(char*) librdf_uri_to_filename(URIHandle* uri);
    URIHandle* librdf_get_concept_ms_namespace     (RedlandWorldHandle* world);
    URIHandle* librdf_get_concept_schema_namespace (RedlandWorldHandle* world);
    URIHandle* librdf_new_uri2(RedlandWorldHandle* world, const char *uri_string, size_t length);
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
    bool isFileURI() {
        return librdf_uri_is_file_uri(handle) != 0;
    }
    string toFilename() {
        const char* ptr = librdf_uri_to_filename(handle);
        scope(exit) librdf_free_memory(cast(char*)ptr);
        return ptr.fromStringz.idup;
    }
}

struct URI {
    mixin WithFinalize!(URIHandle,
                        URIWithoutFinalize,
                        URI,
                        librdf_free_uri);
    mixin CompareHandles!(librdf_uri_equals, librdf_uri_compare);
    static URIWithoutFinalize fromRaptor(rdf.raptor.uri.URIWithoutFinalize uri) { // FIXME: WithoutFinalize? // FIXME: Move to other struct?
        return URI.fromHandle(cast(URIHandle*)uri.handle);
    }
    static URI fromString(RedlandWorldWithoutFinalize world, string uri) {
        return fromNonnullHandle(librdf_new_uri2(world.handle, uri.ptr, uri.length));
    }
}

// From http://librdf.org/docs/api/redland-concepts.html:
URIWithoutFinalize conceptMsNamespace(RedlandWorldWithoutFinalize world) {
    return URIWithoutFinalize.fromNonnullHandle(librdf_get_concept_ms_namespace(world.handle));
}

URIWithoutFinalize conceptSchemaNamespace(RedlandWorldWithoutFinalize world) {
    return URIWithoutFinalize.fromNonnullHandle(librdf_get_concept_schema_namespace(world.handle));
}

// TODO: Stopped at From_URI_Local_Name

