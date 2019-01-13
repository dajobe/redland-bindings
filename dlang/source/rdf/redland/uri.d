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
    URIHandle* librdf_new_uri_from_uri_local_name(URIHandle* old_uri, const char *local_name);
    URIHandle* librdf_new_uri_normalised_to_base(const char *uri_string,
                                                 URIHandle* source_uri,
                                                 URIHandle* base_uri);
    URIHandle* librdf_new_uri_relative_to_base(URIHandle* base_uri, const char *uri_string);
    URIHandle* librdf_new_uri_from_filename(RedlandWorldHandle* world, const char *filename);
}

struct URIWithoutFinalize {
    mixin WithoutFinalize!(URIHandle,
                           URIWithoutFinalize,
                           URI,
                           librdf_new_uri_from_uri);
    mixin CompareHandles!(librdf_uri_equals, librdf_uri_compare);
    @property rdf.raptor.uri.URI toRaptor() { // FIXME: also dup() in Ada
      return rdf.raptor.uri.URIWithoutFinalize.fromHandle(cast(rdf.raptor.uri.URIHandle*)handle).dup;
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
    static URI fromRaptor(rdf.raptor.uri.URIWithoutFinalize uri) { // FIXME: also dup() in Ada // FIXME: Move to other struct?
        return URIWithoutFinalize.fromHandle(cast(URIHandle*)uri.handle).dup;
    }
    static URI fromString(RedlandWorldWithoutFinalize world, string uri) {
        return fromNonnullHandle(librdf_new_uri2(world.handle, uri.ptr, uri.length));
    }
    static URI fromURILocalName(URIWithoutFinalize oldURI, string localName) {
        return fromNonnullHandle(
            librdf_new_uri_from_uri_local_name(oldURI.handle, localName.toStringz));
    }
    static URI normalisedToBase(string uriStr,
                                URIWithoutFinalize sourceURI,
                                URIWithoutFinalize baseURI)
    {
        return fromNonnullHandle(
            librdf_new_uri_normalised_to_base(uriStr.toStringz, sourceURI.handle, baseURI.handle));
    }
    static URI relativeToBase(URIWithoutFinalize baseURI, string uri) {
        return fromNonnullHandle(
            librdf_new_uri_relative_to_base(baseURI.handle, uri.toStringz));
    }
    static fromFilename (RedlandWorldWithoutFinalize world, string filename) {
        return fromNonnullHandle(librdf_new_uri_from_filename(world.handle, filename.toStringz));
    }
}

// From http://librdf.org/docs/api/redland-concepts.html:
URIWithoutFinalize conceptMsNamespace(RedlandWorldWithoutFinalize world) {
    return URIWithoutFinalize.fromNonnullHandle(librdf_get_concept_ms_namespace(world.handle));
}

URIWithoutFinalize conceptSchemaNamespace(RedlandWorldWithoutFinalize world) {
    return URIWithoutFinalize.fromNonnullHandle(librdf_get_concept_schema_namespace(world.handle));
}

