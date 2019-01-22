module rdf.raptor.uri;

import std.string;
import std.stdio : FILE, File;
import rdf.auxiliary.handled_record;
import rdf.raptor.memory;
import rdf.raptor.world;
import rdf.raptor.iostream;
import rdf.raptor.namespace_stack;

struct URIHandle;

private extern extern(C) {
    void raptor_free_uri(URIHandle* uri);
    URIHandle* raptor_uri_copy(URIHandle* uri);
    int raptor_uri_compare(URIHandle* uri1, URIHandle* uri2);
    int raptor_uri_equals(URIHandle* uri1, URIHandle* uri2);
    char* raptor_uri_as_string(URIHandle* uri);
    char* raptor_uri_to_relative_uri_string(URIHandle* base_uri, URIHandle* reference_uri);
    size_t raptor_uri_resolve_uri_reference(const char *base_uri,
                                            const char *reference_uri,
                                            char *buffer,
                                            size_t length);
    char* raptor_uri_counted_filename_to_uri_string(const char *filename, size_t filename_len);
    int raptor_uri_uri_string_is_absolute(const char *uri_string);
    int raptor_uri_uri_string_is_file_uri(const char *uri_string);
    char* raptor_uri_uri_string_to_filename(const char *uri_string);
    char* raptor_uri_uri_string_to_filename_fragment(const char *uri_string, char **fragment_p);
    int raptor_uri_print(const URIHandle* uri, FILE* stream);
    RaptorWorldHandle* raptor_uri_get_world(URIHandle* uri);
    int raptor_uri_write(URIHandle* uri, IOStreamHandle* iostr);
    int raptor_uri_file_exists(URIHandle* uri);
    int raptor_uri_filename_exists(const char *path);
    URIHandle* raptor_new_uri_from_counted_string(RaptorWorldHandle* world, const char *uriString, size_t length);
    URIHandle* raptor_new_uri_from_uri_local_name(RaptorWorldHandle* world, URIHandle* uri, const char *local_name);
    URIHandle* raptor_new_uri_from_uri_or_file_string(RaptorWorldHandle* world, URIHandle* base_uri, const char *uri_or_file_string);
    URIHandle* raptor_new_uri_relative_to_base_counted(RaptorWorldHandle* world,
                                                       URIHandle* base_uri,
                                                       const char *uri_string,
                                                       size_t uri_len);
    URIHandle* raptor_new_uri_from_id(RaptorWorldHandle* world, URIHandle* base_uri, const char *id);
    URIHandle* raptor_new_uri_for_rdf_concept(RaptorWorldHandle* world, const char *name);
    URIHandle* raptor_new_uri_for_xmlbase(URIHandle* old_uri);
    URIHandle* raptor_new_uri_for_retrieval(URIHandle* old_uri);
    char* raptor_uri_to_turtle_string(RaptorWorldHandle* world,
                                      URIHandle* uri,
                                      NamespaceStackHandle* nstack,
                                      URIHandle* base_uri);
    int raptor_uri_turtle_write(RaptorWorldHandle* world,
                                IOStreamHandle* iostr,
                                URIHandle* uri,
                                NamespaceStackHandle* nstack,
                                URIHandle* base_uri);
}

/// Only absolute URIs!
struct URIWithoutFinalize {
    mixin WithoutFinalize!(URIHandle,
                           URIWithoutFinalize,
                           URI,
                           raptor_uri_copy);
    mixin CompareHandles!(raptor_uri_equals, raptor_uri_compare);
    string toString() const {
        return raptor_uri_as_string(handle).fromStringz.idup;
    }
    alias toString this;
    /// Not supposed to be used, but included for completeness
    void print(File file) const {
        if(raptor_uri_print(handle, file.getFP) != 0)
            throw new IOStreamException();
    }
    @property RaptorWorldWithoutFinalize world() const {
        return RaptorWorldWithoutFinalize.fromHandle(raptor_uri_get_world(handle));
    }
    void write(IOStreamWithoutFinalize stream) const {
        if(raptor_uri_write(handle, stream.handle) != 0)
            throw new IOStreamException();
    }
    bool uriFileExists() const {
        immutable int result = raptor_uri_file_exists(handle);
        if(result < 0) throw new RDFException();
        return result != 0;
    }
    string toTurtleString(RaptorWorldWithoutFinalize world,
                          NamespaceStackWithoutFinalize stack,
                          URIWithoutFinalize baseURI) const
    {
        // Better use raptor_uri_to_turtle_counted_string() instead (here and in Ada)
        char* str = raptor_uri_to_turtle_string(world.handle, handle, stack.handle, baseURI.handle);
        if(!str) throw new RDFException();
        scope(exit) raptor_free_memory(str);
        return str.fromStringz.idup;
    }
    void turtleWrite(RaptorWorldWithoutFinalize world,
                     IOStreamWithoutFinalize stream,
                     NamespaceStackWithoutFinalize stack,
                     URIWithoutFinalize baseURI) const
    {
        int res = raptor_uri_turtle_write(world.handle,
                                          stream.handle,
                                          handle,
                                          stack.handle,
                                          baseURI.handle);
        if(res != 0) throw new RDFException();
    }
}

struct URI {
    mixin WithFinalize!(URIHandle,
                        URIWithoutFinalize,
                        URI,
                        raptor_free_uri);
    mixin CompareHandles!(raptor_uri_equals, raptor_uri_compare);
    static URI fromString(RaptorWorldWithoutFinalize world, string uriString) {
        URIHandle* handle =
            raptor_new_uri_from_counted_string(world.handle, uriString.ptr, uriString.length);
        return fromNonnullHandle(handle);
    }
    this(URIHandle* ptr) {
        this.ptr = ptr;
    }
    this(RaptorWorldWithoutFinalize world, string s) {
        ptr = raptor_new_uri_from_counted_string(world.handle, s.ptr, s.length);
        if(!ptr) throw new NullRDFException();
    }
    static URI fromURIWithLocalName(RaptorWorldWithoutFinalize world, 
                                    URIWithoutFinalize uri,
                                    string localName)
    {
        URIHandle* handle =
            raptor_new_uri_from_uri_local_name(world.handle, uri.handle, localName.toStringz);
        return fromNonnullHandle(handle);
    }
    static URI fromURIOrFileString(RaptorWorldWithoutFinalize world, 
                                   URIWithoutFinalize baseURI,
                                   string uriOrFile)
    {
        URIHandle* handle =
            raptor_new_uri_from_uri_or_file_string(world.handle, baseURI.handle, uriOrFile.toStringz);
        return fromNonnullHandle(handle);
    }
    static URI fromURIRelativeToBase(RaptorWorldWithoutFinalize world, 
                                     URIWithoutFinalize baseURI,
                                     string uri)
    {
        URIHandle* handle =
            raptor_new_uri_relative_to_base_counted(world.handle, baseURI.handle, uri.ptr, uri.length);
        return fromNonnullHandle(handle);
    }
    static URI fromID(RaptorWorldWithoutFinalize world, URIWithoutFinalize baseURI, string id) {
        URIHandle* handle =
            raptor_new_uri_from_id(world.handle, baseURI.handle, id.toStringz);
        return fromNonnullHandle(handle);
    }
    static URI fromRDFConcept(RaptorWorldWithoutFinalize world, string name) {
        return fromNonnullHandle(raptor_new_uri_for_rdf_concept(world.handle, name.toStringz));
    }
    static URI fromXMLBase(URIWithoutFinalize oldURI) {
        return fromNonnullHandle(raptor_new_uri_for_xmlbase(oldURI.handle));
    }
    static URI forRetrieval(URIWithoutFinalize oldURI) {
        return fromNonnullHandle(raptor_new_uri_for_retrieval(oldURI.handle));
    }
}

string toRelativeURIString(URIWithoutFinalize baseURI, URIWithoutFinalize referenceURI) {
  char* str = raptor_uri_to_relative_uri_string(baseURI.handle, referenceURI.handle);
  if(!str) throw new NullRDFException();
  scope(exit) raptor_free_memory(str);
  return str.fromStringz.idup;
}

string resolveURIReference(string baseURI, string referenceURI) {
    immutable size_t bufferLength = baseURI.length + referenceURI.length + 1;
    char[] buffer = new char[bufferLength];
    immutable res = raptor_uri_resolve_uri_reference(baseURI.toStringz,
                                                     referenceURI.toStringz,
                                                     buffer.ptr,
                                                     bufferLength);
    if(!res) throw new RDFException();
    buffer.length = bufferLength;
    return cast(string)buffer;
}

string filenameToURIString(string filename) {
    char* result1 = raptor_uri_counted_filename_to_uri_string(filename.ptr, filename.length);
    if(!result1) throw new RDFException();
    scope(exit) raptor_free_memory(result1);
    return result1.fromStringz.idup;
}

bool uriStringIsAbsolute(string uriString) {
    immutable int res = raptor_uri_uri_string_is_absolute(uriString.toStringz);
    if(res < 0) throw new RDFException();
    return res > 0;
}

bool uriStringIsFileURI(string uriString) {
    return raptor_uri_uri_string_is_file_uri(uriString.toStringz) != 0;
}

string uriStringToFilename(string uriString) {
    char* result = raptor_uri_uri_string_to_filename(uriString.toStringz);
    if(!result) throw new RDFException();
    scope(exit) raptor_free_memory(result);
    return result.fromStringz.idup;
}

struct FilenameAndFragment {
    string filename, fragment;
}

FilenameAndFragment uriStringToFilenameAndFragment(string uriString) {
    char* fragment;
    char* filename =
        raptor_uri_uri_string_to_filename_fragment(uriString.toStringz, &fragment);
    if(!filename) throw new RDFException();
    scope(exit) {
        raptor_free_memory(filename);
        if(fragment) raptor_free_memory (fragment);
    }
    return FilenameAndFragment(filename.fromStringz.idup, fragment.fromStringz.idup);
}

bool filenameExists(string filename) {
    immutable int result = raptor_uri_filename_exists(filename.toStringz);
    if(result < 0) throw new RDFException();
    return result != 0;
}

unittest {
    RaptorWorld world = RaptorWorld.createAndOpen();

    // Conversions to/from string
    URI u = URI(world, "http://example.com");
    assert(u.toString == "http://example.com");
    // assert(u == "http://example.com"); does not work with DMD v2.080.1

    string URI_1 = "http://example.org/xyz";
    string URI_3 = "http://example.org/123";
    URI URI_1_Obj = URI.fromString(world, URI_1);
    URI URI_2_Obj = URI.fromString(world, URI_1);
    URI URI_3_Obj = URI.fromString(world, URI_3);
    URI URI_4_Obj = URI.fromString(world, "http://example.org");
    assert(URI_1_Obj.toString == URI_1, "Converting URI to string");
    assert(URI_1_Obj == URI_2_Obj, "Comparing identical URIs");
    assert(URI_1_Obj != URI_3_Obj, "Comparing different URIs");
    assert(URI.fromURIRelativeToBase(world, URI_1_Obj, "zxc").toString == "http://example.org/zxc", "Relative URI to base");
    assert(URI.forRetrieval(URI_4_Obj).toString == "http://example.org/", "URI for retrieval");
}
