module rdf.raptor.uri;

import std.string;
import std.stdio : FILE, File;
import rdf.auxiliary.handled_record;
import rdf.raptor.memory;
import rdf.raptor.world;

private extern extern(C) {
    void raptor_free_uri(Dummy* uri);
    Dummy* raptor_uri_copy(Dummy* uri);
    int raptor_uri_compare(Dummy* uri1, Dummy* uri2);
    int raptor_uri_equals(Dummy* uri1, Dummy* uri2);
    char* raptor_uri_as_string(Dummy* uri);
    char* raptor_uri_to_relative_uri_string(Dummy* base_uri, Dummy* reference_uri);
    size_t raptor_uri_resolve_uri_reference(const char *base_uri,
                                            const char *reference_uri,
                                            char *buffer,
                                            size_t length);
    char* raptor_uri_counted_filename_to_uri_string(const char *filename, size_t filename_len);
    int raptor_uri_uri_string_is_absolute(const char *uri_string);
    int raptor_uri_uri_string_is_file_uri(const char *uri_string);
    char* raptor_uri_uri_string_to_filename(const char *uri_string);
    char* raptor_uri_uri_string_to_filename_fragment(const char *uri_string, char **fragment_p);
    int raptor_uri_print(const Dummy* uri, FILE* stream);
    Dummy* raptor_uri_get_world(Dummy* uri);
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
    /// Not supposed to be used, but included for completeness
    void print(File file) {
        if(raptor_uri_print(handle, file.getFP()) != 0) {
            //throw new IOStreamException(); // FIXME: Add this
        }
    }
    @property RaptorWorldWithoutFinalize world() {
        return RaptorWorldWithoutFinalize.from_handle(raptor_uri_get_world(handle));
    }
}

struct URI {
    mixin WithFinalize!(URIWithoutFinalize,
                        URI,
                        raptor_free_uri);
    mixin CompareHandles!(raptor_uri_equals, raptor_uri_compare);
}

string toRelativeURIString(URIWithoutFinalize baseURI, URIWithoutFinalize referenceURI) {
  char* str =
        raptor_uri_to_relative_uri_string(baseURI.handle, referenceURI.handle);
  if(!str) throw new NullRDFException();
  scope(exit) raptor_free_memory(str);
  return fromStringz(str).idup;
}

string resolveURIReference(string baseURI, string referenceURI) {
    immutable size_t bufferLength = baseURI.length + referenceURI.length + 1;
    char[] buffer = new char[bufferLength];
    immutable res = raptor_uri_resolve_uri_reference(toStringz(baseURI),
                                                      toStringz(referenceURI),
                                                      buffer.ptr,
                                                      bufferLength);
    if(!res) throw new RDFException();
    buffer.length = bufferLength;
    return cast(string)buffer;
}

string filenameToURIString(string filename) {
    char* result1 =
        raptor_uri_counted_filename_to_uri_string(filename.ptr, filename.length);
    if(!result1) throw new RDFException();
    scope(exit) raptor_free_memory(result1);
    return fromStringz(result1).idup;
}

bool uriStringIsAbsolute(string uriString) {
    immutable int res = raptor_uri_uri_string_is_absolute(toStringz(uriString));
    if(res < 0) throw new RDFException();
    return res > 0;
}

bool uriStringIsFileURI(string uriString) {
    return raptor_uri_uri_string_is_file_uri(toStringz(uriString)) != 0;
}

string uriStringToFilename(string uriString) {
    char* result1 = raptor_uri_uri_string_to_filename(toStringz(uriString));
    if(!result1) throw new RDFException();
    scope(exit) raptor_free_memory(result1);
    return fromStringz(result1).idup;
}

struct FilenameAndFragment {
    string filename, fragment;
}

FilenameAndFragment uriStringToFilenameAndFragment(string uriString) {
    char* fragment;
    char* filename =
        raptor_uri_uri_string_to_filename_fragment(toStringz(uriString), &fragment);
    if(!filename) throw new RDFException();
    scope(exit) {
        raptor_free_memory(filename);
        if(fragment) raptor_free_memory (fragment);
    }
    return FilenameAndFragment(fromStringz(filename).idup, fromStringz(fragment).idup);
}

// TODO: Stopped at function Write
