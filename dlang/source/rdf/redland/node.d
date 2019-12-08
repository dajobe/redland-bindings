module rdf.redland.node;

import std.string;
import std.stdio : File, FILE;
import rdf.auxiliary.handled_record;
import rdf.raptor.iostream;
static import rdf.raptor.term;
import rdf.redland.world;
import rdf.redland.uri;

struct NodeHandle;

alias NodeKind = rdf.raptor.term.TermKind;

private extern extern(C) {
    void librdf_free_node(NodeHandle* node);
    NodeHandle* librdf_new_node_from_node(NodeHandle* node);
    size_t librdf_node_encode(NodeHandle* node, char *buffer, size_t length);
    int librdf_node_equals(NodeHandle* first_node, NodeHandle* second_node);
    char* librdf_node_get_counted_blank_identifier(NodeHandle* node, size_t *len_p);
    int librdf_node_get_li_ordinal(NodeHandle* node);
    char* librdf_node_get_literal_value_as_counted_string(NodeHandle* node, size_t *len_p);
    char* librdf_node_get_literal_value_as_latin1(NodeHandle* node);
    URIHandle* librdf_node_get_literal_value_datatype_uri(NodeHandle* node);
    int librdf_node_get_literal_value_is_wf_xml(NodeHandle* node);
    char* librdf_node_get_literal_value_language(NodeHandle* node);
    NodeKind librdf_node_get_type(NodeHandle* node);
    URIHandle* librdf_node_get_uri(NodeHandle* node);
    int librdf_node_is_blank(NodeHandle* node);
    int librdf_node_is_literal(NodeHandle* node);
    int librdf_node_is_resource(NodeHandle* node);
    void librdf_node_print(NodeHandle* node, FILE *fh);
    int librdf_node_write(NodeHandle* node, IOStreamHandle* iostr);
    NodeHandle* librdf_new_node(RedlandWorldHandle* world);
    NodeHandle* librdf_new_node_from_counted_blank_identifier(RedlandWorldHandle* world,
                                                              const char *identifier,
                                                              size_t identifier_len);
    NodeHandle* librdf_new_node_from_counted_uri_string(RedlandWorldHandle* world,
                                                        const char *uri_string,
                                                        size_t len);
    NodeHandle* librdf_new_node_from_literal(RedlandWorldHandle* world,
                                             const char *string,
                                             const char *xml_language,
                                             int is_wf_xml);
    NodeHandle* librdf_new_node_from_normalised_uri_string(RedlandWorldHandle* world,
                                                           const char *uri_string,
                                                           URIHandle* source_uri,
                                                           URIHandle* base_uri);
    NodeHandle* librdf_new_node_from_typed_counted_literal(RedlandWorldHandle* world,
                                                           const char *value,
                                                           size_t value_len,
                                                           const char *xml_language,
                                                           size_t xml_language_len,
                                                           URIHandle* datatype_uri);
    NodeHandle* librdf_new_node_from_uri(RedlandWorldHandle* world, URIHandle* uri);
    NodeHandle* librdf_new_node_from_uri_local_name(RedlandWorldHandle* world,
                                                    URIHandle* uri,
                                                    const char *local_name);
    NodeHandle* librdf_node_decode(RedlandWorldHandle* world,
                                   size_t *size_p,
                                   const char *buffer,
                                   size_t length);
}

struct NodeWithoutFinalize {
    mixin WithoutFinalize!(NodeHandle,
                           NodeWithoutFinalize,
                           Node,
                           librdf_new_node_from_node);
    bool opEquals(NodeWithoutFinalize other) const {
        return librdf_node_equals(handle, other.handle) != 0;
    }
    @property rdf.raptor.term.Term toRaptor() const { // FIXME: also dup() in Ada
      return rdf.raptor.term.TermWithoutFinalize.fromHandle(cast(rdf.raptor.term.TermHandle*)handle).dup;
    }
    string encode() const {
        size_t length = librdf_node_encode(handle, null, 0);
        char[] buffer = new char[length];
        cast(void)librdf_node_encode(handle, buffer.ptr, length);
        return cast(string)buffer;
    }
    @property string blankIdentifier() const {
        size_t length;
        char* buffer = librdf_node_get_counted_blank_identifier(handle, &length);
        return buffer[0..length].idup;
    }
    @property uint liOrdinal() const {
        int result = librdf_node_get_li_ordinal(handle);
        if(result <= 0) throw new RDFException();
        return result;
    }
    string toString() const {
        size_t length;
        char* buffer = librdf_node_get_literal_value_as_counted_string(handle, &length);
        if(!buffer) throw new RDFException();
        return buffer[0..length].idup;
    }
    string asLatin1() const {
        char* result = librdf_node_get_literal_value_as_latin1(handle);
        if(!result) throw new RDFException();
        return result.fromStringz.idup;
    }
    @property URIWithoutFinalize datatypeURI() const {
        return URIWithoutFinalize.fromHandle(librdf_node_get_literal_value_datatype_uri(handle));
    }
    @property isWFXML() const {
        return librdf_node_get_literal_value_is_wf_xml(handle) != 0;
    }
    /// Return "" if no language
    @property string language() const {
        char* ptr = librdf_node_get_literal_value_language(handle);
        return ptr ? ptr.fromStringz.idup : "";
    }
    @property NodeKind type() const {
        return librdf_node_get_type(handle);
    }
    @property URIWithoutFinalize uri() const {
        return URIWithoutFinalize.fromNonnullHandle(librdf_node_get_uri(handle));
    }
    @property bool isBlank() const {
        return librdf_node_is_blank(handle) != 0;
    }
    @property bool isLiteral() const {
        return librdf_node_is_literal(handle) != 0;
    }
    @property bool isResource() const {
        return librdf_node_is_resource(handle) != 0;
    }
    // librdf_node_write() deliberately not bound
    void print(File file) const {
        librdf_node_print(handle, file.getFP);
    }
    void write(IOStreamWithoutFinalize stream) const {
        if(librdf_node_write(handle, stream.handle) != 0)
             throw new RDFException();
    }
    // TODO
    //string formatAsString() const {
    //    StreamToString stream;
    //    write(stream.record);
    //    return stream.value();
    //}
}

struct Node {
    mixin WithFinalize!(NodeHandle,
                        NodeWithoutFinalize,
                        Node,
                        librdf_free_node);
    bool opEquals(Node other) const {
        return librdf_node_equals(handle, other.handle) != 0;
    }
    static Node fromRaptor(rdf.raptor.term.TermWithoutFinalize uri) { // FIXME: also dup() in Ada
        return NodeWithoutFinalize.fromHandle(cast(NodeHandle*)uri.handle).dup;
    }
    static Node create(RedlandWorldWithoutFinalize world) {
        return Node.fromNonnullHandle(librdf_new_node(world.handle));
    }
    /// "No identifier" is signified by empty string
    static Node fromBlankIdentifier(RedlandWorldWithoutFinalize world, string id) {
        const char* ptr = id == "" ? null : id.ptr;
        NodeHandle* handle =
            librdf_new_node_from_counted_blank_identifier(world.handle, ptr, id.length);
        return Node.fromNonnullHandle(handle);
    }
    static Node fromURIString(RedlandWorldWithoutFinalize world, string uriString) {
        NodeHandle* handle =
            librdf_new_node_from_counted_uri_string(world.handle, uriString.ptr, uriString.length);
        return Node.fromNonnullHandle(handle);
    }
    static Node fromLiteral(RedlandWorldWithoutFinalize world,
                            string text,
                            string language,
                            bool isXML = false)
    {
        NodeHandle* handle =
            librdf_new_node_from_literal(world.handle,
                                         text.toStringz,
                                         language.toStringz,
                                         isXML ? 1 : 0);
        return Node.fromNonnullHandle(handle);
    }
    static fromNormalisedURIString(RedlandWorldWithoutFinalize world,
                                   string uri,
                                   URIWithoutFinalize sourceURI,
                                   URIWithoutFinalize baseURI)
    {
        NodeHandle* handle =
            librdf_new_node_from_normalised_uri_string(world.handle,
                                                       uri.toStringz,
                                                       sourceURI.handle,
                                                       baseURI.handle);
        return Node.fromNonnullHandle(handle);
    }
    static fromTypedLiteral(RedlandWorldWithoutFinalize world,
                            string text,
                            string language,
                            URIWithoutFinalize datatype)
    {
        NodeHandle* handle =
            librdf_new_node_from_typed_counted_literal(world.handle,
                                                       text.ptr,
                                                       text.length,
                                                       language.ptr,
                                                       language.length,
                                                       datatype.handle);
        return Node.fromNonnullHandle(handle);
    }
    static Node fromURI(RedlandWorldWithoutFinalize world, URIWithoutFinalize uri) {
        return Node.fromNonnullHandle(librdf_new_node_from_uri(world.handle, uri.handle));
    }
    static Node fromURILocalName(RedlandWorldWithoutFinalize world,
                                 URIWithoutFinalize uri,
                                 string localName)
    {
        NodeHandle* handle =
            librdf_new_node_from_uri_local_name(world.handle, uri.handle, localName.toStringz);
        return Node.fromNonnullHandle(handle);
    }
    static Node decode(RedlandWorldWithoutFinalize world, string buffer) {
        NodeHandle* handle = librdf_node_decode(world.handle, null, buffer.ptr, buffer.length);
        return Node.fromNonnullHandle(handle);
    }
}

