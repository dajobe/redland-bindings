module rdf.redland.node;

import std.string;
import rdf.auxiliary.handled_record;
static import rdf.raptor.term;
import rdf.redland.world;
import rdf.redland.uri;

struct NodeHandle;

alias NodeKind = rdf.raptor.term.TermKind;

private extern extern(C) {
    void librdf_free_node(NodeHandle* node);
    NodeHandle* librdf_new_node_from_node(NodeHandle* node);
    NodeHandle* librdf_node_decode(RedlandWorldHandle* world,
                                   size_t *size_p,
                                   char *buffer,
                                   size_t length);
    size_t librdf_node_encode(NodeHandle* node, char *buffer, size_t length);
    int librdf_node_equals(NodeHandle* first_node, NodeHandle* second_node);
    char* librdf_node_get_counted_blank_identifier(NodeHandle* node, size_t *len_p);
    int librdf_node_get_li_ordinal(NodeHandle* node);
    char* librdf_node_get_literal_value_as_counted_string(NodeHandle* node, size_t *len_p);
    char* librdf_node_get_literal_value_as_latin1(NodeHandle* node);
    URIHandle* librdf_node_get_literal_value_datatype_uri(NodeHandle* node);
    int librdf_node_get_literal_value_is_wf_xml(NodeHandle* node);
}

struct NodeWithoutFinalize {
    mixin WithoutFinalize!(NodeHandle,
                           NodeWithoutFinalize,
                           Node,
                           librdf_new_node_from_node);
    @property rdf.raptor.term.Term toRaptor() { // FIXME: also dup() in Ada
      return rdf.raptor.term.TermWithoutFinalize.fromHandle(cast(rdf.raptor.term.TermHandle*)handle).dup;
    }
    string encode() {
        size_t length = librdf_node_encode(handle, null, 0);
        char[] buffer = new char[length];
        cast(void)librdf_node_encode(handle, buffer.ptr, length);
        return cast(string)buffer;
    }
    bool opEquals(NodeWithoutFinalize other) {
        return librdf_node_equals(handle, other.handle) != 0;
    }
    @property string blankIdentifier() {
        size_t length;
        char* buffer = librdf_node_get_counted_blank_identifier(handle, &length);
        return buffer[0..length].idup;
    }
    @property uint liOrdinal() {
        int result = librdf_node_get_li_ordinal(handle);
        if(result <= 0) throw new RDFException();
        return result;
    }
    string toString() {
        size_t length;
        char* buffer = librdf_node_get_literal_value_as_counted_string(handle, &length);
        if(!buffer) throw new RDFException();
        return buffer[0..length].idup;
    }
    string asLatin1() {
        char* result = librdf_node_get_literal_value_as_latin1(handle);
        if(!result) throw new RDFException();
        return result.fromStringz.idup;
    }
    @property URIWithoutFinalize datatypeURI() {
        return URIWithoutFinalize.fromHandle(librdf_node_get_literal_value_datatype_uri(handle));
    }
    @property isWFXML() {
        return librdf_node_get_literal_value_is_wf_xml(handle) != 0;
    }
}

struct Node {
    mixin WithFinalize!(NodeHandle,
                        NodeWithoutFinalize,
                        Node,
                        librdf_free_node);
    static Node fromRaptor(rdf.raptor.term.TermWithoutFinalize uri) { // FIXME: also dup() in Ada
        return NodeWithoutFinalize.fromHandle(cast(NodeHandle*)uri.handle).dup;
    }
    bool opEquals(NodeWithoutFinalize other) {
        return librdf_node_equals(handle, other.handle) != 0;
    }
}

// TODO: Stopped at Get_Language

