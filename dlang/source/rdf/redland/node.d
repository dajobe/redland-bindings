module rdf.redland.node;

import rdf.auxiliary.handled_record;
static import rdf.raptor.term;
import rdf.redland.world;

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

// TODO: Stopped at As_String

