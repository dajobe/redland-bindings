module rdf.redland.node;

import rdf.auxiliary.handled_record;
static import rdf.raptor.term;

struct NodeHandle;

alias NodeKind = rdf.raptor.term.TermKind;

private extern extern(C) {
    void librdf_free_node(NodeHandle* node);
    NodeHandle* librdf_new_node_from_node(NodeHandle* node);
}

struct NodeWithoutFinalize {
    mixin WithoutFinalize!(NodeHandle,
                           NodeWithoutFinalize,
                           Node,
                           librdf_new_node_from_node);
    @property rdf.raptor.term.Term toRaptor() { // FIXME: also dup() in Ada
      return rdf.raptor.term.TermWithoutFinalize.fromHandle(cast(rdf.raptor.term.TermHandle*)handle).dup;
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
}

// TODO: Stopped at Encode

