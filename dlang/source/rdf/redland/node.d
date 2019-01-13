module rdf.redland.node;

import rdf.auxiliary.handled_record;

struct NodeHandle;

private extern extern(C) {
    void librdf_free_node(NodeHandle* node);
    NodeHandle* librdf_new_node_from_node(NodeHandle* node);
}

struct NodeWithoutFinalize {
    mixin WithoutFinalize!(NodeHandle,
                           NodeWithoutFinalize,
                           Node,
                           librdf_new_node_from_node);
}

struct Node {
    mixin WithFinalize!(NodeHandle,
                        NodeWithoutFinalize,
                        Node,
                        librdf_free_node);
}

// TODO: Stopped at:
// subtype Node_Kind is RDF.Raptor.Term.Term_Kind;

