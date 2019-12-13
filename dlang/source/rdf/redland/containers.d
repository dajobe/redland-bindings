module rdf.redland.containers;

import rdf.auxiliary.handled_record;
import rdf.redland.world;
import rdf.redland.model;
import rdf.redland.node;
import rdf.redland.uri;

class WrongListFormat: RDFException {
    this(string msg, string file = __FILE__, size_t line = __LINE__) {
        super(msg, file, line);
    }
    this(string file = __FILE__, size_t line = __LINE__) {
        this("Wrong RDF list", file, line);
    }
}

NodeWithoutFinalize[] rdfList(RedlandWorldWithoutFinalize world, ModelWithoutFinalize model, NodeWithoutFinalize start) {
    NodeWithoutFinalize[] result;
    // Ugly and not very efficient code.
    auto firstURI = Node.fromURI(world, URI.fromString(world, "http://www.w3.org/1999/02/22-rdf-syntax-ns#first"));
    auto nilURI = Node.fromURI(world, URI.fromString(world, "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"));
    auto nextURI = Node.fromURI(world, URI.fromString(world, "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"));
    while (start != nilURI) {
        import std.array;
        auto first = model.getTargets(start, firstURI).array;
        if(first.length != 1) throw new WrongListFormat();
        result ~= first;
        auto next = model.getTargets(start, nextURI).array;
        if(next.length != 1) throw new WrongListFormat();
        start = next.front;
    }
    return result;
}

unittest {
    // see main.d instead
}