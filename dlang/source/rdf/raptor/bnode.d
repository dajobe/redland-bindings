module rdf.raptor.bnode;

import std.string;
import rdf.auxiliary.handled_record;
import rdf.auxiliary.user;
import rdf.raptor.memory;
import rdf.raptor.world;

private extern extern(C) {
    char* raptor_world_generate_bnodeid(Dummy* world);
}

struct BNode {
    static string generateId(RaptorWorldWithoutFinalize world) {
        char* str = raptor_world_generate_bnodeid(world.handle);
        if(!str) throw new NullRDFException;
        string dup;
        try {
            dup = fromStringz(str).idup;
        }
        finally {
            raptor_free_memory(str);
        }
        return dup;
    }
}

class UserBNode : UserObject!BNode {
}

// TODO
