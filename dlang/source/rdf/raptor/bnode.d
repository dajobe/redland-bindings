module rdf.raptor.bnode;

import std.string;
import std.typecons;
import rdf.auxiliary.nullable_string;
import rdf.auxiliary.handled_record;
import rdf.auxiliary.user;
import rdf.raptor.memory;
import rdf.raptor.world;

private extern extern(C) {
    char* raptor_world_generate_bnodeid(RaptorWorldHandle* world);
    alias raptor_generate_bnodeid_handler = const(char)* function(char *data, char* userID);
    void raptor_world_set_generate_bnodeid_handler(RaptorWorldHandle* world,
                                                   void *user_data,
                                                   raptor_generate_bnodeid_handler handler);
    void raptor_world_set_generate_bnodeid_parameters(RaptorWorldHandle* world, const char *prefix, int base);
}

struct BNode {
    static string generateId(RaptorWorldWithoutFinalize world) {
        char* str = raptor_world_generate_bnodeid(world.handle);
        if(!str) throw new NullRDFException;
        scope(exit) raptor_free_memory(str);
        return str.fromStringz.idup;
    }
    static void setGenerateBnodeidParameters(RaptorWorldWithoutFinalize world,
                                             Nullable!string prefix,
                                             int base = 1)
    {
        raptor_world_set_generate_bnodeid_parameters(world.handle, myToStringz(prefix), base);
    }
}

class BNodeIDHandler : UnmovableObject {
    abstract string do_handle(Nullable!string userID);
    private static extern(C) const(char)* handleImpl(char* data, char* userID) {
        scope(exit) {
            if(userID) raptor_free_memory(userID);
        }
        Nullable!string userID2;
        if(userID) userID2 = cast(string)fromStringz(userID);
        return (cast(BNodeIDHandler)data).do_handle(userID2).toStringz;
    }
    final void set(RaptorWorldWithoutFinalize world) {
        raptor_world_set_generate_bnodeid_handler(world.handle, cast(void*)this, &handleImpl);
    }
}
