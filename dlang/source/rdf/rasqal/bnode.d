module rdf.rasqal.bnode;

import std.typecons;
import std.string;
import rdf.auxiliary.handled_record;
import rdf.auxiliary.nullable_string;
import rdf.auxiliary.user;
import rdf.rasqal.memory;
import rdf.rasqal.world;

private extern extern(C) {
    alias rasqal_generate_bnodeid_handler = const(char*) function(RasqalWorldHandle* world,
                                                                  void *user_data,
                                                                  char *user_bnodeid);
    int rasqal_world_set_generate_bnodeid_handler(RasqalWorldHandle* world,
                                                  void *user_data,
                                                  rasqal_generate_bnodeid_handler handler);
    int rasqal_world_set_default_generate_bnodeid_parameters(RasqalWorldHandle* world,
                                                             const char *prefix,
                                                             int base);
}

class BNodeIDHandler : UnmovableObject {
    this(RasqalWorldWithoutFinalize world) {
        rasqal_world_set_generate_bnodeid_handler(world.handle, cast(void*)this, &bnodeIDHandleImpl);
    }
    abstract string doHandle(RasqalWorldWithoutFinalize world, Nullable!string userID);
    static extern(C) const(char*)
    bnodeIDHandleImpl(RasqalWorldHandle* world, void* data, char* userID) {
        scope(exit) rasqal_free_memory(userID);
        return (cast(BNodeIDHandler)data).doHandle(
            RasqalWorldWithoutFinalize.fromNonnullHandle(world),
            userID.myFromStringz).toStringz;
   }
}

void setDefaultGenerateBnodeidParameters(RasqalWorldWithoutFinalize world,
                                         Nullable!string prefix,
                                         int base)
{
    immutable int result =
        rasqal_world_set_default_generate_bnodeid_parameters(world.handle, prefix.myToStringz, base);
    if(result != 0)
        throw new RDFException();
}
