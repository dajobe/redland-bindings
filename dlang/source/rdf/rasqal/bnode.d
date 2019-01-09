module rdf.rasqal.bnode;

import std.typecons;
import std.string;
import rdf.auxiliary.nullable_string;
import rdf.rasqal.memory;
import rdf.rasqal.world;

class BNodeIDHandler {
   abstract string doHandle(RasqalWorldWithoutFinalize world, Nullable!string userID);
   static const(char*) C_BNode_ID_Handle_Impl(RasqalWorldHandle* world, void* data, char* userID) {
        // TODO: Check this code
        scope(exit) rasqal_free_memory(userID);
        return (cast(BNodeIDHandler*)data).doHandle(
            RasqalWorldWithoutFinalize.fromNonnullHandle(world),
            (cast(const char*)userID).myFromStringz).toStringz;
   }
   // TODO
}

