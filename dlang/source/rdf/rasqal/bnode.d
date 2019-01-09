module rdf.rasqal.bnode;

import std.typecons;
import std.string;
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
}

class BNodeIDHandler : UnmovableObject {
    this(RasqalWorldWithoutFinalize world) {
        rasqal_world_set_generate_bnodeid_handler(world.handle, cast(void*)this, &C_BNode_ID_Handle_Impl);
    }
    abstract string doHandle(RasqalWorldWithoutFinalize world, Nullable!string userID);
    static extern(C) const(char*)
    C_BNode_ID_Handle_Impl(RasqalWorldHandle* world, void* data, char* userID) {
        // TODO: Check this code
        scope(exit) rasqal_free_memory(userID);
        return (cast(BNodeIDHandler*)data).doHandle(
            RasqalWorldWithoutFinalize.fromNonnullHandle(world),
            (cast(char*)userID).myFromStringz).toStringz;
   }
}

// TODO:
// void setDefaultGenerateBnodeidParameters(RasqalWorldWithoutFinalize world,
//                                          Nullable!string prefix,
//                                          int base)
// {
//       C_Prefix: constant chars_ptr := New_String(Prefix);
//       Result: constant int :=
//         rasqal_world_set_default_generate_bnodeid_parameters(Get_Handle(World), C_Prefix, Base);
//    begin
//       RDF.Rasqal.Memory.Rasqal_Free_Memory(C_Prefix);
//       if Result /= 0 then
//          raise RDF.Auxiliary.RDF_Exception;
// }