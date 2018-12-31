module rdf.rasqal.world;

import rdf.auxiliary.handled_record;
import rdf.raptor.world;

private extern extern(C) {
    Dummy* rasqal_new_world();
    void rasqal_free_world(Dummy* world);
    int rasqal_world_open(Dummy* world);
    Dummy* rasqal_world_get_raptor(Dummy* world);
    void rasqal_world_set_raptor(Dummy* world, Dummy* raptor_world_ptr);
}

struct RasqalWorldWithoutFinalize {
    mixin WithoutFinalize!(RasqalWorldWithoutFinalize,
                           RasqalWorld);
    void open() {
        rasqal_world_open(handle);
    }
    @property RaptorWorldWithoutFinalize raptor() {
        return RaptorWorldWithoutFinalize.from_nonnull_handle(rasqal_world_get_raptor(this.handle));
    }
    @property void raptor(RaptorWorldWithoutFinalize world) {
        rasqal_world_set_raptor(handle, world.handle);
    }
    // TODO: Set_Log_Handler Set_Warning_Level Guess_Query_Results_Format_Name
}

struct RasqalWorld {
    mixin WithFinalize!(RasqalWorldWithoutFinalize,
                        RasqalWorld,
                        rasqal_free_world,
                        rasqal_new_world);
    static RasqalWorld createAndOpen() {
        RasqalWorld world = create();
        world.open();
        return world;
    }
}

unittest {
    RasqalWorld world2 = RasqalWorld.createAndOpen();
    RaptorWorldWithoutFinalize world = world2.raptor;
}
