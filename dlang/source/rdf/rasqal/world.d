module rdf.rasqal.world;

import rdf.auxiliary.handled_record;
import rdf.raptor.world;

struct RasqalWorldHandle;

private extern extern(C) {
    RasqalWorldHandle* rasqal_new_world();
    void rasqal_free_world(RasqalWorldHandle* world);
    int rasqal_world_open(RasqalWorldHandle* world);
    RaptorWorldHandle* rasqal_world_get_raptor(RasqalWorldHandle* world);
    void rasqal_world_set_raptor(RasqalWorldHandle* world, RaptorWorldHandle* raptor_world);
}

struct RasqalWorldWithoutFinalize {
    mixin WithoutFinalize!(RasqalWorldHandle,
                           RasqalWorldWithoutFinalize,
                           RasqalWorld);
    void open() {
        rasqal_world_open(handle);
    }
    @property RaptorWorldWithoutFinalize raptor() {
        return RaptorWorldWithoutFinalize.fromNonnullHandle(rasqal_world_get_raptor(this.handle));
    }
    @property void raptor(RaptorWorldWithoutFinalize world) {
        rasqal_world_set_raptor(handle, world.handle);
    }
    // TODO: Set_Log_Handler Set_Warning_Level Guess_Query_Results_Format_Name
}

struct RasqalWorld {
    mixin WithFinalize!(RasqalWorldHandle,
                        RasqalWorldWithoutFinalize,
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
