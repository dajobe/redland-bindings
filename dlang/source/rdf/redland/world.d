module rdf.redland.world;

import rdf.auxiliary.handled_record;
import rdf.raptor.world;
import rdf.rasqal.world;

struct RedlandWorldHandle;

private extern extern(C) {
    void librdf_free_world(RedlandWorldHandle* world);
    RedlandWorldHandle* librdf_new_world();
    void librdf_world_open(RedlandWorldHandle* world);
    RaptorWorldHandle* librdf_world_get_raptor(RedlandWorldHandle* world);
    RasqalWorldHandle* librdf_world_get_rasqal(RedlandWorldHandle* world);
    void librdf_world_set_raptor(RedlandWorldHandle* world, RaptorWorldHandle* raptor_world_ptr);
    void librdf_world_set_rasqal(RedlandWorldHandle* world, RasqalWorldHandle* rasqal_world_ptr);
}

enum {
    featureGenidBase = "http://feature.librdf.org/genid-base",
    featureGenidCounter = "http://feature.librdf.org/genid-counter",
}

struct RedlandWorldWithoutFinalize {
    mixin WithoutFinalize!(RedlandWorldHandle,
                           RedlandWorldWithoutFinalize,
                           RedlandWorld);
    void open() {
        librdf_world_open(handle);
    }
    @property void rasqal(RasqalWorldWithoutFinalize rasqalWorld) {
        librdf_world_set_rasqal(handle, rasqalWorld.handle);
    }
    @property RasqalWorldWithoutFinalize rasqal() {
         return RasqalWorldWithoutFinalize.fromNonnullHandle(librdf_world_get_rasqal(handle));
    }
    @property void raptor(RaptorWorldWithoutFinalize raptorWorld) {
        librdf_world_set_raptor(handle, raptorWorld.handle);
    }
    @property RaptorWorldWithoutFinalize raptor() {
         return RaptorWorldWithoutFinalize.fromNonnullHandle(librdf_world_get_raptor(handle));
    }
}

struct RedlandWorld {
    mixin WithFinalize!(RedlandWorldHandle,
                        RedlandWorldWithoutFinalize,
                        RedlandWorld,
                        librdf_free_world,
                        librdf_new_world);
}

// TODO: Stopped at Set_Digest

