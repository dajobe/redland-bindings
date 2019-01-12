module rdf.redland.world;

import rdf.auxiliary.handled_record;

struct RedlandWorldHandle;

private extern extern(C) {
    void librdf_free_world(RedlandWorldHandle* world);
    RedlandWorldHandle* librdf_new_world();
    void librdf_world_open(RedlandWorldHandle* world);
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
}

struct RedlandWorld {
    mixin WithFinalize!(RedlandWorldHandle,
                        RedlandWorldWithoutFinalize,
                        RedlandWorld,
                        librdf_free_world,
                        librdf_new_world);
}

// TODO: Stopped at Set_Rasqal

