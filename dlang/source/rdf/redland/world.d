module rdf.redland.world;

import rdf.auxiliary.handled_record;

struct RedlandWorldHandle;

private extern extern(C) {
    void librdf_free_world(RedlandWorldHandle* world);
    RedlandWorldHandle* librdf_new_world();
}

enum {
    featureGenidBase = "http://feature.librdf.org/genid-base",
    featureGenidCounter = "http://feature.librdf.org/genid-counter",
}

struct RedlandWorldWithoutFinalize {
    mixin WithoutFinalize!(RedlandWorldHandle,
                           RedlandWorldWithoutFinalize,
                           RedlandWorld);
}

struct RedlandWorld {
    mixin WithFinalize!(RedlandWorldHandle,
                        RedlandWorldWithoutFinalize,
                        RedlandWorld,
                        librdf_free_world,
                        librdf_new_world);
}

// TODO: Stopped at Open

