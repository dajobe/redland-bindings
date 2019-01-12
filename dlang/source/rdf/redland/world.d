module rdf.redland.world;

import rdf.auxiliary.handled_record;

struct RedlandWorldHandle;

private extern extern(C) {
    void librdf_free_world(RedlandWorldHandle* world);
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
                        librdf_free_world);
}

