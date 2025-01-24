module rdf.redland.world;

import std.string;
import rdf.auxiliary.handled_record;
import rdf.raptor.world;
import rdf.rasqal.world;
import rdf.redland.uri;
import rdf.redland.node;

struct RedlandWorldHandle;

private extern extern(C) {
    void librdf_free_world(RedlandWorldHandle* world);
    RedlandWorldHandle* librdf_new_world();
    void librdf_world_open(RedlandWorldHandle* world);
    RaptorWorldHandle* librdf_world_get_raptor(RedlandWorldHandle* world);
    RasqalWorldHandle* librdf_world_get_rasqal(RedlandWorldHandle* world);
    void librdf_world_set_raptor(RedlandWorldHandle* world, RaptorWorldHandle* raptor_world_ptr);
    void librdf_world_set_rasqal(RedlandWorldHandle* world, RasqalWorldHandle* rasqal_world_ptr);
    void librdf_world_set_digest(RedlandWorldHandle* world, const char *name);
    NodeHandle* librdf_world_get_feature(RedlandWorldHandle* world, URIHandle* feature);
    int librdf_world_set_feature(RedlandWorldHandle* world, URIHandle* feature, NodeHandle* value);
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
    @property RasqalWorldWithoutFinalize rasqal() const {
         return RasqalWorldWithoutFinalize.fromNonnullHandle(librdf_world_get_rasqal(handle));
    }
    @property void raptor(RaptorWorldWithoutFinalize raptorWorld) {
        librdf_world_set_raptor(handle, raptorWorld.handle);
    }
    @property RaptorWorldWithoutFinalize raptor() const {
         return RaptorWorldWithoutFinalize.fromNonnullHandle(librdf_world_get_raptor(handle));
    }
    @property void digest(string name) const {
        librdf_world_set_digest(handle, name.toStringz);
    }
    @property Node feature(URIWithoutFinalize feature) const {
        return Node.fromHandle(librdf_world_get_feature(handle, feature.handle));
    }
    @property void feature(URIWithoutFinalize feature, NodeWithoutFinalize value) {
        if(librdf_world_set_feature(handle, feature.handle, value.handle) != 0)
            throw new RDFException();
    }
    // librdf_world_set_error() and librdf_world_set_warning() deliberately not implemented.

    // I deliberately not implement librdf_world_set_raptor_init_handler() and
    // librdf_world_set_rasqal_init_handler().
    // I recommend to use properties `raptor` and `rasqal` instead.
}

struct RedlandWorld {
    mixin WithFinalize!(RedlandWorldHandle,
                        RedlandWorldWithoutFinalize,
                        RedlandWorld,
                        librdf_free_world,
                        librdf_new_world);
    static RedlandWorld createAndOpen() {
        RedlandWorld world = create();
        world.open();
        return world;
    }
}

