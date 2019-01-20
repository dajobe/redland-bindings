module rdf.rasqal.world;

import std.typecons;
import rdf.auxiliary.handled_record;
import rdf.auxiliary.nullable_string;
import rdf.raptor.world;
import rdf.raptor.uri;

struct RasqalWorldHandle;

private extern extern(C) {
    RasqalWorldHandle* rasqal_new_world();
    void rasqal_free_world(RasqalWorldHandle* world);
    int rasqal_world_open(RasqalWorldHandle* world);
    RaptorWorldHandle* rasqal_world_get_raptor(RasqalWorldHandle* world);
    void rasqal_world_set_raptor(RasqalWorldHandle* world, RaptorWorldHandle* raptor_world);
    int rasqal_world_set_warning_level(RasqalWorldHandle* world, uint warning_level);
    const(char*) rasqal_world_guess_query_results_format_name(RasqalWorldHandle* world,
                                                              URIHandle* uri,
                                                              const char *mime_type,
                                                              const char *buffer,
                                                              size_t len,
                                                              const char *identifier);
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
    @property void warningLevel(uint level) {
        if(rasqal_world_set_warning_level(handle, level) != 0)
            throw new RDFException();
    }
    Nullable!string guessQueryResultsFormatName(URIWithoutFinalize uri,
                                                Nullable!string mimeType,
                                                Nullable!string buffer,
                                                Nullable!string identifier)
    {
        const char* result = rasqal_world_guess_query_results_format_name(handle,
                                                                          uri.handle,
                                                                          mimeType.myToStringz,
                                                                          buffer.myToStringz,
                                                                          buffer.myLength,
                                                                          identifier.myToStringz);
        return result.myFromStringz;
    }
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
