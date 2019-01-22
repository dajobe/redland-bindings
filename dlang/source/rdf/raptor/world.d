module rdf.raptor.world;

import rdf.auxiliary.handled_record;

struct RaptorWorldHandle;

enum RaptorFlagType : char { libxmlErrorSave = 1,
                             libxmlStructuredErrorSave = 2,
                             uriInterning = 3,
                             wwwSkipInitFinish = 4 }

struct FlagAndValue {
    RaptorFlagType flag;
    bool value;
}

private extern extern(C) {
    RaptorWorldHandle* raptor_new_world_internal(uint _version);
    void raptor_free_world(RaptorWorldHandle* world);
    void raptor_world_open(RaptorWorldHandle* world);
    int raptor_world_set_flag(RaptorWorldHandle* raptor_world, int flag, int value);
}

private RaptorWorldHandle* raptor_new_world() {
    import rdf.raptor.constants : versionDecimal;
    return raptor_new_world_internal(versionDecimal);
}

struct RaptorWorldWithoutFinalize {
    mixin WithoutFinalize!(RaptorWorldHandle,
                           RaptorWorldWithoutFinalize,
                           RaptorWorld);
    void open() {
        raptor_world_open(handle);
    }
    void open(FlagAndValue[] flags) {
        setFlags(flags);
        open();
    }
    void setFlag(RaptorFlagType flag, bool value) {
        raptor_world_set_flag(handle, flag, value ? 1 : 0);
    }
    void setFlags(FlagAndValue[] flags) {
        foreach(f; flags)
            setFlag(f.flag, f.value);
    }
}

struct RaptorWorld {
    mixin WithFinalize!(RaptorWorldHandle,
                        RaptorWorldWithoutFinalize,
                        RaptorWorld,
                        raptor_free_world,
                        raptor_new_world);
    static RaptorWorld createAndOpen() {
        RaptorWorld world = create();
        world.open();
        return world;
    }
    static RaptorWorld createAndOpen(FlagAndValue[] flags) {
        RaptorWorld world = create();
        world.open(flags);
        return world;
    }
}

// Not implemented: set_libxslt_security_preferences()

unittest {
    RaptorWorld defaultWorld = RaptorWorld.createAndOpen();
    RaptorWorld worldWithSomeFlags =
        RaptorWorld.createAndOpen([FlagAndValue(RaptorFlagType.uriInterning, false)]);
}
