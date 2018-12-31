module rdf.raptor.world;

import rdf.auxiliary.handled_record;

enum RaptorFlagType : char { LibxmlErrorSave = 1,
                             LibxmlStructuredErrorSave = 2,
                             URIInterning = 3,
                             WWWSkipInitFinish = 4 }

struct FlagAndValue {
    RaptorFlagType flag;
    bool value;
}

private extern extern(C) Dummy* raptor_new_world();
private extern extern(C) void raptor_free_world(Dummy* world);
private extern extern(C) void raptor_world_open(Dummy* world);
private extern extern(C) int raptor_world_set_flag(Dummy *world, int flag, int value);

struct RaptorWorldWithoutFinalize {
    mixin WithoutFinalize!(RaptorWorldWithoutFinalize,
                           RaptorWorld,
                           raptor_free_world,
                           raptor_new_world);
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
    mixin WithFinalize!(RaptorWorldWithoutFinalize,
                        RaptorWorld,
                        raptor_free_world,
                        raptor_new_world);
    static RaptorWorld open() {
        RaptorWorld world = create();
        world.open();
        return world;
    }
    static RaptorWorld open(FlagAndValue[] flags) {
        RaptorWorld world = create();
        world.open(flags);
        return world;
    }
}

// Not implemented: set_libxslt_security_preferences()

// TODO

unittest {
    assert(1);
}
