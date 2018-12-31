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

private extern extern(C) {
    Dummy* raptor_new_world_internal(uint _version);
    void raptor_free_world(Dummy* world);
    void raptor_world_open(Dummy* world);
    int raptor_world_set_flag(Dummy* raptor_world, int flag, int value);
}

private Dummy* raptor_new_world() {
    import rdf.raptor.constants : version_decimal;
    // FIXME
    import std.stdio;
    writeln("XX");
    return raptor_new_world_internal(version_decimal);
}

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
    static RaptorWorld create_and_open() {
        RaptorWorld world = create();
        world.open();
        return world;
    }
    static RaptorWorld create_and_open(FlagAndValue[] flags) {
        RaptorWorld world = create();
        world.open(flags);
        return world;
    }
}

// Not implemented: set_libxslt_security_preferences()

unittest {
//     RaptorWorld defaultWorld = RaptorWorld.create_and_open();
//     RaptorWorld worldWithSomeFlags =
//         RaptorWorld.create_and_open([FlagAndValue(RaptorFlagType.URIInterning, false)]);
    // TODO:
//     World2: Rasqal_World_Type := Open;
//     World: Raptor_World_Type_Without_Finalize := Get_Raptor(World2) with Unreferenced;
}
