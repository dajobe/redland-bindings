module rdf.raptor.world;

import rdf.auxiliary.handled_record;

private extern extern(C) Dummy* raptor_new_world();
private extern extern(C) void raptor_free_world(Dummy* world);

struct RaptorWorldWithoutFinalization {
    mixin WithoutFinalization!(RaptorWorldWithoutFinalization,
                               RaptorWorld,
                               raptor_free_world,
                               raptor_new_world);
}

struct RaptorWorld {
    mixin WithFinalization!(RaptorWorldWithoutFinalization,
                            RaptorWorld,
                            raptor_free_world,
                            raptor_new_world);
}

enum RaptorFlagType : char { LibxmlErrorSave = 1,
                             LibxmlStructuredErrorSave = 2,
                             URIInterning = 3,
                             WWWSkipInitFinish = 4 }

struct FlagAndValue {
    RaptorFlagType flag;
    bool value;
}

unittest {
    assert(1);
}

// TODO
