module rdf.redland.model;

import std.typecons;
import std.string;
import rdf.auxiliary.handled_record;
import rdf.redland.world;

struct ModelHandle;

private extern extern(C) {
    void librdf_free_model(ModelHandle* model);
    int librdf_model_enumerate(RedlandWorldHandle* world,
                               const int counter,
                               const char **name,
                               const char **label);
}

struct ModelInfo {
    string name, label;
}

struct ModelWithoutFinalize {
    mixin WithoutFinalize!(ModelHandle,
                           ModelWithoutFinalize,
                           Model);
    Nullable!ModelInfo enumerateModels(RedlandWorldWithoutFinalize world, uint counter) {
        char* name, label;
        int Result = librdf_model_enumerate(world.handle, counter, &name, &label);
        return Nullable!ModelInfo(ModelInfo(name.fromStringz.idup, label.fromStringz.idup));
    }
}

struct Model {
    mixin WithFinalize!(ModelHandle,
                        ModelWithoutFinalize,
                        Model,
                        librdf_free_model);
}

// TODO: Stopped at Enumerate_Models_Cursor

