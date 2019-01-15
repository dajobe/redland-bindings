module rdf.redland.model;

import rdf.auxiliary.handled_record;

struct ModelHandle;

private extern extern(C) {
    void librdf_free_model(ModelHandle* model);
}

struct ModelWithoutFinalize {
    mixin WithoutFinalize!(ModelHandle,
                           ModelWithoutFinalize,
                           Model);
}

struct Model {
    mixin WithFinalize!(ModelHandle,
                        ModelWithoutFinalize,
                        Model,
                        librdf_free_model);
}

