module rdf.rasqal.features;

import rdf.raptor.uri;
import rdf.rasqal.world;

enum FeatureType { Unknown = -1, No_Net = 0, Rand_Seed = 1 }

private extern extern(C) {
    FeatureType rasqal_feature_from_uri(RasqalWorldHandle* world, URIHandle* uri);
}

FeatureType Feature_From_URI(RasqalWorldWithoutFinalize world, URIWithoutFinalize uri) {
    return rasqal_feature_from_uri(world.handle, uri.handle);
}

// TODO: Stopped at Feature_Value_Type
