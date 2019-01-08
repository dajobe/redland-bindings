module rdf.rasqal.features;

import rdf.raptor.uri;
import rdf.rasqal.world;

enum FeatureType { Unknown = -1, No_Net = 0, Rand_Seed = 1 }

enum FeatureValueType { Other = -1, Integer_Type = 0, String_Type = 1 }

private extern extern(C) {
    FeatureType rasqal_feature_from_uri(RasqalWorldHandle* world, URIHandle* uri);
    int rasqal_feature_value_type(const FeatureType feature);
}

FeatureType Feature_From_URI(RasqalWorldWithoutFinalize world, URIWithoutFinalize uri) {
    return rasqal_feature_from_uri(world.handle, uri.handle);
}

FeatureValueType getType(FeatureType feature) {
    return cast(FeatureValueType)rasqal_feature_value_type(feature);
}

// TODO: Stopped at Feature_Description
