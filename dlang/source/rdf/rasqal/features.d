module rdf.rasqal.features;

import std.string;
import rdf.auxiliary.handled_record;
import rdf.raptor.uri;
import rdf.rasqal.world;

enum FeatureType { unknown = -1, noNet = 0, randSeed = 1 }

enum FeatureValueType { other = -1, integerType = 0, stringType = 1 }

private extern extern(C) {
    FeatureType rasqal_feature_from_uri(RasqalWorldHandle* world, URIHandle* uri);
    int rasqal_feature_value_type(const FeatureType feature);
    int rasqal_features_enumerate(RasqalWorldHandle* world,
                                                         const FeatureType feature,
                                                         const char**name,
                                                         URIHandle** uri,
                                                         const char** label);
    uint rasqal_get_feature_count();
}

FeatureType Feature_From_URI(RasqalWorldWithoutFinalize world, URIWithoutFinalize uri) {
    return rasqal_feature_from_uri(world.handle, uri.handle);
}

FeatureValueType getType(FeatureType feature) {
    return cast(FeatureValueType)rasqal_feature_value_type(feature);
}

// TODO: Make data fields private?
struct FeatureDescription {
    string name, label;
    URI uri; // with finalize
}

// For API simplicity, I don't support the C library feature to retrieve only a part of the data.
// For API simplicity, we do not differentiate between failure and unknown feature. (TODO: Differentiate)
const(FeatureDescription) getFeatureDescription(RasqalWorldWithoutFinalize world, FeatureType feature) {
    char* name, label;
    URIHandle* uri;
    if(rasqal_features_enumerate(world.handle, feature, &name, &uri, &label) != 0)
        throw new RDFException();
    return FeatureDescription(name.fromStringz.idup, label.fromStringz.idup, URI.fromNonnullHandle(uri));
}

uint getFeatureCount() {
    return rasqal_get_feature_count();
}

struct FeatureIterator {
private:
    RasqalWorldWithoutFinalize _world;
    uint _num = 0;
public:
    this(RasqalWorldWithoutFinalize world) {
        _world = world;
    }
    bool empty() {
        return _num == getFeatureCount();
    }
    const(FeatureDescription) front() {
        return getFeatureDescription(_world, cast(FeatureType)_num);
    }
    void popFront() {
        ++_num;
    }
}

// TODO: Stopped at Features_Cursor
