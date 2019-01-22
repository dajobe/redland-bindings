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

FeatureType featureFromURI(RasqalWorldWithoutFinalize world, URIWithoutFinalize uri) {
    return rasqal_feature_from_uri(world.handle, uri.handle);
}

FeatureValueType getType(FeatureType feature) {
    return cast(FeatureValueType)rasqal_feature_value_type(feature);
}

/// In a future version the field may be made readonly
struct FeatureDescription {
    string name, label;
    URI uri; // with finalize
}

class RDFUnknownFeature: RDFException {
    this(string msg, string file = __FILE__, size_t line = __LINE__) {
        super(msg, file, line);
    }
    this(string file = __FILE__, size_t line = __LINE__) {
        this("IOStream error", file, line);
    }
}


// For API simplicity, I don't support the C library feature to retrieve only a part of the data.
const(FeatureDescription) getFeatureDescription(RasqalWorldWithoutFinalize world, FeatureType feature) {
    char* name, label;
    URIHandle* uri;
    int res = rasqal_features_enumerate(world.handle, feature, &name, &uri, &label);
    if(res > 0) throw new RDFUnknownFeature();
    if(res != 0) throw new RDFException();
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
    @property size_t position() { return _num; }
}

