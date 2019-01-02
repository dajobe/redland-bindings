module rdf.auxiliary.nullable_string;

import std.typecons;
import std.string;

immutable(char)* myToStringz(Nullable!string s) {
    return s.isNull ? null : s.get.toStringz;
}

size_t length(Nullable!string s) {
    return s.isNull ? 0 : s.get.length;
}
