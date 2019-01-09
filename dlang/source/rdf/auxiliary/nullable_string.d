module rdf.auxiliary.nullable_string;

import std.typecons;
import std.string;

immutable(char)* myToStringz(Nullable!string s) {
    return s.isNull ? null : s.get.toStringz;
}

Nullable!string myFromStringz(const char* s) {
    return s ? Nullable!string(s.fromStringz.idup) : Nullable!string();
}

size_t length(Nullable!string s) {
    return s.isNull ? 0 : s.get.length;
}

// const(char*) idup(Nullable!string s) {
//     return s ? s.get.idup : null;
// }