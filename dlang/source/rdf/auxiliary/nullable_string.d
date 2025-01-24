module rdf.auxiliary.nullable_string;

import std.typecons;
import std.string;

immutable(char)* myToStringz(Nullable!string s) {
    return s.isNull ? null : s.get.toStringz;
}

// Can't use fromStringz and length because of automatic type conversion to string

Nullable!string myFromStringz(const char* s) {
    return s ? Nullable!string(s.fromStringz.idup) : Nullable!string();
}

size_t myLength(Nullable!string s) {
    return s.isNull ? 0 : s.get.length;
}

// const(char*) idup(Nullable!string s) {
//     return s ? s.get.idup : null;
// }
