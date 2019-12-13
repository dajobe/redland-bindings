module rdf.auxiliary.versions;

import std.conv : to;
import std.algorithm : map;
import std.algorithm.comparison : cmp;
import std.algorithm.iteration : splitter;

struct Version {
    private string str;
    this(string str) {
        this.str = str;
    }
    int opCmp(Version other) const {
        auto i = str.splitter(".").map!(x => to!int(x));
        auto j = other.str.splitter(".").map!(x => to!int(x));
        return cmp(i, j);
    }
}

