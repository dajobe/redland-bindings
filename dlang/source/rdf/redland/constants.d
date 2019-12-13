module rdf.librdf.constants;

import std.string;

private extern extern(C) immutable __gshared uint librdf_version_major;
private extern extern(C) immutable __gshared uint librdf_version_minor;
private extern extern(C) immutable __gshared uint librdf_version_release;
private extern extern(C) immutable __gshared uint librdf_version_decimal;

alias versionMajor   = librdf_version_major;
alias versionMinor   = librdf_version_minor;
alias versionRelease = librdf_version_release;
alias versionDecimal = librdf_version_decimal;

private extern extern(C) {
    immutable char* librdf_copyright_string;
    immutable char* librdf_home_url_string;
    immutable char* librdf_license_string;
    immutable char* librdf_short_copyright_string;
    immutable char* librdf_version_string;
}

string copyrightString() {
    return librdf_copyright_string.fromStringz;
}

string homeURLString() {
    return librdf_home_url_string.fromStringz;
}

string licenseString() {
    return librdf_license_string.fromStringz;
}

string shortCopyrightString() {
    return librdf_short_copyright_string.fromStringz;
}

string versionString() {
    return librdf_version_string.fromStringz;
}

unittest {
    import std.conv;

    string combinedString =
        to!string(versionMajor) ~ '.' ~ to!string(versionMinor) ~ '.' ~ to!string(versionRelease);
    uint combinedDecimal =
        versionMajor * 10000 + versionMinor * 100 + versionRelease;
    assert(combinedString == versionString, "Combined version string");
    assert(combinedDecimal == versionDecimal, "Combined decimal version");
}
