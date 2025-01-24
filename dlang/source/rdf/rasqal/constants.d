module rdf.rasqal.constants;

import std.string;

private extern extern(C) immutable __gshared uint rasqal_version_major;
private extern extern(C) immutable __gshared uint rasqal_version_minor;
private extern extern(C) immutable __gshared uint rasqal_version_release;
private extern extern(C) immutable __gshared uint rasqal_version_decimal;

alias versionMajor   = rasqal_version_major;
alias versionMinor   = rasqal_version_minor;
alias versionRelease = rasqal_version_release;
alias versionDecimal = rasqal_version_decimal;

private extern extern(C) {
    immutable char* rasqal_copyright_string;
    immutable char* rasqal_home_url_string;
    immutable char* rasqal_license_string;
    immutable char* rasqal_short_copyright_string;
    immutable char* rasqal_version_string;
}

string copyrightString() {
    return rasqal_copyright_string.fromStringz;
}

string homeURLString() {
    return rasqal_home_url_string.fromStringz;
}

string licenseString() {
    return rasqal_license_string.fromStringz;
}

string shortCopyrightString() {
    return rasqal_short_copyright_string.fromStringz;
}

string versionString() {
    return rasqal_version_string.fromStringz;
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
